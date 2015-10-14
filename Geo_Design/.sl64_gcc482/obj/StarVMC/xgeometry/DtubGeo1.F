C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE DTUBGEO1                                                 1
C DTUBGEO1 GEOMETRY FOR PIXEL DTUBE                                       1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'DTUBGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'GEOMETRY FOR PIXEL DTUBE'/                           1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR JB                                                               1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JB'/                                          2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED JAN, 23TH, 2013                                                 2
      CHARACTER*40 CREATED /'JAN, 23TH, 2013'/                            3
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
      INTEGER BLOCK_DTUH                                                  4
      INTEGER BLOCK_APTO                                                  60
      INTEGER BLOCK_UCTO                                                  60
      INTEGER BLOCK_OUCY                                                  60
      INTEGER BLOCK_DCBO                                                  60
      INTEGER BLOCK_APBM                                                  60
      INTEGER BLOCK_UCBO                                                  60
      INTEGER BLOCK_DCTO                                                  60
      INTEGER BLOCK_INCY                                                  60
      INTEGER BLOCK_CYTR                                                  60
      INTEGER BLOCK_APTE                                                  60
      INTEGER BLOCK_APBE                                                  60
      INTEGER BLOCK_UCTE                                                  60
      INTEGER BLOCK_DCBE                                                  60
      INTEGER BLOCK_OUCE                                                  60
      INTEGER BLOCK_ATTI                                                  60
      INTEGER BLOCK_ATBI                                                  60
      INTEGER BLOCK_OUCI                                                  60
      INTEGER BLOCK_UCTI                                                  60
      INTEGER BLOCK_DCBI                                                  60
      INTEGER BLOCK_DCTH                                                  60
      INTEGER BLOCK_TCTH                                                  60
      INTEGER BLOCK_CSTI                                                  60
      INTEGER BLOCK_CSBI                                                  60
      INTEGER BLOCK_LSAA                                                  60
      INTEGER BLOCK_LSAB                                                  60
      INTEGER BLOCK_LSAC                                                  60
      INTEGER BLOCK_LSAD                                                  60
      INTEGER BLOCK_LSAE                                                  60
      INTEGER BLOCK_LSAF                                                  60
      INTEGER BLOCK_LSAG                                                  60
      INTEGER BLOCK_LSAH                                                  60
      INTEGER BLOCK_LSAI                                                  60
      INTEGER BLOCK_LSAJ                                                  60
      INTEGER BLOCK_LSAK                                                  60
      INTEGER BLOCK_LSAL                                                  60
      INTEGER BLOCK_LSAM                                                  60
      INTEGER BLOCK_LSAN                                                  60
      INTEGER BLOCK_LSAO                                                  60
      INTEGER BLOCK_LSAP                                                  60
      INTEGER BLOCK_LSAQ                                                  60
      INTEGER BLOCK_LSAR                                                  60
      INTEGER BLOCK_LSAS                                                  60
      INTEGER BLOCK_LSAT                                                  60
      INTEGER BLOCK_SCAD                                                  60
      INTEGER BLOCK_LCAD                                                  60
      INTEGER BLOCK_APET                                                  60
      INTEGER BLOCK_APEB                                                  60
      INTEGER BLOCK_OCYE                                                  60
      INTEGER BLOCK_OCEA                                                  60
      INTEGER BLOCK_UPTE                                                  60
      INTEGER BLOCK_BOTE                                                  60
      INTEGER BLOCK_OCEB                                                  60
      INTEGER BLOCK_OCEC                                                  60
      INTEGER BLOCK_OCED                                                  60
      INTEGER BLOCK_TUBA                                                  60
      REAL *4 MAXZVOL/23.26/                                              61
      REAL *4 DSEA/27.5145/                                               63
      REAL *4 DSEB/2.6467/                                                65
      REAL *4 DSEC/1.5153/                                                67
      REAL *4 DSED/2.2735/                                                69
      REAL *4 DHSA/3.7738/                                                71
      REAL *4 DHSB/4.2264/                                                73
      REAL *4 RMINTUBESECTIONAIN/6.2/                                     75
      REAL *4 RMAXTUBESECTIONAIN/6.4/                                     77
      REAL *4 RMINTUBESECTIONAOUT/15.0/                                   79
      REAL *4 RMAXTUBESECTIONAOUT/15.2/                                   81
      REAL *4 ANGLETOPLEFT/99.784/                                        83
      REAL *4 ANGLEBOTTOMLEFT/69.721/                                     85
      REAL *4 THK/0.1/                                                    87
      REAL *4 ARCLENGTHOUT/0.32/                                          89
      REAL *4 DTUBEW/7.5/                                                 91
      REAL *4 DTUBEH/14.2/                                                93
      REAL *4 THKR/0.2441/                                                95
      REAL *4 XPOS,YPOS,ZPOS,ANGLE,OUTERTUBELENGTH                        97
      INTEGER *4 SECTOR                                                   99
      REAL *4 ZOFFD/-40.25/                                               101
      REAL *4 ZOFFDM/-1.63/                                               103
      REAL *4 ZOFFP/-21.65/                                               105
      INTEGER *4 II,JJ,KK,LL                                              107
      REAL *4 XCABLE,YCABLE                                               109
      REAL *4 RADCABLE/0.165/                                             111
      REAL *4 LENGTHCAL/4.7996/                                           113
      REAL *4 LENGTHCAS/1.8518/                                           115
      REAL *4 ZOFFCABLEL/-25.6745/                                        117
      REAL *4 ZOFFCABLES/-27.0759/                                        119
      REAL *4 SECTIONLENGTH,ANGLEORI,ARCLENGTH,PIRATIO,TWOPIRATIO         121
C -                                                                       123
      INTEGER SCSD_VERSION                                                123
      EQUIVALENCE (BANK_SCSD(3),SCSD_VERSION,RE_SCSD_VERSION),            123
     *(NAMES_SCSD(1),NSCSD_VERSION)                                       123
      EQUIVALENCE (COMMEN_SCSD(1),CSCSD_VERSION),(MAP_SCSD(1,1),          123
     *MSCSD_VERSION)                                                      123
      CHARACTER CSCSD_VERSION*40/' '/                                     123
      CHARACTER NSCSD_VERSION*12/'I.VERSION'/                             123
      INTEGER MSCSD_VERSION(2)/1,1/                                       123
      REAL RE_SCSD_VERSION                                                123
C -                                                                       123
      INTEGER SCSD_NUMBEROFCABLES                                         123
      EQUIVALENCE (BANK_SCSD(4),SCSD_NUMBEROFCABLES,                      123
     *RE_SCSD_NUMBEROFCABLES),(NAMES_SCSD(2),NSCSD_NUMBEROFCABLES)        123
      EQUIVALENCE (COMMEN_SCSD(2),CSCSD_NUMBEROFCABLES),(MAP_SCSD(1,2),   123
     *MSCSD_NUMBEROFCABLES)                                               123
      CHARACTER CSCSD_NUMBEROFCABLES*40/' '/                              123
      CHARACTER NSCSD_NUMBEROFCABLES*12/'I.NUMBEROFCABLES'/               123
      INTEGER MSCSD_NUMBEROFCABLES(2)/1,1/                                123
      REAL RE_SCSD_NUMBEROFCABLES                                         123
C -                                                                       123
      REAL SCSD_XPOS(5)                                                   123
      EQUIVALENCE (BANK_SCSD(5),SCSD_XPOS,RE_SCSD_XPOS),(NAMES_SCSD(3),   123
     *NSCSD_XPOS)                                                         123
      EQUIVALENCE (COMMEN_SCSD(3),CSCSD_XPOS),(MAP_SCSD(1,3),             123
     *MSCSD_XPOS)                                                         123
      CHARACTER CSCSD_XPOS*40/' '/                                        123
      CHARACTER NSCSD_XPOS*12/'R.XPOS'/                                   123
      INTEGER MSCSD_XPOS(2)/5,1/                                          123
      REAL RE_SCSD_XPOS                                                   123
C -                                                                       123
      REAL SCSD_YPOS(5)                                                   123
      EQUIVALENCE (BANK_SCSD(10),SCSD_YPOS,RE_SCSD_YPOS),(NAMES_SCSD(4),  123
     *NSCSD_YPOS)                                                         123
      EQUIVALENCE (COMMEN_SCSD(4),CSCSD_YPOS),(MAP_SCSD(1,4),             123
     *MSCSD_YPOS)                                                         123
      CHARACTER CSCSD_YPOS*40/' '/                                        123
      CHARACTER NSCSD_YPOS*12/'R.YPOS'/                                   123
      INTEGER MSCSD_YPOS(2)/5,1/                                          123
      REAL RE_SCSD_YPOS                                                   123
C -                                                                       123
      INTEGER LEN_SCSD,LLEN_SCSD,LINK_SCSD                                123
      PARAMETER (LEN_SCSD=14)                                             123
      PARAMETER (LLEN_SCSD=4)                                             123
      INTEGER FLAG_SCSD/0/,MAP_SCSD(2,LLEN_SCSD),IDNUM_SCSD(10)           123
      CHARACTER NAMES_SCSD(LLEN_SCSD)*12,COMMEN_SCSD(LLEN_SCSD)*40        123
      DATA IDNUM_SCSD/10*0/                                               123
      REAL BANK_SCSD(LEN_SCSD)                                            123
      CHARACTER*60 BPATH_SCSD/' '/                                        123
      SAVE FLAG_SCSD,MAP_SCSD,IDNUM_SCSD,NAMES_SCSD,COMMEN_SCSD,          123
     *BPATH_SCSD                                                          123
      DATA LINK_SCSD/0/,BANK_SCSD/LEN_SCSD*0/                             123
C *                                                                       123
C -                                                                       124
      INTEGER LCSD_VERSION                                                124
      EQUIVALENCE (BANK_LCSD(3),LCSD_VERSION,RE_LCSD_VERSION),            124
     *(NAMES_LCSD(1),NLCSD_VERSION)                                       124
      EQUIVALENCE (COMMEN_LCSD(1),CLCSD_VERSION),(MAP_LCSD(1,1),          124
     *MLCSD_VERSION)                                                      124
      CHARACTER CLCSD_VERSION*40/' '/                                     124
      CHARACTER NLCSD_VERSION*12/'I.VERSION'/                             124
      INTEGER MLCSD_VERSION(2)/1,1/                                       124
      REAL RE_LCSD_VERSION                                                124
C -                                                                       124
      INTEGER LCSD_NUMBEROFCABLES                                         124
      EQUIVALENCE (BANK_LCSD(4),LCSD_NUMBEROFCABLES,                      124
     *RE_LCSD_NUMBEROFCABLES),(NAMES_LCSD(2),NLCSD_NUMBEROFCABLES)        124
      EQUIVALENCE (COMMEN_LCSD(2),CLCSD_NUMBEROFCABLES),(MAP_LCSD(1,2),   124
     *MLCSD_NUMBEROFCABLES)                                               124
      CHARACTER CLCSD_NUMBEROFCABLES*40/' '/                              124
      CHARACTER NLCSD_NUMBEROFCABLES*12/'I.NUMBEROFCABLES'/               124
      INTEGER MLCSD_NUMBEROFCABLES(2)/1,1/                                124
      REAL RE_LCSD_NUMBEROFCABLES                                         124
C -                                                                       124
      REAL LCSD_XPOS(15)                                                  124
      EQUIVALENCE (BANK_LCSD(5),LCSD_XPOS,RE_LCSD_XPOS),(NAMES_LCSD(3),   124
     *NLCSD_XPOS)                                                         124
      EQUIVALENCE (COMMEN_LCSD(3),CLCSD_XPOS),(MAP_LCSD(1,3),             124
     *MLCSD_XPOS)                                                         124
      CHARACTER CLCSD_XPOS*40/' '/                                        124
      CHARACTER NLCSD_XPOS*12/'R.XPOS'/                                   124
      INTEGER MLCSD_XPOS(2)/15,1/                                         124
      REAL RE_LCSD_XPOS                                                   124
C -                                                                       124
      REAL LCSD_YPOS(15)                                                  124
      EQUIVALENCE (BANK_LCSD(20),LCSD_YPOS,RE_LCSD_YPOS),(NAMES_LCSD(4),  124
     *NLCSD_YPOS)                                                         124
      EQUIVALENCE (COMMEN_LCSD(4),CLCSD_YPOS),(MAP_LCSD(1,4),             124
     *MLCSD_YPOS)                                                         124
      CHARACTER CLCSD_YPOS*40/' '/                                        124
      CHARACTER NLCSD_YPOS*12/'R.YPOS'/                                   124
      INTEGER MLCSD_YPOS(2)/15,1/                                         124
      REAL RE_LCSD_YPOS                                                   124
C -                                                                       124
      INTEGER LEN_LCSD,LLEN_LCSD,LINK_LCSD                                124
      PARAMETER (LEN_LCSD=34)                                             124
      PARAMETER (LLEN_LCSD=4)                                             124
      INTEGER FLAG_LCSD/0/,MAP_LCSD(2,LLEN_LCSD),IDNUM_LCSD(10)           124
      CHARACTER NAMES_LCSD(LLEN_LCSD)*12,COMMEN_LCSD(LLEN_LCSD)*40        124
      DATA IDNUM_LCSD/10*0/                                               124
      REAL BANK_LCSD(LEN_LCSD)                                            124
      CHARACTER*60 BPATH_LCSD/' '/                                        124
      SAVE FLAG_LCSD,MAP_LCSD,IDNUM_LCSD,NAMES_LCSD,COMMEN_LCSD,          124
     *BPATH_LCSD                                                          124
      DATA LINK_LCSD/0/,BANK_LCSD/LEN_LCSD*0/                             124
C *                                                                       124
C -                                                                       126
      INTEGER RADD_VERSION                                                126
      EQUIVALENCE (BANK_RADD(3),RADD_VERSION,RE_RADD_VERSION),            126
     *(NAMES_RADD(1),NRADD_VERSION)                                       126
      EQUIVALENCE (COMMEN_RADD(1),CRADD_VERSION),(MAP_RADD(1,1),          126
     *MRADD_VERSION)                                                      126
      CHARACTER CRADD_VERSION*40/' '/                                     126
      CHARACTER NRADD_VERSION*12/'I.VERSION'/                             126
      INTEGER MRADD_VERSION(2)/1,1/                                       126
      REAL RE_RADD_VERSION                                                126
C -                                                                       126
      INTEGER RADD_NUMBEROFSECTIONS                                       126
      EQUIVALENCE (BANK_RADD(4),RADD_NUMBEROFSECTIONS,                    126
     *RE_RADD_NUMBEROFSECTIONS),(NAMES_RADD(2),NRADD_NUMBEROFSECTIONS)    126
      EQUIVALENCE (COMMEN_RADD(2),CRADD_NUMBEROFSECTIONS),(MAP_RADD(1,    126
     *2),MRADD_NUMBEROFSECTIONS)                                          126
      CHARACTER CRADD_NUMBEROFSECTIONS*40/' '/                            126
      CHARACTER NRADD_NUMBEROFSECTIONS*12/'I.NUMBEROFSECTIONS'/           126
      INTEGER MRADD_NUMBEROFSECTIONS(2)/1,1/                              126
      REAL RE_RADD_NUMBEROFSECTIONS                                       126
C -                                                                       126
      INTEGER RADD_NUMBEROFCABLES                                         126
      EQUIVALENCE (BANK_RADD(5),RADD_NUMBEROFCABLES,                      126
     *RE_RADD_NUMBEROFCABLES),(NAMES_RADD(3),NRADD_NUMBEROFCABLES)        126
      EQUIVALENCE (COMMEN_RADD(3),CRADD_NUMBEROFCABLES),(MAP_RADD(1,3),   126
     *MRADD_NUMBEROFCABLES)                                               126
      CHARACTER CRADD_NUMBEROFCABLES*40/' '/                              126
      CHARACTER NRADD_NUMBEROFCABLES*12/'I.NUMBEROFCABLES'/               126
      INTEGER MRADD_NUMBEROFCABLES(2)/1,1/                                126
      REAL RE_RADD_NUMBEROFCABLES                                         126
C -                                                                       126
      REAL RADD_ANGLEMIN(20)                                              126
      EQUIVALENCE (BANK_RADD(6),RADD_ANGLEMIN,RE_RADD_ANGLEMIN),          126
     *(NAMES_RADD(4),NRADD_ANGLEMIN)                                      126
      EQUIVALENCE (COMMEN_RADD(4),CRADD_ANGLEMIN),(MAP_RADD(1,4),         126
     *MRADD_ANGLEMIN)                                                     126
      CHARACTER CRADD_ANGLEMIN*40/' '/                                    126
      CHARACTER NRADD_ANGLEMIN*12/'R.ANGLEMIN'/                           126
      INTEGER MRADD_ANGLEMIN(2)/20,1/                                     126
      REAL RE_RADD_ANGLEMIN                                               126
C -                                                                       126
      REAL RADD_ANGLEMAX(20)                                              126
      EQUIVALENCE (BANK_RADD(26),RADD_ANGLEMAX,RE_RADD_ANGLEMAX),         126
     *(NAMES_RADD(5),NRADD_ANGLEMAX)                                      126
      EQUIVALENCE (COMMEN_RADD(5),CRADD_ANGLEMAX),(MAP_RADD(1,5),         126
     *MRADD_ANGLEMAX)                                                     126
      CHARACTER CRADD_ANGLEMAX*40/' '/                                    126
      CHARACTER NRADD_ANGLEMAX*12/'R.ANGLEMAX'/                           126
      INTEGER MRADD_ANGLEMAX(2)/20,1/                                     126
      REAL RE_RADD_ANGLEMAX                                               126
C -                                                                       126
      REAL RADD_ZPOS(20)                                                  126
      EQUIVALENCE (BANK_RADD(46),RADD_ZPOS,RE_RADD_ZPOS),(NAMES_RADD(6),  126
     *NRADD_ZPOS)                                                         126
      EQUIVALENCE (COMMEN_RADD(6),CRADD_ZPOS),(MAP_RADD(1,6),             126
     *MRADD_ZPOS)                                                         126
      CHARACTER CRADD_ZPOS*40/' '/                                        126
      CHARACTER NRADD_ZPOS*12/'R.ZPOS'/                                   126
      INTEGER MRADD_ZPOS(2)/20,1/                                         126
      REAL RE_RADD_ZPOS                                                   126
C -                                                                       126
      REAL RADD_RADIUS                                                    126
      EQUIVALENCE (BANK_RADD(66),RADD_RADIUS,RE_RADD_RADIUS),             126
     *(NAMES_RADD(7),NRADD_RADIUS)                                        126
      EQUIVALENCE (COMMEN_RADD(7),CRADD_RADIUS),(MAP_RADD(1,7),           126
     *MRADD_RADIUS)                                                       126
      CHARACTER CRADD_RADIUS*40/' '/                                      126
      CHARACTER NRADD_RADIUS*12/'R.RADIUS'/                               126
      INTEGER MRADD_RADIUS(2)/1,1/                                        126
      REAL RE_RADD_RADIUS                                                 126
C -                                                                       126
      INTEGER LEN_RADD,LLEN_RADD,LINK_RADD                                126
      PARAMETER (LEN_RADD=66)                                             126
      PARAMETER (LLEN_RADD=7)                                             126
      INTEGER FLAG_RADD/0/,MAP_RADD(2,LLEN_RADD),IDNUM_RADD(10)           126
      CHARACTER NAMES_RADD(LLEN_RADD)*12,COMMEN_RADD(LLEN_RADD)*40        126
      DATA IDNUM_RADD/10*0/                                               126
      REAL BANK_RADD(LEN_RADD)                                            126
      CHARACTER*60 BPATH_RADD/' '/                                        126
      SAVE FLAG_RADD,MAP_RADD,IDNUM_RADD,NAMES_RADD,COMMEN_RADD,          126
     *BPATH_RADD                                                          126
      DATA LINK_RADD/0/,BANK_RADD/LEN_RADD*0/                             126
C *                                                                       126
C *                                                                       127
      CALL AGSBEGM(MODULE,IPRIN)                                          127
      IF (IPRIN.LT.0) RETURN                                              127
C Filling bank SCSD - SHORT CABLE SECTION D                               127
      BANK_TITLE='SHORT CABLE SECTION D'                                  127
      BPATH_SCSD='SCSD*'                                                  127
      IDNUM_SCSD(1)=0                                                     128
C NONE                                                                    128
      SCSD_VERSION=1                                                      128
      CSCSD_VERSION='NONE'                                                128
C NONE                                                                    129
      SCSD_NUMBEROFCABLES=5                                               129
      CSCSD_NUMBEROFCABLES='NONE'                                         129
C NONE                                                                    131
      SCSD_XPOS(1)=-3.5642                                                131
      SCSD_XPOS(2)=-7.2561                                                131
      SCSD_XPOS(3)=-8.1764                                                131
      SCSD_XPOS(4)=-5.9736                                                131
      SCSD_XPOS(5)=-1.4891                                                131
      CSCSD_XPOS='NONE'                                                   131
C NONE                                                                    133
      SCSD_YPOS(1)=7.4391                                                 133
      SCSD_YPOS(2)=3.9234                                                 133
      SCSD_YPOS(3)=-1.0909                                                133
      SCSD_YPOS(4)=-5.6885                                                133
      SCSD_YPOS(5)=-8.1133                                                133
      CSCSD_YPOS='NONE'                                                   133
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCSD',    134
     *BPATH_SCSD,IDNUM_SCSD,1, MAP_SCSD,NAMES_SCSD,COMMEN_SCSD,           134
     *BANK_SCSD,LEN_SCSD,LLEN_SCSD,LINK_SCSD,FLAG_SCSD)                   134
C *                                                                       134
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCSD',    134
     *BPATH_SCSD,IDNUM_SCSD,1, MAP_SCSD,NAMES_SCSD,COMMEN_SCSD,           134
     *BANK_SCSD,LEN_SCSD,LLEN_SCSD,LINK_SCSD,FLAG_SCSD)                   134
C *                                                                       135
      CALL AGSBEGM(MODULE,IPRIN)                                          135
      IF (IPRIN.LT.0) RETURN                                              135
C Filling bank LCSD - LONG CABLE SECTION D                                135
      BANK_TITLE='LONG CABLE SECTION D'                                   135
      BPATH_LCSD='LCSD*'                                                  135
      IDNUM_LCSD(1)=0                                                     136
C NONE                                                                    136
      LCSD_VERSION=1                                                      136
      CLCSD_VERSION='NONE'                                                136
C NONE                                                                    137
      LCSD_NUMBEROFCABLES=15                                              137
      CLCSD_NUMBEROFCABLES='NONE'                                         137
C NONE                                                                    141
      LCSD_XPOS(1)=-1.2374                                                141
      LCSD_XPOS(2)=-2.9952                                                141
      LCSD_XPOS(3)=-4.62                                                  141
      LCSD_XPOS(4)=-6.0408                                                141
      LCSD_XPOS(5)=-7.2015                                                141
      LCSD_XPOS(6)=-8.0455                                                141
      LCSD_XPOS(7)=-8.5368                                                141
      LCSD_XPOS(8)=-8.657                                                 141
      LCSD_XPOS(9)=-8.3979                                                141
      LCSD_XPOS(10)=-7.772                                                141
      LCSD_XPOS(11)=-6.8059                                               141
      LCSD_XPOS(12)=-5.5426                                               141
      LCSD_XPOS(13)=-4.0386                                               141
      LCSD_XPOS(14)=-2.3551                                               141
      LCSD_XPOS(15)=-0.5702                                               141
      CLCSD_XPOS='NONE'                                                   141
C NONE                                                                    145
      LCSD_YPOS(1)=8.574                                                  145
      LCSD_YPOS(2)=8.1293                                                 145
      LCSD_YPOS(3)=7.3289                                                 145
      LCSD_YPOS(4)=6.2092                                                 145
      LCSD_YPOS(5)=4.8162                                                 145
      LCSD_YPOS(6)=3.2137                                                 145
      LCSD_YPOS(7)=1.4727                                                 145
      LCSD_YPOS(8)=-0.3365                                                145
      LCSD_YPOS(9)=-2.1291                                                145
      LCSD_YPOS(10)=-3.8264                                               145
      LCSD_YPOS(11)=-5.3607                                               145
      LCSD_YPOS(12)=-6.6586                                               145
      LCSD_YPOS(13)=-7.6639                                               145
      LCSD_YPOS(14)=-8.3373                                               145
      LCSD_YPOS(15)=-8.6448                                               145
      CLCSD_YPOS='NONE'                                                   145
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LCSD',    146
     *BPATH_LCSD,IDNUM_LCSD,1, MAP_LCSD,NAMES_LCSD,COMMEN_LCSD,           146
     *BANK_LCSD,LEN_LCSD,LLEN_LCSD,LINK_LCSD,FLAG_LCSD)                   146
C *                                                                       146
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LCSD',    146
     *BPATH_LCSD,IDNUM_LCSD,1, MAP_LCSD,NAMES_LCSD,COMMEN_LCSD,           146
     *BANK_LCSD,LEN_LCSD,LLEN_LCSD,LINK_LCSD,FLAG_LCSD)                   146
C *                                                                       147
      CALL AGSBEGM(MODULE,IPRIN)                                          147
      IF (IPRIN.LT.0) RETURN                                              147
C Filling bank RADD - SHORT CABLE SECTION D                               147
      BANK_TITLE='SHORT CABLE SECTION D'                                  147
      BPATH_RADD='RADD*'                                                  147
      IDNUM_RADD(1)=0                                                     148
C NONE                                                                    148
      RADD_VERSION=1                                                      148
      CRADD_VERSION='NONE'                                                148
C NONE                                                                    149
      RADD_NUMBEROFSECTIONS=10                                            149
      CRADD_NUMBEROFSECTIONS='NONE'                                       149
C NONE                                                                    150
      RADD_NUMBEROFCABLES=20                                              150
      CRADD_NUMBEROFCABLES='NONE'                                         150
C NONE                                                                    154
      RADD_ANGLEMIN(1)=-46.62                                             154
      RADD_ANGLEMIN(2)=-40.97                                             154
      RADD_ANGLEMIN(3)=-35.32                                             154
      RADD_ANGLEMIN(4)=-29.67                                             154
      RADD_ANGLEMIN(5)=-24.01                                             154
      RADD_ANGLEMIN(6)=-18.36                                             154
      RADD_ANGLEMIN(7)=-12.71                                             154
      RADD_ANGLEMIN(8)=-7.06                                              154
      RADD_ANGLEMIN(9)=-1.41                                              154
      RADD_ANGLEMIN(10)=4.24                                              154
      RADD_ANGLEMIN(11)=9.89                                              154
      RADD_ANGLEMIN(12)=15.54                                             154
      RADD_ANGLEMIN(13)=21.19                                             154
      RADD_ANGLEMIN(14)=26.84                                             154
      RADD_ANGLEMIN(15)=32.50                                             154
      RADD_ANGLEMIN(16)=38.15                                             154
      RADD_ANGLEMIN(17)=43.80                                             154
      RADD_ANGLEMIN(18)=49.45                                             154
      RADD_ANGLEMIN(19)=55.10                                             154
      RADD_ANGLEMIN(20)=60.75                                             154
      CRADD_ANGLEMIN='NONE'                                               154
C NONE                                                                    158
      RADD_ANGLEMAX(1)=38.27                                              158
      RADD_ANGLEMAX(2)=38.27                                              158
      RADD_ANGLEMAX(3)=38.27                                              158
      RADD_ANGLEMAX(4)=38.27                                              158
      RADD_ANGLEMAX(5)=45.75                                              158
      RADD_ANGLEMAX(6)=45.75                                              158
      RADD_ANGLEMAX(7)=45.75                                              158
      RADD_ANGLEMAX(8)=45.75                                              158
      RADD_ANGLEMAX(9)=45.75                                              158
      RADD_ANGLEMAX(10)=52.62                                             158
      RADD_ANGLEMAX(11)=52.62                                             158
      RADD_ANGLEMAX(12)=52.62                                             158
      RADD_ANGLEMAX(13)=58.91                                             158
      RADD_ANGLEMAX(14)=58.91                                             158
      RADD_ANGLEMAX(15)=58.91                                             158
      RADD_ANGLEMAX(16)=58.91                                             158
      RADD_ANGLEMAX(17)=64.97                                             158
      RADD_ANGLEMAX(18)=64.97                                             158
      RADD_ANGLEMAX(19)=64.97                                             158
      RADD_ANGLEMAX(20)=64.97                                             158
      CRADD_ANGLEMAX='NONE'                                               158
C NONE                                                                    163
      RADD_ZPOS(1)=-53.2                                                  163
      RADD_ZPOS(2)=-52.5                                                  163
      RADD_ZPOS(3)=-51.8                                                  163
      RADD_ZPOS(4)=-51.1                                                  163
      RADD_ZPOS(5)=-50.4                                                  163
      RADD_ZPOS(6)=-49.7                                                  163
      RADD_ZPOS(7)=-49                                                    163
      RADD_ZPOS(8)=-48.3                                                  163
      RADD_ZPOS(9)=-47.6                                                  163
      RADD_ZPOS(10)=-46.9                                                 163
      RADD_ZPOS(11)=-46.2                                                 163
      RADD_ZPOS(12)=-45.5                                                 163
      RADD_ZPOS(13)=-44.8                                                 163
      RADD_ZPOS(14)=-44.0999                                              163
      RADD_ZPOS(15)=-43.401                                               163
      RADD_ZPOS(16)=-42.7                                                 163
      RADD_ZPOS(17)=-42.0                                                 163
      RADD_ZPOS(18)=-41.3                                                 163
      RADD_ZPOS(19)=-40.6                                                 163
      RADD_ZPOS(20)=-39.9                                                 163
      CRADD_ZPOS='NONE'                                                   163
C NONE                                                                    164
      RADD_RADIUS=8.1                                                     164
      CRADD_RADIUS='NONE'                                                 164
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RADD',    165
     *BPATH_RADD,IDNUM_RADD,1, MAP_RADD,NAMES_RADD,COMMEN_RADD,           165
     *BANK_RADD,LEN_RADD,LLEN_RADD,LINK_RADD,FLAG_RADD)                   165
C *                                                                       165
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RADD',    165
     *BPATH_RADD,IDNUM_RADD,1, MAP_RADD,NAMES_RADD,COMMEN_RADD,           165
     *BANK_RADD,LEN_RADD,LLEN_RADD,LINK_RADD,FLAG_RADD)                   165
C use SCSD bank                                                           166
      BPATH_SCSD='SCSD*'                                                  166
      IDNUM_SCSD(1)=0                                                     166
      SCSD_VERSION=1                                                      166
      CALL AGDGETP('DTUBGEO1','*',-999,'SCSD',NSCSD_VERSION,              166
     *RE_SCSD_VERSION,BPATH_SCSD,IDNUM_SCSD,1,MAP_SCSD,NAMES_SCSD,        166
     *BANK_SCSD,LEN_SCSD,LLEN_SCSD,LINK_SCSD,FLAG_SCSD)                   166
C use LCSD bank                                                           167
      BPATH_LCSD='LCSD*'                                                  167
      IDNUM_LCSD(1)=0                                                     167
      LCSD_VERSION=1                                                      167
      CALL AGDGETP('DTUBGEO1','*',-999,'LCSD',NLCSD_VERSION,              167
     *RE_LCSD_VERSION,BPATH_LCSD,IDNUM_LCSD,1,MAP_LCSD,NAMES_LCSD,        167
     *BANK_LCSD,LEN_LCSD,LLEN_LCSD,LINK_LCSD,FLAG_LCSD)                   167
C use RADD bank                                                           168
      BPATH_RADD='RADD*'                                                  168
      IDNUM_RADD(1)=0                                                     168
      RADD_VERSION=1                                                      168
      CALL AGDGETP('DTUBGEO1','*',-999,'RADD',NRADD_VERSION,              168
     *RE_RADD_VERSION,BPATH_RADD,IDNUM_RADD,1,MAP_RADD,NAMES_RADD,        168
     *BANK_RADD,LEN_RADD,LLEN_RADD,LINK_RADD,FLAG_RADD)                   168
C Geant MATERIAL : CFRPMIX ===> A=12 Z=6 DENS=1.3 RADL=23.9 <===          169
      CALL AGSRESET                                                       169
      AG_A = 12                                                           169
      AG_Z = 6                                                            169
      AG_DENS = 1.3                                                       169
      AG_RADL = 23.9                                                      169
      AG_TITLE = 'CFRPMIX'                                                169
      AG_EXNAME = 'MATERIAL'                                              169
      AG_PARLIST = 'A_Z_DENS_RADL'                                        169
      CALL AXMATERIAL                                                     169
C *                                                                       169
      OUTERTUBELENGTH = 360-2*ANGLETOPLEFT                                169
      ZOFFDM = -(33.95 - DSEA)/2.0                                        171
      PIRATIO=PI/180.0                                                    172
      TWOPIRATIO=(2*PI)/360                                               173
      CALL AGSBEGM(MODULE,IPRIN)                                          174
C Check IPRIN<0                                                           174
      IF (IPRIN.LT.0) RETURN                                              174
C Create DTUH :                                                           174
C Call To Local Procedure DTUH                                            174
      ASSIGN 5020 TO BLOCK_DTUH                                           174
      GO TO  5010                                                         174
5020  CONTINUE                                                            174
C *                                                                       174
C Geant POSITION : DTUH ===> MOTHER='IDSM' X=0 Y=0 Z=ZOFFD KONLY='MANY'   175
C <===                                                                    175
      CALL AGSRESET                                                       175
      AG_MOTHER = 'IDSM'                                                  175
      AG_X = 0                                                            175
      AG_Y = 0                                                            175
      AG_Z = ZOFFD                                                        175
      AG_KONLY = 'MANY'                                                   175
      AG_TITLE = 'DTUH'                                                   175
      AG_EXNAME = 'POSITION'                                              175
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   175
      CALL AXPOSITION                                                     175
C *                                                                       175
C Geant POSITION : DTUH ===> MOTHER='IDSM' X=0 Y=0 Z=ZOFFD ALPHAY=180 OR  175
C T=XY-Z KONLY='MANY' <===                                                175
      CALL AGSRESET                                                       176
      AG_MOTHER = 'IDSM'                                                  176
      AG_X = 0                                                            176
      AG_Y = 0                                                            176
      AG_Z = ZOFFD                                                        176
      AG_ALPHAN=AG_ALPHAN+1                                               176
      AG_ALPHAK(AG_ALPHAN)=2                                              176
      AG_ALPHAL(AG_ALPHAN)=180                                            176
      AG_ALPHAN=AG_ALPHAN+1                                               176
      AG_ALPHAK(AG_ALPHAN)=IGOR('XY-Z')                                   176
      AG_KONLY = 'MANY'                                                   176
      AG_TITLE = 'DTUH'                                                   176
      AG_EXNAME = 'POSITION'                                              176
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_ORT_KONLY'                        176
      CALL AXPOSITION                                                     176
C *                                                                       176
C ----------------------------------------------------------------------  176
C ------------ DTUH                                                       176
      CALL AGSENDM                                                        178
      RETURN                                                              178
C Begin Local Procedure DTUH                                              178
C DTUH HALF DTUBE                                                         178
5010  CONTINUE                                                            178
         AG_TITLE='DTUH'                                                  178
         AG_MODULE=MODULE                                                 178
         AG_MARK='V00'                                                    178
         CALL AGSPUSH                                                     178
         AG_MODULE=MODULE                                                 178
C    *                                                                    178
C    Geant MATERIAL : AIR ===> inherited arguments used <===              179
         CALL AGSRESET                                                    179
         AG_TITLE = 'AIR'                                                 179
         AG_EXNAME = 'MATERIAL'                                           179
         AG_PARLIST = 'NONE'                                              179
         CALL AXMATERIAL                                                  179
C    *                                                                    179
C    Geant ATTRIBUTE : DTUH ===> SEEN=0 COLO=1 <===                       179
         CALL AGSRESET                                                    180
         AG_SEEN = 0                                                      180
         AG_COLO = 1                                                      180
         AG_TITLE = 'DTUH'                                                180
         AG_EXNAME = 'ATTRIBUTE'                                          180
         AG_PARLIST = 'SEEN_COLO'                                         180
         CALL AXATTRIBUTE                                                 180
C    *                                                                    180
C    Geant SHAPE : TUBS ===> RMIN=2.7 RMAX=8.9 PHI1=-270 PHI2=-90, DZ=33  180
C    .95/2. <===                                                          180
         CALL AGSRESET                                                    182
         AG_RMIN = 2.7                                                    182
         AG_RMAX = 8.9                                                    182
         AG_PHI1 = -270                                                   182
         AG_PHI2 = -90                                                    182
         AG_DZ = 33.95/2.                                                 182
         AG_TITLE = 'TUBS'                                                182
         AG_EXNAME = 'SHAPE'                                              182
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            182
         CALL AXSHAPE                                                     182
C    *                                                                    182
C    *                                                                    182
         KK=1                                                             183
            GO TO 5033                                                    183
5031        KK=KK+1                                                       183
5033        IF(KK-(RADD_NUMBEROFCABLES).GT.0) GO TO 5032                  183
            ARCLENGTH=                                                    184
     *      (RADD_ANGLEMAX(KK)-RADD_ANGLEMIN(KK))*(TWOPIRATIO*            184
     *      RADD_RADIUS)                                                  184
            ANGLEORI=(RADD_ANGLEMAX(KK)-RADD_ANGLEMIN(KK))/               185
     *      (1.0*RADD_NUMBEROFSECTIONS)                                   185
C       *                                                                 186
            II=1                                                          186
               GO TO 5043                                                 186
5041           II=II+1                                                    186
5043           IF(II-(RADD_NUMBEROFSECTIONS).GT.0) GO TO 5042             186
               ANGLE=                                                     187
     *         PIRATIO*(ANGLEORI*(II-1)+RADD_ANGLEMIN(KK)+0.5*ANGLEORI)   187
               XCABLE=RADD_RADIUS*COS(ANGLE)                              188
               YCABLE=RADD_RADIUS*SIN(ANGLE)                              189
C          Create TUBA :                                                  190
C          Call To Local Procedure TUBA                                   190
               ASSIGN 5060 TO BLOCK_TUBA                                  190
               GO TO  5050                                                190
5060           CONTINUE                                                   190
C          *                                                              190
C          Geant POSITION : TUBA ===> MOTHER='DTUH' X=-1*XCABLE Y=YCABLE  192
C           Z=RADD_ZPOS(KK)-ZOFFD ALPHAX=90 ALPHAZ=-1*(ANGLEORI*(II-1)+0  192
C          .5*ANGLEORI+RADD_ANGLEMIN(KK)) <===                            192
               CALL AGSRESET                                              192
               AG_MOTHER = 'DTUH'                                         192
               AG_X = -1*XCABLE                                           192
               AG_Y = YCABLE                                              192
               AG_Z = RADD_ZPOS(KK)-ZOFFD                                 192
               AG_ALPHAN=AG_ALPHAN+1                                      192
               AG_ALPHAK(AG_ALPHAN)=1                                     192
               AG_ALPHAL(AG_ALPHAN)=90                                    192
               AG_ALPHAN=AG_ALPHAN+1                                      192
               AG_ALPHAK(AG_ALPHAN)=3                                     192
               AG_ALPHAL(AG_ALPHAN)=                                      192
     *         -1*(ANGLEORI*(II-1)+0.5*ANGLEORI+RADD_ANGLEMIN(KK))        192
               AG_TITLE = 'TUBA'                                          192
               AG_EXNAME = 'POSITION'                                     192
               AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX_ALPHAZ'                  192
               CALL AXPOSITION                                            192
C          *                                                              192
            GO TO 5041                                                    193
5042        CONTINUE                                                      193
         GO TO 5031                                                       194
5032     CONTINUE                                                         194
C    Create APTO :                                                        195
C    Call To Local Procedure APTO                                         195
         ASSIGN 5080 TO BLOCK_APTO                                        195
         GO TO  5070                                                      195
5080     CONTINUE                                                         195
C    *                                                                    195
C    Geant POSITION : APTO ===> X=-0.8-THK/2. Y=5.1887 Z=0+ZOFFDM <===    196
         CALL AGSRESET                                                    196
         AG_X = -0.8-THK/2.                                               196
         AG_Y = 5.1887                                                    196
         AG_Z = 0+ZOFFDM                                                  196
         AG_TITLE = 'APTO'                                                196
         AG_EXNAME = 'POSITION'                                           196
         AG_PARLIST = 'X_Y_Z'                                             196
         CALL AXPOSITION                                                  196
C    *                                                                    196
C    Create UCTO :                                                        197
C    Call To Local Procedure UCTO                                         197
         ASSIGN 5100 TO BLOCK_UCTO                                        197
         GO TO  5090                                                      197
5100     CONTINUE                                                         197
C    *                                                                    197
C    Geant POSITION : UCTO ===> X=-1.22 Y=7.0756 Z=0+ZOFFDM <===          198
         CALL AGSRESET                                                    198
         AG_X = -1.22                                                     198
         AG_Y = 7.0756                                                    198
         AG_Z = 0+ZOFFDM                                                  198
         AG_TITLE = 'UCTO'                                                198
         AG_EXNAME = 'POSITION'                                           198
         AG_PARLIST = 'X_Y_Z'                                             198
         CALL AXPOSITION                                                  198
C    *                                                                    198
C    Create OUCY :                                                        199
C    Call To Local Procedure OUCY                                         199
         ASSIGN 5120 TO BLOCK_OUCY                                        199
         GO TO  5110                                                      199
5120     CONTINUE                                                         199
C    *                                                                    199
C    Geant POSITION : OUCY ===> X=0 Y=0 Z=0+ZOFFDM KONLY='ONLY' <===      200
         CALL AGSRESET                                                    200
         AG_X = 0                                                         200
         AG_Y = 0                                                         200
         AG_Z = 0+ZOFFDM                                                  200
         AG_KONLY = 'ONLY'                                                200
         AG_TITLE = 'OUCY'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'X_Y_Z_KONLY'                                       200
         CALL AXPOSITION                                                  200
C    *                                                                    200
C    Create DCBO :                                                        201
C    Call To Local Procedure DCBO                                         201
         ASSIGN 5140 TO BLOCK_DCBO                                        201
         GO TO  5130                                                      201
5140     CONTINUE                                                         201
C    *                                                                    201
C    Geant POSITION : DCBO ===> X=-1.22 Y=-7.0756 Z=0+ZOFFDM <===         202
         CALL AGSRESET                                                    202
         AG_X = -1.22                                                     202
         AG_Y = -7.0756                                                   202
         AG_Z = 0+ZOFFDM                                                  202
         AG_TITLE = 'DCBO'                                                202
         AG_EXNAME = 'POSITION'                                           202
         AG_PARLIST = 'X_Y_Z'                                             202
         CALL AXPOSITION                                                  202
C    *                                                                    202
C    Create APBM :                                                        203
C    Call To Local Procedure APBM                                         203
         ASSIGN 5160 TO BLOCK_APBM                                        203
         GO TO  5150                                                      203
5160     CONTINUE                                                         203
C    *                                                                    203
C    Geant POSITION : APBM ===> X=-.8-THK/2. Y=-5.1887 Z=0+ZOFFDM <===    204
         CALL AGSRESET                                                    204
         AG_X = -.8-THK/2.                                                204
         AG_Y = -5.1887                                                   204
         AG_Z = 0+ZOFFDM                                                  204
         AG_TITLE = 'APBM'                                                204
         AG_EXNAME = 'POSITION'                                           204
         AG_PARLIST = 'X_Y_Z'                                             204
         CALL AXPOSITION                                                  204
C    *                                                                    204
C    Create UCBO :                                                        205
C    Call To Local Procedure UCBO                                         205
         ASSIGN 5180 TO BLOCK_UCBO                                        205
         GO TO  5170                                                      205
5180     CONTINUE                                                         205
C    *                                                                    205
C    Geant POSITION : UCBO ===> X=-1.22 Y=3.3018 Z=0+ZOFFDM <===          206
         CALL AGSRESET                                                    206
         AG_X = -1.22                                                     206
         AG_Y = 3.3018                                                    206
         AG_Z = 0+ZOFFDM                                                  206
         AG_TITLE = 'UCBO'                                                206
         AG_EXNAME = 'POSITION'                                           206
         AG_PARLIST = 'X_Y_Z'                                             206
         CALL AXPOSITION                                                  206
C    *                                                                    206
C    Create DCTO :                                                        207
C    Call To Local Procedure DCTO                                         207
         ASSIGN 5200 TO BLOCK_DCTO                                        207
         GO TO  5190                                                      207
5200     CONTINUE                                                         207
C    *                                                                    207
C    Geant POSITION : DCTO ===> X=-1.22 Y=-3.3018 Z=0+ZOFFDM <===         208
         CALL AGSRESET                                                    208
         AG_X = -1.22                                                     208
         AG_Y = -3.3018                                                   208
         AG_Z = 0+ZOFFDM                                                  208
         AG_TITLE = 'DCTO'                                                208
         AG_EXNAME = 'POSITION'                                           208
         AG_PARLIST = 'X_Y_Z'                                             208
         CALL AXPOSITION                                                  208
C    *                                                                    208
C    Create INCY :                                                        209
C    Call To Local Procedure INCY                                         209
         ASSIGN 5220 TO BLOCK_INCY                                        209
         GO TO  5210                                                      209
5220     CONTINUE                                                         209
C    *                                                                    209
C    Geant POSITION : INCY ===> X=0 Y=0 Z=0+ZOFFDM <===                   210
         CALL AGSRESET                                                    210
         AG_X = 0                                                         210
         AG_Y = 0                                                         210
         AG_Z = 0+ZOFFDM                                                  210
         AG_TITLE = 'INCY'                                                210
         AG_EXNAME = 'POSITION'                                           210
         AG_PARLIST = 'X_Y_Z'                                             210
         CALL AXPOSITION                                                  210
C    *                                                                    210
C    Create CYTR :                                                        211
C    Call To Local Procedure CYTR                                         211
         ASSIGN 5240 TO BLOCK_CYTR                                        211
         GO TO  5230                                                      211
5240     CONTINUE                                                         211
C    *                                                                    211
C    Geant POSITION : CYTR ===> X=0 Y=0 Z=0+ZOFFDM <===                   212
         CALL AGSRESET                                                    212
         AG_X = 0                                                         212
         AG_Y = 0                                                         212
         AG_Z = 0+ZOFFDM                                                  212
         AG_TITLE = 'CYTR'                                                212
         AG_EXNAME = 'POSITION'                                           212
         AG_PARLIST = 'X_Y_Z'                                             212
         CALL AXPOSITION                                                  212
C    *                                                                    212
C    Create APTE :                                                        213
C    Call To Local Procedure APTE                                         213
         ASSIGN 5260 TO BLOCK_APTE                                        213
         GO TO  5250                                                      213
5260     CONTINUE                                                         213
C    *                                                                    213
C    Geant POSITION : APTE ===> X=-0.8-THK/2. Y=4.9624 Z=DSEA/2. + DSEB   214
C    + DSEC/2.+ZOFFDM <===                                                214
         CALL AGSRESET                                                    214
         AG_X = -0.8-THK/2.                                               214
         AG_Y = 4.9624                                                    214
         AG_Z = DSEA/2. + DSEB + DSEC/2.+ZOFFDM                           214
         AG_TITLE = 'APTE'                                                214
         AG_EXNAME = 'POSITION'                                           214
         AG_PARLIST = 'X_Y_Z'                                             214
         CALL AXPOSITION                                                  214
C    *                                                                    214
C    Create UCTE :                                                        215
C    Call To Local Procedure UCTE                                         215
         ASSIGN 5280 TO BLOCK_UCTE                                        215
         GO TO  5270                                                      215
5280     CONTINUE                                                         215
C    *                                                                    215
C    Geant POSITION : UCTE ===> X=-1.22 Y=7.0756 Z=DSEA/2. + DSEB + DSEC  216
C    /2.+ZOFFDM <===                                                      216
         CALL AGSRESET                                                    216
         AG_X = -1.22                                                     216
         AG_Y = 7.0756                                                    216
         AG_Z = DSEA/2. + DSEB + DSEC/2.+ZOFFDM                           216
         AG_TITLE = 'UCTE'                                                216
         AG_EXNAME = 'POSITION'                                           216
         AG_PARLIST = 'X_Y_Z'                                             216
         CALL AXPOSITION                                                  216
C    *                                                                    216
C    Create APBE :                                                        217
C    Call To Local Procedure APBE                                         217
         ASSIGN 5300 TO BLOCK_APBE                                        217
         GO TO  5290                                                      217
5300     CONTINUE                                                         217
C    *                                                                    217
C    Geant POSITION : APBE ===> X=-0.8-THK/2. Y=-4.9624 Z=DSEA/2. + DSEB  218
C     + DSEC/2.+ZOFFDM <===                                               218
         CALL AGSRESET                                                    218
         AG_X = -0.8-THK/2.                                               218
         AG_Y = -4.9624                                                   218
         AG_Z = DSEA/2. + DSEB + DSEC/2.+ZOFFDM                           218
         AG_TITLE = 'APBE'                                                218
         AG_EXNAME = 'POSITION'                                           218
         AG_PARLIST = 'X_Y_Z'                                             218
         CALL AXPOSITION                                                  218
C    *                                                                    218
C    Create DCBE :                                                        219
C    Call To Local Procedure DCBE                                         219
         ASSIGN 5320 TO BLOCK_DCBE                                        219
         GO TO  5310                                                      219
5320     CONTINUE                                                         219
C    *                                                                    219
C    Geant POSITION : DCBE ===> X=-1.22 Y=-7.0756 Z=DSEA/2. + DSEB + DSE  220
C    C/2.+ZOFFDM <===                                                     220
         CALL AGSRESET                                                    220
         AG_X = -1.22                                                     220
         AG_Y = -7.0756                                                   220
         AG_Z = DSEA/2. + DSEB + DSEC/2.+ZOFFDM                           220
         AG_TITLE = 'DCBE'                                                220
         AG_EXNAME = 'POSITION'                                           220
         AG_PARLIST = 'X_Y_Z'                                             220
         CALL AXPOSITION                                                  220
C    *                                                                    220
C    Create OUCE :                                                        221
C    Call To Local Procedure OUCE                                         221
         ASSIGN 5340 TO BLOCK_OUCE                                        221
         GO TO  5330                                                      221
5340     CONTINUE                                                         221
C    *                                                                    221
C    Geant POSITION : OUCE ===> X=0 Y=0 Z=DSEA/2. + DSEB + DSEC/2.+ZOFFD  222
C    M <===                                                               222
         CALL AGSRESET                                                    222
         AG_X = 0                                                         222
         AG_Y = 0                                                         222
         AG_Z = DSEA/2. + DSEB + DSEC/2.+ZOFFDM                           222
         AG_TITLE = 'OUCE'                                                222
         AG_EXNAME = 'POSITION'                                           222
         AG_PARLIST = 'X_Y_Z'                                             222
         CALL AXPOSITION                                                  222
C    *                                                                    222
C    Create OUCI :                                                        223
C    Call To Local Procedure OUCI                                         223
         ASSIGN 5360 TO BLOCK_OUCI                                        223
         GO TO  5350                                                      223
5360     CONTINUE                                                         223
C    *                                                                    223
C    Geant POSITION : OUCI ===> X=0 Y=0 Z=DSEA/2. + DSEB/2.+ZOFFDM <===   224
         CALL AGSRESET                                                    224
         AG_X = 0                                                         224
         AG_Y = 0                                                         224
         AG_Z = DSEA/2. + DSEB/2.+ZOFFDM                                  224
         AG_TITLE = 'OUCI'                                                224
         AG_EXNAME = 'POSITION'                                           224
         AG_PARLIST = 'X_Y_Z'                                             224
         CALL AXPOSITION                                                  224
C    *                                                                    224
C    Create UCTI :                                                        225
C    Call To Local Procedure UCTI                                         225
         ASSIGN 5380 TO BLOCK_UCTI                                        225
         GO TO  5370                                                      225
5380     CONTINUE                                                         225
C    *                                                                    225
C    Geant POSITION : UCTI ===> X=-1.22 Y=7.0756 Z=DSEA/2. + DSEB/2.+ZOF  226
C    FDM <===                                                             226
         CALL AGSRESET                                                    226
         AG_X = -1.22                                                     226
         AG_Y = 7.0756                                                    226
         AG_Z = DSEA/2. + DSEB/2.+ZOFFDM                                  226
         AG_TITLE = 'UCTI'                                                226
         AG_EXNAME = 'POSITION'                                           226
         AG_PARLIST = 'X_Y_Z'                                             226
         CALL AXPOSITION                                                  226
C    *                                                                    226
C    Create DCBI :                                                        227
C    Call To Local Procedure DCBI                                         227
         ASSIGN 5400 TO BLOCK_DCBI                                        227
         GO TO  5390                                                      227
5400     CONTINUE                                                         227
C    *                                                                    227
C    Geant POSITION : DCBI ===> X=-1.22 Y=-7.0756 Z=DSEA/2. + DSEB/2.+ZO  228
C    FFDM <===                                                            228
         CALL AGSRESET                                                    228
         AG_X = -1.22                                                     228
         AG_Y = -7.0756                                                   228
         AG_Z = DSEA/2. + DSEB/2.+ZOFFDM                                  228
         AG_TITLE = 'DCBI'                                                228
         AG_EXNAME = 'POSITION'                                           228
         AG_PARLIST = 'X_Y_Z'                                             228
         CALL AXPOSITION                                                  228
C    *                                                                    228
C    Create ATTI :                                                        229
C    Call To Local Procedure ATTI                                         229
         ASSIGN 5420 TO BLOCK_ATTI                                        229
         GO TO  5410                                                      229
5420     CONTINUE                                                         229
C    *                                                                    229
C    Geant POSITION : ATTI ===> X=-0.8-THK/2. Y=5.0705 Z=15.0806+ZOFFDM   231
C    ALPHAY=90 ALPHAX=-90 ALPHAZ=180 <===                                 231
         CALL AGSRESET                                                    231
         AG_X = -0.8-THK/2.                                               231
         AG_Y = 5.0705                                                    231
         AG_Z = 15.0806+ZOFFDM                                            231
         AG_ALPHAN=AG_ALPHAN+1                                            231
         AG_ALPHAK(AG_ALPHAN)=2                                           231
         AG_ALPHAL(AG_ALPHAN)=90                                          231
         AG_ALPHAN=AG_ALPHAN+1                                            231
         AG_ALPHAK(AG_ALPHAN)=1                                           231
         AG_ALPHAL(AG_ALPHAN)=-90                                         231
         AG_ALPHAN=AG_ALPHAN+1                                            231
         AG_ALPHAK(AG_ALPHAN)=3                                           231
         AG_ALPHAL(AG_ALPHAN)=180                                         231
         AG_TITLE = 'ATTI'                                                231
         AG_EXNAME = 'POSITION'                                           231
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAX_ALPHAZ'                        231
         CALL AXPOSITION                                                  231
C    *                                                                    231
C    Create ATBI :                                                        232
C    Call To Local Procedure ATBI                                         232
         ASSIGN 5440 TO BLOCK_ATBI                                        232
         GO TO  5430                                                      232
5440     CONTINUE                                                         232
C    *                                                                    232
C    Geant POSITION : ATBI ===> X=-0.8-THK/2. Y=-5.0705 Z=15.0806+ZOFFDM  233
C     ALPHAY=90 ALPHAX=-90 <===                                           233
         CALL AGSRESET                                                    233
         AG_X = -0.8-THK/2.                                               233
         AG_Y = -5.0705                                                   233
         AG_Z = 15.0806+ZOFFDM                                            233
         AG_ALPHAN=AG_ALPHAN+1                                            233
         AG_ALPHAK(AG_ALPHAN)=2                                           233
         AG_ALPHAL(AG_ALPHAN)=90                                          233
         AG_ALPHAN=AG_ALPHAN+1                                            233
         AG_ALPHAK(AG_ALPHAN)=1                                           233
         AG_ALPHAL(AG_ALPHAN)=-90                                         233
         AG_TITLE = 'ATBI'                                                233
         AG_EXNAME = 'POSITION'                                           233
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAX'                               233
         CALL AXPOSITION                                                  233
C    *                                                                    233
C    Create DCTH :                                                        234
C    Call To Local Procedure DCTH                                         234
         ASSIGN 5460 TO BLOCK_DCTH                                        234
         GO TO  5450                                                      234
5460     CONTINUE                                                         234
C    *                                                                    234
C    Geant POSITION : DCTH ===> X=-0.8-THK/2.0+THK/2.-0.32+0.02 Y=2.4967  236
C    +THK/2.+0.32-0.02 Z=DSEA/2.+DSEB+(DSEC+DSED)/2.+ZOFFDM <===          236
         CALL AGSRESET                                                    236
         AG_X = -0.8-THK/2.0+THK/2.-0.32+0.02                             236
         AG_Y = 2.4967+THK/2.+0.32-0.02                                   236
         AG_Z = DSEA/2.+DSEB+(DSEC+DSED)/2.+ZOFFDM                        236
         AG_TITLE = 'DCTH'                                                236
         AG_EXNAME = 'POSITION'                                           236
         AG_PARLIST = 'X_Y_Z'                                             236
         CALL AXPOSITION                                                  236
C    *                                                                    236
C    Create TCTH :                                                        237
C    Call To Local Procedure TCTH                                         237
         ASSIGN 5480 TO BLOCK_TCTH                                        237
         GO TO  5470                                                      237
5480     CONTINUE                                                         237
C    *                                                                    237
C    Geant POSITION : TCTH ===> X=-0.8-THK/2.0+THK/2.-0.32+0.02 Y=-1*(2.  239
C    4967+THK/2.+0.32-0.02) Z=DSEA/2.+DSEB+(DSEC+DSED)/2.+ZOFFDM <===     239
         CALL AGSRESET                                                    239
         AG_X = -0.8-THK/2.0+THK/2.-0.32+0.02                             239
         AG_Y = -1*(2.4967+THK/2.+0.32-0.02)                              239
         AG_Z = DSEA/2.+DSEB+(DSEC+DSED)/2.+ZOFFDM                        239
         AG_TITLE = 'TCTH'                                                239
         AG_EXNAME = 'POSITION'                                           239
         AG_PARLIST = 'X_Y_Z'                                             239
         CALL AXPOSITION                                                  239
C    *                                                                    239
C    Create CSTI :                                                        240
C    Call To Local Procedure CSTI                                         240
         ASSIGN 5500 TO BLOCK_CSTI                                        240
         GO TO  5490                                                      240
5500     CONTINUE                                                         240
C    *                                                                    240
C    Geant POSITION : CSTI ===> X=-.8-.2*THK-.32+.02 Y=-1*(2.4967+2*THK+  242
C    .4) Z=DSEA/2.+DSEB/2.+.1+ZOFFDM ALPHAX=-8.75 ALPHAY=6 <===           242
         CALL AGSRESET                                                    242
         AG_X = -.8-.2*THK-.32+.02                                        242
         AG_Y = -1*(2.4967+2*THK+.4)                                      242
         AG_Z = DSEA/2.+DSEB/2.+.1+ZOFFDM                                 242
         AG_ALPHAN=AG_ALPHAN+1                                            242
         AG_ALPHAK(AG_ALPHAN)=1                                           242
         AG_ALPHAL(AG_ALPHAN)=-8.75                                       242
         AG_ALPHAN=AG_ALPHAN+1                                            242
         AG_ALPHAK(AG_ALPHAN)=2                                           242
         AG_ALPHAL(AG_ALPHAN)=6                                           242
         AG_TITLE = 'CSTI'                                                242
         AG_EXNAME = 'POSITION'                                           242
         AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAY'                               242
         CALL AXPOSITION                                                  242
C    *                                                                    242
C    Create CSBI :                                                        243
C    Call To Local Procedure CSBI                                         243
         ASSIGN 5520 TO BLOCK_CSBI                                        243
         GO TO  5510                                                      243
5520     CONTINUE                                                         243
C    *                                                                    243
C    Geant POSITION : CSBI ===> X=-.8-.2*THK-.32+.02 Y=(2.4967+2*THK+.4)  245
C     Z=DSEA/2.+DSEB/2.+.1+ZOFFDM ALPHAX=8.75 ALPHAY=6 <===               245
         CALL AGSRESET                                                    245
         AG_X = -.8-.2*THK-.32+.02                                        245
         AG_Y = (2.4967+2*THK+.4)                                         245
         AG_Z = DSEA/2.+DSEB/2.+.1+ZOFFDM                                 245
         AG_ALPHAN=AG_ALPHAN+1                                            245
         AG_ALPHAK(AG_ALPHAN)=1                                           245
         AG_ALPHAL(AG_ALPHAN)=8.75                                        245
         AG_ALPHAN=AG_ALPHAN+1                                            245
         AG_ALPHAK(AG_ALPHAN)=2                                           245
         AG_ALPHAL(AG_ALPHAN)=6                                           245
         AG_TITLE = 'CSBI'                                                245
         AG_EXNAME = 'POSITION'                                           245
         AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAY'                               245
         CALL AXPOSITION                                                  245
C    *                                                                    245
C    Create APET :                                                        246
C    Call To Local Procedure APET                                         246
         ASSIGN 5540 TO BLOCK_APET                                        246
         GO TO  5530                                                      246
5540     CONTINUE                                                         246
C    *                                                                    246
C    Geant POSITION : APET ===> X=-0.8-THK/2. Y=4.9624 Z=DSEA/2.+DSEB+DS  247
C    EC+DSED/2.+ZOFFDM <===                                               247
         CALL AGSRESET                                                    247
         AG_X = -0.8-THK/2.                                               247
         AG_Y = 4.9624                                                    247
         AG_Z = DSEA/2.+DSEB+DSEC+DSED/2.+ZOFFDM                          247
         AG_TITLE = 'APET'                                                247
         AG_EXNAME = 'POSITION'                                           247
         AG_PARLIST = 'X_Y_Z'                                             247
         CALL AXPOSITION                                                  247
C    *                                                                    247
C    Create APEB :                                                        248
C    Call To Local Procedure APEB                                         248
         ASSIGN 5560 TO BLOCK_APEB                                        248
         GO TO  5550                                                      248
5560     CONTINUE                                                         248
C    *                                                                    248
C    Geant POSITION : APEB ===> X=-0.8-THK/2. Y=-4.9624 Z=DSEA/2.+DSEB+D  249
C    SEC+DSED/2.+ZOFFDM <===                                              249
         CALL AGSRESET                                                    249
         AG_X = -0.8-THK/2.                                               249
         AG_Y = -4.9624                                                   249
         AG_Z = DSEA/2.+DSEB+DSEC+DSED/2.+ZOFFDM                          249
         AG_TITLE = 'APEB'                                                249
         AG_EXNAME = 'POSITION'                                           249
         AG_PARLIST = 'X_Y_Z'                                             249
         CALL AXPOSITION                                                  249
C    *                                                                    249
C    Create OCYE :                                                        250
C    Call To Local Procedure OCYE                                         250
         ASSIGN 5580 TO BLOCK_OCYE                                        250
         GO TO  5570                                                      250
5580     CONTINUE                                                         250
C    *                                                                    250
C    Geant POSITION : OCYE ===> X=0 Y=0 Z=DSEA/2.+DSEB+DSEC+(DSED-0.475)  251
C    /2.+ZOFFDM <===                                                      251
         CALL AGSRESET                                                    251
         AG_X = 0                                                         251
         AG_Y = 0                                                         251
         AG_Z = DSEA/2.+DSEB+DSEC+(DSED-0.475)/2.+ZOFFDM                  251
         AG_TITLE = 'OCYE'                                                251
         AG_EXNAME = 'POSITION'                                           251
         AG_PARLIST = 'X_Y_Z'                                             251
         CALL AXPOSITION                                                  251
C    *                                                                    251
C    Create OCEA :                                                        252
C    Call To Local Procedure OCEA                                         252
         ASSIGN 5600 TO BLOCK_OCEA                                        252
         GO TO  5590                                                      252
5600     CONTINUE                                                         252
C    *                                                                    252
C    Geant POSITION : OCEA ===> X=0 Y=0 Z=DSEA/2.+DSEB+DSEC+DSED-0.475/2  253
C    .+ZOFFDM <===                                                        253
         CALL AGSRESET                                                    253
         AG_X = 0                                                         253
         AG_Y = 0                                                         253
         AG_Z = DSEA/2.+DSEB+DSEC+DSED-0.475/2.+ZOFFDM                    253
         AG_TITLE = 'OCEA'                                                253
         AG_EXNAME = 'POSITION'                                           253
         AG_PARLIST = 'X_Y_Z'                                             253
         CALL AXPOSITION                                                  253
C    *                                                                    253
C    Create OCEB :                                                        254
C    Call To Local Procedure OCEB                                         254
         ASSIGN 5620 TO BLOCK_OCEB                                        254
         GO TO  5610                                                      254
5620     CONTINUE                                                         254
C    *                                                                    254
C    Geant POSITION : OCEB ===> X=0 Y=0 Z=DSEA/2.+DSEB+DSEC+DSED-0.475/2  255
C    .+ZOFFDM <===                                                        255
         CALL AGSRESET                                                    255
         AG_X = 0                                                         255
         AG_Y = 0                                                         255
         AG_Z = DSEA/2.+DSEB+DSEC+DSED-0.475/2.+ZOFFDM                    255
         AG_TITLE = 'OCEB'                                                255
         AG_EXNAME = 'POSITION'                                           255
         AG_PARLIST = 'X_Y_Z'                                             255
         CALL AXPOSITION                                                  255
C    *                                                                    255
C    Create OCEC :                                                        256
C    Call To Local Procedure OCEC                                         256
         ASSIGN 5640 TO BLOCK_OCEC                                        256
         GO TO  5630                                                      256
5640     CONTINUE                                                         256
C    *                                                                    256
C    Geant POSITION : OCEC ===> X=0 Y=0 Z=DSEA/2.+DSEB+DSEC+DSED-0.475/2  257
C    .+ZOFFDM <===                                                        257
         CALL AGSRESET                                                    257
         AG_X = 0                                                         257
         AG_Y = 0                                                         257
         AG_Z = DSEA/2.+DSEB+DSEC+DSED-0.475/2.+ZOFFDM                    257
         AG_TITLE = 'OCEC'                                                257
         AG_EXNAME = 'POSITION'                                           257
         AG_PARLIST = 'X_Y_Z'                                             257
         CALL AXPOSITION                                                  257
C    *                                                                    257
C    Create OCED :                                                        258
C    Call To Local Procedure OCED                                         258
         ASSIGN 5660 TO BLOCK_OCED                                        258
         GO TO  5650                                                      258
5660     CONTINUE                                                         258
C    *                                                                    258
C    Geant POSITION : OCED ===> X=0 Y=0 Z=DSEA/2.+DSEB+DSEC+DSED-0.475/2  259
C    .+ZOFFDM <===                                                        259
         CALL AGSRESET                                                    259
         AG_X = 0                                                         259
         AG_Y = 0                                                         259
         AG_Z = DSEA/2.+DSEB+DSEC+DSED-0.475/2.+ZOFFDM                    259
         AG_TITLE = 'OCED'                                                259
         AG_EXNAME = 'POSITION'                                           259
         AG_PARLIST = 'X_Y_Z'                                             259
         CALL AXPOSITION                                                  259
C    *                                                                    259
C    Create UPTE :                                                        260
C    Call To Local Procedure UPTE                                         260
         ASSIGN 5680 TO BLOCK_UPTE                                        260
         GO TO  5670                                                      260
5680     CONTINUE                                                         260
C    *                                                                    260
C    Geant POSITION : UPTE ===> X=-1.22 Y=7.0756 Z=DSEA/2.+DSEB+DSEC+DSE  261
C    D/2.+ZOFFDM <===                                                     261
         CALL AGSRESET                                                    261
         AG_X = -1.22                                                     261
         AG_Y = 7.0756                                                    261
         AG_Z = DSEA/2.+DSEB+DSEC+DSED/2.+ZOFFDM                          261
         AG_TITLE = 'UPTE'                                                261
         AG_EXNAME = 'POSITION'                                           261
         AG_PARLIST = 'X_Y_Z'                                             261
         CALL AXPOSITION                                                  261
C    *                                                                    261
C    Create BOTE :                                                        262
C    Call To Local Procedure BOTE                                         262
         ASSIGN 5700 TO BLOCK_BOTE                                        262
         GO TO  5690                                                      262
5700     CONTINUE                                                         262
C    *                                                                    262
C    Geant POSITION : BOTE ===> X=-1.22 Y=-7.0756 Z=DSEA/2.+DSEB+DSEC+DS  263
C    ED/2.+ZOFFDM <===                                                    263
         CALL AGSRESET                                                    263
         AG_X = -1.22                                                     263
         AG_Y = -7.0756                                                   263
         AG_Z = DSEA/2.+DSEB+DSEC+DSED/2.+ZOFFDM                          263
         AG_TITLE = 'BOTE'                                                263
         AG_EXNAME = 'POSITION'                                           263
         AG_PARLIST = 'X_Y_Z'                                             263
         CALL AXPOSITION                                                  263
C    *                                                                    263
C    Create LSAA :                                                        264
C    Call To Local Procedure LSAA                                         264
         ASSIGN 5720 TO BLOCK_LSAA                                        264
         GO TO  5710                                                      264
5720     CONTINUE                                                         264
C    *                                                                    264
C    Geant POSITION : LSAA ===> X=-4.8024 Y=6.5352 Z=-35.9334-ZOFFD <===  265
         CALL AGSRESET                                                    265
         AG_X = -4.8024                                                   265
         AG_Y = 6.5352                                                    265
         AG_Z = -35.9334-ZOFFD                                            265
         AG_TITLE = 'LSAA'                                                265
         AG_EXNAME = 'POSITION'                                           265
         AG_PARLIST = 'X_Y_Z'                                             265
         CALL AXPOSITION                                                  265
C    *                                                                    265
C    Create LSAB :                                                        266
C    Call To Local Procedure LSAB                                         266
         ASSIGN 5740 TO BLOCK_LSAB                                        266
         GO TO  5730                                                      266
5740     CONTINUE                                                         266
C    *                                                                    266
C    Geant POSITION : LSAB ===> X=-5.4216 Y=6.0305 Z=-36.2522-ZOFFD <===  267
         CALL AGSRESET                                                    267
         AG_X = -5.4216                                                   267
         AG_Y = 6.0305                                                    267
         AG_Z = -36.2522-ZOFFD                                            267
         AG_TITLE = 'LSAB'                                                267
         AG_EXNAME = 'POSITION'                                           267
         AG_PARLIST = 'X_Y_Z'                                             267
         CALL AXPOSITION                                                  267
C    *                                                                    267
C    Create LSAC :                                                        268
C    Call To Local Procedure LSAC                                         268
         ASSIGN 5760 TO BLOCK_LSAC                                        268
         GO TO  5750                                                      268
5760     CONTINUE                                                         268
C    *                                                                    268
C    Geant POSITION : LSAC ===> X=-5.9902 Y=5.4672 Z=-36.5991-ZOFFD <===  269
         CALL AGSRESET                                                    269
         AG_X = -5.9902                                                   269
         AG_Y = 5.4672                                                    269
         AG_Z = -36.5991-ZOFFD                                            269
         AG_TITLE = 'LSAC'                                                269
         AG_EXNAME = 'POSITION'                                           269
         AG_PARLIST = 'X_Y_Z'                                             269
         CALL AXPOSITION                                                  269
C    *                                                                    269
C    Create LSAD :                                                        270
C    Call To Local Procedure LSAD                                         270
         ASSIGN 5780 TO BLOCK_LSAD                                        270
         GO TO  5770                                                      270
5780     CONTINUE                                                         270
C    *                                                                    270
C    Geant POSITION : LSAD ===> X=-6.4995 Y=4.8506 Z=-36.9485-ZOFFD <===  271
         CALL AGSRESET                                                    271
         AG_X = -6.4995                                                   271
         AG_Y = 4.8506                                                    271
         AG_Z = -36.9485-ZOFFD                                            271
         AG_TITLE = 'LSAD'                                                271
         AG_EXNAME = 'POSITION'                                           271
         AG_PARLIST = 'X_Y_Z'                                             271
         CALL AXPOSITION                                                  271
C    *                                                                    271
C    Create LSAE :                                                        272
C    Call To Local Procedure LSAE                                         272
         ASSIGN 5800 TO BLOCK_LSAE                                        272
         GO TO  5790                                                      272
5800     CONTINUE                                                         272
C    *                                                                    272
C    Geant POSITION : LSAE ===> X=-6.9456 Y=4.187 Z=-37.2614-ZOFFD <===   273
         CALL AGSRESET                                                    273
         AG_X = -6.9456                                                   273
         AG_Y = 4.187                                                     273
         AG_Z = -37.2614-ZOFFD                                            273
         AG_TITLE = 'LSAE'                                                273
         AG_EXNAME = 'POSITION'                                           273
         AG_PARLIST = 'X_Y_Z'                                             273
         CALL AXPOSITION                                                  273
C    *                                                                    273
C    Create LSAF :                                                        274
C    Call To Local Procedure LSAF                                         274
         ASSIGN 5820 TO BLOCK_LSAF                                        274
         GO TO  5810                                                      274
5820     CONTINUE                                                         274
C    *                                                                    274
C    Geant POSITION : LSAF ===> X=-7.3242 Y=3.4826 Z=-37.57-ZOFFD <===    275
         CALL AGSRESET                                                    275
         AG_X = -7.3242                                                   275
         AG_Y = 3.4826                                                    275
         AG_Z = -37.57-ZOFFD                                              275
         AG_TITLE = 'LSAF'                                                275
         AG_EXNAME = 'POSITION'                                           275
         AG_PARLIST = 'X_Y_Z'                                             275
         CALL AXPOSITION                                                  275
C    *                                                                    275
C    Create LSAG :                                                        276
C    Call To Local Procedure LSAG                                         276
         ASSIGN 5840 TO BLOCK_LSAG                                        276
         GO TO  5830                                                      276
5840     CONTINUE                                                         276
C    *                                                                    276
C    Geant POSITION : LSAG ===> X=-7.6316 Y=2.7443 Z=-37.9149-ZOFFD <===  277
         CALL AGSRESET                                                    277
         AG_X = -7.6316                                                   277
         AG_Y = 2.7443                                                    277
         AG_Z = -37.9149-ZOFFD                                            277
         AG_TITLE = 'LSAG'                                                277
         AG_EXNAME = 'POSITION'                                           277
         AG_PARLIST = 'X_Y_Z'                                             277
         CALL AXPOSITION                                                  277
C    *                                                                    277
C    Create LSAH :                                                        278
C    Call To Local Procedure LSAH                                         278
         ASSIGN 5860 TO BLOCK_LSAH                                        278
         GO TO  5850                                                      278
5860     CONTINUE                                                         278
C    *                                                                    278
C    Geant POSITION : LSAH ===> X=-7.8647 Y=1.9794 Z=-38.2654-ZOFFD <===  279
         CALL AGSRESET                                                    279
         AG_X = -7.8647                                                   279
         AG_Y = 1.9794                                                    279
         AG_Z = -38.2654-ZOFFD                                            279
         AG_TITLE = 'LSAH'                                                279
         AG_EXNAME = 'POSITION'                                           279
         AG_PARLIST = 'X_Y_Z'                                             279
         CALL AXPOSITION                                                  279
C    *                                                                    279
C    Create LSAI :                                                        280
C    Call To Local Procedure LSAI                                         280
         ASSIGN 5880 TO BLOCK_LSAI                                        280
         GO TO  5870                                                      280
5880     CONTINUE                                                         280
C    *                                                                    280
C    Geant POSITION : LSAI ===> X=-8.0214 Y=1.1952 Z=-38.5812-ZOFFD <===  281
         CALL AGSRESET                                                    281
         AG_X = -8.0214                                                   281
         AG_Y = 1.1952                                                    281
         AG_Z = -38.5812-ZOFFD                                            281
         AG_TITLE = 'LSAI'                                                281
         AG_EXNAME = 'POSITION'                                           281
         AG_PARLIST = 'X_Y_Z'                                             281
         CALL AXPOSITION                                                  281
C    *                                                                    281
C    Create LSAJ :                                                        282
C    Call To Local Procedure LSAJ                                         282
         ASSIGN 5900 TO BLOCK_LSAJ                                        282
         GO TO  5890                                                      282
5900     CONTINUE                                                         282
C    *                                                                    282
C    Geant POSITION : LSAJ ===> X=-8.1002 Y=0.3995 Z=-38.9488-ZOFFD <===  283
         CALL AGSRESET                                                    283
         AG_X = -8.1002                                                   283
         AG_Y = 0.3995                                                    283
         AG_Z = -38.9488-ZOFFD                                            283
         AG_TITLE = 'LSAJ'                                                283
         AG_EXNAME = 'POSITION'                                           283
         AG_PARLIST = 'X_Y_Z'                                             283
         CALL AXPOSITION                                                  283
C    *                                                                    283
C    Create LSAK :                                                        284
C    Call To Local Procedure LSAK                                         284
         ASSIGN 5920 TO BLOCK_LSAK                                        284
         GO TO  5910                                                      284
5920     CONTINUE                                                         284
C    *                                                                    284
C    Geant POSITION : LSAK ===> X=-8.1001 Y=-0.4002 Z=-39.287-ZOFFD <===  285
         CALL AGSRESET                                                    285
         AG_X = -8.1001                                                   285
         AG_Y = -0.4002                                                   285
         AG_Z = -39.287-ZOFFD                                             285
         AG_TITLE = 'LSAK'                                                285
         AG_EXNAME = 'POSITION'                                           285
         AG_PARLIST = 'X_Y_Z'                                             285
         CALL AXPOSITION                                                  285
C    *                                                                    285
C    Create LSAL :                                                        286
C    Call To Local Procedure LSAL                                         286
         ASSIGN 5940 TO BLOCK_LSAL                                        286
         GO TO  5930                                                      286
5940     CONTINUE                                                         286
C    *                                                                    286
C    Geant POSITION : LSAL ===> X=-8.0213 Y=-1.196 Z=-39.6531-ZOFFD <===  287
         CALL AGSRESET                                                    287
         AG_X = -8.0213                                                   287
         AG_Y = -1.196                                                    287
         AG_Z = -39.6531-ZOFFD                                            287
         AG_TITLE = 'LSAL'                                                287
         AG_EXNAME = 'POSITION'                                           287
         AG_PARLIST = 'X_Y_Z'                                             287
         CALL AXPOSITION                                                  287
C    *                                                                    287
C    Create LSAM :                                                        288
C    Call To Local Procedure LSAM                                         288
         ASSIGN 5960 TO BLOCK_LSAM                                        288
         GO TO  5950                                                      288
5960     CONTINUE                                                         288
C    *                                                                    288
C    Geant POSITION : LSAM ===> X=-7.8645 Y=-1.9802 Z=-40.0446-ZOFFD <==  289
C    =                                                                    289
         CALL AGSRESET                                                    289
         AG_X = -7.8645                                                   289
         AG_Y = -1.9802                                                   289
         AG_Z = -40.0446-ZOFFD                                            289
         AG_TITLE = 'LSAM'                                                289
         AG_EXNAME = 'POSITION'                                           289
         AG_PARLIST = 'X_Y_Z'                                             289
         CALL AXPOSITION                                                  289
C    *                                                                    289
C    Create LSAN :                                                        290
C    Call To Local Procedure LSAN                                         290
         ASSIGN 5980 TO BLOCK_LSAN                                        290
         GO TO  5970                                                      290
5980     CONTINUE                                                         290
C    *                                                                    290
C    Geant POSITION : LSAN ===> X=-7.6313 Y=-2.7451 Z=-40.4339-ZOFFD <==  291
C    =                                                                    291
         CALL AGSRESET                                                    291
         AG_X = -7.6313                                                   291
         AG_Y = -2.7451                                                   291
         AG_Z = -40.4339-ZOFFD                                            291
         AG_TITLE = 'LSAN'                                                291
         AG_EXNAME = 'POSITION'                                           291
         AG_PARLIST = 'X_Y_Z'                                             291
         CALL AXPOSITION                                                  291
C    *                                                                    291
C    Create LSAO :                                                        292
C    Call To Local Procedure LSAO                                         292
         ASSIGN 6000 TO BLOCK_LSAO                                        292
         GO TO  5990                                                      292
6000     CONTINUE                                                         292
C    *                                                                    292
C    Geant POSITION : LSAO ===> X=-7.3239 Y=-3.4833 Z=-40.7774-ZOFFD <==  293
C    =                                                                    293
         CALL AGSRESET                                                    293
         AG_X = -7.3239                                                   293
         AG_Y = -3.4833                                                   293
         AG_Z = -40.7774-ZOFFD                                            293
         AG_TITLE = 'LSAO'                                                293
         AG_EXNAME = 'POSITION'                                           293
         AG_PARLIST = 'X_Y_Z'                                             293
         CALL AXPOSITION                                                  293
C    *                                                                    293
C    Create LSAP :                                                        294
C    Call To Local Procedure LSAP                                         294
         ASSIGN 6020 TO BLOCK_LSAP                                        294
         GO TO  6010                                                      294
6020     CONTINUE                                                         294
C    *                                                                    294
C    Geant POSITION : LSAP ===> X=-6.9452 Y=-4.1876 Z=-41.1379-ZOFFD <==  295
C    =                                                                    295
         CALL AGSRESET                                                    295
         AG_X = -6.9452                                                   295
         AG_Y = -4.1876                                                   295
         AG_Z = -41.1379-ZOFFD                                            295
         AG_TITLE = 'LSAP'                                                295
         AG_EXNAME = 'POSITION'                                           295
         AG_PARLIST = 'X_Y_Z'                                             295
         CALL AXPOSITION                                                  295
C    *                                                                    295
C    Create LSAQ :                                                        296
C    Call To Local Procedure LSAQ                                         296
         ASSIGN 6040 TO BLOCK_LSAQ                                        296
         GO TO  6030                                                      296
6040     CONTINUE                                                         296
C    *                                                                    296
C    Geant POSITION : LSAQ ===> X=-6.499 Y=-4.8513 Z=-41.5212-ZOFFD <===  297
         CALL AGSRESET                                                    297
         AG_X = -6.499                                                    297
         AG_Y = -4.8513                                                   297
         AG_Z = -41.5212-ZOFFD                                            297
         AG_TITLE = 'LSAQ'                                                297
         AG_EXNAME = 'POSITION'                                           297
         AG_PARLIST = 'X_Y_Z'                                             297
         CALL AXPOSITION                                                  297
C    *                                                                    297
C    Create LSAR :                                                        298
C    Call To Local Procedure LSAR                                         298
         ASSIGN 6060 TO BLOCK_LSAR                                        298
         GO TO  6050                                                      298
6060     CONTINUE                                                         298
C    *                                                                    298
C    Geant POSITION : LSAR ===> X=-5.9897 Y=-5.4677 Z=-41.8994-ZOFFD <==  299
C    =                                                                    299
         CALL AGSRESET                                                    299
         AG_X = -5.9897                                                   299
         AG_Y = -5.4677                                                   299
         AG_Z = -41.8994-ZOFFD                                            299
         AG_TITLE = 'LSAR'                                                299
         AG_EXNAME = 'POSITION'                                           299
         AG_PARLIST = 'X_Y_Z'                                             299
         CALL AXPOSITION                                                  299
C    *                                                                    299
C    Create LSAS :                                                        300
C    Call To Local Procedure LSAS                                         300
         ASSIGN 6080 TO BLOCK_LSAS                                        300
         GO TO  6070                                                      300
6080     CONTINUE                                                         300
C    *                                                                    300
C    Geant POSITION : LSAS ===> X=-5.4221 Y=-6.031 Z=-42.24405-ZOFFD <==  301
C    =                                                                    301
         CALL AGSRESET                                                    301
         AG_X = -5.4221                                                   301
         AG_Y = -6.031                                                    301
         AG_Z = -42.24405-ZOFFD                                           301
         AG_TITLE = 'LSAS'                                                301
         AG_EXNAME = 'POSITION'                                           301
         AG_PARLIST = 'X_Y_Z'                                             301
         CALL AXPOSITION                                                  301
C    *                                                                    301
C    Create LSAT :                                                        302
C    Call To Local Procedure LSAT                                         302
         ASSIGN 6100 TO BLOCK_LSAT                                        302
         GO TO  6090                                                      302
6100     CONTINUE                                                         302
C    *                                                                    302
C    Geant POSITION : LSAT ===> X=-4.8017 Y=-6.5357 Z=-42.6021-ZOFFD <==  303
C    =                                                                    303
         CALL AGSRESET                                                    303
         AG_X = -4.8017                                                   303
         AG_Y = -6.5357                                                   303
         AG_Z = -42.6021-ZOFFD                                            303
         AG_TITLE = 'LSAT'                                                303
         AG_EXNAME = 'POSITION'                                           303
         AG_PARLIST = 'X_Y_Z'                                             303
         CALL AXPOSITION                                                  303
C    *                                                                    303
C    *                                                                    303
         JJ=1                                                             304
            GO TO 6113                                                    304
6111        JJ=JJ+1                                                       304
6113        IF(JJ-(SCSD_NUMBEROFCABLES).GT.0) GO TO 6112                  304
C       Create SCAD :                                                     305
C       Call To Local Procedure SCAD                                      305
            ASSIGN 6130 TO BLOCK_SCAD                                     305
            GO TO  6120                                                   305
6130        CONTINUE                                                      305
C       *                                                                 305
C       Geant POSITION : SCAD ===> X=SCSD_XPOS(JJ) Y=SCSD_YPOS(JJ) Z=ZOF  306
C       FCABLES-ZOFFD <===                                                306
            CALL AGSRESET                                                 306
            AG_X = SCSD_XPOS(JJ)                                          306
            AG_Y = SCSD_YPOS(JJ)                                          306
            AG_Z = ZOFFCABLES-ZOFFD                                       306
            AG_TITLE = 'SCAD'                                             306
            AG_EXNAME = 'POSITION'                                        306
            AG_PARLIST = 'X_Y_Z'                                          306
            CALL AXPOSITION                                               306
C       *                                                                 306
         GO TO 6111                                                       307
6112     CONTINUE                                                         307
C    *                                                                    308
         LL=1                                                             308
            GO TO 6143                                                    308
6141        LL=LL+1                                                       308
6143        IF(LL-(LCSD_NUMBEROFCABLES).GT.0) GO TO 6142                  308
C       Create LCAD :                                                     309
C       Call To Local Procedure LCAD                                      309
            ASSIGN 6160 TO BLOCK_LCAD                                     309
            GO TO  6150                                                   309
6160        CONTINUE                                                      309
C       *                                                                 309
C       Geant POSITION : LCAD ===> X=LCSD_XPOS(LL) Y=LCSD_YPOS(LL) Z=ZOF  310
C       FCABLEL-ZOFFD <===                                                310
            CALL AGSRESET                                                 310
            AG_X = LCSD_XPOS(LL)                                          310
            AG_Y = LCSD_YPOS(LL)                                          310
            AG_Z = ZOFFCABLEL-ZOFFD                                       310
            AG_TITLE = 'LCAD'                                             310
            AG_EXNAME = 'POSITION'                                        310
            AG_PARLIST = 'X_Y_Z'                                          310
            CALL AXPOSITION                                               310
C       *                                                                 310
         GO TO 6141                                                       311
6142     CONTINUE                                                         311
5011  CALL AGSPOPB                                                        312
      GO TO BLOCK_DTUH, (5010 ,5020)                                      312
C End of Local Procedure                                                  312
C ----------------------------------------------------------------------  313
C ------------ TUBA                                                       313
C Begin Local Procedure TUBA                                              314
C TUBA A TUBE WITH INNNER AND OUTER RADIUS                                314
5050  CONTINUE                                                            314
         AG_TITLE='TUBA'                                                  314
         AG_MODULE=MODULE                                                 314
         AG_MARK='V00'                                                    314
         CALL AGSPUSH                                                     314
         AG_MODULE=MODULE                                                 314
C    *                                                                    314
C    Geant ATTRIBUTE : TUBA ===> SEEN=1 COLO=629 <===                     315
         CALL AGSRESET                                                    315
         AG_SEEN = 1                                                      315
         AG_COLO = 629                                                    315
         AG_TITLE = 'TUBA'                                                315
         AG_EXNAME = 'ATTRIBUTE'                                          315
         AG_PARLIST = 'SEEN_COLO'                                         315
         CALL AXATTRIBUTE                                                 315
C    *                                                                    315
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=RADCABLE DZ=ARCLENGTH/(2*RADD_N  315
C    UMBEROFSECTIONS) <===                                                315
         CALL AGSRESET                                                    316
         AG_RMIN = 0                                                      316
         AG_RMAX = RADCABLE                                               316
         AG_DZ = ARCLENGTH/(2*RADD_NUMBEROFSECTIONS)                      316
         AG_TITLE = 'TUBE'                                                316
         AG_EXNAME = 'SHAPE'                                              316
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      316
         CALL AXSHAPE                                                     316
C    *                                                                    316
5051  CALL AGSPOPB                                                        317
      GO TO BLOCK_TUBA, (5050 ,5060)                                      317
C End of Local Procedure                                                  317
C ----------------------------------------------------------------------  318
C ------------ APTO                                                       318
C Begin Local Procedure APTO                                              319
C APTO A PLANE TOP                                                        319
5070  CONTINUE                                                            319
         AG_TITLE='APTO'                                                  319
         AG_MODULE=MODULE                                                 319
         AG_MARK='V00'                                                    319
         CALL AGSPUSH                                                     319
         AG_MODULE=MODULE                                                 319
C    *                                                                    319
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          320
         CALL AGSRESET                                                    320
         AG_TITLE = 'CFRPMIX'                                             320
         AG_EXNAME = 'MATERIAL'                                           320
         AG_PARLIST = 'NONE'                                              320
         CALL AXMATERIAL                                                  320
C    *                                                                    320
C    Geant ATTRIBUTE : APTO ===> SEEN=1 COLO=920 <===                     320
         CALL AGSRESET                                                    321
         AG_SEEN = 1                                                      321
         AG_COLO = 920                                                    321
         AG_TITLE = 'APTO'                                                321
         AG_EXNAME = 'ATTRIBUTE'                                          321
         AG_PARLIST = 'SEEN_COLO'                                         321
         CALL AXATTRIBUTE                                                 321
C    *                                                                    321
C    Geant SHAPE : BOX ===> DX=THK/2. DY=DHSA/2. DZ=DSEA/2. <===          321
         CALL AGSRESET                                                    322
         AG_DX = THK/2.                                                   322
         AG_DY = DHSA/2.                                                  322
         AG_DZ = DSEA/2.                                                  322
         AG_TITLE = 'BOX'                                                 322
         AG_EXNAME = 'SHAPE'                                              322
         AG_PARLIST = 'DX_DY_DZ'                                          322
         CALL AXSHAPE                                                     322
C    *                                                                    322
5071  CALL AGSPOPB                                                        323
      GO TO BLOCK_APTO, (5070 ,5080)                                      323
C End of Local Procedure                                                  323
C ----------------------------------------------------------------------  324
C ------------ UCTO                                                       324
C Begin Local Procedure UCTO                                              325
C UCTO A UP CORNER TOP                                                    325
5090  CONTINUE                                                            325
         AG_TITLE='UCTO'                                                  325
         AG_MODULE=MODULE                                                 325
         AG_MARK='V00'                                                    325
         CALL AGSPUSH                                                     325
         AG_MODULE=MODULE                                                 325
C    *                                                                    325
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          326
         CALL AGSRESET                                                    326
         AG_TITLE = 'CFRPMIX'                                             326
         AG_EXNAME = 'MATERIAL'                                           326
         AG_PARLIST = 'NONE'                                              326
         CALL AXMATERIAL                                                  326
C    *                                                                    326
C    Geant ATTRIBUTE : UCTO ===> SEEN=1 COLO=920 <===                     326
         CALL AGSRESET                                                    327
         AG_SEEN = 1                                                      327
         AG_COLO = 920                                                    327
         AG_TITLE = 'UCTO'                                                327
         AG_EXNAME = 'ATTRIBUTE'                                          327
         AG_PARLIST = 'SEEN_COLO'                                         327
         CALL AXATTRIBUTE                                                 327
C    *                                                                    327
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  327
C    I1=0.0 PHI2=ANGLETOPLEFT DZ=DSEA/2. <===                             327
         CALL AGSRESET                                                    329
         AG_RMIN = ARCLENGTHOUT                                           329
         AG_RMAX = ARCLENGTHOUT+THK                                       329
         AG_PHI1 = 0.0                                                    329
         AG_PHI2 = ANGLETOPLEFT                                           329
         AG_DZ = DSEA/2.                                                  329
         AG_TITLE = 'TUBS'                                                329
         AG_EXNAME = 'SHAPE'                                              329
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            329
         CALL AXSHAPE                                                     329
C    *                                                                    329
5091  CALL AGSPOPB                                                        330
      GO TO BLOCK_UCTO, (5090 ,5100)                                      330
C End of Local Procedure                                                  330
C ----------------------------------------------------------------------  331
C ------------ OUCY                                                       331
C Begin Local Procedure OUCY                                              332
C OUCY A OUTSIDE CYLINDER                                                 332
5110  CONTINUE                                                            332
         AG_TITLE='OUCY'                                                  332
         AG_MODULE=MODULE                                                 332
         AG_MARK='V00'                                                    332
         CALL AGSPUSH                                                     332
         AG_MODULE=MODULE                                                 332
C    *                                                                    332
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          333
         CALL AGSRESET                                                    333
         AG_TITLE = 'CFRPMIX'                                             333
         AG_EXNAME = 'MATERIAL'                                           333
         AG_PARLIST = 'NONE'                                              333
         CALL AXMATERIAL                                                  333
C    *                                                                    333
C    Geant ATTRIBUTE : OUCY ===> SEEN=1 COLO=920 <===                     333
         CALL AGSRESET                                                    334
         AG_SEEN = 1                                                      334
         AG_COLO = 920                                                    334
         AG_TITLE = 'OUCY'                                                334
         AG_EXNAME = 'ATTRIBUTE'                                          334
         AG_PARLIST = 'SEEN_COLO'                                         334
         CALL AXATTRIBUTE                                                 334
C    *                                                                    334
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=ANGLETOPLE  334
C    FT, PHI2=ANGLETOPLEFT+OUTERTUBELENGTH, DZ=DSEA/2. <===               334
         CALL AGSRESET                                                    337
         AG_RMIN = DTUBEW                                                 337
         AG_RMAX = DTUBEW+THK                                             337
         AG_PHI1 = ANGLETOPLEFT                                           337
         AG_PHI2 = ANGLETOPLEFT+OUTERTUBELENGTH                           337
         AG_DZ = DSEA/2.                                                  337
         AG_TITLE = 'TUBS'                                                337
         AG_EXNAME = 'SHAPE'                                              337
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            337
         CALL AXSHAPE                                                     337
C    *                                                                    337
5111  CALL AGSPOPB                                                        338
      GO TO BLOCK_OUCY, (5110 ,5120)                                      338
C End of Local Procedure                                                  338
C ----------------------------------------------------------------------  339
C ------------ DCBO                                                       339
C Begin Local Procedure DCBO                                              340
C DCBO A DOWN CORNER BOTTOMUP                                             340
5130  CONTINUE                                                            340
         AG_TITLE='DCBO'                                                  340
         AG_MODULE=MODULE                                                 340
         AG_MARK='V00'                                                    340
         CALL AGSPUSH                                                     340
         AG_MODULE=MODULE                                                 340
C    *                                                                    340
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          341
         CALL AGSRESET                                                    341
         AG_TITLE = 'CFRPMIX'                                             341
         AG_EXNAME = 'MATERIAL'                                           341
         AG_PARLIST = 'NONE'                                              341
         CALL AXMATERIAL                                                  341
C    *                                                                    341
C    Geant ATTRIBUTE : DCBO ===> SEEN=1 COLO=920 <===                     341
         CALL AGSRESET                                                    342
         AG_SEEN = 1                                                      342
         AG_COLO = 920                                                    342
         AG_TITLE = 'DCBO'                                                342
         AG_EXNAME = 'ATTRIBUTE'                                          342
         AG_PARLIST = 'SEEN_COLO'                                         342
         CALL AXATTRIBUTE                                                 342
C    *                                                                    342
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  342
C    I1=360-ANGLETOPLEFT PHI2=360 DZ=DSEA/2. <===                         342
         CALL AGSRESET                                                    344
         AG_RMIN = ARCLENGTHOUT                                           344
         AG_RMAX = ARCLENGTHOUT+THK                                       344
         AG_PHI1 = 360-ANGLETOPLEFT                                       344
         AG_PHI2 = 360                                                    344
         AG_DZ = DSEA/2.                                                  344
         AG_TITLE = 'TUBS'                                                344
         AG_EXNAME = 'SHAPE'                                              344
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            344
         CALL AXSHAPE                                                     344
C    *                                                                    344
5131  CALL AGSPOPB                                                        345
      GO TO BLOCK_DCBO, (5130 ,5140)                                      345
C End of Local Procedure                                                  345
C ----------------------------------------------------------------------  346
C ------------ APBM                                                       346
C Begin Local Procedure APBM                                              347
C APBM A PLANE BOTTOM                                                     347
5150  CONTINUE                                                            347
         AG_TITLE='APBM'                                                  347
         AG_MODULE=MODULE                                                 347
         AG_MARK='V00'                                                    347
         CALL AGSPUSH                                                     347
         AG_MODULE=MODULE                                                 347
C    *                                                                    347
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          348
         CALL AGSRESET                                                    348
         AG_TITLE = 'CFRPMIX'                                             348
         AG_EXNAME = 'MATERIAL'                                           348
         AG_PARLIST = 'NONE'                                              348
         CALL AXMATERIAL                                                  348
C    *                                                                    348
C    Geant ATTRIBUTE : APBM ===> SEEN=1 COLO=920 <===                     348
         CALL AGSRESET                                                    349
         AG_SEEN = 1                                                      349
         AG_COLO = 920                                                    349
         AG_TITLE = 'APBM'                                                349
         AG_EXNAME = 'ATTRIBUTE'                                          349
         AG_PARLIST = 'SEEN_COLO'                                         349
         CALL AXATTRIBUTE                                                 349
C    *                                                                    349
C    Geant SHAPE : BOX ===> DX=THK/2. DY=DHSA/2. DZ=DSEA/2. <===          349
         CALL AGSRESET                                                    350
         AG_DX = THK/2.                                                   350
         AG_DY = DHSA/2.                                                  350
         AG_DZ = DSEA/2.                                                  350
         AG_TITLE = 'BOX'                                                 350
         AG_EXNAME = 'SHAPE'                                              350
         AG_PARLIST = 'DX_DY_DZ'                                          350
         CALL AXSHAPE                                                     350
C    *                                                                    350
5151  CALL AGSPOPB                                                        351
      GO TO BLOCK_APBM, (5150 ,5160)                                      351
C End of Local Procedure                                                  351
C ----------------------------------------------------------------------  352
C ------------ UCBO                                                       352
C Begin Local Procedure UCBO                                              353
C UCBO A UP CORNER BOTTOM                                                 353
5170  CONTINUE                                                            353
         AG_TITLE='UCBO'                                                  353
         AG_MODULE=MODULE                                                 353
         AG_MARK='V00'                                                    353
         CALL AGSPUSH                                                     353
         AG_MODULE=MODULE                                                 353
C    *                                                                    353
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          354
         CALL AGSRESET                                                    354
         AG_TITLE = 'CFRPMIX'                                             354
         AG_EXNAME = 'MATERIAL'                                           354
         AG_PARLIST = 'NONE'                                              354
         CALL AXMATERIAL                                                  354
C    *                                                                    354
C    Geant ATTRIBUTE : UCBO ===> SEEN=1 COLO=920 <===                     354
         CALL AGSRESET                                                    355
         AG_SEEN = 1                                                      355
         AG_COLO = 920                                                    355
         AG_TITLE = 'UCBO'                                                355
         AG_EXNAME = 'ATTRIBUTE'                                          355
         AG_PARLIST = 'SEEN_COLO'                                         355
         CALL AXATTRIBUTE                                                 355
C    *                                                                    355
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  355
C    I1=360-ANGLEBOTTOMLEFT PHI2=360, DZ=DSEA/2. <===                     355
         CALL AGSRESET                                                    358
         AG_RMIN = ARCLENGTHOUT                                           358
         AG_RMAX = ARCLENGTHOUT+THK                                       358
         AG_PHI1 = 360-ANGLEBOTTOMLEFT                                    358
         AG_PHI2 = 360                                                    358
         AG_DZ = DSEA/2.                                                  358
         AG_TITLE = 'TUBS'                                                358
         AG_EXNAME = 'SHAPE'                                              358
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            358
         CALL AXSHAPE                                                     358
C    *                                                                    358
5171  CALL AGSPOPB                                                        359
      GO TO BLOCK_UCBO, (5170 ,5180)                                      359
C End of Local Procedure                                                  359
C ----------------------------------------------------------------------  360
C ------------ DCTO                                                       360
C Begin Local Procedure DCTO                                              361
C DCTO DOWN CORNER TOP                                                    361
5190  CONTINUE                                                            361
         AG_TITLE='DCTO'                                                  361
         AG_MODULE=MODULE                                                 361
         AG_MARK='V00'                                                    361
         CALL AGSPUSH                                                     361
         AG_MODULE=MODULE                                                 361
C    *                                                                    361
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          362
         CALL AGSRESET                                                    362
         AG_TITLE = 'CFRPMIX'                                             362
         AG_EXNAME = 'MATERIAL'                                           362
         AG_PARLIST = 'NONE'                                              362
         CALL AXMATERIAL                                                  362
C    *                                                                    362
C    Geant ATTRIBUTE : DCTO ===> SEEN=1 COLO=920 <===                     362
         CALL AGSRESET                                                    363
         AG_SEEN = 1                                                      363
         AG_COLO = 920                                                    363
         AG_TITLE = 'DCTO'                                                363
         AG_EXNAME = 'ATTRIBUTE'                                          363
         AG_PARLIST = 'SEEN_COLO'                                         363
         CALL AXATTRIBUTE                                                 363
C    *                                                                    363
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  363
C    I1=0 PHI2=ANGLEBOTTOMLEFT DZ=DSEA/2. <===                            363
         CALL AGSRESET                                                    365
         AG_RMIN = ARCLENGTHOUT                                           365
         AG_RMAX = ARCLENGTHOUT+THK                                       365
         AG_PHI1 = 0                                                      365
         AG_PHI2 = ANGLEBOTTOMLEFT                                        365
         AG_DZ = DSEA/2.                                                  365
         AG_TITLE = 'TUBS'                                                365
         AG_EXNAME = 'SHAPE'                                              365
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            365
         CALL AXSHAPE                                                     365
C    *                                                                    365
5191  CALL AGSPOPB                                                        366
      GO TO BLOCK_DCTO, (5190 ,5200)                                      366
C End of Local Procedure                                                  366
C ----------------------------------------------------------------------  367
C ------------ INCY                                                       367
C Begin Local Procedure INCY                                              368
C INCY AN INTERNAL CYLINDER                                               368
5210  CONTINUE                                                            368
         AG_TITLE='INCY'                                                  368
         AG_MODULE=MODULE                                                 368
         AG_MARK='V00'                                                    368
         CALL AGSPUSH                                                     368
         AG_MODULE=MODULE                                                 368
C    *                                                                    368
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          369
         CALL AGSRESET                                                    369
         AG_TITLE = 'CFRPMIX'                                             369
         AG_EXNAME = 'MATERIAL'                                           369
         AG_PARLIST = 'NONE'                                              369
         CALL AXMATERIAL                                                  369
C    *                                                                    369
C    Geant ATTRIBUTE : INCY ===> SEEN=1 COLO=920 <===                     369
         CALL AGSRESET                                                    370
         AG_SEEN = 1                                                      370
         AG_COLO = 920                                                    370
         AG_TITLE = 'INCY'                                                370
         AG_EXNAME = 'ATTRIBUTE'                                          370
         AG_PARLIST = 'SEEN_COLO'                                         370
         CALL AXATTRIBUTE                                                 370
C    *                                                                    370
C    Geant SHAPE : TUBS ===> RMIN=RMINTUBESECTIONAIN/2. RMAX=RMAXTUBESEC  370
C    TIONAIN/2., PHI1=180-ANGLEBOTTOMLEFT, PHI2=180+ANGLEBOTTOMLEFT DZ=D  370
C    SEA/2. <===                                                          370
         CALL AGSRESET                                                    373
         AG_RMIN = RMINTUBESECTIONAIN/2.                                  373
         AG_RMAX = RMAXTUBESECTIONAIN/2.                                  373
         AG_PHI1 = 180-ANGLEBOTTOMLEFT                                    373
         AG_PHI2 = 180+ANGLEBOTTOMLEFT                                    373
         AG_DZ = DSEA/2.                                                  373
         AG_TITLE = 'TUBS'                                                373
         AG_EXNAME = 'SHAPE'                                              373
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            373
         CALL AXSHAPE                                                     373
C    *                                                                    373
5211  CALL AGSPOPB                                                        374
      GO TO BLOCK_INCY, (5210 ,5220)                                      374
C End of Local Procedure                                                  374
C ----------------------------------------------------------------------  375
C ------------ CYTR                                                       375
C Begin Local Procedure CYTR                                              376
C CYTR A CYLINDER TRANSITION                                              376
5230  CONTINUE                                                            376
         AG_TITLE='CYTR'                                                  376
         AG_MODULE=MODULE                                                 376
         AG_MARK='V00'                                                    376
         CALL AGSPUSH                                                     376
         AG_MODULE=MODULE                                                 376
C    *                                                                    376
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          377
         CALL AGSRESET                                                    377
         AG_TITLE = 'CFRPMIX'                                             377
         AG_EXNAME = 'MATERIAL'                                           377
         AG_PARLIST = 'NONE'                                              377
         CALL AXMATERIAL                                                  377
C    *                                                                    377
C    Geant ATTRIBUTE : CYTR ===> SEEN=1 COLO=920 <===                     377
         CALL AGSRESET                                                    378
         AG_SEEN = 1                                                      378
         AG_COLO = 920                                                    378
         AG_TITLE = 'CYTR'                                                378
         AG_EXNAME = 'ATTRIBUTE'                                          378
         AG_PARLIST = 'SEEN_COLO'                                         378
         CALL AXATTRIBUTE                                                 378
C    *                                                                    378
C    Geant SHAPE : PCON ===> PHI1=180-ANGLEBOTTOMLEFT DPHI=2*ANGLEBOTTOM  378
C    LEFT, NZ=3 ZI={DSEA/2.,DSEA/2.+DSEB, DSEA/2.+DSEB+DSEC+DSED} RMN={3  378
C    .1,2.76015, 2.76015} RMX={3.2,2.86015,2.86015} <===                  378
         CALL AGSRESET                                                    382
         AG_PHI1 = 180-ANGLEBOTTOMLEFT                                    382
         AG_DPHI = 2*ANGLEBOTTOMLEFT                                      382
         AG_NZ = 3                                                        382
         AG_ZI(1)=DSEA/2.                                                 382
         AG_ZI(2)=DSEA/2.+DSEB                                            382
         AG_ZI(3)=DSEA/2.+DSEB+DSEC+DSED                                  382
         AG_RMN(1)=3.1                                                    382
         AG_RMN(2)=2.76015                                                382
         AG_RMN(3)=2.76015                                                382
         AG_RMX(1)=3.2                                                    382
         AG_RMX(2)=2.86015                                                382
         AG_RMX(3)=2.86015                                                382
         AG_TITLE = 'PCON'                                                382
         AG_EXNAME = 'SHAPE'                                              382
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           382
         CALL AXSHAPE                                                     382
C    *                                                                    382
5231  CALL AGSPOPB                                                        383
      GO TO BLOCK_CYTR, (5230 ,5240)                                      383
C End of Local Procedure                                                  383
C ----------------------------------------------------------------------  384
C ------------ APTE                                                       384
C Begin Local Procedure APTE                                              385
C APTE A PLANE TOP END                                                    385
5250  CONTINUE                                                            385
         AG_TITLE='APTE'                                                  385
         AG_MODULE=MODULE                                                 385
         AG_MARK='V00'                                                    385
         CALL AGSPUSH                                                     385
         AG_MODULE=MODULE                                                 385
C    *                                                                    385
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          386
         CALL AGSRESET                                                    386
         AG_TITLE = 'CFRPMIX'                                             386
         AG_EXNAME = 'MATERIAL'                                           386
         AG_PARLIST = 'NONE'                                              386
         CALL AXMATERIAL                                                  386
C    *                                                                    386
C    Geant ATTRIBUTE : APTE ===> SEEN=1 COLO=920 <===                     386
         CALL AGSRESET                                                    387
         AG_SEEN = 1                                                      387
         AG_COLO = 920                                                    387
         AG_TITLE = 'APTE'                                                387
         AG_EXNAME = 'ATTRIBUTE'                                          387
         AG_PARLIST = 'SEEN_COLO'                                         387
         CALL AXATTRIBUTE                                                 387
C    *                                                                    387
C    Geant SHAPE : BOX ===> DX=THK/2. DY=DHSB/2. DZ=DSEC/2. <===          387
         CALL AGSRESET                                                    388
         AG_DX = THK/2.                                                   388
         AG_DY = DHSB/2.                                                  388
         AG_DZ = DSEC/2.                                                  388
         AG_TITLE = 'BOX'                                                 388
         AG_EXNAME = 'SHAPE'                                              388
         AG_PARLIST = 'DX_DY_DZ'                                          388
         CALL AXSHAPE                                                     388
C    *                                                                    388
5251  CALL AGSPOPB                                                        389
      GO TO BLOCK_APTE, (5250 ,5260)                                      389
C End of Local Procedure                                                  389
C ----------------------------------------------------------------------  390
C ------------ UCTE                                                       390
C Begin Local Procedure UCTE                                              391
C UCTE A UP CORNER END                                                    391
5270  CONTINUE                                                            391
         AG_TITLE='UCTE'                                                  391
         AG_MODULE=MODULE                                                 391
         AG_MARK='V00'                                                    391
         CALL AGSPUSH                                                     391
         AG_MODULE=MODULE                                                 391
C    *                                                                    391
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          392
         CALL AGSRESET                                                    392
         AG_TITLE = 'CFRPMIX'                                             392
         AG_EXNAME = 'MATERIAL'                                           392
         AG_PARLIST = 'NONE'                                              392
         CALL AXMATERIAL                                                  392
C    *                                                                    392
C    Geant ATTRIBUTE : UCTE ===> SEEN=1 COLO=920 <===                     392
         CALL AGSRESET                                                    393
         AG_SEEN = 1                                                      393
         AG_COLO = 920                                                    393
         AG_TITLE = 'UCTE'                                                393
         AG_EXNAME = 'ATTRIBUTE'                                          393
         AG_PARLIST = 'SEEN_COLO'                                         393
         CALL AXATTRIBUTE                                                 393
C    *                                                                    393
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  393
C    I1=0.0 PHI2=ANGLETOPLEFT DZ=DSEC/2. <===                             393
         CALL AGSRESET                                                    395
         AG_RMIN = ARCLENGTHOUT                                           395
         AG_RMAX = ARCLENGTHOUT+THK                                       395
         AG_PHI1 = 0.0                                                    395
         AG_PHI2 = ANGLETOPLEFT                                           395
         AG_DZ = DSEC/2.                                                  395
         AG_TITLE = 'TUBS'                                                395
         AG_EXNAME = 'SHAPE'                                              395
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            395
         CALL AXSHAPE                                                     395
C    *                                                                    395
5271  CALL AGSPOPB                                                        396
      GO TO BLOCK_UCTE, (5270 ,5280)                                      396
C End of Local Procedure                                                  396
C ----------------------------------------------------------------------  397
C ------------ APBE                                                       397
C Begin Local Procedure APBE                                              398
C APBE A PLANE BOTTOM END                                                 398
5290  CONTINUE                                                            398
         AG_TITLE='APBE'                                                  398
         AG_MODULE=MODULE                                                 398
         AG_MARK='V00'                                                    398
         CALL AGSPUSH                                                     398
         AG_MODULE=MODULE                                                 398
C    *                                                                    398
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          399
         CALL AGSRESET                                                    399
         AG_TITLE = 'CFRPMIX'                                             399
         AG_EXNAME = 'MATERIAL'                                           399
         AG_PARLIST = 'NONE'                                              399
         CALL AXMATERIAL                                                  399
C    *                                                                    399
C    Geant ATTRIBUTE : APBE ===> SEEN=1 COLO=920 <===                     399
         CALL AGSRESET                                                    400
         AG_SEEN = 1                                                      400
         AG_COLO = 920                                                    400
         AG_TITLE = 'APBE'                                                400
         AG_EXNAME = 'ATTRIBUTE'                                          400
         AG_PARLIST = 'SEEN_COLO'                                         400
         CALL AXATTRIBUTE                                                 400
C    *                                                                    400
C    Geant SHAPE : BOX ===> DX=THK/2. DY=DHSB/2. DZ=DSEC/2. <===          400
         CALL AGSRESET                                                    401
         AG_DX = THK/2.                                                   401
         AG_DY = DHSB/2.                                                  401
         AG_DZ = DSEC/2.                                                  401
         AG_TITLE = 'BOX'                                                 401
         AG_EXNAME = 'SHAPE'                                              401
         AG_PARLIST = 'DX_DY_DZ'                                          401
         CALL AXSHAPE                                                     401
C    *                                                                    401
5291  CALL AGSPOPB                                                        402
      GO TO BLOCK_APBE, (5290 ,5300)                                      402
C End of Local Procedure                                                  402
C ----------------------------------------------------------------------  403
C ------------ DCBE                                                       403
C Begin Local Procedure DCBE                                              404
C DCBE A DOWN CORNER BOTTOM END                                           404
5310  CONTINUE                                                            404
         AG_TITLE='DCBE'                                                  404
         AG_MODULE=MODULE                                                 404
         AG_MARK='V00'                                                    404
         CALL AGSPUSH                                                     404
         AG_MODULE=MODULE                                                 404
C    *                                                                    404
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          405
         CALL AGSRESET                                                    405
         AG_TITLE = 'CFRPMIX'                                             405
         AG_EXNAME = 'MATERIAL'                                           405
         AG_PARLIST = 'NONE'                                              405
         CALL AXMATERIAL                                                  405
C    *                                                                    405
C    Geant ATTRIBUTE : DCBE ===> SEEN=1 COLO=920 <===                     405
         CALL AGSRESET                                                    406
         AG_SEEN = 1                                                      406
         AG_COLO = 920                                                    406
         AG_TITLE = 'DCBE'                                                406
         AG_EXNAME = 'ATTRIBUTE'                                          406
         AG_PARLIST = 'SEEN_COLO'                                         406
         CALL AXATTRIBUTE                                                 406
C    *                                                                    406
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  406
C    I1=360-ANGLETOPLEFT PHI2=360 DZ=DSEC/2. <===                         406
         CALL AGSRESET                                                    408
         AG_RMIN = ARCLENGTHOUT                                           408
         AG_RMAX = ARCLENGTHOUT+THK                                       408
         AG_PHI1 = 360-ANGLETOPLEFT                                       408
         AG_PHI2 = 360                                                    408
         AG_DZ = DSEC/2.                                                  408
         AG_TITLE = 'TUBS'                                                408
         AG_EXNAME = 'SHAPE'                                              408
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            408
         CALL AXSHAPE                                                     408
C    *                                                                    408
5311  CALL AGSPOPB                                                        409
      GO TO BLOCK_DCBE, (5310 ,5320)                                      409
C End of Local Procedure                                                  409
C ----------------------------------------------------------------------  410
C ------------ OUCE                                                       410
C Begin Local Procedure OUCE                                              411
C OUCE A OUTSIDE CYLINDER END                                             411
5330  CONTINUE                                                            411
         AG_TITLE='OUCE'                                                  411
         AG_MODULE=MODULE                                                 411
         AG_MARK='V00'                                                    411
         CALL AGSPUSH                                                     411
         AG_MODULE=MODULE                                                 411
C    *                                                                    411
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          412
         CALL AGSRESET                                                    412
         AG_TITLE = 'CFRPMIX'                                             412
         AG_EXNAME = 'MATERIAL'                                           412
         AG_PARLIST = 'NONE'                                              412
         CALL AXMATERIAL                                                  412
C    *                                                                    412
C    Geant ATTRIBUTE : OUCE ===> SEEN=1 COLO=920 <===                     412
         CALL AGSRESET                                                    413
         AG_SEEN = 1                                                      413
         AG_COLO = 920                                                    413
         AG_TITLE = 'OUCE'                                                413
         AG_EXNAME = 'ATTRIBUTE'                                          413
         AG_PARLIST = 'SEEN_COLO'                                         413
         CALL AXATTRIBUTE                                                 413
C    *                                                                    413
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=ANGLETOPLE  413
C    FT, PHI2=ANGLETOPLEFT+OUTERTUBELENGTH, DZ=DSEC/2. <===               413
         CALL AGSRESET                                                    416
         AG_RMIN = DTUBEW                                                 416
         AG_RMAX = DTUBEW+THK                                             416
         AG_PHI1 = ANGLETOPLEFT                                           416
         AG_PHI2 = ANGLETOPLEFT+OUTERTUBELENGTH                           416
         AG_DZ = DSEC/2.                                                  416
         AG_TITLE = 'TUBS'                                                416
         AG_EXNAME = 'SHAPE'                                              416
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            416
         CALL AXSHAPE                                                     416
C    *                                                                    416
5331  CALL AGSPOPB                                                        417
      GO TO BLOCK_OUCE, (5330 ,5340)                                      417
C End of Local Procedure                                                  417
C ----------------------------------------------------------------------  418
C ------------ OUCI                                                       418
C Begin Local Procedure OUCI                                              419
C OUCI A OUTSIDE CYLINDER INTERMEDIATE                                    419
5350  CONTINUE                                                            419
         AG_TITLE='OUCI'                                                  419
         AG_MODULE=MODULE                                                 419
         AG_MARK='V00'                                                    419
         CALL AGSPUSH                                                     419
         AG_MODULE=MODULE                                                 419
C    *                                                                    419
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          420
         CALL AGSRESET                                                    420
         AG_TITLE = 'CFRPMIX'                                             420
         AG_EXNAME = 'MATERIAL'                                           420
         AG_PARLIST = 'NONE'                                              420
         CALL AXMATERIAL                                                  420
C    *                                                                    420
C    Geant ATTRIBUTE : OUCI ===> SEEN=1 COLO=920 <===                     420
         CALL AGSRESET                                                    421
         AG_SEEN = 1                                                      421
         AG_COLO = 920                                                    421
         AG_TITLE = 'OUCI'                                                421
         AG_EXNAME = 'ATTRIBUTE'                                          421
         AG_PARLIST = 'SEEN_COLO'                                         421
         CALL AXATTRIBUTE                                                 421
C    *                                                                    421
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=ANGLETOPLE  421
C    FT, PHI2=ANGLETOPLEFT+OUTERTUBELENGTH, DZ=DSEB/2. <===               421
         CALL AGSRESET                                                    424
         AG_RMIN = DTUBEW                                                 424
         AG_RMAX = DTUBEW+THK                                             424
         AG_PHI1 = ANGLETOPLEFT                                           424
         AG_PHI2 = ANGLETOPLEFT+OUTERTUBELENGTH                           424
         AG_DZ = DSEB/2.                                                  424
         AG_TITLE = 'TUBS'                                                424
         AG_EXNAME = 'SHAPE'                                              424
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            424
         CALL AXSHAPE                                                     424
C    *                                                                    424
5351  CALL AGSPOPB                                                        425
      GO TO BLOCK_OUCI, (5350 ,5360)                                      425
C End of Local Procedure                                                  425
C ----------------------------------------------------------------------  426
C ------------ ATTI                                                       426
C Begin Local Procedure ATTI                                              427
C ATTI A TRAP TOP INTERMDIATE                                             427
5410  CONTINUE                                                            427
         AG_TITLE='ATTI'                                                  427
         AG_MODULE=MODULE                                                 427
         AG_MARK='V00'                                                    427
         CALL AGSPUSH                                                     427
         AG_MODULE=MODULE                                                 427
C    *                                                                    427
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          428
         CALL AGSRESET                                                    428
         AG_TITLE = 'CFRPMIX'                                             428
         AG_EXNAME = 'MATERIAL'                                           428
         AG_PARLIST = 'NONE'                                              428
         CALL AXMATERIAL                                                  428
C    *                                                                    428
C    Geant ATTRIBUTE : ATTI ===> SEEN=1 COLO=920 <===                     428
         CALL AGSRESET                                                    429
         AG_SEEN = 1                                                      429
         AG_COLO = 920                                                    429
         AG_TITLE = 'ATTI'                                                429
         AG_EXNAME = 'ATTRIBUTE'                                          429
         AG_PARLIST = 'SEEN_COLO'                                         429
         CALL AXATTRIBUTE                                                 429
C    *                                                                    429
C    Geant SHAPE : TRAP ===> DZ=THK/2.0 THET=0 PHI=0 H1=2.6466/2, BL1=4.  429
C    2264/2 TL1=3.7738/2 ALP1=4.85 H2=2.6466/2, BL2=4.2264/2 TL2=3.7738/  429
C    2 ALP2=4.85 <===                                                     429
         CALL AGSRESET                                                    432
         AG_DZ = THK/2.0                                                  432
         AG_THET = 0                                                      432
         AG_PHI = 0                                                       432
         AG_H1 = 2.6466/2                                                 432
         AG_BL1 = 4.2264/2                                                432
         AG_TL1 = 3.7738/2                                                432
         AG_ALP1 = 4.85                                                   432
         AG_H2 = 2.6466/2                                                 432
         AG_BL2 = 4.2264/2                                                432
         AG_TL2 = 3.7738/2                                                432
         AG_ALP2 = 4.85                                                   432
         AG_TITLE = 'TRAP'                                                432
         AG_EXNAME = 'SHAPE'                                              432
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       432
         CALL AXSHAPE                                                     432
C    *                                                                    432
5411  CALL AGSPOPB                                                        433
      GO TO BLOCK_ATTI, (5410 ,5420)                                      433
C End of Local Procedure                                                  433
C ----------------------------------------------------------------------  434
C ------------ ATBI                                                       434
C Begin Local Procedure ATBI                                              435
C ATBI A TRAP BOTTOM INTERMEDIATE                                         435
5430  CONTINUE                                                            435
         AG_TITLE='ATBI'                                                  435
         AG_MODULE=MODULE                                                 435
         AG_MARK='V00'                                                    435
         CALL AGSPUSH                                                     435
         AG_MODULE=MODULE                                                 435
C    *                                                                    435
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          436
         CALL AGSRESET                                                    436
         AG_TITLE = 'CFRPMIX'                                             436
         AG_EXNAME = 'MATERIAL'                                           436
         AG_PARLIST = 'NONE'                                              436
         CALL AXMATERIAL                                                  436
C    *                                                                    436
C    Geant ATTRIBUTE : ATBI ===> SEEN=1 COLO=920 <===                     436
         CALL AGSRESET                                                    437
         AG_SEEN = 1                                                      437
         AG_COLO = 920                                                    437
         AG_TITLE = 'ATBI'                                                437
         AG_EXNAME = 'ATTRIBUTE'                                          437
         AG_PARLIST = 'SEEN_COLO'                                         437
         CALL AXATTRIBUTE                                                 437
C    *                                                                    437
C    Geant SHAPE : TRAP ===> DZ=THK/2.0 THET=0 PHI=0 H1=2.6466/2, BL1=4.  437
C    2264/2 TL1=3.7738/2 ALP1=4.85 H2=2.6466/2, BL2=4.2264/2 TL2=3.7738/  437
C    2 ALP2=4.85 <===                                                     437
         CALL AGSRESET                                                    440
         AG_DZ = THK/2.0                                                  440
         AG_THET = 0                                                      440
         AG_PHI = 0                                                       440
         AG_H1 = 2.6466/2                                                 440
         AG_BL1 = 4.2264/2                                                440
         AG_TL1 = 3.7738/2                                                440
         AG_ALP1 = 4.85                                                   440
         AG_H2 = 2.6466/2                                                 440
         AG_BL2 = 4.2264/2                                                440
         AG_TL2 = 3.7738/2                                                440
         AG_ALP2 = 4.85                                                   440
         AG_TITLE = 'TRAP'                                                440
         AG_EXNAME = 'SHAPE'                                              440
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       440
         CALL AXSHAPE                                                     440
C    *                                                                    440
5431  CALL AGSPOPB                                                        441
      GO TO BLOCK_ATBI, (5430 ,5440)                                      441
C End of Local Procedure                                                  441
C ----------------------------------------------------------------------  442
C ------------ UCTI                                                       442
C Begin Local Procedure UCTI                                              443
C UCTI A UP CORNER INTERMEDIATE                                           443
5370  CONTINUE                                                            443
         AG_TITLE='UCTI'                                                  443
         AG_MODULE=MODULE                                                 443
         AG_MARK='V00'                                                    443
         CALL AGSPUSH                                                     443
         AG_MODULE=MODULE                                                 443
C    *                                                                    443
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          444
         CALL AGSRESET                                                    444
         AG_TITLE = 'CFRPMIX'                                             444
         AG_EXNAME = 'MATERIAL'                                           444
         AG_PARLIST = 'NONE'                                              444
         CALL AXMATERIAL                                                  444
C    *                                                                    444
C    Geant ATTRIBUTE : UCTI ===> SEEN=1 COLO=920 <===                     444
         CALL AGSRESET                                                    445
         AG_SEEN = 1                                                      445
         AG_COLO = 920                                                    445
         AG_TITLE = 'UCTI'                                                445
         AG_EXNAME = 'ATTRIBUTE'                                          445
         AG_PARLIST = 'SEEN_COLO'                                         445
         CALL AXATTRIBUTE                                                 445
C    *                                                                    445
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  445
C    I1=0.0 PHI2=ANGLETOPLEFT DZ=DSEB/2. <===                             445
         CALL AGSRESET                                                    447
         AG_RMIN = ARCLENGTHOUT                                           447
         AG_RMAX = ARCLENGTHOUT+THK                                       447
         AG_PHI1 = 0.0                                                    447
         AG_PHI2 = ANGLETOPLEFT                                           447
         AG_DZ = DSEB/2.                                                  447
         AG_TITLE = 'TUBS'                                                447
         AG_EXNAME = 'SHAPE'                                              447
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            447
         CALL AXSHAPE                                                     447
C    *                                                                    447
5371  CALL AGSPOPB                                                        448
      GO TO BLOCK_UCTI, (5370 ,5380)                                      448
C End of Local Procedure                                                  448
C ----------------------------------------------------------------------  449
C ------------ DCBI                                                       449
C Begin Local Procedure DCBI                                              450
C DCBI A DOWN CORNER BOTTOM INTERMEDIATE                                  450
5390  CONTINUE                                                            450
         AG_TITLE='DCBI'                                                  450
         AG_MODULE=MODULE                                                 450
         AG_MARK='V00'                                                    450
         CALL AGSPUSH                                                     450
         AG_MODULE=MODULE                                                 450
C    *                                                                    450
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          451
         CALL AGSRESET                                                    451
         AG_TITLE = 'CFRPMIX'                                             451
         AG_EXNAME = 'MATERIAL'                                           451
         AG_PARLIST = 'NONE'                                              451
         CALL AXMATERIAL                                                  451
C    *                                                                    451
C    Geant ATTRIBUTE : DCBI ===> SEEN=1 COLO=920 <===                     451
         CALL AGSRESET                                                    452
         AG_SEEN = 1                                                      452
         AG_COLO = 920                                                    452
         AG_TITLE = 'DCBI'                                                452
         AG_EXNAME = 'ATTRIBUTE'                                          452
         AG_PARLIST = 'SEEN_COLO'                                         452
         CALL AXATTRIBUTE                                                 452
C    *                                                                    452
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  452
C    I1=360-ANGLETOPLEFT PHI2=360 DZ=DSEB/2. <===                         452
         CALL AGSRESET                                                    454
         AG_RMIN = ARCLENGTHOUT                                           454
         AG_RMAX = ARCLENGTHOUT+THK                                       454
         AG_PHI1 = 360-ANGLETOPLEFT                                       454
         AG_PHI2 = 360                                                    454
         AG_DZ = DSEB/2.                                                  454
         AG_TITLE = 'TUBS'                                                454
         AG_EXNAME = 'SHAPE'                                              454
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            454
         CALL AXSHAPE                                                     454
C    *                                                                    454
5391  CALL AGSPOPB                                                        455
      GO TO BLOCK_DCBI, (5390 ,5400)                                      455
C End of Local Procedure                                                  455
C ----------------------------------------------------------------------  456
C ------------ DCTH                                                       456
C Begin Local Procedure DCTH                                              457
C DCTH DOWN CORNER TRANSITION HEIGHT                                      457
5450  CONTINUE                                                            457
         AG_TITLE='DCTH'                                                  457
         AG_MODULE=MODULE                                                 457
         AG_MARK='V00'                                                    457
         CALL AGSPUSH                                                     457
         AG_MODULE=MODULE                                                 457
C    *                                                                    457
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          458
         CALL AGSRESET                                                    458
         AG_TITLE = 'CFRPMIX'                                             458
         AG_EXNAME = 'MATERIAL'                                           458
         AG_PARLIST = 'NONE'                                              458
         CALL AXMATERIAL                                                  458
C    *                                                                    458
C    Geant ATTRIBUTE : DCTH ===> SEEN=1 COLO=920 <===                     458
         CALL AGSRESET                                                    459
         AG_SEEN = 1                                                      459
         AG_COLO = 920                                                    459
         AG_TITLE = 'DCTH'                                                459
         AG_EXNAME = 'ATTRIBUTE'                                          459
         AG_PARLIST = 'SEEN_COLO'                                         459
         CALL AXATTRIBUTE                                                 459
C    *                                                                    459
C    Geant SHAPE : TUBS ===> RMIN=.20 RMAX=.20+THK PHI1=360-62, PHI2=360  459
C     DZ=(DSEC+DSED)/2. <===                                              459
         CALL AGSRESET                                                    461
         AG_RMIN = .20                                                    461
         AG_RMAX = .20+THK                                                461
         AG_PHI1 = 360-62                                                 461
         AG_PHI2 = 360                                                    461
         AG_DZ = (DSEC+DSED)/2.                                           461
         AG_TITLE = 'TUBS'                                                461
         AG_EXNAME = 'SHAPE'                                              461
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            461
         CALL AXSHAPE                                                     461
C    *                                                                    461
5451  CALL AGSPOPB                                                        462
      GO TO BLOCK_DCTH, (5450 ,5460)                                      462
C End of Local Procedure                                                  462
C ----------------------------------------------------------------------  463
C ------------ TCTH                                                       463
C Begin Local Procedure TCTH                                              464
C TCTH TOP CORNER TRANSITION HEIGHT                                       464
5470  CONTINUE                                                            464
         AG_TITLE='TCTH'                                                  464
         AG_MODULE=MODULE                                                 464
         AG_MARK='V00'                                                    464
         CALL AGSPUSH                                                     464
         AG_MODULE=MODULE                                                 464
C    *                                                                    464
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          465
         CALL AGSRESET                                                    465
         AG_TITLE = 'CFRPMIX'                                             465
         AG_EXNAME = 'MATERIAL'                                           465
         AG_PARLIST = 'NONE'                                              465
         CALL AXMATERIAL                                                  465
C    *                                                                    465
C    Geant ATTRIBUTE : TCTH ===> SEEN=1 COLO=920 <===                     465
         CALL AGSRESET                                                    466
         AG_SEEN = 1                                                      466
         AG_COLO = 920                                                    466
         AG_TITLE = 'TCTH'                                                466
         AG_EXNAME = 'ATTRIBUTE'                                          466
         AG_PARLIST = 'SEEN_COLO'                                         466
         CALL AXATTRIBUTE                                                 466
C    *                                                                    466
C    Geant SHAPE : TUBS ===> RMIN=.20 RMAX=.20+THK PHI1=0, PHI2=62 DZ=(D  466
C    SEC+DSED)/2. <===                                                    466
         CALL AGSRESET                                                    468
         AG_RMIN = .20                                                    468
         AG_RMAX = .20+THK                                                468
         AG_PHI1 = 0                                                      468
         AG_PHI2 = 62                                                     468
         AG_DZ = (DSEC+DSED)/2.                                           468
         AG_TITLE = 'TUBS'                                                468
         AG_EXNAME = 'SHAPE'                                              468
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            468
         CALL AXSHAPE                                                     468
C    *                                                                    468
5471  CALL AGSPOPB                                                        469
      GO TO BLOCK_TCTH, (5470 ,5480)                                      469
C End of Local Procedure                                                  469
C ----------------------------------------------------------------------  470
C ------------ CSTI                                                       470
C Begin Local Procedure CSTI                                              471
C CSTI CONE SEGMENT TOP INTERMEDIATE                                      471
5490  CONTINUE                                                            471
         AG_TITLE='CSTI'                                                  471
         AG_MODULE=MODULE                                                 471
         AG_MARK='V00'                                                    471
         CALL AGSPUSH                                                     471
         AG_MODULE=MODULE                                                 471
C    *                                                                    471
C    Geant ATTRIBUTE : CSTI ===> SEEN=1 COLO=920 <===                     472
         CALL AGSRESET                                                    472
         AG_SEEN = 1                                                      472
         AG_COLO = 920                                                    472
         AG_TITLE = 'CSTI'                                                472
         AG_EXNAME = 'ATTRIBUTE'                                          472
         AG_PARLIST = 'SEEN_COLO'                                         472
         CALL AXATTRIBUTE                                                 472
C    *                                                                    472
C    Geant SHAPE : CONS ===> DZ=DSEB/2.+.1 RMN1=.32 RMX1=.32+THK, RMN2=.  472
C    22 RMX2=.22+THK PHI1=0 PHI2=62 <===                                  472
         CALL AGSRESET                                                    474
         AG_DZ = DSEB/2.+.1                                               474
         AG_RMN1 = .32                                                    474
         AG_RMX1 = .32+THK                                                474
         AG_RMN2 = .22                                                    474
         AG_RMX2 = .22+THK                                                474
         AG_PHI1 = 0                                                      474
         AG_PHI2 = 62                                                     474
         AG_TITLE = 'CONS'                                                474
         AG_EXNAME = 'SHAPE'                                              474
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  474
         CALL AXSHAPE                                                     474
C    *                                                                    474
5491  CALL AGSPOPB                                                        475
      GO TO BLOCK_CSTI, (5490 ,5500)                                      475
C End of Local Procedure                                                  475
C ----------------------------------------------------------------------  476
C ------------ CSBI                                                       476
C Begin Local Procedure CSBI                                              477
C CSBI CONE SEGMENT TOP INTERMEDIATE                                      477
5510  CONTINUE                                                            477
         AG_TITLE='CSBI'                                                  477
         AG_MODULE=MODULE                                                 477
         AG_MARK='V00'                                                    477
         CALL AGSPUSH                                                     477
         AG_MODULE=MODULE                                                 477
C    *                                                                    477
C    Geant ATTRIBUTE : CSBI ===> SEEN=1 COLO=920 <===                     478
         CALL AGSRESET                                                    478
         AG_SEEN = 1                                                      478
         AG_COLO = 920                                                    478
         AG_TITLE = 'CSBI'                                                478
         AG_EXNAME = 'ATTRIBUTE'                                          478
         AG_PARLIST = 'SEEN_COLO'                                         478
         CALL AXATTRIBUTE                                                 478
C    *                                                                    478
C    Geant SHAPE : CONS ===> DZ=DSEB/2.+.1 RMN1=.32 RMX1=.32+THK, RMN2=.  478
C    22 RMX2=.22+THK PHI1=360-62 PHI2=360 <===                            478
         CALL AGSRESET                                                    480
         AG_DZ = DSEB/2.+.1                                               480
         AG_RMN1 = .32                                                    480
         AG_RMX1 = .32+THK                                                480
         AG_RMN2 = .22                                                    480
         AG_RMX2 = .22+THK                                                480
         AG_PHI1 = 360-62                                                 480
         AG_PHI2 = 360                                                    480
         AG_TITLE = 'CONS'                                                480
         AG_EXNAME = 'SHAPE'                                              480
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  480
         CALL AXSHAPE                                                     480
C    *                                                                    480
5511  CALL AGSPOPB                                                        481
      GO TO BLOCK_CSBI, (5510 ,5520)                                      481
C End of Local Procedure                                                  481
C ----------------------------------------------------------------------  482
C ------------ APET                                                       482
C Begin Local Procedure APET                                              483
C APET A PLANE END TOP                                                    483
5530  CONTINUE                                                            483
         AG_TITLE='APET'                                                  483
         AG_MODULE=MODULE                                                 483
         AG_MARK='V00'                                                    483
         CALL AGSPUSH                                                     483
         AG_MODULE=MODULE                                                 483
C    *                                                                    483
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          484
         CALL AGSRESET                                                    484
         AG_TITLE = 'CFRPMIX'                                             484
         AG_EXNAME = 'MATERIAL'                                           484
         AG_PARLIST = 'NONE'                                              484
         CALL AXMATERIAL                                                  484
C    *                                                                    484
C    Geant ATTRIBUTE : APET ===> SEEN=1 COLO=920 <===                     484
         CALL AGSRESET                                                    485
         AG_SEEN = 1                                                      485
         AG_COLO = 920                                                    485
         AG_TITLE = 'APET'                                                485
         AG_EXNAME = 'ATTRIBUTE'                                          485
         AG_PARLIST = 'SEEN_COLO'                                         485
         CALL AXATTRIBUTE                                                 485
C    *                                                                    485
C    Geant SHAPE : BOX ===> DX=THK/2. DY=DHSB/2. DZ=DSED/2. <===          485
         CALL AGSRESET                                                    486
         AG_DX = THK/2.                                                   486
         AG_DY = DHSB/2.                                                  486
         AG_DZ = DSED/2.                                                  486
         AG_TITLE = 'BOX'                                                 486
         AG_EXNAME = 'SHAPE'                                              486
         AG_PARLIST = 'DX_DY_DZ'                                          486
         CALL AXSHAPE                                                     486
C    *                                                                    486
5531  CALL AGSPOPB                                                        487
      GO TO BLOCK_APET, (5530 ,5540)                                      487
C End of Local Procedure                                                  487
C ----------------------------------------------------------------------  488
C ------------ APEB                                                       488
C Begin Local Procedure APEB                                              489
C APEB A PLANE END BOTTOM                                                 489
5550  CONTINUE                                                            489
         AG_TITLE='APEB'                                                  489
         AG_MODULE=MODULE                                                 489
         AG_MARK='V00'                                                    489
         CALL AGSPUSH                                                     489
         AG_MODULE=MODULE                                                 489
C    *                                                                    489
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          490
         CALL AGSRESET                                                    490
         AG_TITLE = 'CFRPMIX'                                             490
         AG_EXNAME = 'MATERIAL'                                           490
         AG_PARLIST = 'NONE'                                              490
         CALL AXMATERIAL                                                  490
C    *                                                                    490
C    Geant ATTRIBUTE : APEB ===> SEEN=1 COLO=920 <===                     490
         CALL AGSRESET                                                    491
         AG_SEEN = 1                                                      491
         AG_COLO = 920                                                    491
         AG_TITLE = 'APEB'                                                491
         AG_EXNAME = 'ATTRIBUTE'                                          491
         AG_PARLIST = 'SEEN_COLO'                                         491
         CALL AXATTRIBUTE                                                 491
C    *                                                                    491
C    Geant SHAPE : BOX ===> DX=THK/2. DY=DHSB/2. DZ=DSED/2. <===          491
         CALL AGSRESET                                                    492
         AG_DX = THK/2.                                                   492
         AG_DY = DHSB/2.                                                  492
         AG_DZ = DSED/2.                                                  492
         AG_TITLE = 'BOX'                                                 492
         AG_EXNAME = 'SHAPE'                                              492
         AG_PARLIST = 'DX_DY_DZ'                                          492
         CALL AXSHAPE                                                     492
C    *                                                                    492
5551  CALL AGSPOPB                                                        493
      GO TO BLOCK_APEB, (5550 ,5560)                                      493
C End of Local Procedure                                                  493
C ----------------------------------------------------------------------  494
C ------------ OCYE                                                       494
C Begin Local Procedure OCYE                                              495
C OCYE A OUTSIDE CYLINDER END                                             495
5570  CONTINUE                                                            495
         AG_TITLE='OCYE'                                                  495
         AG_MODULE=MODULE                                                 495
         AG_MARK='V00'                                                    495
         CALL AGSPUSH                                                     495
         AG_MODULE=MODULE                                                 495
C    *                                                                    495
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          496
         CALL AGSRESET                                                    496
         AG_TITLE = 'CFRPMIX'                                             496
         AG_EXNAME = 'MATERIAL'                                           496
         AG_PARLIST = 'NONE'                                              496
         CALL AXMATERIAL                                                  496
C    *                                                                    496
C    Geant ATTRIBUTE : OCYE ===> SEEN=1 COLO=920 <===                     496
         CALL AGSRESET                                                    497
         AG_SEEN = 1                                                      497
         AG_COLO = 920                                                    497
         AG_TITLE = 'OCYE'                                                497
         AG_EXNAME = 'ATTRIBUTE'                                          497
         AG_PARLIST = 'SEEN_COLO'                                         497
         CALL AXATTRIBUTE                                                 497
C    *                                                                    497
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=ANGLETOPLE  497
C    FT, PHI2=ANGLETOPLEFT+OUTERTUBELENGTH, DZ=(DSED-0.475)/2. <===       497
         CALL AGSRESET                                                    500
         AG_RMIN = DTUBEW                                                 500
         AG_RMAX = DTUBEW+THK                                             500
         AG_PHI1 = ANGLETOPLEFT                                           500
         AG_PHI2 = ANGLETOPLEFT+OUTERTUBELENGTH                           500
         AG_DZ = (DSED-0.475)/2.                                          500
         AG_TITLE = 'TUBS'                                                500
         AG_EXNAME = 'SHAPE'                                              500
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            500
         CALL AXSHAPE                                                     500
C    *                                                                    500
5571  CALL AGSPOPB                                                        501
      GO TO BLOCK_OCYE, (5570 ,5580)                                      501
C End of Local Procedure                                                  501
C ----------------------------------------------------------------------  502
C ------------ OCEA                                                       502
C Begin Local Procedure OCEA                                              503
C OCEA A OUTSIDE CYLINDER END PART A                                      503
5590  CONTINUE                                                            503
         AG_TITLE='OCEA'                                                  503
         AG_MODULE=MODULE                                                 503
         AG_MARK='V00'                                                    503
         CALL AGSPUSH                                                     503
         AG_MODULE=MODULE                                                 503
C    *                                                                    503
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          504
         CALL AGSRESET                                                    504
         AG_TITLE = 'CFRPMIX'                                             504
         AG_EXNAME = 'MATERIAL'                                           504
         AG_PARLIST = 'NONE'                                              504
         CALL AXMATERIAL                                                  504
C    *                                                                    504
C    Geant ATTRIBUTE : OCEA ===> SEEN=1 COLO=920 <===                     504
         CALL AGSRESET                                                    505
         AG_SEEN = 1                                                      505
         AG_COLO = 920                                                    505
         AG_TITLE = 'OCEA'                                                505
         AG_EXNAME = 'ATTRIBUTE'                                          505
         AG_PARLIST = 'SEEN_COLO'                                         505
         CALL AXATTRIBUTE                                                 505
C    *                                                                    505
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=180-67.41,  505
C     PHI2=180-40.58 DZ=(0.475)/2. <===                                   505
         CALL AGSRESET                                                    507
         AG_RMIN = DTUBEW                                                 507
         AG_RMAX = DTUBEW+THK                                             507
         AG_PHI1 = 180-67.41                                              507
         AG_PHI2 = 180-40.58                                              507
         AG_DZ = (0.475)/2.                                               507
         AG_TITLE = 'TUBS'                                                507
         AG_EXNAME = 'SHAPE'                                              507
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            507
         CALL AXSHAPE                                                     507
C    *                                                                    507
5591  CALL AGSPOPB                                                        508
      GO TO BLOCK_OCEA, (5590 ,5600)                                      508
C End of Local Procedure                                                  508
C ----------------------------------------------------------------------  509
C ------------ OCEB                                                       509
C Begin Local Procedure OCEB                                              510
C OCEB A OUTSIDE CYLINDER END PART B                                      510
5610  CONTINUE                                                            510
         AG_TITLE='OCEB'                                                  510
         AG_MODULE=MODULE                                                 510
         AG_MARK='V00'                                                    510
         CALL AGSPUSH                                                     510
         AG_MODULE=MODULE                                                 510
C    *                                                                    510
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          511
         CALL AGSRESET                                                    511
         AG_TITLE = 'CFRPMIX'                                             511
         AG_EXNAME = 'MATERIAL'                                           511
         AG_PARLIST = 'NONE'                                              511
         CALL AXMATERIAL                                                  511
C    *                                                                    511
C    Geant ATTRIBUTE : OCEB ===> SEEN=1 COLO=920 <===                     511
         CALL AGSRESET                                                    512
         AG_SEEN = 1                                                      512
         AG_COLO = 920                                                    512
         AG_TITLE = 'OCEB'                                                512
         AG_EXNAME = 'ATTRIBUTE'                                          512
         AG_PARLIST = 'SEEN_COLO'                                         512
         CALL AXATTRIBUTE                                                 512
C    *                                                                    512
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=180-31.41,  512
C     PHI2=180-4.58 DZ=(0.475)/2. <===                                    512
         CALL AGSRESET                                                    514
         AG_RMIN = DTUBEW                                                 514
         AG_RMAX = DTUBEW+THK                                             514
         AG_PHI1 = 180-31.41                                              514
         AG_PHI2 = 180-4.58                                               514
         AG_DZ = (0.475)/2.                                               514
         AG_TITLE = 'TUBS'                                                514
         AG_EXNAME = 'SHAPE'                                              514
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            514
         CALL AXSHAPE                                                     514
C    *                                                                    514
5611  CALL AGSPOPB                                                        515
      GO TO BLOCK_OCEB, (5610 ,5620)                                      515
C End of Local Procedure                                                  515
C ----------------------------------------------------------------------  516
C ------------ OCEC                                                       516
C Begin Local Procedure OCEC                                              517
C OCEC A OUTSIDE CYLINDER END PART C                                      517
5630  CONTINUE                                                            517
         AG_TITLE='OCEC'                                                  517
         AG_MODULE=MODULE                                                 517
         AG_MARK='V00'                                                    517
         CALL AGSPUSH                                                     517
         AG_MODULE=MODULE                                                 517
C    *                                                                    517
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          518
         CALL AGSRESET                                                    518
         AG_TITLE = 'CFRPMIX'                                             518
         AG_EXNAME = 'MATERIAL'                                           518
         AG_PARLIST = 'NONE'                                              518
         CALL AXMATERIAL                                                  518
C    *                                                                    518
C    Geant ATTRIBUTE : OCEC ===> SEEN=1 COLO=920 <===                     518
         CALL AGSRESET                                                    519
         AG_SEEN = 1                                                      519
         AG_COLO = 920                                                    519
         AG_TITLE = 'OCEC'                                                519
         AG_EXNAME = 'ATTRIBUTE'                                          519
         AG_PARLIST = 'SEEN_COLO'                                         519
         CALL AXATTRIBUTE                                                 519
C    *                                                                    519
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=180+4.55,   519
C    PHI2=180+31.41 DZ=(0.475)/2. <===                                    519
         CALL AGSRESET                                                    521
         AG_RMIN = DTUBEW                                                 521
         AG_RMAX = DTUBEW+THK                                             521
         AG_PHI1 = 180+4.55                                               521
         AG_PHI2 = 180+31.41                                              521
         AG_DZ = (0.475)/2.                                               521
         AG_TITLE = 'TUBS'                                                521
         AG_EXNAME = 'SHAPE'                                              521
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            521
         CALL AXSHAPE                                                     521
C    *                                                                    521
5631  CALL AGSPOPB                                                        522
      GO TO BLOCK_OCEC, (5630 ,5640)                                      522
C End of Local Procedure                                                  522
C ----------------------------------------------------------------------  523
C ------------ OCED                                                       523
C Begin Local Procedure OCED                                              524
C OCED A OUTSIDE CYLINDER END PART C                                      524
5650  CONTINUE                                                            524
         AG_TITLE='OCED'                                                  524
         AG_MODULE=MODULE                                                 524
         AG_MARK='V00'                                                    524
         CALL AGSPUSH                                                     524
         AG_MODULE=MODULE                                                 524
C    *                                                                    524
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          525
         CALL AGSRESET                                                    525
         AG_TITLE = 'CFRPMIX'                                             525
         AG_EXNAME = 'MATERIAL'                                           525
         AG_PARLIST = 'NONE'                                              525
         CALL AXMATERIAL                                                  525
C    *                                                                    525
C    Geant ATTRIBUTE : OCED ===> SEEN=1 COLO=920 <===                     525
         CALL AGSRESET                                                    526
         AG_SEEN = 1                                                      526
         AG_COLO = 920                                                    526
         AG_TITLE = 'OCED'                                                526
         AG_EXNAME = 'ATTRIBUTE'                                          526
         AG_PARLIST = 'SEEN_COLO'                                         526
         CALL AXATTRIBUTE                                                 526
C    *                                                                    526
C    Geant SHAPE : TUBS ===> RMIN=DTUBEW RMAX=DTUBEW+THK PHI1=180+40.58,  526
C     PHI2=180+67.41 DZ=(0.475)/2. <===                                   526
         CALL AGSRESET                                                    528
         AG_RMIN = DTUBEW                                                 528
         AG_RMAX = DTUBEW+THK                                             528
         AG_PHI1 = 180+40.58                                              528
         AG_PHI2 = 180+67.41                                              528
         AG_DZ = (0.475)/2.                                               528
         AG_TITLE = 'TUBS'                                                528
         AG_EXNAME = 'SHAPE'                                              528
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            528
         CALL AXSHAPE                                                     528
C    *                                                                    528
5651  CALL AGSPOPB                                                        529
      GO TO BLOCK_OCED, (5650 ,5660)                                      529
C End of Local Procedure                                                  529
C ----------------------------------------------------------------------  530
C ------------ UPTE                                                       530
C Begin Local Procedure UPTE                                              531
C UPTE A UP CORNER TOP END                                                531
5670  CONTINUE                                                            531
         AG_TITLE='UPTE'                                                  531
         AG_MODULE=MODULE                                                 531
         AG_MARK='V00'                                                    531
         CALL AGSPUSH                                                     531
         AG_MODULE=MODULE                                                 531
C    *                                                                    531
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          532
         CALL AGSRESET                                                    532
         AG_TITLE = 'CFRPMIX'                                             532
         AG_EXNAME = 'MATERIAL'                                           532
         AG_PARLIST = 'NONE'                                              532
         CALL AXMATERIAL                                                  532
C    *                                                                    532
C    Geant ATTRIBUTE : UPTE ===> SEEN=1 COLO=920 <===                     532
         CALL AGSRESET                                                    533
         AG_SEEN = 1                                                      533
         AG_COLO = 920                                                    533
         AG_TITLE = 'UPTE'                                                533
         AG_EXNAME = 'ATTRIBUTE'                                          533
         AG_PARLIST = 'SEEN_COLO'                                         533
         CALL AXATTRIBUTE                                                 533
C    *                                                                    533
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  533
C    I1=0.0 PHI2=ANGLETOPLEFT DZ=DSED/2. <===                             533
         CALL AGSRESET                                                    535
         AG_RMIN = ARCLENGTHOUT                                           535
         AG_RMAX = ARCLENGTHOUT+THK                                       535
         AG_PHI1 = 0.0                                                    535
         AG_PHI2 = ANGLETOPLEFT                                           535
         AG_DZ = DSED/2.                                                  535
         AG_TITLE = 'TUBS'                                                535
         AG_EXNAME = 'SHAPE'                                              535
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            535
         CALL AXSHAPE                                                     535
C    *                                                                    535
5671  CALL AGSPOPB                                                        536
      GO TO BLOCK_UPTE, (5670 ,5680)                                      536
C End of Local Procedure                                                  536
C ----------------------------------------------------------------------  537
C ------------ BOTE                                                       537
C Begin Local Procedure BOTE                                              538
C BOTE A BOTTOM CORNER TOP END                                            538
5690  CONTINUE                                                            538
         AG_TITLE='BOTE'                                                  538
         AG_MODULE=MODULE                                                 538
         AG_MARK='V00'                                                    538
         CALL AGSPUSH                                                     538
         AG_MODULE=MODULE                                                 538
C    *                                                                    538
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          539
         CALL AGSRESET                                                    539
         AG_TITLE = 'CFRPMIX'                                             539
         AG_EXNAME = 'MATERIAL'                                           539
         AG_PARLIST = 'NONE'                                              539
         CALL AXMATERIAL                                                  539
C    *                                                                    539
C    Geant ATTRIBUTE : BOTE ===> SEEN=1 COLO=920 <===                     539
         CALL AGSRESET                                                    540
         AG_SEEN = 1                                                      540
         AG_COLO = 920                                                    540
         AG_TITLE = 'BOTE'                                                540
         AG_EXNAME = 'ATTRIBUTE'                                          540
         AG_PARLIST = 'SEEN_COLO'                                         540
         CALL AXATTRIBUTE                                                 540
C    *                                                                    540
C    Geant SHAPE : TUBS ===> RMIN=ARCLENGTHOUT RMAX=ARCLENGTHOUT+THK, PH  540
C    I1=360-ANGLETOPLEFT PHI2=360 DZ=DSED/2. <===                         540
         CALL AGSRESET                                                    542
         AG_RMIN = ARCLENGTHOUT                                           542
         AG_RMAX = ARCLENGTHOUT+THK                                       542
         AG_PHI1 = 360-ANGLETOPLEFT                                       542
         AG_PHI2 = 360                                                    542
         AG_DZ = DSED/2.                                                  542
         AG_TITLE = 'TUBS'                                                542
         AG_EXNAME = 'SHAPE'                                              542
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            542
         CALL AXSHAPE                                                     542
C    *                                                                    542
5691  CALL AGSPOPB                                                        543
      GO TO BLOCK_BOTE, (5690 ,5700)                                      543
C End of Local Procedure                                                  543
C ----------------------------------------------------------------------  544
C ------------ SCAD                                                       544
C Begin Local Procedure SCAD                                              545
C SCAD SHORT CABLE SECTION D                                              545
6120  CONTINUE                                                            545
         AG_TITLE='SCAD'                                                  545
         AG_MODULE=MODULE                                                 545
         AG_MARK='V00'                                                    545
         CALL AGSPUSH                                                     545
         AG_MODULE=MODULE                                                 545
C    *                                                                    545
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     546
         CALL AGSRESET                                                    546
         AG_SEEN = 1                                                      546
         AG_COLO = 629                                                    546
         AG_TITLE = 'NONE'                                                546
         AG_EXNAME = 'ATTRIBUTE'                                          546
         AG_PARLIST = 'SEEN_COLO'                                         546
         CALL AXATTRIBUTE                                                 546
C    *                                                                    546
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           546
         CALL AGSRESET                                                    547
         AG_TITLE = 'COPPER'                                              547
         AG_EXNAME = 'MATERIAL'                                           547
         AG_PARLIST = 'NONE'                                              547
         CALL AXMATERIAL                                                  547
C    *                                                                    547
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=LE  547
C    NGTHCAS/2 <===                                                       547
         CALL AGSRESET                                                    549
         AG_RMIN = 0                                                      549
         AG_RMAX = RADCABLE                                               549
         AG_PHI1 = 0                                                      549
         AG_PHI2 = 360                                                    549
         AG_DZ = LENGTHCAS/2                                              549
         AG_TITLE = 'TUBS'                                                549
         AG_EXNAME = 'SHAPE'                                              549
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            549
         CALL AXSHAPE                                                     549
C    *                                                                    549
6121  CALL AGSPOPB                                                        550
      GO TO BLOCK_SCAD, (6120 ,6130)                                      550
C End of Local Procedure                                                  550
C ----------------------------------------------------------------------  551
C ------------ LCAD                                                       551
C Begin Local Procedure LCAD                                              552
C LCAD LONG CABLE SECTION D                                               552
6150  CONTINUE                                                            552
         AG_TITLE='LCAD'                                                  552
         AG_MODULE=MODULE                                                 552
         AG_MARK='V00'                                                    552
         CALL AGSPUSH                                                     552
         AG_MODULE=MODULE                                                 552
C    *                                                                    552
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     553
         CALL AGSRESET                                                    553
         AG_SEEN = 1                                                      553
         AG_COLO = 629                                                    553
         AG_TITLE = 'NONE'                                                553
         AG_EXNAME = 'ATTRIBUTE'                                          553
         AG_PARLIST = 'SEEN_COLO'                                         553
         CALL AXATTRIBUTE                                                 553
C    *                                                                    553
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           553
         CALL AGSRESET                                                    554
         AG_TITLE = 'COPPER'                                              554
         AG_EXNAME = 'MATERIAL'                                           554
         AG_PARLIST = 'NONE'                                              554
         CALL AXMATERIAL                                                  554
C    *                                                                    554
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=LE  554
C    NGTHCAL/2 <===                                                       554
         CALL AGSRESET                                                    556
         AG_RMIN = 0                                                      556
         AG_RMAX = RADCABLE                                               556
         AG_PHI1 = 0                                                      556
         AG_PHI2 = 360                                                    556
         AG_DZ = LENGTHCAL/2                                              556
         AG_TITLE = 'TUBS'                                                556
         AG_EXNAME = 'SHAPE'                                              556
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            556
         CALL AXSHAPE                                                     556
C    *                                                                    556
6151  CALL AGSPOPB                                                        557
      GO TO BLOCK_LCAD, (6150 ,6160)                                      557
C End of Local Procedure                                                  557
C ----------------------------------------------------------------------  558
C ------------ LSAA                                                       558
C Begin Local Procedure LSAA                                              559
C LSAA LONG CABLE SECTION A A                                             559
5710  CONTINUE                                                            559
         AG_TITLE='LSAA'                                                  559
         AG_MODULE=MODULE                                                 559
         AG_MARK='V00'                                                    559
         CALL AGSPUSH                                                     559
         AG_MODULE=MODULE                                                 559
C    *                                                                    559
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     560
         CALL AGSRESET                                                    560
         AG_SEEN = 1                                                      560
         AG_COLO = 629                                                    560
         AG_TITLE = 'NONE'                                                560
         AG_EXNAME = 'ATTRIBUTE'                                          560
         AG_PARLIST = 'SEEN_COLO'                                         560
         CALL AXATTRIBUTE                                                 560
C    *                                                                    560
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           560
         CALL AGSRESET                                                    561
         AG_TITLE = 'COPPER'                                              561
         AG_EXNAME = 'MATERIAL'                                           561
         AG_PARLIST = 'NONE'                                              561
         CALL AXMATERIAL                                                  561
C    *                                                                    561
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=6.  561
C    1331/2 <===                                                          561
         CALL AGSRESET                                                    563
         AG_RMIN = 0                                                      563
         AG_RMAX = RADCABLE                                               563
         AG_PHI1 = 0                                                      563
         AG_PHI2 = 360                                                    563
         AG_DZ = 6.1331/2                                                 563
         AG_TITLE = 'TUBS'                                                563
         AG_EXNAME = 'SHAPE'                                              563
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            563
         CALL AXSHAPE                                                     563
C    *                                                                    563
5711  CALL AGSPOPB                                                        564
      GO TO BLOCK_LSAA, (5710 ,5720)                                      564
C End of Local Procedure                                                  564
C ----------------------------------------------------------------------  565
C ------------ LSAB                                                       565
C Begin Local Procedure LSAB                                              566
C LSAB LONG CABLE SECTION A B                                             566
5730  CONTINUE                                                            566
         AG_TITLE='LSAB'                                                  566
         AG_MODULE=MODULE                                                 566
         AG_MARK='V00'                                                    566
         CALL AGSPUSH                                                     566
         AG_MODULE=MODULE                                                 566
C    *                                                                    566
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     567
         CALL AGSRESET                                                    567
         AG_SEEN = 1                                                      567
         AG_COLO = 629                                                    567
         AG_TITLE = 'NONE'                                                567
         AG_EXNAME = 'ATTRIBUTE'                                          567
         AG_PARLIST = 'SEEN_COLO'                                         567
         CALL AXATTRIBUTE                                                 567
C    *                                                                    567
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           567
         CALL AGSRESET                                                    568
         AG_TITLE = 'COPPER'                                              568
         AG_EXNAME = 'MATERIAL'                                           568
         AG_PARLIST = 'NONE'                                              568
         CALL AXMATERIAL                                                  568
C    *                                                                    568
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=6.  568
C    8955/2 <===                                                          568
         CALL AGSRESET                                                    570
         AG_RMIN = 0                                                      570
         AG_RMAX = RADCABLE                                               570
         AG_PHI1 = 0                                                      570
         AG_PHI2 = 360                                                    570
         AG_DZ = 6.8955/2                                                 570
         AG_TITLE = 'TUBS'                                                570
         AG_EXNAME = 'SHAPE'                                              570
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            570
         CALL AXSHAPE                                                     570
C    *                                                                    570
5731  CALL AGSPOPB                                                        571
      GO TO BLOCK_LSAB, (5730 ,5740)                                      571
C End of Local Procedure                                                  571
C ----------------------------------------------------------------------  572
C ------------ LSAC                                                       572
C Begin Local Procedure LSAC                                              573
C LSAC LONG CABLE SECTION A C                                             573
5750  CONTINUE                                                            573
         AG_TITLE='LSAC'                                                  573
         AG_MODULE=MODULE                                                 573
         AG_MARK='V00'                                                    573
         CALL AGSPUSH                                                     573
         AG_MODULE=MODULE                                                 573
C    *                                                                    573
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     574
         CALL AGSRESET                                                    574
         AG_SEEN = 1                                                      574
         AG_COLO = 629                                                    574
         AG_TITLE = 'NONE'                                                574
         AG_EXNAME = 'ATTRIBUTE'                                          574
         AG_PARLIST = 'SEEN_COLO'                                         574
         CALL AXATTRIBUTE                                                 574
C    *                                                                    574
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           574
         CALL AGSRESET                                                    575
         AG_TITLE = 'COPPER'                                              575
         AG_EXNAME = 'MATERIAL'                                           575
         AG_PARLIST = 'NONE'                                              575
         CALL AXMATERIAL                                                  575
C    *                                                                    575
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=7.  575
C    6018/2 <===                                                          575
         CALL AGSRESET                                                    577
         AG_RMIN = 0                                                      577
         AG_RMAX = RADCABLE                                               577
         AG_PHI1 = 0                                                      577
         AG_PHI2 = 360                                                    577
         AG_DZ = 7.6018/2                                                 577
         AG_TITLE = 'TUBS'                                                577
         AG_EXNAME = 'SHAPE'                                              577
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            577
         CALL AXSHAPE                                                     577
C    *                                                                    577
5751  CALL AGSPOPB                                                        578
      GO TO BLOCK_LSAC, (5750 ,5760)                                      578
C End of Local Procedure                                                  578
C ----------------------------------------------------------------------  579
C ------------ LSAD                                                       579
C Begin Local Procedure LSAD                                              580
C LSAD LONG CABLE SECTION A D                                             580
5770  CONTINUE                                                            580
         AG_TITLE='LSAD'                                                  580
         AG_MODULE=MODULE                                                 580
         AG_MARK='V00'                                                    580
         CALL AGSPUSH                                                     580
         AG_MODULE=MODULE                                                 580
C    *                                                                    580
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     581
         CALL AGSRESET                                                    581
         AG_SEEN = 1                                                      581
         AG_COLO = 629                                                    581
         AG_TITLE = 'NONE'                                                581
         AG_EXNAME = 'ATTRIBUTE'                                          581
         AG_PARLIST = 'SEEN_COLO'                                         581
         CALL AXATTRIBUTE                                                 581
C    *                                                                    581
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           581
         CALL AGSRESET                                                    582
         AG_TITLE = 'COPPER'                                              582
         AG_EXNAME = 'MATERIAL'                                           582
         AG_PARLIST = 'NONE'                                              582
         CALL AXMATERIAL                                                  582
C    *                                                                    582
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=8.  582
C    303/2 <===                                                           582
         CALL AGSRESET                                                    584
         AG_RMIN = 0                                                      584
         AG_RMAX = RADCABLE                                               584
         AG_PHI1 = 0                                                      584
         AG_PHI2 = 360                                                    584
         AG_DZ = 8.303/2                                                  584
         AG_TITLE = 'TUBS'                                                584
         AG_EXNAME = 'SHAPE'                                              584
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            584
         CALL AXSHAPE                                                     584
C    *                                                                    584
5771  CALL AGSPOPB                                                        585
      GO TO BLOCK_LSAD, (5770 ,5780)                                      585
C End of Local Procedure                                                  585
C ----------------------------------------------------------------------  586
C ------------ LSAE                                                       586
C Begin Local Procedure LSAE                                              587
C LSAE LONG CABLE SECTION A E                                             587
5790  CONTINUE                                                            587
         AG_TITLE='LSAE'                                                  587
         AG_MODULE=MODULE                                                 587
         AG_MARK='V00'                                                    587
         CALL AGSPUSH                                                     587
         AG_MODULE=MODULE                                                 587
C    *                                                                    587
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     588
         CALL AGSRESET                                                    588
         AG_SEEN = 1                                                      588
         AG_COLO = 629                                                    588
         AG_TITLE = 'NONE'                                                588
         AG_EXNAME = 'ATTRIBUTE'                                          588
         AG_PARLIST = 'SEEN_COLO'                                         588
         CALL AXATTRIBUTE                                                 588
C    *                                                                    588
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           588
         CALL AGSRESET                                                    589
         AG_TITLE = 'COPPER'                                              589
         AG_EXNAME = 'MATERIAL'                                           589
         AG_PARLIST = 'NONE'                                              589
         CALL AXMATERIAL                                                  589
C    *                                                                    589
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=9.  589
C    0772/2 <===                                                          589
         CALL AGSRESET                                                    591
         AG_RMIN = 0                                                      591
         AG_RMAX = RADCABLE                                               591
         AG_PHI1 = 0                                                      591
         AG_PHI2 = 360                                                    591
         AG_DZ = 9.0772/2                                                 591
         AG_TITLE = 'TUBS'                                                591
         AG_EXNAME = 'SHAPE'                                              591
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            591
         CALL AXSHAPE                                                     591
C    *                                                                    591
5791  CALL AGSPOPB                                                        592
      GO TO BLOCK_LSAE, (5790 ,5800)                                      592
C End of Local Procedure                                                  592
C ----------------------------------------------------------------------  593
C ------------ LSAF                                                       593
C Begin Local Procedure LSAF                                              594
C LSAF LONG CABLE SECTION A F                                             594
5810  CONTINUE                                                            594
         AG_TITLE='LSAF'                                                  594
         AG_MODULE=MODULE                                                 594
         AG_MARK='V00'                                                    594
         CALL AGSPUSH                                                     594
         AG_MODULE=MODULE                                                 594
C    *                                                                    594
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     595
         CALL AGSRESET                                                    595
         AG_SEEN = 1                                                      595
         AG_COLO = 629                                                    595
         AG_TITLE = 'NONE'                                                595
         AG_EXNAME = 'ATTRIBUTE'                                          595
         AG_PARLIST = 'SEEN_COLO'                                         595
         CALL AXATTRIBUTE                                                 595
C    *                                                                    595
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           595
         CALL AGSRESET                                                    596
         AG_TITLE = 'COPPER'                                              596
         AG_EXNAME = 'MATERIAL'                                           596
         AG_PARLIST = 'NONE'                                              596
         CALL AXMATERIAL                                                  596
C    *                                                                    596
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=9.  596
C    86/2 <===                                                            596
         CALL AGSRESET                                                    598
         AG_RMIN = 0                                                      598
         AG_RMAX = RADCABLE                                               598
         AG_PHI1 = 0                                                      598
         AG_PHI2 = 360                                                    598
         AG_DZ = 9.86/2                                                   598
         AG_TITLE = 'TUBS'                                                598
         AG_EXNAME = 'SHAPE'                                              598
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            598
         CALL AXSHAPE                                                     598
C    *                                                                    598
5811  CALL AGSPOPB                                                        599
      GO TO BLOCK_LSAF, (5810 ,5820)                                      599
C End of Local Procedure                                                  599
C ----------------------------------------------------------------------  600
C ------------ LSAG                                                       600
C Begin Local Procedure LSAG                                              601
C LSAG LONG CABLE SECTION A G                                             601
5830  CONTINUE                                                            601
         AG_TITLE='LSAG'                                                  601
         AG_MODULE=MODULE                                                 601
         AG_MARK='V00'                                                    601
         CALL AGSPUSH                                                     601
         AG_MODULE=MODULE                                                 601
C    *                                                                    601
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     602
         CALL AGSRESET                                                    602
         AG_SEEN = 1                                                      602
         AG_COLO = 629                                                    602
         AG_TITLE = 'NONE'                                                602
         AG_EXNAME = 'ATTRIBUTE'                                          602
         AG_PARLIST = 'SEEN_COLO'                                         602
         CALL AXATTRIBUTE                                                 602
C    *                                                                    602
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           602
         CALL AGSRESET                                                    603
         AG_TITLE = 'COPPER'                                              603
         AG_EXNAME = 'MATERIAL'                                           603
         AG_PARLIST = 'NONE'                                              603
         CALL AXMATERIAL                                                  603
C    *                                                                    603
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=10  603
C    .5702/2 <===                                                         603
         CALL AGSRESET                                                    605
         AG_RMIN = 0                                                      605
         AG_RMAX = RADCABLE                                               605
         AG_PHI1 = 0                                                      605
         AG_PHI2 = 360                                                    605
         AG_DZ = 10.5702/2                                                605
         AG_TITLE = 'TUBS'                                                605
         AG_EXNAME = 'SHAPE'                                              605
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            605
         CALL AXSHAPE                                                     605
C    *                                                                    605
5831  CALL AGSPOPB                                                        606
      GO TO BLOCK_LSAG, (5830 ,5840)                                      606
C End of Local Procedure                                                  606
C ----------------------------------------------------------------------  607
C ------------ LSAH                                                       607
C Begin Local Procedure LSAH                                              608
C LSAH LONG CABLE SECTION A H                                             608
5850  CONTINUE                                                            608
         AG_TITLE='LSAH'                                                  608
         AG_MODULE=MODULE                                                 608
         AG_MARK='V00'                                                    608
         CALL AGSPUSH                                                     608
         AG_MODULE=MODULE                                                 608
C    *                                                                    608
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     609
         CALL AGSRESET                                                    609
         AG_SEEN = 1                                                      609
         AG_COLO = 629                                                    609
         AG_TITLE = 'NONE'                                                609
         AG_EXNAME = 'ATTRIBUTE'                                          609
         AG_PARLIST = 'SEEN_COLO'                                         609
         CALL AXATTRIBUTE                                                 609
C    *                                                                    609
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           609
         CALL AGSRESET                                                    610
         AG_TITLE = 'COPPER'                                              610
         AG_EXNAME = 'MATERIAL'                                           610
         AG_PARLIST = 'NONE'                                              610
         CALL AXMATERIAL                                                  610
C    *                                                                    610
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=11  610
C    .2692/2 <===                                                         610
         CALL AGSRESET                                                    612
         AG_RMIN = 0                                                      612
         AG_RMAX = RADCABLE                                               612
         AG_PHI1 = 0                                                      612
         AG_PHI2 = 360                                                    612
         AG_DZ = 11.2692/2                                                612
         AG_TITLE = 'TUBS'                                                612
         AG_EXNAME = 'SHAPE'                                              612
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            612
         CALL AXSHAPE                                                     612
C    *                                                                    612
5851  CALL AGSPOPB                                                        613
      GO TO BLOCK_LSAH, (5850 ,5860)                                      613
C End of Local Procedure                                                  613
C ----------------------------------------------------------------------  614
C ------------ LSAI                                                       614
C Begin Local Procedure LSAI                                              615
C LSAI LONG CABLE SECTION A I                                             615
5870  CONTINUE                                                            615
         AG_TITLE='LSAI'                                                  615
         AG_MODULE=MODULE                                                 615
         AG_MARK='V00'                                                    615
         CALL AGSPUSH                                                     615
         AG_MODULE=MODULE                                                 615
C    *                                                                    615
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     616
         CALL AGSRESET                                                    616
         AG_SEEN = 1                                                      616
         AG_COLO = 629                                                    616
         AG_TITLE = 'NONE'                                                616
         AG_EXNAME = 'ATTRIBUTE'                                          616
         AG_PARLIST = 'SEEN_COLO'                                         616
         CALL AXATTRIBUTE                                                 616
C    *                                                                    616
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           616
         CALL AGSRESET                                                    617
         AG_TITLE = 'COPPER'                                              617
         AG_EXNAME = 'MATERIAL'                                           617
         AG_PARLIST = 'NONE'                                              617
         CALL AXMATERIAL                                                  617
C    *                                                                    617
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=12  617
C    .0376/2 <===                                                         617
         CALL AGSRESET                                                    619
         AG_RMIN = 0                                                      619
         AG_RMAX = RADCABLE                                               619
         AG_PHI1 = 0                                                      619
         AG_PHI2 = 360                                                    619
         AG_DZ = 12.0376/2                                                619
         AG_TITLE = 'TUBS'                                                619
         AG_EXNAME = 'SHAPE'                                              619
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            619
         CALL AXSHAPE                                                     619
C    *                                                                    619
5871  CALL AGSPOPB                                                        620
      GO TO BLOCK_LSAI, (5870 ,5880)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ LSAJ                                                       621
C Begin Local Procedure LSAJ                                              622
C LSAJ LONG CABLE SECTION A J                                             622
5890  CONTINUE                                                            622
         AG_TITLE='LSAJ'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     623
         CALL AGSRESET                                                    623
         AG_SEEN = 1                                                      623
         AG_COLO = 629                                                    623
         AG_TITLE = 'NONE'                                                623
         AG_EXNAME = 'ATTRIBUTE'                                          623
         AG_PARLIST = 'SEEN_COLO'                                         623
         CALL AXATTRIBUTE                                                 623
C    *                                                                    623
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           623
         CALL AGSRESET                                                    624
         AG_TITLE = 'COPPER'                                              624
         AG_EXNAME = 'MATERIAL'                                           624
         AG_PARLIST = 'NONE'                                              624
         CALL AXMATERIAL                                                  624
C    *                                                                    624
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=12  624
C    .7023/2 <===                                                         624
         CALL AGSRESET                                                    626
         AG_RMIN = 0                                                      626
         AG_RMAX = RADCABLE                                               626
         AG_PHI1 = 0                                                      626
         AG_PHI2 = 360                                                    626
         AG_DZ = 12.7023/2                                                626
         AG_TITLE = 'TUBS'                                                626
         AG_EXNAME = 'SHAPE'                                              626
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            626
         CALL AXSHAPE                                                     626
C    *                                                                    626
5891  CALL AGSPOPB                                                        627
      GO TO BLOCK_LSAJ, (5890 ,5900)                                      627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ LSAK                                                       628
C Begin Local Procedure LSAK                                              629
C LSAK LONG CABLE SECTION A K                                             629
5910  CONTINUE                                                            629
         AG_TITLE='LSAK'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     630
         CALL AGSRESET                                                    630
         AG_SEEN = 1                                                      630
         AG_COLO = 629                                                    630
         AG_TITLE = 'NONE'                                                630
         AG_EXNAME = 'ATTRIBUTE'                                          630
         AG_PARLIST = 'SEEN_COLO'                                         630
         CALL AXATTRIBUTE                                                 630
C    *                                                                    630
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           630
         CALL AGSRESET                                                    631
         AG_TITLE = 'COPPER'                                              631
         AG_EXNAME = 'MATERIAL'                                           631
         AG_PARLIST = 'NONE'                                              631
         CALL AXMATERIAL                                                  631
C    *                                                                    631
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=13  631
C    .426/2 <===                                                          631
         CALL AGSRESET                                                    633
         AG_RMIN = 0                                                      633
         AG_RMAX = RADCABLE                                               633
         AG_PHI1 = 0                                                      633
         AG_PHI2 = 360                                                    633
         AG_DZ = 13.426/2                                                 633
         AG_TITLE = 'TUBS'                                                633
         AG_EXNAME = 'SHAPE'                                              633
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            633
         CALL AXSHAPE                                                     633
C    *                                                                    633
5911  CALL AGSPOPB                                                        634
      GO TO BLOCK_LSAK, (5910 ,5920)                                      634
C End of Local Procedure                                                  634
C ----------------------------------------------------------------------  635
C ------------ LSAL                                                       635
C Begin Local Procedure LSAL                                              636
C LSAL LONG CABLE SECTION A L                                             636
5930  CONTINUE                                                            636
         AG_TITLE='LSAL'                                                  636
         AG_MODULE=MODULE                                                 636
         AG_MARK='V00'                                                    636
         CALL AGSPUSH                                                     636
         AG_MODULE=MODULE                                                 636
C    *                                                                    636
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     637
         CALL AGSRESET                                                    637
         AG_SEEN = 1                                                      637
         AG_COLO = 629                                                    637
         AG_TITLE = 'NONE'                                                637
         AG_EXNAME = 'ATTRIBUTE'                                          637
         AG_PARLIST = 'SEEN_COLO'                                         637
         CALL AXATTRIBUTE                                                 637
C    *                                                                    637
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           637
         CALL AGSRESET                                                    638
         AG_TITLE = 'COPPER'                                              638
         AG_EXNAME = 'MATERIAL'                                           638
         AG_PARLIST = 'NONE'                                              638
         CALL AXMATERIAL                                                  638
C    *                                                                    638
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=14  638
C    .0937/2 <===                                                         638
         CALL AGSRESET                                                    640
         AG_RMIN = 0                                                      640
         AG_RMAX = RADCABLE                                               640
         AG_PHI1 = 0                                                      640
         AG_PHI2 = 360                                                    640
         AG_DZ = 14.0937/2                                                640
         AG_TITLE = 'TUBS'                                                640
         AG_EXNAME = 'SHAPE'                                              640
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            640
         CALL AXSHAPE                                                     640
C    *                                                                    640
5931  CALL AGSPOPB                                                        641
      GO TO BLOCK_LSAL, (5930 ,5940)                                      641
C End of Local Procedure                                                  641
C ----------------------------------------------------------------------  642
C ------------ LSAM                                                       642
C Begin Local Procedure LSAM                                              643
C LSAM LONG CABLE SECTION A M                                             643
5950  CONTINUE                                                            643
         AG_TITLE='LSAM'                                                  643
         AG_MODULE=MODULE                                                 643
         AG_MARK='V00'                                                    643
         CALL AGSPUSH                                                     643
         AG_MODULE=MODULE                                                 643
C    *                                                                    643
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     644
         CALL AGSRESET                                                    644
         AG_SEEN = 1                                                      644
         AG_COLO = 629                                                    644
         AG_TITLE = 'NONE'                                                644
         AG_EXNAME = 'ATTRIBUTE'                                          644
         AG_PARLIST = 'SEEN_COLO'                                         644
         CALL AXATTRIBUTE                                                 644
C    *                                                                    644
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           644
         CALL AGSRESET                                                    645
         AG_TITLE = 'COPPER'                                              645
         AG_EXNAME = 'MATERIAL'                                           645
         AG_PARLIST = 'NONE'                                              645
         CALL AXMATERIAL                                                  645
C    *                                                                    645
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=14  645
C    .7107/2 <===                                                         645
         CALL AGSRESET                                                    647
         AG_RMIN = 0                                                      647
         AG_RMAX = RADCABLE                                               647
         AG_PHI1 = 0                                                      647
         AG_PHI2 = 360                                                    647
         AG_DZ = 14.7107/2                                                647
         AG_TITLE = 'TUBS'                                                647
         AG_EXNAME = 'SHAPE'                                              647
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            647
         CALL AXSHAPE                                                     647
C    *                                                                    647
5951  CALL AGSPOPB                                                        648
      GO TO BLOCK_LSAM, (5950 ,5960)                                      648
C End of Local Procedure                                                  648
C ----------------------------------------------------------------------  649
C ------------ LSAN                                                       649
C Begin Local Procedure LSAN                                              650
C LSAN LONG CABLE SECTION A N                                             650
5970  CONTINUE                                                            650
         AG_TITLE='LSAN'                                                  650
         AG_MODULE=MODULE                                                 650
         AG_MARK='V00'                                                    650
         CALL AGSPUSH                                                     650
         AG_MODULE=MODULE                                                 650
C    *                                                                    650
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     651
         CALL AGSRESET                                                    651
         AG_SEEN = 1                                                      651
         AG_COLO = 629                                                    651
         AG_TITLE = 'NONE'                                                651
         AG_EXNAME = 'ATTRIBUTE'                                          651
         AG_PARLIST = 'SEEN_COLO'                                         651
         CALL AXATTRIBUTE                                                 651
C    *                                                                    651
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           651
         CALL AGSRESET                                                    652
         AG_TITLE = 'COPPER'                                              652
         AG_EXNAME = 'MATERIAL'                                           652
         AG_PARLIST = 'NONE'                                              652
         CALL AXMATERIAL                                                  652
C    *                                                                    652
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=15  652
C    .3322/2 <===                                                         652
         CALL AGSRESET                                                    654
         AG_RMIN = 0                                                      654
         AG_RMAX = RADCABLE                                               654
         AG_PHI1 = 0                                                      654
         AG_PHI2 = 360                                                    654
         AG_DZ = 15.3322/2                                                654
         AG_TITLE = 'TUBS'                                                654
         AG_EXNAME = 'SHAPE'                                              654
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            654
         CALL AXSHAPE                                                     654
C    *                                                                    654
5971  CALL AGSPOPB                                                        655
      GO TO BLOCK_LSAN, (5970 ,5980)                                      655
C End of Local Procedure                                                  655
C ----------------------------------------------------------------------  656
C ------------ LSAO                                                       656
C Begin Local Procedure LSAO                                              657
C LSAO LONG CABLE SECTION A O                                             657
5990  CONTINUE                                                            657
         AG_TITLE='LSAO'                                                  657
         AG_MODULE=MODULE                                                 657
         AG_MARK='V00'                                                    657
         CALL AGSPUSH                                                     657
         AG_MODULE=MODULE                                                 657
C    *                                                                    657
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     658
         CALL AGSRESET                                                    658
         AG_SEEN = 1                                                      658
         AG_COLO = 629                                                    658
         AG_TITLE = 'NONE'                                                658
         AG_EXNAME = 'ATTRIBUTE'                                          658
         AG_PARLIST = 'SEEN_COLO'                                         658
         CALL AXATTRIBUTE                                                 658
C    *                                                                    658
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           658
         CALL AGSRESET                                                    659
         AG_TITLE = 'COPPER'                                              659
         AG_EXNAME = 'MATERIAL'                                           659
         AG_PARLIST = 'NONE'                                              659
         CALL AXMATERIAL                                                  659
C    *                                                                    659
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=16  659
C    .0452/2 <===                                                         659
         CALL AGSRESET                                                    661
         AG_RMIN = 0                                                      661
         AG_RMAX = RADCABLE                                               661
         AG_PHI1 = 0                                                      661
         AG_PHI2 = 360                                                    661
         AG_DZ = 16.0452/2                                                661
         AG_TITLE = 'TUBS'                                                661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            661
         CALL AXSHAPE                                                     661
C    *                                                                    661
5991  CALL AGSPOPB                                                        662
      GO TO BLOCK_LSAO, (5990 ,6000)                                      662
C End of Local Procedure                                                  662
C ----------------------------------------------------------------------  663
C ------------ LSAP                                                       663
C Begin Local Procedure LSAP                                              664
C LSAP LONG CABLE SECTION A P                                             664
6010  CONTINUE                                                            664
         AG_TITLE='LSAP'                                                  664
         AG_MODULE=MODULE                                                 664
         AG_MARK='V00'                                                    664
         CALL AGSPUSH                                                     664
         AG_MODULE=MODULE                                                 664
C    *                                                                    664
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     665
         CALL AGSRESET                                                    665
         AG_SEEN = 1                                                      665
         AG_COLO = 629                                                    665
         AG_TITLE = 'NONE'                                                665
         AG_EXNAME = 'ATTRIBUTE'                                          665
         AG_PARLIST = 'SEEN_COLO'                                         665
         CALL AXATTRIBUTE                                                 665
C    *                                                                    665
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           665
         CALL AGSRESET                                                    666
         AG_TITLE = 'COPPER'                                              666
         AG_EXNAME = 'MATERIAL'                                           666
         AG_PARLIST = 'NONE'                                              666
         CALL AXMATERIAL                                                  666
C    *                                                                    666
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=16  666
C    .7242/2 <===                                                         666
         CALL AGSRESET                                                    668
         AG_RMIN = 0                                                      668
         AG_RMAX = RADCABLE                                               668
         AG_PHI1 = 0                                                      668
         AG_PHI2 = 360                                                    668
         AG_DZ = 16.7242/2                                                668
         AG_TITLE = 'TUBS'                                                668
         AG_EXNAME = 'SHAPE'                                              668
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            668
         CALL AXSHAPE                                                     668
C    *                                                                    668
6011  CALL AGSPOPB                                                        669
      GO TO BLOCK_LSAP, (6010 ,6020)                                      669
C End of Local Procedure                                                  669
C ----------------------------------------------------------------------  670
C ------------ LSAQ                                                       670
C Begin Local Procedure LSAQ                                              671
C LSAQ LONG CABLE SECTION A Q                                             671
6030  CONTINUE                                                            671
         AG_TITLE='LSAQ'                                                  671
         AG_MODULE=MODULE                                                 671
         AG_MARK='V00'                                                    671
         CALL AGSPUSH                                                     671
         AG_MODULE=MODULE                                                 671
C    *                                                                    671
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     672
         CALL AGSRESET                                                    672
         AG_SEEN = 1                                                      672
         AG_COLO = 629                                                    672
         AG_TITLE = 'NONE'                                                672
         AG_EXNAME = 'ATTRIBUTE'                                          672
         AG_PARLIST = 'SEEN_COLO'                                         672
         CALL AXATTRIBUTE                                                 672
C    *                                                                    672
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           672
         CALL AGSRESET                                                    673
         AG_TITLE = 'COPPER'                                              673
         AG_EXNAME = 'MATERIAL'                                           673
         AG_PARLIST = 'NONE'                                              673
         CALL AXMATERIAL                                                  673
C    *                                                                    673
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=17  673
C    .3576/2 <===                                                         673
         CALL AGSRESET                                                    675
         AG_RMIN = 0                                                      675
         AG_RMAX = RADCABLE                                               675
         AG_PHI1 = 0                                                      675
         AG_PHI2 = 360                                                    675
         AG_DZ = 17.3576/2                                                675
         AG_TITLE = 'TUBS'                                                675
         AG_EXNAME = 'SHAPE'                                              675
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            675
         CALL AXSHAPE                                                     675
C    *                                                                    675
6031  CALL AGSPOPB                                                        676
      GO TO BLOCK_LSAQ, (6030 ,6040)                                      676
C End of Local Procedure                                                  676
C ----------------------------------------------------------------------  677
C ------------ LSAR                                                       677
C Begin Local Procedure LSAR                                              678
C LSAR LONG CABLE SECTION A R                                             678
6050  CONTINUE                                                            678
         AG_TITLE='LSAR'                                                  678
         AG_MODULE=MODULE                                                 678
         AG_MARK='V00'                                                    678
         CALL AGSPUSH                                                     678
         AG_MODULE=MODULE                                                 678
C    *                                                                    678
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     679
         CALL AGSRESET                                                    679
         AG_SEEN = 1                                                      679
         AG_COLO = 629                                                    679
         AG_TITLE = 'NONE'                                                679
         AG_EXNAME = 'ATTRIBUTE'                                          679
         AG_PARLIST = 'SEEN_COLO'                                         679
         CALL AXATTRIBUTE                                                 679
C    *                                                                    679
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           679
         CALL AGSRESET                                                    680
         AG_TITLE = 'COPPER'                                              680
         AG_EXNAME = 'MATERIAL'                                           680
         AG_PARLIST = 'NONE'                                              680
         CALL AXMATERIAL                                                  680
C    *                                                                    680
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=18  680
C    .0012/2 <===                                                         680
         CALL AGSRESET                                                    682
         AG_RMIN = 0                                                      682
         AG_RMAX = RADCABLE                                               682
         AG_PHI1 = 0                                                      682
         AG_PHI2 = 360                                                    682
         AG_DZ = 18.0012/2                                                682
         AG_TITLE = 'TUBS'                                                682
         AG_EXNAME = 'SHAPE'                                              682
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            682
         CALL AXSHAPE                                                     682
C    *                                                                    682
6051  CALL AGSPOPB                                                        683
      GO TO BLOCK_LSAR, (6050 ,6060)                                      683
C End of Local Procedure                                                  683
C ----------------------------------------------------------------------  684
C ------------ LSAS                                                       684
C Begin Local Procedure LSAS                                              685
C LSAS LONG CABLE SECTION A S                                             685
6070  CONTINUE                                                            685
         AG_TITLE='LSAS'                                                  685
         AG_MODULE=MODULE                                                 685
         AG_MARK='V00'                                                    685
         CALL AGSPUSH                                                     685
         AG_MODULE=MODULE                                                 685
C    *                                                                    685
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     686
         CALL AGSRESET                                                    686
         AG_SEEN = 1                                                      686
         AG_COLO = 629                                                    686
         AG_TITLE = 'NONE'                                                686
         AG_EXNAME = 'ATTRIBUTE'                                          686
         AG_PARLIST = 'SEEN_COLO'                                         686
         CALL AXATTRIBUTE                                                 686
C    *                                                                    686
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           686
         CALL AGSRESET                                                    687
         AG_TITLE = 'COPPER'                                              687
         AG_EXNAME = 'MATERIAL'                                           687
         AG_PARLIST = 'NONE'                                              687
         CALL AXMATERIAL                                                  687
C    *                                                                    687
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=18  687
C    .7119/2 <===                                                         687
         CALL AGSRESET                                                    689
         AG_RMIN = 0                                                      689
         AG_RMAX = RADCABLE                                               689
         AG_PHI1 = 0                                                      689
         AG_PHI2 = 360                                                    689
         AG_DZ = 18.7119/2                                                689
         AG_TITLE = 'TUBS'                                                689
         AG_EXNAME = 'SHAPE'                                              689
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            689
         CALL AXSHAPE                                                     689
C    *                                                                    689
6071  CALL AGSPOPB                                                        690
      GO TO BLOCK_LSAS, (6070 ,6080)                                      690
C End of Local Procedure                                                  690
C ----------------------------------------------------------------------  691
C ------------ LSAT                                                       691
C Begin Local Procedure LSAT                                              692
C LSAT LONG CABLE SECTION A T                                             692
6090  CONTINUE                                                            692
         AG_TITLE='LSAT'                                                  692
         AG_MODULE=MODULE                                                 692
         AG_MARK='V00'                                                    692
         CALL AGSPUSH                                                     692
         AG_MODULE=MODULE                                                 692
C    *                                                                    692
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     693
         CALL AGSRESET                                                    693
         AG_SEEN = 1                                                      693
         AG_COLO = 629                                                    693
         AG_TITLE = 'NONE'                                                693
         AG_EXNAME = 'ATTRIBUTE'                                          693
         AG_PARLIST = 'SEEN_COLO'                                         693
         CALL AXATTRIBUTE                                                 693
C    *                                                                    693
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           693
         CALL AGSRESET                                                    694
         AG_TITLE = 'COPPER'                                              694
         AG_EXNAME = 'MATERIAL'                                           694
         AG_PARLIST = 'NONE'                                              694
         CALL AXMATERIAL                                                  694
C    *                                                                    694
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=19  694
C    .3957/2 <===                                                         694
         CALL AGSRESET                                                    696
         AG_RMIN = 0                                                      696
         AG_RMAX = RADCABLE                                               696
         AG_PHI1 = 0                                                      696
         AG_PHI2 = 360                                                    696
         AG_DZ = 19.3957/2                                                696
         AG_TITLE = 'TUBS'                                                696
         AG_EXNAME = 'SHAPE'                                              696
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            696
         CALL AXSHAPE                                                     696
C    *                                                                    696
6091  CALL AGSPOPB                                                        697
      GO TO BLOCK_LSAT, (6090 ,6100)                                      697
C End of Local Procedure                                                  697
      WRITE(LOUT,6170)'===>>  [End AgML Module DtubGeo1]  <<==='          697
6170  FORMAT(A32,/,/)                                                     697
C MODULE DTUBGEO1                                                         699
      END                                                                 699
