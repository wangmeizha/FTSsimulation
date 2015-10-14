C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO4                                                 1
C SVTTGEO4 IS THE SVT GEOMETRY FOR STAR: CORRECTED ELECTRONICS            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SVTTGEO4'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE SVT GEOMETRY FOR STAR: CORRECTED ELECTRONICS  1
     *'/                                                                  1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR MAXIM POTEKHIN                                                   1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 24 MAR 2004                                                     2
      CHARACTER*40 CREATED /'24 MAR 2004'/                                3
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
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
C - GEANT description of logical units                                    5
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            6
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      6
      COMMON/GCMAIL/ CHMAIL                                               6
      CHARACTER*132 CHMAIL                                                6
      INTEGER BLOCK_SVTT                                                  6
      INTEGER BLOCK_SLYD                                                  12
      INTEGER BLOCK_SLSD                                                  12
      INTEGER BLOCK_SLDI                                                  12
      INTEGER BLOCK_STLI                                                  12
      INTEGER BLOCK_STSI                                                  12
      INTEGER BLOCK_SVTD                                                  12
      INTEGER BLOCK_SBER                                                  12
      INTEGER BLOCK_STAB                                                  12
      INTEGER BLOCK_STRU                                                  12
      INTEGER BLOCK_SPCB                                                  12
      INTEGER BLOCK_SRHC                                                  12
      INTEGER BLOCK_SBWC                                                  12
      INTEGER BLOCK_SWCM                                                  12
      INTEGER BLOCK_SXAI                                                  12
      INTEGER BLOCK_SXBI                                                  12
      INTEGER BLOCK_SELE                                                  12
      INTEGER BLOCK_SWCH                                                  12
      INTEGER BLOCK_SWCW                                                  12
      INTEGER BLOCK_SWCS                                                  12
      INTEGER BLOCK_SBOI                                                  12
      INTEGER BLOCK_SAGP                                                  12
      INTEGER BLOCK_SDYE                                                  12
      INTEGER BLOCK_SECA                                                  12
      INTEGER BLOCK_SIRP                                                  12
      INTEGER BLOCK_SIRT                                                  12
      INTEGER BLOCK_SOER                                                  12
      INTEGER BLOCK_SCON                                                  12
      INTEGER BLOCK_SROD                                                  12
      INTEGER BLOCK_SGRA                                                  12
      INTEGER BLOCK_STAP                                                  12
      INTEGER BLOCK_STAC                                                  12
      INTEGER BLOCK_SHLA                                                  12
      INTEGER BLOCK_SHLB                                                  12
      INTEGER BLOCK_SHMA                                                  12
      INTEGER BLOCK_SHMB                                                  12
      INTEGER BLOCK_SWHO                                                  12
      INTEGER BLOCK_SHWA                                                  12
      INTEGER BLOCK_SCMY                                                  12
      INTEGER BLOCK_SCAL                                                  12
      INTEGER BLOCK_SWMM                                                  12
      INTEGER BLOCK_SWMB                                                  12
      INTEGER BLOCK_SWRP                                                  12
      INTEGER BLOCK_SXRL                                                  12
      INTEGER BLOCK_SYRU                                                  12
      INTEGER BLOCK_SWMT                                                  12
      INTEGER BLOCK_SWMS                                                  12
      INTEGER BLOCK_SWMW                                                  12
      INTEGER BLOCK_SOTB                                                  12
      INTEGER BLOCK_SITB                                                  12
      INTEGER BLOCK_SBRG                                                  12
      INTEGER BLOCK_SBRM                                                  12
      INTEGER BLOCK_SBRI                                                  12
      INTEGER BLOCK_SOES                                                  12
      INTEGER BLOCK_SIES                                                  12
      INTEGER BLOCK_SOSM                                                  12
      INTEGER BLOCK_SISM                                                  12
      INTEGER BLOCK_SCRW                                                  12
      INTEGER BLOCK_SGLA                                                  12
      INTEGER BLOCK_SBSP                                                  12
      INTEGER BLOCK_SAKM                                                  12
      INTEGER BLOCK_SCKM                                                  12
      INTEGER BLOCK_SBSR                                                  12
      INTEGER BLOCK_SBCR                                                  12
      INTEGER BLOCK_SBRX                                                  12
      INTEGER BLOCK_SBRL                                                  12
      INTEGER BLOCK_SBMM                                                  12
      INTEGER BLOCK_SBMO                                                  12
      INTEGER BLOCK_SBMI                                                  12
      INTEGER BLOCK_SMRD                                                  12
      INTEGER BLOCK_SALM                                                  12
      INTEGER BLOCK_SISH                                                  12
      INTEGER BLOCK_SSSH                                                  12
      INTEGER BLOCK_SOSH                                                  12
      INTEGER BLOCK_SCBM                                                  12
      INTEGER BLOCK_SCBL                                                  12
      INTEGER BLOCK_SFED                                                  12
      INTEGER BLOCK_SPLS                                                  12
      INTEGER BLOCK_SOUM                                                  12
      INTEGER BLOCK_SOUR                                                  12
C -                                                                       14
      REAL SVTG_VERSION                                                   14
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            14
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       14
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          14
     *MSVTG_VERSION)                                                      14
      CHARACTER CSVTG_VERSION*40/' '/                                     14
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             14
      INTEGER MSVTG_VERSION(2)/1,1/                                       14
      REAL RE_SVTG_VERSION                                                14
C -                                                                       14
      INTEGER SVTG_NLAYER                                                 14
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              14
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        14
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           14
     *MSVTG_NLAYER)                                                       14
      CHARACTER CSVTG_NLAYER*40/' '/                                      14
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               14
      INTEGER MSVTG_NLAYER(2)/1,1/                                        14
      REAL RE_SVTG_NLAYER                                                 14
C -                                                                       14
      REAL SVTG_RSIZEMIN                                                  14
      EQUIVALENCE (BANK_SVTG(5),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          14
     *(NAMES_SVTG(3),NSVTG_RSIZEMIN)                                      14
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_RSIZEMIN),(MAP_SVTG(1,3),         14
     *MSVTG_RSIZEMIN)                                                     14
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    14
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           14
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      14
      REAL RE_SVTG_RSIZEMIN                                               14
C -                                                                       14
      REAL SVTG_RSIZEMAX                                                  14
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          14
     *(NAMES_SVTG(4),NSVTG_RSIZEMAX)                                      14
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMAX),(MAP_SVTG(1,4),         14
     *MSVTG_RSIZEMAX)                                                     14
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    14
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           14
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      14
      REAL RE_SVTG_RSIZEMAX                                               14
C -                                                                       14
      REAL SVTG_ZSIZEMAX                                                  14
      EQUIVALENCE (BANK_SVTG(7),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          14
     *(NAMES_SVTG(5),NSVTG_ZSIZEMAX)                                      14
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_ZSIZEMAX),(MAP_SVTG(1,5),         14
     *MSVTG_ZSIZEMAX)                                                     14
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    14
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           14
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      14
      REAL RE_SVTG_ZSIZEMAX                                               14
C -                                                                       14
      REAL SVTG_ANGOFF                                                    14
      EQUIVALENCE (BANK_SVTG(8),SVTG_ANGOFF,RE_SVTG_ANGOFF),              14
     *(NAMES_SVTG(6),NSVTG_ANGOFF)                                        14
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ANGOFF),(MAP_SVTG(1,6),           14
     *MSVTG_ANGOFF)                                                       14
      CHARACTER CSVTG_ANGOFF*40/' '/                                      14
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               14
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        14
      REAL RE_SVTG_ANGOFF                                                 14
C -                                                                       14
      REAL SVTG_SUPPORTVER                                                14
      EQUIVALENCE (BANK_SVTG(9),SVTG_SUPPORTVER,RE_SVTG_SUPPORTVER),      14
     *(NAMES_SVTG(7),NSVTG_SUPPORTVER)                                    14
      EQUIVALENCE (COMMEN_SVTG(7),CSVTG_SUPPORTVER),(MAP_SVTG(1,7),       14
     *MSVTG_SUPPORTVER)                                                   14
      CHARACTER CSVTG_SUPPORTVER*40/' '/                                  14
      CHARACTER NSVTG_SUPPORTVER*12/'R.SUPPORTVER'/                       14
      INTEGER MSVTG_SUPPORTVER(2)/1,1/                                    14
      REAL RE_SVTG_SUPPORTVER                                             14
C -                                                                       14
      REAL SVTG_CONEVER                                                   14
      EQUIVALENCE (BANK_SVTG(10),SVTG_CONEVER,RE_SVTG_CONEVER),           14
     *(NAMES_SVTG(8),NSVTG_CONEVER)                                       14
      EQUIVALENCE (COMMEN_SVTG(8),CSVTG_CONEVER),(MAP_SVTG(1,8),          14
     *MSVTG_CONEVER)                                                      14
      CHARACTER CSVTG_CONEVER*40/' '/                                     14
      CHARACTER NSVTG_CONEVER*12/'R.CONEVER'/                             14
      INTEGER MSVTG_CONEVER(2)/1,1/                                       14
      REAL RE_SVTG_CONEVER                                                14
C -                                                                       14
      REAL SVTG_IFMANY                                                    14
      EQUIVALENCE (BANK_SVTG(11),SVTG_IFMANY,RE_SVTG_IFMANY),             14
     *(NAMES_SVTG(9),NSVTG_IFMANY)                                        14
      EQUIVALENCE (COMMEN_SVTG(9),CSVTG_IFMANY),(MAP_SVTG(1,9),           14
     *MSVTG_IFMANY)                                                       14
      CHARACTER CSVTG_IFMANY*40/' '/                                      14
      CHARACTER NSVTG_IFMANY*12/'R.IFMANY'/                               14
      INTEGER MSVTG_IFMANY(2)/1,1/                                        14
      REAL RE_SVTG_IFMANY                                                 14
C -                                                                       14
      REAL SVTG_NMIN                                                      14
      EQUIVALENCE (BANK_SVTG(12),SVTG_NMIN,RE_SVTG_NMIN),                 14
     *(NAMES_SVTG(10),NSVTG_NMIN)                                         14
      EQUIVALENCE (COMMEN_SVTG(10),CSVTG_NMIN),(MAP_SVTG(1,10),           14
     *MSVTG_NMIN)                                                         14
      CHARACTER CSVTG_NMIN*40/' '/                                        14
      CHARACTER NSVTG_NMIN*12/'R.NMIN'/                                   14
      INTEGER MSVTG_NMIN(2)/1,1/                                          14
      REAL RE_SVTG_NMIN                                                   14
C -                                                                       14
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                14
      PARAMETER (LEN_SVTG=12)                                             14
      PARAMETER (LLEN_SVTG=10)                                            14
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           14
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        14
      DATA IDNUM_SVTG/10*0/                                               14
      REAL BANK_SVTG(LEN_SVTG)                                            14
      CHARACTER*60 BPATH_SVTG/' '/                                        14
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          14
     *BPATH_SVTG                                                          14
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             14
C *                                                                       14
C -                                                                       16
      REAL SWCA_VERSION                                                   16
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            16
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       16
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          16
     *MSWCA_VERSION)                                                      16
      CHARACTER CSWCA_VERSION*40/' '/                                     16
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             16
      INTEGER MSWCA_VERSION(2)/1,1/                                       16
      REAL RE_SWCA_VERSION                                                16
C -                                                                       16
      REAL SWCA_LENGTH                                                    16
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              16
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        16
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           16
     *MSWCA_LENGTH)                                                       16
      CHARACTER CSWCA_LENGTH*40/' '/                                      16
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               16
      INTEGER MSWCA_LENGTH(2)/1,1/                                        16
      REAL RE_SWCA_LENGTH                                                 16
C -                                                                       16
      REAL SWCA_WAFERWID                                                  16
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          16
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      16
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         16
     *MSWCA_WAFERWID)                                                     16
      CHARACTER CSWCA_WAFERWID*40/' '/                                    16
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           16
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      16
      REAL RE_SWCA_WAFERWID                                               16
C -                                                                       16
      REAL SWCA_WAFERLEN                                                  16
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          16
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      16
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         16
     *MSWCA_WAFERLEN)                                                     16
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    16
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           16
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      16
      REAL RE_SWCA_WAFERLEN                                               16
C -                                                                       16
      REAL SWCA_WAFERTHK                                                  16
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          16
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      16
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         16
     *MSWCA_WAFERTHK)                                                     16
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    16
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           16
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      16
      REAL RE_SWCA_WAFERTHK                                               16
C -                                                                       16
      REAL SWCA_ROHATHK                                                   16
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            16
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       16
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          16
     *MSWCA_ROHATHK)                                                      16
      CHARACTER CSWCA_ROHATHK*40/' '/                                     16
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             16
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       16
      REAL RE_SWCA_ROHATHK                                                16
C -                                                                       16
      REAL SWCA_WAFCARWD                                                  16
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          16
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      16
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         16
     *MSWCA_WAFCARWD)                                                     16
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    16
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           16
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      16
      REAL RE_SWCA_WAFCARWD                                               16
C -                                                                       16
      REAL SWCA_WAFCARTH                                                  16
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         16
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      16
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         16
     *MSWCA_WAFCARTH)                                                     16
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    16
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           16
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      16
      REAL RE_SWCA_WAFCARTH                                               16
C -                                                                       16
      REAL SWCA_WAFERGAP                                                  16
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         16
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      16
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         16
     *MSWCA_WAFERGAP)                                                     16
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    16
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           16
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      16
      REAL RE_SWCA_WAFERGAP                                               16
C -                                                                       16
      REAL SWCA_DRIFT                                                     16
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               16
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        16
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          16
     *MSWCA_DRIFT)                                                        16
      CHARACTER CSWCA_DRIFT*40/' '/                                       16
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 16
      INTEGER MSWCA_DRIFT(2)/1,1/                                         16
      REAL RE_SWCA_DRIFT                                                  16
C -                                                                       16
      REAL SWCA_STRUTLEN                                                  16
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         16
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     16
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       16
     *MSWCA_STRUTLEN)                                                     16
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    16
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           16
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      16
      REAL RE_SWCA_STRUTLEN                                               16
C -                                                                       16
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                16
      PARAMETER (LEN_SWCA=13)                                             16
      PARAMETER (LLEN_SWCA=11)                                            16
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           16
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        16
      DATA IDNUM_SWCA/10*0/                                               16
      REAL BANK_SWCA(LEN_SWCA)                                            16
      CHARACTER*60 BPATH_SWCA/' '/                                        16
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          16
     *BPATH_SWCA                                                          16
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             16
C *                                                                       16
C -                                                                       19
      REAL SSUP_VERSION                                                   19
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            19
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       19
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          19
     *MSSUP_VERSION)                                                      19
      CHARACTER CSSUP_VERSION*40/' '/                                     19
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             19
      INTEGER MSSUP_VERSION(2)/1,1/                                       19
      REAL RE_SSUP_VERSION                                                19
C -                                                                       19
      REAL SSUP_CABTHK                                                    19
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              19
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        19
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           19
     *MSSUP_CABTHK)                                                       19
      CHARACTER CSSUP_CABTHK*40/' '/                                      19
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               19
      INTEGER MSSUP_CABTHK(2)/1,1/                                        19
      REAL RE_SSUP_CABTHK                                                 19
C -                                                                       19
      REAL SSUP_HOSRMN                                                    19
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              19
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        19
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           19
     *MSSUP_HOSRMN)                                                       19
      CHARACTER CSSUP_HOSRMN*40/' '/                                      19
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               19
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        19
      REAL RE_SSUP_HOSRMN                                                 19
C -                                                                       19
      REAL SSUP_HOSRMX                                                    19
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              19
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        19
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           19
     *MSSUP_HOSRMX)                                                       19
      CHARACTER CSSUP_HOSRMX*40/' '/                                      19
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               19
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        19
      REAL RE_SSUP_HOSRMX                                                 19
C -                                                                       19
      REAL SSUP_NHOSES                                                    19
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              19
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        19
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           19
     *MSSUP_NHOSES)                                                       19
      CHARACTER CSSUP_NHOSES*40/' '/                                      19
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               19
      INTEGER MSSUP_NHOSES(2)/1,1/                                        19
      REAL RE_SSUP_NHOSES                                                 19
C -                                                                       19
      REAL SSUP_WRPMYTHK                                                  19
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          19
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      19
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         19
     *MSSUP_WRPMYTHK)                                                     19
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    19
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           19
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      19
      REAL RE_SSUP_WRPMYTHK                                               19
C -                                                                       19
      REAL SSUP_WRPALTHK                                                  19
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          19
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      19
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         19
     *MSSUP_WRPALTHK)                                                     19
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    19
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           19
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      19
      REAL RE_SSUP_WRPALTHK                                               19
C -                                                                       19
      REAL SSUP_GRPHTHK                                                   19
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           19
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       19
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          19
     *MSSUP_GRPHTHK)                                                      19
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     19
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             19
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       19
      REAL RE_SSUP_GRPHTHK                                                19
C -                                                                       19
      REAL SSUP_CONE1ZMN                                                  19
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         19
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      19
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         19
     *MSSUP_CONE1ZMN)                                                     19
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    19
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           19
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      19
      REAL RE_SSUP_CONE1ZMN                                               19
C -                                                                       19
      REAL SSUP_RODLEN                                                    19
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             19
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       19
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         19
     *MSSUP_RODLEN)                                                       19
      CHARACTER CSSUP_RODLEN*40/' '/                                      19
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               19
      INTEGER MSSUP_RODLEN(2)/1,1/                                        19
      REAL RE_SSUP_RODLEN                                                 19
C -                                                                       19
      REAL SSUP_RODDIST                                                   19
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           19
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      19
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        19
     *MSSUP_RODDIST)                                                      19
      CHARACTER CSSUP_RODDIST*40/' '/                                     19
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             19
      INTEGER MSSUP_RODDIST(2)/1,1/                                       19
      REAL RE_SSUP_RODDIST                                                19
C -                                                                       19
      REAL SSUP_RODID                                                     19
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               19
     *(NAMES_SSUP(12),NSSUP_RODID)                                        19
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          19
     *MSSUP_RODID)                                                        19
      CHARACTER CSSUP_RODID*40/' '/                                       19
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 19
      INTEGER MSSUP_RODID(2)/1,1/                                         19
      REAL RE_SSUP_RODID                                                  19
C -                                                                       19
      REAL SSUP_RODOD                                                     19
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               19
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        19
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          19
     *MSSUP_RODOD)                                                        19
      CHARACTER CSSUP_RODOD*40/' '/                                       19
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 19
      INTEGER MSSUP_RODOD(2)/1,1/                                         19
      REAL RE_SSUP_RODOD                                                  19
C -                                                                       19
      REAL SSUP_CON1IDMN                                                  19
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         19
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     19
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       19
     *MSSUP_CON1IDMN)                                                     19
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    19
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           19
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      19
      REAL RE_SSUP_CON1IDMN                                               19
C -                                                                       19
      REAL SSUP_CON3IDMN                                                  19
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         19
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     19
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       19
     *MSSUP_CON3IDMN)                                                     19
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    19
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           19
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      19
      REAL RE_SSUP_CON3IDMN                                               19
C -                                                                       19
      REAL SSUP_CON4IDMN                                                  19
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         19
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     19
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       19
     *MSSUP_CON4IDMN)                                                     19
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    19
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           19
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      19
      REAL RE_SSUP_CON4IDMN                                               19
C -                                                                       19
      REAL SSUP_CON4IDMX                                                  19
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         19
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     19
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       19
     *MSSUP_CON4IDMX)                                                     19
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    19
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           19
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      19
      REAL RE_SSUP_CON4IDMX                                               19
C -                                                                       19
      REAL SSUP_CONE3ZMX                                                  19
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         19
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     19
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       19
     *MSSUP_CONE3ZMX)                                                     19
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    19
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           19
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      19
      REAL RE_SSUP_CONE3ZMX                                               19
C -                                                                       19
      REAL SSUP_CONE4ZMX                                                  19
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         19
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     19
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       19
     *MSSUP_CONE4ZMX)                                                     19
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    19
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           19
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      19
      REAL RE_SSUP_CONE4ZMX                                               19
C -                                                                       19
      REAL SSUP_BRATHK                                                    19
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             19
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       19
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         19
     *MSSUP_BRATHK)                                                       19
      CHARACTER CSSUP_BRATHK*40/' '/                                      19
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               19
      INTEGER MSSUP_BRATHK(2)/1,1/                                        19
      REAL RE_SSUP_BRATHK                                                 19
C -                                                                       19
      REAL SSUP_ERJTHK                                                    19
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             19
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       19
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         19
     *MSSUP_ERJTHK)                                                       19
      CHARACTER CSSUP_ERJTHK*40/' '/                                      19
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               19
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        19
      REAL RE_SSUP_ERJTHK                                                 19
C -                                                                       19
      REAL SSUP_ERJWID                                                    19
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             19
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       19
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         19
     *MSSUP_ERJWID)                                                       19
      CHARACTER CSSUP_ERJWID*40/' '/                                      19
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               19
      INTEGER MSSUP_ERJWID(2)/1,1/                                        19
      REAL RE_SSUP_ERJWID                                                 19
C -                                                                       19
      REAL SSUP_ERJLEN                                                    19
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             19
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       19
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         19
     *MSSUP_ERJLEN)                                                       19
      CHARACTER CSSUP_ERJLEN*40/' '/                                      19
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               19
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        19
      REAL RE_SSUP_ERJLEN                                                 19
C -                                                                       19
      REAL SSUP_ERJZDIS                                                   19
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           19
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      19
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        19
     *MSSUP_ERJZDIS)                                                      19
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     19
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             19
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       19
      REAL RE_SSUP_ERJZDIS                                                19
C -                                                                       19
      REAL SSUP_ERJ1X                                                     19
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               19
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        19
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          19
     *MSSUP_ERJ1X)                                                        19
      CHARACTER CSSUP_ERJ1X*40/' '/                                       19
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 19
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         19
      REAL RE_SSUP_ERJ1X                                                  19
C -                                                                       19
      REAL SSUP_ERJ2X                                                     19
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               19
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        19
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          19
     *MSSUP_ERJ2X)                                                        19
      CHARACTER CSSUP_ERJ2X*40/' '/                                       19
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 19
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         19
      REAL RE_SSUP_ERJ2X                                                  19
C -                                                                       19
      REAL SSUP_ERJ2Y                                                     19
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               19
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        19
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          19
     *MSSUP_ERJ2Y)                                                        19
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       19
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 19
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         19
      REAL RE_SSUP_ERJ2Y                                                  19
C -                                                                       19
      REAL SSUP_ERJRAD                                                    19
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             19
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       19
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         19
     *MSSUP_ERJRAD)                                                       19
      CHARACTER CSSUP_ERJRAD*40/' '/                                      19
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               19
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        19
      REAL RE_SSUP_ERJRAD                                                 19
C -                                                                       19
      REAL SSUP_ERJDIA                                                    19
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             19
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       19
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         19
     *MSSUP_ERJDIA)                                                       19
      CHARACTER CSSUP_ERJDIA*40/' '/                                      19
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               19
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        19
      REAL RE_SSUP_ERJDIA                                                 19
C -                                                                       19
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                19
      PARAMETER (LEN_SSUP=31)                                             19
      PARAMETER (LLEN_SSUP=29)                                            19
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           19
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        19
      DATA IDNUM_SSUP/10*0/                                               19
      REAL BANK_SSUP(LEN_SSUP)                                            19
      CHARACTER*60 BPATH_SSUP/' '/                                        19
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          19
     *BPATH_SSUP                                                          19
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             19
C *                                                                       19
C -                                                                       23
      REAL SSUB_VERSION                                                   23
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            23
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       23
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          23
     *MSSUB_VERSION)                                                      23
      CHARACTER CSSUB_VERSION*40/' '/                                     23
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             23
      INTEGER MSSUB_VERSION(2)/1,1/                                       23
      REAL RE_SSUB_VERSION                                                23
C -                                                                       23
      REAL SSUB_KMOUNTID                                                  23
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          23
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      23
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         23
     *MSSUB_KMOUNTID)                                                     23
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    23
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           23
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      23
      REAL RE_SSUB_KMOUNTID                                               23
C -                                                                       23
      REAL SSUB_KMOUNTOD                                                  23
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          23
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      23
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         23
     *MSSUB_KMOUNTOD)                                                     23
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    23
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           23
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      23
      REAL RE_SSUB_KMOUNTOD                                               23
C -                                                                       23
      REAL SSUB_KMNTTHK                                                   23
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            23
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       23
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          23
     *MSSUB_KMNTTHK)                                                      23
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     23
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             23
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       23
      REAL RE_SSUB_KMNTTHK                                                23
C -                                                                       23
      REAL SSUB_KMCUTOD                                                   23
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            23
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       23
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          23
     *MSSUB_KMCUTOD)                                                      23
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     23
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             23
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       23
      REAL RE_SSUB_KMCUTOD                                                23
C -                                                                       23
      REAL SSUB_KMCUTID                                                   23
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            23
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       23
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          23
     *MSSUB_KMCUTID)                                                      23
      CHARACTER CSSUB_KMCUTID*40/' '/                                     23
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             23
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       23
      REAL RE_SSUB_KMCUTID                                                23
C -                                                                       23
      REAL SSUB_KMCUTOA                                                   23
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            23
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       23
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          23
     *MSSUB_KMCUTOA)                                                      23
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     23
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             23
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       23
      REAL RE_SSUB_KMCUTOA                                                23
C -                                                                       23
      REAL SSUB_KMCUTOFF                                                  23
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         23
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      23
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         23
     *MSSUB_KMCUTOFF)                                                     23
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    23
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           23
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      23
      REAL RE_SSUB_KMCUTOFF                                               23
C -                                                                       23
      REAL SSUB_SRINGID                                                   23
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           23
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       23
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          23
     *MSSUB_SRINGID)                                                      23
      CHARACTER CSSUB_SRINGID*40/' '/                                     23
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             23
      INTEGER MSSUB_SRINGID(2)/1,1/                                       23
      REAL RE_SSUB_SRINGID                                                23
C -                                                                       23
      REAL SSUB_SRINGOD                                                   23
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           23
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      23
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        23
     *MSSUB_SRINGOD)                                                      23
      CHARACTER CSSUB_SRINGOD*40/' '/                                     23
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             23
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       23
      REAL RE_SSUB_SRINGOD                                                23
C -                                                                       23
      REAL SSUB_SRINGTHK                                                  23
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         23
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     23
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       23
     *MSSUB_SRINGTHK)                                                     23
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    23
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           23
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      23
      REAL RE_SSUB_SRINGTHK                                               23
C -                                                                       23
      REAL SSUB_SRCUTPHI                                                  23
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         23
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     23
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       23
     *MSSUB_SRCUTPHI)                                                     23
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    23
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           23
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      23
      REAL RE_SSUB_SRCUTPHI                                               23
C -                                                                       23
      REAL SSUB_SRCUTWID                                                  23
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         23
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     23
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       23
     *MSSUB_SRCUTWID)                                                     23
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    23
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           23
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      23
      REAL RE_SSUB_SRCUTWID                                               23
C -                                                                       23
      REAL SSUB_SRCUTOUT                                                  23
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         23
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     23
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       23
     *MSSUB_SRCUTOUT)                                                     23
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    23
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           23
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      23
      REAL RE_SSUB_SRCUTOUT                                               23
C -                                                                       23
      REAL SSUB_SRCUTIN                                                   23
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           23
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      23
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        23
     *MSSUB_SRCUTIN)                                                      23
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     23
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             23
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       23
      REAL RE_SSUB_SRCUTIN                                                23
C -                                                                       23
      REAL SSUB_SROLLID                                                   23
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           23
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      23
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        23
     *MSSUB_SROLLID)                                                      23
      CHARACTER CSSUB_SROLLID*40/' '/                                     23
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             23
      INTEGER MSSUB_SROLLID(2)/1,1/                                       23
      REAL RE_SSUB_SROLLID                                                23
C -                                                                       23
      REAL SSUB_SROLLOD                                                   23
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           23
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      23
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        23
     *MSSUB_SROLLOD)                                                      23
      CHARACTER CSSUB_SROLLOD*40/' '/                                     23
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             23
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       23
      REAL RE_SSUB_SROLLOD                                                23
C -                                                                       23
      REAL SSUB_SROLLLEN                                                  23
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         23
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     23
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       23
     *MSSUB_SROLLLEN)                                                     23
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    23
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           23
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      23
      REAL RE_SSUB_SROLLLEN                                               23
C -                                                                       23
      REAL SSUB_SWIRELEN                                                  23
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         23
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     23
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       23
     *MSSUB_SWIRELEN)                                                     23
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    23
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           23
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      23
      REAL RE_SSUB_SWIRELEN                                               23
C -                                                                       23
      REAL SSUB_MBLKHGH                                                   23
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           23
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      23
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        23
     *MSSUB_MBLKHGH)                                                      23
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     23
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             23
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       23
      REAL RE_SSUB_MBLKHGH                                                23
C -                                                                       23
      REAL SSUB_MBLKOWID                                                  23
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         23
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     23
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       23
     *MSSUB_MBLKOWID)                                                     23
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    23
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           23
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      23
      REAL RE_SSUB_MBLKOWID                                               23
C -                                                                       23
      REAL SSUB_MBLKOLEN                                                  23
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         23
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     23
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       23
     *MSSUB_MBLKOLEN)                                                     23
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    23
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           23
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      23
      REAL RE_SSUB_MBLKOLEN                                               23
C -                                                                       23
      REAL SSUB_MBLKIWID                                                  23
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         23
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     23
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       23
     *MSSUB_MBLKIWID)                                                     23
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    23
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           23
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      23
      REAL RE_SSUB_MBLKIWID                                               23
C -                                                                       23
      REAL SSUB_MBLKILEN                                                  23
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         23
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     23
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       23
     *MSSUB_MBLKILEN)                                                     23
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    23
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           23
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      23
      REAL RE_SSUB_MBLKILEN                                               23
C -                                                                       23
      REAL SSUB_MBLKORAD                                                  23
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         23
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     23
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       23
     *MSSUB_MBLKORAD)                                                     23
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    23
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           23
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      23
      REAL RE_SSUB_MBLKORAD                                               23
C -                                                                       23
      REAL SSUB_MBLKIRAD                                                  23
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         23
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     23
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       23
     *MSSUB_MBLKIRAD)                                                     23
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    23
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           23
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      23
      REAL RE_SSUB_MBLKIRAD                                               23
C -                                                                       23
      REAL SSUB_MRODDIA                                                   23
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           23
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      23
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        23
     *MSSUB_MRODDIA)                                                      23
      CHARACTER CSSUB_MRODDIA*40/' '/                                     23
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             23
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       23
      REAL RE_SSUB_MRODDIA                                                23
C -                                                                       23
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                23
      PARAMETER (LEN_SSUB=29)                                             23
      PARAMETER (LLEN_SSUB=27)                                            23
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           23
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        23
      DATA IDNUM_SSUB/10*0/                                               23
      REAL BANK_SSUB(LEN_SSUB)                                            23
      CHARACTER*60 BPATH_SSUB/' '/                                        23
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          23
     *BPATH_SSUB                                                          23
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             23
C *                                                                       23
C -                                                                       24
      REAL SWAM_VERSION                                                   24
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            24
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       24
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          24
     *MSWAM_VERSION)                                                      24
      CHARACTER CSWAM_VERSION*40/' '/                                     24
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             24
      INTEGER MSWAM_VERSION(2)/1,1/                                       24
      REAL RE_SWAM_VERSION                                                24
C -                                                                       24
      REAL SWAM_ZMIN                                                      24
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   24
     *NSWAM_ZMIN)                                                         24
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             24
     *MSWAM_ZMIN)                                                         24
      CHARACTER CSWAM_ZMIN*40/' '/                                        24
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   24
      INTEGER MSWAM_ZMIN(2)/1,1/                                          24
      REAL RE_SWAM_ZMIN                                                   24
C -                                                                       24
      REAL SWAM_LEN                                                       24
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     24
     *NSWAM_LEN)                                                          24
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    24
      CHARACTER CSWAM_LEN*40/' '/                                         24
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     24
      INTEGER MSWAM_LEN(2)/1,1/                                           24
      REAL RE_SWAM_LEN                                                    24
C -                                                                       24
      REAL SWAM_RMIN                                                      24
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   24
     *NSWAM_RMIN)                                                         24
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             24
     *MSWAM_RMIN)                                                         24
      CHARACTER CSWAM_RMIN*40/' '/                                        24
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   24
      INTEGER MSWAM_RMIN(2)/1,1/                                          24
      REAL RE_SWAM_RMIN                                                   24
C -                                                                       24
      REAL SWAM_RMAX                                                      24
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   24
     *NSWAM_RMAX)                                                         24
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             24
     *MSWAM_RMAX)                                                         24
      CHARACTER CSWAM_RMAX*40/' '/                                        24
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   24
      INTEGER MSWAM_RMAX(2)/1,1/                                          24
      REAL RE_SWAM_RMAX                                                   24
C -                                                                       24
      REAL SWAM_TBRDTHK                                                   24
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            24
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       24
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          24
     *MSWAM_TBRDTHK)                                                      24
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     24
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             24
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       24
      REAL RE_SWAM_TBRDTHK                                                24
C -                                                                       24
      REAL SWAM_WALLTHK                                                   24
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            24
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       24
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          24
     *MSWAM_WALLTHK)                                                      24
      CHARACTER CSWAM_WALLTHK*40/' '/                                     24
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             24
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       24
      REAL RE_SWAM_WALLTHK                                                24
C -                                                                       24
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                24
      PARAMETER (LEN_SWAM=9)                                              24
      PARAMETER (LLEN_SWAM=7)                                             24
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           24
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        24
      DATA IDNUM_SWAM/10*0/                                               24
      REAL BANK_SWAM(LEN_SWAM)                                            24
      CHARACTER*60 BPATH_SWAM/' '/                                        24
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          24
     *BPATH_SWAM                                                          24
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             24
C *                                                                       24
C -                                                                       25
      REAL SERG_VERSION                                                   25
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            25
     *(NAMES_SERG(1),NSERG_VERSION)                                       25
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          25
     *MSERG_VERSION)                                                      25
      CHARACTER CSERG_VERSION*40/' '/                                     25
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             25
      INTEGER MSERG_VERSION(2)/1,1/                                       25
      REAL RE_SERG_VERSION                                                25
C -                                                                       25
      REAL SERG_IRNGTRMX                                                  25
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          25
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      25
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         25
     *MSERG_IRNGTRMX)                                                     25
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    25
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           25
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      25
      REAL RE_SERG_IRNGTRMX                                               25
C -                                                                       25
      REAL SERG_IRNGPRMN                                                  25
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          25
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      25
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         25
     *MSERG_IRNGPRMN)                                                     25
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    25
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           25
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      25
      REAL RE_SERG_IRNGPRMN                                               25
C -                                                                       25
      REAL SERG_ORNGRMIN                                                  25
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          25
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      25
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         25
     *MSERG_ORNGRMIN)                                                     25
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    25
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           25
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      25
      REAL RE_SERG_ORNGRMIN                                               25
C -                                                                       25
      REAL SERG_ORNGRMAX                                                  25
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          25
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      25
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         25
     *MSERG_ORNGRMAX)                                                     25
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    25
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           25
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      25
      REAL RE_SERG_ORNGRMAX                                               25
C -                                                                       25
      REAL SERG_ENDRNGTH                                                  25
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          25
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      25
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         25
     *MSERG_ENDRNGTH)                                                     25
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    25
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           25
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      25
      REAL RE_SERG_ENDRNGTH                                               25
C -                                                                       25
      REAL SERG_ENDRNGZM                                                  25
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          25
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      25
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         25
     *MSERG_ENDRNGZM)                                                     25
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    25
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           25
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      25
      REAL RE_SERG_ENDRNGZM                                               25
C -                                                                       25
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                25
      PARAMETER (LEN_SERG=9)                                              25
      PARAMETER (LLEN_SERG=7)                                             25
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           25
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        25
      DATA IDNUM_SERG/10*0/                                               25
      REAL BANK_SERG(LEN_SERG)                                            25
      CHARACTER*60 BPATH_SERG/' '/                                        25
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          25
     *BPATH_SERG                                                          25
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             25
C *                                                                       25
C -                                                                       27
      REAL SELC_VERSION                                                   27
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            27
     *(NAMES_SELC(1),NSELC_VERSION)                                       27
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          27
     *MSELC_VERSION)                                                      27
      CHARACTER CSELC_VERSION*40/' '/                                     27
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             27
      INTEGER MSELC_VERSION(2)/1,1/                                       27
      REAL RE_SELC_VERSION                                                27
C -                                                                       27
      REAL SELC_BETHK                                                     27
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                27
     *(NAMES_SELC(2),NSELC_BETHK)                                         27
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            27
     *MSELC_BETHK)                                                        27
      CHARACTER CSELC_BETHK*40/' '/                                       27
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 27
      INTEGER MSELC_BETHK(2)/1,1/                                         27
      REAL RE_SELC_BETHK                                                  27
C -                                                                       27
      REAL SELC_WATTHK                                                    27
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              27
     *(NAMES_SELC(3),NSELC_WATTHK)                                        27
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           27
     *MSELC_WATTHK)                                                       27
      CHARACTER CSELC_WATTHK*40/' '/                                      27
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               27
      INTEGER MSELC_WATTHK(2)/1,1/                                        27
      REAL RE_SELC_WATTHK                                                 27
C -                                                                       27
      REAL SELC_BEOTHK                                                    27
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              27
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        27
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           27
     *MSELC_BEOTHK)                                                       27
      CHARACTER CSELC_BEOTHK*40/' '/                                      27
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               27
      INTEGER MSELC_BEOTHK(2)/1,1/                                        27
      REAL RE_SELC_BEOTHK                                                 27
C -                                                                       27
      REAL SELC_DYETHK                                                    27
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              27
     *(NAMES_SELC(5),NSELC_DYETHK)                                        27
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           27
     *MSELC_DYETHK)                                                       27
      CHARACTER CSELC_DYETHK*40/' '/                                      27
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               27
      INTEGER MSELC_DYETHK(2)/1,1/                                        27
      REAL RE_SELC_DYETHK                                                 27
C -                                                                       27
      REAL SELC_DYEWID                                                    27
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              27
     *(NAMES_SELC(6),NSELC_DYEWID)                                        27
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           27
     *MSELC_DYEWID)                                                       27
      CHARACTER CSELC_DYEWID*40/' '/                                      27
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               27
      INTEGER MSELC_DYEWID(2)/1,1/                                        27
      REAL RE_SELC_DYEWID                                                 27
C -                                                                       27
      REAL SELC_DYESPC                                                    27
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              27
     *(NAMES_SELC(7),NSELC_DYESPC)                                        27
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           27
     *MSELC_DYESPC)                                                       27
      CHARACTER CSELC_DYESPC*40/' '/                                      27
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               27
      INTEGER MSELC_DYESPC(2)/1,1/                                        27
      REAL RE_SELC_DYESPC                                                 27
C -                                                                       27
      REAL SELC_ELCAWID                                                   27
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           27
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       27
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          27
     *MSELC_ELCAWID)                                                      27
      CHARACTER CSELC_ELCAWID*40/' '/                                     27
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             27
      INTEGER MSELC_ELCAWID(2)/1,1/                                       27
      REAL RE_SELC_ELCAWID                                                27
C -                                                                       27
      REAL SELC_AGPDTHK                                                   27
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           27
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       27
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          27
     *MSELC_AGPDTHK)                                                      27
      CHARACTER CSELC_AGPDTHK*40/' '/                                     27
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             27
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       27
      REAL RE_SELC_AGPDTHK                                                27
C -                                                                       27
      REAL SELC_GLASSTHK                                                  27
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         27
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     27
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       27
     *MSELC_GLASSTHK)                                                     27
      CHARACTER CSELC_GLASSTHK*40/' '/                                    27
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           27
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      27
      REAL RE_SELC_GLASSTHK                                               27
C -                                                                       27
      REAL SELC_CABTHK                                                    27
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             27
     *(NAMES_SELC(11),NSELC_CABTHK)                                       27
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         27
     *MSELC_CABTHK)                                                       27
      CHARACTER CSELC_CABTHK*40/' '/                                      27
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               27
      INTEGER MSELC_CABTHK(2)/1,1/                                        27
      REAL RE_SELC_CABTHK                                                 27
C -                                                                       27
      REAL SELC_CABWID                                                    27
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             27
     *(NAMES_SELC(12),NSELC_CABWID)                                       27
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         27
     *MSELC_CABWID)                                                       27
      CHARACTER CSELC_CABWID*40/' '/                                      27
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               27
      INTEGER MSELC_CABWID(2)/1,1/                                        27
      REAL RE_SELC_CABWID                                                 27
C -                                                                       27
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                27
      PARAMETER (LEN_SELC=14)                                             27
      PARAMETER (LLEN_SELC=12)                                            27
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           27
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        27
      DATA IDNUM_SELC/10*0/                                               27
      REAL BANK_SELC(LEN_SELC)                                            27
      CHARACTER*60 BPATH_SELC/' '/                                        27
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          27
     *BPATH_SELC                                                          27
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             27
C *                                                                       27
C -                                                                       29
      INTEGER SVTL_LAYER                                                  29
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                29
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         29
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            29
     *MSVTL_LAYER)                                                        29
      CHARACTER CSVTL_LAYER*40/' '/                                       29
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 29
      INTEGER MSVTL_LAYER(2)/1,1/                                         29
      REAL RE_SVTL_LAYER                                                  29
C -                                                                       29
      REAL SVTL_NLADDER                                                   29
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            29
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       29
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          29
     *MSVTL_NLADDER)                                                      29
      CHARACTER CSVTL_NLADDER*40/' '/                                     29
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             29
      INTEGER MSVTL_NLADDER(2)/1,1/                                       29
      REAL RE_SVTL_NLADDER                                                29
C -                                                                       29
      REAL SVTL_NWAFER                                                    29
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              29
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        29
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           29
     *MSVTL_NWAFER)                                                       29
      CHARACTER CSVTL_NWAFER*40/' '/                                      29
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               29
      INTEGER MSVTL_NWAFER(2)/1,1/                                        29
      REAL RE_SVTL_NWAFER                                                 29
C -                                                                       29
      REAL SVTL_RADIUS                                                    29
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              29
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        29
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           29
     *MSVTL_RADIUS)                                                       29
      CHARACTER CSVTL_RADIUS*40/' '/                                      29
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               29
      INTEGER MSVTL_RADIUS(2)/1,1/                                        29
      REAL RE_SVTL_RADIUS                                                 29
C -                                                                       29
      REAL SVTL_BAREEDGE                                                  29
      EQUIVALENCE (BANK_SVTL(7),SVTL_BAREEDGE,RE_SVTL_BAREEDGE),          29
     *(NAMES_SVTL(5),NSVTL_BAREEDGE)                                      29
      EQUIVALENCE (COMMEN_SVTL(5),CSVTL_BAREEDGE),(MAP_SVTL(1,5),         29
     *MSVTL_BAREEDGE)                                                     29
      CHARACTER CSVTL_BAREEDGE*40/' '/                                    29
      CHARACTER NSVTL_BAREEDGE*12/'R.BAREEDGE'/                           29
      INTEGER MSVTL_BAREEDGE(2)/1,1/                                      29
      REAL RE_SVTL_BAREEDGE                                               29
C -                                                                       29
      REAL SVTL_PCBLEN                                                    29
      EQUIVALENCE (BANK_SVTL(8),SVTL_PCBLEN,RE_SVTL_PCBLEN),              29
     *(NAMES_SVTL(6),NSVTL_PCBLEN)                                        29
      EQUIVALENCE (COMMEN_SVTL(6),CSVTL_PCBLEN),(MAP_SVTL(1,6),           29
     *MSVTL_PCBLEN)                                                       29
      CHARACTER CSVTL_PCBLEN*40/' '/                                      29
      CHARACTER NSVTL_PCBLEN*12/'R.PCBLEN'/                               29
      INTEGER MSVTL_PCBLEN(2)/1,1/                                        29
      REAL RE_SVTL_PCBLEN                                                 29
C -                                                                       29
      REAL SVTL_PCBWIDTH                                                  29
      EQUIVALENCE (BANK_SVTL(9),SVTL_PCBWIDTH,RE_SVTL_PCBWIDTH),          29
     *(NAMES_SVTL(7),NSVTL_PCBWIDTH)                                      29
      EQUIVALENCE (COMMEN_SVTL(7),CSVTL_PCBWIDTH),(MAP_SVTL(1,7),         29
     *MSVTL_PCBWIDTH)                                                     29
      CHARACTER CSVTL_PCBWIDTH*40/' '/                                    29
      CHARACTER NSVTL_PCBWIDTH*12/'R.PCBWIDTH'/                           29
      INTEGER MSVTL_PCBWIDTH(2)/1,1/                                      29
      REAL RE_SVTL_PCBWIDTH                                               29
C -                                                                       29
      REAL SVTL_PCBTHK                                                    29
      EQUIVALENCE (BANK_SVTL(10),SVTL_PCBTHK,RE_SVTL_PCBTHK),             29
     *(NAMES_SVTL(8),NSVTL_PCBTHK)                                        29
      EQUIVALENCE (COMMEN_SVTL(8),CSVTL_PCBTHK),(MAP_SVTL(1,8),           29
     *MSVTL_PCBTHK)                                                       29
      CHARACTER CSVTL_PCBTHK*40/' '/                                      29
      CHARACTER NSVTL_PCBTHK*12/'R.PCBTHK'/                               29
      INTEGER MSVTL_PCBTHK(2)/1,1/                                        29
      REAL RE_SVTL_PCBTHK                                                 29
C -                                                                       29
      REAL SVTL_PCBGAP                                                    29
      EQUIVALENCE (BANK_SVTL(11),SVTL_PCBGAP,RE_SVTL_PCBGAP),             29
     *(NAMES_SVTL(9),NSVTL_PCBGAP)                                        29
      EQUIVALENCE (COMMEN_SVTL(9),CSVTL_PCBGAP),(MAP_SVTL(1,9),           29
     *MSVTL_PCBGAP)                                                       29
      CHARACTER CSVTL_PCBGAP*40/' '/                                      29
      CHARACTER NSVTL_PCBGAP*12/'R.PCBGAP'/                               29
      INTEGER MSVTL_PCBGAP(2)/1,1/                                        29
      REAL RE_SVTL_PCBGAP                                                 29
C -                                                                       29
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                29
      PARAMETER (LEN_SVTL=11)                                             29
      PARAMETER (LLEN_SVTL=9)                                             29
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           29
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        29
      DATA IDNUM_SVTL/10*0/                                               29
      REAL BANK_SVTL(LEN_SVTL)                                            29
      CHARACTER*60 BPATH_SVTL/' '/                                        29
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          29
     *BPATH_SVTL                                                          29
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             29
C *                                                                       29
C -                                                                       31
      REAL SSLD_VERSION                                                   31
      EQUIVALENCE (BANK_SSLD(3),SSLD_VERSION,RE_SSLD_VERSION),            31
     *(NAMES_SSLD(1),NSSLD_VERSION)                                       31
      EQUIVALENCE (COMMEN_SSLD(1),CSSLD_VERSION),(MAP_SSLD(1,1),          31
     *MSSLD_VERSION)                                                      31
      CHARACTER CSSLD_VERSION*40/' '/                                     31
      CHARACTER NSSLD_VERSION*12/'R.VERSION'/                             31
      INTEGER MSSLD_VERSION(2)/1,1/                                       31
      REAL RE_SSLD_VERSION                                                31
C -                                                                       31
      REAL SSLD_SINRINN                                                   31
      EQUIVALENCE (BANK_SSLD(4),SSLD_SINRINN,RE_SSLD_SINRINN),            31
     *(NAMES_SSLD(2),NSSLD_SINRINN)                                       31
      EQUIVALENCE (COMMEN_SSLD(2),CSSLD_SINRINN),(MAP_SSLD(1,2),          31
     *MSSLD_SINRINN)                                                      31
      CHARACTER CSSLD_SINRINN*40/' '/                                     31
      CHARACTER NSSLD_SINRINN*12/'R.SINRINN'/                             31
      INTEGER MSSLD_SINRINN(2)/1,1/                                       31
      REAL RE_SSLD_SINRINN                                                31
C -                                                                       31
      REAL SSLD_SINROUT                                                   31
      EQUIVALENCE (BANK_SSLD(5),SSLD_SINROUT,RE_SSLD_SINROUT),            31
     *(NAMES_SSLD(3),NSSLD_SINROUT)                                       31
      EQUIVALENCE (COMMEN_SSLD(3),CSSLD_SINROUT),(MAP_SSLD(1,3),          31
     *MSSLD_SINROUT)                                                      31
      CHARACTER CSSLD_SINROUT*40/' '/                                     31
      CHARACTER NSSLD_SINROUT*12/'R.SINROUT'/                             31
      INTEGER MSSLD_SINROUT(2)/1,1/                                       31
      REAL RE_SSLD_SINROUT                                                31
C -                                                                       31
      REAL SSLD_SINLEN                                                    31
      EQUIVALENCE (BANK_SSLD(6),SSLD_SINLEN,RE_SSLD_SINLEN),              31
     *(NAMES_SSLD(4),NSSLD_SINLEN)                                        31
      EQUIVALENCE (COMMEN_SSLD(4),CSSLD_SINLEN),(MAP_SSLD(1,4),           31
     *MSSLD_SINLEN)                                                       31
      CHARACTER CSSLD_SINLEN*40/' '/                                      31
      CHARACTER NSSLD_SINLEN*12/'R.SINLEN'/                               31
      INTEGER MSSLD_SINLEN(2)/1,1/                                        31
      REAL RE_SSLD_SINLEN                                                 31
C -                                                                       31
      REAL SSLD_SSEPRINN                                                  31
      EQUIVALENCE (BANK_SSLD(7),SSLD_SSEPRINN,RE_SSLD_SSEPRINN),          31
     *(NAMES_SSLD(5),NSSLD_SSEPRINN)                                      31
      EQUIVALENCE (COMMEN_SSLD(5),CSSLD_SSEPRINN),(MAP_SSLD(1,5),         31
     *MSSLD_SSEPRINN)                                                     31
      CHARACTER CSSLD_SSEPRINN*40/' '/                                    31
      CHARACTER NSSLD_SSEPRINN*12/'R.SSEPRINN'/                           31
      INTEGER MSSLD_SSEPRINN(2)/1,1/                                      31
      REAL RE_SSLD_SSEPRINN                                               31
C -                                                                       31
      REAL SSLD_SSEPROUT                                                  31
      EQUIVALENCE (BANK_SSLD(8),SSLD_SSEPROUT,RE_SSLD_SSEPROUT),          31
     *(NAMES_SSLD(6),NSSLD_SSEPROUT)                                      31
      EQUIVALENCE (COMMEN_SSLD(6),CSSLD_SSEPROUT),(MAP_SSLD(1,6),         31
     *MSSLD_SSEPROUT)                                                     31
      CHARACTER CSSLD_SSEPROUT*40/' '/                                    31
      CHARACTER NSSLD_SSEPROUT*12/'R.SSEPROUT'/                           31
      INTEGER MSSLD_SSEPROUT(2)/1,1/                                      31
      REAL RE_SSLD_SSEPROUT                                               31
C -                                                                       31
      REAL SSLD_SSEPLEN                                                   31
      EQUIVALENCE (BANK_SSLD(9),SSLD_SSEPLEN,RE_SSLD_SSEPLEN),            31
     *(NAMES_SSLD(7),NSSLD_SSEPLEN)                                       31
      EQUIVALENCE (COMMEN_SSLD(7),CSSLD_SSEPLEN),(MAP_SSLD(1,7),          31
     *MSSLD_SSEPLEN)                                                      31
      CHARACTER CSSLD_SSEPLEN*40/' '/                                     31
      CHARACTER NSSLD_SSEPLEN*12/'R.SSEPLEN'/                             31
      INTEGER MSSLD_SSEPLEN(2)/1,1/                                       31
      REAL RE_SSLD_SSEPLEN                                                31
C -                                                                       31
      REAL SSLD_SOUTRINN                                                  31
      EQUIVALENCE (BANK_SSLD(10),SSLD_SOUTRINN,RE_SSLD_SOUTRINN),         31
     *(NAMES_SSLD(8),NSSLD_SOUTRINN)                                      31
      EQUIVALENCE (COMMEN_SSLD(8),CSSLD_SOUTRINN),(MAP_SSLD(1,8),         31
     *MSSLD_SOUTRINN)                                                     31
      CHARACTER CSSLD_SOUTRINN*40/' '/                                    31
      CHARACTER NSSLD_SOUTRINN*12/'R.SOUTRINN'/                           31
      INTEGER MSSLD_SOUTRINN(2)/1,1/                                      31
      REAL RE_SSLD_SOUTRINN                                               31
C -                                                                       31
      REAL SSLD_SOUTROUT                                                  31
      EQUIVALENCE (BANK_SSLD(11),SSLD_SOUTROUT,RE_SSLD_SOUTROUT),         31
     *(NAMES_SSLD(9),NSSLD_SOUTROUT)                                      31
      EQUIVALENCE (COMMEN_SSLD(9),CSSLD_SOUTROUT),(MAP_SSLD(1,9),         31
     *MSSLD_SOUTROUT)                                                     31
      CHARACTER CSSLD_SOUTROUT*40/' '/                                    31
      CHARACTER NSSLD_SOUTROUT*12/'R.SOUTROUT'/                           31
      INTEGER MSSLD_SOUTROUT(2)/1,1/                                      31
      REAL RE_SSLD_SOUTROUT                                               31
C -                                                                       31
      REAL SSLD_SOUTLEN                                                   31
      EQUIVALENCE (BANK_SSLD(12),SSLD_SOUTLEN,RE_SSLD_SOUTLEN),           31
     *(NAMES_SSLD(10),NSSLD_SOUTLEN)                                      31
      EQUIVALENCE (COMMEN_SSLD(10),CSSLD_SOUTLEN),(MAP_SSLD(1,10),        31
     *MSSLD_SOUTLEN)                                                      31
      CHARACTER CSSLD_SOUTLEN*40/' '/                                     31
      CHARACTER NSSLD_SOUTLEN*12/'R.SOUTLEN'/                             31
      INTEGER MSSLD_SOUTLEN(2)/1,1/                                       31
      REAL RE_SSLD_SOUTLEN                                                31
C -                                                                       31
      REAL SSLD_ALMESHID                                                  31
      EQUIVALENCE (BANK_SSLD(13),SSLD_ALMESHID,RE_SSLD_ALMESHID),         31
     *(NAMES_SSLD(11),NSSLD_ALMESHID)                                     31
      EQUIVALENCE (COMMEN_SSLD(11),CSSLD_ALMESHID),(MAP_SSLD(1,11),       31
     *MSSLD_ALMESHID)                                                     31
      CHARACTER CSSLD_ALMESHID*40/' '/                                    31
      CHARACTER NSSLD_ALMESHID*12/'R.ALMESHID'/                           31
      INTEGER MSSLD_ALMESHID(2)/1,1/                                      31
      REAL RE_SSLD_ALMESHID                                               31
C -                                                                       31
      REAL SSLD_ALMESHOD                                                  31
      EQUIVALENCE (BANK_SSLD(14),SSLD_ALMESHOD,RE_SSLD_ALMESHOD),         31
     *(NAMES_SSLD(12),NSSLD_ALMESHOD)                                     31
      EQUIVALENCE (COMMEN_SSLD(12),CSSLD_ALMESHOD),(MAP_SSLD(1,12),       31
     *MSSLD_ALMESHOD)                                                     31
      CHARACTER CSSLD_ALMESHOD*40/' '/                                    31
      CHARACTER NSSLD_ALMESHOD*12/'R.ALMESHOD'/                           31
      INTEGER MSSLD_ALMESHOD(2)/1,1/                                      31
      REAL RE_SSLD_ALMESHOD                                               31
C -                                                                       31
      REAL SSLD_ALMSHTHK                                                  31
      EQUIVALENCE (BANK_SSLD(15),SSLD_ALMSHTHK,RE_SSLD_ALMSHTHK),         31
     *(NAMES_SSLD(13),NSSLD_ALMSHTHK)                                     31
      EQUIVALENCE (COMMEN_SSLD(13),CSSLD_ALMSHTHK),(MAP_SSLD(1,13),       31
     *MSSLD_ALMSHTHK)                                                     31
      CHARACTER CSSLD_ALMSHTHK*40/' '/                                    31
      CHARACTER NSSLD_ALMSHTHK*12/'R.ALMSHTHK'/                           31
      INTEGER MSSLD_ALMSHTHK(2)/1,1/                                      31
      REAL RE_SSLD_ALMSHTHK                                               31
C -                                                                       31
      REAL SSLD_ALMSHPOS                                                  31
      EQUIVALENCE (BANK_SSLD(16),SSLD_ALMSHPOS,RE_SSLD_ALMSHPOS),         31
     *(NAMES_SSLD(14),NSSLD_ALMSHPOS)                                     31
      EQUIVALENCE (COMMEN_SSLD(14),CSSLD_ALMSHPOS),(MAP_SSLD(1,14),       31
     *MSSLD_ALMSHPOS)                                                     31
      CHARACTER CSSLD_ALMSHPOS*40/' '/                                    31
      CHARACTER NSSLD_ALMSHPOS*12/'R.ALMSHPOS'/                           31
      INTEGER MSSLD_ALMSHPOS(2)/1,1/                                      31
      REAL RE_SSLD_ALMSHPOS                                               31
C -                                                                       31
      INTEGER LEN_SSLD,LLEN_SSLD,LINK_SSLD                                31
      PARAMETER (LEN_SSLD=16)                                             31
      PARAMETER (LLEN_SSLD=14)                                            31
      INTEGER FLAG_SSLD/0/,MAP_SSLD(2,LLEN_SSLD),IDNUM_SSLD(10)           31
      CHARACTER NAMES_SSLD(LLEN_SSLD)*12,COMMEN_SSLD(LLEN_SSLD)*40        31
      DATA IDNUM_SSLD/10*0/                                               31
      REAL BANK_SSLD(LEN_SSLD)                                            31
      CHARACTER*60 BPATH_SSLD/' '/                                        31
      SAVE FLAG_SSLD,MAP_SSLD,IDNUM_SSLD,NAMES_SSLD,COMMEN_SSLD,          31
     *BPATH_SSLD                                                          31
      DATA LINK_SSLD/0/,BANK_SSLD/LEN_SSLD*0/                             31
C *                                                                       31
C -                                                                       32
      INTEGER SCBP_LAYER                                                  32
      EQUIVALENCE (BANK_SCBP(3),SCBP_LAYER,RE_SCBP_LAYER),                32
     *(NAMES_SCBP(1),NSCBP_LAYER)                                         32
      EQUIVALENCE (COMMEN_SCBP(1),CSCBP_LAYER),(MAP_SCBP(1,1),            32
     *MSCBP_LAYER)                                                        32
      CHARACTER CSCBP_LAYER*40/' '/                                       32
      CHARACTER NSCBP_LAYER*12/'I.LAYER'/                                 32
      INTEGER MSCBP_LAYER(2)/1,1/                                         32
      REAL RE_SCBP_LAYER                                                  32
C -                                                                       32
      REAL SCBP_LEN                                                       32
      EQUIVALENCE (BANK_SCBP(4),SCBP_LEN,RE_SCBP_LEN),(NAMES_SCBP(2),     32
     *NSCBP_LEN)                                                          32
      EQUIVALENCE (COMMEN_SCBP(2),CSCBP_LEN),(MAP_SCBP(1,2),MSCBP_LEN)    32
      CHARACTER CSCBP_LEN*40/' '/                                         32
      CHARACTER NSCBP_LEN*12/'R.LEN'/                                     32
      INTEGER MSCBP_LEN(2)/1,1/                                           32
      REAL RE_SCBP_LEN                                                    32
C -                                                                       32
      REAL SCBP_RMIN1                                                     32
      EQUIVALENCE (BANK_SCBP(5),SCBP_RMIN1,RE_SCBP_RMIN1),                32
     *(NAMES_SCBP(3),NSCBP_RMIN1)                                         32
      EQUIVALENCE (COMMEN_SCBP(3),CSCBP_RMIN1),(MAP_SCBP(1,3),            32
     *MSCBP_RMIN1)                                                        32
      CHARACTER CSCBP_RMIN1*40/' '/                                       32
      CHARACTER NSCBP_RMIN1*12/'R.RMIN1'/                                 32
      INTEGER MSCBP_RMIN1(2)/1,1/                                         32
      REAL RE_SCBP_RMIN1                                                  32
C -                                                                       32
      REAL SCBP_RMAX1                                                     32
      EQUIVALENCE (BANK_SCBP(6),SCBP_RMAX1,RE_SCBP_RMAX1),                32
     *(NAMES_SCBP(4),NSCBP_RMAX1)                                         32
      EQUIVALENCE (COMMEN_SCBP(4),CSCBP_RMAX1),(MAP_SCBP(1,4),            32
     *MSCBP_RMAX1)                                                        32
      CHARACTER CSCBP_RMAX1*40/' '/                                       32
      CHARACTER NSCBP_RMAX1*12/'R.RMAX1'/                                 32
      INTEGER MSCBP_RMAX1(2)/1,1/                                         32
      REAL RE_SCBP_RMAX1                                                  32
C -                                                                       32
      REAL SCBP_RMIN2                                                     32
      EQUIVALENCE (BANK_SCBP(7),SCBP_RMIN2,RE_SCBP_RMIN2),                32
     *(NAMES_SCBP(5),NSCBP_RMIN2)                                         32
      EQUIVALENCE (COMMEN_SCBP(5),CSCBP_RMIN2),(MAP_SCBP(1,5),            32
     *MSCBP_RMIN2)                                                        32
      CHARACTER CSCBP_RMIN2*40/' '/                                       32
      CHARACTER NSCBP_RMIN2*12/'R.RMIN2'/                                 32
      INTEGER MSCBP_RMIN2(2)/1,1/                                         32
      REAL RE_SCBP_RMIN2                                                  32
C -                                                                       32
      REAL SCBP_RMAX2                                                     32
      EQUIVALENCE (BANK_SCBP(8),SCBP_RMAX2,RE_SCBP_RMAX2),                32
     *(NAMES_SCBP(6),NSCBP_RMAX2)                                         32
      EQUIVALENCE (COMMEN_SCBP(6),CSCBP_RMAX2),(MAP_SCBP(1,6),            32
     *MSCBP_RMAX2)                                                        32
      CHARACTER CSCBP_RMAX2*40/' '/                                       32
      CHARACTER NSCBP_RMAX2*12/'R.RMAX2'/                                 32
      INTEGER MSCBP_RMAX2(2)/1,1/                                         32
      REAL RE_SCBP_RMAX2                                                  32
C -                                                                       32
      REAL SCBP_VOL                                                       32
      EQUIVALENCE (BANK_SCBP(9),SCBP_VOL,RE_SCBP_VOL),(NAMES_SCBP(7),     32
     *NSCBP_VOL)                                                          32
      EQUIVALENCE (COMMEN_SCBP(7),CSCBP_VOL),(MAP_SCBP(1,7),MSCBP_VOL)    32
      CHARACTER CSCBP_VOL*40/' '/                                         32
      CHARACTER NSCBP_VOL*12/'R.VOL'/                                     32
      INTEGER MSCBP_VOL(2)/1,1/                                           32
      REAL RE_SCBP_VOL                                                    32
C -                                                                       32
      INTEGER LEN_SCBP,LLEN_SCBP,LINK_SCBP                                32
      PARAMETER (LEN_SCBP=9)                                              32
      PARAMETER (LLEN_SCBP=7)                                             32
      INTEGER FLAG_SCBP/0/,MAP_SCBP(2,LLEN_SCBP),IDNUM_SCBP(10)           32
      CHARACTER NAMES_SCBP(LLEN_SCBP)*12,COMMEN_SCBP(LLEN_SCBP)*40        32
      DATA IDNUM_SCBP/10*0/                                               32
      REAL BANK_SCBP(LEN_SCBP)                                            32
      CHARACTER*60 BPATH_SCBP/' '/                                        32
      SAVE FLAG_SCBP,MAP_SCBP,IDNUM_SCBP,NAMES_SCBP,COMMEN_SCBP,          32
     *BPATH_SCBP                                                          32
      DATA LINK_SCBP/0/,BANK_SCBP/LEN_SCBP*0/                             32
C *                                                                       32
C -                                                                       33
      INTEGER SFEP_LAYER                                                  33
      EQUIVALENCE (BANK_SFEP(3),SFEP_LAYER,RE_SFEP_LAYER),                33
     *(NAMES_SFEP(1),NSFEP_LAYER)                                         33
      EQUIVALENCE (COMMEN_SFEP(1),CSFEP_LAYER),(MAP_SFEP(1,1),            33
     *MSFEP_LAYER)                                                        33
      CHARACTER CSFEP_LAYER*40/' '/                                       33
      CHARACTER NSFEP_LAYER*12/'I.LAYER'/                                 33
      INTEGER MSFEP_LAYER(2)/1,1/                                         33
      REAL RE_SFEP_LAYER                                                  33
C -                                                                       33
      REAL SFEP_LEN                                                       33
      EQUIVALENCE (BANK_SFEP(4),SFEP_LEN,RE_SFEP_LEN),(NAMES_SFEP(2),     33
     *NSFEP_LEN)                                                          33
      EQUIVALENCE (COMMEN_SFEP(2),CSFEP_LEN),(MAP_SFEP(1,2),MSFEP_LEN)    33
      CHARACTER CSFEP_LEN*40/' '/                                         33
      CHARACTER NSFEP_LEN*12/'R.LEN'/                                     33
      INTEGER MSFEP_LEN(2)/1,1/                                           33
      REAL RE_SFEP_LEN                                                    33
C -                                                                       33
      REAL SFEP_RMIN1                                                     33
      EQUIVALENCE (BANK_SFEP(5),SFEP_RMIN1,RE_SFEP_RMIN1),                33
     *(NAMES_SFEP(3),NSFEP_RMIN1)                                         33
      EQUIVALENCE (COMMEN_SFEP(3),CSFEP_RMIN1),(MAP_SFEP(1,3),            33
     *MSFEP_RMIN1)                                                        33
      CHARACTER CSFEP_RMIN1*40/' '/                                       33
      CHARACTER NSFEP_RMIN1*12/'R.RMIN1'/                                 33
      INTEGER MSFEP_RMIN1(2)/1,1/                                         33
      REAL RE_SFEP_RMIN1                                                  33
C -                                                                       33
      REAL SFEP_RMAX1                                                     33
      EQUIVALENCE (BANK_SFEP(6),SFEP_RMAX1,RE_SFEP_RMAX1),                33
     *(NAMES_SFEP(4),NSFEP_RMAX1)                                         33
      EQUIVALENCE (COMMEN_SFEP(4),CSFEP_RMAX1),(MAP_SFEP(1,4),            33
     *MSFEP_RMAX1)                                                        33
      CHARACTER CSFEP_RMAX1*40/' '/                                       33
      CHARACTER NSFEP_RMAX1*12/'R.RMAX1'/                                 33
      INTEGER MSFEP_RMAX1(2)/1,1/                                         33
      REAL RE_SFEP_RMAX1                                                  33
C -                                                                       33
      REAL SFEP_RMIN2                                                     33
      EQUIVALENCE (BANK_SFEP(7),SFEP_RMIN2,RE_SFEP_RMIN2),                33
     *(NAMES_SFEP(5),NSFEP_RMIN2)                                         33
      EQUIVALENCE (COMMEN_SFEP(5),CSFEP_RMIN2),(MAP_SFEP(1,5),            33
     *MSFEP_RMIN2)                                                        33
      CHARACTER CSFEP_RMIN2*40/' '/                                       33
      CHARACTER NSFEP_RMIN2*12/'R.RMIN2'/                                 33
      INTEGER MSFEP_RMIN2(2)/1,1/                                         33
      REAL RE_SFEP_RMIN2                                                  33
C -                                                                       33
      REAL SFEP_RMAX2                                                     33
      EQUIVALENCE (BANK_SFEP(8),SFEP_RMAX2,RE_SFEP_RMAX2),                33
     *(NAMES_SFEP(6),NSFEP_RMAX2)                                         33
      EQUIVALENCE (COMMEN_SFEP(6),CSFEP_RMAX2),(MAP_SFEP(1,6),            33
     *MSFEP_RMAX2)                                                        33
      CHARACTER CSFEP_RMAX2*40/' '/                                       33
      CHARACTER NSFEP_RMAX2*12/'R.RMAX2'/                                 33
      INTEGER MSFEP_RMAX2(2)/1,1/                                         33
      REAL RE_SFEP_RMAX2                                                  33
C -                                                                       33
      REAL SFEP_VOL                                                       33
      EQUIVALENCE (BANK_SFEP(9),SFEP_VOL,RE_SFEP_VOL),(NAMES_SFEP(7),     33
     *NSFEP_VOL)                                                          33
      EQUIVALENCE (COMMEN_SFEP(7),CSFEP_VOL),(MAP_SFEP(1,7),MSFEP_VOL)    33
      CHARACTER CSFEP_VOL*40/' '/                                         33
      CHARACTER NSFEP_VOL*12/'R.VOL'/                                     33
      INTEGER MSFEP_VOL(2)/1,1/                                           33
      REAL RE_SFEP_VOL                                                    33
C -                                                                       33
      REAL SFEP_VOLPLAST                                                  33
      EQUIVALENCE (BANK_SFEP(10),SFEP_VOLPLAST,RE_SFEP_VOLPLAST),         33
     *(NAMES_SFEP(8),NSFEP_VOLPLAST)                                      33
      EQUIVALENCE (COMMEN_SFEP(8),CSFEP_VOLPLAST),(MAP_SFEP(1,8),         33
     *MSFEP_VOLPLAST)                                                     33
      CHARACTER CSFEP_VOLPLAST*40/' '/                                    33
      CHARACTER NSFEP_VOLPLAST*12/'R.VOLPLAST'/                           33
      INTEGER MSFEP_VOLPLAST(2)/1,1/                                      33
      REAL RE_SFEP_VOLPLAST                                               33
C -                                                                       33
      INTEGER LEN_SFEP,LLEN_SFEP,LINK_SFEP                                33
      PARAMETER (LEN_SFEP=10)                                             33
      PARAMETER (LLEN_SFEP=8)                                             33
      INTEGER FLAG_SFEP/0/,MAP_SFEP(2,LLEN_SFEP),IDNUM_SFEP(10)           33
      CHARACTER NAMES_SFEP(LLEN_SFEP)*12,COMMEN_SFEP(LLEN_SFEP)*40        33
      DATA IDNUM_SFEP/10*0/                                               33
      REAL BANK_SFEP(LEN_SFEP)                                            33
      CHARACTER*60 BPATH_SFEP/' '/                                        33
      SAVE FLAG_SFEP,MAP_SFEP,IDNUM_SFEP,NAMES_SFEP,COMMEN_SFEP,          33
     *BPATH_SFEP                                                          33
      DATA LINK_SFEP/0/,BANK_SFEP/LEN_SFEP*0/                             33
C *                                                                       33
C -                                                                       34
      INTEGER SWCX_LAYER                                                  34
      EQUIVALENCE (BANK_SWCX(3),SWCX_LAYER,RE_SWCX_LAYER),                34
     *(NAMES_SWCX(1),NSWCX_LAYER)                                         34
      EQUIVALENCE (COMMEN_SWCX(1),CSWCX_LAYER),(MAP_SWCX(1,1),            34
     *MSWCX_LAYER)                                                        34
      CHARACTER CSWCX_LAYER*40/' '/                                       34
      CHARACTER NSWCX_LAYER*12/'I.LAYER'/                                 34
      INTEGER MSWCX_LAYER(2)/1,1/                                         34
      REAL RE_SWCX_LAYER                                                  34
C -                                                                       34
      REAL SWCX_LENGTH                                                    34
      EQUIVALENCE (BANK_SWCX(4),SWCX_LENGTH,RE_SWCX_LENGTH),              34
     *(NAMES_SWCX(2),NSWCX_LENGTH)                                        34
      EQUIVALENCE (COMMEN_SWCX(2),CSWCX_LENGTH),(MAP_SWCX(1,2),           34
     *MSWCX_LENGTH)                                                       34
      CHARACTER CSWCX_LENGTH*40/' '/                                      34
      CHARACTER NSWCX_LENGTH*12/'R.LENGTH'/                               34
      INTEGER MSWCX_LENGTH(2)/1,1/                                        34
      REAL RE_SWCX_LENGTH                                                 34
C -                                                                       34
      REAL SWCX_DR                                                        34
      EQUIVALENCE (BANK_SWCX(5),SWCX_DR,RE_SWCX_DR),(NAMES_SWCX(3),       34
     *NSWCX_DR)                                                           34
      EQUIVALENCE (COMMEN_SWCX(3),CSWCX_DR),(MAP_SWCX(1,3),MSWCX_DR)      34
      CHARACTER CSWCX_DR*40/' '/                                          34
      CHARACTER NSWCX_DR*12/'R.DR'/                                       34
      INTEGER MSWCX_DR(2)/1,1/                                            34
      REAL RE_SWCX_DR                                                     34
C -                                                                       34
      REAL SWCX_OFFSET                                                    34
      EQUIVALENCE (BANK_SWCX(6),SWCX_OFFSET,RE_SWCX_OFFSET),              34
     *(NAMES_SWCX(4),NSWCX_OFFSET)                                        34
      EQUIVALENCE (COMMEN_SWCX(4),CSWCX_OFFSET),(MAP_SWCX(1,4),           34
     *MSWCX_OFFSET)                                                       34
      CHARACTER CSWCX_OFFSET*40/' '/                                      34
      CHARACTER NSWCX_OFFSET*12/'R.OFFSET'/                               34
      INTEGER MSWCX_OFFSET(2)/1,1/                                        34
      REAL RE_SWCX_OFFSET                                                 34
C -                                                                       34
      REAL SWCX_RAD                                                       34
      EQUIVALENCE (BANK_SWCX(7),SWCX_RAD,RE_SWCX_RAD),(NAMES_SWCX(5),     34
     *NSWCX_RAD)                                                          34
      EQUIVALENCE (COMMEN_SWCX(5),CSWCX_RAD),(MAP_SWCX(1,5),MSWCX_RAD)    34
      CHARACTER CSWCX_RAD*40/' '/                                         34
      CHARACTER NSWCX_RAD*12/'R.RAD'/                                     34
      INTEGER MSWCX_RAD(2)/1,1/                                           34
      REAL RE_SWCX_RAD                                                    34
C -                                                                       34
      REAL SWCX_WALL                                                      34
      EQUIVALENCE (BANK_SWCX(8),SWCX_WALL,RE_SWCX_WALL),(NAMES_SWCX(6),   34
     *NSWCX_WALL)                                                         34
      EQUIVALENCE (COMMEN_SWCX(6),CSWCX_WALL),(MAP_SWCX(1,6),             34
     *MSWCX_WALL)                                                         34
      CHARACTER CSWCX_WALL*40/' '/                                        34
      CHARACTER NSWCX_WALL*12/'R.WALL'/                                   34
      INTEGER MSWCX_WALL(2)/1,1/                                          34
      REAL RE_SWCX_WALL                                                   34
C -                                                                       34
      REAL SWCX_ROFFSET                                                   34
      EQUIVALENCE (BANK_SWCX(9),SWCX_ROFFSET,RE_SWCX_ROFFSET),            34
     *(NAMES_SWCX(7),NSWCX_ROFFSET)                                       34
      EQUIVALENCE (COMMEN_SWCX(7),CSWCX_ROFFSET),(MAP_SWCX(1,7),          34
     *MSWCX_ROFFSET)                                                      34
      CHARACTER CSWCX_ROFFSET*40/' '/                                     34
      CHARACTER NSWCX_ROFFSET*12/'R.ROFFSET'/                             34
      INTEGER MSWCX_ROFFSET(2)/1,1/                                       34
      REAL RE_SWCX_ROFFSET                                                34
C -                                                                       34
      INTEGER LEN_SWCX,LLEN_SWCX,LINK_SWCX                                34
      PARAMETER (LEN_SWCX=9)                                              34
      PARAMETER (LLEN_SWCX=7)                                             34
      INTEGER FLAG_SWCX/0/,MAP_SWCX(2,LLEN_SWCX),IDNUM_SWCX(10)           34
      CHARACTER NAMES_SWCX(LLEN_SWCX)*12,COMMEN_SWCX(LLEN_SWCX)*40        34
      DATA IDNUM_SWCX/10*0/                                               34
      REAL BANK_SWCX(LEN_SWCX)                                            34
      CHARACTER*60 BPATH_SWCX/' '/                                        34
      SAVE FLAG_SWCX,MAP_SWCX,IDNUM_SWCX,NAMES_SWCX,COMMEN_SWCX,          34
     *BPATH_SWCX                                                          34
      DATA LINK_SWCX/0/,BANK_SWCX/LEN_SWCX*0/                             34
C *                                                                       34
C -                                                                       35
      REAL SOUP_VERSION                                                   35
      EQUIVALENCE (BANK_SOUP(3),SOUP_VERSION,RE_SOUP_VERSION),            35
     *(NAMES_SOUP(1),NSOUP_VERSION)                                       35
      EQUIVALENCE (COMMEN_SOUP(1),CSOUP_VERSION),(MAP_SOUP(1,1),          35
     *MSOUP_VERSION)                                                      35
      CHARACTER CSOUP_VERSION*40/' '/                                     35
      CHARACTER NSOUP_VERSION*12/'R.VERSION'/                             35
      INTEGER MSOUP_VERSION(2)/1,1/                                       35
      REAL RE_SOUP_VERSION                                                35
C -                                                                       35
      REAL SOUP_LENGTH                                                    35
      EQUIVALENCE (BANK_SOUP(4),SOUP_LENGTH,RE_SOUP_LENGTH),              35
     *(NAMES_SOUP(2),NSOUP_LENGTH)                                        35
      EQUIVALENCE (COMMEN_SOUP(2),CSOUP_LENGTH),(MAP_SOUP(1,2),           35
     *MSOUP_LENGTH)                                                       35
      CHARACTER CSOUP_LENGTH*40/' '/                                      35
      CHARACTER NSOUP_LENGTH*12/'R.LENGTH'/                               35
      INTEGER MSOUP_LENGTH(2)/1,1/                                        35
      REAL RE_SOUP_LENGTH                                                 35
C -                                                                       35
      REAL SOUP_ROUT                                                      35
      EQUIVALENCE (BANK_SOUP(5),SOUP_ROUT,RE_SOUP_ROUT),(NAMES_SOUP(3),   35
     *NSOUP_ROUT)                                                         35
      EQUIVALENCE (COMMEN_SOUP(3),CSOUP_ROUT),(MAP_SOUP(1,3),             35
     *MSOUP_ROUT)                                                         35
      CHARACTER CSOUP_ROUT*40/' '/                                        35
      CHARACTER NSOUP_ROUT*12/'R.ROUT'/                                   35
      INTEGER MSOUP_ROUT(2)/1,1/                                          35
      REAL RE_SOUP_ROUT                                                   35
C -                                                                       35
      REAL SOUP_DR                                                        35
      EQUIVALENCE (BANK_SOUP(6),SOUP_DR,RE_SOUP_DR),(NAMES_SOUP(4),       35
     *NSOUP_DR)                                                           35
      EQUIVALENCE (COMMEN_SOUP(4),CSOUP_DR),(MAP_SOUP(1,4),MSOUP_DR)      35
      CHARACTER CSOUP_DR*40/' '/                                          35
      CHARACTER NSOUP_DR*12/'R.DR'/                                       35
      INTEGER MSOUP_DR(2)/1,1/                                            35
      REAL RE_SOUP_DR                                                     35
C -                                                                       35
      REAL SOUP_PHI1                                                      35
      EQUIVALENCE (BANK_SOUP(7),SOUP_PHI1,RE_SOUP_PHI1),(NAMES_SOUP(5),   35
     *NSOUP_PHI1)                                                         35
      EQUIVALENCE (COMMEN_SOUP(5),CSOUP_PHI1),(MAP_SOUP(1,5),             35
     *MSOUP_PHI1)                                                         35
      CHARACTER CSOUP_PHI1*40/' '/                                        35
      CHARACTER NSOUP_PHI1*12/'R.PHI1'/                                   35
      INTEGER MSOUP_PHI1(2)/1,1/                                          35
      REAL RE_SOUP_PHI1                                                   35
C -                                                                       35
      REAL SOUP_PHI2                                                      35
      EQUIVALENCE (BANK_SOUP(8),SOUP_PHI2,RE_SOUP_PHI2),(NAMES_SOUP(6),   35
     *NSOUP_PHI2)                                                         35
      EQUIVALENCE (COMMEN_SOUP(6),CSOUP_PHI2),(MAP_SOUP(1,6),             35
     *MSOUP_PHI2)                                                         35
      CHARACTER CSOUP_PHI2*40/' '/                                        35
      CHARACTER NSOUP_PHI2*12/'R.PHI2'/                                   35
      INTEGER MSOUP_PHI2(2)/1,1/                                          35
      REAL RE_SOUP_PHI2                                                   35
C -                                                                       35
      REAL SOUP_DIAMOUT                                                   35
      EQUIVALENCE (BANK_SOUP(9),SOUP_DIAMOUT,RE_SOUP_DIAMOUT),            35
     *(NAMES_SOUP(7),NSOUP_DIAMOUT)                                       35
      EQUIVALENCE (COMMEN_SOUP(7),CSOUP_DIAMOUT),(MAP_SOUP(1,7),          35
     *MSOUP_DIAMOUT)                                                      35
      CHARACTER CSOUP_DIAMOUT*40/' '/                                     35
      CHARACTER NSOUP_DIAMOUT*12/'R.DIAMOUT'/                             35
      INTEGER MSOUP_DIAMOUT(2)/1,1/                                       35
      REAL RE_SOUP_DIAMOUT                                                35
C -                                                                       35
      REAL SOUP_DIAMIN                                                    35
      EQUIVALENCE (BANK_SOUP(10),SOUP_DIAMIN,RE_SOUP_DIAMIN),             35
     *(NAMES_SOUP(8),NSOUP_DIAMIN)                                        35
      EQUIVALENCE (COMMEN_SOUP(8),CSOUP_DIAMIN),(MAP_SOUP(1,8),           35
     *MSOUP_DIAMIN)                                                       35
      CHARACTER CSOUP_DIAMIN*40/' '/                                      35
      CHARACTER NSOUP_DIAMIN*12/'R.DIAMIN'/                               35
      INTEGER MSOUP_DIAMIN(2)/1,1/                                        35
      REAL RE_SOUP_DIAMIN                                                 35
C -                                                                       35
      INTEGER LEN_SOUP,LLEN_SOUP,LINK_SOUP                                35
      PARAMETER (LEN_SOUP=10)                                             35
      PARAMETER (LLEN_SOUP=8)                                             35
      INTEGER FLAG_SOUP/0/,MAP_SOUP(2,LLEN_SOUP),IDNUM_SOUP(10)           35
      CHARACTER NAMES_SOUP(LLEN_SOUP)*12,COMMEN_SOUP(LLEN_SOUP)*40        35
      DATA IDNUM_SOUP/10*0/                                               35
      REAL BANK_SOUP(LEN_SOUP)                                            35
      CHARACTER*60 BPATH_SOUP/' '/                                        35
      SAVE FLAG_SOUP,MAP_SOUP,IDNUM_SOUP,NAMES_SOUP,COMMEN_SOUP,          35
     *BPATH_SOUP                                                          35
      DATA LINK_SOUP/0/,BANK_SOUP/LEN_SOUP*0/                             35
C *                                                                       35
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 36
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         38
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           40
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          42
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    44
      REAL XBUF,PHI,XBUF1,XBUF2                                           46
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     48
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          50
     *SHIELD_PHI(4)                                                       50
      INTEGER I_PHI                                                       52
      RADII(1)= 6.37                                                      54
      RADII(2)= 7.38                                                      55
      RADII(3)=10.38                                                      56
      RADII(4)=11.27                                                      57
      RADII(5)=14.19                                                      58
      RADII(6)=15.13                                                      59
      RAD_OFFSET=0.015                                                    60
      SHIELD_PHI(1)=9.0                                                   61
      SHIELD_PHI(2)=27.0                                                  62
      SHIELD_PHI(3)=45.0                                                  63
      SHIELD_PHI(4)=63.0                                                  64
C *                                                                       65
      CALL AGSBEGM(MODULE,IPRIN)                                          65
      IF (IPRIN.LT.0) RETURN                                              65
C Filling bank SVTG - BASIC SVT DIMENSIONS                                65
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   65
      BPATH_SVTG='SVTG*'                                                  65
      IDNUM_SVTG(1)=0                                                     66
C GEOMETRY VERSION                                                        66
      SVTG_VERSION=2                                                      66
      CSVTG_VERSION='GEOMETRY VERSION'                                    66
C NUMBER OF SVT LAYERS (WAS 7)                                            67
      SVTG_NLAYER=7                                                       67
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         67
C STV INNERMOST RADIUS                                                    68
      SVTG_RSIZEMIN=4.100                                                 68
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               68
C STV OUTERMOST RADIUS                                                    69
      SVTG_RSIZEMAX=46.107                                                69
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               69
C SVT+FTPC LENGTH                                                         70
      SVTG_ZSIZEMAX=270                                                   70
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    70
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          71
      SVTG_ANGOFF=0                                                       71
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       71
C VERSIONING OF THE SHIELD                                                72
      SVTG_SUPPORTVER=1                                                   72
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         72
C VERSIONING OF THE SUPPORT CONE                                          73
      SVTG_CONEVER=1                                                      73
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      73
C WHETHER WE USE THE GEANT MANY OPTION                                    74
      SVTG_IFMANY=0                                                       74
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 74
C THE INDEX OF THE INNERMOST LAYER                                        75
      SVTG_NMIN=1                                                         75
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       75
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    76
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           76
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   76
C *                                                                       76
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    76
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           76
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   76
C *                                                                       77
      CALL AGSBEGM(MODULE,IPRIN)                                          77
      IF (IPRIN.LT.0) RETURN                                              77
C Filling bank SWCA - WAFER CARRIER                                       77
      BANK_TITLE='WAFER CARRIER'                                          77
      BPATH_SWCA='SWCA*'                                                  77
      IDNUM_SWCA(1)=0                                                     78
C GEOMETRY VERSION                                                        78
      SWCA_VERSION=1                                                      78
      CSWCA_VERSION='GEOMETRY VERSION'                                    78
C LADDER LENGTH                                                           79
      SWCA_LENGTH=56.04                                                   79
      CSWCA_LENGTH='LADDER LENGTH'                                        79
C WAFER WIDTH                                                             80
      SWCA_WAFERWID=6.305                                                 80
      CSWCA_WAFERWID='WAFER WIDTH'                                        80
C WAFER LENGTH                                                            81
      SWCA_WAFERLEN=6.305                                                 81
      CSWCA_WAFERLEN='WAFER LENGTH'                                       81
C WAFER THICKNESS                                                         82
      SWCA_WAFERTHK=0.0300                                                82
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    82
C ROHA CELL PLUS GLUE THICKNESS                                           83
      SWCA_ROHATHK=0.0381                                                 83
      CSWCA_ROHATHK='ROHA CELL PLUS GLUE THICKNESS'                       83
C WAFER CARRIER RAILS WIDTH                                               84
      SWCA_WAFCARWD=1.5                                                   84
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          84
C WAFER CARRIER THICKNESS                                                 85
      SWCA_WAFCARTH=0.0300                                                85
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            85
C WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIONS)                   86
      SWCA_WAFERGAP=0.05                                                  86
      CSWCA_WAFERGAP='WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIO  86
     *NS)'                                                                86
C DRIFT DIRECTION                                                         87
      SWCA_DRIFT=1                                                        87
      CSWCA_DRIFT='DRIFT DIRECTION'                                       87
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        88
      SWCA_STRUTLEN=1.0                                                   88
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   88
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    89
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           89
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   89
C *                                                                       89
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    89
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           89
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   89
C *                                                                       90
      CALL AGSBEGM(MODULE,IPRIN)                                          90
      IF (IPRIN.LT.0) RETURN                                              90
C Filling bank SSUP - SUPPORT STRUCTURES                                  90
      BANK_TITLE='SUPPORT STRUCTURES'                                     90
      BPATH_SSUP='SSUP*'                                                  90
      IDNUM_SSUP(1)=0                                                     91
C GEOMETRY VERSION                                                        91
      SSUP_VERSION=1                                                      91
      CSSUP_VERSION='GEOMETRY VERSION'                                    91
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            92
      SSUP_CABTHK=0.05                                                    92
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         92
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             93
      SSUP_HOSRMN=0.75                                                    93
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          93
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             94
      SSUP_HOSRMX=0.95                                                    94
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          94
C NUMBER OF WATER HOSES                                                   95
      SSUP_NHOSES=10                                                      95
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                95
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             96
      SSUP_WRPMYTHK=0.10                                                  96
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        96
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   97
      SSUP_WRPALTHK=0.01                                                  97
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              97
C SUPPORT CONE THICKNESS                                                  98
      SSUP_GRPHTHK=0.16                                                   98
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              98
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              99
      SSUP_CONE1ZMN=52.23                                                 99
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         99
C LENGTH OF SUPPORT RODS                                                  100
      SSUP_RODLEN=110.8                                                   100
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               100
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               101
      SSUP_RODDIST=17.5                                                   101
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           101
C ID OF CARBON SUPPORT RODS (APPROX)                                      102
      SSUP_RODID=2.5                                                      102
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    102
C OD OF CARBON SUPPORT RODS (APPROX)                                      103
      SSUP_RODOD=3.05                                                     103
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    103
C MINIMUM ID OF CONE 1                                                    104
      SSUP_CON1IDMN=15.67                                                 104
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               104
C MINIMUM ID OF CONE 3 (TBD)                                              105
      SSUP_CON3IDMN=21.67                                                 105
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         105
C MINIMUM ID OF CONE 4 (TBD)                                              106
      SSUP_CON4IDMN=37.4                                                  106
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         106
C MAXIMUM ID OF CONE 4 (TBD)                                              107
      SSUP_CON4IDMX=37.4                                                  107
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         107
C MAXIMUM Z OF CONE 3 (TBD)                                               108
      SSUP_CONE3ZMX=150.0                                                 108
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          108
C MAXIMUM Z OF CONE 4 (TBD)                                               109
      SSUP_CONE4ZMX=229.36                                                109
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          109
C THICKNESS OF AL BRACKETS                                                110
      SSUP_BRATHK=.2                                                      110
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             110
C (Z) THICKNESS OF END RING JOINING BRACKETS                              111
      SSUP_ERJTHK=.1                                                      111
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           111
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          112
      SSUP_ERJWID=2.07                                                    112
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       112
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            113
      SSUP_ERJLEN=5.19                                                    113
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         113
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    114
      SSUP_ERJZDIS=2.0                                                    114
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                114
C ERJ SCREW 1 X POSITION (RADIAL)                                         115
      SSUP_ERJ1X=0.31                                                     115
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       115
C ERJ SCREW 2 X POSITION                                                  116
      SSUP_ERJ2X=1.15                                                     116
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                116
C ERJ SCREW 2 Y POSITION                                                  117
      SSUP_ERJ2Y=0.72                                                     117
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                117
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   118
      SSUP_ERJRAD=10.80                                                   118
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                118
C ERJ SCREW DIAMETER                                                      119
      SSUP_ERJDIA=0.17                                                    119
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   119
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    120
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           120
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   120
C *                                                                       120
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    120
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           120
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   120
C *                                                                       121
      CALL AGSBEGM(MODULE,IPRIN)                                          121
      IF (IPRIN.LT.0) RETURN                                              121
C Filling bank SSUP - SUPPORT STRUCTURES                                  121
      BANK_TITLE='SUPPORT STRUCTURES'                                     121
      BPATH_SSUP='SSUP*'                                                  121
      IDNUM_SSUP(1)=0                                                     122
C GEOMETRY VERSION                                                        122
      SSUP_VERSION=2                                                      122
      CSSUP_VERSION='GEOMETRY VERSION'                                    122
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            123
      SSUP_CABTHK=0.21                                                    123
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         123
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             124
      SSUP_HOSRMN=0.75                                                    124
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          124
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             125
      SSUP_HOSRMX=0.95                                                    125
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          125
C NUMBER OF WATER HOSES                                                   126
      SSUP_NHOSES=10                                                      126
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                126
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             127
      SSUP_WRPMYTHK=0.10                                                  127
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        127
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   128
      SSUP_WRPALTHK=0.01                                                  128
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              128
C SUPPORT CONE THICKNESS                                                  129
      SSUP_GRPHTHK=0.16                                                   129
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              129
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              130
      SSUP_CONE1ZMN=52.23                                                 130
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         130
C LENGTH OF SUPPORT RODS                                                  131
      SSUP_RODLEN=110.8                                                   131
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               131
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               132
      SSUP_RODDIST=17.5                                                   132
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           132
C ID OF CARBON SUPPORT RODS (APPROX)                                      133
      SSUP_RODID=2.5                                                      133
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    133
C OD OF CARBON SUPPORT RODS (APPROX)                                      134
      SSUP_RODOD=3.05                                                     134
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    134
C MINIMUM ID OF CONE 1                                                    135
      SSUP_CON1IDMN=15.67                                                 135
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               135
C MINIMUM ID OF CONE 3 (TBD)                                              136
      SSUP_CON3IDMN=21.67                                                 136
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         136
C MINIMUM ID OF CONE 4 (TBD)                                              137
      SSUP_CON4IDMN=37.4                                                  137
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         137
C MAXIMUM ID OF CONE 4 (TBD)                                              138
      SSUP_CON4IDMX=37.4                                                  138
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         138
C MAXIMUM Z OF CONE 3 (TBD)                                               139
      SSUP_CONE3ZMX=150.0                                                 139
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          139
C MAXIMUM Z OF CONE 4 (TBD)                                               140
      SSUP_CONE4ZMX=229.36                                                140
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          140
C THICKNESS OF AL BRACKETS                                                141
      SSUP_BRATHK=.2                                                      141
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             141
C (Z) THICKNESS OF END RING JOINING BRACKETS                              142
      SSUP_ERJTHK=.1                                                      142
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           142
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          143
      SSUP_ERJWID=2.07                                                    143
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       143
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            144
      SSUP_ERJLEN=5.19                                                    144
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         144
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    145
      SSUP_ERJZDIS=2.0                                                    145
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                145
C ERJ SCREW 1 X POSITION (RADIAL)                                         146
      SSUP_ERJ1X=0.31                                                     146
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       146
C ERJ SCREW 2 X POSITION                                                  147
      SSUP_ERJ2X=1.15                                                     147
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                147
C ERJ SCREW 2 Y POSITION                                                  148
      SSUP_ERJ2Y=0.72                                                     148
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                148
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   149
      SSUP_ERJRAD=10.80                                                   149
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                149
C ERJ SCREW DIAMETER                                                      150
      SSUP_ERJDIA=0.17                                                    150
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   150
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    151
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           151
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   151
C *                                                                       151
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    151
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           151
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   151
C *                                                                       152
      CALL AGSBEGM(MODULE,IPRIN)                                          152
      IF (IPRIN.LT.0) RETURN                                              152
C Filling bank SSUB - BEAMPIPE SUPPORT                                    152
      BANK_TITLE='BEAMPIPE SUPPORT'                                       152
      BPATH_SSUB='SSUB*'                                                  152
      IDNUM_SSUB(1)=0                                                     153
C GEOMETRY VERSION                                                        153
      SSUB_VERSION=1                                                      153
      CSSUB_VERSION='GEOMETRY VERSION'                                    153
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  154
      SSUB_KMOUNTID=31.34                                                 154
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             154
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  155
      SSUB_KMOUNTOD=38.96                                                 155
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             155
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    156
      SSUB_KMNTTHK=0.64                                                   156
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                156
C OD OF CUTOUT IN KINEMATIC MOUNT                                         157
      SSUB_KMCUTOD=18.31                                                  157
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     157
C ID OF CUTOUT IN KINEMATIC MOUNT                                         158
      SSUB_KMCUTID=14                                                     158
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     158
C OPENING ANGLE OF CUTOUT                                                 159
      SSUB_KMCUTOA=38                                                     159
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             159
C OFFSET OF CUTOUT CENTER FROM AXIS                                       160
      SSUB_KMCUTOFF=26.58                                                 160
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  160
C ID OF BEAMPIPE SUPPORT RING                                             161
      SSUB_SRINGID=8.47                                                   161
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         161
C OD OF BEAMPIPE SUPPORT RING                                             162
      SSUB_SRINGOD=12.78                                                  162
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         162
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     163
      SSUB_SRINGTHK=0.64                                                  163
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                163
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     164
      SSUB_SRCUTPHI=38                                                    164
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                164
C SUPPORT RING CUTOUT WIDTH                                               165
      SSUB_SRCUTWID=3.63                                                  165
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          165
C SUPPORT RING CUTOUT DEPTH                                               166
      SSUB_SRCUTOUT=5.08                                                  166
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          166
C SUPPORT RING CUTOUT START                                               167
      SSUB_SRCUTIN=3.5                                                    167
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           167
C SUPPORT ROLLER ID                                                       168
      SSUB_SROLLID=0.2                                                    168
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   168
C SUPPORT ROLLER OD                                                       169
      SSUB_SROLLOD=0.62                                                   169
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   169
C SUPPORT ROLLER LENGTH                                                   170
      SSUB_SROLLLEN=2.54                                                  170
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              170
C SUPPORT ROLLER AXIS LENGTH                                              171
      SSUB_SWIRELEN=5.08                                                  171
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         171
C MOUNTING BLOCK HEIGHT                                                   172
      SSUB_MBLKHGH=0.97                                                   172
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               172
C OUTER MOUNTING BLOCK WIDTH                                              173
      SSUB_MBLKOWID=2.54                                                  173
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         173
C OUTER MOUNTING BLOCK LENGTH                                             174
      SSUB_MBLKOLEN=1.27                                                  174
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        174
C INNER MOUNTING BLOCK WIDTH                                              175
      SSUB_MBLKIWID=3.175                                                 175
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         175
C INNER MOUNTING BLOCK LENGTH                                             176
      SSUB_MBLKILEN=1.27                                                  176
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        176
C OUTER MOUNTING BLOCK AT RADIUS                                          177
      SSUB_MBLKORAD=17.4                                                  177
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     177
C INNER MOUNTING BLOCK AT RADIUS                                          178
      SSUB_MBLKIRAD=5.42                                                  178
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     178
C MOUNTING SUPPORT ROD DIAMETER                                           179
      SSUB_MRODDIA=0.32                                                   179
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       179
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    180
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           180
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   180
C *                                                                       180
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    180
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           180
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   180
C *                                                                       181
      CALL AGSBEGM(MODULE,IPRIN)                                          181
      IF (IPRIN.LT.0) RETURN                                              181
C Filling bank SERG - END RINGS                                           181
      BANK_TITLE='END RINGS'                                              181
      BPATH_SERG='SERG*'                                                  181
      IDNUM_SERG(1)=0                                                     182
C GEOMETRY VERSION                                                        182
      SERG_VERSION=1                                                      182
      CSERG_VERSION='GEOMETRY VERSION'                                    182
C INNER END RING TUBE MAXIMUM RADIUS                                      183
      SERG_IRNGTRMX=9.703                                                 183
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 183
C INNER END RING POLYGON MINIMUM RADIUS                                   184
      SERG_IRNGPRMN=7.671                                                 184
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              184
C OUTER END RING MINIMUM RADIUS                                           185
      SERG_ORNGRMIN=11.900                                                185
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      185
C OUTER END RING MAXIMUM RADIUS                                           186
      SERG_ORNGRMAX=13.805                                                186
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      186
C END RING THICKNESS                                                      187
      SERG_ENDRNGTH=0.2                                                   187
      CSERG_ENDRNGTH='END RING THICKNESS'                                 187
C MINIMUM Z FOR END RINGS                                                 188
      SERG_ENDRNGZM=23.01                                                 188
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            188
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    189
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           189
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   189
C *                                                                       189
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    189
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           189
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   189
C *                                                                       190
      CALL AGSBEGM(MODULE,IPRIN)                                          190
      IF (IPRIN.LT.0) RETURN                                              190
C Filling bank SWAM - WATER MANIFOLD                                      190
      BANK_TITLE='WATER MANIFOLD'                                         190
      BPATH_SWAM='SWAM*'                                                  190
      IDNUM_SWAM(1)=0                                                     191
C GEOMETRY VERSION                                                        191
      SWAM_VERSION=1                                                      191
      CSWAM_VERSION='GEOMETRY VERSION'                                    191
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                192
      SWAM_ZMIN=33.9                                                      192
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               192
C WATER MANIFOLD FULL LENGTH                                              193
      SWAM_LEN=16.0                                                       193
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              193
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         194
      SWAM_RMIN=15.24                                                     194
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        194
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         195
      SWAM_RMAX=16.83                                                     195
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        195
C TRANSITION BOARD THICKNESS                                              196
      SWAM_TBRDTHK=0.1                                                    196
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          196
C WATER MANIFOLD WALL THICKNESS                                           197
      SWAM_WALLTHK=0.1                                                    197
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       197
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    198
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           198
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   198
C *                                                                       198
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    198
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           198
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   198
C *                                                                       199
      CALL AGSBEGM(MODULE,IPRIN)                                          199
      IF (IPRIN.LT.0) RETURN                                              199
C Filling bank SELC - ELECTRONICS CARRIER DATA                            199
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               199
      BPATH_SELC='SELC*'                                                  199
      IDNUM_SELC(1)=0                                                     200
C GEOMETRY VERSION                                                        200
      SELC_VERSION=1                                                      200
      CSELC_VERSION='GEOMETRY VERSION'                                    200
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              201
      SELC_BETHK=0.0500                                                   201
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            201
C THICKNESS OF WATER CHANNEL                                              202
      SELC_WATTHK=0.0750                                                  202
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           202
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    203
      SELC_BEOTHK=0.0500                                                  203
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 203
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                204
      SELC_DYETHK=0.0340                                                  204
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             204
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                205
      SELC_DYEWID=0.3                                                     205
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             205
C SPACE IC CHIPS                                                          206
      SELC_DYESPC=0.1                                                     206
      CSELC_DYESPC='SPACE IC CHIPS'                                       206
C ELECTRONICS CARRIER WIDTH                                               207
      SELC_ELCAWID=2.0                                                    207
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           207
C THICKNESS OF AG-PD CONDUCTOR                                            208
      SELC_AGPDTHK=0.0030                                                 208
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        208
C THICKNESS OF GLASS INSULATOR                                            209
      SELC_GLASSTHK=0.0150                                                209
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       209
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        210
      SELC_CABTHK=0.0033                                                  210
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     210
C CABLE WIDTH                                                             211
      SELC_CABWID=0.6                                                     211
      CSELC_CABWID='CABLE WIDTH'                                          211
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    212
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           212
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   212
C *                                                                       212
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    212
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           212
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   212
C *                                                                       213
      CALL AGSBEGM(MODULE,IPRIN)                                          213
      IF (IPRIN.LT.0) RETURN                                              213
C Filling bank SVTL - SINGLE LAYER DATA                                   213
      BANK_TITLE='SINGLE LAYER DATA'                                      213
      BPATH_SVTL='SVTL*'                                                  213
      IDNUM_SVTL(1)=0                                                     214
C LAYER NUMBER                                                            214
      SVTL_LAYER=1                                                        214
      CSVTL_LAYER='LAYER NUMBER'                                          214
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 215
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     215
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              215
C NUMBER OF LADDER                                                        216
      SVTL_NLADDER=4                                                      216
      CSVTL_NLADDER='NUMBER OF LADDER'                                    216
C NUMBER OF WAFERS                                                        217
      SVTL_NWAFER=4                                                       217
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     217
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        218
      SVTL_BAREEDGE=1.0                                                   218
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   218
C PCB LENGTH                                                              219
      SVTL_PCBLEN=14.9                                                    219
      CSVTL_PCBLEN='PCB LENGTH'                                           219
C PCB WIDTH                                                               220
      SVTL_PCBWIDTH=6.3                                                   220
      CSVTL_PCBWIDTH='PCB WIDTH'                                          220
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            221
      SVTL_PCBTHK=0.1                                                     221
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  221
     *R COPPER'                                                           221
C GAP BETWEEN THE PCB AND THE SI WAFER                                    222
      SVTL_PCBGAP=0.2                                                     222
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 222
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    223
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           223
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   223
C *                                                                       223
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    223
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           223
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   223
C *                                                                       224
      CALL AGSBEGM(MODULE,IPRIN)                                          224
      IF (IPRIN.LT.0) RETURN                                              224
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             224
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                224
      BPATH_SVTL='SVTL*'                                                  224
      IDNUM_SVTL(1)=0                                                     225
C LAYER NUMBER                                                            225
      SVTL_LAYER=2                                                        225
      CSVTL_LAYER='LAYER NUMBER'                                          225
C LAYER RADIUS                                                            226
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     226
      CSVTL_RADIUS='LAYER RADIUS'                                         226
C PCB LENGTH                                                              227
      SVTL_PCBLEN=14.9                                                    227
      CSVTL_PCBLEN='PCB LENGTH'                                           227
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    228
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           228
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   228
C *                                                                       228
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    228
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           228
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   228
C *                                                                       229
      CALL AGSBEGM(MODULE,IPRIN)                                          229
      IF (IPRIN.LT.0) RETURN                                              229
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             229
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                229
      BPATH_SVTL='SVTL*'                                                  229
      IDNUM_SVTL(1)=0                                                     230
C LAYER NUMBER                                                            230
      SVTL_LAYER=3                                                        230
      CSVTL_LAYER='LAYER NUMBER'                                          230
C LAYER RADIUS                                                            231
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     231
      CSVTL_RADIUS='LAYER RADIUS'                                         231
C NUMBER OF LADDER                                                        232
      SVTL_NLADDER=6                                                      232
      CSVTL_NLADDER='NUMBER OF LADDER'                                    232
C NUMBER OF WAFERS                                                        233
      SVTL_NWAFER=6                                                       233
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     233
C PCB LENGTH                                                              234
      SVTL_PCBLEN=7.5                                                     234
      CSVTL_PCBLEN='PCB LENGTH'                                           234
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    235
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           235
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   235
C *                                                                       235
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    235
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           235
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   235
C *                                                                       236
      CALL AGSBEGM(MODULE,IPRIN)                                          236
      IF (IPRIN.LT.0) RETURN                                              236
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             236
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                236
      BPATH_SVTL='SVTL*'                                                  236
      IDNUM_SVTL(1)=0                                                     237
C LAYER NUMBER                                                            237
      SVTL_LAYER=4                                                        237
      CSVTL_LAYER='LAYER NUMBER'                                          237
C LAYER RADIUS                                                            238
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     238
      CSVTL_RADIUS='LAYER RADIUS'                                         238
C PCB LENGTH                                                              239
      SVTL_PCBLEN=7.5                                                     239
      CSVTL_PCBLEN='PCB LENGTH'                                           239
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    240
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           240
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   240
C *                                                                       240
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    240
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           240
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   240
C *                                                                       241
      CALL AGSBEGM(MODULE,IPRIN)                                          241
      IF (IPRIN.LT.0) RETURN                                              241
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             241
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                241
      BPATH_SVTL='SVTL*'                                                  241
      IDNUM_SVTL(1)=0                                                     242
C LAYER NUMBER                                                            242
      SVTL_LAYER=5                                                        242
      CSVTL_LAYER='LAYER NUMBER'                                          242
C LAYER RADIUS                                                            243
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     243
      CSVTL_RADIUS='LAYER RADIUS'                                         243
C NUMBER OF LADDER                                                        244
      SVTL_NLADDER=8                                                      244
      CSVTL_NLADDER='NUMBER OF LADDER'                                    244
C NUMBER OF WAFERS                                                        245
      SVTL_NWAFER=7                                                       245
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     245
C PCB LENGTH                                                              246
      SVTL_PCBLEN=4.4                                                     246
      CSVTL_PCBLEN='PCB LENGTH'                                           246
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    247
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           247
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   247
C *                                                                       247
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    247
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           247
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   247
C *                                                                       248
      CALL AGSBEGM(MODULE,IPRIN)                                          248
      IF (IPRIN.LT.0) RETURN                                              248
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             248
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                248
      BPATH_SVTL='SVTL*'                                                  248
      IDNUM_SVTL(1)=0                                                     249
C LAYER NUMBER                                                            249
      SVTL_LAYER=6                                                        249
      CSVTL_LAYER='LAYER NUMBER'                                          249
C LAYER RADIUS                                                            250
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     250
      CSVTL_RADIUS='LAYER RADIUS'                                         250
C PCB LENGTH                                                              251
      SVTL_PCBLEN=4.4                                                     251
      CSVTL_PCBLEN='PCB LENGTH'                                           251
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    252
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           252
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   252
C *                                                                       252
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    252
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           252
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   252
C *                                                                       253
      CALL AGSBEGM(MODULE,IPRIN)                                          253
      IF (IPRIN.LT.0) RETURN                                              253
C Filling bank SSLD - SHIELDING PARAMETERS                                253
      BANK_TITLE='SHIELDING PARAMETERS'                                   253
      BPATH_SSLD='SSLD*'                                                  253
      IDNUM_SSLD(1)=0                                                     254
C GEOMETRY VERSION                                                        254
      SSLD_VERSION=1                                                      254
      CSSLD_VERSION='GEOMETRY VERSION'                                    254
C INNER SHIELD CYLINDER, INNER RADIUS                                     255
      SSLD_SINRINN=5                                                      255
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 255
C INNER SHIELD CYLINDER, OUTER RADIUS                                     256
      SSLD_SINROUT=5.008                                                  256
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 256
C INNER SHIELD CYLINDER, HALF LENGTH                                      257
      SSLD_SINLEN=53.5                                                    257
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   257
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                258
      SSLD_SSEPRINN=22                                                    258
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           258
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                259
      SSLD_SSEPROUT=22.018                                                259
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           259
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 260
      SSLD_SSEPLEN=55.4                                                   260
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             260
C OUTER SHIELD CYLINDER, INNER RADIUS                                     261
      SSLD_SOUTRINN=29.5                                                  261
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                261
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     262
      SSLD_SOUTROUT=29.52                                                 262
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                262
C OUTER SHIELD CYLINDER, HALF LENGTH                                      263
      SSLD_SOUTLEN=65.4                                                   263
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  263
C ALUMINUM SHIELD MESH INNER DIAMETER                                     264
      SSLD_ALMESHID=9.7                                                   264
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                264
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     265
      SSLD_ALMESHOD=44                                                    265
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                265
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                266
      SSLD_ALMSHTHK=0.03                                                  266
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           266
C ALUMINUM SHIELD MESH Z POSITION                                         267
      SSLD_ALMSHPOS=53.5                                                  267
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    267
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    268
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           268
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   268
C *                                                                       268
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    268
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           268
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   268
C *                                                                       269
      CALL AGSBEGM(MODULE,IPRIN)                                          269
      IF (IPRIN.LT.0) RETURN                                              269
C Filling bank SSLD - SHIELDING PARAMETERS                                269
      BANK_TITLE='SHIELDING PARAMETERS'                                   269
      BPATH_SSLD='SSLD*'                                                  269
      IDNUM_SSLD(1)=0                                                     270
C GEOMETRY VERSION                                                        270
      SSLD_VERSION=2                                                      270
      CSSLD_VERSION='GEOMETRY VERSION'                                    270
C INNER SHIELD CYLINDER, INNER RADIUS                                     271
      SSLD_SINRINN=5.7                                                    271
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 271
C INNER SHIELD CYLINDER, OUTER RADIUS                                     272
      SSLD_SINROUT=5.708                                                  272
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 272
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    273
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           273
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   273
C *                                                                       273
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    273
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           273
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   273
C *                                                                       274
      DO 5011 I=1,4                                                       274
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 275
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 276
5011  CONTINUE                                                            277
5012  CONTINUE                                                            277
C *                                                                       278
      CALL AGSBEGM(MODULE,IPRIN)                                          278
      IF (IPRIN.LT.0) RETURN                                              278
C Filling bank SCBP - CABLING                                             278
      BANK_TITLE='CABLING'                                                278
      BPATH_SCBP='SCBP*'                                                  278
      IDNUM_SCBP(1)=0                                                     279
C LAYER                                                                   279
      SCBP_LAYER=1                                                        279
      CSCBP_LAYER='LAYER'                                                 279
C LENGTH                                                                  280
      SCBP_LEN=1.85                                                       280
      CSCBP_LEN='LENGTH'                                                  280
C MIN RADIUS CLOSER TO WAFERS                                             281
      SCBP_RMIN1=RAD_CONES_IN(1)                                          281
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           281
C MIN RADIUS FURTHER FROM WAFERS                                          282
      SCBP_RMIN2=RAD_CONES_OUT(1)                                         282
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        282
C VOLUME OF COPPER, LV+HV CABLES                                          283
      SCBP_VOL=7.24+3.21                                                  283
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          283
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    284
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           284
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   284
C *                                                                       284
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    284
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           284
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   284
C *                                                                       285
      CALL AGSBEGM(MODULE,IPRIN)                                          285
      IF (IPRIN.LT.0) RETURN                                              285
C Filling bank SCBP - CABLING                                             285
      BANK_TITLE='CABLING'                                                285
      BPATH_SCBP='SCBP*'                                                  285
      IDNUM_SCBP(1)=0                                                     286
C LAYER                                                                   286
      SCBP_LAYER=2                                                        286
      CSCBP_LAYER='LAYER'                                                 286
C MIN RADIUS CLOSER TO WAFERS                                             287
      SCBP_RMIN1=RAD_CONES_IN(2)                                          287
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           287
C MIN RADIUS FURTHER FROM WAFERS                                          288
      SCBP_RMIN2=RAD_CONES_OUT(2)                                         288
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        288
C VOLUME OF COPPER, LV+HV CABLES                                          289
      SCBP_VOL=15.54+5.7                                                  289
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          289
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    290
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           290
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   290
C *                                                                       290
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    290
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           290
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   290
C *                                                                       291
      CALL AGSBEGM(MODULE,IPRIN)                                          291
      IF (IPRIN.LT.0) RETURN                                              291
C Filling bank SCBP - CABLING                                             291
      BANK_TITLE='CABLING'                                                291
      BPATH_SCBP='SCBP*'                                                  291
      IDNUM_SCBP(1)=0                                                     292
C LAYER                                                                   292
      SCBP_LAYER=3                                                        292
      CSCBP_LAYER='LAYER'                                                 292
C MIN RADIUS CLOSER TO WAFERS                                             293
      SCBP_RMIN1=RAD_CONES_IN(3)                                          293
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           293
C MIN RADIUS FURTHER FROM WAFERS                                          294
      SCBP_RMIN2=RAD_CONES_OUT(3)                                         294
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        294
C VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE                   295
      SCBP_VOL=4.05+2.02+3.67+1.69                                        295
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE'   295
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    296
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           296
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   296
C *                                                                       296
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    296
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           296
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   296
C *                                                                       297
      CALL AGSBEGM(MODULE,IPRIN)                                          297
      IF (IPRIN.LT.0) RETURN                                              297
C Filling bank SCBP - CABLING                                             297
      BANK_TITLE='CABLING'                                                297
      BPATH_SCBP='SCBP*'                                                  297
      IDNUM_SCBP(1)=0                                                     298
C LAYER (FORMER 5TH)                                                      298
      SCBP_LAYER=4                                                        298
      CSCBP_LAYER='LAYER (FORMER 5TH)'                                    298
C MIN RADIUS CLOSER TO WAFERS                                             299
      SCBP_RMIN1=RAD_CONES_IN(4)                                          299
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           299
C MIN RADIUS FURTHER FROM WAFERS                                          300
      SCBP_RMIN2=RAD_CONES_OUT(4)                                         300
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        300
C VOLUME OF COPPER, LV+HV CABLES                                          301
      SCBP_VOL=6.95+2.43                                                  301
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          301
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    302
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           302
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   302
C *                                                                       302
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    302
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           302
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   302
C *                                                                       303
      CALL AGSBEGM(MODULE,IPRIN)                                          303
      IF (IPRIN.LT.0) RETURN                                              303
C Filling bank SFEP - WATER FEED                                          303
      BANK_TITLE='WATER FEED'                                             303
      BPATH_SFEP='SFEP*'                                                  303
      IDNUM_SFEP(1)=0                                                     304
C LAYER                                                                   304
      SFEP_LAYER=1                                                        304
      CSFEP_LAYER='LAYER'                                                 304
C LENGTH                                                                  305
      SFEP_LEN=1.85                                                       305
      CSFEP_LEN='LENGTH'                                                  305
C MIN RADIUS CLOSER TO WAFERS                                             306
      SFEP_RMIN1=0.5*(RAD_CONES_IN(1) +RAD_CONES_IN(2))                   306
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           306
C MIN RADIUS FURTHER FROM WAFERS                                          307
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(1)+RAD_CONES_OUT(2))                  307
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        307
C VOLUME OF WATER                                                         308
      SFEP_VOL=16.0                                                       308
      CSFEP_VOL='VOLUME OF WATER'                                         308
C VOLUME OF PLASTIC                                                       309
      SFEP_VOLPLAST=38.4                                                  309
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  309
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    310
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           310
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   310
C *                                                                       310
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    310
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           310
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   310
C *                                                                       311
      CALL AGSBEGM(MODULE,IPRIN)                                          311
      IF (IPRIN.LT.0) RETURN                                              311
C Filling bank SFEP - WATER FEED                                          311
      BANK_TITLE='WATER FEED'                                             311
      BPATH_SFEP='SFEP*'                                                  311
      IDNUM_SFEP(1)=0                                                     312
C LAYER                                                                   312
      SFEP_LAYER=2                                                        312
      CSFEP_LAYER='LAYER'                                                 312
C MIN RADIUS CLOSER TO WAFERS                                             313
      SFEP_RMIN1=0.5*(RAD_CONES_IN(2) +RAD_CONES_IN(3))                   313
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           313
C MIN RADIUS FURTHER FROM WAFERS                                          314
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(2)+RAD_CONES_OUT(3))                  314
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        314
C VOLUME OF WATER                                                         315
      SFEP_VOL=24.0                                                       315
      CSFEP_VOL='VOLUME OF WATER'                                         315
C VOLUME OF PLASTIC                                                       316
      SFEP_VOLPLAST=57.6                                                  316
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  316
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    317
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           317
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   317
C *                                                                       317
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    317
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           317
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   317
C *                                                                       318
      CALL AGSBEGM(MODULE,IPRIN)                                          318
      IF (IPRIN.LT.0) RETURN                                              318
C Filling bank SFEP - WATER FEED                                          318
      BANK_TITLE='WATER FEED'                                             318
      BPATH_SFEP='SFEP*'                                                  318
      IDNUM_SFEP(1)=0                                                     319
C LAYER                                                                   319
      SFEP_LAYER=3                                                        319
      CSFEP_LAYER='LAYER'                                                 319
C MIN RADIUS CLOSER TO WAFERS                                             320
      SFEP_RMIN1=0.5*(RAD_CONES_IN(3) +RAD_CONES_IN(4))                   320
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           320
C MIN RADIUS FURTHER FROM WAFERS                                          321
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(3)+RAD_CONES_OUT(4))                  321
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        321
C VOLUME OF WATER                                                         322
      SFEP_VOL=32                                                         322
      CSFEP_VOL='VOLUME OF WATER'                                         322
C VOLUME OF PLASTIC                                                       323
      SFEP_VOLPLAST=76.8                                                  323
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  323
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    324
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           324
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   324
C *                                                                       324
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    324
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           324
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   324
C *                                                                       325
      CALL AGSBEGM(MODULE,IPRIN)                                          325
      IF (IPRIN.LT.0) RETURN                                              325
C Filling bank SWCX - SEGMENTS OF THE WATER DISTRIBUTION PIPES            325
      BANK_TITLE='SEGMENTS OF THE WATER DISTRIBUTION PIPES'               325
      BPATH_SWCX='SWCX*'                                                  325
      IDNUM_SWCX(1)=0                                                     326
C VERSION                                                                 326
      SWCX_LAYER=1                                                        326
      CSWCX_LAYER='VERSION'                                               326
C OF THE RING IN THE Z DIRECTION                                          327
      SWCX_LENGTH=2.8                                                     327
      CSWCX_LENGTH='OF THE RING IN THE Z DIRECTION'                       327
C THICKNESS OF THE MOTHER LAYER                                           328
      SWCX_DR=0.72                                                        328
      CSWCX_DR='THICKNESS OF THE MOTHER LAYER'                            328
C INNER PLASTIC TUBE RADIUS                                               329
      SWCX_RAD=0.2                                                        329
      CSWCX_RAD='INNER PLASTIC TUBE RADIUS'                               329
C FROM THE EDGE OF THE LADDER SUPPORT, INWARD                             330
      SWCX_OFFSET=-2.0                                                    330
      CSWCX_OFFSET='FROM THE EDGE OF THE LADDER SUPPORT, INWARD'          330
C RADIAL OFFSET                                                           331
      SWCX_ROFFSET=1.0                                                    331
      CSWCX_ROFFSET='RADIAL OFFSET'                                       331
C THICKNESS OF THE PLASTIC PIPE WALL                                      332
      SWCX_WALL=0.16                                                      332
      CSWCX_WALL='THICKNESS OF THE PLASTIC PIPE WALL'                     332
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    333
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           333
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   333
C *                                                                       333
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    333
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           333
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   333
C *                                                                       334
      CALL AGSBEGM(MODULE,IPRIN)                                          334
      IF (IPRIN.LT.0) RETURN                                              334
C Filling bank SOUP - MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS      334
      BANK_TITLE='MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS'         334
      BPATH_SOUP='SOUP*'                                                  334
      IDNUM_SOUP(1)=0                                                     335
C VERSION                                                                 335
      SOUP_VERSION=1                                                      335
      CSOUP_VERSION='VERSION'                                             335
C LENGTH                                                                  336
      SOUP_LENGTH=82.5                                                    336
      CSOUP_LENGTH='LENGTH'                                               336
C OUTER RADIUS OF THE SHIELD                                              337
      SOUP_ROUT=19.41                                                     337
      CSOUP_ROUT='OUTER RADIUS OF THE SHIELD'                             337
C DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF THE MOTHER)     338
      SOUP_DR=0.711                                                       338
      CSOUP_DR='DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF   338
     *THE MOTHER)'                                                        338
C STARTING ANGLE OF THE MOTHER                                            339
      SOUP_PHI1=0.0                                                       339
      CSOUP_PHI1='STARTING ANGLE OF THE MOTHER'                           339
C FINISHING ANGLE OF THE MOTHER                                           340
      SOUP_PHI2=70.0                                                      340
      CSOUP_PHI2='FINISHING ANGLE OF THE MOTHER'                          340
C OUTER DIAM OF THE CARBON TUBE                                           341
      SOUP_DIAMOUT=0.711                                                  341
      CSOUP_DIAMOUT='OUTER DIAM OF THE CARBON TUBE'                       341
C INNER DIAM OF THE CARBON TUBE                                           342
      SOUP_DIAMIN=0.620                                                   342
      CSOUP_DIAMIN='INNER DIAM OF THE CARBON TUBE'                        342
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    343
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           343
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   343
C *                                                                       343
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    343
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           343
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   343
C use default SVTG bank                                                   344
      BPATH_SVTG='SVTG*'                                                  344
      IDNUM_SVTG(1)=0                                                     344
      CALL AGDGETP('SVTTGEO4','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,  344
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      344
     *LINK_SVTG,FLAG_SVTG)                                                344
C use default SWCA bank                                                   345
      BPATH_SWCA='SWCA*'                                                  345
      IDNUM_SWCA(1)=0                                                     345
      CALL AGDGETP('SVTTGEO4','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,  345
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      345
     *LINK_SWCA,FLAG_SWCA)                                                345
C use default SELC bank                                                   346
      BPATH_SELC='SELC*'                                                  346
      IDNUM_SELC(1)=0                                                     346
      CALL AGDGETP('SVTTGEO4','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  346
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      346
     *LINK_SELC,FLAG_SELC)                                                346
C use SSUP bank                                                           347
      BPATH_SSUP='SSUP*'                                                  347
      IDNUM_SSUP(1)=0                                                     347
      SSUP_VERSION=SVTG_CONEVER                                           347
      CALL AGDGETP('SVTTGEO4','*',-999,'SSUP',NSSUP_VERSION,              347
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        347
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   347
C use default SSUB bank                                                   348
      BPATH_SSUB='SSUB*'                                                  348
      IDNUM_SSUB(1)=0                                                     348
      CALL AGDGETP('SVTTGEO4','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,  348
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      348
     *LINK_SSUB,FLAG_SSUB)                                                348
C use default SERG bank                                                   349
      BPATH_SERG='SERG*'                                                  349
      IDNUM_SERG(1)=0                                                     349
      CALL AGDGETP('SVTTGEO4','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,  349
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      349
     *LINK_SERG,FLAG_SERG)                                                349
C use default SWAM bank                                                   350
      BPATH_SWAM='SWAM*'                                                  350
      IDNUM_SWAM(1)=0                                                     350
      CALL AGDGETP('SVTTGEO4','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,  350
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      350
     *LINK_SWAM,FLAG_SWAM)                                                350
C use default SELC bank                                                   351
      BPATH_SELC='SELC*'                                                  351
      IDNUM_SELC(1)=0                                                     351
      CALL AGDGETP('SVTTGEO4','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  351
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      351
     *LINK_SELC,FLAG_SELC)                                                351
C use SSLD bank                                                           352
      BPATH_SSLD='SSLD*'                                                  352
      IDNUM_SSLD(1)=0                                                     352
      SSLD_VERSION=SVTG_SUPPORTVER                                        352
      CALL AGDGETP('SVTTGEO4','*',-999,'SSLD',NSSLD_VERSION,              352
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        352
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   352
C use default SCBP bank                                                   353
      BPATH_SCBP='SCBP*'                                                  353
      IDNUM_SCBP(1)=0                                                     353
      CALL AGDGETP('SVTTGEO4','*',-999,'SCBP',' ',BANK_SCBP, BPATH_SCBP,  353
     *IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,LLEN_SCBP,      353
     *LINK_SCBP,FLAG_SCBP)                                                353
C use default SWCX bank                                                   354
      BPATH_SWCX='SWCX*'                                                  354
      IDNUM_SWCX(1)=0                                                     354
      CALL AGDGETP('SVTTGEO4','*',-999,'SWCX',' ',BANK_SWCX, BPATH_SWCX,  354
     *IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,LLEN_SWCX,      354
     *LINK_SWCX,FLAG_SWCX)                                                354
C use default SOUP bank                                                   355
      BPATH_SOUP='SOUP*'                                                  355
      IDNUM_SOUP(1)=0                                                     355
      CALL AGDGETP('SVTTGEO4','*',-999,'SOUP',' ',BANK_SOUP, BPATH_SOUP,  355
     *IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,LLEN_SOUP,      355
     *LINK_SOUP,FLAG_SOUP)                                                355
C Check SVTG_SUPPORTVER==2                                                356
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      356
      SVTG_RSIZEMIN = 1.4                                                 357
      ENDIF                                                               358
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             359
      CALL AGSRESET                                                       359
      AG_A = 28.08                                                        359
      AG_Z = 14                                                           359
      AG_W = 0.6*1*28./60.                                                359
      AG_TITLE = 'SI'                                                     359
      AG_EXNAME = 'COMPONENT'                                             359
      AG_PARLIST = 'A_Z_W'                                                359
      CALL AXCOMPONENT                                                    359
C *                                                                       359
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  359
C =                                                                       359
      CALL AGSRESET                                                       360
      AG_A = 16                                                           360
      AG_Z = 8                                                            360
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               360
      AG_TITLE = 'O'                                                      360
      AG_EXNAME = 'COMPONENT'                                             360
      AG_PARLIST = 'A_Z_W'                                                360
      CALL AXCOMPONENT                                                    360
C *                                                                       360
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 360
      CALL AGSRESET                                                       361
      AG_A = 12                                                           361
      AG_Z = 6                                                            361
      AG_W = 0.4*8*12./174.                                               361
      AG_TITLE = 'C'                                                      361
      AG_EXNAME = 'COMPONENT'                                             361
      AG_PARLIST = 'A_Z_W'                                                361
      CALL AXCOMPONENT                                                    361
C *                                                                       361
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  361
      CALL AGSRESET                                                       362
      AG_A = 1                                                            362
      AG_Z = 1                                                            362
      AG_W = 0.4*14*1./174.                                               362
      AG_TITLE = 'H'                                                      362
      AG_EXNAME = 'COMPONENT'                                             362
      AG_PARLIST = 'A_Z_W'                                                362
      CALL AXCOMPONENT                                                    362
C *                                                                       362
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  362
      CALL AGSRESET                                                       363
      AG_DENS = 1.7                                                       363
      AG_TITLE = 'G10'                                                    363
      AG_EXNAME = 'MIXTURE'                                               363
      AG_PARLIST = 'DENS'                                                 363
      CALL AXMIXTURE                                                      363
C *                                                                       363
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       363
      CALL AGSRESET                                                       364
      AG_A = 12                                                           364
      AG_Z = 6                                                            364
      AG_W = 10000*5                                                      364
      AG_TITLE = 'C5'                                                     364
      AG_EXNAME = 'COMPONENT'                                             364
      AG_PARLIST = 'A_Z_W'                                                364
      CALL AXCOMPONENT                                                    364
C *                                                                       364
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        364
      CALL AGSRESET                                                       365
      AG_A = 1                                                            365
      AG_Z = 1                                                            365
      AG_W = 10000*4                                                      365
      AG_TITLE = 'H4'                                                     365
      AG_EXNAME = 'COMPONENT'                                             365
      AG_PARLIST = 'A_Z_W'                                                365
      CALL AXCOMPONENT                                                    365
C *                                                                       365
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       365
      CALL AGSRESET                                                       366
      AG_A = 16                                                           366
      AG_Z = 8                                                            366
      AG_W = 10000*2                                                      366
      AG_TITLE = 'O2'                                                     366
      AG_EXNAME = 'COMPONENT'                                             366
      AG_PARLIST = 'A_Z_W'                                                366
      CALL AXCOMPONENT                                                    366
C *                                                                       366
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 366
      CALL AGSRESET                                                       367
      AG_A = 27                                                           367
      AG_Z = 13                                                           367
      AG_W = 10000*0.2302                                                 367
      AG_TITLE = 'AL'                                                     367
      AG_EXNAME = 'COMPONENT'                                             367
      AG_PARLIST = 'A_Z_W'                                                367
      CALL AXCOMPONENT                                                    367
C *                                                                       367
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              367
      CALL AGSRESET                                                       368
      AG_DENS = 1.432                                                     368
      AG_TITLE = 'ALKAP'                                                  368
      AG_EXNAME = 'MIXTURE'                                               368
      AG_PARLIST = 'DENS'                                                 368
      CALL AXMIXTURE                                                      368
C *                                                                       368
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       368
      CALL AGSRESET                                                       369
      AG_A = 12                                                           369
      AG_Z = 6                                                            369
      AG_W = 10000*5                                                      369
      AG_TITLE = 'C5'                                                     369
      AG_EXNAME = 'COMPONENT'                                             369
      AG_PARLIST = 'A_Z_W'                                                369
      CALL AXCOMPONENT                                                    369
C *                                                                       369
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        369
      CALL AGSRESET                                                       370
      AG_A = 1                                                            370
      AG_Z = 1                                                            370
      AG_W = 10000*4                                                      370
      AG_TITLE = 'H4'                                                     370
      AG_EXNAME = 'COMPONENT'                                             370
      AG_PARLIST = 'A_Z_W'                                                370
      CALL AXCOMPONENT                                                    370
C *                                                                       370
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       370
      CALL AGSRESET                                                       371
      AG_A = 16                                                           371
      AG_Z = 8                                                            371
      AG_W = 10000*2                                                      371
      AG_TITLE = 'O2'                                                     371
      AG_EXNAME = 'COMPONENT'                                             371
      AG_PARLIST = 'A_Z_W'                                                371
      CALL AXCOMPONENT                                                    371
C *                                                                       371
C Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                          371
      CALL AGSRESET                                                       372
      AG_A = 27                                                           372
      AG_Z = 13                                                           372
      AG_W = 986                                                          372
      AG_TITLE = 'AL'                                                     372
      AG_EXNAME = 'COMPONENT'                                             372
      AG_PARLIST = 'A_Z_W'                                                372
      CALL AXCOMPONENT                                                    372
C *                                                                       372
C Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                          372
      CALL AGSRESET                                                       373
      AG_DENS = 1.40845                                                   373
      AG_TITLE = 'SSDALMY'                                                373
      AG_EXNAME = 'MIXTURE'                                               373
      AG_PARLIST = 'DENS'                                                 373
      CALL AXMIXTURE                                                      373
C *                                                                       373
C Check SWAM_LEN>0                                                        373
      IF (SWAM_LEN.GT.0) THEN                                             374
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              375
      CALL AGSRESET                                                       375
      AG_A = 1                                                            375
      AG_Z = 1                                                            375
      AG_W = 2                                                            375
      AG_TITLE = 'H2'                                                     375
      AG_EXNAME = 'COMPONENT'                                             375
      AG_PARLIST = 'A_Z_W'                                                375
      CALL AXCOMPONENT                                                    375
C *                                                                       375
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              375
      CALL AGSRESET                                                       376
      AG_A = 16                                                           376
      AG_Z = 8                                                            376
      AG_W = 1                                                            376
      AG_TITLE = 'O'                                                      376
      AG_EXNAME = 'COMPONENT'                                             376
      AG_PARLIST = 'A_Z_W'                                                376
      CALL AXCOMPONENT                                                    376
C *                                                                       376
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                376
      CALL AGSRESET                                                       377
      AG_DENS = 1.0                                                       377
      AG_TITLE = 'WATER'                                                  377
      AG_EXNAME = 'MIXTURE'                                               377
      AG_PARLIST = 'DENS'                                                 377
      CALL AXMIXTURE                                                      377
C *                                                                       377
      ELSE                                                                377
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             379
      CALL AGSRESET                                                       379
      AG_DENS = 0.0009                                                    379
      AG_TITLE = 'WATER'                                                  379
      AG_EXNAME = 'MIXTURE'                                               379
      AG_PARLIST = 'DENS'                                                 379
      CALL AXMIXTURE                                                      379
C *                                                                       379
      ENDIF                                                               379
C Check SVTG_IFMANY>0                                                     381
      IF (SVTG_IFMANY.GT.0) THEN                                          381
      CALL AGSBEGM(MODULE,IPRIN)                                          382
C Check IPRIN<0                                                           382
      IF (IPRIN.LT.0) RETURN                                              382
C Create SVTT :                                                           382
C Call To Local Procedure SVTT                                            382
      ASSIGN 5030 TO BLOCK_SVTT                                           382
      GO TO  5020                                                         382
5030  CONTINUE                                                            382
C *                                                                       382
C Geant POSITION : SVTT ===> MOTHER='CAVE' KONLY='MANY' <===              383
      CALL AGSRESET                                                       383
      AG_MOTHER = 'CAVE'                                                  383
      AG_KONLY = 'MANY'                                                   383
      AG_TITLE = 'SVTT'                                                   383
      AG_EXNAME = 'POSITION'                                              383
      AG_PARLIST = 'MOTHER_KONLY'                                         383
      CALL AXPOSITION                                                     383
C *                                                                       383
      ELSE                                                                383
      CALL AGSBEGM(MODULE,IPRIN)                                          385
C Check IPRIN<0                                                           385
      IF (IPRIN.LT.0) RETURN                                              385
C Create SVTT :                                                           385
C Call To Local Procedure SVTT                                            385
      ASSIGN 5040 TO BLOCK_SVTT                                           385
      GO TO  5020                                                         385
5040  CONTINUE                                                            385
C *                                                                       385
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           386
      CALL AGSRESET                                                       386
      AG_MOTHER = 'CAVE'                                                  386
      AG_TITLE = 'SVTT'                                                   386
      AG_EXNAME = 'POSITION'                                              386
      AG_PARLIST = 'MOTHER'                                               386
      CALL AXPOSITION                                                     386
C *                                                                       386
      ENDIF                                                               386
C ----------------------------------------------------------------------  388
C ------------ SVTT                                                       388
      CALL AGSENDM                                                        389
      RETURN                                                              389
C Begin Local Procedure SVTT                                              389
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   389
5020  CONTINUE                                                            389
         AG_TITLE='SVTT'                                                  389
         AG_MODULE=MODULE                                                 389
         AG_MARK='V00'                                                    389
         CALL AGSPUSH                                                     389
         AG_MODULE=MODULE                                                 389
C    *                                                                    389
         RSIZEMAX=SSUP_CON4IDMX                                           390
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 391
     *   2.0*SSUP_HOSRMX                                                  391
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                392
         CLEARANCE=SVTG_RSIZEMAX - RSIZEMAX                               393
C    Geant MATERIAL : AIR ===> inherited arguments used <===              394
         CALL AGSRESET                                                    394
         AG_TITLE = 'AIR'                                                 394
         AG_EXNAME = 'MATERIAL'                                           394
         AG_PARLIST = 'NONE'                                              394
         CALL AXMATERIAL                                                  394
C    *                                                                    394
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       394
         CALL AGSRESET                                                    395
         AG_SEEN = 0                                                      395
         AG_COLO = 1                                                      395
         AG_TITLE = 'SVTT'                                                395
         AG_EXNAME = 'ATTRIBUTE'                                          395
         AG_PARLIST = 'SEEN_COLO'                                         395
         CALL AXATTRIBUTE                                                 395
C    *                                                                    395
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  395
C    VTG_ZSIZEMAX <===                                                    395
         CALL AGSRESET                                                    397
         AG_RMIN = SVTG_RSIZEMIN                                          397
         AG_RMAX = SVTG_RSIZEMAX                                          397
         AG_DZ = SVTG_ZSIZEMAX                                            397
         AG_TITLE = 'TUBE'                                                397
         AG_EXNAME = 'SHAPE'                                              397
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      397
         CALL AXSHAPE                                                     397
C    *                                                                    397
C    Create SIRP :                                                        398
C    Call To Local Procedure SIRP                                         398
         ASSIGN 5060 TO BLOCK_SIRP                                        398
         GO TO  5050                                                      398
5060     CONTINUE                                                         398
C    *                                                                    398
C    Geant POSITION : SIRP ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  399
C    2.5 <===                                                             399
         CALL AGSRESET                                                    399
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             399
         AG_ALPHAN=AG_ALPHAN+1                                            399
         AG_ALPHAK(AG_ALPHAN)=3                                           399
         AG_ALPHAL(AG_ALPHAN)=22.5                                        399
         AG_TITLE = 'SIRP'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'Z_ALPHAZ'                                          399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Geant POSITION : SIRP ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  399
C    22.5 <===                                                            399
         CALL AGSRESET                                                    400
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            400
         AG_ALPHAN=AG_ALPHAN+1                                            400
         AG_ALPHAK(AG_ALPHAN)=3                                           400
         AG_ALPHAL(AG_ALPHAN)=22.5                                        400
         AG_TITLE = 'SIRP'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'Z_ALPHAZ'                                          400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Create SIRT :                                                        401
C    Call To Local Procedure SIRT                                         401
         ASSIGN 5080 TO BLOCK_SIRT                                        401
         GO TO  5070                                                      401
5080     CONTINUE                                                         401
C    *                                                                    401
C    Geant POSITION : SIRT ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  402
C    2.5 <===                                                             402
         CALL AGSRESET                                                    402
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             402
         AG_ALPHAN=AG_ALPHAN+1                                            402
         AG_ALPHAK(AG_ALPHAN)=3                                           402
         AG_ALPHAL(AG_ALPHAN)=22.5                                        402
         AG_TITLE = 'SIRT'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'Z_ALPHAZ'                                          402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Geant POSITION : SIRT ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  402
C    22.5 <===                                                            402
         CALL AGSRESET                                                    403
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            403
         AG_ALPHAN=AG_ALPHAN+1                                            403
         AG_ALPHAK(AG_ALPHAN)=3                                           403
         AG_ALPHAL(AG_ALPHAN)=22.5                                        403
         AG_TITLE = 'SIRT'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'Z_ALPHAZ'                                          403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Create SOER :                                                        404
C    Call To Local Procedure SOER                                         404
         ASSIGN 5100 TO BLOCK_SOER                                        404
         GO TO  5090                                                      404
5100     CONTINUE                                                         404
C    *                                                                    404
C    Geant POSITION : SOER ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 <===      405
         CALL AGSRESET                                                    405
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             405
         AG_TITLE = 'SOER'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'Z'                                                 405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Geant POSITION : SOER ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 <===     405
         CALL AGSRESET                                                    406
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            406
         AG_TITLE = 'SOER'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'Z'                                                 406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Create SBRG :                                                        407
C    Call To Local Procedure SBRG                                         407
         ASSIGN 5120 TO BLOCK_SBRG                                        407
         GO TO  5110                                                      407
5120     CONTINUE                                                         407
C    *                                                                    407
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  408
C    /2 <===                                                              408
         CALL AGSRESET                                                    408
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  408
         AG_TITLE = 'SBRG'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'Z'                                                 408
         CALL AXPOSITION                                                  408
C    *                                                                    408
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  408
C    K/2 <===                                                             408
         CALL AGSRESET                                                    409
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 409
         AG_TITLE = 'SBRG'                                                409
         AG_EXNAME = 'POSITION'                                           409
         AG_PARLIST = 'Z'                                                 409
         CALL AXPOSITION                                                  409
C    *                                                                    409
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           409
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             411
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      412
C    Create SOES :                                                        413
C    Call To Local Procedure SOES                                         413
         ASSIGN 5140 TO BLOCK_SOES                                        413
         GO TO  5130                                                      413
5140     CONTINUE                                                         413
C    *                                                                    413
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               414
         CALL AGSRESET                                                    414
         AG_Z = SCREW_Z                                                   414
         AG_KONLY = 'MANY'                                                414
         AG_TITLE = 'SOES'                                                414
         AG_EXNAME = 'POSITION'                                           414
         AG_PARLIST = 'Z_KONLY'                                           414
         CALL AXPOSITION                                                  414
C    *                                                                    414
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              414
         CALL AGSRESET                                                    415
         AG_Z = -SCREW_Z                                                  415
         AG_KONLY = 'MANY'                                                415
         AG_TITLE = 'SOES'                                                415
         AG_EXNAME = 'POSITION'                                           415
         AG_PARLIST = 'Z_KONLY'                                           415
         CALL AXPOSITION                                                  415
C    *                                                                    415
C    Create SIES :                                                        416
C    Call To Local Procedure SIES                                         416
         ASSIGN 5160 TO BLOCK_SIES                                        416
         GO TO  5150                                                      416
5160     CONTINUE                                                         416
C    *                                                                    416
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               417
         CALL AGSRESET                                                    417
         AG_Z = SCREW_Z                                                   417
         AG_KONLY = 'MANY'                                                417
         AG_TITLE = 'SIES'                                                417
         AG_EXNAME = 'POSITION'                                           417
         AG_PARLIST = 'Z_KONLY'                                           417
         CALL AXPOSITION                                                  417
C    *                                                                    417
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              417
         CALL AGSRESET                                                    418
         AG_Z = -SCREW_Z                                                  418
         AG_KONLY = 'MANY'                                                418
         AG_TITLE = 'SIES'                                                418
         AG_EXNAME = 'POSITION'                                           418
         AG_PARLIST = 'Z_KONLY'                                           418
         CALL AXPOSITION                                                  418
C    *                                                                    418
C    Check SWAM_LEN>0                                                     418
         IF (SWAM_LEN.GT.0) THEN                                          419
C    Create SWMM :                                                        420
C    Call To Local Procedure SWMM                                         420
         ASSIGN 5180 TO BLOCK_SWMM                                        420
         GO TO  5170                                                      420
5180     CONTINUE                                                         420
C    *                                                                    420
C    Geant POSITION : SWMM ===> Z=SWAM_ZMIN+SWAM_LEN/2 <===               421
         CALL AGSRESET                                                    421
         AG_Z = SWAM_ZMIN+SWAM_LEN/2                                      421
         AG_TITLE = 'SWMM'                                                421
         AG_EXNAME = 'POSITION'                                           421
         AG_PARLIST = 'Z'                                                 421
         CALL AXPOSITION                                                  421
C    *                                                                    421
C    Geant POSITION : SWMM ===> Z=-SWAM_ZMIN-SWAM_LEN/2 <===              421
         CALL AGSRESET                                                    422
         AG_Z = -SWAM_ZMIN-SWAM_LEN/2                                     422
         AG_TITLE = 'SWMM'                                                422
         AG_EXNAME = 'POSITION'                                           422
         AG_PARLIST = 'Z'                                                 422
         CALL AXPOSITION                                                  422
C    *                                                                    422
         ENDIF                                                            422
C    Check SWAM_LEN>0                                                     424
         IF (SWAM_LEN.GT.0) THEN                                          424
C    Create SBWC :                                                        425
C    Call To Local Procedure SBWC                                         425
         ASSIGN 5200 TO BLOCK_SBWC                                        425
         GO TO  5190                                                      425
5200     CONTINUE                                                         425
C    *                                                                    425
C    Geant POSITION : SBWC ===> Z=(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SW  426
C    AM_ZMIN+SWAM_LEN))/2) <===                                           426
         CALL AGSRESET                                                    426
         AG_Z =                                                           426
     *   (SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)      426
         AG_TITLE = 'SBWC'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'Z'                                                 426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Geant POSITION : SBWC ===> Z=-(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(S  426
C    WAM_ZMIN+SWAM_LEN))/2) THETAZ=180 <===                               426
         CALL AGSRESET                                                    428
         AG_Z =                                                           428
     *   -(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)     428
         AG_THETAZ = 180                                                  428
         AG_TITLE = 'SBWC'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'Z_THETAZ'                                          428
         CALL AXPOSITION                                                  428
C    *                                                                    428
         ENDIF                                                            428
C    Create SCON :                                                        430
C    Call To Local Procedure SCON                                         430
         ASSIGN 5220 TO BLOCK_SCON                                        430
         GO TO  5210                                                      430
5220     CONTINUE                                                         430
C    *                                                                    430
C    Geant POSITION : SCON ===> inherited arguments used <===             431
         CALL AGSRESET                                                    431
         AG_TITLE = 'SCON'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'NONE'                                              431
         CALL AXPOSITION                                                  431
C    *                                                                    431
C    Geant POSITION : SCON ===> THETAZ=180 <===                           431
         CALL AGSRESET                                                    432
         AG_THETAZ = 180                                                  432
         AG_TITLE = 'SCON'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'THETAZ'                                            432
         CALL AXPOSITION                                                  432
C    *                                                                    432
C    Create SBSP :                                                        433
C    Call To Local Procedure SBSP                                         433
         ASSIGN 5240 TO BLOCK_SBSP                                        433
         GO TO  5230                                                      433
5240     CONTINUE                                                         433
C    *                                                                    433
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     434
         CALL AGSRESET                                                    434
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            434
         AG_TITLE = 'SBSP'                                                434
         AG_EXNAME = 'POSITION'                                           434
         AG_PARLIST = 'Z'                                                 434
         CALL AXPOSITION                                                  434
C    *                                                                    434
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  434
C    =180 <===                                                            434
         CALL AGSRESET                                                    435
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           435
         AG_THETAZ = 180                                                  435
         AG_TITLE = 'SBSP'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'Z_THETAZ'                                          435
         CALL AXPOSITION                                                  435
C    *                                                                    435
C    Create SROD :                                                        436
C    Call To Local Procedure SROD                                         436
         ASSIGN 5260 TO BLOCK_SROD                                        436
         GO TO  5250                                                      436
5260     CONTINUE                                                         436
C    *                                                                    436
C    Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===          437
         CALL AGSRESET                                                    437
         AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                 437
         AG_TITLE = 'SROD'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'Y'                                                 437
         CALL AXPOSITION                                                  437
C    *                                                                    437
C    Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===         437
         CALL AGSRESET                                                    438
         AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                438
         AG_TITLE = 'SROD'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'Y'                                                 438
         CALL AXPOSITION                                                  438
C    *                                                                    438
         RADMAX=SVTG_RSIZEMAX                                             438
C    *                                                                    440
         ILAYER=SVTG_NMIN                                                 440
            GO TO 5273                                                    440
5271        ILAYER=ILAYER+1                                               440
5273        IF(ILAYER-(MIN(6,SVTG_NLAYER)).GT.0) GO TO 5272               440
C       Check ILAYER<6                                                    441
            IF (ILAYER.LT.6) THEN                                         441
C       use SVTL bank                                                     442
            BPATH_SVTL='SVTL*'                                            442
            IDNUM_SVTL(1)=0                                               442
            SVTL_LAYER=ILAYER+1                                           442
            CALL AGDGETP('SVTTGEO4','*',-999,'SVTL',NSVTL_LAYER,          442
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    442
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             442
            RADMAX=SVTL_RADIUS                                            443
            ELSE                                                          444
            RADMAX=SVTG_RSIZEMAX                                          445
            ENDIF                                                         446
C       use SVTL bank                                                     447
            BPATH_SVTL='SVTL*'                                            447
            IDNUM_SVTL(1)=0                                               447
            SVTL_LAYER=ILAYER                                             447
            CALL AGDGETP('SVTTGEO4','*',-999,'SVTL',NSVTL_LAYER,          447
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    447
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             447
C       Create SLYD :                                                     448
C       Call To Local Procedure SLYD                                      448
            ASSIGN 5290 TO BLOCK_SLYD                                     448
            GO TO  5280                                                   448
5290        CONTINUE                                                      448
C       *                                                                 448
C       Check ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6                   449
            IF (ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6) THEN           449
C       Geant POSITION : SLYD ===> KONLY='MANY' <===                      450
            CALL AGSRESET                                                 450
            AG_KONLY = 'MANY'                                             450
            AG_TITLE = 'SLYD'                                             450
            AG_EXNAME = 'POSITION'                                        450
            AG_PARLIST = 'KONLY'                                          450
            CALL AXPOSITION                                               450
C       *                                                                 450
            ELSE                                                          450
C       Geant POSITION : SLYD ===> inherited arguments used <===          452
            CALL AGSRESET                                                 452
            AG_TITLE = 'SLYD'                                             452
            AG_EXNAME = 'POSITION'                                        452
            AG_PARLIST = 'NONE'                                           452
            CALL AXPOSITION                                               452
C       *                                                                 452
            ENDIF                                                         452
         GO TO 5271                                                       454
5272     CONTINUE                                                         454
C    Check SVTG_NLAYER<0                                                  455
         IF (SVTG_NLAYER.LT.0) THEN                                       455
C    use SVTL bank                                                        456
         BPATH_SVTL='SVTL*'                                               456
         IDNUM_SVTL(1)=0                                                  456
         SVTL_LAYER=-SVTG_NLAYER                                          456
         CALL AGDGETP('SVTTGEO4','*',-999,'SVTL',NSVTL_LAYER,             456
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       456
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                456
C    Create SLYD :                                                        457
C    Call To Local Procedure SLYD                                         457
         ASSIGN 5300 TO BLOCK_SLYD                                        457
         GO TO  5280                                                      457
5300     CONTINUE                                                         457
C    *                                                                    457
C    Geant POSITION : SLYD ===> inherited arguments used <===             458
         CALL AGSRESET                                                    458
         AG_TITLE = 'SLYD'                                                458
         AG_EXNAME = 'POSITION'                                           458
         AG_PARLIST = 'NONE'                                              458
         CALL AXPOSITION                                                  458
C    *                                                                    458
         ENDIF                                                            458
C    Create SISH :                                                        460
C    Call To Local Procedure SISH                                         460
         ASSIGN 5320 TO BLOCK_SISH                                        460
         GO TO  5310                                                      460
5320     CONTINUE                                                         460
C    *                                                                    460
C    Geant POSITION : SISH ===> inherited arguments used <===             461
         CALL AGSRESET                                                    461
         AG_TITLE = 'SISH'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'NONE'                                              461
         CALL AXPOSITION                                                  461
C    *                                                                    461
C    Create SOSH :                                                        462
C    Call To Local Procedure SOSH                                         462
         ASSIGN 5340 TO BLOCK_SOSH                                        462
         GO TO  5330                                                      462
5340     CONTINUE                                                         462
C    *                                                                    462
C    Geant POSITION : SOSH ===> inherited arguments used <===             463
         CALL AGSRESET                                                    463
         AG_TITLE = 'SOSH'                                                463
         AG_EXNAME = 'POSITION'                                           463
         AG_PARLIST = 'NONE'                                              463
         CALL AXPOSITION                                                  463
C    *                                                                    463
C    Check SVTG_NLAYER>6                                                  463
         IF (SVTG_NLAYER.GT.6) THEN                                       464
C    Create SSSH :                                                        465
C    Call To Local Procedure SSSH                                         465
         ASSIGN 5360 TO BLOCK_SSSH                                        465
         GO TO  5350                                                      465
5360     CONTINUE                                                         465
C    *                                                                    465
C    Geant POSITION : SSSH ===> inherited arguments used <===             466
         CALL AGSRESET                                                    466
         AG_TITLE = 'SSSH'                                                466
         AG_EXNAME = 'POSITION'                                           466
         AG_PARLIST = 'NONE'                                              466
         CALL AXPOSITION                                                  466
C    *                                                                    466
         ENDIF                                                            466
C    Create SALM :                                                        468
C    Call To Local Procedure SALM                                         468
         ASSIGN 5380 TO BLOCK_SALM                                        468
         GO TO  5370                                                      468
5380     CONTINUE                                                         468
C    *                                                                    468
C    Geant POSITION : SALM ===> Z=SSLD_ALMSHPOS-SSLD_ALMSHTHK/2 <===      469
         CALL AGSRESET                                                    469
         AG_Z = SSLD_ALMSHPOS-SSLD_ALMSHTHK/2                             469
         AG_TITLE = 'SALM'                                                469
         AG_EXNAME = 'POSITION'                                           469
         AG_PARLIST = 'Z'                                                 469
         CALL AXPOSITION                                                  469
C    *                                                                    469
C    Geant POSITION : SALM ===> Z=-(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2) <===   469
         CALL AGSRESET                                                    470
         AG_Z = -(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2)                          470
         AG_TITLE = 'SALM'                                                470
         AG_EXNAME = 'POSITION'                                           470
         AG_PARLIST = 'Z'                                                 470
         CALL AXPOSITION                                                  470
C    *                                                                    470
C    Create SCBM :                                                        471
C    Call To Local Procedure SCBM                                         471
         ASSIGN 5400 TO BLOCK_SCBM                                        471
         GO TO  5390                                                      471
5400     CONTINUE                                                         471
C    *                                                                    471
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=SWAM_ZMIN-SCBP_LEN <===     472
         CALL AGSRESET                                                    472
         AG_X = 0.0                                                       472
         AG_Y = 0.0                                                       472
         AG_Z = SWAM_ZMIN-SCBP_LEN                                        472
         AG_TITLE = 'SCBM'                                                472
         AG_EXNAME = 'POSITION'                                           472
         AG_PARLIST = 'X_Y_Z'                                             472
         CALL AXPOSITION                                                  472
C    *                                                                    472
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=-(SWAM_ZMIN-SCBP_LEN) THET  472
C    AZ=180.0 <===                                                        472
         CALL AGSRESET                                                    473
         AG_X = 0.0                                                       473
         AG_Y = 0.0                                                       473
         AG_Z = -(SWAM_ZMIN-SCBP_LEN)                                     473
         AG_THETAZ = 180.0                                                473
         AG_TITLE = 'SCBM'                                                473
         AG_EXNAME = 'POSITION'                                           473
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      473
         CALL AXPOSITION                                                  473
C    *                                                                    473
C    use SVTL bank                                                        473
         BPATH_SVTL='SVTL*'                                               474
         IDNUM_SVTL(1)=0                                                  474
         SVTL_LAYER=6                                                     474
         CALL AGDGETP('SVTTGEO4','*',-999,'SVTL',NSVTL_LAYER,             474
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       474
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                474
C    Create SXRL :                                                        475
C    Call To Local Procedure SXRL                                         475
         ASSIGN 5420 TO BLOCK_SXRL                                        475
         GO TO  5410                                                      475
5420     CONTINUE                                                         475
C    *                                                                    475
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  477
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  477
         CALL AGSRESET                                                    477
         AG_X = 0.0                                                       477
         AG_Y = 0.0                                                       477
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               477
         AG_KONLY = 'MANY'                                                477
         AG_TITLE = 'SXRL'                                                477
         AG_EXNAME = 'POSITION'                                           477
         AG_PARLIST = 'X_Y_Z_KONLY'                                       477
         CALL AXPOSITION                                                  477
C    *                                                                    477
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  477
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 477
         CALL AGSRESET                                                    479
         AG_X = 0.0                                                       479
         AG_Y = 0.0                                                       479
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              479
         AG_KONLY = 'MANY'                                                479
         AG_TITLE = 'SXRL'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Y_Z_KONLY'                                       479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    use SVTL bank                                                        479
         BPATH_SVTL='SVTL*'                                               480
         IDNUM_SVTL(1)=0                                                  480
         SVTL_LAYER=4                                                     480
         CALL AGDGETP('SVTTGEO4','*',-999,'SVTL',NSVTL_LAYER,             480
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       480
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                480
C    Create SXRL :                                                        481
C    Call To Local Procedure SXRL                                         481
         ASSIGN 5430 TO BLOCK_SXRL                                        481
         GO TO  5410                                                      481
5430     CONTINUE                                                         481
C    *                                                                    481
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  483
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  483
         CALL AGSRESET                                                    483
         AG_X = 0.0                                                       483
         AG_Y = 0.0                                                       483
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               483
         AG_KONLY = 'MANY'                                                483
         AG_TITLE = 'SXRL'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'X_Y_Z_KONLY'                                       483
         CALL AXPOSITION                                                  483
C    *                                                                    483
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  483
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 483
         CALL AGSRESET                                                    485
         AG_X = 0.0                                                       485
         AG_Y = 0.0                                                       485
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              485
         AG_KONLY = 'MANY'                                                485
         AG_TITLE = 'SXRL'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'X_Y_Z_KONLY'                                       485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    use SVTL bank                                                        485
         BPATH_SVTL='SVTL*'                                               486
         IDNUM_SVTL(1)=0                                                  486
         SVTL_LAYER=2                                                     486
         CALL AGDGETP('SVTTGEO4','*',-999,'SVTL',NSVTL_LAYER,             486
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       486
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                486
C    Create SXRL :                                                        487
C    Call To Local Procedure SXRL                                         487
         ASSIGN 5440 TO BLOCK_SXRL                                        487
         GO TO  5410                                                      487
5440     CONTINUE                                                         487
C    *                                                                    487
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  489
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  489
         CALL AGSRESET                                                    489
         AG_X = 0.0                                                       489
         AG_Y = 0.0                                                       489
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               489
         AG_KONLY = 'MANY'                                                489
         AG_TITLE = 'SXRL'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'X_Y_Z_KONLY'                                       489
         CALL AXPOSITION                                                  489
C    *                                                                    489
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  489
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 489
         CALL AGSRESET                                                    491
         AG_X = 0.0                                                       491
         AG_Y = 0.0                                                       491
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              491
         AG_KONLY = 'MANY'                                                491
         AG_TITLE = 'SXRL'                                                491
         AG_EXNAME = 'POSITION'                                           491
         AG_PARLIST = 'X_Y_Z_KONLY'                                       491
         CALL AXPOSITION                                                  491
C    *                                                                    491
C    Create SOUM :                                                        492
C    Call To Local Procedure SOUM                                         492
         ASSIGN 5460 TO BLOCK_SOUM                                        492
         GO TO  5450                                                      492
5460     CONTINUE                                                         492
C    *                                                                    492
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 <===                    493
         CALL AGSRESET                                                    493
         AG_X = 0.0                                                       493
         AG_Y = 0.0                                                       493
         AG_Z = 0.0                                                       493
         AG_TITLE = 'SOUM'                                                493
         AG_EXNAME = 'POSITION'                                           493
         AG_PARLIST = 'X_Y_Z'                                             493
         CALL AXPOSITION                                                  493
C    *                                                                    493
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 <===       493
         CALL AGSRESET                                                    494
         AG_X = 0.0                                                       494
         AG_Y = 0.0                                                       494
         AG_Z = 0.0                                                       494
         AG_ALPHAN=AG_ALPHAN+1                                            494
         AG_ALPHAK(AG_ALPHAN)=2                                           494
         AG_ALPHAL(AG_ALPHAN)=180.0                                       494
         AG_TITLE = 'SOUM'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 ALPHAZ=18  494
C    0 <===                                                               494
         CALL AGSRESET                                                    495
         AG_X = 0.0                                                       495
         AG_Y = 0.0                                                       495
         AG_Z = 0.0                                                       495
         AG_ALPHAN=AG_ALPHAN+1                                            495
         AG_ALPHAK(AG_ALPHAN)=2                                           495
         AG_ALPHAL(AG_ALPHAN)=180.0                                       495
         AG_ALPHAN=AG_ALPHAN+1                                            495
         AG_ALPHAK(AG_ALPHAN)=3                                           495
         AG_ALPHAL(AG_ALPHAN)=180                                         495
         AG_TITLE = 'SOUM'                                                495
         AG_EXNAME = 'POSITION'                                           495
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               495
         CALL AXPOSITION                                                  495
C    *                                                                    495
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAZ=180 <===         495
         CALL AGSRESET                                                    496
         AG_X = 0.0                                                       496
         AG_Y = 0.0                                                       496
         AG_Z = 0.0                                                       496
         AG_ALPHAN=AG_ALPHAN+1                                            496
         AG_ALPHAK(AG_ALPHAN)=3                                           496
         AG_ALPHAL(AG_ALPHAN)=180                                         496
         AG_TITLE = 'SOUM'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      496
         CALL AXPOSITION                                                  496
C    *                                                                    496
5021  CALL AGSPOPB                                                        497
      GO TO BLOCK_SVTT, (5020 ,5030,5040)                                 497
C End of Local Procedure                                                  497
C ----------------------------------------------------------------------  498
C ------------ SXRL                                                       498
C Begin Local Procedure SXRL                                              499
C SXRL IS THE MOTHER OF THE CIRCULAR WATER PIPES                          499
5410  CONTINUE                                                            499
         AG_TITLE='SXRL'                                                  499
         AG_MODULE=MODULE                                                 499
         AG_MARK='V00'                                                    499
         CALL AGSPUSH                                                     499
         AG_MODULE=MODULE                                                 499
C    *                                                                    499
C    Geant MATERIAL : AIR ===> inherited arguments used <===              500
         CALL AGSRESET                                                    500
         AG_TITLE = 'AIR'                                                 500
         AG_EXNAME = 'MATERIAL'                                           500
         AG_PARLIST = 'NONE'                                              500
         CALL AXMATERIAL                                                  500
C    *                                                                    500
C    Geant ATTRIBUTE : SXRL ===> SEEN=0 COLO=1 <===                       500
         CALL AGSRESET                                                    501
         AG_SEEN = 0                                                      501
         AG_COLO = 1                                                      501
         AG_TITLE = 'SXRL'                                                501
         AG_EXNAME = 'ATTRIBUTE'                                          501
         AG_PARLIST = 'SEEN_COLO'                                         501
         CALL AXATTRIBUTE                                                 501
C    *                                                                    501
C    Geant SHAPE : TUBE ===> RMIN=SWCX_ROFFSET+SVTL_RADIUS, RMAX=SWCX_RO  501
C    FFSET+SVTL_RADIUS+SWCX_DR DZ=SWCX_LENGTH/2.0 <===                    501
         CALL AGSRESET                                                    503
         AG_RMIN = SWCX_ROFFSET+SVTL_RADIUS                               503
         AG_RMAX = SWCX_ROFFSET+SVTL_RADIUS+SWCX_DR                       503
         AG_DZ = SWCX_LENGTH/2.0                                          503
         AG_TITLE = 'TUBE'                                                503
         AG_EXNAME = 'SHAPE'                                              503
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      503
         CALL AXSHAPE                                                     503
C    *                                                                    503
C    Create SWRP :                                                        504
C    Call To Local Procedure SWRP                                         504
         ASSIGN 5480 TO BLOCK_SWRP                                        504
         GO TO  5470                                                      504
5480     CONTINUE                                                         504
C    *                                                                    504
C    Create SYRU :                                                        505
C    Call To Local Procedure SYRU                                         505
         ASSIGN 5500 TO BLOCK_SYRU                                        505
         GO TO  5490                                                      505
5500     CONTINUE                                                         505
C    *                                                                    505
C    *                                                                    506
         I_PHI=1                                                          506
            GO TO 5513                                                    506
5511        I_PHI=I_PHI+1                                                 506
5513        IF(I_PHI-(4*SVTL_NLADDER).GT.0) GO TO 5512                    506
            TUBE_ANGLE=(PI/(2.0*SVTL_NLADDER))*(I_PHI-0.5)                507
C       Geant POSITION : SWRP ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  509
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  509
C       SWCX_DR/2.0) Z=0.0 <===                                           509
            CALL AGSRESET                                                 509
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     509
     *      2.0)                                                          509
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     509
     *      2.0)                                                          509
            AG_Z = 0.0                                                    509
            AG_TITLE = 'SWRP'                                             509
            AG_EXNAME = 'POSITION'                                        509
            AG_PARLIST = 'X_Y_Z'                                          509
            CALL AXPOSITION                                               509
C       *                                                                 509
C       Geant POSITION : SYRU ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  509
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  509
C       SWCX_DR/2.0) Z=0.0 <===                                           509
            CALL AGSRESET                                                 511
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     511
     *      2.0)                                                          511
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     511
     *      2.0)                                                          511
            AG_Z = 0.0                                                    511
            AG_TITLE = 'SYRU'                                             511
            AG_EXNAME = 'POSITION'                                        511
            AG_PARLIST = 'X_Y_Z'                                          511
            CALL AXPOSITION                                               511
C       *                                                                 511
         GO TO 5511                                                       512
5512     CONTINUE                                                         512
5411  CALL AGSPOPB                                                        513
      GO TO BLOCK_SXRL, (5410 ,5420,5430,5440)                            513
C End of Local Procedure                                                  513
C ----------------------------------------------------------------------  514
C ------------ SWRP                                                       514
C Begin Local Procedure SWRP                                              516
C SWRP IS AN APPROXIMATION OF WATER IN THE CIRCULAR PIPE, A RECTANGULAR,  516
C  ONE                                                                    516
5470  CONTINUE                                                            516
         AG_TITLE='SWRP'                                                  516
         AG_MODULE=MODULE                                                 516
         AG_MARK='V00'                                                    516
         CALL AGSPUSH                                                     516
         AG_MODULE=MODULE                                                 516
C    *                                                                    516
C    Geant MATERIAL : WATER ===> inherited arguments used <===            517
         CALL AGSRESET                                                    517
         AG_TITLE = 'WATER'                                               517
         AG_EXNAME = 'MATERIAL'                                           517
         AG_PARLIST = 'NONE'                                              517
         CALL AXMATERIAL                                                  517
C    *                                                                    517
C    Geant ATTRIBUTE : SWRP ===> SEEN=1 COLO=4 <===                       517
         CALL AGSRESET                                                    518
         AG_SEEN = 1                                                      518
         AG_COLO = 4                                                      518
         AG_TITLE = 'SWRP'                                                518
         AG_EXNAME = 'ATTRIBUTE'                                          518
         AG_PARLIST = 'SEEN_COLO'                                         518
         CALL AXATTRIBUTE                                                 518
C    *                                                                    518
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SWCX_RAD DZ=SWCX_LENGTH/2.0 <  518
C    ===                                                                  518
         CALL AGSRESET                                                    519
         AG_RMIN = 0.0                                                    519
         AG_RMAX = SWCX_RAD                                               519
         AG_DZ = SWCX_LENGTH/2.0                                          519
         AG_TITLE = 'TUBE'                                                519
         AG_EXNAME = 'SHAPE'                                              519
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      519
         CALL AXSHAPE                                                     519
C    *                                                                    519
5471  CALL AGSPOPB                                                        520
      GO TO BLOCK_SWRP, (5470 ,5480)                                      520
C End of Local Procedure                                                  520
C ----------------------------------------------------------------------  521
C ------------ SYRU                                                       521
C Begin Local Procedure SYRU                                              522
C SYRU IS THE WALL OF THE WATER PIPE                                      522
5490  CONTINUE                                                            522
         AG_TITLE='SYRU'                                                  522
         AG_MODULE=MODULE                                                 522
         AG_MARK='V00'                                                    522
         CALL AGSPUSH                                                     522
         AG_MODULE=MODULE                                                 522
C    *                                                                    522
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           523
         CALL AGSRESET                                                    523
         AG_A = 12                                                        523
         AG_Z = 6                                                         523
         AG_W = 1                                                         523
         AG_TITLE = 'C'                                                   523
         AG_EXNAME = 'COMPONENT'                                          523
         AG_PARLIST = 'A_Z_W'                                             523
         CALL AXCOMPONENT                                                 523
C    *                                                                    523
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           523
         CALL AGSRESET                                                    524
         AG_A = 1                                                         524
         AG_Z = 1                                                         524
         AG_W = 2                                                         524
         AG_TITLE = 'H2'                                                  524
         AG_EXNAME = 'COMPONENT'                                          524
         AG_PARLIST = 'A_Z_W'                                             524
         CALL AXCOMPONENT                                                 524
C    *                                                                    524
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             524
         CALL AGSRESET                                                    525
         AG_DENS = 0.935                                                  525
         AG_TITLE = 'CH2'                                                 525
         AG_EXNAME = 'MIXTURE'                                            525
         AG_PARLIST = 'DENS'                                              525
         CALL AXMIXTURE                                                   525
C    *                                                                    525
C    Geant ATTRIBUTE : SYRU ===> SEEN=1 COLO=4 <===                       525
         CALL AGSRESET                                                    526
         AG_SEEN = 1                                                      526
         AG_COLO = 4                                                      526
         AG_TITLE = 'SYRU'                                                526
         AG_EXNAME = 'ATTRIBUTE'                                          526
         AG_PARLIST = 'SEEN_COLO'                                         526
         CALL AXATTRIBUTE                                                 526
C    *                                                                    526
C    Geant SHAPE : TUBE ===> RMIN=SWCX_RAD RMAX=SWCX_RAD+SWCX_WALL, DZ=S  526
C    WCX_LENGTH/2.0 <===                                                  526
         CALL AGSRESET                                                    528
         AG_RMIN = SWCX_RAD                                               528
         AG_RMAX = SWCX_RAD+SWCX_WALL                                     528
         AG_DZ = SWCX_LENGTH/2.0                                          528
         AG_TITLE = 'TUBE'                                                528
         AG_EXNAME = 'SHAPE'                                              528
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      528
         CALL AXSHAPE                                                     528
C    *                                                                    528
5491  CALL AGSPOPB                                                        529
      GO TO BLOCK_SYRU, (5490 ,5500)                                      529
C End of Local Procedure                                                  529
C ----------------------------------------------------------------------  530
C ------------ SOUM                                                       530
C Begin Local Procedure SOUM                                              531
C SOUM IS THE MOTHER OF THE ARRAY OF THE OUTER SHILEDING SUPPORT TUBES    531
5450  CONTINUE                                                            531
         AG_TITLE='SOUM'                                                  531
         AG_MODULE=MODULE                                                 531
         AG_MARK='V00'                                                    531
         CALL AGSPUSH                                                     531
         AG_MODULE=MODULE                                                 531
C    *                                                                    531
C    Geant MATERIAL : AIR ===> inherited arguments used <===              532
         CALL AGSRESET                                                    532
         AG_TITLE = 'AIR'                                                 532
         AG_EXNAME = 'MATERIAL'                                           532
         AG_PARLIST = 'NONE'                                              532
         CALL AXMATERIAL                                                  532
C    *                                                                    532
C    Geant ATTRIBUTE : SOUM ===> SEEN=0 COLO=1 <===                       532
         CALL AGSRESET                                                    533
         AG_SEEN = 0                                                      533
         AG_COLO = 1                                                      533
         AG_TITLE = 'SOUM'                                                533
         AG_EXNAME = 'ATTRIBUTE'                                          533
         AG_PARLIST = 'SEEN_COLO'                                         533
         CALL AXATTRIBUTE                                                 533
C    *                                                                    533
C    Geant SHAPE : TUBS ===> RMIN=SOUP_ROUT-SOUP_DR RMAX=SOUP_ROUT, PHI1  533
C    =SOUP_PHI1 PHI2=SOUP_PHI2 DZ=SOUP_LENGTH/2.0 <===                    533
         CALL AGSRESET                                                    535
         AG_RMIN = SOUP_ROUT-SOUP_DR                                      535
         AG_RMAX = SOUP_ROUT                                              535
         AG_PHI1 = SOUP_PHI1                                              535
         AG_PHI2 = SOUP_PHI2                                              535
         AG_DZ = SOUP_LENGTH/2.0                                          535
         AG_TITLE = 'TUBS'                                                535
         AG_EXNAME = 'SHAPE'                                              535
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            535
         CALL AXSHAPE                                                     535
C    *                                                                    535
C    Create SOUR :                                                        536
C    Call To Local Procedure SOUR                                         536
         ASSIGN 5530 TO BLOCK_SOUR                                        536
         GO TO  5520                                                      536
5530     CONTINUE                                                         536
C    *                                                                    536
C    *                                                                    537
         I_PHI=1                                                          537
            GO TO 5543                                                    537
5541        I_PHI=I_PHI+1                                                 537
5543        IF(I_PHI-(4).GT.0) GO TO 5542                                 537
C       Geant POSITION : SOUR ===> X=COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SO  539
C       UP_ROUT-SOUP_DR/2.0) Y=SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROU  539
C       T-SOUP_DR/2.0) Z=0.0 <===                                         539
            CALL AGSRESET                                                 539
            AG_X = COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    539
     *      2.0)                                                          539
            AG_Y = SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    539
     *      2.0)                                                          539
            AG_Z = 0.0                                                    539
            AG_TITLE = 'SOUR'                                             539
            AG_EXNAME = 'POSITION'                                        539
            AG_PARLIST = 'X_Y_Z'                                          539
            CALL AXPOSITION                                               539
C       *                                                                 539
         GO TO 5541                                                       540
5542     CONTINUE                                                         540
5451  CALL AGSPOPB                                                        541
      GO TO BLOCK_SOUM, (5450 ,5460)                                      541
C End of Local Procedure                                                  541
C ----------------------------------------------------------------------  542
C ------------ SOUR                                                       542
C Begin Local Procedure SOUR                                              543
C SOUR IS THE OUTER SHILEDING SUPPORT TUBES (RODS)                        543
5520  CONTINUE                                                            543
         AG_TITLE='SOUR'                                                  543
         AG_MODULE=MODULE                                                 543
         AG_MARK='V00'                                                    543
         CALL AGSPUSH                                                     543
         AG_MODULE=MODULE                                                 543
C    *                                                                    543
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           544
         CALL AGSRESET                                                    544
         AG_TITLE = 'CARBON'                                              544
         AG_EXNAME = 'MATERIAL'                                           544
         AG_PARLIST = 'NONE'                                              544
         CALL AXMATERIAL                                                  544
C    *                                                                    544
C    Geant ATTRIBUTE : SOUM ===> SEEN=1 COLO=4 <===                       544
         CALL AGSRESET                                                    545
         AG_SEEN = 1                                                      545
         AG_COLO = 4                                                      545
         AG_TITLE = 'SOUM'                                                545
         AG_EXNAME = 'ATTRIBUTE'                                          545
         AG_PARLIST = 'SEEN_COLO'                                         545
         CALL AXATTRIBUTE                                                 545
C    *                                                                    545
C    Geant SHAPE : TUBE ===> RMIN=SOUP_DIAMIN/2.0 RMAX=SOUP_DIAMOUT/2.0,  545
C     DZ=SOUP_LENGTH/2.0 <===                                             545
         CALL AGSRESET                                                    547
         AG_RMIN = SOUP_DIAMIN/2.0                                        547
         AG_RMAX = SOUP_DIAMOUT/2.0                                       547
         AG_DZ = SOUP_LENGTH/2.0                                          547
         AG_TITLE = 'TUBE'                                                547
         AG_EXNAME = 'SHAPE'                                              547
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      547
         CALL AXSHAPE                                                     547
C    *                                                                    547
5521  CALL AGSPOPB                                                        548
      GO TO BLOCK_SOUR, (5520 ,5530)                                      548
C End of Local Procedure                                                  548
C ----------------------------------------------------------------------  549
C ------------ SLYD                                                       549
C Begin Local Procedure SLYD                                              550
C SLYD IS A SINGLE SVT LAYER                                              550
5280  CONTINUE                                                            550
         AG_TITLE='SLYD'                                                  550
         AG_MODULE=MODULE                                                 550
         AG_MARK='V00'                                                    550
         CALL AGSPUSH                                                     550
         AG_MODULE=MODULE                                                 550
C    *                                                                    550
         LADTHK = SWCA_WAFERTHK/2 + SWCA_ROHATHK + SWCA_WAFCARTH          551
         ELETHK=2.0*SELC_BETHK + SELC_WATTHK + SELC_BEOTHK +              552
     *   SELC_AGPDTHK                                                     552
         ELETHK=ELETHK + SELC_GLASSTHK + SELC_DYETHK                      553
         DEG=180.0/SVTL_NLADDER                                           554
         RAD=PI/SVTL_NLADDER                                              555
C    Geant MATERIAL : AIR ===> inherited arguments used <===              556
         CALL AGSRESET                                                    556
         AG_TITLE = 'AIR'                                                 556
         AG_EXNAME = 'MATERIAL'                                           556
         AG_PARLIST = 'NONE'                                              556
         CALL AXMATERIAL                                                  556
C    *                                                                    556
C    Geant ATTRIBUTE : SLYD ===> SEEN=0 COLO=1 <===                       556
         CALL AGSRESET                                                    557
         AG_SEEN = 0                                                      557
         AG_COLO = 1                                                      557
         AG_TITLE = 'SLYD'                                                557
         AG_EXNAME = 'ATTRIBUTE'                                          557
         AG_PARLIST = 'SEEN_COLO'                                         557
         CALL AXATTRIBUTE                                                 557
C    *                                                                    557
         RMIN=SVTL_RADIUS - LADTHK                                        557
         RMAX = SVTL_RADIUS*SVTL_RADIUS                                   559
         RMAX=RMAX+SWCA_WAFERWID*SWCA_WAFERWID/4.0                        560
         RMAX = SQRT(RMAX) + ELETHK                                       561
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   562
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    563
         CALL AGSRESET                                                    563
         AG_RMIN = RMIN                                                   563
         AG_RMAX = RMAX                                                   563
         AG_DZ = SWCA_LENGTH/2                                            563
         AG_TITLE = 'TUBE'                                                563
         AG_EXNAME = 'SHAPE'                                              563
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      563
         CALL AXSHAPE                                                     563
C    *                                                                    563
C    Create SLSD :                                                        564
C    Call To Local Procedure SLSD                                         564
         ASSIGN 5560 TO BLOCK_SLSD                                        564
         GO TO  5550                                                      564
5560     CONTINUE                                                         564
C    *                                                                    564
5281  CALL AGSPOPB                                                        565
      GO TO BLOCK_SLYD, (5280 ,5290,5300)                                 565
C End of Local Procedure                                                  565
C ----------------------------------------------------------------------  566
C ------------ SLSD                                                       566
C Begin Local Procedure SLSD                                              567
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         567
5550  CONTINUE                                                            567
         AG_TITLE='SLSD'                                                  567
         AG_MODULE=MODULE                                                 567
         AG_MARK='V00'                                                    567
         CALL AGSPUSH                                                     567
         AG_MODULE=MODULE                                                 567
C    *                                                                    567
C    Geant ATTRIBUTE : SLSD ===> SEEN=0 COLO=1 <===                       568
         CALL AGSRESET                                                    568
         AG_SEEN = 0                                                      568
         AG_COLO = 1                                                      568
         AG_TITLE = 'SLSD'                                                568
         AG_EXNAME = 'ATTRIBUTE'                                          568
         AG_PARLIST = 'SEEN_COLO'                                         568
         CALL AXATTRIBUTE                                                 568
C    *                                                                    568
C    Check SVTG_NLAYER<0                                                  568
         IF (SVTG_NLAYER.LT.0) THEN                                       569
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               570
         CALL AGSRESET                                                    570
         AG_NDIV = 1                                                      570
         AG_IAXIS = 2                                                     570
         AG_C0 = -90                                                      570
         AG_TITLE = 'DIVISION'                                            570
         AG_EXNAME = 'SHAPE'                                              570
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     570
         CALL AXSHAPE                                                     570
C    *                                                                    570
         ELSE                                                             570
C    Check SVTG_VERSION==1                                                572
         IF (SVTG_VERSION.EQ.1) THEN                                      572
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              573
         ELSE                                                             574
         C0=90-DEG*MOD(ILAYER,2)                                          575
         ENDIF                                                            576
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NLADDER IAXIS=2, C0=C0 <===    578
         CALL AGSRESET                                                    578
         AG_NDIV = SVTL_NLADDER                                           578
         AG_IAXIS = 2                                                     578
         AG_C0 = C0                                                       578
         AG_TITLE = 'DIVISION'                                            578
         AG_EXNAME = 'SHAPE'                                              578
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     578
         CALL AXSHAPE                                                     578
C    *                                                                    578
         ENDIF                                                            578
C    Create SLDI :                                                        580
C    Call To Local Procedure SLDI                                         580
         ASSIGN 5580 TO BLOCK_SLDI                                        580
         GO TO  5570                                                      580
5580     CONTINUE                                                         580
C    *                                                                    580
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ORT=YZX <===                581
         CALL AGSRESET                                                    581
         AG_X = SVTL_RADIUS                                               581
         AG_ALPHAN=AG_ALPHAN+1                                            581
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 581
         AG_TITLE = 'SLDI'                                                581
         AG_EXNAME = 'POSITION'                                           581
         AG_PARLIST = 'X_ORT'                                             581
         CALL AXPOSITION                                                  581
C    *                                                                    581
C    Create SELE :                                                        582
C    Call To Local Procedure SELE                                         582
         ASSIGN 5600 TO BLOCK_SELE                                        582
         GO TO  5590                                                      582
5600     CONTINUE                                                         582
C    *                                                                    582
         DEG=180.0/SVTL_NLADDER                                           583
         RAD=(TWOPI/2)/SVTL_NLADDER                                       584
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 585
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 586
C    *                                                                    587
         S=-1                                                             587
            GOTO 5613                                                     587
5611        S=S+(2)                                                       587
5613        IF((2)*(S-(1)).GT.0) GO TO 5612                               587
            SIDE=S                                                        588
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  590
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=S*DEG <===         590
            CALL AGSRESET                                                 590
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                590
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               590
            AG_ALPHAN=AG_ALPHAN+1                                         590
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              590
            AG_ALPHAN=AG_ALPHAN+1                                         590
            AG_ALPHAK(AG_ALPHAN)=1                                        590
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 590
            AG_ALPHAN=AG_ALPHAN+1                                         590
            AG_ALPHAK(AG_ALPHAN)=3                                        590
            AG_ALPHAL(AG_ALPHAN)=S*DEG                                    590
            AG_TITLE = 'SELE'                                             590
            AG_EXNAME = 'POSITION'                                        590
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          590
            CALL AXPOSITION                                               590
C       *                                                                 590
         GO TO 5611                                                       591
5612     CONTINUE                                                         591
5551  CALL AGSPOPB                                                        592
      GO TO BLOCK_SLSD, (5550 ,5560)                                      592
C End of Local Procedure                                                  592
C ----------------------------------------------------------------------  593
C ------------ SLDI                                                       593
C Begin Local Procedure SLDI                                              594
C SLDI IS A LADDER VOLUME                                                 594
5570  CONTINUE                                                            594
         AG_TITLE='SLDI'                                                  594
         AG_MODULE=MODULE                                                 594
         AG_MARK='V00'                                                    594
         CALL AGSPUSH                                                     594
         AG_MODULE=MODULE                                                 594
C    *                                                                    594
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       595
C    Geant MATERIAL : AIR ===> inherited arguments used <===              596
         CALL AGSRESET                                                    596
         AG_TITLE = 'AIR'                                                 596
         AG_EXNAME = 'MATERIAL'                                           596
         AG_PARLIST = 'NONE'                                              596
         CALL AXMATERIAL                                                  596
C    *                                                                    596
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 SERIAL=SVTL_NWAFER <===    596
         CALL AGSRESET                                                    597
         AG_SEEN = 0                                                      597
         AG_COLO = 1                                                      597
         AG_SERIAL = SVTL_NWAFER                                          597
         AG_TITLE = 'SLDI'                                                597
         AG_EXNAME = 'ATTRIBUTE'                                          597
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  597
         CALL AXATTRIBUTE                                                 597
C    *                                                                    597
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  597
C    HK <===                                                              597
         CALL AGSRESET                                                    599
         AG_DX = SWCA_WAFERWID/2                                          599
         AG_DY = SWCA_LENGTH/2                                            599
         AG_DZ = LADTHK                                                   599
         AG_TITLE = 'BOX'                                                 599
         AG_EXNAME = 'SHAPE'                                              599
         AG_PARLIST = 'DX_DY_DZ'                                          599
         CALL AXSHAPE                                                     599
C    *                                                                    599
C    Create STLI :                                                        600
C    Call To Local Procedure STLI                                         600
         ASSIGN 5630 TO BLOCK_STLI                                        600
         GO TO  5620                                                      600
5630     CONTINUE                                                         600
C    *                                                                    600
C    Geant POSITION : STLI ===> inherited arguments used <===             601
         CALL AGSRESET                                                    601
         AG_TITLE = 'STLI'                                                601
         AG_EXNAME = 'POSITION'                                           601
         AG_PARLIST = 'NONE'                                              601
         CALL AXPOSITION                                                  601
C    *                                                                    601
C    Create SBER :                                                        602
C    Call To Local Procedure SBER                                         602
         ASSIGN 5650 TO BLOCK_SBER                                        602
         GO TO  5640                                                      602
5650     CONTINUE                                                         602
C    *                                                                    602
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  603
C    DTHK+SWCA_WAFCARTH/2 <===                                            603
         CALL AGSRESET                                                    603
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          603
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   603
         AG_TITLE = 'SBER'                                                603
         AG_EXNAME = 'POSITION'                                           603
         AG_PARLIST = 'X_Z'                                               603
         CALL AXPOSITION                                                  603
C    *                                                                    603
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  603
C    DTHK+SWCA_WAFCARTH/2 <===                                            603
         CALL AGSRESET                                                    604
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          604
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   604
         AG_TITLE = 'SBER'                                                604
         AG_EXNAME = 'POSITION'                                           604
         AG_PARLIST = 'X_Z'                                               604
         CALL AXPOSITION                                                  604
C    *                                                                    604
C    Create STAB :                                                        605
C    Call To Local Procedure STAB                                         605
         ASSIGN 5670 TO BLOCK_STAB                                        605
         GO TO  5660                                                      605
5670     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  606
C    WAFCARTH/2 <===                                                      606
         CALL AGSRESET                                                    606
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    606
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   606
         AG_TITLE = 'STAB'                                                606
         AG_EXNAME = 'POSITION'                                           606
         AG_PARLIST = 'Y_Z'                                               606
         CALL AXPOSITION                                                  606
C    *                                                                    606
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  606
C    _WAFCARTH/2 <===                                                     606
         CALL AGSRESET                                                    607
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   607
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   607
         AG_TITLE = 'STAB'                                                607
         AG_EXNAME = 'POSITION'                                           607
         AG_PARLIST = 'Y_Z'                                               607
         CALL AXPOSITION                                                  607
C    *                                                                    607
C    Create STRU :                                                        608
C    Call To Local Procedure STRU                                         608
         ASSIGN 5690 TO BLOCK_STRU                                        608
         GO TO  5680                                                      608
5690     CONTINUE                                                         608
C    *                                                                    608
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  611
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  611
C    =                                                                    611
         CALL AGSRESET                                                    611
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              611
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  611
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   611
         AG_TITLE = 'STRU'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'Y_Z'                                               611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  611
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  611
C    ==                                                                   611
         CALL AGSRESET                                                    614
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             614
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  614
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   614
         AG_TITLE = 'STRU'                                                614
         AG_EXNAME = 'POSITION'                                           614
         AG_PARLIST = 'Y_Z'                                               614
         CALL AXPOSITION                                                  614
C    *                                                                    614
C    Create SRHC :                                                        615
C    Call To Local Procedure SRHC                                         615
         ASSIGN 5710 TO BLOCK_SRHC                                        615
         GO TO  5700                                                      615
5710     CONTINUE                                                         615
C    *                                                                    615
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  617
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         617
         CALL AGSRESET                                                    617
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          617
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                617
         AG_TITLE = 'SRHC'                                                617
         AG_EXNAME = 'POSITION'                                           617
         AG_PARLIST = 'X_Z'                                               617
         CALL AXPOSITION                                                  617
C    *                                                                    617
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  617
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         617
         CALL AGSRESET                                                    619
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          619
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                619
         AG_TITLE = 'SRHC'                                                619
         AG_EXNAME = 'POSITION'                                           619
         AG_PARLIST = 'X_Z'                                               619
         CALL AXPOSITION                                                  619
C    *                                                                    619
C    Create SPCB :                                                        620
C    Call To Local Procedure SPCB                                         620
         ASSIGN 5730 TO BLOCK_SPCB                                        620
         GO TO  5720                                                      620
5730     CONTINUE                                                         620
C    *                                                                    620
         YPCB = SVTL_NWAFER*(SWCA_WAFERLEN + SWCA_WAFERGAP)/2.0 +         621
     *   SVTL_PCBGAP + SVTL_PCBLEN/2.0                                    621
C    Geant POSITION : SPCB ===> X=0.0 Y=YPCB Z=SVTL_PCBTHK/2.0 <===       622
         CALL AGSRESET                                                    622
         AG_X = 0.0                                                       622
         AG_Y = YPCB                                                      622
         AG_Z = SVTL_PCBTHK/2.0                                           622
         AG_TITLE = 'SPCB'                                                622
         AG_EXNAME = 'POSITION'                                           622
         AG_PARLIST = 'X_Y_Z'                                             622
         CALL AXPOSITION                                                  622
C    *                                                                    622
C    Geant POSITION : SPCB ===> X=0.0 Y=-YPCB Z=SVTL_PCBTHK/2.0 ALPHAZ=1  622
C    80.0 <===                                                            622
         CALL AGSRESET                                                    623
         AG_X = 0.0                                                       623
         AG_Y = -YPCB                                                     623
         AG_Z = SVTL_PCBTHK/2.0                                           623
         AG_ALPHAN=AG_ALPHAN+1                                            623
         AG_ALPHAK(AG_ALPHAN)=3                                           623
         AG_ALPHAL(AG_ALPHAN)=180.0                                       623
         AG_TITLE = 'SPCB'                                                623
         AG_EXNAME = 'POSITION'                                           623
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      623
         CALL AXPOSITION                                                  623
C    *                                                                    623
5571  CALL AGSPOPB                                                        624
      GO TO BLOCK_SLDI, (5570 ,5580)                                      624
C End of Local Procedure                                                  624
C ----------------------------------------------------------------------  625
C ------------ SRHC                                                       625
C Begin Local Procedure SRHC                                              626
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     626
5700  CONTINUE                                                            626
         AG_TITLE='SRHC'                                                  626
         AG_MODULE=MODULE                                                 626
         AG_MARK='V00'                                                    626
         CALL AGSPUSH                                                     626
         AG_MODULE=MODULE                                                 626
C    *                                                                    626
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           627
         CALL AGSRESET                                                    627
         AG_A = 12                                                        627
         AG_Z = 6                                                         627
         AG_W = 1                                                         627
         AG_TITLE = 'C'                                                   627
         AG_EXNAME = 'COMPONENT'                                          627
         AG_PARLIST = 'A_Z_W'                                             627
         CALL AXCOMPONENT                                                 627
C    *                                                                    627
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           627
         CALL AGSRESET                                                    628
         AG_A = 1                                                         628
         AG_Z = 1                                                         628
         AG_W = 2                                                         628
         AG_TITLE = 'H2'                                                  628
         AG_EXNAME = 'COMPONENT'                                          628
         AG_PARLIST = 'A_Z_W'                                             628
         CALL AXCOMPONENT                                                 628
C    *                                                                    628
C    Geant MIXTURE : ROHA ===> DENS=0.0304 <===                           628
         CALL AGSRESET                                                    629
         AG_DENS = 0.0304                                                 629
         AG_TITLE = 'ROHA'                                                629
         AG_EXNAME = 'MIXTURE'                                            629
         AG_PARLIST = 'DENS'                                              629
         CALL AXMIXTURE                                                   629
C    *                                                                    629
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       629
         CALL AGSRESET                                                    630
         AG_SEEN = 1                                                      630
         AG_COLO = 3                                                      630
         AG_TITLE = 'SRHC'                                                630
         AG_EXNAME = 'ATTRIBUTE'                                          630
         AG_PARLIST = 'SEEN_COLO'                                         630
         CALL AXATTRIBUTE                                                 630
C    *                                                                    630
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  630
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        630
         CALL AGSRESET                                                    632
         AG_DX = SWCA_WAFCARWD/2                                          632
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              632
         AG_DZ = SWCA_ROHATHK/2                                           632
         AG_TITLE = 'BOX'                                                 632
         AG_EXNAME = 'SHAPE'                                              632
         AG_PARLIST = 'DX_DY_DZ'                                          632
         CALL AXSHAPE                                                     632
C    *                                                                    632
5701  CALL AGSPOPB                                                        633
      GO TO BLOCK_SRHC, (5700 ,5710)                                      633
C End of Local Procedure                                                  633
C ----------------------------------------------------------------------  634
C ------------ STLI                                                       634
C Begin Local Procedure STLI                                              635
C STLI IS THE WAVER PACK CONTAINER                                        635
5620  CONTINUE                                                            635
         AG_TITLE='STLI'                                                  635
         AG_MODULE=MODULE                                                 635
         AG_MARK='V00'                                                    635
         CALL AGSPUSH                                                     635
         AG_MODULE=MODULE                                                 635
C    *                                                                    635
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              636
         CALL AGSRESET                                                    636
         AG_SEEN = 0                                                      636
         AG_COLO = 1                                                      636
         AG_SERIAL = 0                                                    636
         AG_TITLE = 'STLI'                                                636
         AG_EXNAME = 'ATTRIBUTE'                                          636
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  636
         CALL AXATTRIBUTE                                                 636
C    *                                                                    636
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  636
C    P), DZ=SWCA_WAFERTHK/2 <===                                          636
         CALL AGSRESET                                                    638
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              638
         AG_DZ = SWCA_WAFERTHK/2                                          638
         AG_TITLE = 'BOX'                                                 638
         AG_EXNAME = 'SHAPE'                                              638
         AG_PARLIST = 'DY_DZ'                                             638
         CALL AXSHAPE                                                     638
C    *                                                                    638
C    Create STSI :                                                        639
C    Call To Local Procedure STSI                                         639
         ASSIGN 5750 TO BLOCK_STSI                                        639
         GO TO  5740                                                      639
5750     CONTINUE                                                         639
C    *                                                                    639
5621  CALL AGSPOPB                                                        640
      GO TO BLOCK_STLI, (5620 ,5630)                                      640
C End of Local Procedure                                                  640
C ----------------------------------------------------------------------  641
C ------------ STSI                                                       641
C Begin Local Procedure STSI                                              642
C STSI IS A SINGLE WAVER CONTAINER                                        642
5740  CONTINUE                                                            642
         AG_TITLE='STSI'                                                  642
         AG_MODULE=MODULE                                                 642
         AG_MARK='V00'                                                    642
         CALL AGSPUSH                                                     642
         AG_MODULE=MODULE                                                 642
C    *                                                                    642
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            643
         CALL AGSRESET                                                    643
         AG_NDIV = SVTL_NWAFER                                            643
         AG_IAXIS = 2                                                     643
         AG_TITLE = 'DIVISION'                                            643
         AG_EXNAME = 'SHAPE'                                              643
         AG_PARLIST = 'NDIV_IAXIS'                                        643
         CALL AXSHAPE                                                     643
C    *                                                                    643
C    Create SVTD :                                                        644
C    Call To Local Procedure SVTD                                         644
         ASSIGN 5770 TO BLOCK_SVTD                                        644
         GO TO  5760                                                      644
5770     CONTINUE                                                         644
C    *                                                                    644
C    Geant POSITION : SVTD ===> inherited arguments used <===             645
         CALL AGSRESET                                                    645
         AG_TITLE = 'SVTD'                                                645
         AG_EXNAME = 'POSITION'                                           645
         AG_PARLIST = 'NONE'                                              645
         CALL AXPOSITION                                                  645
C    *                                                                    645
5741  CALL AGSPOPB                                                        646
      GO TO BLOCK_STSI, (5740 ,5750)                                      646
C End of Local Procedure                                                  646
C ----------------------------------------------------------------------  647
C ------------ SVTD                                                       647
C Begin Local Procedure SVTD                                              648
C SVTD IS AN ACTIVE WAFER VOLUME                                          648
5760  CONTINUE                                                            648
         AG_TITLE='SVTD'                                                  648
         AG_MODULE=MODULE                                                 648
         AG_MARK='V00'                                                    648
         CALL AGSPUSH                                                     648
         AG_MODULE=MODULE                                                 648
C    *                                                                    648
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          649
         CALL AGSRESET                                                    649
         AG_TITLE = 'SILICON'                                             649
         AG_EXNAME = 'MATERIAL'                                           649
         AG_PARLIST = 'NONE'                                              649
         CALL AXMATERIAL                                                  649
C    *                                                                    649
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         649
         CALL AGSRESET                                                    650
         AG_ISVOL = 1                                                     650
         AG_TITLE = 'SENSITIVE'                                           650
         AG_EXNAME = 'MATERIAL'                                           650
         AG_PARLIST = 'ISVOL'                                             650
         CALL AXMATERIAL                                                  650
C    *                                                                    650
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       650
         CALL AGSRESET                                                    651
         AG_SEEN = 1                                                      651
         AG_COLO = 4                                                      651
         AG_TITLE = 'SVTD'                                                651
         AG_EXNAME = 'ATTRIBUTE'                                          651
         AG_PARLIST = 'SEEN_COLO'                                         651
         CALL AXATTRIBUTE                                                 651
C    *                                                                    651
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       651
         CALL AGSRESET                                                    652
         AG_DY = SWCA_WAFERLEN/2                                          652
         AG_TITLE = 'BOX'                                                 652
         AG_EXNAME = 'SHAPE'                                              652
         AG_PARLIST = 'DY'                                                652
         CALL AXSHAPE                                                     652
C    *                                                                    652
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 652
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  655
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  655
C    .,0.01)                                                              655
         AG_TITLE = 'SVTD'                                                655
         AG_EXNAME = 'HITS'                                               655
         AG_TYPE=AG_MAGIC                                                 655
         CALL AGSDETV('SVTD')                                             655
C    *                                                                    655
         AG_PARLIST='Z'                                                   655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = AG_MAGIC                                              655
         AG_BIN=.001                                                      655
         AG_OPTION='S'                                                    655
         AG_FHMIN = AG_MAGIC                                              655
         AG_FHMAX = AG_MAGIC                                              655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='Y'                                                   655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = AG_MAGIC                                              655
         AG_BIN=.001                                                      655
         AG_FHMIN=AG_MAGIC                                                655
         AG_FHMAX=AG_MAGIC                                                655
         AG_OPTION='*'                                                    655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='X'                                                   655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = AG_MAGIC                                              655
         AG_BIN=.001                                                      655
         AG_FHMIN=AG_MAGIC                                                655
         AG_FHMAX=AG_MAGIC                                                655
         AG_OPTION='*'                                                    655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='PTOT'                                                655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 18                                                    655
         AG_BIN=AG_MAGIC                                                  655
         AG_OPTION='*'                                                    655
         AG_FHMIN = 0                                                     655
         AG_FHMAX = 100                                                   655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='CX'                                                  655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 10                                                    655
         AG_BIN=AG_MAGIC                                                  655
         AG_FHMIN=AG_MAGIC                                                655
         AG_FHMAX=AG_MAGIC                                                655
         AG_OPTION='*'                                                    655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='CY'                                                  655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 10                                                    655
         AG_BIN=AG_MAGIC                                                  655
         AG_FHMIN=AG_MAGIC                                                655
         AG_FHMAX=AG_MAGIC                                                655
         AG_OPTION='*'                                                    655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='CZ'                                                  655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 10                                                    655
         AG_BIN=AG_MAGIC                                                  655
         AG_FHMIN=AG_MAGIC                                                655
         AG_FHMAX=AG_MAGIC                                                655
         AG_OPTION='*'                                                    655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='SLENG'                                               655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 16                                                    655
         AG_BIN=AG_MAGIC                                                  655
         AG_OPTION='*'                                                    655
         AG_FHMIN = 0                                                     655
         AG_FHMAX = 500                                                   655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='TOF'                                                 655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 16                                                    655
         AG_BIN=AG_MAGIC                                                  655
         AG_OPTION='*'                                                    655
         AG_FHMIN = 0                                                     655
         AG_FHMAX = 1.0E-6                                                655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='STEP'                                                655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = AG_MAGIC                                              655
         AG_BIN=.01                                                       655
         AG_FHMIN=AG_MAGIC                                                655
         AG_FHMAX=AG_MAGIC                                                655
         AG_OPTION='*'                                                    655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='ELOSS'                                               655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 22                                                    655
         AG_BIN=AG_MAGIC                                                  655
         AG_OPTION='*'                                                    655
         AG_FHMIN = 0.                                                    655
         AG_FHMAX = 0.01                                                  655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         CALL AGSDETU('SVTD')                                             655
C    *                                                                    655
5761  CALL AGSPOPB                                                        656
      GO TO BLOCK_SVTD, (5760 ,5770)                                      656
C End of Local Procedure                                                  656
C ----------------------------------------------------------------------  657
C ------------ SBER                                                       657
C Begin Local Procedure SBER                                              658
C SBER ARE THE BERILLIUM WAFER CARRIER RAILS                              658
5640  CONTINUE                                                            658
         AG_TITLE='SBER'                                                  658
         AG_MODULE=MODULE                                                 658
         AG_MARK='V00'                                                    658
         CALL AGSPUSH                                                     658
         AG_MODULE=MODULE                                                 658
C    *                                                                    658
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        659
         CALL AGSRESET                                                    659
         AG_TITLE = 'BERILLIUM'                                           659
         AG_EXNAME = 'MATERIAL'                                           659
         AG_PARLIST = 'NONE'                                              659
         CALL AXMATERIAL                                                  659
C    *                                                                    659
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       659
         CALL AGSRESET                                                    660
         AG_SEEN = 1                                                      660
         AG_COLO = 2                                                      660
         AG_TITLE = 'SBER'                                                660
         AG_EXNAME = 'ATTRIBUTE'                                          660
         AG_PARLIST = 'SEEN_COLO'                                         660
         CALL AXATTRIBUTE                                                 660
C    *                                                                    660
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  660
C    _WAFCARTH/2 <===                                                     660
         CALL AGSRESET                                                    662
         AG_DX = SWCA_WAFCARWD/2                                          662
         AG_DY = SWCA_LENGTH/2                                            662
         AG_DZ = SWCA_WAFCARTH/2                                          662
         AG_TITLE = 'BOX'                                                 662
         AG_EXNAME = 'SHAPE'                                              662
         AG_PARLIST = 'DX_DY_DZ'                                          662
         CALL AXSHAPE                                                     662
C    *                                                                    662
5641  CALL AGSPOPB                                                        663
      GO TO BLOCK_SBER, (5640 ,5650)                                      663
C End of Local Procedure                                                  663
C ----------------------------------------------------------------------  664
C ------------ STAB                                                       664
C Begin Local Procedure STAB                                              665
C STAB ARE THE BERILLIUM WAFER CARRIER END TABS                           665
5660  CONTINUE                                                            665
         AG_TITLE='STAB'                                                  665
         AG_MODULE=MODULE                                                 665
         AG_MARK='V00'                                                    665
         CALL AGSPUSH                                                     665
         AG_MODULE=MODULE                                                 665
C    *                                                                    665
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        666
         CALL AGSRESET                                                    666
         AG_TITLE = 'BERILLIUM'                                           666
         AG_EXNAME = 'MATERIAL'                                           666
         AG_PARLIST = 'NONE'                                              666
         CALL AXMATERIAL                                                  666
C    *                                                                    666
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       666
         CALL AGSRESET                                                    667
         AG_SEEN = 1                                                      667
         AG_COLO = 2                                                      667
         AG_TITLE = 'STAB'                                                667
         AG_EXNAME = 'ATTRIBUTE'                                          667
         AG_PARLIST = 'SEEN_COLO'                                         667
         CALL AXATTRIBUTE                                                 667
C    *                                                                    667
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  667
C    2 DZ=SWCA_WAFCARTH/2 <===                                            667
         CALL AGSRESET                                                    669
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            669
         AG_DY = TABLEN/2                                                 669
         AG_DZ = SWCA_WAFCARTH/2                                          669
         AG_TITLE = 'BOX'                                                 669
         AG_EXNAME = 'SHAPE'                                              669
         AG_PARLIST = 'DX_DY_DZ'                                          669
         CALL AXSHAPE                                                     669
C    *                                                                    669
5661  CALL AGSPOPB                                                        670
      GO TO BLOCK_STAB, (5660 ,5670)                                      670
C End of Local Procedure                                                  670
C ----------------------------------------------------------------------  671
C ------------ STRU                                                       671
C Begin Local Procedure STRU                                              672
C STRU ARE THE BERILLIUM STRUTS BETWEEN THE WAFER CARRIER RAILS           672
5680  CONTINUE                                                            672
         AG_TITLE='STRU'                                                  672
         AG_MODULE=MODULE                                                 672
         AG_MARK='V00'                                                    672
         CALL AGSPUSH                                                     672
         AG_MODULE=MODULE                                                 672
C    *                                                                    672
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        673
         CALL AGSRESET                                                    673
         AG_TITLE = 'BERILLIUM'                                           673
         AG_EXNAME = 'MATERIAL'                                           673
         AG_PARLIST = 'NONE'                                              673
         CALL AXMATERIAL                                                  673
C    *                                                                    673
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       673
         CALL AGSRESET                                                    674
         AG_SEEN = 1                                                      674
         AG_COLO = 2                                                      674
         AG_TITLE = 'STRU'                                                674
         AG_EXNAME = 'ATTRIBUTE'                                          674
         AG_PARLIST = 'SEEN_COLO'                                         674
         CALL AXATTRIBUTE                                                 674
C    *                                                                    674
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  674
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     674
         CALL AGSRESET                                                    676
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            676
         AG_DY = SWCA_STRUTLEN/2                                          676
         AG_DZ = SWCA_WAFCARTH/2                                          676
         AG_TITLE = 'BOX'                                                 676
         AG_EXNAME = 'SHAPE'                                              676
         AG_PARLIST = 'DX_DY_DZ'                                          676
         CALL AXSHAPE                                                     676
C    *                                                                    676
5681  CALL AGSPOPB                                                        677
      GO TO BLOCK_STRU, (5680 ,5690)                                      677
C End of Local Procedure                                                  677
C ----------------------------------------------------------------------  678
C ------------ SPCB                                                       678
C Begin Local Procedure SPCB                                              679
C SPCB IS THE G10 PCB                                                     679
5720  CONTINUE                                                            679
         AG_TITLE='SPCB'                                                  679
         AG_MODULE=MODULE                                                 679
         AG_MARK='V00'                                                    679
         CALL AGSPUSH                                                     679
         AG_MODULE=MODULE                                                 679
C    *                                                                    679
C    Geant MATERIAL : G10 ===> inherited arguments used <===              680
         CALL AGSRESET                                                    680
         AG_TITLE = 'G10'                                                 680
         AG_EXNAME = 'MATERIAL'                                           680
         AG_PARLIST = 'NONE'                                              680
         CALL AXMATERIAL                                                  680
C    *                                                                    680
C    Geant ATTRIBUTE : SPCB ===> SEEN=1 COLO=5 <===                       680
         CALL AGSRESET                                                    681
         AG_SEEN = 1                                                      681
         AG_COLO = 5                                                      681
         AG_TITLE = 'SPCB'                                                681
         AG_EXNAME = 'ATTRIBUTE'                                          681
         AG_PARLIST = 'SEEN_COLO'                                         681
         CALL AXATTRIBUTE                                                 681
C    *                                                                    681
C    Geant SHAPE : BOX ===> DX=SVTL_PCBWIDTH/2.0 DY=SVTL_PCBLEN/2.0, DZ=  681
C    SVTL_PCBTHK/2.0 <===                                                 681
         CALL AGSRESET                                                    683
         AG_DX = SVTL_PCBWIDTH/2.0                                        683
         AG_DY = SVTL_PCBLEN/2.0                                          683
         AG_DZ = SVTL_PCBTHK/2.0                                          683
         AG_TITLE = 'BOX'                                                 683
         AG_EXNAME = 'SHAPE'                                              683
         AG_PARLIST = 'DX_DY_DZ'                                          683
         CALL AXSHAPE                                                     683
C    *                                                                    683
5721  CALL AGSPOPB                                                        684
      GO TO BLOCK_SPCB, (5720 ,5730)                                      684
C End of Local Procedure                                                  684
C ----------------------------------------------------------------------  685
C ------------ SCBM                                                       685
C Begin Local Procedure SCBM                                              686
C SCBM IS THE MOTHER FOR THE BUNDLE OF CABLES GOING FROM PCBS             686
5390  CONTINUE                                                            686
         AG_TITLE='SCBM'                                                  686
         AG_MODULE=MODULE                                                 686
         AG_MARK='V00'                                                    686
         CALL AGSPUSH                                                     686
         AG_MODULE=MODULE                                                 686
C    *                                                                    686
C    Geant MATERIAL : AIR ===> inherited arguments used <===              687
         CALL AGSRESET                                                    687
         AG_TITLE = 'AIR'                                                 687
         AG_EXNAME = 'MATERIAL'                                           687
         AG_PARLIST = 'NONE'                                              687
         CALL AXMATERIAL                                                  687
C    *                                                                    687
C    Geant ATTRIBUTE : SCBM ===> SEEN=0 COLO=1 <===                       687
         CALL AGSRESET                                                    688
         AG_SEEN = 0                                                      688
         AG_COLO = 1                                                      688
         AG_TITLE = 'SCBM'                                                688
         AG_EXNAME = 'ATTRIBUTE'                                          688
         AG_PARLIST = 'SEEN_COLO'                                         688
         CALL AXATTRIBUTE                                                 688
C    *                                                                    688
C    Geant SHAPE : TUBE ===> RMIN=RADII(1) RMAX=SWAM_RMAX, DZ=SCBP_LEN <  688
C    ===                                                                  688
         CALL AGSRESET                                                    690
         AG_RMIN = RADII(1)                                               690
         AG_RMAX = SWAM_RMAX                                              690
         AG_DZ = SCBP_LEN                                                 690
         AG_TITLE = 'TUBE'                                                690
         AG_EXNAME = 'SHAPE'                                              690
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      690
         CALL AXSHAPE                                                     690
C    *                                                                    690
C    *                                                                    690
         ILAYER=1                                                         691
            GO TO 5783                                                    691
5781        ILAYER=ILAYER+1                                               691
5783        IF(ILAYER-(4).GT.0) GO TO 5782                                691
C       use SCBP bank                                                     692
            BPATH_SCBP='SCBP*'                                            692
            IDNUM_SCBP(1)=0                                               692
            SCBP_LAYER=ILAYER                                             692
            CALL AGDGETP('SVTTGEO4','*',-999,'SCBP',NSCBP_LAYER,          692
     *      RE_SCBP_LAYER,BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,    692
     *      BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)             692
C       Create SCBL :                                                     693
C       Call To Local Procedure SCBL                                      693
            ASSIGN 5800 TO BLOCK_SCBL                                     693
            GO TO  5790                                                   693
5800        CONTINUE                                                      693
C       *                                                                 693
C       Geant POSITION : SCBL ===> X=0.0 Y=0.0 Z=0.0 <===                 694
            CALL AGSRESET                                                 694
            AG_X = 0.0                                                    694
            AG_Y = 0.0                                                    694
            AG_Z = 0.0                                                    694
            AG_TITLE = 'SCBL'                                             694
            AG_EXNAME = 'POSITION'                                        694
            AG_PARLIST = 'X_Y_Z'                                          694
            CALL AXPOSITION                                               694
C       *                                                                 694
         GO TO 5781                                                       695
5782     CONTINUE                                                         695
C    *                                                                    696
         ILAYER=1                                                         696
            GO TO 5813                                                    696
5811        ILAYER=ILAYER+1                                               696
5813        IF(ILAYER-(3).GT.0) GO TO 5812                                696
C       use SFEP bank                                                     697
            BPATH_SFEP='SFEP*'                                            697
            IDNUM_SFEP(1)=0                                               697
            SFEP_LAYER=ILAYER                                             697
            CALL AGDGETP('SVTTGEO4','*',-999,'SFEP',NSFEP_LAYER,          697
     *      RE_SFEP_LAYER,BPATH_SFEP,IDNUM_SFEP,1,MAP_SFEP,NAMES_SFEP,    697
     *      BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)             697
C       Create SFED :                                                     698
C       Call To Local Procedure SFED                                      698
            ASSIGN 5830 TO BLOCK_SFED                                     698
            GO TO  5820                                                   698
5830        CONTINUE                                                      698
C       *                                                                 698
C       Geant POSITION : SFED ===> X=0.0 Y=0.0 Z=0.0 <===                 699
            CALL AGSRESET                                                 699
            AG_X = 0.0                                                    699
            AG_Y = 0.0                                                    699
            AG_Z = 0.0                                                    699
            AG_TITLE = 'SFED'                                             699
            AG_EXNAME = 'POSITION'                                        699
            AG_PARLIST = 'X_Y_Z'                                          699
            CALL AXPOSITION                                               699
C       *                                                                 699
C       Create SPLS :                                                     700
C       Call To Local Procedure SPLS                                      700
            ASSIGN 5850 TO BLOCK_SPLS                                     700
            GO TO  5840                                                   700
5850        CONTINUE                                                      700
C       *                                                                 700
C       Geant POSITION : SPLS ===> X=0.0 Y=0.0 Z=0.0 <===                 701
            CALL AGSRESET                                                 701
            AG_X = 0.0                                                    701
            AG_Y = 0.0                                                    701
            AG_Z = 0.0                                                    701
            AG_TITLE = 'SPLS'                                             701
            AG_EXNAME = 'POSITION'                                        701
            AG_PARLIST = 'X_Y_Z'                                          701
            CALL AXPOSITION                                               701
C       *                                                                 701
         GO TO 5811                                                       702
5812     CONTINUE                                                         702
5391  CALL AGSPOPB                                                        703
      GO TO BLOCK_SCBM, (5390 ,5400)                                      703
C End of Local Procedure                                                  703
C ----------------------------------------------------------------------  704
C ------------ SCBL                                                       704
C Begin Local Procedure SCBL                                              705
C SCBL IS THE BUNDLE OF CABLES GOING FROM PCBS TO MANIFOLDS               705
5790  CONTINUE                                                            705
         AG_TITLE='SCBL'                                                  705
         AG_MODULE=MODULE                                                 705
         AG_MARK='V00'                                                    705
         CALL AGSPUSH                                                     705
         AG_MODULE=MODULE                                                 705
C    *                                                                    705
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           706
         CALL AGSRESET                                                    706
         AG_TITLE = 'COPPER'                                              706
         AG_EXNAME = 'MATERIAL'                                           706
         AG_PARLIST = 'NONE'                                              706
         CALL AXMATERIAL                                                  706
C    *                                                                    706
C    Geant ATTRIBUTE : SCBL ===> SEEN=1 COLO=3 <===                       706
         CALL AGSRESET                                                    707
         AG_SEEN = 1                                                      707
         AG_COLO = 3                                                      707
         AG_TITLE = 'SCBL'                                                707
         AG_EXNAME = 'ATTRIBUTE'                                          707
         AG_PARLIST = 'SEEN_COLO'                                         707
         CALL AXATTRIBUTE                                                 707
C    *                                                                    707
         SQ =(SCBP_LEN*SCBP_LEN)                                          707
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  709
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  710
         A = PI                                                           711
         A=A*(SCBP_RMIN1*SCBP_RMIN1 + SCBP_RMIN2*SCBP_RMIN2)              712
         A=A*SQRT(1 + SQ)                                                 713
         CUTHK=(SCBP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            714
C    Geant SHAPE : CONE ===> DZ=SCBP_LEN RMN1=SCBP_RMIN1 RMX1=SCBP_RMIN1  716
C    +CUTHK, RMN2=SCBP_RMIN2 RMX2=SCBP_RMIN2+CUTHK <===                   716
         CALL AGSRESET                                                    716
         AG_DZ = SCBP_LEN                                                 716
         AG_RMN1 = SCBP_RMIN1                                             716
         AG_RMX1 = SCBP_RMIN1+CUTHK                                       716
         AG_RMN2 = SCBP_RMIN2                                             716
         AG_RMX2 = SCBP_RMIN2+CUTHK                                       716
         AG_TITLE = 'CONE'                                                716
         AG_EXNAME = 'SHAPE'                                              716
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            716
         CALL AXSHAPE                                                     716
C    *                                                                    716
5791  CALL AGSPOPB                                                        717
      GO TO BLOCK_SCBL, (5790 ,5800)                                      717
C End of Local Procedure                                                  717
C ----------------------------------------------------------------------  718
C ------------ SFED                                                       718
C Begin Local Procedure SFED                                              719
C SFED IS THE WATRER IN THE BUNDLE OF PIPES GOING TO MANIFOLDS            719
5820  CONTINUE                                                            719
         AG_TITLE='SFED'                                                  719
         AG_MODULE=MODULE                                                 719
         AG_MARK='V00'                                                    719
         CALL AGSPUSH                                                     719
         AG_MODULE=MODULE                                                 719
C    *                                                                    719
C    Geant MATERIAL : WATER ===> inherited arguments used <===            720
         CALL AGSRESET                                                    720
         AG_TITLE = 'WATER'                                               720
         AG_EXNAME = 'MATERIAL'                                           720
         AG_PARLIST = 'NONE'                                              720
         CALL AXMATERIAL                                                  720
C    *                                                                    720
C    Geant ATTRIBUTE : SFED ===> SEEN=1 COLO=1 <===                       720
         CALL AGSRESET                                                    721
         AG_SEEN = 1                                                      721
         AG_COLO = 1                                                      721
         AG_TITLE = 'SFED'                                                721
         AG_EXNAME = 'ATTRIBUTE'                                          721
         AG_PARLIST = 'SEEN_COLO'                                         721
         CALL AXATTRIBUTE                                                 721
C    *                                                                    721
         SQ = SFEP_LEN*SFEP_LEN                                           721
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  723
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  724
         A = PI                                                           725
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            726
         A=A*SQRT(1 + SQ)                                                 727
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            728
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1 RMX1=SFEP_RMIN1  730
C    +CUTHK, RMN2=SFEP_RMIN2 RMX2=SFEP_RMIN2+CUTHK <===                   730
         CALL AGSRESET                                                    730
         AG_DZ = SFEP_LEN                                                 730
         AG_RMN1 = SFEP_RMIN1                                             730
         AG_RMX1 = SFEP_RMIN1+CUTHK                                       730
         AG_RMN2 = SFEP_RMIN2                                             730
         AG_RMX2 = SFEP_RMIN2+CUTHK                                       730
         AG_TITLE = 'CONE'                                                730
         AG_EXNAME = 'SHAPE'                                              730
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            730
         CALL AXSHAPE                                                     730
C    *                                                                    730
5821  CALL AGSPOPB                                                        731
      GO TO BLOCK_SFED, (5820 ,5830)                                      731
C End of Local Procedure                                                  731
C ----------------------------------------------------------------------  732
C ------------ SPLS                                                       732
C Begin Local Procedure SPLS                                              733
C SPLS IS THE PLASTIC WALLS OF THE BUNDLE OF PIPES GOING TO MANIFOLDS     733
5840  CONTINUE                                                            733
         AG_TITLE='SPLS'                                                  733
         AG_MODULE=MODULE                                                 733
         AG_MARK='V00'                                                    733
         CALL AGSPUSH                                                     733
         AG_MODULE=MODULE                                                 733
C    *                                                                    733
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           734
         CALL AGSRESET                                                    734
         AG_A = 12                                                        734
         AG_Z = 6                                                         734
         AG_W = 1                                                         734
         AG_TITLE = 'C'                                                   734
         AG_EXNAME = 'COMPONENT'                                          734
         AG_PARLIST = 'A_Z_W'                                             734
         CALL AXCOMPONENT                                                 734
C    *                                                                    734
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           734
         CALL AGSRESET                                                    735
         AG_A = 1                                                         735
         AG_Z = 1                                                         735
         AG_W = 2                                                         735
         AG_TITLE = 'H2'                                                  735
         AG_EXNAME = 'COMPONENT'                                          735
         AG_PARLIST = 'A_Z_W'                                             735
         CALL AXCOMPONENT                                                 735
C    *                                                                    735
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             735
         CALL AGSRESET                                                    736
         AG_DENS = 0.935                                                  736
         AG_TITLE = 'CH2'                                                 736
         AG_EXNAME = 'MIXTURE'                                            736
         AG_PARLIST = 'DENS'                                              736
         CALL AXMIXTURE                                                   736
C    *                                                                    736
C    Geant ATTRIBUTE : SPLS ===> SEEN=1 COLO=6 <===                       736
         CALL AGSRESET                                                    737
         AG_SEEN = 1                                                      737
         AG_COLO = 6                                                      737
         AG_TITLE = 'SPLS'                                                737
         AG_EXNAME = 'ATTRIBUTE'                                          737
         AG_PARLIST = 'SEEN_COLO'                                         737
         CALL AXATTRIBUTE                                                 737
C    *                                                                    737
         SQ = SFEP_LEN*SFEP_LEN                                           737
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  739
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  740
         A = PI                                                           741
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            742
         A=A*SQRT(1 + SQ)                                                 743
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            744
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1+0.2, RMX1=SFEP_  747
C    RMIN1+0.2+CUTHK RMN2=SFEP_RMIN2+0.2, RMX2=SFEP_RMIN2+0.2+CUTHK <===  747
         CALL AGSRESET                                                    747
         AG_DZ = SFEP_LEN                                                 747
         AG_RMN1 = SFEP_RMIN1+0.2                                         747
         AG_RMX1 = SFEP_RMIN1+0.2+CUTHK                                   747
         AG_RMN2 = SFEP_RMIN2+0.2                                         747
         AG_RMX2 = SFEP_RMIN2+0.2+CUTHK                                   747
         AG_TITLE = 'CONE'                                                747
         AG_EXNAME = 'SHAPE'                                              747
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            747
         CALL AXSHAPE                                                     747
C    *                                                                    747
5841  CALL AGSPOPB                                                        748
      GO TO BLOCK_SPLS, (5840 ,5850)                                      748
C End of Local Procedure                                                  748
C ----------------------------------------------------------------------  749
C ------------ SELE                                                       749
C Begin Local Procedure SELE                                              750
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   750
5590  CONTINUE                                                            750
         AG_TITLE='SELE'                                                  750
         AG_MODULE=MODULE                                                 750
         AG_MARK='V00'                                                    750
         CALL AGSPUSH                                                     750
         AG_MODULE=MODULE                                                 750
C    *                                                                    750
C    Geant MATERIAL : AIR ===> inherited arguments used <===              751
         CALL AGSRESET                                                    751
         AG_TITLE = 'AIR'                                                 751
         AG_EXNAME = 'MATERIAL'                                           751
         AG_PARLIST = 'NONE'                                              751
         CALL AXMATERIAL                                                  751
C    *                                                                    751
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 SERIAL=ILAYER <===         751
         CALL AGSRESET                                                    752
         AG_SEEN = 0                                                      752
         AG_COLO = 1                                                      752
         AG_SERIAL = ILAYER                                               752
         AG_TITLE = 'SELE'                                                752
         AG_EXNAME = 'ATTRIBUTE'                                          752
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  752
         CALL AXATTRIBUTE                                                 752
C    *                                                                    752
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  752
C    K/2 <===                                                             752
         CALL AGSRESET                                                    754
         AG_DX = SELC_ELCAWID/2                                           754
         AG_DY = SWCA_LENGTH/2                                            754
         AG_DZ = ELETHK/2                                                 754
         AG_TITLE = 'BOX'                                                 754
         AG_EXNAME = 'SHAPE'                                              754
         AG_PARLIST = 'DX_DY_DZ'                                          754
         CALL AXSHAPE                                                     754
C    *                                                                    754
C    Create SWCH :                                                        755
C    Call To Local Procedure SWCH                                         755
         ASSIGN 5870 TO BLOCK_SWCH                                        755
         GO TO  5860                                                      755
5870     CONTINUE                                                         755
C    *                                                                    755
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             756
         CALL AGSRESET                                                    756
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    756
         AG_TITLE = 'SWCH'                                                756
         AG_EXNAME = 'POSITION'                                           756
         AG_PARLIST = 'Z'                                                 756
         CALL AXPOSITION                                                  756
C    *                                                                    756
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  756
C    BETHK/2 <===                                                         756
         CALL AGSRESET                                                    757
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             757
         AG_TITLE = 'SWCH'                                                757
         AG_EXNAME = 'POSITION'                                           757
         AG_PARLIST = 'Z'                                                 757
         CALL AXPOSITION                                                  757
C    *                                                                    757
C    Create SWCS :                                                        758
C    Call To Local Procedure SWCS                                         758
         ASSIGN 5890 TO BLOCK_SWCS                                        758
         GO TO  5880                                                      758
5890     CONTINUE                                                         758
C    *                                                                    758
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  760
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     760
         CALL AGSRESET                                                    760
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              760
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        760
         AG_TITLE = 'SWCS'                                                760
         AG_EXNAME = 'POSITION'                                           760
         AG_PARLIST = 'X_Z'                                               760
         CALL AXPOSITION                                                  760
C    *                                                                    760
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  760
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     760
         CALL AGSRESET                                                    762
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              762
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        762
         AG_TITLE = 'SWCS'                                                762
         AG_EXNAME = 'POSITION'                                           762
         AG_PARLIST = 'X_Z'                                               762
         CALL AXPOSITION                                                  762
C    *                                                                    762
C    Create SWCW :                                                        763
C    Call To Local Procedure SWCW                                         763
         ASSIGN 5910 TO BLOCK_SWCW                                        763
         GO TO  5900                                                      763
5910     CONTINUE                                                         763
C    *                                                                    763
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  764
C    =                                                                    764
         CALL AGSRESET                                                    764
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        764
         AG_TITLE = 'SWCW'                                                764
         AG_EXNAME = 'POSITION'                                           764
         AG_PARLIST = 'Z'                                                 764
         CALL AXPOSITION                                                  764
C    *                                                                    764
C    Create SBOI :                                                        765
C    Call To Local Procedure SBOI                                         765
         ASSIGN 5930 TO BLOCK_SBOI                                        765
         GO TO  5920                                                      765
5930     CONTINUE                                                         765
C    *                                                                    765
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  766
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       766
         CALL AGSRESET                                                    766
         AG_Z = (ELETHK/                                                  766
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          766
         AG_TITLE = 'SBOI'                                                766
         AG_EXNAME = 'POSITION'                                           766
         AG_PARLIST = 'Z'                                                 766
         CALL AXPOSITION                                                  766
C    *                                                                    766
C    Create SGLA :                                                        767
C    Call To Local Procedure SGLA                                         767
         ASSIGN 5950 TO BLOCK_SGLA                                        767
         GO TO  5940                                                      767
5950     CONTINUE                                                         767
C    *                                                                    767
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  768
C    _GLASSTHK/2 <===                                                     768
         CALL AGSRESET                                                    768
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         768
         AG_TITLE = 'SGLA'                                                768
         AG_EXNAME = 'POSITION'                                           768
         AG_PARLIST = 'Z'                                                 768
         CALL AXPOSITION                                                  768
C    *                                                                    768
C    Create SAGP :                                                        769
C    Call To Local Procedure SAGP                                         769
         ASSIGN 5970 TO BLOCK_SAGP                                        769
         GO TO  5960                                                      769
5970     CONTINUE                                                         769
C    *                                                                    769
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  770
C    ==                                                                   770
         CALL AGSRESET                                                    770
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       770
         AG_TITLE = 'SAGP'                                                770
         AG_EXNAME = 'POSITION'                                           770
         AG_PARLIST = 'Z'                                                 770
         CALL AXPOSITION                                                  770
C    *                                                                    770
C    Create SDYE :                                                        771
C    Call To Local Procedure SDYE                                         771
         ASSIGN 5990 TO BLOCK_SDYE                                        771
         GO TO  5980                                                      771
5990     CONTINUE                                                         771
C    *                                                                    771
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  773
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     773
         CALL AGSRESET                                                    773
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  773
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    773
         AG_TITLE = 'SDYE'                                                773
         AG_EXNAME = 'POSITION'                                           773
         AG_PARLIST = 'X_Z'                                               773
         CALL AXPOSITION                                                  773
C    *                                                                    773
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  773
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             773
         CALL AGSRESET                                                    775
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          775
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    775
         AG_TITLE = 'SDYE'                                                775
         AG_EXNAME = 'POSITION'                                           775
         AG_PARLIST = 'X_Z'                                               775
         CALL AXPOSITION                                                  775
C    *                                                                    775
C    Create SECA :                                                        776
C    Call To Local Procedure SECA                                         776
         ASSIGN 6010 TO BLOCK_SECA                                        776
         GO TO  6000                                                      776
6010     CONTINUE                                                         776
C    *                                                                    776
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  778
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    778
         CALL AGSRESET                                                    778
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             778
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        778
         AG_TITLE = 'SECA'                                                778
         AG_EXNAME = 'POSITION'                                           778
         AG_PARLIST = 'X_Z'                                               778
         CALL AXPOSITION                                                  778
C    *                                                                    778
5591  CALL AGSPOPB                                                        779
      GO TO BLOCK_SELE, (5590 ,5600)                                      779
C End of Local Procedure                                                  779
C ----------------------------------------------------------------------  780
C ------------ SDYE                                                       780
C Begin Local Procedure SDYE                                              781
C SDYE IS THE IC CHIP ON THE HYBRID                                       781
5980  CONTINUE                                                            781
         AG_TITLE='SDYE'                                                  781
         AG_MODULE=MODULE                                                 781
         AG_MARK='V00'                                                    781
         CALL AGSPUSH                                                     781
         AG_MODULE=MODULE                                                 781
C    *                                                                    781
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          782
         CALL AGSRESET                                                    782
         AG_TITLE = 'SILICON'                                             782
         AG_EXNAME = 'MATERIAL'                                           782
         AG_PARLIST = 'NONE'                                              782
         CALL AXMATERIAL                                                  782
C    *                                                                    782
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 SERIAL=0 <===              782
         CALL AGSRESET                                                    783
         AG_SEEN = 1                                                      783
         AG_COLO = 6                                                      783
         AG_SERIAL = 0                                                    783
         AG_TITLE = 'SDYE'                                                783
         AG_EXNAME = 'ATTRIBUTE'                                          783
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  783
         CALL AXATTRIBUTE                                                 783
C    *                                                                    783
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  783
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           783
         CALL AGSRESET                                                    785
         AG_DX = SELC_DYEWID/2                                            785
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              785
         AG_DZ = SELC_DYETHK/2                                            785
         AG_TITLE = 'BOX'                                                 785
         AG_EXNAME = 'SHAPE'                                              785
         AG_PARLIST = 'DX_DY_DZ'                                          785
         CALL AXSHAPE                                                     785
C    *                                                                    785
5981  CALL AGSPOPB                                                        786
      GO TO BLOCK_SDYE, (5980 ,5990)                                      786
C End of Local Procedure                                                  786
C ----------------------------------------------------------------------  787
C ------------ SECA                                                       787
C Begin Local Procedure SECA                                              788
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            788
6000  CONTINUE                                                            788
         AG_TITLE='SECA'                                                  788
         AG_MODULE=MODULE                                                 788
         AG_MARK='V00'                                                    788
         CALL AGSPUSH                                                     788
         AG_MODULE=MODULE                                                 788
C    *                                                                    788
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           789
         CALL AGSRESET                                                    789
         AG_TITLE = 'COPPER'                                              789
         AG_EXNAME = 'MATERIAL'                                           789
         AG_PARLIST = 'NONE'                                              789
         CALL AXMATERIAL                                                  789
C    *                                                                    789
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 SERIAL=0 <===              789
         CALL AGSRESET                                                    790
         AG_SEEN = 1                                                      790
         AG_COLO = 2                                                      790
         AG_SERIAL = 0                                                    790
         AG_TITLE = 'SECA'                                                790
         AG_EXNAME = 'ATTRIBUTE'                                          790
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  790
         CALL AXATTRIBUTE                                                 790
C    *                                                                    790
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  790
C    ABTHK/2 <===                                                         790
         CALL AGSRESET                                                    792
         AG_DX = SELC_CABWID/2                                            792
         AG_DY = SWCA_LENGTH/2                                            792
         AG_DZ = SELC_CABTHK/2                                            792
         AG_TITLE = 'BOX'                                                 792
         AG_EXNAME = 'SHAPE'                                              792
         AG_PARLIST = 'DX_DY_DZ'                                          792
         CALL AXSHAPE                                                     792
C    *                                                                    792
6001  CALL AGSPOPB                                                        793
      GO TO BLOCK_SECA, (6000 ,6010)                                      793
C End of Local Procedure                                                  793
C ----------------------------------------------------------------------  794
C ------------ SBOI                                                       794
C Begin Local Procedure SBOI                                              795
C SBOI IS THE BERILLIA LAYER                                              795
5920  CONTINUE                                                            795
         AG_TITLE='SBOI'                                                  795
         AG_MODULE=MODULE                                                 795
         AG_MARK='V00'                                                    795
         CALL AGSPUSH                                                     795
         AG_MODULE=MODULE                                                 795
C    *                                                                    795
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1 <===                           796
         CALL AGSRESET                                                    796
         AG_A = 9                                                         796
         AG_Z = 4                                                         796
         AG_W = 1                                                         796
         AG_TITLE = 'BE'                                                  796
         AG_EXNAME = 'COMPONENT'                                          796
         AG_PARLIST = 'A_Z_W'                                             796
         CALL AXCOMPONENT                                                 796
C    *                                                                    796
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           796
         CALL AGSRESET                                                    797
         AG_A = 16                                                        797
         AG_Z = 8                                                         797
         AG_W = 1                                                         797
         AG_TITLE = 'O'                                                   797
         AG_EXNAME = 'COMPONENT'                                          797
         AG_PARLIST = 'A_Z_W'                                             797
         CALL AXCOMPONENT                                                 797
C    *                                                                    797
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              797
         CALL AGSRESET                                                    798
         AG_DENS = 2.85                                                   798
         AG_TITLE = 'BEO'                                                 798
         AG_EXNAME = 'MIXTURE'                                            798
         AG_PARLIST = 'DENS'                                              798
         CALL AXMIXTURE                                                   798
C    *                                                                    798
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 SERIAL=0 <===              798
         CALL AGSRESET                                                    799
         AG_SEEN = 1                                                      799
         AG_COLO = 6                                                      799
         AG_SERIAL = 0                                                    799
         AG_TITLE = 'SBOI'                                                799
         AG_EXNAME = 'ATTRIBUTE'                                          799
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  799
         CALL AXATTRIBUTE                                                 799
C    *                                                                    799
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  799
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          799
         CALL AGSRESET                                                    801
         AG_DX = SELC_ELCAWID/2                                           801
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              801
         AG_DZ = SELC_BEOTHK/2                                            801
         AG_TITLE = 'BOX'                                                 801
         AG_EXNAME = 'SHAPE'                                              801
         AG_PARLIST = 'DX_DY_DZ'                                          801
         CALL AXSHAPE                                                     801
C    *                                                                    801
5921  CALL AGSPOPB                                                        802
      GO TO BLOCK_SBOI, (5920 ,5930)                                      802
C End of Local Procedure                                                  802
C ----------------------------------------------------------------------  803
C ------------ SAGP                                                       803
C Begin Local Procedure SAGP                                              804
C SAGP IS THE SILVER-PALLADIUM LAYER                                      804
5960  CONTINUE                                                            804
         AG_TITLE='SAGP'                                                  804
         AG_MODULE=MODULE                                                 804
         AG_MARK='V00'                                                    804
         CALL AGSPUSH                                                     804
         AG_MODULE=MODULE                                                 804
C    *                                                                    804
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1 <===                        805
         CALL AGSRESET                                                    805
         AG_A = 108                                                       805
         AG_Z = 47                                                        805
         AG_W = 1                                                         805
         AG_TITLE = 'AG'                                                  805
         AG_EXNAME = 'COMPONENT'                                          805
         AG_PARLIST = 'A_Z_W'                                             805
         CALL AXCOMPONENT                                                 805
C    *                                                                    805
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1 <===                        805
         CALL AGSRESET                                                    806
         AG_A = 106                                                       806
         AG_Z = 46                                                        806
         AG_W = 1                                                         806
         AG_TITLE = 'PD'                                                  806
         AG_EXNAME = 'COMPONENT'                                          806
         AG_PARLIST = 'A_Z_W'                                             806
         CALL AXCOMPONENT                                                 806
C    *                                                                    806
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            806
         CALL AGSRESET                                                    807
         AG_DENS = 11.25                                                  807
         AG_TITLE = 'AGPD'                                                807
         AG_EXNAME = 'MIXTURE'                                            807
         AG_PARLIST = 'DENS'                                              807
         CALL AXMIXTURE                                                   807
C    *                                                                    807
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 SERIAL=0 <===              807
         CALL AGSRESET                                                    808
         AG_SEEN = 1                                                      808
         AG_COLO = 2                                                      808
         AG_SERIAL = 0                                                    808
         AG_TITLE = 'SAGP'                                                808
         AG_EXNAME = 'ATTRIBUTE'                                          808
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  808
         CALL AXATTRIBUTE                                                 808
C    *                                                                    808
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  808
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         808
         CALL AGSRESET                                                    810
         AG_DX = SELC_ELCAWID/2                                           810
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              810
         AG_DZ = SELC_AGPDTHK/2                                           810
         AG_TITLE = 'BOX'                                                 810
         AG_EXNAME = 'SHAPE'                                              810
         AG_PARLIST = 'DX_DY_DZ'                                          810
         CALL AXSHAPE                                                     810
C    *                                                                    810
5961  CALL AGSPOPB                                                        811
      GO TO BLOCK_SAGP, (5960 ,5970)                                      811
C End of Local Procedure                                                  811
C ----------------------------------------------------------------------  812
C ------------ SGLA                                                       812
C Begin Local Procedure SGLA                                              813
C SGLA IS THE INSULATING GLASS LAYER                                      813
5940  CONTINUE                                                            813
         AG_TITLE='SGLA'                                                  813
         AG_MODULE=MODULE                                                 813
         AG_MARK='V00'                                                    813
         CALL AGSPUSH                                                     813
         AG_MODULE=MODULE                                                 813
C    *                                                                    813
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         814
         CALL AGSRESET                                                    814
         AG_A = 28                                                        814
         AG_Z = 14                                                        814
         AG_W = 1                                                         814
         AG_TITLE = 'SI'                                                  814
         AG_EXNAME = 'COMPONENT'                                          814
         AG_PARLIST = 'A_Z_W'                                             814
         CALL AXCOMPONENT                                                 814
C    *                                                                    814
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           814
         CALL AGSRESET                                                    815
         AG_A = 16                                                        815
         AG_Z = 8                                                         815
         AG_W = 2                                                         815
         AG_TITLE = 'O'                                                   815
         AG_EXNAME = 'COMPONENT'                                          815
         AG_PARLIST = 'A_Z_W'                                             815
         CALL AXCOMPONENT                                                 815
C    *                                                                    815
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             815
         CALL AGSRESET                                                    816
         AG_DENS = 2.2                                                    816
         AG_TITLE = 'GLASS'                                               816
         AG_EXNAME = 'MIXTURE'                                            816
         AG_PARLIST = 'DENS'                                              816
         CALL AXMIXTURE                                                   816
C    *                                                                    816
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 SERIAL=0 <===              816
         CALL AGSRESET                                                    817
         AG_SEEN = 1                                                      817
         AG_COLO = 6                                                      817
         AG_SERIAL = 0                                                    817
         AG_TITLE = 'SGLA'                                                817
         AG_EXNAME = 'ATTRIBUTE'                                          817
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  817
         CALL AXATTRIBUTE                                                 817
C    *                                                                    817
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  817
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        817
         CALL AGSRESET                                                    819
         AG_DX = SELC_ELCAWID/2                                           819
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              819
         AG_DZ = SELC_GLASSTHK/2                                          819
         AG_TITLE = 'BOX'                                                 819
         AG_EXNAME = 'SHAPE'                                              819
         AG_PARLIST = 'DX_DY_DZ'                                          819
         CALL AXSHAPE                                                     819
C    *                                                                    819
5941  CALL AGSPOPB                                                        820
      GO TO BLOCK_SGLA, (5940 ,5950)                                      820
C End of Local Procedure                                                  820
C ----------------------------------------------------------------------  821
C ------------ SWCH                                                       821
C Begin Local Procedure SWCH                                              822
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      822
5860  CONTINUE                                                            822
         AG_TITLE='SWCH'                                                  822
         AG_MODULE=MODULE                                                 822
         AG_MARK='V00'                                                    822
         CALL AGSPUSH                                                     822
         AG_MODULE=MODULE                                                 822
C    *                                                                    822
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        823
         CALL AGSRESET                                                    823
         AG_TITLE = 'BERILLIUM'                                           823
         AG_EXNAME = 'MATERIAL'                                           823
         AG_PARLIST = 'NONE'                                              823
         CALL AXMATERIAL                                                  823
C    *                                                                    823
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 SERIAL=0 <===              823
         CALL AGSRESET                                                    824
         AG_SEEN = 1                                                      824
         AG_COLO = 2                                                      824
         AG_SERIAL = 0                                                    824
         AG_TITLE = 'SWCH'                                                824
         AG_EXNAME = 'ATTRIBUTE'                                          824
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  824
         CALL AXATTRIBUTE                                                 824
C    *                                                                    824
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  824
C    BETHK/2 <===                                                         824
         CALL AGSRESET                                                    826
         AG_DX = SELC_ELCAWID/2                                           826
         AG_DY = SWCA_LENGTH/2                                            826
         AG_DZ = SELC_BETHK/2                                             826
         AG_TITLE = 'BOX'                                                 826
         AG_EXNAME = 'SHAPE'                                              826
         AG_PARLIST = 'DX_DY_DZ'                                          826
         CALL AXSHAPE                                                     826
C    *                                                                    826
5861  CALL AGSPOPB                                                        827
      GO TO BLOCK_SWCH, (5860 ,5870)                                      827
C End of Local Procedure                                                  827
C ----------------------------------------------------------------------  828
C ------------ SWCS                                                       828
C Begin Local Procedure SWCS                                              829
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                829
5880  CONTINUE                                                            829
         AG_TITLE='SWCS'                                                  829
         AG_MODULE=MODULE                                                 829
         AG_MARK='V00'                                                    829
         CALL AGSPUSH                                                     829
         AG_MODULE=MODULE                                                 829
C    *                                                                    829
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        830
         CALL AGSRESET                                                    830
         AG_TITLE = 'BERILLIUM'                                           830
         AG_EXNAME = 'MATERIAL'                                           830
         AG_PARLIST = 'NONE'                                              830
         CALL AXMATERIAL                                                  830
C    *                                                                    830
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 SERIAL=0 <===              830
         CALL AGSRESET                                                    831
         AG_SEEN = 1                                                      831
         AG_COLO = 2                                                      831
         AG_SERIAL = 0                                                    831
         AG_TITLE = 'SWCS'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  831
C    TTHK/2 <===                                                          831
         CALL AGSRESET                                                    833
         AG_DX = SELC_BETHK/2                                             833
         AG_DY = SWCA_LENGTH/2                                            833
         AG_DZ = SELC_WATTHK/2                                            833
         AG_TITLE = 'BOX'                                                 833
         AG_EXNAME = 'SHAPE'                                              833
         AG_PARLIST = 'DX_DY_DZ'                                          833
         CALL AXSHAPE                                                     833
C    *                                                                    833
5881  CALL AGSPOPB                                                        834
      GO TO BLOCK_SWCS, (5880 ,5890)                                      834
C End of Local Procedure                                                  834
C ----------------------------------------------------------------------  835
C ------------ SWCW                                                       835
C Begin Local Procedure SWCW                                              836
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       836
5900  CONTINUE                                                            836
         AG_TITLE='SWCW'                                                  836
         AG_MODULE=MODULE                                                 836
         AG_MARK='V00'                                                    836
         CALL AGSPUSH                                                     836
         AG_MODULE=MODULE                                                 836
C    *                                                                    836
C    Geant MATERIAL : WATER ===> inherited arguments used <===            837
         CALL AGSRESET                                                    837
         AG_TITLE = 'WATER'                                               837
         AG_EXNAME = 'MATERIAL'                                           837
         AG_PARLIST = 'NONE'                                              837
         CALL AXMATERIAL                                                  837
C    *                                                                    837
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 SERIAL=0 <===              837
         CALL AGSRESET                                                    838
         AG_SEEN = 1                                                      838
         AG_COLO = 6                                                      838
         AG_SERIAL = 0                                                    838
         AG_TITLE = 'SWCW'                                                838
         AG_EXNAME = 'ATTRIBUTE'                                          838
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  838
         CALL AXATTRIBUTE                                                 838
C    *                                                                    838
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  838
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     838
         CALL AGSRESET                                                    840
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          840
         AG_DY = SWCA_LENGTH/2                                            840
         AG_DZ = (SELC_WATTHK/2)                                          840
         AG_TITLE = 'BOX'                                                 840
         AG_EXNAME = 'SHAPE'                                              840
         AG_PARLIST = 'DX_DY_DZ'                                          840
         CALL AXSHAPE                                                     840
C    *                                                                    840
5901  CALL AGSPOPB                                                        841
      GO TO BLOCK_SWCW, (5900 ,5910)                                      841
C End of Local Procedure                                                  841
C ----------------------------------------------------------------------  842
C ------------ SIRT                                                       842
C Begin Local Procedure SIRT                                              843
C SIRT IS THE SVT INNER END RING TUBE                                     843
5070  CONTINUE                                                            843
         AG_TITLE='SIRT'                                                  843
         AG_MODULE=MODULE                                                 843
         AG_MARK='V00'                                                    843
         CALL AGSPUSH                                                     843
         AG_MODULE=MODULE                                                 843
C    *                                                                    843
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        844
         CALL AGSRESET                                                    844
         AG_TITLE = 'BERILLIUM'                                           844
         AG_EXNAME = 'MATERIAL'                                           844
         AG_PARLIST = 'NONE'                                              844
         CALL AXMATERIAL                                                  844
C    *                                                                    844
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       844
         CALL AGSRESET                                                    845
         AG_SEEN = 1                                                      845
         AG_COLO = 2                                                      845
         AG_TITLE = 'SIRT'                                                845
         AG_EXNAME = 'ATTRIBUTE'                                          845
         AG_PARLIST = 'SEEN_COLO'                                         845
         CALL AXATTRIBUTE                                                 845
C    *                                                                    845
         IR_RMIN = COS(PI/8.0)                                            845
         IR_RMIN=IR_RMIN+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/           847
     *   8.0)*SIN(PI/8.0))                                                847
         IR_RMIN=IR_RMIN*SERG_IRNGPRMN                                    848
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  850
C    DRNGTH/2 <===                                                        850
         CALL AGSRESET                                                    850
         AG_RMIN = IR_RMIN                                                850
         AG_RMAX = SERG_IRNGTRMX                                          850
         AG_DZ = SERG_ENDRNGTH/2                                          850
         AG_TITLE = 'TUBE'                                                850
         AG_EXNAME = 'SHAPE'                                              850
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      850
         CALL AXSHAPE                                                     850
C    *                                                                    850
5071  CALL AGSPOPB                                                        851
      GO TO BLOCK_SIRT, (5070 ,5080)                                      851
C End of Local Procedure                                                  851
C ----------------------------------------------------------------------  852
C ------------ SIRP                                                       852
C Begin Local Procedure SIRP                                              853
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 853
5050  CONTINUE                                                            853
         AG_TITLE='SIRP'                                                  853
         AG_MODULE=MODULE                                                 853
         AG_MARK='V00'                                                    853
         CALL AGSPUSH                                                     853
         AG_MODULE=MODULE                                                 853
C    *                                                                    853
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        854
         CALL AGSRESET                                                    854
         AG_TITLE = 'BERILLIUM'                                           854
         AG_EXNAME = 'MATERIAL'                                           854
         AG_PARLIST = 'NONE'                                              854
         CALL AXMATERIAL                                                  854
C    *                                                                    854
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       854
         CALL AGSRESET                                                    855
         AG_SEEN = 1                                                      855
         AG_COLO = 2                                                      855
         AG_TITLE = 'SIRP'                                                855
         AG_EXNAME = 'ATTRIBUTE'                                          855
         AG_PARLIST = 'SEEN_COLO'                                         855
         CALL AXATTRIBUTE                                                 855
C    *                                                                    855
         ROU = COS(PI/8.0)                                                855
         ROU=ROU+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/8.0)*SIN(PI/       857
     *   8.0))                                                            857
         ROU = SERG_IRNGTRMX/ROU                                          858
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  861
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  861
C    OU,ROU} <===                                                         861
         CALL AGSRESET                                                    861
         AG_PHI1 = 0                                                      861
         AG_DPHI = 360                                                    861
         AG_NPDIV = 8                                                     861
         AG_NZ = 2                                                        861
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        861
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        861
         AG_RMN(1)=SERG_IRNGPRMN                                          861
         AG_RMN(2)=SERG_IRNGPRMN                                          861
         AG_RMX(1)=ROU                                                    861
         AG_RMX(2)=ROU                                                    861
         AG_TITLE = 'PGON'                                                861
         AG_EXNAME = 'SHAPE'                                              861
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     861
         CALL AXSHAPE                                                     861
C    *                                                                    861
5051  CALL AGSPOPB                                                        862
      GO TO BLOCK_SIRP, (5050 ,5060)                                      862
C End of Local Procedure                                                  862
C ----------------------------------------------------------------------  863
C ------------ SOER                                                       863
C Begin Local Procedure SOER                                              864
C SOER IS THE SVT OUTER END RING                                          864
5090  CONTINUE                                                            864
         AG_TITLE='SOER'                                                  864
         AG_MODULE=MODULE                                                 864
         AG_MARK='V00'                                                    864
         CALL AGSPUSH                                                     864
         AG_MODULE=MODULE                                                 864
C    *                                                                    864
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        865
         CALL AGSRESET                                                    865
         AG_TITLE = 'BERILLIUM'                                           865
         AG_EXNAME = 'MATERIAL'                                           865
         AG_PARLIST = 'NONE'                                              865
         CALL AXMATERIAL                                                  865
C    *                                                                    865
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       865
         CALL AGSRESET                                                    866
         AG_SEEN = 1                                                      866
         AG_COLO = 2                                                      866
         AG_TITLE = 'SOER'                                                866
         AG_EXNAME = 'ATTRIBUTE'                                          866
         AG_PARLIST = 'SEEN_COLO'                                         866
         CALL AXATTRIBUTE                                                 866
C    *                                                                    866
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  866
C    ERG_ENDRNGTH/2 <===                                                  866
         CALL AGSRESET                                                    868
         AG_RMIN = SERG_ORNGRMIN                                          868
         AG_RMAX = SERG_ORNGRMAX                                          868
         AG_DZ = SERG_ENDRNGTH/2                                          868
         AG_TITLE = 'TUBE'                                                868
         AG_EXNAME = 'SHAPE'                                              868
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      868
         CALL AXSHAPE                                                     868
C    *                                                                    868
5091  CALL AGSPOPB                                                        869
      GO TO BLOCK_SOER, (5090 ,5100)                                      869
C End of Local Procedure                                                  869
C ----------------------------------------------------------------------  870
C ------------ SIES                                                       870
C Begin Local Procedure SIES                                              871
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         871
5150  CONTINUE                                                            871
         AG_TITLE='SIES'                                                  871
         AG_MODULE=MODULE                                                 871
         AG_MARK='V00'                                                    871
         CALL AGSPUSH                                                     871
         AG_MODULE=MODULE                                                 871
C    *                                                                    871
C    Geant MATERIAL : AIR ===> inherited arguments used <===              872
         CALL AGSRESET                                                    872
         AG_TITLE = 'AIR'                                                 872
         AG_EXNAME = 'MATERIAL'                                           872
         AG_PARLIST = 'NONE'                                              872
         CALL AXMATERIAL                                                  872
C    *                                                                    872
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       872
         CALL AGSRESET                                                    873
         AG_SEEN = 0                                                      873
         AG_COLO = 1                                                      873
         AG_TITLE = 'SIES'                                                873
         AG_EXNAME = 'ATTRIBUTE'                                          873
         AG_PARLIST = 'SEEN_COLO'                                         873
         CALL AXATTRIBUTE                                                 873
C    *                                                                    873
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  873
C    ACK_Z-ENDRNG_Z) <===                                                 873
         CALL AGSRESET                                                    875
         AG_RMIN = IR_RMIN                                                875
         AG_RMAX = SERG_IRNGTRMX                                          875
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   875
         AG_TITLE = 'TUBE'                                                875
         AG_EXNAME = 'SHAPE'                                              875
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      875
         CALL AXSHAPE                                                     875
C    *                                                                    875
C    Create SISM :                                                        876
C    Call To Local Procedure SISM                                         876
         ASSIGN 6030 TO BLOCK_SISM                                        876
         GO TO  6020                                                      876
6030     CONTINUE                                                         876
C    *                                                                    876
5151  CALL AGSPOPB                                                        877
      GO TO BLOCK_SIES, (5150 ,5160)                                      877
C End of Local Procedure                                                  877
C ----------------------------------------------------------------------  878
C ------------ SISM                                                       878
C Begin Local Procedure SISM                                              879
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        879
6020  CONTINUE                                                            879
         AG_TITLE='SISM'                                                  879
         AG_MODULE=MODULE                                                 879
         AG_MARK='V00'                                                    879
         CALL AGSPUSH                                                     879
         AG_MODULE=MODULE                                                 879
C    *                                                                    879
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       880
         CALL AGSRESET                                                    880
         AG_SEEN = 0                                                      880
         AG_COLO = 1                                                      880
         AG_TITLE = 'SISM'                                                880
         AG_EXNAME = 'ATTRIBUTE'                                          880
         AG_PARLIST = 'SEEN_COLO'                                         880
         CALL AXATTRIBUTE                                                 880
C    *                                                                    880
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                880
         CALL AGSRESET                                                    881
         AG_NDIV = 4                                                      881
         AG_IAXIS = 2                                                     881
         AG_C0 = 45                                                       881
         AG_TITLE = 'DIVISION'                                            881
         AG_EXNAME = 'SHAPE'                                              881
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     881
         CALL AXSHAPE                                                     881
C    *                                                                    881
C    Create SCRW :                                                        882
C    Call To Local Procedure SCRW                                         882
         ASSIGN 6050 TO BLOCK_SCRW                                        882
         GO TO  6040                                                      882
6050     CONTINUE                                                         882
C    *                                                                    882
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  883
C    ===                                                                  883
         CALL AGSRESET                                                    883
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      883
         AG_TITLE = 'SCRW'                                                883
         AG_EXNAME = 'POSITION'                                           883
         AG_PARLIST = 'X'                                                 883
         CALL AXPOSITION                                                  883
C    *                                                                    883
6021  CALL AGSPOPB                                                        884
      GO TO BLOCK_SISM, (6020 ,6030)                                      884
C End of Local Procedure                                                  884
C ----------------------------------------------------------------------  885
C ------------ SOES                                                       885
C Begin Local Procedure SOES                                              886
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         886
5130  CONTINUE                                                            886
         AG_TITLE='SOES'                                                  886
         AG_MODULE=MODULE                                                 886
         AG_MARK='V00'                                                    886
         CALL AGSPUSH                                                     886
         AG_MODULE=MODULE                                                 886
C    *                                                                    886
C    Geant MATERIAL : AIR ===> inherited arguments used <===              887
         CALL AGSRESET                                                    887
         AG_TITLE = 'AIR'                                                 887
         AG_EXNAME = 'MATERIAL'                                           887
         AG_PARLIST = 'NONE'                                              887
         CALL AXMATERIAL                                                  887
C    *                                                                    887
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       887
         CALL AGSRESET                                                    888
         AG_SEEN = 0                                                      888
         AG_COLO = 1                                                      888
         AG_TITLE = 'SOES'                                                888
         AG_EXNAME = 'ATTRIBUTE'                                          888
         AG_PARLIST = 'SEEN_COLO'                                         888
         CALL AXATTRIBUTE                                                 888
C    *                                                                    888
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  888
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           888
         CALL AGSRESET                                                    890
         AG_RMIN = SERG_ORNGRMIN                                          890
         AG_RMAX = SERG_ORNGRMAX                                          890
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   890
         AG_TITLE = 'TUBE'                                                890
         AG_EXNAME = 'SHAPE'                                              890
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      890
         CALL AXSHAPE                                                     890
C    *                                                                    890
C    Create SOSM :                                                        891
C    Call To Local Procedure SOSM                                         891
         ASSIGN 6070 TO BLOCK_SOSM                                        891
         GO TO  6060                                                      891
6070     CONTINUE                                                         891
C    *                                                                    891
5131  CALL AGSPOPB                                                        892
      GO TO BLOCK_SOES, (5130 ,5140)                                      892
C End of Local Procedure                                                  892
C ----------------------------------------------------------------------  893
C ------------ SOSM                                                       893
C Begin Local Procedure SOSM                                              894
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        894
6060  CONTINUE                                                            894
         AG_TITLE='SOSM'                                                  894
         AG_MODULE=MODULE                                                 894
         AG_MARK='V00'                                                    894
         CALL AGSPUSH                                                     894
         AG_MODULE=MODULE                                                 894
C    *                                                                    894
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       895
         CALL AGSRESET                                                    895
         AG_SEEN = 0                                                      895
         AG_COLO = 1                                                      895
         AG_TITLE = 'SOSM'                                                895
         AG_EXNAME = 'ATTRIBUTE'                                          895
         AG_PARLIST = 'SEEN_COLO'                                         895
         CALL AXATTRIBUTE                                                 895
C    *                                                                    895
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                895
         CALL AGSRESET                                                    896
         AG_NDIV = 4                                                      896
         AG_IAXIS = 2                                                     896
         AG_C0 = 45                                                       896
         AG_TITLE = 'DIVISION'                                            896
         AG_EXNAME = 'SHAPE'                                              896
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     896
         CALL AXSHAPE                                                     896
C    *                                                                    896
C    Create SCRW :                                                        897
C    Call To Local Procedure SCRW                                         897
         ASSIGN 6080 TO BLOCK_SCRW                                        897
         GO TO  6040                                                      897
6080     CONTINUE                                                         897
C    *                                                                    897
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  898
C    ===                                                                  898
         CALL AGSRESET                                                    898
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      898
         AG_TITLE = 'SCRW'                                                898
         AG_EXNAME = 'POSITION'                                           898
         AG_PARLIST = 'X'                                                 898
         CALL AXPOSITION                                                  898
C    *                                                                    898
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  898
C    ===                                                                  898
         CALL AGSRESET                                                    899
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      899
         AG_TITLE = 'SCRW'                                                899
         AG_EXNAME = 'POSITION'                                           899
         AG_PARLIST = 'X'                                                 899
         CALL AXPOSITION                                                  899
C    *                                                                    899
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  899
C    =SSUP_ERJ2Y <===                                                     899
         CALL AGSRESET                                                    900
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      900
         AG_Y = SSUP_ERJ2Y                                                900
         AG_TITLE = 'SCRW'                                                900
         AG_EXNAME = 'POSITION'                                           900
         AG_PARLIST = 'X_Y'                                               900
         CALL AXPOSITION                                                  900
C    *                                                                    900
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  900
C    =-SSUP_ERJ2Y <===                                                    900
         CALL AGSRESET                                                    901
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      901
         AG_Y = -SSUP_ERJ2Y                                               901
         AG_TITLE = 'SCRW'                                                901
         AG_EXNAME = 'POSITION'                                           901
         AG_PARLIST = 'X_Y'                                               901
         CALL AXPOSITION                                                  901
C    *                                                                    901
6061  CALL AGSPOPB                                                        902
      GO TO BLOCK_SOSM, (6060 ,6070)                                      902
C End of Local Procedure                                                  902
C ----------------------------------------------------------------------  903
C ------------ SBRG                                                       903
C Begin Local Procedure SBRG                                              904
C SBRG IS THE BRACKET JOINING THE END RINGS                               904
5110  CONTINUE                                                            904
         AG_TITLE='SBRG'                                                  904
         AG_MODULE=MODULE                                                 904
         AG_MARK='V00'                                                    904
         CALL AGSPUSH                                                     904
         AG_MODULE=MODULE                                                 904
C    *                                                                    904
         RIN=SERG_IRNGPRMN                                                905
         ROU=SERG_ORNGRMAX                                                906
C    Geant MATERIAL : AIR ===> inherited arguments used <===              907
         CALL AGSRESET                                                    907
         AG_TITLE = 'AIR'                                                 907
         AG_EXNAME = 'MATERIAL'                                           907
         AG_PARLIST = 'NONE'                                              907
         CALL AXMATERIAL                                                  907
C    *                                                                    907
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       907
         CALL AGSRESET                                                    908
         AG_SEEN = 0                                                      908
         AG_COLO = 1                                                      908
         AG_TITLE = 'SBRG'                                                908
         AG_EXNAME = 'ATTRIBUTE'                                          908
         AG_PARLIST = 'SEEN_COLO'                                         908
         CALL AXATTRIBUTE                                                 908
C    *                                                                    908
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      908
         CALL AGSRESET                                                    909
         AG_RMIN = RIN                                                    909
         AG_RMAX = ROU                                                    909
         AG_DZ = SSUP_ERJTHK/2                                            909
         AG_TITLE = 'TUBE'                                                909
         AG_EXNAME = 'SHAPE'                                              909
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      909
         CALL AXSHAPE                                                     909
C    *                                                                    909
C    Create SBRM :                                                        910
C    Call To Local Procedure SBRM                                         910
         ASSIGN 6100 TO BLOCK_SBRM                                        910
         GO TO  6090                                                      910
6100     CONTINUE                                                         910
C    *                                                                    910
5111  CALL AGSPOPB                                                        911
      GO TO BLOCK_SBRG, (5110 ,5120)                                      911
C End of Local Procedure                                                  911
C ----------------------------------------------------------------------  912
C ------------ SBRM                                                       912
C Begin Local Procedure SBRM                                              913
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          913
6090  CONTINUE                                                            913
         AG_TITLE='SBRM'                                                  913
         AG_MODULE=MODULE                                                 913
         AG_MARK='V00'                                                    913
         CALL AGSPUSH                                                     913
         AG_MODULE=MODULE                                                 913
C    *                                                                    913
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       914
         CALL AGSRESET                                                    914
         AG_SEEN = 0                                                      914
         AG_COLO = 1                                                      914
         AG_TITLE = 'SBRM'                                                914
         AG_EXNAME = 'ATTRIBUTE'                                          914
         AG_PARLIST = 'SEEN_COLO'                                         914
         CALL AXATTRIBUTE                                                 914
C    *                                                                    914
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                914
         CALL AGSRESET                                                    915
         AG_NDIV = 4                                                      915
         AG_IAXIS = 2                                                     915
         AG_C0 = 45                                                       915
         AG_TITLE = 'DIVISION'                                            915
         AG_EXNAME = 'SHAPE'                                              915
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     915
         CALL AXSHAPE                                                     915
C    *                                                                    915
C    Create SBRI :                                                        916
C    Call To Local Procedure SBRI                                         916
         ASSIGN 6120 TO BLOCK_SBRI                                        916
         GO TO  6110                                                      916
6120     CONTINUE                                                         916
C    *                                                                    916
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        917
         CALL AGSRESET                                                    917
         AG_X = SSUP_ERJRAD                                               917
         AG_TITLE = 'SBRI'                                                917
         AG_EXNAME = 'POSITION'                                           917
         AG_PARLIST = 'X'                                                 917
         CALL AXPOSITION                                                  917
C    *                                                                    917
6091  CALL AGSPOPB                                                        918
      GO TO BLOCK_SBRM, (6090 ,6100)                                      918
C End of Local Procedure                                                  918
C ----------------------------------------------------------------------  919
C ------------ SBRI                                                       919
C Begin Local Procedure SBRI                                              920
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               920
6110  CONTINUE                                                            920
         AG_TITLE='SBRI'                                                  920
         AG_MODULE=MODULE                                                 920
         AG_MARK='V00'                                                    920
         CALL AGSPUSH                                                     920
         AG_MODULE=MODULE                                                 920
C    *                                                                    920
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       921
         CALL AGSRESET                                                    921
         AG_SEEN = 1                                                      921
         AG_COLO = 2                                                      921
         AG_TITLE = 'SBRI'                                                921
         AG_EXNAME = 'ATTRIBUTE'                                          921
         AG_PARLIST = 'SEEN_COLO'                                         921
         CALL AXATTRIBUTE                                                 921
C    *                                                                    921
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        921
         CALL AGSRESET                                                    922
         AG_TITLE = 'BERILLIUM'                                           922
         AG_EXNAME = 'MATERIAL'                                           922
         AG_PARLIST = 'NONE'                                              922
         CALL AXMATERIAL                                                  922
C    *                                                                    922
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  922
C    RJTHK/2 <===                                                         922
         CALL AGSRESET                                                    924
         AG_DX = SSUP_ERJLEN/2                                            924
         AG_DY = SSUP_ERJWID/2                                            924
         AG_DZ = SSUP_ERJTHK/2                                            924
         AG_TITLE = 'BOX'                                                 924
         AG_EXNAME = 'SHAPE'                                              924
         AG_PARLIST = 'DX_DY_DZ'                                          924
         CALL AXSHAPE                                                     924
C    *                                                                    924
6111  CALL AGSPOPB                                                        925
      GO TO BLOCK_SBRI, (6110 ,6120)                                      925
C End of Local Procedure                                                  925
C ----------------------------------------------------------------------  926
C ------------ SROD                                                       926
C Begin Local Procedure SROD                                              927
C SROD IS THE SVT CARBON COMPOSITE SUPPORT ROD                            927
5250  CONTINUE                                                            927
         AG_TITLE='SROD'                                                  927
         AG_MODULE=MODULE                                                 927
         AG_MARK='V00'                                                    927
         CALL AGSPUSH                                                     927
         AG_MODULE=MODULE                                                 927
C    *                                                                    927
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           928
         CALL AGSRESET                                                    928
         AG_TITLE = 'CARBON'                                              928
         AG_EXNAME = 'MATERIAL'                                           928
         AG_PARLIST = 'NONE'                                              928
         CALL AXMATERIAL                                                  928
C    *                                                                    928
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       928
         CALL AGSRESET                                                    929
         AG_SEEN = 1                                                      929
         AG_COLO = 2                                                      929
         AG_TITLE = 'SROD'                                                929
         AG_EXNAME = 'ATTRIBUTE'                                          929
         AG_PARLIST = 'SEEN_COLO'                                         929
         CALL AXATTRIBUTE                                                 929
C    *                                                                    929
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  929
C    P_RODLEN/2 <===                                                      929
         CALL AGSRESET                                                    931
         AG_RMIN = SSUP_RODID/2                                           931
         AG_RMAX = SSUP_RODOD/2                                           931
         AG_DZ = SSUP_RODLEN/2                                            931
         AG_TITLE = 'TUBE'                                                931
         AG_EXNAME = 'SHAPE'                                              931
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      931
         CALL AXSHAPE                                                     931
C    *                                                                    931
5251  CALL AGSPOPB                                                        932
      GO TO BLOCK_SROD, (5250 ,5260)                                      932
C End of Local Procedure                                                  932
C ----------------------------------------------------------------------  933
C ------------ SCON                                                       933
C Begin Local Procedure SCON                                              934
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               934
5210  CONTINUE                                                            934
         AG_TITLE='SCON'                                                  934
         AG_MODULE=MODULE                                                 934
         AG_MARK='V00'                                                    934
         CALL AGSPUSH                                                     934
         AG_MODULE=MODULE                                                 934
C    *                                                                    934
         CONE_THK1=SSUP_GRPHTHK                                           935
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                936
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              937
C    Geant MATERIAL : AIR ===> inherited arguments used <===              938
         CALL AGSRESET                                                    938
         AG_TITLE = 'AIR'                                                 938
         AG_EXNAME = 'MATERIAL'                                           938
         AG_PARLIST = 'NONE'                                              938
         CALL AXMATERIAL                                                  938
C    *                                                                    938
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       938
         CALL AGSRESET                                                    939
         AG_SEEN = 0                                                      939
         AG_COLO = 1                                                      939
         AG_TITLE = 'SCON'                                                939
         AG_EXNAME = 'ATTRIBUTE'                                          939
         AG_PARLIST = 'SEEN_COLO'                                         939
         CALL AXATTRIBUTE                                                 939
C    *                                                                    939
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  939
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  939
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  939
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  939
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  939
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  939
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      939
         CALL AGSRESET                                                    949
         AG_PHI1 = 0                                                      949
         AG_DPHI = 360                                                    949
         AG_NZ = 7                                                        949
         AG_ZI(1)=SSUP_CONE1ZMN                                           949
         AG_ZI(2)=SSUP_RODLEN/2                                           949
         AG_ZI(3)=SSUP_RODLEN/2                                           949
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              949
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              949
         AG_ZI(6)=SSUP_CONE3ZMX                                           949
         AG_ZI(7)=SSUP_CONE4ZMX                                           949
         AG_RMN(1)=SSUP_CON1IDMN                                          949
         AG_RMN(2)=SSUP_CON1IDMN                                          949
         AG_RMN(3)=SSUP_CON1IDMN                                          949
         AG_RMN(4)=SSUP_CON1IDMN                                          949
         AG_RMN(5)=SSUP_CON3IDMN                                          949
         AG_RMN(6)=SSUP_CON4IDMN                                          949
         AG_RMN(7)=SSUP_CON4IDMX                                          949
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                949
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                949
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                949
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                949
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                949
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                949
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                949
         AG_TITLE = 'PCON'                                                949
         AG_EXNAME = 'SHAPE'                                              949
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           949
         CALL AXSHAPE                                                     949
C    *                                                                    949
C    Create SGRA :                                                        950
C    Call To Local Procedure SGRA                                         950
         ASSIGN 6140 TO BLOCK_SGRA                                        950
         GO TO  6130                                                      950
6140     CONTINUE                                                         950
C    *                                                                    950
C    Geant POSITION : SGRA ===> inherited arguments used <===             951
         CALL AGSRESET                                                    951
         AG_TITLE = 'SGRA'                                                951
         AG_EXNAME = 'POSITION'                                           951
         AG_PARLIST = 'NONE'                                              951
         CALL AXPOSITION                                                  951
C    *                                                                    951
C    Create STAP :                                                        952
C    Call To Local Procedure STAP                                         952
         ASSIGN 6160 TO BLOCK_STAP                                        952
         GO TO  6150                                                      952
6160     CONTINUE                                                         952
C    *                                                                    952
C    Geant POSITION : STAP ===> inherited arguments used <===             953
         CALL AGSRESET                                                    953
         AG_TITLE = 'STAP'                                                953
         AG_EXNAME = 'POSITION'                                           953
         AG_PARLIST = 'NONE'                                              953
         CALL AXPOSITION                                                  953
C    *                                                                    953
C    Create STAC :                                                        954
C    Call To Local Procedure STAC                                         954
         ASSIGN 6180 TO BLOCK_STAC                                        954
         GO TO  6170                                                      954
6180     CONTINUE                                                         954
C    *                                                                    954
C    Geant POSITION : STAC ===> inherited arguments used <===             955
         CALL AGSRESET                                                    955
         AG_TITLE = 'STAC'                                                955
         AG_EXNAME = 'POSITION'                                           955
         AG_PARLIST = 'NONE'                                              955
         CALL AXPOSITION                                                  955
C    *                                                                    955
C    Create SHLA :                                                        956
C    Call To Local Procedure SHLA                                         956
         ASSIGN 6200 TO BLOCK_SHLA                                        956
         GO TO  6190                                                      956
6200     CONTINUE                                                         956
C    *                                                                    956
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  958
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             958
         CALL AGSRESET                                                    958
         AG_Z = SSUP_RODLEN/                                              958
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    958
         AG_TITLE = 'SHLA'                                                958
         AG_EXNAME = 'POSITION'                                           958
         AG_PARLIST = 'Z'                                                 958
         CALL AXPOSITION                                                  958
C    *                                                                    958
C    Create SHLB :                                                        959
C    Call To Local Procedure SHLB                                         959
         ASSIGN 6220 TO BLOCK_SHLB                                        959
         GO TO  6210                                                      959
6220     CONTINUE                                                         959
C    *                                                                    959
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  960
C    CONE3ZMX) <===                                                       960
         CALL AGSRESET                                                    960
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           960
         AG_TITLE = 'SHLB'                                                960
         AG_EXNAME = 'POSITION'                                           960
         AG_PARLIST = 'Z'                                                 960
         CALL AXPOSITION                                                  960
C    *                                                                    960
C    Create SCMY :                                                        961
C    Call To Local Procedure SCMY                                         961
         ASSIGN 6240 TO BLOCK_SCMY                                        961
         GO TO  6230                                                      961
6240     CONTINUE                                                         961
C    *                                                                    961
C    Geant POSITION : SCMY ===> inherited arguments used <===             962
         CALL AGSRESET                                                    962
         AG_TITLE = 'SCMY'                                                962
         AG_EXNAME = 'POSITION'                                           962
         AG_PARLIST = 'NONE'                                              962
         CALL AXPOSITION                                                  962
C    *                                                                    962
C    Create SCAL :                                                        963
C    Call To Local Procedure SCAL                                         963
         ASSIGN 6260 TO BLOCK_SCAL                                        963
         GO TO  6250                                                      963
6260     CONTINUE                                                         963
C    *                                                                    963
C    Geant POSITION : SCAL ===> inherited arguments used <===             964
         CALL AGSRESET                                                    964
         AG_TITLE = 'SCAL'                                                964
         AG_EXNAME = 'POSITION'                                           964
         AG_PARLIST = 'NONE'                                              964
         CALL AXPOSITION                                                  964
C    *                                                                    964
5211  CALL AGSPOPB                                                        965
      GO TO BLOCK_SCON, (5210 ,5220)                                      965
C End of Local Procedure                                                  965
C ----------------------------------------------------------------------  966
C ------------ SGRA                                                       966
C Begin Local Procedure SGRA                                              967
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 967
6130  CONTINUE                                                            967
         AG_TITLE='SGRA'                                                  967
         AG_MODULE=MODULE                                                 967
         AG_MARK='V00'                                                    967
         CALL AGSPUSH                                                     967
         AG_MODULE=MODULE                                                 967
C    *                                                                    967
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           968
         CALL AGSRESET                                                    968
         AG_TITLE = 'CARBON'                                              968
         AG_EXNAME = 'MATERIAL'                                           968
         AG_PARLIST = 'NONE'                                              968
         CALL AXMATERIAL                                                  968
C    *                                                                    968
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       968
         CALL AGSRESET                                                    969
         AG_SEEN = 1                                                      969
         AG_COLO = 6                                                      969
         AG_TITLE = 'SGRA'                                                969
         AG_EXNAME = 'ATTRIBUTE'                                          969
         AG_PARLIST = 'SEEN_COLO'                                         969
         CALL AXATTRIBUTE                                                 969
C    *                                                                    969
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  969
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  969
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  969
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  969
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  969
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              969
         CALL AGSRESET                                                    977
         AG_PHI1 = 0                                                      977
         AG_DPHI = 360                                                    977
         AG_NZ = 5                                                        977
         AG_ZI(1)=SSUP_RODLEN/2                                           977
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              977
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              977
         AG_ZI(4)=SSUP_CONE3ZMX                                           977
         AG_ZI(5)=SSUP_CONE4ZMX                                           977
         AG_RMN(1)=SSUP_CON1IDMN                                          977
         AG_RMN(2)=SSUP_CON1IDMN                                          977
         AG_RMN(3)=SSUP_CON3IDMN                                          977
         AG_RMN(4)=SSUP_CON4IDMN                                          977
         AG_RMN(5)=SSUP_CON4IDMX                                          977
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             977
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             977
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             977
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             977
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             977
         AG_TITLE = 'PCON'                                                977
         AG_EXNAME = 'SHAPE'                                              977
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           977
         CALL AXSHAPE                                                     977
C    *                                                                    977
6131  CALL AGSPOPB                                                        978
      GO TO BLOCK_SGRA, (6130 ,6140)                                      978
C End of Local Procedure                                                  978
C ----------------------------------------------------------------------  979
C ------------ STAP                                                       979
C Begin Local Procedure STAP                                              980
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  980
C E)                                                                      980
6150  CONTINUE                                                            980
         AG_TITLE='STAP'                                                  980
         AG_MODULE=MODULE                                                 980
         AG_MARK='V00'                                                    980
         CALL AGSPUSH                                                     980
         AG_MODULE=MODULE                                                 980
C    *                                                                    980
         ROFFSET=SSUP_GRPHTHK                                             981
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           982
         CALL AGSRESET                                                    982
         AG_A = 12                                                        982
         AG_Z = 6                                                         982
         AG_W = 1                                                         982
         AG_TITLE = 'C'                                                   982
         AG_EXNAME = 'COMPONENT'                                          982
         AG_PARLIST = 'A_Z_W'                                             982
         CALL AXCOMPONENT                                                 982
C    *                                                                    982
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           982
         CALL AGSRESET                                                    983
         AG_A = 1                                                         983
         AG_Z = 1                                                         983
         AG_W = 2                                                         983
         AG_TITLE = 'H2'                                                  983
         AG_EXNAME = 'COMPONENT'                                          983
         AG_PARLIST = 'A_Z_W'                                             983
         CALL AXCOMPONENT                                                 983
C    *                                                                    983
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             983
         CALL AGSRESET                                                    984
         AG_DENS = 0.935                                                  984
         AG_TITLE = 'CH2'                                                 984
         AG_EXNAME = 'MIXTURE'                                            984
         AG_PARLIST = 'DENS'                                              984
         CALL AXMIXTURE                                                   984
C    *                                                                    984
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       984
         CALL AGSRESET                                                    985
         AG_SEEN = 1                                                      985
         AG_COLO = 3                                                      985
         AG_TITLE = 'STAP'                                                985
         AG_EXNAME = 'ATTRIBUTE'                                          985
         AG_PARLIST = 'SEEN_COLO'                                         985
         CALL AXATTRIBUTE                                                 985
C    *                                                                    985
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  985
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  985
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  985
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  985
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     985
         CALL AGSRESET                                                    991
         AG_PHI1 = 0                                                      991
         AG_DPHI = 360                                                    991
         AG_NZ = 3                                                        991
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              991
         AG_ZI(2)=SSUP_CONE3ZMX                                           991
         AG_ZI(3)=SSUP_CONE4ZMX                                           991
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  991
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  991
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  991
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    991
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    991
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    991
         AG_TITLE = 'PCON'                                                991
         AG_EXNAME = 'SHAPE'                                              991
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           991
         CALL AXSHAPE                                                     991
C    *                                                                    991
6151  CALL AGSPOPB                                                        992
      GO TO BLOCK_STAP, (6150 ,6160)                                      992
C End of Local Procedure                                                  992
C ----------------------------------------------------------------------  993
C ------------ STAC                                                       993
C Begin Local Procedure STAC                                              994
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      994
6170  CONTINUE                                                            994
         AG_TITLE='STAC'                                                  994
         AG_MODULE=MODULE                                                 994
         AG_MARK='V00'                                                    994
         CALL AGSPUSH                                                     994
         AG_MODULE=MODULE                                                 994
C    *                                                                    994
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             995
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           996
         CALL AGSRESET                                                    996
         AG_TITLE = 'COPPER'                                              996
         AG_EXNAME = 'MATERIAL'                                           996
         AG_PARLIST = 'NONE'                                              996
         CALL AXMATERIAL                                                  996
C    *                                                                    996
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       996
         CALL AGSRESET                                                    997
         AG_SEEN = 1                                                      997
         AG_COLO = 2                                                      997
         AG_TITLE = 'STAC'                                                997
         AG_EXNAME = 'ATTRIBUTE'                                          997
         AG_PARLIST = 'SEEN_COLO'                                         997
         CALL AXATTRIBUTE                                                 997
C    *                                                                    997
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  997
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  997
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  997
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  997
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     997
         CALL AGSRESET                                                    1003
         AG_PHI1 = 0                                                      1003
         AG_DPHI = 360                                                    1003
         AG_NZ = 3                                                        1003
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1003
         AG_ZI(2)=SSUP_CONE3ZMX                                           1003
         AG_ZI(3)=SSUP_CONE4ZMX                                           1003
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1003
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1003
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1003
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1003
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1003
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1003
         AG_TITLE = 'PCON'                                                1003
         AG_EXNAME = 'SHAPE'                                              1003
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1003
         CALL AXSHAPE                                                     1003
C    *                                                                    1003
6171  CALL AGSPOPB                                                        1004
      GO TO BLOCK_STAC, (6170 ,6180)                                      1004
C End of Local Procedure                                                  1004
C ----------------------------------------------------------------------  1005
C ------------ SHLA                                                       1005
C Begin Local Procedure SHLA                                              1006
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              1006
6190  CONTINUE                                                            1006
         AG_TITLE='SHLA'                                                  1006
         AG_MODULE=MODULE                                                 1006
         AG_MARK='V00'                                                    1006
         CALL AGSPUSH                                                     1006
         AG_MODULE=MODULE                                                 1006
C    *                                                                    1006
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1007
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1008
         CALL AGSRESET                                                    1008
         AG_TITLE = 'AIR'                                                 1008
         AG_EXNAME = 'MATERIAL'                                           1008
         AG_PARLIST = 'NONE'                                              1008
         CALL AXMATERIAL                                                  1008
C    *                                                                    1008
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       1008
         CALL AGSRESET                                                    1009
         AG_SEEN = 0                                                      1009
         AG_COLO = 1                                                      1009
         AG_TITLE = 'SHLA'                                                1009
         AG_EXNAME = 'ATTRIBUTE'                                          1009
         AG_PARLIST = 'SEEN_COLO'                                         1009
         CALL AXATTRIBUTE                                                 1009
C    *                                                                    1009
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  1009
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  1009
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  1009
C    .0*SSUP_HOSRMX <===                                                  1009
         CALL AGSRESET                                                    1013
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            1013
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  1013
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1013
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  1013
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1013
         AG_TITLE = 'CONE'                                                1013
         AG_EXNAME = 'SHAPE'                                              1013
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1013
         CALL AXSHAPE                                                     1013
C    *                                                                    1013
C    Create SHMA :                                                        1014
C    Call To Local Procedure SHMA                                         1014
         ASSIGN 6280 TO BLOCK_SHMA                                        1014
         GO TO  6270                                                      1014
6280     CONTINUE                                                         1014
C    *                                                                    1014
6191  CALL AGSPOPB                                                        1015
      GO TO BLOCK_SHLA, (6190 ,6200)                                      1015
C End of Local Procedure                                                  1015
C ----------------------------------------------------------------------  1016
C ------------ SHMA                                                       1016
C Begin Local Procedure SHMA                                              1017
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           1017
6270  CONTINUE                                                            1017
         AG_TITLE='SHMA'                                                  1017
         AG_MODULE=MODULE                                                 1017
         AG_MARK='V00'                                                    1017
         CALL AGSPUSH                                                     1017
         AG_MODULE=MODULE                                                 1017
C    *                                                                    1017
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1018
         RMIN=SSUP_CON3IDMN + ROFFSET                                     1019
         RMAX=SSUP_CON4IDMN + ROFFSET                                     1020
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                1021
         ZMAX=SSUP_CONE3ZMX                                               1022
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1023
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1024
         CONE_LEN = SQRT(CONE_LEN)                                        1025
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1026
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1027
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1028
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1029
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1030
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       1031
         CALL AGSRESET                                                    1031
         AG_SEEN = 0                                                      1031
         AG_COLO = 1                                                      1031
         AG_TITLE = 'SHMA'                                                1031
         AG_EXNAME = 'ATTRIBUTE'                                          1031
         AG_PARLIST = 'SEEN_COLO'                                         1031
         CALL AXATTRIBUTE                                                 1031
C    *                                                                    1031
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1031
         CALL AGSRESET                                                    1033
         AG_NDIV = SSUP_NHOSES                                            1033
         AG_IAXIS = 2                                                     1033
         AG_C0 = 0.0                                                      1033
         AG_TITLE = 'DIVISION'                                            1033
         AG_EXNAME = 'SHAPE'                                              1033
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1033
         CALL AXSHAPE                                                     1033
C    *                                                                    1033
C    Create SWHO :                                                        1034
C    Call To Local Procedure SWHO                                         1034
         ASSIGN 6300 TO BLOCK_SWHO                                        1034
         GO TO  6290                                                      1034
6300     CONTINUE                                                         1034
C    *                                                                    1034
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1035
         CALL AGSRESET                                                    1035
         AG_X = XPOS                                                      1035
         AG_Y = 0                                                         1035
         AG_Z = ZPOS                                                      1035
         AG_ALPHAN=AG_ALPHAN+1                                            1035
         AG_ALPHAK(AG_ALPHAN)=2                                           1035
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1035
         AG_TITLE = 'SWHO'                                                1035
         AG_EXNAME = 'POSITION'                                           1035
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1035
         CALL AXPOSITION                                                  1035
C    *                                                                    1035
6271  CALL AGSPOPB                                                        1036
      GO TO BLOCK_SHMA, (6270 ,6280)                                      1036
C End of Local Procedure                                                  1036
C ----------------------------------------------------------------------  1037
C ------------ SHLB                                                       1037
C Begin Local Procedure SHLB                                              1038
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               1038
6210  CONTINUE                                                            1038
         AG_TITLE='SHLB'                                                  1038
         AG_MODULE=MODULE                                                 1038
         AG_MARK='V00'                                                    1038
         CALL AGSPUSH                                                     1038
         AG_MODULE=MODULE                                                 1038
C    *                                                                    1038
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1039
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1040
         CALL AGSRESET                                                    1040
         AG_TITLE = 'AIR'                                                 1040
         AG_EXNAME = 'MATERIAL'                                           1040
         AG_PARLIST = 'NONE'                                              1040
         CALL AXMATERIAL                                                  1040
C    *                                                                    1040
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       1040
         CALL AGSRESET                                                    1041
         AG_SEEN = 0                                                      1041
         AG_COLO = 1                                                      1041
         AG_TITLE = 'SHLB'                                                1041
         AG_EXNAME = 'ATTRIBUTE'                                          1041
         AG_PARLIST = 'SEEN_COLO'                                         1041
         CALL AXATTRIBUTE                                                 1041
C    *                                                                    1041
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  1041
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  1041
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  1041
C    X <===                                                               1041
         CALL AGSRESET                                                    1045
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         1045
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  1045
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1045
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  1045
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  1045
         AG_TITLE = 'CONE'                                                1045
         AG_EXNAME = 'SHAPE'                                              1045
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1045
         CALL AXSHAPE                                                     1045
C    *                                                                    1045
C    Create SHMB :                                                        1046
C    Call To Local Procedure SHMB                                         1046
         ASSIGN 6320 TO BLOCK_SHMB                                        1046
         GO TO  6310                                                      1046
6320     CONTINUE                                                         1046
C    *                                                                    1046
6211  CALL AGSPOPB                                                        1047
      GO TO BLOCK_SHLB, (6210 ,6220)                                      1047
C End of Local Procedure                                                  1047
C ----------------------------------------------------------------------  1048
C ------------ SHMB                                                       1048
C Begin Local Procedure SHMB                                              1049
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           1049
6310  CONTINUE                                                            1049
         AG_TITLE='SHMB'                                                  1049
         AG_MODULE=MODULE                                                 1049
         AG_MARK='V00'                                                    1049
         CALL AGSPUSH                                                     1049
         AG_MODULE=MODULE                                                 1049
C    *                                                                    1049
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1050
         RMIN=SSUP_CON4IDMN + ROFFSET                                     1051
         RMAX=SSUP_CON4IDMX + ROFFSET                                     1052
         ZMIN=SSUP_CONE3ZMX                                               1053
         ZMAX=SSUP_CONE4ZMX                                               1054
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1055
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1056
         CONE_LEN = SQRT(CONE_LEN)                                        1057
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1058
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1059
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1060
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1061
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1062
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       1063
         CALL AGSRESET                                                    1063
         AG_SEEN = 0                                                      1063
         AG_COLO = 1                                                      1063
         AG_TITLE = 'SHMB'                                                1063
         AG_EXNAME = 'ATTRIBUTE'                                          1063
         AG_PARLIST = 'SEEN_COLO'                                         1063
         CALL AXATTRIBUTE                                                 1063
C    *                                                                    1063
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1063
         CALL AGSRESET                                                    1065
         AG_NDIV = SSUP_NHOSES                                            1065
         AG_IAXIS = 2                                                     1065
         AG_C0 = 0.0                                                      1065
         AG_TITLE = 'DIVISION'                                            1065
         AG_EXNAME = 'SHAPE'                                              1065
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1065
         CALL AXSHAPE                                                     1065
C    *                                                                    1065
C    Create SWHO :                                                        1066
C    Call To Local Procedure SWHO                                         1066
         ASSIGN 6330 TO BLOCK_SWHO                                        1066
         GO TO  6290                                                      1066
6330     CONTINUE                                                         1066
C    *                                                                    1066
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1067
         CALL AGSRESET                                                    1067
         AG_X = XPOS                                                      1067
         AG_Y = 0                                                         1067
         AG_Z = ZPOS                                                      1067
         AG_ALPHAN=AG_ALPHAN+1                                            1067
         AG_ALPHAK(AG_ALPHAN)=2                                           1067
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1067
         AG_TITLE = 'SWHO'                                                1067
         AG_EXNAME = 'POSITION'                                           1067
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1067
         CALL AXPOSITION                                                  1067
C    *                                                                    1067
6311  CALL AGSPOPB                                                        1068
      GO TO BLOCK_SHMB, (6310 ,6320)                                      1068
C End of Local Procedure                                                  1068
C ----------------------------------------------------------------------  1069
C ------------ SWHO                                                       1069
C Begin Local Procedure SWHO                                              1070
C SWHO IS A WATER HOSE                                                    1070
6290  CONTINUE                                                            1070
         AG_TITLE='SWHO'                                                  1070
         AG_MODULE=MODULE                                                 1070
         AG_MARK='V00'                                                    1070
         CALL AGSPUSH                                                     1070
         AG_MODULE=MODULE                                                 1070
C    *                                                                    1070
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1071
         CALL AGSRESET                                                    1071
         AG_SEEN = 1                                                      1071
         AG_COLO = 3                                                      1071
         AG_TITLE = 'SWHO'                                                1071
         AG_EXNAME = 'ATTRIBUTE'                                          1071
         AG_PARLIST = 'SEEN_COLO'                                         1071
         CALL AXATTRIBUTE                                                 1071
C    *                                                                    1071
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1071
         CALL AGSRESET                                                    1072
         AG_A = 12                                                        1072
         AG_Z = 6                                                         1072
         AG_W = 1                                                         1072
         AG_TITLE = 'C'                                                   1072
         AG_EXNAME = 'COMPONENT'                                          1072
         AG_PARLIST = 'A_Z_W'                                             1072
         CALL AXCOMPONENT                                                 1072
C    *                                                                    1072
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1072
         CALL AGSRESET                                                    1073
         AG_A = 1                                                         1073
         AG_Z = 1                                                         1073
         AG_W = 2                                                         1073
         AG_TITLE = 'H2'                                                  1073
         AG_EXNAME = 'COMPONENT'                                          1073
         AG_PARLIST = 'A_Z_W'                                             1073
         CALL AXCOMPONENT                                                 1073
C    *                                                                    1073
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1073
         CALL AGSRESET                                                    1074
         AG_DENS = 0.935                                                  1074
         AG_TITLE = 'CH2'                                                 1074
         AG_EXNAME = 'MIXTURE'                                            1074
         AG_PARLIST = 'DENS'                                              1074
         CALL AXMIXTURE                                                   1074
C    *                                                                    1074
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1074
         CALL AGSRESET                                                    1075
         AG_SEEN = 1                                                      1075
         AG_COLO = 3                                                      1075
         AG_TITLE = 'SWHO'                                                1075
         AG_EXNAME = 'ATTRIBUTE'                                          1075
         AG_PARLIST = 'SEEN_COLO'                                         1075
         CALL AXATTRIBUTE                                                 1075
C    *                                                                    1075
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  1075
C    =                                                                    1075
         CALL AGSRESET                                                    1076
         AG_RMIN = 0                                                      1076
         AG_RMAX = SSUP_HOSRMX                                            1076
         AG_DZ = 0.5*CONE_LEN                                             1076
         AG_TITLE = 'TUBE'                                                1076
         AG_EXNAME = 'SHAPE'                                              1076
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1076
         CALL AXSHAPE                                                     1076
C    *                                                                    1076
C    Create SHWA :                                                        1077
C    Call To Local Procedure SHWA                                         1077
         ASSIGN 6350 TO BLOCK_SHWA                                        1077
         GO TO  6340                                                      1077
6350     CONTINUE                                                         1077
C    *                                                                    1077
C    Geant POSITION : SHWA ===> inherited arguments used <===             1078
         CALL AGSRESET                                                    1078
         AG_TITLE = 'SHWA'                                                1078
         AG_EXNAME = 'POSITION'                                           1078
         AG_PARLIST = 'NONE'                                              1078
         CALL AXPOSITION                                                  1078
C    *                                                                    1078
6291  CALL AGSPOPB                                                        1079
      GO TO BLOCK_SWHO, (6290 ,6300,6330)                                 1079
C End of Local Procedure                                                  1079
C ----------------------------------------------------------------------  1080
C ------------ SHWA                                                       1080
C Begin Local Procedure SHWA                                              1081
C SHWA IS THE WATER IN THE HOSE                                           1081
6340  CONTINUE                                                            1081
         AG_TITLE='SHWA'                                                  1081
         AG_MODULE=MODULE                                                 1081
         AG_MARK='V00'                                                    1081
         CALL AGSPUSH                                                     1081
         AG_MODULE=MODULE                                                 1081
C    *                                                                    1081
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       1082
         CALL AGSRESET                                                    1082
         AG_SEEN = 1                                                      1082
         AG_COLO = 6                                                      1082
         AG_TITLE = 'SHWA'                                                1082
         AG_EXNAME = 'ATTRIBUTE'                                          1082
         AG_PARLIST = 'SEEN_COLO'                                         1082
         CALL AXATTRIBUTE                                                 1082
C    *                                                                    1082
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1082
         CALL AGSRESET                                                    1083
         AG_TITLE = 'WATER'                                               1083
         AG_EXNAME = 'MATERIAL'                                           1083
         AG_PARLIST = 'NONE'                                              1083
         CALL AXMATERIAL                                                  1083
C    *                                                                    1083
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        1083
         CALL AGSRESET                                                    1084
         AG_RMAX = SSUP_HOSRMN                                            1084
         AG_TITLE = 'TUBE'                                                1084
         AG_EXNAME = 'SHAPE'                                              1084
         AG_PARLIST = 'RMAX'                                              1084
         CALL AXSHAPE                                                     1084
C    *                                                                    1084
6341  CALL AGSPOPB                                                        1085
      GO TO BLOCK_SHWA, (6340 ,6350)                                      1085
C End of Local Procedure                                                  1085
C ----------------------------------------------------------------------  1086
C ------------ SCMY                                                       1086
C Begin Local Procedure SCMY                                              1087
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            1087
6230  CONTINUE                                                            1087
         AG_TITLE='SCMY'                                                  1087
         AG_MODULE=MODULE                                                 1087
         AG_MARK='V00'                                                    1087
         CALL AGSPUSH                                                     1087
         AG_MODULE=MODULE                                                 1087
C    *                                                                    1087
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             1088
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          1089
         CALL AGSRESET                                                    1089
         AG_A = 12                                                        1089
         AG_Z = 6                                                         1089
         AG_W = 5                                                         1089
         AG_TITLE = 'C5'                                                  1089
         AG_EXNAME = 'COMPONENT'                                          1089
         AG_PARLIST = 'A_Z_W'                                             1089
         CALL AXCOMPONENT                                                 1089
C    *                                                                    1089
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           1089
         CALL AGSRESET                                                    1090
         AG_A = 1                                                         1090
         AG_Z = 1                                                         1090
         AG_W = 4                                                         1090
         AG_TITLE = 'H4'                                                  1090
         AG_EXNAME = 'COMPONENT'                                          1090
         AG_PARLIST = 'A_Z_W'                                             1090
         CALL AXCOMPONENT                                                 1090
C    *                                                                    1090
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          1090
         CALL AGSRESET                                                    1091
         AG_A = 16                                                        1091
         AG_Z = 8                                                         1091
         AG_W = 2                                                         1091
         AG_TITLE = 'O2'                                                  1091
         AG_EXNAME = 'COMPONENT'                                          1091
         AG_PARLIST = 'A_Z_W'                                             1091
         CALL AXCOMPONENT                                                 1091
C    *                                                                    1091
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            1091
         CALL AGSRESET                                                    1092
         AG_DENS = 1.39                                                   1092
         AG_TITLE = 'MYLAR'                                               1092
         AG_EXNAME = 'MIXTURE'                                            1092
         AG_PARLIST = 'DENS'                                              1092
         CALL AXMIXTURE                                                   1092
C    *                                                                    1092
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       1092
         CALL AGSRESET                                                    1093
         AG_SEEN = 1                                                      1093
         AG_COLO = 3                                                      1093
         AG_TITLE = 'SCMY'                                                1093
         AG_EXNAME = 'ATTRIBUTE'                                          1093
         AG_PARLIST = 'SEEN_COLO'                                         1093
         CALL AXATTRIBUTE                                                 1093
C    *                                                                    1093
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1093
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1093
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1093
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  1093
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     1093
         CALL AGSRESET                                                    1099
         AG_PHI1 = 0                                                      1099
         AG_DPHI = 360                                                    1099
         AG_NZ = 3                                                        1099
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1099
         AG_ZI(2)=SSUP_CONE3ZMX                                           1099
         AG_ZI(3)=SSUP_CONE4ZMX                                           1099
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1099
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1099
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1099
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    1099
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    1099
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    1099
         AG_TITLE = 'PCON'                                                1099
         AG_EXNAME = 'SHAPE'                                              1099
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1099
         CALL AXSHAPE                                                     1099
C    *                                                                    1099
6231  CALL AGSPOPB                                                        1100
      GO TO BLOCK_SCMY, (6230 ,6240)                                      1100
C End of Local Procedure                                                  1100
C ----------------------------------------------------------------------  1101
C ------------ SCAL                                                       1101
C Begin Local Procedure SCAL                                              1102
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     1102
6250  CONTINUE                                                            1102
         AG_TITLE='SCAL'                                                  1102
         AG_MODULE=MODULE                                                 1102
         AG_MARK='V00'                                                    1102
         CALL AGSPUSH                                                     1102
         AG_MODULE=MODULE                                                 1102
C    *                                                                    1102
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           1103
     *   SSUP_WRPMYTHK                                                    1103
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1104
         CALL AGSRESET                                                    1104
         AG_TITLE = 'ALUMINIUM'                                           1104
         AG_EXNAME = 'MATERIAL'                                           1104
         AG_PARLIST = 'NONE'                                              1104
         CALL AXMATERIAL                                                  1104
C    *                                                                    1104
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       1104
         CALL AGSRESET                                                    1105
         AG_SEEN = 1                                                      1105
         AG_COLO = 2                                                      1105
         AG_TITLE = 'SCAL'                                                1105
         AG_EXNAME = 'ATTRIBUTE'                                          1105
         AG_PARLIST = 'SEEN_COLO'                                         1105
         CALL AXATTRIBUTE                                                 1105
C    *                                                                    1105
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1105
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1105
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1105
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  1105
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     1105
         CALL AGSRESET                                                    1111
         AG_PHI1 = 0                                                      1111
         AG_DPHI = 360                                                    1111
         AG_NZ = 3                                                        1111
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1111
         AG_ZI(2)=SSUP_CONE3ZMX                                           1111
         AG_ZI(3)=SSUP_CONE4ZMX                                           1111
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1111
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1111
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1111
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    1111
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    1111
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    1111
         AG_TITLE = 'PCON'                                                1111
         AG_EXNAME = 'SHAPE'                                              1111
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1111
         CALL AXSHAPE                                                     1111
C    *                                                                    1111
6251  CALL AGSPOPB                                                        1112
      GO TO BLOCK_SCAL, (6250 ,6260)                                      1112
C End of Local Procedure                                                  1112
C ----------------------------------------------------------------------  1113
C ------------ SWMM                                                       1113
C Begin Local Procedure SWMM                                              1114
C SWMM IS THE WATER MANIFOLD MOTHER                                       1114
5170  CONTINUE                                                            1114
         AG_TITLE='SWMM'                                                  1114
         AG_MODULE=MODULE                                                 1114
         AG_MARK='V00'                                                    1114
         CALL AGSPUSH                                                     1114
         AG_MODULE=MODULE                                                 1114
C    *                                                                    1114
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     1115
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     1116
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1117
         CALL AGSRESET                                                    1117
         AG_TITLE = 'AIR'                                                 1117
         AG_EXNAME = 'MATERIAL'                                           1117
         AG_PARLIST = 'NONE'                                              1117
         CALL AXMATERIAL                                                  1117
C    *                                                                    1117
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       1117
         CALL AGSRESET                                                    1118
         AG_SEEN = 0                                                      1118
         AG_COLO = 1                                                      1118
         AG_TITLE = 'SWMM'                                                1118
         AG_EXNAME = 'ATTRIBUTE'                                          1118
         AG_PARLIST = 'SEEN_COLO'                                         1118
         CALL AXATTRIBUTE                                                 1118
C    *                                                                    1118
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1118
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   1118
         CALL AGSRESET                                                    1121
         AG_PHI1 = 0                                                      1121
         AG_DPHI = 360                                                    1121
         AG_NPDIV = 18                                                    1121
         AG_NZ = 2                                                        1121
         AG_ZI(1)=-SWAM_LEN/2                                             1121
         AG_ZI(2)=+SWAM_LEN/2                                             1121
         AG_RMN(1)=RIN                                                    1121
         AG_RMN(2)=RIN                                                    1121
         AG_RMX(1)=ROU                                                    1121
         AG_RMX(2)=ROU                                                    1121
         AG_TITLE = 'PGON'                                                1121
         AG_EXNAME = 'SHAPE'                                              1121
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1121
         CALL AXSHAPE                                                     1121
C    *                                                                    1121
C    Create SWMB :                                                        1122
C    Call To Local Procedure SWMB                                         1122
         ASSIGN 6370 TO BLOCK_SWMB                                        1122
         GO TO  6360                                                      1122
6370     CONTINUE                                                         1122
C    *                                                                    1122
C    Geant POSITION : SWMB ===> inherited arguments used <===             1123
         CALL AGSRESET                                                    1123
         AG_TITLE = 'SWMB'                                                1123
         AG_EXNAME = 'POSITION'                                           1123
         AG_PARLIST = 'NONE'                                              1123
         CALL AXPOSITION                                                  1123
C    *                                                                    1123
C    Create SWMT :                                                        1124
C    Call To Local Procedure SWMT                                         1124
         ASSIGN 6390 TO BLOCK_SWMT                                        1124
         GO TO  6380                                                      1124
6390     CONTINUE                                                         1124
C    *                                                                    1124
C    Geant POSITION : SWMT ===> inherited arguments used <===             1125
         CALL AGSRESET                                                    1125
         AG_TITLE = 'SWMT'                                                1125
         AG_EXNAME = 'POSITION'                                           1125
         AG_PARLIST = 'NONE'                                              1125
         CALL AXPOSITION                                                  1125
C    *                                                                    1125
C    Create SWMS :                                                        1126
C    Call To Local Procedure SWMS                                         1126
         ASSIGN 6410 TO BLOCK_SWMS                                        1126
         GO TO  6400                                                      1126
6410     CONTINUE                                                         1126
C    *                                                                    1126
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         1127
         CALL AGSRESET                                                    1127
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                1127
         AG_TITLE = 'SWMS'                                                1127
         AG_EXNAME = 'POSITION'                                           1127
         AG_PARLIST = 'Z'                                                 1127
         CALL AXPOSITION                                                  1127
C    *                                                                    1127
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          1127
         CALL AGSRESET                                                    1128
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 1128
         AG_TITLE = 'SWMS'                                                1128
         AG_EXNAME = 'POSITION'                                           1128
         AG_PARLIST = 'Z'                                                 1128
         CALL AXPOSITION                                                  1128
C    *                                                                    1128
C    Create SWMW :                                                        1129
C    Call To Local Procedure SWMW                                         1129
         ASSIGN 6430 TO BLOCK_SWMW                                        1129
         GO TO  6420                                                      1129
6430     CONTINUE                                                         1129
C    *                                                                    1129
C    Geant POSITION : SWMW ===> inherited arguments used <===             1130
         CALL AGSRESET                                                    1130
         AG_TITLE = 'SWMW'                                                1130
         AG_EXNAME = 'POSITION'                                           1130
         AG_PARLIST = 'NONE'                                              1130
         CALL AXPOSITION                                                  1130
C    *                                                                    1130
C    Create SOTB :                                                        1131
C    Call To Local Procedure SOTB                                         1131
         ASSIGN 6450 TO BLOCK_SOTB                                        1131
         GO TO  6440                                                      1131
6450     CONTINUE                                                         1131
C    *                                                                    1131
C    Geant POSITION : SOTB ===> inherited arguments used <===             1132
         CALL AGSRESET                                                    1132
         AG_TITLE = 'SOTB'                                                1132
         AG_EXNAME = 'POSITION'                                           1132
         AG_PARLIST = 'NONE'                                              1132
         CALL AXPOSITION                                                  1132
C    *                                                                    1132
C    Create SITB :                                                        1133
C    Call To Local Procedure SITB                                         1133
         ASSIGN 6470 TO BLOCK_SITB                                        1133
         GO TO  6460                                                      1133
6470     CONTINUE                                                         1133
C    *                                                                    1133
C    Geant POSITION : SITB ===> inherited arguments used <===             1134
         CALL AGSRESET                                                    1134
         AG_TITLE = 'SITB'                                                1134
         AG_EXNAME = 'POSITION'                                           1134
         AG_PARLIST = 'NONE'                                              1134
         CALL AXPOSITION                                                  1134
C    *                                                                    1134
5171  CALL AGSPOPB                                                        1135
      GO TO BLOCK_SWMM, (5170 ,5180)                                      1135
C End of Local Procedure                                                  1135
C ----------------------------------------------------------------------  1136
C ------------ SWMB                                                       1136
C Begin Local Procedure SWMB                                              1137
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       1137
6360  CONTINUE                                                            1137
         AG_TITLE='SWMB'                                                  1137
         AG_MODULE=MODULE                                                 1137
         AG_MARK='V00'                                                    1137
         CALL AGSPUSH                                                     1137
         AG_MODULE=MODULE                                                 1137
C    *                                                                    1137
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1138
         CALL AGSRESET                                                    1138
         AG_TITLE = 'ALUMINIUM'                                           1138
         AG_EXNAME = 'MATERIAL'                                           1138
         AG_PARLIST = 'NONE'                                              1138
         CALL AXMATERIAL                                                  1138
C    *                                                                    1138
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       1138
         CALL AGSRESET                                                    1139
         AG_SEEN = 1                                                      1139
         AG_COLO = 2                                                      1139
         AG_TITLE = 'SWMB'                                                1139
         AG_EXNAME = 'ATTRIBUTE'                                          1139
         AG_PARLIST = 'SEEN_COLO'                                         1139
         CALL AXATTRIBUTE                                                 1139
C    *                                                                    1139
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1139
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  1139
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          1139
         CALL AGSRESET                                                    1143
         AG_PHI1 = 0                                                      1143
         AG_DPHI = 360                                                    1143
         AG_NPDIV = 18                                                    1143
         AG_NZ = 2                                                        1143
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1143
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1143
         AG_RMN(1)=SWAM_RMIN                                              1143
         AG_RMN(2)=SWAM_RMIN                                              1143
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 1143
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 1143
         AG_TITLE = 'PGON'                                                1143
         AG_EXNAME = 'SHAPE'                                              1143
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1143
         CALL AXSHAPE                                                     1143
C    *                                                                    1143
6361  CALL AGSPOPB                                                        1144
      GO TO BLOCK_SWMB, (6360 ,6370)                                      1144
C End of Local Procedure                                                  1144
C ----------------------------------------------------------------------  1145
C ------------ SWMT                                                       1145
C Begin Local Procedure SWMT                                              1146
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            1146
6380  CONTINUE                                                            1146
         AG_TITLE='SWMT'                                                  1146
         AG_MODULE=MODULE                                                 1146
         AG_MARK='V00'                                                    1146
         CALL AGSPUSH                                                     1146
         AG_MODULE=MODULE                                                 1146
C    *                                                                    1146
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1147
         CALL AGSRESET                                                    1147
         AG_TITLE = 'ALUMINIUM'                                           1147
         AG_EXNAME = 'MATERIAL'                                           1147
         AG_PARLIST = 'NONE'                                              1147
         CALL AXMATERIAL                                                  1147
C    *                                                                    1147
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       1147
         CALL AGSRESET                                                    1148
         AG_SEEN = 1                                                      1148
         AG_COLO = 2                                                      1148
         AG_TITLE = 'SWMT'                                                1148
         AG_EXNAME = 'ATTRIBUTE'                                          1148
         AG_PARLIST = 'SEEN_COLO'                                         1148
         CALL AXATTRIBUTE                                                 1148
C    *                                                                    1148
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1148
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  1148
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          1148
         CALL AGSRESET                                                    1152
         AG_PHI1 = 0                                                      1152
         AG_DPHI = 360                                                    1152
         AG_NPDIV = 18                                                    1152
         AG_NZ = 2                                                        1152
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1152
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1152
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 1152
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 1152
         AG_RMX(1)=SWAM_RMAX                                              1152
         AG_RMX(2)=SWAM_RMAX                                              1152
         AG_TITLE = 'PGON'                                                1152
         AG_EXNAME = 'SHAPE'                                              1152
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1152
         CALL AXSHAPE                                                     1152
C    *                                                                    1152
6381  CALL AGSPOPB                                                        1153
      GO TO BLOCK_SWMT, (6380 ,6390)                                      1153
C End of Local Procedure                                                  1153
C ----------------------------------------------------------------------  1154
C ------------ SWMS                                                       1154
C Begin Local Procedure SWMS                                              1155
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  1155
6400  CONTINUE                                                            1155
         AG_TITLE='SWMS'                                                  1155
         AG_MODULE=MODULE                                                 1155
         AG_MARK='V00'                                                    1155
         CALL AGSPUSH                                                     1155
         AG_MODULE=MODULE                                                 1155
C    *                                                                    1155
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1156
         CALL AGSRESET                                                    1156
         AG_TITLE = 'ALUMINIUM'                                           1156
         AG_EXNAME = 'MATERIAL'                                           1156
         AG_PARLIST = 'NONE'                                              1156
         CALL AXMATERIAL                                                  1156
C    *                                                                    1156
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       1156
         CALL AGSRESET                                                    1157
         AG_SEEN = 1                                                      1157
         AG_COLO = 2                                                      1157
         AG_TITLE = 'SWMS'                                                1157
         AG_EXNAME = 'ATTRIBUTE'                                          1157
         AG_PARLIST = 'SEEN_COLO'                                         1157
         CALL AXATTRIBUTE                                                 1157
C    *                                                                    1157
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  1157
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  1157
C    WAM_RMAX} <===                                                       1157
         CALL AGSRESET                                                    1160
         AG_PHI1 = 0                                                      1160
         AG_DPHI = 360                                                    1160
         AG_NPDIV = 18                                                    1160
         AG_NZ = 2                                                        1160
         AG_ZI(1)=-SWAM_WALLTHK/2                                         1160
         AG_ZI(2)=SWAM_WALLTHK/2                                          1160
         AG_RMN(1)=SWAM_RMIN                                              1160
         AG_RMN(2)=SWAM_RMIN                                              1160
         AG_RMX(1)=SWAM_RMAX                                              1160
         AG_RMX(2)=SWAM_RMAX                                              1160
         AG_TITLE = 'PGON'                                                1160
         AG_EXNAME = 'SHAPE'                                              1160
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1160
         CALL AXSHAPE                                                     1160
C    *                                                                    1160
6401  CALL AGSPOPB                                                        1161
      GO TO BLOCK_SWMS, (6400 ,6410)                                      1161
C End of Local Procedure                                                  1161
C ----------------------------------------------------------------------  1162
C ------------ SWMW                                                       1162
C Begin Local Procedure SWMW                                              1163
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 1163
6420  CONTINUE                                                            1163
         AG_TITLE='SWMW'                                                  1163
         AG_MODULE=MODULE                                                 1163
         AG_MARK='V00'                                                    1163
         CALL AGSPUSH                                                     1163
         AG_MODULE=MODULE                                                 1163
C    *                                                                    1163
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       1164
         CALL AGSRESET                                                    1164
         AG_SEEN = 1                                                      1164
         AG_COLO = 6                                                      1164
         AG_TITLE = 'SWMW'                                                1164
         AG_EXNAME = 'ATTRIBUTE'                                          1164
         AG_PARLIST = 'SEEN_COLO'                                         1164
         CALL AXATTRIBUTE                                                 1164
C    *                                                                    1164
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1164
         CALL AGSRESET                                                    1165
         AG_TITLE = 'WATER'                                               1165
         AG_EXNAME = 'MATERIAL'                                           1165
         AG_PARLIST = 'NONE'                                              1165
         CALL AXMATERIAL                                                  1165
C    *                                                                    1165
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1165
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  1165
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  1165
C    -SWAM_WALLTHK} <===                                                  1165
         CALL AGSRESET                                                    1170
         AG_PHI1 = 0                                                      1170
         AG_DPHI = 360                                                    1170
         AG_NPDIV = 18                                                    1170
         AG_NZ = 2                                                        1170
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1170
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1170
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 1170
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 1170
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 1170
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 1170
         AG_TITLE = 'PGON'                                                1170
         AG_EXNAME = 'SHAPE'                                              1170
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1170
         CALL AXSHAPE                                                     1170
C    *                                                                    1170
6421  CALL AGSPOPB                                                        1171
      GO TO BLOCK_SWMW, (6420 ,6430)                                      1171
C End of Local Procedure                                                  1171
C ----------------------------------------------------------------------  1172
C ------------ SOTB                                                       1172
C Begin Local Procedure SOTB                                              1173
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            1173
6440  CONTINUE                                                            1173
         AG_TITLE='SOTB'                                                  1173
         AG_MODULE=MODULE                                                 1173
         AG_MARK='V00'                                                    1173
         CALL AGSPUSH                                                     1173
         AG_MODULE=MODULE                                                 1173
C    *                                                                    1173
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1174
         CALL AGSRESET                                                    1174
         AG_TITLE = 'G10'                                                 1174
         AG_EXNAME = 'MATERIAL'                                           1174
         AG_PARLIST = 'NONE'                                              1174
         CALL AXMATERIAL                                                  1174
C    *                                                                    1174
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       1174
         CALL AGSRESET                                                    1175
         AG_SEEN = 1                                                      1175
         AG_COLO = 3                                                      1175
         AG_TITLE = 'SOTB'                                                1175
         AG_EXNAME = 'ATTRIBUTE'                                          1175
         AG_PARLIST = 'SEEN_COLO'                                         1175
         CALL AXATTRIBUTE                                                 1175
C    *                                                                    1175
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1175
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  1175
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    1175
         CALL AGSRESET                                                    1178
         AG_PHI1 = 0                                                      1178
         AG_DPHI = 360                                                    1178
         AG_NPDIV = 18                                                    1178
         AG_NZ = 2                                                        1178
         AG_ZI(1)=-SWAM_LEN/2                                             1178
         AG_ZI(2)=+SWAM_LEN/2                                             1178
         AG_RMN(1)=SWAM_RMAX                                              1178
         AG_RMN(2)=SWAM_RMAX                                              1178
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 1178
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 1178
         AG_TITLE = 'PGON'                                                1178
         AG_EXNAME = 'SHAPE'                                              1178
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1178
         CALL AXSHAPE                                                     1178
C    *                                                                    1178
6441  CALL AGSPOPB                                                        1179
      GO TO BLOCK_SOTB, (6440 ,6450)                                      1179
C End of Local Procedure                                                  1179
C ----------------------------------------------------------------------  1180
C ------------ SITB                                                       1180
C Begin Local Procedure SITB                                              1181
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            1181
6460  CONTINUE                                                            1181
         AG_TITLE='SITB'                                                  1181
         AG_MODULE=MODULE                                                 1181
         AG_MARK='V00'                                                    1181
         CALL AGSPUSH                                                     1181
         AG_MODULE=MODULE                                                 1181
C    *                                                                    1181
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1182
         CALL AGSRESET                                                    1182
         AG_A = 12                                                        1182
         AG_Z = 6                                                         1182
         AG_W = 1                                                         1182
         AG_TITLE = 'C'                                                   1182
         AG_EXNAME = 'COMPONENT'                                          1182
         AG_PARLIST = 'A_Z_W'                                             1182
         CALL AXCOMPONENT                                                 1182
C    *                                                                    1182
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1182
         CALL AGSRESET                                                    1183
         AG_A = 1                                                         1183
         AG_Z = 1                                                         1183
         AG_W = 2                                                         1183
         AG_TITLE = 'H2'                                                  1183
         AG_EXNAME = 'COMPONENT'                                          1183
         AG_PARLIST = 'A_Z_W'                                             1183
         CALL AXCOMPONENT                                                 1183
C    *                                                                    1183
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1183
         CALL AGSRESET                                                    1184
         AG_DENS = 0.935                                                  1184
         AG_TITLE = 'CH2'                                                 1184
         AG_EXNAME = 'MIXTURE'                                            1184
         AG_PARLIST = 'DENS'                                              1184
         CALL AXMIXTURE                                                   1184
C    *                                                                    1184
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       1184
         CALL AGSRESET                                                    1185
         AG_SEEN = 1                                                      1185
         AG_COLO = 3                                                      1185
         AG_TITLE = 'SITB'                                                1185
         AG_EXNAME = 'ATTRIBUTE'                                          1185
         AG_PARLIST = 'SEEN_COLO'                                         1185
         CALL AXATTRIBUTE                                                 1185
C    *                                                                    1185
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1185
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  1185
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   1185
         CALL AGSRESET                                                    1189
         AG_PHI1 = 0                                                      1189
         AG_DPHI = 360                                                    1189
         AG_NPDIV = 18                                                    1189
         AG_NZ = 2                                                        1189
         AG_ZI(1)=-SWAM_LEN/2                                             1189
         AG_ZI(2)=+SWAM_LEN/2                                             1189
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 1189
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 1189
         AG_RMX(1)=SWAM_RMIN                                              1189
         AG_RMX(2)=SWAM_RMIN                                              1189
         AG_TITLE = 'PGON'                                                1189
         AG_EXNAME = 'SHAPE'                                              1189
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1189
         CALL AXSHAPE                                                     1189
C    *                                                                    1189
6461  CALL AGSPOPB                                                        1190
      GO TO BLOCK_SITB, (6460 ,6470)                                      1190
C End of Local Procedure                                                  1190
C ----------------------------------------------------------------------  1191
C ------------ SBWC                                                       1191
C Begin Local Procedure SBWC                                              1192
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           1192
5190  CONTINUE                                                            1192
         AG_TITLE='SBWC'                                                  1192
         AG_MODULE=MODULE                                                 1192
         AG_MARK='V00'                                                    1192
         CALL AGSPUSH                                                     1192
         AG_MODULE=MODULE                                                 1192
C    *                                                                    1192
         RIN=SWAM_RMIN                                                    1193
         ROU=SSUP_CON1IDMN                                                1194
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1195
         CALL AGSRESET                                                    1195
         AG_TITLE = 'AIR'                                                 1195
         AG_EXNAME = 'MATERIAL'                                           1195
         AG_PARLIST = 'NONE'                                              1195
         CALL AXMATERIAL                                                  1195
C    *                                                                    1195
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       1195
         CALL AGSRESET                                                    1196
         AG_SEEN = 0                                                      1196
         AG_COLO = 1                                                      1196
         AG_TITLE = 'SBWC'                                                1196
         AG_EXNAME = 'ATTRIBUTE'                                          1196
         AG_PARLIST = 'SEEN_COLO'                                         1196
         CALL AXATTRIBUTE                                                 1196
C    *                                                                    1196
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  1196
C    MIN+SWAM_LEN))/2 <===                                                1196
         CALL AGSRESET                                                    1197
         AG_RMIN = RIN                                                    1197
         AG_RMAX = ROU                                                    1197
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   1197
         AG_TITLE = 'TUBE'                                                1197
         AG_EXNAME = 'SHAPE'                                              1197
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1197
         CALL AXSHAPE                                                     1197
C    *                                                                    1197
C    Create SWCM :                                                        1198
C    Call To Local Procedure SWCM                                         1198
         ASSIGN 6490 TO BLOCK_SWCM                                        1198
         GO TO  6480                                                      1198
6490     CONTINUE                                                         1198
C    *                                                                    1198
5191  CALL AGSPOPB                                                        1199
      GO TO BLOCK_SBWC, (5190 ,5200)                                      1199
C End of Local Procedure                                                  1199
C ----------------------------------------------------------------------  1200
C ------------ SWCM                                                       1200
C Begin Local Procedure SWCM                                              1201
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   1201
6480  CONTINUE                                                            1201
         AG_TITLE='SWCM'                                                  1201
         AG_MODULE=MODULE                                                 1201
         AG_MARK='V00'                                                    1201
         CALL AGSPUSH                                                     1201
         AG_MODULE=MODULE                                                 1201
C    *                                                                    1201
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       1202
         CALL AGSRESET                                                    1202
         AG_SEEN = 0                                                      1202
         AG_COLO = 1                                                      1202
         AG_TITLE = 'SWCM'                                                1202
         AG_EXNAME = 'ATTRIBUTE'                                          1202
         AG_PARLIST = 'SEEN_COLO'                                         1202
         CALL AXATTRIBUTE                                                 1202
C    *                                                                    1202
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 1202
         CALL AGSRESET                                                    1203
         AG_NDIV = 3                                                      1203
         AG_IAXIS = 2                                                     1203
         AG_C0 = 0                                                        1203
         AG_TITLE = 'DIVISION'                                            1203
         AG_EXNAME = 'SHAPE'                                              1203
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1203
         CALL AXSHAPE                                                     1203
C    *                                                                    1203
C    Create SXAI :                                                        1204
C    Call To Local Procedure SXAI                                         1204
         ASSIGN 6510 TO BLOCK_SXAI                                        1204
         GO TO  6500                                                      1204
6510     CONTINUE                                                         1204
C    *                                                                    1204
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  1205
C    /2+SSUP_BRATHK/2) <===                                               1205
         CALL AGSRESET                                                    1205
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   1205
         AG_TITLE = 'SXAI'                                                1205
         AG_EXNAME = 'POSITION'                                           1205
         AG_PARLIST = 'Z'                                                 1205
         CALL AXPOSITION                                                  1205
C    *                                                                    1205
C    Create SXBI :                                                        1206
C    Call To Local Procedure SXBI                                         1206
         ASSIGN 6530 TO BLOCK_SXBI                                        1206
         GO TO  6520                                                      1206
6530     CONTINUE                                                         1206
C    *                                                                    1206
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    1207
         CALL AGSRESET                                                    1207
         AG_Z = (SSUP_BRATHK/2)                                           1207
         AG_TITLE = 'SXBI'                                                1207
         AG_EXNAME = 'POSITION'                                           1207
         AG_PARLIST = 'Z'                                                 1207
         CALL AXPOSITION                                                  1207
C    *                                                                    1207
6481  CALL AGSPOPB                                                        1208
      GO TO BLOCK_SWCM, (6480 ,6490)                                      1208
C End of Local Procedure                                                  1208
C ----------------------------------------------------------------------  1209
C ------------ SXAI                                                       1209
C Begin Local Procedure SXAI                                              1210
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      1210
6500  CONTINUE                                                            1210
         AG_TITLE='SXAI'                                                  1210
         AG_MODULE=MODULE                                                 1210
         AG_MARK='V00'                                                    1210
         CALL AGSPUSH                                                     1210
         AG_MODULE=MODULE                                                 1210
C    *                                                                    1210
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       1211
         CALL AGSRESET                                                    1211
         AG_SEEN = 1                                                      1211
         AG_COLO = 2                                                      1211
         AG_TITLE = 'SXAI'                                                1211
         AG_EXNAME = 'ATTRIBUTE'                                          1211
         AG_PARLIST = 'SEEN_COLO'                                         1211
         CALL AXATTRIBUTE                                                 1211
C    *                                                                    1211
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1211
         CALL AGSRESET                                                    1212
         AG_TITLE = 'ALUMINIUM'                                           1212
         AG_EXNAME = 'MATERIAL'                                           1212
         AG_PARLIST = 'NONE'                                              1212
         CALL AXMATERIAL                                                  1212
C    *                                                                    1212
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   1212
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         1212
         CALL AGSRESET                                                    1214
         AG_RMIN = SWAM_RMIN                                              1214
         AG_RMAX = SSUP_CON1IDMN                                          1214
         AG_PHI1 = -5                                                     1214
         AG_PHI2 = 5                                                      1214
         AG_DZ = SSUP_BRATHK/2                                            1214
         AG_TITLE = 'TUBS'                                                1214
         AG_EXNAME = 'SHAPE'                                              1214
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1214
         CALL AXSHAPE                                                     1214
C    *                                                                    1214
6501  CALL AGSPOPB                                                        1215
      GO TO BLOCK_SXAI, (6500 ,6510)                                      1215
C End of Local Procedure                                                  1215
C ----------------------------------------------------------------------  1216
C ------------ SXBI                                                       1216
C Begin Local Procedure SXBI                                              1217
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     1217
6520  CONTINUE                                                            1217
         AG_TITLE='SXBI'                                                  1217
         AG_MODULE=MODULE                                                 1217
         AG_MARK='V00'                                                    1217
         CALL AGSPUSH                                                     1217
         AG_MODULE=MODULE                                                 1217
C    *                                                                    1217
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       1218
         CALL AGSRESET                                                    1218
         AG_SEEN = 1                                                      1218
         AG_COLO = 2                                                      1218
         AG_TITLE = 'SXBI'                                                1218
         AG_EXNAME = 'ATTRIBUTE'                                          1218
         AG_PARLIST = 'SEEN_COLO'                                         1218
         CALL AXATTRIBUTE                                                 1218
C    *                                                                    1218
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1218
         CALL AGSRESET                                                    1219
         AG_TITLE = 'ALUMINIUM'                                           1219
         AG_EXNAME = 'MATERIAL'                                           1219
         AG_PARLIST = 'NONE'                                              1219
         CALL AXMATERIAL                                                  1219
C    *                                                                    1219
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  1219
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  1219
C    _BRATHK)/2) <===                                                     1219
         CALL AGSRESET                                                    1221
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              1221
         AG_RMAX = SSUP_CON1IDMN                                          1221
         AG_PHI1 = -5                                                     1221
         AG_PHI2 = 5                                                      1221
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     1221
         AG_TITLE = 'TUBS'                                                1221
         AG_EXNAME = 'SHAPE'                                              1221
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1221
         CALL AXSHAPE                                                     1221
C    *                                                                    1221
6521  CALL AGSPOPB                                                        1222
      GO TO BLOCK_SXBI, (6520 ,6530)                                      1222
C End of Local Procedure                                                  1222
C ----------------------------------------------------------------------  1223
C ------------ SCRW                                                       1223
C Begin Local Procedure SCRW                                              1224
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   1224
6040  CONTINUE                                                            1224
         AG_TITLE='SCRW'                                                  1224
         AG_MODULE=MODULE                                                 1224
         AG_MARK='V00'                                                    1224
         CALL AGSPUSH                                                     1224
         AG_MODULE=MODULE                                                 1224
C    *                                                                    1224
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        1225
         CALL AGSRESET                                                    1225
         AG_TITLE = 'BERILLIUM'                                           1225
         AG_EXNAME = 'MATERIAL'                                           1225
         AG_PARLIST = 'NONE'                                              1225
         CALL AXMATERIAL                                                  1225
C    *                                                                    1225
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       1225
         CALL AGSRESET                                                    1226
         AG_SEEN = 1                                                      1226
         AG_COLO = 2                                                      1226
         AG_TITLE = 'SCRW'                                                1226
         AG_EXNAME = 'ATTRIBUTE'                                          1226
         AG_PARLIST = 'SEEN_COLO'                                         1226
         CALL AXATTRIBUTE                                                 1226
C    *                                                                    1226
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  1226
C    NDRNG_Z) <===                                                        1226
         CALL AGSRESET                                                    1227
         AG_RMIN = 0                                                      1227
         AG_RMAX = SSUP_ERJDIA/2                                          1227
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   1227
         AG_TITLE = 'TUBE'                                                1227
         AG_EXNAME = 'SHAPE'                                              1227
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1227
         CALL AXSHAPE                                                     1227
C    *                                                                    1227
6041  CALL AGSPOPB                                                        1228
      GO TO BLOCK_SCRW, (6040 ,6050,6080)                                 1228
C End of Local Procedure                                                  1228
C ----------------------------------------------------------------------  1229
C ------------ SBSP                                                       1229
C Begin Local Procedure SBSP                                              1230
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1230
5230  CONTINUE                                                            1230
         AG_TITLE='SBSP'                                                  1230
         AG_MODULE=MODULE                                                 1230
         AG_MARK='V00'                                                    1230
         CALL AGSPUSH                                                     1230
         AG_MODULE=MODULE                                                 1230
C    *                                                                    1230
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1231
         CALL AGSRESET                                                    1231
         AG_TITLE = 'AIR'                                                 1231
         AG_EXNAME = 'MATERIAL'                                           1231
         AG_PARLIST = 'NONE'                                              1231
         CALL AXMATERIAL                                                  1231
C    *                                                                    1231
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1231
         CALL AGSRESET                                                    1232
         AG_SEEN = 0                                                      1232
         AG_COLO = 1                                                      1232
         AG_TITLE = 'SBSP'                                                1232
         AG_EXNAME = 'ATTRIBUTE'                                          1232
         AG_PARLIST = 'SEEN_COLO'                                         1232
         CALL AXATTRIBUTE                                                 1232
C    *                                                                    1232
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1232
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1232
         CALL AGSRESET                                                    1234
         AG_RMIN = SVTG_RSIZEMIN                                          1234
         AG_RMAX = SSUB_KMOUNTOD/2                                        1234
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1234
         AG_TITLE = 'TUBE'                                                1234
         AG_EXNAME = 'SHAPE'                                              1234
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1234
         CALL AXSHAPE                                                     1234
C    *                                                                    1234
C    Create SAKM :                                                        1235
C    Call To Local Procedure SAKM                                         1235
         ASSIGN 6550 TO BLOCK_SAKM                                        1235
         GO TO  6540                                                      1235
6550     CONTINUE                                                         1235
C    *                                                                    1235
C    Geant POSITION : SAKM ===> inherited arguments used <===             1236
         CALL AGSRESET                                                    1236
         AG_TITLE = 'SAKM'                                                1236
         AG_EXNAME = 'POSITION'                                           1236
         AG_PARLIST = 'NONE'                                              1236
         CALL AXPOSITION                                                  1236
C    *                                                                    1236
C    Create SBMM :                                                        1237
C    Call To Local Procedure SBMM                                         1237
         ASSIGN 6570 TO BLOCK_SBMM                                        1237
         GO TO  6560                                                      1237
6570     CONTINUE                                                         1237
C    *                                                                    1237
C    Create SBMM :                                                        1238
C    Call To Local Procedure SBMM                                         1238
         ASSIGN 6580 TO BLOCK_SBMM                                        1238
         GO TO  6560                                                      1238
6580     CONTINUE                                                         1238
C    *                                                                    1238
C    Create SBRL :                                                        1239
C    Call To Local Procedure SBRL                                         1239
         ASSIGN 6600 TO BLOCK_SBRL                                        1239
         GO TO  6590                                                      1239
6600     CONTINUE                                                         1239
C    *                                                                    1239
C    Create SBRX :                                                        1240
C    Call To Local Procedure SBRX                                         1240
         ASSIGN 6620 TO BLOCK_SBRX                                        1240
         GO TO  6610                                                      1240
6620     CONTINUE                                                         1240
C    *                                                                    1240
C    *                                                                    1241
         I=-1                                                             1241
            GOTO 6633                                                     1241
6631        I=I+(2)                                                       1241
6633        IF((2)*(I-(1)).GT.0) GO TO 6632                               1241
C       *                                                                 1242
            J=0                                                           1242
               GO TO 6643                                                 1242
6641           J=J+1                                                      1242
6643           IF(J-(1).GT.0) GO TO 6642                                  1242
               PHI=I*SSUB_SRCUTPHI+180*J                                  1243
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1244
     *         2.0)                                                       1244
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1246
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1246
C          ==                                                             1246
               CALL AGSRESET                                              1246
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1246
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1246
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1246
               AG_ALPHAN=AG_ALPHAN+1                                      1246
               AG_ALPHAK(AG_ALPHAN)=3                                     1246
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1246
               AG_TITLE = 'SBMM'                                          1246
               AG_EXNAME = 'POSITION'                                     1246
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1246
               CALL AXPOSITION                                            1246
C          *                                                              1246
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1246
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1249
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1249
C           ORT=ZXY <===                                                  1249
               CALL AGSRESET                                              1249
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1249
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1249
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1249
               AG_ALPHAN=AG_ALPHAN+1                                      1249
               AG_ALPHAK(AG_ALPHAN)=3                                     1249
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1249
               AG_ALPHAN=AG_ALPHAN+1                                      1249
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1249
               AG_TITLE = 'SBRL'                                          1249
               AG_EXNAME = 'POSITION'                                     1249
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1249
               CALL AXPOSITION                                            1249
C          *                                                              1249
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1249
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1249
C           ORT=ZXY <===                                                  1249
               CALL AGSRESET                                              1251
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1251
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1251
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1251
               AG_ALPHAN=AG_ALPHAN+1                                      1251
               AG_ALPHAK(AG_ALPHAN)=3                                     1251
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1251
               AG_ALPHAN=AG_ALPHAN+1                                      1251
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1251
               AG_TITLE = 'SBRX'                                          1251
               AG_EXNAME = 'POSITION'                                     1251
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1251
               CALL AXPOSITION                                            1251
C          *                                                              1251
            GO TO 6641                                                    1252
6642        CONTINUE                                                      1252
         GO TO 6631                                                       1253
6632     CONTINUE                                                         1253
C    Create SBSR :                                                        1254
C    Call To Local Procedure SBSR                                         1254
         ASSIGN 6660 TO BLOCK_SBSR                                        1254
         GO TO  6650                                                      1254
6660     CONTINUE                                                         1254
C    *                                                                    1254
C    Geant POSITION : SBSR ===> inherited arguments used <===             1255
         CALL AGSRESET                                                    1255
         AG_TITLE = 'SBSR'                                                1255
         AG_EXNAME = 'POSITION'                                           1255
         AG_PARLIST = 'NONE'                                              1255
         CALL AXPOSITION                                                  1255
C    *                                                                    1255
5231  CALL AGSPOPB                                                        1256
      GO TO BLOCK_SBSP, (5230 ,5240)                                      1256
C End of Local Procedure                                                  1256
C ----------------------------------------------------------------------  1257
C ------------ SAKM                                                       1257
C Begin Local Procedure SAKM                                              1258
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1258
6540  CONTINUE                                                            1258
         AG_TITLE='SAKM'                                                  1258
         AG_MODULE=MODULE                                                 1258
         AG_MARK='V00'                                                    1258
         CALL AGSPUSH                                                     1258
         AG_MODULE=MODULE                                                 1258
C    *                                                                    1258
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1259
         CALL AGSRESET                                                    1259
         AG_TITLE = 'ALUMINIUM'                                           1259
         AG_EXNAME = 'MATERIAL'                                           1259
         AG_PARLIST = 'NONE'                                              1259
         CALL AXMATERIAL                                                  1259
C    *                                                                    1259
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1259
         CALL AGSRESET                                                    1260
         AG_SEEN = 1                                                      1260
         AG_COLO = 2                                                      1260
         AG_TITLE = 'SAKM'                                                1260
         AG_EXNAME = 'ATTRIBUTE'                                          1260
         AG_PARLIST = 'SEEN_COLO'                                         1260
         CALL AXATTRIBUTE                                                 1260
C    *                                                                    1260
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1260
C    DZ=SSUB_KMNTTHK/2 <===                                               1260
         CALL AGSRESET                                                    1262
         AG_RMIN = SSUB_KMOUNTID/2                                        1262
         AG_RMAX = SSUB_KMOUNTOD/2                                        1262
         AG_DZ = SSUB_KMNTTHK/2                                           1262
         AG_TITLE = 'TUBE'                                                1262
         AG_EXNAME = 'SHAPE'                                              1262
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1262
         CALL AXSHAPE                                                     1262
C    *                                                                    1262
C    Create SCKM :                                                        1263
C    Call To Local Procedure SCKM                                         1263
         ASSIGN 6680 TO BLOCK_SCKM                                        1263
         GO TO  6670                                                      1263
6680     CONTINUE                                                         1263
C    *                                                                    1263
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1264
         CALL AGSRESET                                                    1264
         AG_Y = SSUB_KMCUTOFF                                             1264
         AG_KONLY = 'MANY'                                                1264
         AG_TITLE = 'SCKM'                                                1264
         AG_EXNAME = 'POSITION'                                           1264
         AG_PARLIST = 'Y_KONLY'                                           1264
         CALL AXPOSITION                                                  1264
C    *                                                                    1264
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1264
C     <===                                                                1264
         CALL AGSRESET                                                    1265
         AG_Y = -SSUB_KMCUTOFF                                            1265
         AG_ALPHAN=AG_ALPHAN+1                                            1265
         AG_ALPHAK(AG_ALPHAN)=3                                           1265
         AG_ALPHAL(AG_ALPHAN)=180                                         1265
         AG_KONLY = 'MANY'                                                1265
         AG_TITLE = 'SCKM'                                                1265
         AG_EXNAME = 'POSITION'                                           1265
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1265
         CALL AXPOSITION                                                  1265
C    *                                                                    1265
6541  CALL AGSPOPB                                                        1266
      GO TO BLOCK_SAKM, (6540 ,6550)                                      1266
C End of Local Procedure                                                  1266
C ----------------------------------------------------------------------  1267
C ------------ SCKM                                                       1267
C Begin Local Procedure SCKM                                              1268
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1268
6670  CONTINUE                                                            1268
         AG_TITLE='SCKM'                                                  1268
         AG_MODULE=MODULE                                                 1268
         AG_MARK='V00'                                                    1268
         CALL AGSPUSH                                                     1268
         AG_MODULE=MODULE                                                 1268
C    *                                                                    1268
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1269
         CALL AGSRESET                                                    1269
         AG_TITLE = 'AIR'                                                 1269
         AG_EXNAME = 'MATERIAL'                                           1269
         AG_PARLIST = 'NONE'                                              1269
         CALL AXMATERIAL                                                  1269
C    *                                                                    1269
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1269
         CALL AGSRESET                                                    1270
         AG_SEEN = 1                                                      1270
         AG_COLO = 1                                                      1270
         AG_TITLE = 'SCKM'                                                1270
         AG_EXNAME = 'ATTRIBUTE'                                          1270
         AG_PARLIST = 'SEEN_COLO'                                         1270
         CALL AXATTRIBUTE                                                 1270
C    *                                                                    1270
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1270
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1270
         CALL AGSRESET                                                    1273
         AG_RMIN = SSUB_KMCUTID/2                                         1273
         AG_RMAX = SSUB_KMCUTOD/2                                         1273
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1273
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1273
         AG_DZ = SSUB_KMNTTHK/2                                           1273
         AG_TITLE = 'TUBS'                                                1273
         AG_EXNAME = 'SHAPE'                                              1273
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1273
         CALL AXSHAPE                                                     1273
C    *                                                                    1273
6671  CALL AGSPOPB                                                        1274
      GO TO BLOCK_SCKM, (6670 ,6680)                                      1274
C End of Local Procedure                                                  1274
C ----------------------------------------------------------------------  1275
C ------------ SBSR                                                       1275
C Begin Local Procedure SBSR                                              1276
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1276
6650  CONTINUE                                                            1276
         AG_TITLE='SBSR'                                                  1276
         AG_MODULE=MODULE                                                 1276
         AG_MARK='V00'                                                    1276
         CALL AGSPUSH                                                     1276
         AG_MODULE=MODULE                                                 1276
C    *                                                                    1276
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1277
         CALL AGSRESET                                                    1277
         AG_TITLE = 'G10'                                                 1277
         AG_EXNAME = 'MATERIAL'                                           1277
         AG_PARLIST = 'NONE'                                              1277
         CALL AXMATERIAL                                                  1277
C    *                                                                    1277
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1277
         CALL AGSRESET                                                    1278
         AG_SEEN = 1                                                      1278
         AG_COLO = 3                                                      1278
         AG_TITLE = 'SBSR'                                                1278
         AG_EXNAME = 'ATTRIBUTE'                                          1278
         AG_PARLIST = 'SEEN_COLO'                                         1278
         CALL AXATTRIBUTE                                                 1278
C    *                                                                    1278
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1278
C    =SSUB_SRINGTHK/2 <===                                                1278
         CALL AGSRESET                                                    1280
         AG_RMIN = SSUB_SRINGID/2                                         1280
         AG_RMAX = SSUB_SRINGOD/2                                         1280
         AG_DZ = SSUB_SRINGTHK/2                                          1280
         AG_TITLE = 'TUBE'                                                1280
         AG_EXNAME = 'SHAPE'                                              1280
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1280
         CALL AXSHAPE                                                     1280
C    *                                                                    1280
C    Create SBCR :                                                        1281
C    Call To Local Procedure SBCR                                         1281
         ASSIGN 6700 TO BLOCK_SBCR                                        1281
         GO TO  6690                                                      1281
6700     CONTINUE                                                         1281
C    *                                                                    1281
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1282
C    *                                                                    1283
         I=-1                                                             1283
            GOTO 6713                                                     1283
6711        I=I+(2)                                                       1283
6713        IF((2)*(I-(1)).GT.0) GO TO 6712                               1283
C       *                                                                 1284
            J=0                                                           1284
               GO TO 6723                                                 1284
6721           J=J+1                                                      1284
6723           IF(J-(1).GT.0) GO TO 6722                                  1284
               PHI=I*SSUB_SRCUTPHI+180*J                                  1285
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1287
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1287
               CALL AGSRESET                                              1287
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1287
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1287
               AG_ALPHAN=AG_ALPHAN+1                                      1287
               AG_ALPHAK(AG_ALPHAN)=3                                     1287
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1287
               AG_KONLY = 'MANY'                                          1287
               AG_TITLE = 'SBCR'                                          1287
               AG_EXNAME = 'POSITION'                                     1287
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1287
               CALL AXPOSITION                                            1287
C          *                                                              1287
            GO TO 6721                                                    1288
6722        CONTINUE                                                      1288
         GO TO 6711                                                       1289
6712     CONTINUE                                                         1289
6651  CALL AGSPOPB                                                        1290
      GO TO BLOCK_SBSR, (6650 ,6660)                                      1290
C End of Local Procedure                                                  1290
C ----------------------------------------------------------------------  1291
C ------------ SBCR                                                       1291
C Begin Local Procedure SBCR                                              1292
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1292
6690  CONTINUE                                                            1292
         AG_TITLE='SBCR'                                                  1292
         AG_MODULE=MODULE                                                 1292
         AG_MARK='V00'                                                    1292
         CALL AGSPUSH                                                     1292
         AG_MODULE=MODULE                                                 1292
C    *                                                                    1292
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1293
         CALL AGSRESET                                                    1293
         AG_TITLE = 'AIR'                                                 1293
         AG_EXNAME = 'MATERIAL'                                           1293
         AG_PARLIST = 'NONE'                                              1293
         CALL AXMATERIAL                                                  1293
C    *                                                                    1293
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1293
         CALL AGSRESET                                                    1294
         AG_SEEN = 1                                                      1294
         AG_COLO = 1                                                      1294
         AG_TITLE = 'SBCR'                                                1294
         AG_EXNAME = 'ATTRIBUTE'                                          1294
         AG_PARLIST = 'SEEN_COLO'                                         1294
         CALL AXATTRIBUTE                                                 1294
C    *                                                                    1294
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1294
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1294
         CALL AGSRESET                                                    1296
         AG_DX = SSUB_SRCUTWID/2                                          1296
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1296
         AG_DZ = SSUB_SRINGTHK/2                                          1296
         AG_TITLE = 'BOX'                                                 1296
         AG_EXNAME = 'SHAPE'                                              1296
         AG_PARLIST = 'DX_DY_DZ'                                          1296
         CALL AXSHAPE                                                     1296
C    *                                                                    1296
6691  CALL AGSPOPB                                                        1297
      GO TO BLOCK_SBCR, (6690 ,6700)                                      1297
C End of Local Procedure                                                  1297
C ----------------------------------------------------------------------  1298
C ------------ SBRL                                                       1298
C Begin Local Procedure SBRL                                              1299
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1299
6590  CONTINUE                                                            1299
         AG_TITLE='SBRL'                                                  1299
         AG_MODULE=MODULE                                                 1299
         AG_MARK='V00'                                                    1299
         CALL AGSPUSH                                                     1299
         AG_MODULE=MODULE                                                 1299
C    *                                                                    1299
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1300
C    ADL=12.6 <===                                                        1300
         CALL AGSRESET                                                    1300
         AG_A = 20.719                                                    1300
         AG_Z = 10.307                                                    1300
         AG_DENS = 2.23                                                   1300
         AG_ABSL = 50.7                                                   1300
         AG_RADL = 12.6                                                   1300
         AG_TITLE = 'PYREX'                                               1300
         AG_EXNAME = 'MATERIAL'                                           1300
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1300
         CALL AXMATERIAL                                                  1300
C    *                                                                    1300
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1300
         CALL AGSRESET                                                    1301
         AG_SEEN = 1                                                      1301
         AG_COLO = 6                                                      1301
         AG_TITLE = 'SBRL'                                                1301
         AG_EXNAME = 'ATTRIBUTE'                                          1301
         AG_PARLIST = 'SEEN_COLO'                                         1301
         CALL AXATTRIBUTE                                                 1301
C    *                                                                    1301
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1301
C    =                                                                    1301
         CALL AGSRESET                                                    1302
         AG_RMIN = SSUB_SROLLID/2                                         1302
         AG_RMAX = SSUB_SROLLOD/2                                         1302
         AG_TITLE = 'TUBE'                                                1302
         AG_EXNAME = 'SHAPE'                                              1302
         AG_PARLIST = 'RMIN_RMAX'                                         1302
         CALL AXSHAPE                                                     1302
C    *                                                                    1302
6591  CALL AGSPOPB                                                        1303
      GO TO BLOCK_SBRL, (6590 ,6600)                                      1303
C End of Local Procedure                                                  1303
C ----------------------------------------------------------------------  1304
C ------------ SBRX                                                       1304
C Begin Local Procedure SBRX                                              1305
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1305
6610  CONTINUE                                                            1305
         AG_TITLE='SBRX'                                                  1305
         AG_MODULE=MODULE                                                 1305
         AG_MARK='V00'                                                    1305
         CALL AGSPUSH                                                     1305
         AG_MODULE=MODULE                                                 1305
C    *                                                                    1305
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1306
         CALL AGSRESET                                                    1306
         AG_TITLE = 'IRON'                                                1306
         AG_EXNAME = 'MATERIAL'                                           1306
         AG_PARLIST = 'NONE'                                              1306
         CALL AXMATERIAL                                                  1306
C    *                                                                    1306
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1306
         CALL AGSRESET                                                    1307
         AG_SEEN = 1                                                      1307
         AG_COLO = 2                                                      1307
         AG_TITLE = 'SBRX'                                                1307
         AG_EXNAME = 'ATTRIBUTE'                                          1307
         AG_PARLIST = 'SEEN_COLO'                                         1307
         CALL AXATTRIBUTE                                                 1307
C    *                                                                    1307
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1307
C    LEN/2 <===                                                           1307
         CALL AGSRESET                                                    1309
         AG_RMIN = 0.0                                                    1309
         AG_RMAX = SSUB_SROLLID/2                                         1309
         AG_DZ = SSUB_SWIRELEN/2                                          1309
         AG_TITLE = 'TUBE'                                                1309
         AG_EXNAME = 'SHAPE'                                              1309
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1309
         CALL AXSHAPE                                                     1309
C    *                                                                    1309
6611  CALL AGSPOPB                                                        1310
      GO TO BLOCK_SBRX, (6610 ,6620)                                      1310
C End of Local Procedure                                                  1310
C ----------------------------------------------------------------------  1311
C ------------ SBMM                                                       1311
C Begin Local Procedure SBMM                                              1312
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1312
6560  CONTINUE                                                            1312
         AG_TITLE='SBMM'                                                  1312
         AG_MODULE=MODULE                                                 1312
         AG_MARK='V00'                                                    1312
         CALL AGSPUSH                                                     1312
         AG_MODULE=MODULE                                                 1312
C    *                                                                    1312
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1313
         CALL AGSRESET                                                    1313
         AG_TITLE = 'AIR'                                                 1313
         AG_EXNAME = 'MATERIAL'                                           1313
         AG_PARLIST = 'NONE'                                              1313
         CALL AXMATERIAL                                                  1313
C    *                                                                    1313
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1313
         CALL AGSRESET                                                    1314
         AG_SEEN = 0                                                      1314
         AG_COLO = 1                                                      1314
         AG_TITLE = 'SBMM'                                                1314
         AG_EXNAME = 'ATTRIBUTE'                                          1314
         AG_PARLIST = 'SEEN_COLO'                                         1314
         CALL AXATTRIBUTE                                                 1314
C    *                                                                    1314
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1314
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1314
         CALL AGSRESET                                                    1316
         AG_DX = SSUB_MBLKIWID/2                                          1316
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1316
         AG_DZ = SSUB_MBLKHGH/2                                           1316
         AG_TITLE = 'BOX'                                                 1316
         AG_EXNAME = 'SHAPE'                                              1316
         AG_PARLIST = 'DX_DY_DZ'                                          1316
         CALL AXSHAPE                                                     1316
C    *                                                                    1316
C    Create SMRD :                                                        1317
C    Call To Local Procedure SMRD                                         1317
         ASSIGN 6740 TO BLOCK_SMRD                                        1317
         GO TO  6730                                                      1317
6740     CONTINUE                                                         1317
C    *                                                                    1317
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1318
     *   SSUB_SROLLOD                                                     1318
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1319
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1319
         CALL AGSRESET                                                    1319
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1319
         AG_ALPHAN=AG_ALPHAN+1                                            1319
         AG_ALPHAK(AG_ALPHAN)=1                                           1319
         AG_ALPHAL(AG_ALPHAN)=90                                          1319
         AG_TITLE = 'SMRD'                                                1319
         AG_EXNAME = 'POSITION'                                           1319
         AG_PARLIST = 'Y_ALPHAX'                                          1319
         CALL AXPOSITION                                                  1319
C    *                                                                    1319
C    Create SBMO :                                                        1320
C    Call To Local Procedure SBMO                                         1320
         ASSIGN 6760 TO BLOCK_SBMO                                        1320
         GO TO  6750                                                      1320
6760     CONTINUE                                                         1320
C    *                                                                    1320
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1321
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1322
C    UB_SROLLOD)/2 <===                                                   1322
         CALL AGSRESET                                                    1322
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1322
         AG_TITLE = 'SBMO'                                                1322
         AG_EXNAME = 'POSITION'                                           1322
         AG_PARLIST = 'Y'                                                 1322
         CALL AXPOSITION                                                  1322
C    *                                                                    1322
C    Create SBMI :                                                        1323
C    Call To Local Procedure SBMI                                         1323
         ASSIGN 6780 TO BLOCK_SBMI                                        1323
         GO TO  6770                                                      1323
6780     CONTINUE                                                         1323
C    *                                                                    1323
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1324
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1325
C    UB_SROLLOD)/2 <===                                                   1325
         CALL AGSRESET                                                    1325
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1325
         AG_TITLE = 'SBMI'                                                1325
         AG_EXNAME = 'POSITION'                                           1325
         AG_PARLIST = 'Y'                                                 1325
         CALL AXPOSITION                                                  1325
C    *                                                                    1325
6561  CALL AGSPOPB                                                        1326
      GO TO BLOCK_SBMM, (6560 ,6570,6580)                                 1326
C End of Local Procedure                                                  1326
C ----------------------------------------------------------------------  1327
C ------------ SMRD                                                       1327
C Begin Local Procedure SMRD                                              1328
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1328
6730  CONTINUE                                                            1328
         AG_TITLE='SMRD'                                                  1328
         AG_MODULE=MODULE                                                 1328
         AG_MARK='V00'                                                    1328
         CALL AGSPUSH                                                     1328
         AG_MODULE=MODULE                                                 1328
C    *                                                                    1328
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1329
         CALL AGSRESET                                                    1329
         AG_TITLE = 'ALUMINIUM'                                           1329
         AG_EXNAME = 'MATERIAL'                                           1329
         AG_PARLIST = 'NONE'                                              1329
         CALL AXMATERIAL                                                  1329
C    *                                                                    1329
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1329
         CALL AGSRESET                                                    1330
         AG_SEEN = 1                                                      1330
         AG_COLO = 2                                                      1330
         AG_TITLE = 'SMRD'                                                1330
         AG_EXNAME = 'ATTRIBUTE'                                          1330
         AG_PARLIST = 'SEEN_COLO'                                         1330
         CALL AXATTRIBUTE                                                 1330
C    *                                                                    1330
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1330
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1330
         CALL AGSRESET                                                    1332
         AG_RMIN = 0.0                                                    1332
         AG_RMAX = SSUB_MRODDIA/2                                         1332
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1332
         AG_TITLE = 'TUBE'                                                1332
         AG_EXNAME = 'SHAPE'                                              1332
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1332
         CALL AXSHAPE                                                     1332
C    *                                                                    1332
6731  CALL AGSPOPB                                                        1333
      GO TO BLOCK_SMRD, (6730 ,6740)                                      1333
C End of Local Procedure                                                  1333
C ----------------------------------------------------------------------  1334
C ------------ SBMO                                                       1334
C Begin Local Procedure SBMO                                              1335
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1335
6750  CONTINUE                                                            1335
         AG_TITLE='SBMO'                                                  1335
         AG_MODULE=MODULE                                                 1335
         AG_MARK='V00'                                                    1335
         CALL AGSPUSH                                                     1335
         AG_MODULE=MODULE                                                 1335
C    *                                                                    1335
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1336
         CALL AGSRESET                                                    1336
         AG_TITLE = 'G10'                                                 1336
         AG_EXNAME = 'MATERIAL'                                           1336
         AG_PARLIST = 'NONE'                                              1336
         CALL AXMATERIAL                                                  1336
C    *                                                                    1336
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1336
         CALL AGSRESET                                                    1337
         AG_SEEN = 1                                                      1337
         AG_COLO = 3                                                      1337
         AG_TITLE = 'SBMO'                                                1337
         AG_EXNAME = 'ATTRIBUTE'                                          1337
         AG_PARLIST = 'SEEN_COLO'                                         1337
         CALL AXATTRIBUTE                                                 1337
C    *                                                                    1337
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1337
C    UB_MBLKHGH/2 <===                                                    1337
         CALL AGSRESET                                                    1339
         AG_DX = SSUB_MBLKOWID/2                                          1339
         AG_DY = SSUB_MBLKOLEN/2                                          1339
         AG_DZ = SSUB_MBLKHGH/2                                           1339
         AG_TITLE = 'BOX'                                                 1339
         AG_EXNAME = 'SHAPE'                                              1339
         AG_PARLIST = 'DX_DY_DZ'                                          1339
         CALL AXSHAPE                                                     1339
C    *                                                                    1339
6751  CALL AGSPOPB                                                        1340
      GO TO BLOCK_SBMO, (6750 ,6760)                                      1340
C End of Local Procedure                                                  1340
C ----------------------------------------------------------------------  1341
C ------------ SBMI                                                       1341
C Begin Local Procedure SBMI                                              1342
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1342
6770  CONTINUE                                                            1342
         AG_TITLE='SBMI'                                                  1342
         AG_MODULE=MODULE                                                 1342
         AG_MARK='V00'                                                    1342
         CALL AGSPUSH                                                     1342
         AG_MODULE=MODULE                                                 1342
C    *                                                                    1342
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1343
         CALL AGSRESET                                                    1343
         AG_TITLE = 'G10'                                                 1343
         AG_EXNAME = 'MATERIAL'                                           1343
         AG_PARLIST = 'NONE'                                              1343
         CALL AXMATERIAL                                                  1343
C    *                                                                    1343
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1343
         CALL AGSRESET                                                    1344
         AG_SEEN = 1                                                      1344
         AG_COLO = 3                                                      1344
         AG_TITLE = 'SBMO'                                                1344
         AG_EXNAME = 'ATTRIBUTE'                                          1344
         AG_PARLIST = 'SEEN_COLO'                                         1344
         CALL AXATTRIBUTE                                                 1344
C    *                                                                    1344
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1344
C    UB_MBLKHGH/2 <===                                                    1344
         CALL AGSRESET                                                    1346
         AG_DX = SSUB_MBLKIWID/2                                          1346
         AG_DY = SSUB_MBLKILEN/2                                          1346
         AG_DZ = SSUB_MBLKHGH/2                                           1346
         AG_TITLE = 'BOX'                                                 1346
         AG_EXNAME = 'SHAPE'                                              1346
         AG_PARLIST = 'DX_DY_DZ'                                          1346
         CALL AXSHAPE                                                     1346
C    *                                                                    1346
6771  CALL AGSPOPB                                                        1347
      GO TO BLOCK_SBMI, (6770 ,6780)                                      1347
C End of Local Procedure                                                  1347
C ----------------------------------------------------------------------  1348
C ------------ SALM                                                       1348
C Begin Local Procedure SALM                                              1349
C SALM IS THE ALUMINUM SHIELD MESH                                        1349
5370  CONTINUE                                                            1349
         AG_TITLE='SALM'                                                  1349
         AG_MODULE=MODULE                                                 1349
         AG_MARK='V00'                                                    1349
         CALL AGSPUSH                                                     1349
         AG_MODULE=MODULE                                                 1349
C    *                                                                    1349
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1350
         CALL AGSRESET                                                    1350
         AG_TITLE = 'ALUMINIUM'                                           1350
         AG_EXNAME = 'MATERIAL'                                           1350
         AG_PARLIST = 'NONE'                                              1350
         CALL AXMATERIAL                                                  1350
C    *                                                                    1350
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1350
         CALL AGSRESET                                                    1351
         AG_SEEN = 1                                                      1351
         AG_COLO = 2                                                      1351
         AG_TITLE = 'SALM'                                                1351
         AG_EXNAME = 'ATTRIBUTE'                                          1351
         AG_PARLIST = 'SEEN_COLO'                                         1351
         CALL AXATTRIBUTE                                                 1351
C    *                                                                    1351
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1351
C    DZ=SSLD_ALMSHTHK/2 <===                                              1351
         CALL AGSRESET                                                    1353
         AG_RMIN = SSLD_ALMESHID/2                                        1353
         AG_RMAX = SSLD_ALMESHOD/2                                        1353
         AG_DZ = SSLD_ALMSHTHK/2                                          1353
         AG_TITLE = 'TUBE'                                                1353
         AG_EXNAME = 'SHAPE'                                              1353
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1353
         CALL AXSHAPE                                                     1353
C    *                                                                    1353
5371  CALL AGSPOPB                                                        1354
      GO TO BLOCK_SALM, (5370 ,5380)                                      1354
C End of Local Procedure                                                  1354
C ----------------------------------------------------------------------  1355
C ------------ SISH                                                       1355
C Begin Local Procedure SISH                                              1356
C SISH IS THE INNER SHIELD CYLINDER                                       1356
5310  CONTINUE                                                            1356
         AG_TITLE='SISH'                                                  1356
         AG_MODULE=MODULE                                                 1356
         AG_MARK='V00'                                                    1356
         CALL AGSPUSH                                                     1356
         AG_MODULE=MODULE                                                 1356
C    *                                                                    1356
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1357
         CALL AGSRESET                                                    1357
         AG_TITLE = 'ALKAP'                                               1357
         AG_EXNAME = 'MATERIAL'                                           1357
         AG_PARLIST = 'NONE'                                              1357
         CALL AXMATERIAL                                                  1357
C    *                                                                    1357
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1357
         CALL AGSRESET                                                    1358
         AG_SEEN = 1                                                      1358
         AG_COLO = 3                                                      1358
         AG_TITLE = 'SISH'                                                1358
         AG_EXNAME = 'ATTRIBUTE'                                          1358
         AG_PARLIST = 'SEEN_COLO'                                         1358
         CALL AXATTRIBUTE                                                 1358
C    *                                                                    1358
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1358
C    D_SINLEN <===                                                        1358
         CALL AGSRESET                                                    1360
         AG_RMIN = SSLD_SINRINN                                           1360
         AG_RMAX = SSLD_SINROUT                                           1360
         AG_DZ = SSLD_SINLEN                                              1360
         AG_TITLE = 'TUBE'                                                1360
         AG_EXNAME = 'SHAPE'                                              1360
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1360
         CALL AXSHAPE                                                     1360
C    *                                                                    1360
5311  CALL AGSPOPB                                                        1361
      GO TO BLOCK_SISH, (5310 ,5320)                                      1361
C End of Local Procedure                                                  1361
C ----------------------------------------------------------------------  1362
C ------------ SSSH                                                       1362
C Begin Local Procedure SSSH                                              1363
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1363
5350  CONTINUE                                                            1363
         AG_TITLE='SSSH'                                                  1363
         AG_MODULE=MODULE                                                 1363
         AG_MARK='V00'                                                    1363
         CALL AGSPUSH                                                     1363
         AG_MODULE=MODULE                                                 1363
C    *                                                                    1363
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1364
         CALL AGSRESET                                                    1364
         AG_TITLE = 'SSDALMY'                                             1364
         AG_EXNAME = 'MATERIAL'                                           1364
         AG_PARLIST = 'NONE'                                              1364
         CALL AXMATERIAL                                                  1364
C    *                                                                    1364
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1364
         CALL AGSRESET                                                    1365
         AG_SEEN = 1                                                      1365
         AG_COLO = 3                                                      1365
         AG_TITLE = 'SSSH'                                                1365
         AG_EXNAME = 'ATTRIBUTE'                                          1365
         AG_PARLIST = 'SEEN_COLO'                                         1365
         CALL AXATTRIBUTE                                                 1365
C    *                                                                    1365
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1365
C    SLD_SSEPLEN <===                                                     1365
         CALL AGSRESET                                                    1367
         AG_RMIN = SSLD_SSEPRINN                                          1367
         AG_RMAX = SSLD_SSEPROUT                                          1367
         AG_DZ = SSLD_SSEPLEN                                             1367
         AG_TITLE = 'TUBE'                                                1367
         AG_EXNAME = 'SHAPE'                                              1367
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1367
         CALL AXSHAPE                                                     1367
C    *                                                                    1367
5351  CALL AGSPOPB                                                        1368
      GO TO BLOCK_SSSH, (5350 ,5360)                                      1368
C End of Local Procedure                                                  1368
C ----------------------------------------------------------------------  1369
C ------------ SOSH                                                       1369
C Begin Local Procedure SOSH                                              1370
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1370
5330  CONTINUE                                                            1370
         AG_TITLE='SOSH'                                                  1370
         AG_MODULE=MODULE                                                 1370
         AG_MARK='V00'                                                    1370
         CALL AGSPUSH                                                     1370
         AG_MODULE=MODULE                                                 1370
C    *                                                                    1370
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1371
         CALL AGSRESET                                                    1371
         AG_TITLE = 'SSDALMY'                                             1371
         AG_EXNAME = 'MATERIAL'                                           1371
         AG_PARLIST = 'NONE'                                              1371
         CALL AXMATERIAL                                                  1371
C    *                                                                    1371
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1371
         CALL AGSRESET                                                    1372
         AG_SEEN = 1                                                      1372
         AG_COLO = 3                                                      1372
         AG_TITLE = 'SOSH'                                                1372
         AG_EXNAME = 'ATTRIBUTE'                                          1372
         AG_PARLIST = 'SEEN_COLO'                                         1372
         CALL AXATTRIBUTE                                                 1372
C    *                                                                    1372
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1372
C    SLD_SOUTLEN <===                                                     1372
         CALL AGSRESET                                                    1374
         AG_RMIN = SSLD_SOUTRINN                                          1374
         AG_RMAX = SSLD_SOUTROUT                                          1374
         AG_DZ = SSLD_SOUTLEN                                             1374
         AG_TITLE = 'TUBE'                                                1374
         AG_EXNAME = 'SHAPE'                                              1374
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1374
         CALL AXSHAPE                                                     1374
C    *                                                                    1374
5331  CALL AGSPOPB                                                        1375
      GO TO BLOCK_SOSH, (5330 ,5340)                                      1375
C End of Local Procedure                                                  1375
      WRITE(LOUT,6790)'===>>  [End AgML Module SVTTGEO4]  <<==='          1375
6790  FORMAT(A32,/,/)                                                     1375
C MODULE SVTTGEO4                                                         1377
      END                                                                 1377
