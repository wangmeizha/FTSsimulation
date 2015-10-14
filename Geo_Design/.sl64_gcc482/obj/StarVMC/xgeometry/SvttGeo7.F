C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO7                                                 1
C SVTTGEO7 IS EXTENSION OF THE SVTTGEO6: CLAMSHELLS AND DISTORTIONS       1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SVTTGEO7'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS EXTENSION OF THE SVTTGEO6: CLAMSHELLS AND DISTOR  1
     *TIONS'/                                                             1
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
C CREATED 24 MAR 2006                                                     2
      CHARACTER*40 CREATED /'24 MAR 2006'/                                3
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
      INTEGER BLOCK_STRA                                                  12
      INTEGER BLOCK_SSID                                                  12
      INTEGER BLOCK_SSIR                                                  12
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
      REAL SWCA_SENSWID                                                   16
      EQUIVALENCE (BANK_SWCA(14),SWCA_SENSWID,RE_SWCA_SENSWID),           16
     *(NAMES_SWCA(12),NSWCA_SENSWID)                                      16
      EQUIVALENCE (COMMEN_SWCA(12),CSWCA_SENSWID),(MAP_SWCA(1,12),        16
     *MSWCA_SENSWID)                                                      16
      CHARACTER CSWCA_SENSWID*40/' '/                                     16
      CHARACTER NSWCA_SENSWID*12/'R.SENSWID'/                             16
      INTEGER MSWCA_SENSWID(2)/1,1/                                       16
      REAL RE_SWCA_SENSWID                                                16
C -                                                                       16
      REAL SWCA_SENSLEN                                                   16
      EQUIVALENCE (BANK_SWCA(15),SWCA_SENSLEN,RE_SWCA_SENSLEN),           16
     *(NAMES_SWCA(13),NSWCA_SENSLEN)                                      16
      EQUIVALENCE (COMMEN_SWCA(13),CSWCA_SENSLEN),(MAP_SWCA(1,13),        16
     *MSWCA_SENSLEN)                                                      16
      CHARACTER CSWCA_SENSLEN*40/' '/                                     16
      CHARACTER NSWCA_SENSLEN*12/'R.SENSLEN'/                             16
      INTEGER MSWCA_SENSLEN(2)/1,1/                                       16
      REAL RE_SWCA_SENSLEN                                                16
C -                                                                       16
      REAL SWCA_SENSGIR                                                   16
      EQUIVALENCE (BANK_SWCA(16),SWCA_SENSGIR,RE_SWCA_SENSGIR),           16
     *(NAMES_SWCA(14),NSWCA_SENSGIR)                                      16
      EQUIVALENCE (COMMEN_SWCA(14),CSWCA_SENSGIR),(MAP_SWCA(1,14),        16
     *MSWCA_SENSGIR)                                                      16
      CHARACTER CSWCA_SENSGIR*40/' '/                                     16
      CHARACTER NSWCA_SENSGIR*12/'R.SENSGIR'/                             16
      INTEGER MSWCA_SENSGIR(2)/1,1/                                       16
      REAL RE_SWCA_SENSGIR                                                16
C -                                                                       16
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                16
      PARAMETER (LEN_SWCA=16)                                             16
      PARAMETER (LLEN_SWCA=14)                                            16
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
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J,IPOS                            36
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         38
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           40
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          42
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    44
      REAL XBUF,PHI,XBUF1,XBUF2                                           46
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     48
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          50
     *SHIELD_PHI(4)                                                       50
      REAL TRAPY,SSIDX,SSIRY                                              52
      REAL XDISP,YDISP,ADISP,CS                                           54
      INTEGER I_PHI,CHECKREZ,FOO,LSUB                                     56
      REAL XD(6,16),YD(6,16),AD(6,16)                                     58
      DATA XD/0.020,-0.010,0.010,0.010,0.020,0.000,0.000,0.000,-0.010,    67
     *0.000,-0.010, 0.000,-0.013,0.000,0.000,0.000,0.000,0.000,0.000,     67
     *0.000,0.000,0.000,0.000,0.000, 0.010,0.000,0.000,0.000,0.000,       67
     *0.000,0.000,0.000,0.000,0.000,-0.010,0.000,0.000, 0.000,-0.010,     67
     *0.000,0.000,-0.025,0.000,0.000,0.000,0.000,-0.010,0.000,0.010,      67
     *0.000, 0.000,0.000,0.000,0.000,-0.010,0.000,0.000,0.000,0.000,      67
     *0.000,-0.020,0.000,0.000, 0.000,0.000,0.000,0.000,-0.020,0.000,     67
     *0.000,0.000,0.000,0.000,0.000,0.000,0.000, -0.010,0.000,0.000,      67
     *0.000,-0.010,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,       67
     *0.000,0.000,0.000,0.000,-0.010,0.000,0.000/                         67
      DATA YD/0.000,0.010,0.005,0.005,0.000,0.000,0.020,0.000,0.000,      75
     *0.000,0.005,0.000, -0.010,0.000,0.000,0.000,0.000,0.000,0.020,      75
     *0.010,-0.020,0.020,0.000,0.000,0.000, 0.000,-0.010,0.000,0.000,     75
     *0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000, 0.000,       75
     *-0.010,0.000,-0.011,0.000,0.000,-0.010,0.000,0.000,0.000,0.000,     75
     *0.000,0.000, 0.000,0.000,0.000,-0.020,0.000,0.000,0.020,0.000,      75
     *0.000,-0.010,0.000,-0.010, 0.000,0.000,0.000,-0.010,0.000,0.000,    75
     *0.000,0.000,0.000,0.000,0.000,-0.010,0.000, 0.000,0.000,-0.005,     75
     *0.000,-0.010,0.000,0.000,-0.010,0.000,0.000,-0.010,0.000,0.000,     75
     *0.000,0.000,0.000,0.000,0.010,0.010,0.030/                          75
      DATA AD/0.200,0.000,0.100,0.200,-0.200,0.300,0.000,0.300,0.000,     83
     *0.300,0.000, 0.200,0.200,0.000,0.000,-0.100,0.000,0.100,0.000,      83
     *0.300,-0.200,0.000,0.000,0.000, 0.000,0.000,-0.200,0.000,0.000,     83
     *0.000,0.000,0.000,0.000,0.300,0.000,0.000,0.000,0.000, 0.000,       83
     *-0.200,0.000,-0.200,0.000,0.000,0.000,0.000,0.000,0.000,0.000,      83
     *0.000,0.000, 0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,       83
     *0.000,0.000,0.000,0.000,0.000, 0.000,0.000,0.000,0.000,0.000,       83
     *0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000, 0.000,0.000,       83
     *0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,  83
     *0.000, 0.000,0.000,0.000,0.000,0.000/                               83
      RADII(1)= 6.37                                                      84
      RADII(2)= 7.38                                                      85
      RADII(3)=10.38                                                      86
      RADII(4)=11.27                                                      87
      RADII(5)=14.19                                                      88
      RADII(6)=15.13                                                      89
      RAD_OFFSET=0.015                                                    90
      SHIELD_PHI(1)=9.0                                                   91
      SHIELD_PHI(2)=27.0                                                  92
      SHIELD_PHI(3)=45.0                                                  93
      SHIELD_PHI(4)=63.0                                                  94
C *                                                                       95
      CALL AGSBEGM(MODULE,IPRIN)                                          95
      IF (IPRIN.LT.0) RETURN                                              95
C Filling bank SVTG - BASIC SVT DIMENSIONS                                95
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   95
      BPATH_SVTG='SVTG*'                                                  95
      IDNUM_SVTG(1)=0                                                     96
C GEOMETRY VERSION                                                        96
      SVTG_VERSION=2                                                      96
      CSVTG_VERSION='GEOMETRY VERSION'                                    96
C NUMBER OF SVT LAYERS (WAS 7)                                            97
      SVTG_NLAYER=7                                                       97
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         97
C STV INNERMOST RADIUS                                                    98
      SVTG_RSIZEMIN=4.100                                                 98
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               98
C STV OUTERMOST RADIUS                                                    99
      SVTG_RSIZEMAX=46.107                                                99
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               99
C SVT+FTPC LENGTH                                                         100
      SVTG_ZSIZEMAX=270                                                   100
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    100
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          101
      SVTG_ANGOFF=0                                                       101
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       101
C VERSIONING OF THE SHIELD                                                102
      SVTG_SUPPORTVER=1                                                   102
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         102
C VERSIONING OF THE SUPPORT CONE                                          103
      SVTG_CONEVER=1                                                      103
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      103
C WHETHER WE USE THE GEANT MANY OPTION                                    104
      SVTG_IFMANY=0                                                       104
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 104
C THE INDEX OF THE INNERMOST LAYER                                        105
      SVTG_NMIN=1                                                         105
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       105
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    106
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           106
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   106
C *                                                                       106
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    106
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           106
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   106
C *                                                                       107
      CALL AGSBEGM(MODULE,IPRIN)                                          107
      IF (IPRIN.LT.0) RETURN                                              107
C Filling bank SWCA - WAFER CARRIER                                       107
      BANK_TITLE='WAFER CARRIER'                                          107
      BPATH_SWCA='SWCA*'                                                  107
      IDNUM_SWCA(1)=0                                                     108
C GEOMETRY VERSION                                                        108
      SWCA_VERSION=1                                                      108
      CSWCA_VERSION='GEOMETRY VERSION'                                    108
C LADDER LENGTH                                                           109
      SWCA_LENGTH=56.04                                                   109
      CSWCA_LENGTH='LADDER LENGTH'                                        109
C WAFER WIDTH                                                             110
      SWCA_WAFERWID=6.305                                                 110
      CSWCA_WAFERWID='WAFER WIDTH'                                        110
C WAFER LENGTH                                                            111
      SWCA_WAFERLEN=6.305                                                 111
      CSWCA_WAFERLEN='WAFER LENGTH'                                       111
C WAFER THICKNESS                                                         112
      SWCA_WAFERTHK=0.0300                                                112
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    112
C ROHA CELL PLUS GLUE THICKNESS                                           113
      SWCA_ROHATHK=0.0381                                                 113
      CSWCA_ROHATHK='ROHA CELL PLUS GLUE THICKNESS'                       113
C WAFER CARRIER RAILS WIDTH                                               114
      SWCA_WAFCARWD=1.5                                                   114
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          114
C WAFER CARRIER THICKNESS                                                 115
      SWCA_WAFCARTH=0.0300                                                115
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            115
C INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY                      116
      SWCA_WAFERGAP=0.0                                                   116
      CSWCA_WAFERGAP='INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY  116
     *'                                                                   116
C DRIFT DIRECTION                                                         117
      SWCA_DRIFT=1                                                        117
      CSWCA_DRIFT='DRIFT DIRECTION'                                       117
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        118
      SWCA_STRUTLEN=1.0                                                   118
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   118
C DEFINE THE SENSITIVE SQUARE WIDTH (NEW)                                 119
      SWCA_SENSWID=6.000                                                  119
      CSWCA_SENSWID='DEFINE THE SENSITIVE SQUARE WIDTH (NEW)'             119
C DEFINE THE SENSITIVE SQUARE LENGTH (NEW)                                120
      SWCA_SENSLEN=6.000                                                  120
      CSWCA_SENSLEN='DEFINE THE SENSITIVE SQUARE LENGTH (NEW)'            120
C GIRTH (FOR LACK OF A BETTER WORD)                                       121
      SWCA_SENSGIR=5.775                                                  121
      CSWCA_SENSGIR='GIRTH (FOR LACK OF A BETTER WORD)'                   121
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    122
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           122
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   122
C *                                                                       122
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    122
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           122
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   122
C *                                                                       123
      CALL AGSBEGM(MODULE,IPRIN)                                          123
      IF (IPRIN.LT.0) RETURN                                              123
C Filling bank SSUP - SUPPORT STRUCTURES                                  123
      BANK_TITLE='SUPPORT STRUCTURES'                                     123
      BPATH_SSUP='SSUP*'                                                  123
      IDNUM_SSUP(1)=0                                                     124
C GEOMETRY VERSION                                                        124
      SSUP_VERSION=1                                                      124
      CSSUP_VERSION='GEOMETRY VERSION'                                    124
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            125
      SSUP_CABTHK=0.05                                                    125
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         125
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             126
      SSUP_HOSRMN=0.75                                                    126
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          126
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             127
      SSUP_HOSRMX=0.95                                                    127
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          127
C NUMBER OF WATER HOSES                                                   128
      SSUP_NHOSES=10                                                      128
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                128
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             129
      SSUP_WRPMYTHK=0.10                                                  129
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        129
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   130
      SSUP_WRPALTHK=0.01                                                  130
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              130
C SUPPORT CONE THICKNESS                                                  131
      SSUP_GRPHTHK=0.16                                                   131
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              131
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              132
      SSUP_CONE1ZMN=52.23                                                 132
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         132
C LENGTH OF SUPPORT RODS                                                  133
      SSUP_RODLEN=110.8                                                   133
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               133
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               134
      SSUP_RODDIST=17.5                                                   134
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           134
C ID OF CARBON SUPPORT RODS (APPROX)                                      135
      SSUP_RODID=2.5                                                      135
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    135
C OD OF CARBON SUPPORT RODS (APPROX)                                      136
      SSUP_RODOD=3.05                                                     136
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    136
C MINIMUM ID OF CONE 1                                                    137
      SSUP_CON1IDMN=15.67                                                 137
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               137
C MINIMUM ID OF CONE 3 (TBD)                                              138
      SSUP_CON3IDMN=21.67                                                 138
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         138
C MINIMUM ID OF CONE 4 (TBD)                                              139
      SSUP_CON4IDMN=37.4                                                  139
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         139
C MAXIMUM ID OF CONE 4 (TBD)                                              140
      SSUP_CON4IDMX=37.4                                                  140
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         140
C MAXIMUM Z OF CONE 3 (TBD)                                               141
      SSUP_CONE3ZMX=150.0                                                 141
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          141
C MAXIMUM Z OF CONE 4 (TBD)                                               142
      SSUP_CONE4ZMX=229.36                                                142
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          142
C THICKNESS OF AL BRACKETS                                                143
      SSUP_BRATHK=.2                                                      143
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             143
C (Z) THICKNESS OF END RING JOINING BRACKETS                              144
      SSUP_ERJTHK=.1                                                      144
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           144
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          145
      SSUP_ERJWID=2.07                                                    145
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       145
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            146
      SSUP_ERJLEN=5.19                                                    146
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         146
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    147
      SSUP_ERJZDIS=2.0                                                    147
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                147
C ERJ SCREW 1 X POSITION (RADIAL)                                         148
      SSUP_ERJ1X=0.31                                                     148
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       148
C ERJ SCREW 2 X POSITION                                                  149
      SSUP_ERJ2X=1.15                                                     149
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                149
C ERJ SCREW 2 Y POSITION                                                  150
      SSUP_ERJ2Y=0.72                                                     150
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                150
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   151
      SSUP_ERJRAD=10.80                                                   151
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                151
C ERJ SCREW DIAMETER                                                      152
      SSUP_ERJDIA=0.17                                                    152
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   152
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    153
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           153
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   153
C *                                                                       153
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    153
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           153
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   153
C *                                                                       154
      CALL AGSBEGM(MODULE,IPRIN)                                          154
      IF (IPRIN.LT.0) RETURN                                              154
C Filling bank SSUP - SUPPORT STRUCTURES                                  154
      BANK_TITLE='SUPPORT STRUCTURES'                                     154
      BPATH_SSUP='SSUP*'                                                  154
      IDNUM_SSUP(1)=0                                                     155
C GEOMETRY VERSION                                                        155
      SSUP_VERSION=2                                                      155
      CSSUP_VERSION='GEOMETRY VERSION'                                    155
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            156
      SSUP_CABTHK=0.21                                                    156
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         156
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             157
      SSUP_HOSRMN=0.75                                                    157
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          157
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             158
      SSUP_HOSRMX=0.95                                                    158
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          158
C NUMBER OF WATER HOSES                                                   159
      SSUP_NHOSES=10                                                      159
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                159
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             160
      SSUP_WRPMYTHK=0.10                                                  160
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        160
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   161
      SSUP_WRPALTHK=0.01                                                  161
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              161
C SUPPORT CONE THICKNESS                                                  162
      SSUP_GRPHTHK=0.16                                                   162
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              162
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              163
      SSUP_CONE1ZMN=52.23                                                 163
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         163
C LENGTH OF SUPPORT RODS                                                  164
      SSUP_RODLEN=110.8                                                   164
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               164
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               165
      SSUP_RODDIST=17.5                                                   165
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           165
C ID OF CARBON SUPPORT RODS (APPROX)                                      166
      SSUP_RODID=2.5                                                      166
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    166
C OD OF CARBON SUPPORT RODS (APPROX)                                      167
      SSUP_RODOD=3.05                                                     167
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    167
C MINIMUM ID OF CONE 1                                                    168
      SSUP_CON1IDMN=15.67                                                 168
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               168
C MINIMUM ID OF CONE 3 (TBD)                                              169
      SSUP_CON3IDMN=21.67                                                 169
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         169
C MINIMUM ID OF CONE 4 (TBD)                                              170
      SSUP_CON4IDMN=37.4                                                  170
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         170
C MAXIMUM ID OF CONE 4 (TBD)                                              171
      SSUP_CON4IDMX=37.4                                                  171
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         171
C MAXIMUM Z OF CONE 3 (TBD)                                               172
      SSUP_CONE3ZMX=150.0                                                 172
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          172
C MAXIMUM Z OF CONE 4 (TBD)                                               173
      SSUP_CONE4ZMX=229.36                                                173
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          173
C THICKNESS OF AL BRACKETS                                                174
      SSUP_BRATHK=.2                                                      174
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             174
C (Z) THICKNESS OF END RING JOINING BRACKETS                              175
      SSUP_ERJTHK=.1                                                      175
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           175
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          176
      SSUP_ERJWID=2.07                                                    176
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       176
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            177
      SSUP_ERJLEN=5.19                                                    177
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         177
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    178
      SSUP_ERJZDIS=2.0                                                    178
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                178
C ERJ SCREW 1 X POSITION (RADIAL)                                         179
      SSUP_ERJ1X=0.31                                                     179
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       179
C ERJ SCREW 2 X POSITION                                                  180
      SSUP_ERJ2X=1.15                                                     180
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                180
C ERJ SCREW 2 Y POSITION                                                  181
      SSUP_ERJ2Y=0.72                                                     181
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                181
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   182
      SSUP_ERJRAD=10.80                                                   182
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                182
C ERJ SCREW DIAMETER                                                      183
      SSUP_ERJDIA=0.17                                                    183
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   183
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    184
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           184
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   184
C *                                                                       184
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    184
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           184
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   184
C *                                                                       185
      CALL AGSBEGM(MODULE,IPRIN)                                          185
      IF (IPRIN.LT.0) RETURN                                              185
C Filling bank SSUB - BEAMPIPE SUPPORT                                    185
      BANK_TITLE='BEAMPIPE SUPPORT'                                       185
      BPATH_SSUB='SSUB*'                                                  185
      IDNUM_SSUB(1)=0                                                     186
C GEOMETRY VERSION                                                        186
      SSUB_VERSION=1                                                      186
      CSSUB_VERSION='GEOMETRY VERSION'                                    186
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  187
      SSUB_KMOUNTID=31.34                                                 187
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             187
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  188
      SSUB_KMOUNTOD=38.96                                                 188
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             188
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    189
      SSUB_KMNTTHK=0.64                                                   189
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                189
C OD OF CUTOUT IN KINEMATIC MOUNT                                         190
      SSUB_KMCUTOD=18.31                                                  190
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     190
C ID OF CUTOUT IN KINEMATIC MOUNT                                         191
      SSUB_KMCUTID=14                                                     191
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     191
C OPENING ANGLE OF CUTOUT                                                 192
      SSUB_KMCUTOA=38                                                     192
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             192
C OFFSET OF CUTOUT CENTER FROM AXIS                                       193
      SSUB_KMCUTOFF=26.58                                                 193
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  193
C ID OF BEAMPIPE SUPPORT RING                                             194
      SSUB_SRINGID=8.47                                                   194
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         194
C OD OF BEAMPIPE SUPPORT RING                                             195
      SSUB_SRINGOD=12.78                                                  195
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         195
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     196
      SSUB_SRINGTHK=0.64                                                  196
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                196
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     197
      SSUB_SRCUTPHI=38                                                    197
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                197
C SUPPORT RING CUTOUT WIDTH                                               198
      SSUB_SRCUTWID=3.63                                                  198
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          198
C SUPPORT RING CUTOUT DEPTH                                               199
      SSUB_SRCUTOUT=5.08                                                  199
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          199
C SUPPORT RING CUTOUT START                                               200
      SSUB_SRCUTIN=3.5                                                    200
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           200
C SUPPORT ROLLER ID                                                       201
      SSUB_SROLLID=0.2                                                    201
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   201
C SUPPORT ROLLER OD                                                       202
      SSUB_SROLLOD=0.62                                                   202
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   202
C SUPPORT ROLLER LENGTH                                                   203
      SSUB_SROLLLEN=2.54                                                  203
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              203
C SUPPORT ROLLER AXIS LENGTH                                              204
      SSUB_SWIRELEN=5.08                                                  204
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         204
C MOUNTING BLOCK HEIGHT                                                   205
      SSUB_MBLKHGH=0.97                                                   205
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               205
C OUTER MOUNTING BLOCK WIDTH                                              206
      SSUB_MBLKOWID=2.54                                                  206
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         206
C OUTER MOUNTING BLOCK LENGTH                                             207
      SSUB_MBLKOLEN=1.27                                                  207
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        207
C INNER MOUNTING BLOCK WIDTH                                              208
      SSUB_MBLKIWID=3.175                                                 208
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         208
C INNER MOUNTING BLOCK LENGTH                                             209
      SSUB_MBLKILEN=1.27                                                  209
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        209
C OUTER MOUNTING BLOCK AT RADIUS                                          210
      SSUB_MBLKORAD=17.4                                                  210
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     210
C INNER MOUNTING BLOCK AT RADIUS                                          211
      SSUB_MBLKIRAD=5.42                                                  211
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     211
C MOUNTING SUPPORT ROD DIAMETER                                           212
      SSUB_MRODDIA=0.32                                                   212
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       212
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    213
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           213
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   213
C *                                                                       213
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    213
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           213
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   213
C *                                                                       214
      CALL AGSBEGM(MODULE,IPRIN)                                          214
      IF (IPRIN.LT.0) RETURN                                              214
C Filling bank SERG - END RINGS                                           214
      BANK_TITLE='END RINGS'                                              214
      BPATH_SERG='SERG*'                                                  214
      IDNUM_SERG(1)=0                                                     215
C GEOMETRY VERSION                                                        215
      SERG_VERSION=1                                                      215
      CSERG_VERSION='GEOMETRY VERSION'                                    215
C INNER END RING TUBE MAXIMUM RADIUS                                      216
      SERG_IRNGTRMX=9.703                                                 216
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 216
C INNER END RING POLYGON MINIMUM RADIUS                                   217
      SERG_IRNGPRMN=7.671                                                 217
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              217
C OUTER END RING MINIMUM RADIUS                                           218
      SERG_ORNGRMIN=11.900                                                218
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      218
C OUTER END RING MAXIMUM RADIUS                                           219
      SERG_ORNGRMAX=13.805                                                219
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      219
C END RING THICKNESS                                                      220
      SERG_ENDRNGTH=0.2                                                   220
      CSERG_ENDRNGTH='END RING THICKNESS'                                 220
C MINIMUM Z FOR END RINGS                                                 221
      SERG_ENDRNGZM=23.01                                                 221
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            221
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    222
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           222
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   222
C *                                                                       222
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    222
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           222
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   222
C *                                                                       223
      CALL AGSBEGM(MODULE,IPRIN)                                          223
      IF (IPRIN.LT.0) RETURN                                              223
C Filling bank SWAM - WATER MANIFOLD                                      223
      BANK_TITLE='WATER MANIFOLD'                                         223
      BPATH_SWAM='SWAM*'                                                  223
      IDNUM_SWAM(1)=0                                                     224
C GEOMETRY VERSION                                                        224
      SWAM_VERSION=1                                                      224
      CSWAM_VERSION='GEOMETRY VERSION'                                    224
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                225
      SWAM_ZMIN=33.9                                                      225
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               225
C WATER MANIFOLD FULL LENGTH                                              226
      SWAM_LEN=16.0                                                       226
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              226
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         227
      SWAM_RMIN=15.24                                                     227
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        227
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         228
      SWAM_RMAX=16.83                                                     228
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        228
C TRANSITION BOARD THICKNESS                                              229
      SWAM_TBRDTHK=0.1                                                    229
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          229
C WATER MANIFOLD WALL THICKNESS                                           230
      SWAM_WALLTHK=0.1                                                    230
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       230
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    231
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           231
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   231
C *                                                                       231
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    231
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           231
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   231
C *                                                                       232
      CALL AGSBEGM(MODULE,IPRIN)                                          232
      IF (IPRIN.LT.0) RETURN                                              232
C Filling bank SELC - ELECTRONICS CARRIER DATA                            232
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               232
      BPATH_SELC='SELC*'                                                  232
      IDNUM_SELC(1)=0                                                     233
C GEOMETRY VERSION                                                        233
      SELC_VERSION=1                                                      233
      CSELC_VERSION='GEOMETRY VERSION'                                    233
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              234
      SELC_BETHK=0.0500                                                   234
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            234
C THICKNESS OF WATER CHANNEL                                              235
      SELC_WATTHK=0.0750                                                  235
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           235
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    236
      SELC_BEOTHK=0.0500                                                  236
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 236
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                237
      SELC_DYETHK=0.0340                                                  237
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             237
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                238
      SELC_DYEWID=0.3                                                     238
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             238
C SPACE IC CHIPS                                                          239
      SELC_DYESPC=0.1                                                     239
      CSELC_DYESPC='SPACE IC CHIPS'                                       239
C ELECTRONICS CARRIER WIDTH                                               240
      SELC_ELCAWID=2.0                                                    240
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           240
C THICKNESS OF AG-PD CONDUCTOR                                            241
      SELC_AGPDTHK=0.0030                                                 241
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        241
C THICKNESS OF GLASS INSULATOR                                            242
      SELC_GLASSTHK=0.0150                                                242
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       242
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        243
      SELC_CABTHK=0.0033                                                  243
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     243
C CABLE WIDTH                                                             244
      SELC_CABWID=0.6                                                     244
      CSELC_CABWID='CABLE WIDTH'                                          244
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    245
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           245
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   245
C *                                                                       245
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    245
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           245
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   245
C *                                                                       246
      CALL AGSBEGM(MODULE,IPRIN)                                          246
      IF (IPRIN.LT.0) RETURN                                              246
C Filling bank SVTL - SINGLE LAYER DATA                                   246
      BANK_TITLE='SINGLE LAYER DATA'                                      246
      BPATH_SVTL='SVTL*'                                                  246
      IDNUM_SVTL(1)=0                                                     247
C LAYER NUMBER                                                            247
      SVTL_LAYER=1                                                        247
      CSVTL_LAYER='LAYER NUMBER'                                          247
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 248
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     248
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              248
C NUMBER OF LADDER                                                        249
      SVTL_NLADDER=4                                                      249
      CSVTL_NLADDER='NUMBER OF LADDER'                                    249
C NUMBER OF WAFERS                                                        250
      SVTL_NWAFER=4                                                       250
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     250
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        251
      SVTL_BAREEDGE=1.0                                                   251
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   251
C PCB LENGTH                                                              252
      SVTL_PCBLEN=14.9                                                    252
      CSVTL_PCBLEN='PCB LENGTH'                                           252
C PCB WIDTH                                                               253
      SVTL_PCBWIDTH=6.3                                                   253
      CSVTL_PCBWIDTH='PCB WIDTH'                                          253
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            254
      SVTL_PCBTHK=0.1                                                     254
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  254
     *R COPPER'                                                           254
C GAP BETWEEN THE PCB AND THE SI WAFER                                    255
      SVTL_PCBGAP=0.2                                                     255
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 255
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    256
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           256
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   256
C *                                                                       256
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    256
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           256
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   256
C *                                                                       257
      CALL AGSBEGM(MODULE,IPRIN)                                          257
      IF (IPRIN.LT.0) RETURN                                              257
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             257
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                257
      BPATH_SVTL='SVTL*'                                                  257
      IDNUM_SVTL(1)=0                                                     258
C LAYER NUMBER                                                            258
      SVTL_LAYER=2                                                        258
      CSVTL_LAYER='LAYER NUMBER'                                          258
C LAYER RADIUS                                                            259
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     259
      CSVTL_RADIUS='LAYER RADIUS'                                         259
C PCB LENGTH                                                              260
      SVTL_PCBLEN=14.9                                                    260
      CSVTL_PCBLEN='PCB LENGTH'                                           260
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    261
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           261
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   261
C *                                                                       261
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    261
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           261
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   261
C *                                                                       262
      CALL AGSBEGM(MODULE,IPRIN)                                          262
      IF (IPRIN.LT.0) RETURN                                              262
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             262
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                262
      BPATH_SVTL='SVTL*'                                                  262
      IDNUM_SVTL(1)=0                                                     263
C LAYER NUMBER                                                            263
      SVTL_LAYER=3                                                        263
      CSVTL_LAYER='LAYER NUMBER'                                          263
C LAYER RADIUS                                                            264
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     264
      CSVTL_RADIUS='LAYER RADIUS'                                         264
C NUMBER OF LADDER                                                        265
      SVTL_NLADDER=6                                                      265
      CSVTL_NLADDER='NUMBER OF LADDER'                                    265
C NUMBER OF WAFERS                                                        266
      SVTL_NWAFER=6                                                       266
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     266
C PCB LENGTH                                                              267
      SVTL_PCBLEN=7.5                                                     267
      CSVTL_PCBLEN='PCB LENGTH'                                           267
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    268
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           268
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   268
C *                                                                       268
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    268
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           268
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   268
C *                                                                       269
      CALL AGSBEGM(MODULE,IPRIN)                                          269
      IF (IPRIN.LT.0) RETURN                                              269
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             269
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                269
      BPATH_SVTL='SVTL*'                                                  269
      IDNUM_SVTL(1)=0                                                     270
C LAYER NUMBER                                                            270
      SVTL_LAYER=4                                                        270
      CSVTL_LAYER='LAYER NUMBER'                                          270
C LAYER RADIUS                                                            271
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     271
      CSVTL_RADIUS='LAYER RADIUS'                                         271
C PCB LENGTH                                                              272
      SVTL_PCBLEN=7.5                                                     272
      CSVTL_PCBLEN='PCB LENGTH'                                           272
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    273
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           273
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   273
C *                                                                       273
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    273
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           273
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   273
C *                                                                       274
      CALL AGSBEGM(MODULE,IPRIN)                                          274
      IF (IPRIN.LT.0) RETURN                                              274
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             274
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                274
      BPATH_SVTL='SVTL*'                                                  274
      IDNUM_SVTL(1)=0                                                     275
C LAYER NUMBER                                                            275
      SVTL_LAYER=5                                                        275
      CSVTL_LAYER='LAYER NUMBER'                                          275
C LAYER RADIUS                                                            276
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     276
      CSVTL_RADIUS='LAYER RADIUS'                                         276
C NUMBER OF LADDER                                                        277
      SVTL_NLADDER=8                                                      277
      CSVTL_NLADDER='NUMBER OF LADDER'                                    277
C NUMBER OF WAFERS                                                        278
      SVTL_NWAFER=7                                                       278
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     278
C PCB LENGTH                                                              279
      SVTL_PCBLEN=4.4                                                     279
      CSVTL_PCBLEN='PCB LENGTH'                                           279
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    280
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           280
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   280
C *                                                                       280
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    280
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           280
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   280
C *                                                                       281
      CALL AGSBEGM(MODULE,IPRIN)                                          281
      IF (IPRIN.LT.0) RETURN                                              281
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             281
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                281
      BPATH_SVTL='SVTL*'                                                  281
      IDNUM_SVTL(1)=0                                                     282
C LAYER NUMBER                                                            282
      SVTL_LAYER=6                                                        282
      CSVTL_LAYER='LAYER NUMBER'                                          282
C LAYER RADIUS                                                            283
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     283
      CSVTL_RADIUS='LAYER RADIUS'                                         283
C PCB LENGTH                                                              284
      SVTL_PCBLEN=4.4                                                     284
      CSVTL_PCBLEN='PCB LENGTH'                                           284
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    285
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           285
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   285
C *                                                                       285
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    285
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           285
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   285
C *                                                                       286
      CALL AGSBEGM(MODULE,IPRIN)                                          286
      IF (IPRIN.LT.0) RETURN                                              286
C Filling bank SSLD - SHIELDING PARAMETERS                                286
      BANK_TITLE='SHIELDING PARAMETERS'                                   286
      BPATH_SSLD='SSLD*'                                                  286
      IDNUM_SSLD(1)=0                                                     287
C GEOMETRY VERSION                                                        287
      SSLD_VERSION=1                                                      287
      CSSLD_VERSION='GEOMETRY VERSION'                                    287
C INNER SHIELD CYLINDER, INNER RADIUS                                     288
      SSLD_SINRINN=5                                                      288
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 288
C INNER SHIELD CYLINDER, OUTER RADIUS                                     289
      SSLD_SINROUT=5.008                                                  289
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 289
C INNER SHIELD CYLINDER, HALF LENGTH                                      290
      SSLD_SINLEN=53.5                                                    290
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   290
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                291
      SSLD_SSEPRINN=22                                                    291
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           291
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                292
      SSLD_SSEPROUT=22.018                                                292
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           292
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 293
      SSLD_SSEPLEN=55.4                                                   293
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             293
C OUTER SHIELD CYLINDER, INNER RADIUS                                     294
      SSLD_SOUTRINN=29.5                                                  294
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                294
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     295
      SSLD_SOUTROUT=29.52                                                 295
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                295
C OUTER SHIELD CYLINDER, HALF LENGTH                                      296
      SSLD_SOUTLEN=65.4                                                   296
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  296
C ALUMINUM SHIELD MESH INNER DIAMETER                                     297
      SSLD_ALMESHID=9.7                                                   297
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                297
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     298
      SSLD_ALMESHOD=44                                                    298
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                298
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                299
      SSLD_ALMSHTHK=0.03                                                  299
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           299
C ALUMINUM SHIELD MESH Z POSITION                                         300
      SSLD_ALMSHPOS=53.5                                                  300
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    300
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    301
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           301
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   301
C *                                                                       301
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    301
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           301
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   301
C *                                                                       302
      CALL AGSBEGM(MODULE,IPRIN)                                          302
      IF (IPRIN.LT.0) RETURN                                              302
C Filling bank SSLD - SHIELDING PARAMETERS                                302
      BANK_TITLE='SHIELDING PARAMETERS'                                   302
      BPATH_SSLD='SSLD*'                                                  302
      IDNUM_SSLD(1)=0                                                     303
C GEOMETRY VERSION                                                        303
      SSLD_VERSION=2                                                      303
      CSSLD_VERSION='GEOMETRY VERSION'                                    303
C INNER SHIELD CYLINDER, INNER RADIUS                                     304
      SSLD_SINRINN=5.9                                                    304
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 304
C INNER SHIELD CYLINDER, OUTER RADIUS                                     305
      SSLD_SINROUT=5.908                                                  305
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 305
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    306
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           306
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   306
C *                                                                       306
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    306
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           306
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   306
C *                                                                       307
      DO 5011 I=1,4                                                       307
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 308
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 309
5011  CONTINUE                                                            310
5012  CONTINUE                                                            310
C *                                                                       311
      CALL AGSBEGM(MODULE,IPRIN)                                          311
      IF (IPRIN.LT.0) RETURN                                              311
C Filling bank SCBP - CABLING                                             311
      BANK_TITLE='CABLING'                                                311
      BPATH_SCBP='SCBP*'                                                  311
      IDNUM_SCBP(1)=0                                                     312
C LAYER                                                                   312
      SCBP_LAYER=1                                                        312
      CSCBP_LAYER='LAYER'                                                 312
C LENGTH                                                                  313
      SCBP_LEN=1.85                                                       313
      CSCBP_LEN='LENGTH'                                                  313
C MIN RADIUS CLOSER TO WAFERS                                             314
      SCBP_RMIN1=RAD_CONES_IN(1)                                          314
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           314
C MIN RADIUS FURTHER FROM WAFERS                                          315
      SCBP_RMIN2=RAD_CONES_OUT(1)                                         315
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        315
C VOLUME OF COPPER, LV+HV CABLES                                          316
      SCBP_VOL=7.24+3.21                                                  316
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          316
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    317
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           317
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   317
C *                                                                       317
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    317
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           317
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   317
C *                                                                       318
      CALL AGSBEGM(MODULE,IPRIN)                                          318
      IF (IPRIN.LT.0) RETURN                                              318
C Filling bank SCBP - CABLING                                             318
      BANK_TITLE='CABLING'                                                318
      BPATH_SCBP='SCBP*'                                                  318
      IDNUM_SCBP(1)=0                                                     319
C LAYER                                                                   319
      SCBP_LAYER=2                                                        319
      CSCBP_LAYER='LAYER'                                                 319
C MIN RADIUS CLOSER TO WAFERS                                             320
      SCBP_RMIN1=RAD_CONES_IN(2)                                          320
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           320
C MIN RADIUS FURTHER FROM WAFERS                                          321
      SCBP_RMIN2=RAD_CONES_OUT(2)                                         321
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        321
C VOLUME OF COPPER, LV+HV CABLES                                          322
      SCBP_VOL=15.54+5.7                                                  322
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          322
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    323
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           323
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   323
C *                                                                       323
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    323
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           323
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   323
C *                                                                       324
      CALL AGSBEGM(MODULE,IPRIN)                                          324
      IF (IPRIN.LT.0) RETURN                                              324
C Filling bank SCBP - CABLING                                             324
      BANK_TITLE='CABLING'                                                324
      BPATH_SCBP='SCBP*'                                                  324
      IDNUM_SCBP(1)=0                                                     325
C LAYER                                                                   325
      SCBP_LAYER=3                                                        325
      CSCBP_LAYER='LAYER'                                                 325
C MIN RADIUS CLOSER TO WAFERS                                             326
      SCBP_RMIN1=RAD_CONES_IN(3)                                          326
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           326
C MIN RADIUS FURTHER FROM WAFERS                                          327
      SCBP_RMIN2=RAD_CONES_OUT(3)                                         327
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        327
C VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE                   328
      SCBP_VOL=4.05+2.02+3.67+1.69                                        328
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE'   328
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    329
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           329
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   329
C *                                                                       329
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    329
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           329
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   329
C *                                                                       330
      CALL AGSBEGM(MODULE,IPRIN)                                          330
      IF (IPRIN.LT.0) RETURN                                              330
C Filling bank SCBP - CABLING                                             330
      BANK_TITLE='CABLING'                                                330
      BPATH_SCBP='SCBP*'                                                  330
      IDNUM_SCBP(1)=0                                                     331
C LAYER (FORMER 5TH)                                                      331
      SCBP_LAYER=4                                                        331
      CSCBP_LAYER='LAYER (FORMER 5TH)'                                    331
C MIN RADIUS CLOSER TO WAFERS                                             332
      SCBP_RMIN1=RAD_CONES_IN(4)                                          332
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           332
C MIN RADIUS FURTHER FROM WAFERS                                          333
      SCBP_RMIN2=RAD_CONES_OUT(4)                                         333
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        333
C VOLUME OF COPPER, LV+HV CABLES                                          334
      SCBP_VOL=6.95+2.43                                                  334
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          334
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    335
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           335
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   335
C *                                                                       335
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    335
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           335
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   335
C *                                                                       336
      CALL AGSBEGM(MODULE,IPRIN)                                          336
      IF (IPRIN.LT.0) RETURN                                              336
C Filling bank SFEP - WATER FEED                                          336
      BANK_TITLE='WATER FEED'                                             336
      BPATH_SFEP='SFEP*'                                                  336
      IDNUM_SFEP(1)=0                                                     337
C LAYER                                                                   337
      SFEP_LAYER=1                                                        337
      CSFEP_LAYER='LAYER'                                                 337
C LENGTH                                                                  338
      SFEP_LEN=1.85                                                       338
      CSFEP_LEN='LENGTH'                                                  338
C MIN RADIUS CLOSER TO WAFERS                                             339
      SFEP_RMIN1=0.5*(RAD_CONES_IN(1) +RAD_CONES_IN(2))                   339
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           339
C MIN RADIUS FURTHER FROM WAFERS                                          340
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(1)+RAD_CONES_OUT(2))                  340
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        340
C VOLUME OF WATER                                                         341
      SFEP_VOL=16.0                                                       341
      CSFEP_VOL='VOLUME OF WATER'                                         341
C VOLUME OF PLASTIC                                                       342
      SFEP_VOLPLAST=38.4                                                  342
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  342
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    343
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           343
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   343
C *                                                                       343
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    343
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           343
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   343
C *                                                                       344
      CALL AGSBEGM(MODULE,IPRIN)                                          344
      IF (IPRIN.LT.0) RETURN                                              344
C Filling bank SFEP - WATER FEED                                          344
      BANK_TITLE='WATER FEED'                                             344
      BPATH_SFEP='SFEP*'                                                  344
      IDNUM_SFEP(1)=0                                                     345
C LAYER                                                                   345
      SFEP_LAYER=2                                                        345
      CSFEP_LAYER='LAYER'                                                 345
C MIN RADIUS CLOSER TO WAFERS                                             346
      SFEP_RMIN1=0.5*(RAD_CONES_IN(2) +RAD_CONES_IN(3))                   346
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           346
C MIN RADIUS FURTHER FROM WAFERS                                          347
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(2)+RAD_CONES_OUT(3))                  347
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        347
C VOLUME OF WATER                                                         348
      SFEP_VOL=24.0                                                       348
      CSFEP_VOL='VOLUME OF WATER'                                         348
C VOLUME OF PLASTIC                                                       349
      SFEP_VOLPLAST=57.6                                                  349
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  349
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    350
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           350
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   350
C *                                                                       350
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    350
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           350
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   350
C *                                                                       351
      CALL AGSBEGM(MODULE,IPRIN)                                          351
      IF (IPRIN.LT.0) RETURN                                              351
C Filling bank SFEP - WATER FEED                                          351
      BANK_TITLE='WATER FEED'                                             351
      BPATH_SFEP='SFEP*'                                                  351
      IDNUM_SFEP(1)=0                                                     352
C LAYER                                                                   352
      SFEP_LAYER=3                                                        352
      CSFEP_LAYER='LAYER'                                                 352
C MIN RADIUS CLOSER TO WAFERS                                             353
      SFEP_RMIN1=0.5*(RAD_CONES_IN(3) +RAD_CONES_IN(4))                   353
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           353
C MIN RADIUS FURTHER FROM WAFERS                                          354
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(3)+RAD_CONES_OUT(4))                  354
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        354
C VOLUME OF WATER                                                         355
      SFEP_VOL=32                                                         355
      CSFEP_VOL='VOLUME OF WATER'                                         355
C VOLUME OF PLASTIC                                                       356
      SFEP_VOLPLAST=76.8                                                  356
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  356
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    357
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           357
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   357
C *                                                                       357
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    357
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           357
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   357
C *                                                                       358
      CALL AGSBEGM(MODULE,IPRIN)                                          358
      IF (IPRIN.LT.0) RETURN                                              358
C Filling bank SWCX - SEGMENTS OF THE WATER DISTRIBUTION PIPES            358
      BANK_TITLE='SEGMENTS OF THE WATER DISTRIBUTION PIPES'               358
      BPATH_SWCX='SWCX*'                                                  358
      IDNUM_SWCX(1)=0                                                     359
C VERSION                                                                 359
      SWCX_LAYER=1                                                        359
      CSWCX_LAYER='VERSION'                                               359
C OF THE RING IN THE Z DIRECTION                                          360
      SWCX_LENGTH=2.8                                                     360
      CSWCX_LENGTH='OF THE RING IN THE Z DIRECTION'                       360
C THICKNESS OF THE MOTHER LAYER                                           361
      SWCX_DR=0.72                                                        361
      CSWCX_DR='THICKNESS OF THE MOTHER LAYER'                            361
C INNER PLASTIC TUBE RADIUS                                               362
      SWCX_RAD=0.2                                                        362
      CSWCX_RAD='INNER PLASTIC TUBE RADIUS'                               362
C FROM THE EDGE OF THE LADDER SUPPORT, INWARD                             363
      SWCX_OFFSET=-2.0                                                    363
      CSWCX_OFFSET='FROM THE EDGE OF THE LADDER SUPPORT, INWARD'          363
C RADIAL OFFSET                                                           364
      SWCX_ROFFSET=1.0                                                    364
      CSWCX_ROFFSET='RADIAL OFFSET'                                       364
C THICKNESS OF THE PLASTIC PIPE WALL                                      365
      SWCX_WALL=0.16                                                      365
      CSWCX_WALL='THICKNESS OF THE PLASTIC PIPE WALL'                     365
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    366
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           366
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   366
C *                                                                       366
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    366
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           366
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   366
C *                                                                       367
      CALL AGSBEGM(MODULE,IPRIN)                                          367
      IF (IPRIN.LT.0) RETURN                                              367
C Filling bank SOUP - MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS      367
      BANK_TITLE='MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS'         367
      BPATH_SOUP='SOUP*'                                                  367
      IDNUM_SOUP(1)=0                                                     368
C VERSION                                                                 368
      SOUP_VERSION=1                                                      368
      CSOUP_VERSION='VERSION'                                             368
C LENGTH                                                                  369
      SOUP_LENGTH=82.5                                                    369
      CSOUP_LENGTH='LENGTH'                                               369
C OUTER RADIUS OF THE SHIELD                                              370
      SOUP_ROUT=19.41                                                     370
      CSOUP_ROUT='OUTER RADIUS OF THE SHIELD'                             370
C DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF THE MOTHER)     371
      SOUP_DR=0.711                                                       371
      CSOUP_DR='DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF   371
     *THE MOTHER)'                                                        371
C STARTING ANGLE OF THE MOTHER                                            372
      SOUP_PHI1=0.0                                                       372
      CSOUP_PHI1='STARTING ANGLE OF THE MOTHER'                           372
C FINISHING ANGLE OF THE MOTHER                                           373
      SOUP_PHI2=70.0                                                      373
      CSOUP_PHI2='FINISHING ANGLE OF THE MOTHER'                          373
C OUTER DIAM OF THE CARBON TUBE                                           374
      SOUP_DIAMOUT=0.711                                                  374
      CSOUP_DIAMOUT='OUTER DIAM OF THE CARBON TUBE'                       374
C INNER DIAM OF THE CARBON TUBE                                           375
      SOUP_DIAMIN=0.620                                                   375
      CSOUP_DIAMIN='INNER DIAM OF THE CARBON TUBE'                        375
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    376
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           376
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   376
C *                                                                       376
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    376
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           376
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   376
C use default SVTG bank                                                   377
      BPATH_SVTG='SVTG*'                                                  377
      IDNUM_SVTG(1)=0                                                     377
      CALL AGDGETP('SVTTGEO7','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,  377
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      377
     *LINK_SVTG,FLAG_SVTG)                                                377
C use default SWCA bank                                                   378
      BPATH_SWCA='SWCA*'                                                  378
      IDNUM_SWCA(1)=0                                                     378
      CALL AGDGETP('SVTTGEO7','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,  378
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      378
     *LINK_SWCA,FLAG_SWCA)                                                378
C use default SELC bank                                                   379
      BPATH_SELC='SELC*'                                                  379
      IDNUM_SELC(1)=0                                                     379
      CALL AGDGETP('SVTTGEO7','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  379
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      379
     *LINK_SELC,FLAG_SELC)                                                379
C use SSUP bank                                                           380
      BPATH_SSUP='SSUP*'                                                  380
      IDNUM_SSUP(1)=0                                                     380
      SSUP_VERSION=SVTG_CONEVER                                           380
      CALL AGDGETP('SVTTGEO7','*',-999,'SSUP',NSSUP_VERSION,              380
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        380
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   380
C use default SSUB bank                                                   381
      BPATH_SSUB='SSUB*'                                                  381
      IDNUM_SSUB(1)=0                                                     381
      CALL AGDGETP('SVTTGEO7','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,  381
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      381
     *LINK_SSUB,FLAG_SSUB)                                                381
C use default SERG bank                                                   382
      BPATH_SERG='SERG*'                                                  382
      IDNUM_SERG(1)=0                                                     382
      CALL AGDGETP('SVTTGEO7','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,  382
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      382
     *LINK_SERG,FLAG_SERG)                                                382
C use default SWAM bank                                                   383
      BPATH_SWAM='SWAM*'                                                  383
      IDNUM_SWAM(1)=0                                                     383
      CALL AGDGETP('SVTTGEO7','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,  383
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      383
     *LINK_SWAM,FLAG_SWAM)                                                383
C use default SELC bank                                                   384
      BPATH_SELC='SELC*'                                                  384
      IDNUM_SELC(1)=0                                                     384
      CALL AGDGETP('SVTTGEO7','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  384
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      384
     *LINK_SELC,FLAG_SELC)                                                384
C use SSLD bank                                                           385
      BPATH_SSLD='SSLD*'                                                  385
      IDNUM_SSLD(1)=0                                                     385
      SSLD_VERSION=SVTG_SUPPORTVER                                        385
      CALL AGDGETP('SVTTGEO7','*',-999,'SSLD',NSSLD_VERSION,              385
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        385
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   385
C use default SCBP bank                                                   386
      BPATH_SCBP='SCBP*'                                                  386
      IDNUM_SCBP(1)=0                                                     386
      CALL AGDGETP('SVTTGEO7','*',-999,'SCBP',' ',BANK_SCBP, BPATH_SCBP,  386
     *IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,LLEN_SCBP,      386
     *LINK_SCBP,FLAG_SCBP)                                                386
C use default SWCX bank                                                   387
      BPATH_SWCX='SWCX*'                                                  387
      IDNUM_SWCX(1)=0                                                     387
      CALL AGDGETP('SVTTGEO7','*',-999,'SWCX',' ',BANK_SWCX, BPATH_SWCX,  387
     *IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,LLEN_SWCX,      387
     *LINK_SWCX,FLAG_SWCX)                                                387
C use default SOUP bank                                                   388
      BPATH_SOUP='SOUP*'                                                  388
      IDNUM_SOUP(1)=0                                                     388
      CALL AGDGETP('SVTTGEO7','*',-999,'SOUP',' ',BANK_SOUP, BPATH_SOUP,  388
     *IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,LLEN_SOUP,      388
     *LINK_SOUP,FLAG_SOUP)                                                388
C Check SVTG_SUPPORTVER==2                                                389
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      389
      SVTG_RSIZEMIN = 6.0                                                 390
      ENDIF                                                               391
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             392
      CALL AGSRESET                                                       392
      AG_A = 28.08                                                        392
      AG_Z = 14                                                           392
      AG_W = 0.6*1*28./60.                                                392
      AG_TITLE = 'SI'                                                     392
      AG_EXNAME = 'COMPONENT'                                             392
      AG_PARLIST = 'A_Z_W'                                                392
      CALL AXCOMPONENT                                                    392
C *                                                                       392
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  392
C =                                                                       392
      CALL AGSRESET                                                       393
      AG_A = 16                                                           393
      AG_Z = 8                                                            393
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               393
      AG_TITLE = 'O'                                                      393
      AG_EXNAME = 'COMPONENT'                                             393
      AG_PARLIST = 'A_Z_W'                                                393
      CALL AXCOMPONENT                                                    393
C *                                                                       393
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 393
      CALL AGSRESET                                                       394
      AG_A = 12                                                           394
      AG_Z = 6                                                            394
      AG_W = 0.4*8*12./174.                                               394
      AG_TITLE = 'C'                                                      394
      AG_EXNAME = 'COMPONENT'                                             394
      AG_PARLIST = 'A_Z_W'                                                394
      CALL AXCOMPONENT                                                    394
C *                                                                       394
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  394
      CALL AGSRESET                                                       395
      AG_A = 1                                                            395
      AG_Z = 1                                                            395
      AG_W = 0.4*14*1./174.                                               395
      AG_TITLE = 'H'                                                      395
      AG_EXNAME = 'COMPONENT'                                             395
      AG_PARLIST = 'A_Z_W'                                                395
      CALL AXCOMPONENT                                                    395
C *                                                                       395
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  395
      CALL AGSRESET                                                       396
      AG_DENS = 1.7                                                       396
      AG_TITLE = 'G10'                                                    396
      AG_EXNAME = 'MIXTURE'                                               396
      AG_PARLIST = 'DENS'                                                 396
      CALL AXMIXTURE                                                      396
C *                                                                       396
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       396
      CALL AGSRESET                                                       397
      AG_A = 12                                                           397
      AG_Z = 6                                                            397
      AG_W = 10000*5                                                      397
      AG_TITLE = 'C5'                                                     397
      AG_EXNAME = 'COMPONENT'                                             397
      AG_PARLIST = 'A_Z_W'                                                397
      CALL AXCOMPONENT                                                    397
C *                                                                       397
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        397
      CALL AGSRESET                                                       398
      AG_A = 1                                                            398
      AG_Z = 1                                                            398
      AG_W = 10000*4                                                      398
      AG_TITLE = 'H4'                                                     398
      AG_EXNAME = 'COMPONENT'                                             398
      AG_PARLIST = 'A_Z_W'                                                398
      CALL AXCOMPONENT                                                    398
C *                                                                       398
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       398
      CALL AGSRESET                                                       399
      AG_A = 16                                                           399
      AG_Z = 8                                                            399
      AG_W = 10000*2                                                      399
      AG_TITLE = 'O2'                                                     399
      AG_EXNAME = 'COMPONENT'                                             399
      AG_PARLIST = 'A_Z_W'                                                399
      CALL AXCOMPONENT                                                    399
C *                                                                       399
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 399
      CALL AGSRESET                                                       400
      AG_A = 27                                                           400
      AG_Z = 13                                                           400
      AG_W = 10000*0.2302                                                 400
      AG_TITLE = 'AL'                                                     400
      AG_EXNAME = 'COMPONENT'                                             400
      AG_PARLIST = 'A_Z_W'                                                400
      CALL AXCOMPONENT                                                    400
C *                                                                       400
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              400
      CALL AGSRESET                                                       401
      AG_DENS = 1.432                                                     401
      AG_TITLE = 'ALKAP'                                                  401
      AG_EXNAME = 'MIXTURE'                                               401
      AG_PARLIST = 'DENS'                                                 401
      CALL AXMIXTURE                                                      401
C *                                                                       401
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       401
      CALL AGSRESET                                                       402
      AG_A = 12                                                           402
      AG_Z = 6                                                            402
      AG_W = 10000*5                                                      402
      AG_TITLE = 'C5'                                                     402
      AG_EXNAME = 'COMPONENT'                                             402
      AG_PARLIST = 'A_Z_W'                                                402
      CALL AXCOMPONENT                                                    402
C *                                                                       402
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        402
      CALL AGSRESET                                                       403
      AG_A = 1                                                            403
      AG_Z = 1                                                            403
      AG_W = 10000*4                                                      403
      AG_TITLE = 'H4'                                                     403
      AG_EXNAME = 'COMPONENT'                                             403
      AG_PARLIST = 'A_Z_W'                                                403
      CALL AXCOMPONENT                                                    403
C *                                                                       403
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       403
      CALL AGSRESET                                                       404
      AG_A = 16                                                           404
      AG_Z = 8                                                            404
      AG_W = 10000*2                                                      404
      AG_TITLE = 'O2'                                                     404
      AG_EXNAME = 'COMPONENT'                                             404
      AG_PARLIST = 'A_Z_W'                                                404
      CALL AXCOMPONENT                                                    404
C *                                                                       404
C Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                          404
      CALL AGSRESET                                                       405
      AG_A = 27                                                           405
      AG_Z = 13                                                           405
      AG_W = 986                                                          405
      AG_TITLE = 'AL'                                                     405
      AG_EXNAME = 'COMPONENT'                                             405
      AG_PARLIST = 'A_Z_W'                                                405
      CALL AXCOMPONENT                                                    405
C *                                                                       405
C Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                          405
      CALL AGSRESET                                                       406
      AG_DENS = 1.40845                                                   406
      AG_TITLE = 'SSDALMY'                                                406
      AG_EXNAME = 'MIXTURE'                                               406
      AG_PARLIST = 'DENS'                                                 406
      CALL AXMIXTURE                                                      406
C *                                                                       406
C Check SWAM_LEN>0                                                        406
      IF (SWAM_LEN.GT.0) THEN                                             407
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              408
      CALL AGSRESET                                                       408
      AG_A = 1                                                            408
      AG_Z = 1                                                            408
      AG_W = 2                                                            408
      AG_TITLE = 'H2'                                                     408
      AG_EXNAME = 'COMPONENT'                                             408
      AG_PARLIST = 'A_Z_W'                                                408
      CALL AXCOMPONENT                                                    408
C *                                                                       408
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              408
      CALL AGSRESET                                                       409
      AG_A = 16                                                           409
      AG_Z = 8                                                            409
      AG_W = 1                                                            409
      AG_TITLE = 'O'                                                      409
      AG_EXNAME = 'COMPONENT'                                             409
      AG_PARLIST = 'A_Z_W'                                                409
      CALL AXCOMPONENT                                                    409
C *                                                                       409
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                409
      CALL AGSRESET                                                       410
      AG_DENS = 1.0                                                       410
      AG_TITLE = 'WATER'                                                  410
      AG_EXNAME = 'MIXTURE'                                               410
      AG_PARLIST = 'DENS'                                                 410
      CALL AXMIXTURE                                                      410
C *                                                                       410
      ELSE                                                                410
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             412
      CALL AGSRESET                                                       412
      AG_DENS = 0.0009                                                    412
      AG_TITLE = 'WATER'                                                  412
      AG_EXNAME = 'MIXTURE'                                               412
      AG_PARLIST = 'DENS'                                                 412
      CALL AXMIXTURE                                                      412
C *                                                                       412
      ENDIF                                                               412
C Check SVTG_IFMANY>0                                                     414
      IF (SVTG_IFMANY.GT.0) THEN                                          414
      CALL AGSBEGM(MODULE,IPRIN)                                          415
C Check IPRIN<0                                                           415
      IF (IPRIN.LT.0) RETURN                                              415
C Create SVTT :                                                           415
C Call To Local Procedure SVTT                                            415
      ASSIGN 5030 TO BLOCK_SVTT                                           415
      GO TO  5020                                                         415
5030  CONTINUE                                                            415
C *                                                                       415
C Geant POSITION : SVTT ===> MOTHER='CAVE' KONLY='MANY' <===              416
      CALL AGSRESET                                                       416
      AG_MOTHER = 'CAVE'                                                  416
      AG_KONLY = 'MANY'                                                   416
      AG_TITLE = 'SVTT'                                                   416
      AG_EXNAME = 'POSITION'                                              416
      AG_PARLIST = 'MOTHER_KONLY'                                         416
      CALL AXPOSITION                                                     416
C *                                                                       416
      ELSE                                                                416
      CALL AGSBEGM(MODULE,IPRIN)                                          418
C Check IPRIN<0                                                           418
      IF (IPRIN.LT.0) RETURN                                              418
C Create SVTT :                                                           418
C Call To Local Procedure SVTT                                            418
      ASSIGN 5040 TO BLOCK_SVTT                                           418
      GO TO  5020                                                         418
5040  CONTINUE                                                            418
C *                                                                       418
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           419
      CALL AGSRESET                                                       419
      AG_MOTHER = 'CAVE'                                                  419
      AG_TITLE = 'SVTT'                                                   419
      AG_EXNAME = 'POSITION'                                              419
      AG_PARLIST = 'MOTHER'                                               419
      CALL AXPOSITION                                                     419
C *                                                                       419
      ENDIF                                                               419
C ----------------------------------------------------------------------  421
C ------------ SVTT                                                       421
      CALL AGSENDM                                                        422
      RETURN                                                              422
C Begin Local Procedure SVTT                                              422
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   422
5020  CONTINUE                                                            422
         AG_TITLE='SVTT'                                                  422
         AG_MODULE=MODULE                                                 422
         AG_MARK='V00'                                                    422
         CALL AGSPUSH                                                     422
         AG_MODULE=MODULE                                                 422
C    *                                                                    422
         RSIZEMAX=SSUP_CON4IDMX                                           423
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 424
     *   2.0*SSUP_HOSRMX                                                  424
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                425
         CLEARANCE=SVTG_RSIZEMAX - RSIZEMAX                               426
C    Geant MATERIAL : AIR ===> inherited arguments used <===              427
         CALL AGSRESET                                                    427
         AG_TITLE = 'AIR'                                                 427
         AG_EXNAME = 'MATERIAL'                                           427
         AG_PARLIST = 'NONE'                                              427
         CALL AXMATERIAL                                                  427
C    *                                                                    427
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       427
         CALL AGSRESET                                                    428
         AG_SEEN = 0                                                      428
         AG_COLO = 1                                                      428
         AG_TITLE = 'SVTT'                                                428
         AG_EXNAME = 'ATTRIBUTE'                                          428
         AG_PARLIST = 'SEEN_COLO'                                         428
         CALL AXATTRIBUTE                                                 428
C    *                                                                    428
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  428
C    VTG_ZSIZEMAX <===                                                    428
         CALL AGSRESET                                                    430
         AG_RMIN = SVTG_RSIZEMIN                                          430
         AG_RMAX = SVTG_RSIZEMAX                                          430
         AG_DZ = SVTG_ZSIZEMAX                                            430
         AG_TITLE = 'TUBE'                                                430
         AG_EXNAME = 'SHAPE'                                              430
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      430
         CALL AXSHAPE                                                     430
C    *                                                                    430
C    Create SIRP :                                                        431
C    Call To Local Procedure SIRP                                         431
         ASSIGN 5060 TO BLOCK_SIRP                                        431
         GO TO  5050                                                      431
5060     CONTINUE                                                         431
C    *                                                                    431
C    Geant POSITION : SIRP ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  432
C    2.5 <===                                                             432
         CALL AGSRESET                                                    432
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             432
         AG_ALPHAN=AG_ALPHAN+1                                            432
         AG_ALPHAK(AG_ALPHAN)=3                                           432
         AG_ALPHAL(AG_ALPHAN)=22.5                                        432
         AG_TITLE = 'SIRP'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'Z_ALPHAZ'                                          432
         CALL AXPOSITION                                                  432
C    *                                                                    432
C    Geant POSITION : SIRP ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  432
C    22.5 <===                                                            432
         CALL AGSRESET                                                    433
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            433
         AG_ALPHAN=AG_ALPHAN+1                                            433
         AG_ALPHAK(AG_ALPHAN)=3                                           433
         AG_ALPHAL(AG_ALPHAN)=22.5                                        433
         AG_TITLE = 'SIRP'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'Z_ALPHAZ'                                          433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Create SIRT :                                                        434
C    Call To Local Procedure SIRT                                         434
         ASSIGN 5080 TO BLOCK_SIRT                                        434
         GO TO  5070                                                      434
5080     CONTINUE                                                         434
C    *                                                                    434
C    Geant POSITION : SIRT ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  435
C    2.5 <===                                                             435
         CALL AGSRESET                                                    435
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             435
         AG_ALPHAN=AG_ALPHAN+1                                            435
         AG_ALPHAK(AG_ALPHAN)=3                                           435
         AG_ALPHAL(AG_ALPHAN)=22.5                                        435
         AG_TITLE = 'SIRT'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'Z_ALPHAZ'                                          435
         CALL AXPOSITION                                                  435
C    *                                                                    435
C    Geant POSITION : SIRT ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  435
C    22.5 <===                                                            435
         CALL AGSRESET                                                    436
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            436
         AG_ALPHAN=AG_ALPHAN+1                                            436
         AG_ALPHAK(AG_ALPHAN)=3                                           436
         AG_ALPHAL(AG_ALPHAN)=22.5                                        436
         AG_TITLE = 'SIRT'                                                436
         AG_EXNAME = 'POSITION'                                           436
         AG_PARLIST = 'Z_ALPHAZ'                                          436
         CALL AXPOSITION                                                  436
C    *                                                                    436
C    Create SOER :                                                        437
C    Call To Local Procedure SOER                                         437
         ASSIGN 5100 TO BLOCK_SOER                                        437
         GO TO  5090                                                      437
5100     CONTINUE                                                         437
C    *                                                                    437
C    Geant POSITION : SOER ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 <===      438
         CALL AGSRESET                                                    438
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             438
         AG_TITLE = 'SOER'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'Z'                                                 438
         CALL AXPOSITION                                                  438
C    *                                                                    438
C    Geant POSITION : SOER ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 <===     438
         CALL AGSRESET                                                    439
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            439
         AG_TITLE = 'SOER'                                                439
         AG_EXNAME = 'POSITION'                                           439
         AG_PARLIST = 'Z'                                                 439
         CALL AXPOSITION                                                  439
C    *                                                                    439
C    Create SBRG :                                                        440
C    Call To Local Procedure SBRG                                         440
         ASSIGN 5120 TO BLOCK_SBRG                                        440
         GO TO  5110                                                      440
5120     CONTINUE                                                         440
C    *                                                                    440
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  441
C    /2 <===                                                              441
         CALL AGSRESET                                                    441
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  441
         AG_TITLE = 'SBRG'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'Z'                                                 441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  441
C    K/2 <===                                                             441
         CALL AGSRESET                                                    442
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 442
         AG_TITLE = 'SBRG'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'Z'                                                 442
         CALL AXPOSITION                                                  442
C    *                                                                    442
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           442
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             444
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      445
C    Create SOES :                                                        446
C    Call To Local Procedure SOES                                         446
         ASSIGN 5140 TO BLOCK_SOES                                        446
         GO TO  5130                                                      446
5140     CONTINUE                                                         446
C    *                                                                    446
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               447
         CALL AGSRESET                                                    447
         AG_Z = SCREW_Z                                                   447
         AG_KONLY = 'MANY'                                                447
         AG_TITLE = 'SOES'                                                447
         AG_EXNAME = 'POSITION'                                           447
         AG_PARLIST = 'Z_KONLY'                                           447
         CALL AXPOSITION                                                  447
C    *                                                                    447
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              447
         CALL AGSRESET                                                    448
         AG_Z = -SCREW_Z                                                  448
         AG_KONLY = 'MANY'                                                448
         AG_TITLE = 'SOES'                                                448
         AG_EXNAME = 'POSITION'                                           448
         AG_PARLIST = 'Z_KONLY'                                           448
         CALL AXPOSITION                                                  448
C    *                                                                    448
C    Create SIES :                                                        449
C    Call To Local Procedure SIES                                         449
         ASSIGN 5160 TO BLOCK_SIES                                        449
         GO TO  5150                                                      449
5160     CONTINUE                                                         449
C    *                                                                    449
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               450
         CALL AGSRESET                                                    450
         AG_Z = SCREW_Z                                                   450
         AG_KONLY = 'MANY'                                                450
         AG_TITLE = 'SIES'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'Z_KONLY'                                           450
         CALL AXPOSITION                                                  450
C    *                                                                    450
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              450
         CALL AGSRESET                                                    451
         AG_Z = -SCREW_Z                                                  451
         AG_KONLY = 'MANY'                                                451
         AG_TITLE = 'SIES'                                                451
         AG_EXNAME = 'POSITION'                                           451
         AG_PARLIST = 'Z_KONLY'                                           451
         CALL AXPOSITION                                                  451
C    *                                                                    451
C    Check SWAM_LEN>0                                                     451
         IF (SWAM_LEN.GT.0) THEN                                          452
C    Create SWMM :                                                        453
C    Call To Local Procedure SWMM                                         453
         ASSIGN 5180 TO BLOCK_SWMM                                        453
         GO TO  5170                                                      453
5180     CONTINUE                                                         453
C    *                                                                    453
C    Geant POSITION : SWMM ===> Z=SWAM_ZMIN+SWAM_LEN/2 <===               454
         CALL AGSRESET                                                    454
         AG_Z = SWAM_ZMIN+SWAM_LEN/2                                      454
         AG_TITLE = 'SWMM'                                                454
         AG_EXNAME = 'POSITION'                                           454
         AG_PARLIST = 'Z'                                                 454
         CALL AXPOSITION                                                  454
C    *                                                                    454
C    Geant POSITION : SWMM ===> Z=-SWAM_ZMIN-SWAM_LEN/2 <===              454
         CALL AGSRESET                                                    455
         AG_Z = -SWAM_ZMIN-SWAM_LEN/2                                     455
         AG_TITLE = 'SWMM'                                                455
         AG_EXNAME = 'POSITION'                                           455
         AG_PARLIST = 'Z'                                                 455
         CALL AXPOSITION                                                  455
C    *                                                                    455
         ENDIF                                                            455
C    Check SWAM_LEN>0                                                     457
         IF (SWAM_LEN.GT.0) THEN                                          457
C    Create SBWC :                                                        458
C    Call To Local Procedure SBWC                                         458
         ASSIGN 5200 TO BLOCK_SBWC                                        458
         GO TO  5190                                                      458
5200     CONTINUE                                                         458
C    *                                                                    458
C    Geant POSITION : SBWC ===> Z=(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SW  459
C    AM_ZMIN+SWAM_LEN))/2) <===                                           459
         CALL AGSRESET                                                    459
         AG_Z =                                                           459
     *   (SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)      459
         AG_TITLE = 'SBWC'                                                459
         AG_EXNAME = 'POSITION'                                           459
         AG_PARLIST = 'Z'                                                 459
         CALL AXPOSITION                                                  459
C    *                                                                    459
C    Geant POSITION : SBWC ===> Z=-(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(S  459
C    WAM_ZMIN+SWAM_LEN))/2) THETAZ=180 <===                               459
         CALL AGSRESET                                                    461
         AG_Z =                                                           461
     *   -(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)     461
         AG_THETAZ = 180                                                  461
         AG_TITLE = 'SBWC'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'Z_THETAZ'                                          461
         CALL AXPOSITION                                                  461
C    *                                                                    461
         ENDIF                                                            461
C    Create SCON :                                                        463
C    Call To Local Procedure SCON                                         463
         ASSIGN 5220 TO BLOCK_SCON                                        463
         GO TO  5210                                                      463
5220     CONTINUE                                                         463
C    *                                                                    463
C    Geant POSITION : SCON ===> inherited arguments used <===             464
         CALL AGSRESET                                                    464
         AG_TITLE = 'SCON'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'NONE'                                              464
         CALL AXPOSITION                                                  464
C    *                                                                    464
C    Geant POSITION : SCON ===> THETAZ=180 <===                           464
         CALL AGSRESET                                                    465
         AG_THETAZ = 180                                                  465
         AG_TITLE = 'SCON'                                                465
         AG_EXNAME = 'POSITION'                                           465
         AG_PARLIST = 'THETAZ'                                            465
         CALL AXPOSITION                                                  465
C    *                                                                    465
C    Create SBSP :                                                        466
C    Call To Local Procedure SBSP                                         466
         ASSIGN 5240 TO BLOCK_SBSP                                        466
         GO TO  5230                                                      466
5240     CONTINUE                                                         466
C    *                                                                    466
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     467
         CALL AGSRESET                                                    467
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            467
         AG_TITLE = 'SBSP'                                                467
         AG_EXNAME = 'POSITION'                                           467
         AG_PARLIST = 'Z'                                                 467
         CALL AXPOSITION                                                  467
C    *                                                                    467
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  467
C    =180 <===                                                            467
         CALL AGSRESET                                                    468
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           468
         AG_THETAZ = 180                                                  468
         AG_TITLE = 'SBSP'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'Z_THETAZ'                                          468
         CALL AXPOSITION                                                  468
C    *                                                                    468
C    Create SROD :                                                        469
C    Call To Local Procedure SROD                                         469
         ASSIGN 5260 TO BLOCK_SROD                                        469
         GO TO  5250                                                      469
5260     CONTINUE                                                         469
C    *                                                                    469
C    Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===          470
         CALL AGSRESET                                                    470
         AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                 470
         AG_TITLE = 'SROD'                                                470
         AG_EXNAME = 'POSITION'                                           470
         AG_PARLIST = 'Y'                                                 470
         CALL AXPOSITION                                                  470
C    *                                                                    470
C    Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===         470
         CALL AGSRESET                                                    471
         AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                471
         AG_TITLE = 'SROD'                                                471
         AG_EXNAME = 'POSITION'                                           471
         AG_PARLIST = 'Y'                                                 471
         CALL AXPOSITION                                                  471
C    *                                                                    471
         RADMAX=SVTG_RSIZEMAX                                             471
C    *                                                                    473
         ILAYER=SVTG_NMIN                                                 473
            GO TO 5273                                                    473
5271        ILAYER=ILAYER+1                                               473
5273        IF(ILAYER-(MIN(6,SVTG_NLAYER)).GT.0) GO TO 5272               473
C       Check ILAYER<6                                                    474
            IF (ILAYER.LT.6) THEN                                         474
C       use SVTL bank                                                     475
            BPATH_SVTL='SVTL*'                                            475
            IDNUM_SVTL(1)=0                                               475
            SVTL_LAYER=ILAYER+1                                           475
            CALL AGDGETP('SVTTGEO7','*',-999,'SVTL',NSVTL_LAYER,          475
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    475
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             475
            RADMAX=SVTL_RADIUS                                            476
            ELSE                                                          477
            RADMAX=SVTG_RSIZEMAX                                          478
            ENDIF                                                         479
C       use SVTL bank                                                     480
            BPATH_SVTL='SVTL*'                                            480
            IDNUM_SVTL(1)=0                                               480
            SVTL_LAYER=ILAYER                                             480
            CALL AGDGETP('SVTTGEO7','*',-999,'SVTL',NSVTL_LAYER,          480
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    480
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             480
C       Create SLYD :                                                     481
C       Call To Local Procedure SLYD                                      481
            ASSIGN 5290 TO BLOCK_SLYD                                     481
            GO TO  5280                                                   481
5290        CONTINUE                                                      481
C       *                                                                 481
C       Check ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6                   482
            IF (ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6) THEN           482
C       Geant POSITION : SLYD ===> KONLY='MANY' <===                      483
            CALL AGSRESET                                                 483
            AG_KONLY = 'MANY'                                             483
            AG_TITLE = 'SLYD'                                             483
            AG_EXNAME = 'POSITION'                                        483
            AG_PARLIST = 'KONLY'                                          483
            CALL AXPOSITION                                               483
C       *                                                                 483
            ELSE                                                          483
C       Geant POSITION : SLYD ===> inherited arguments used <===          485
            CALL AGSRESET                                                 485
            AG_TITLE = 'SLYD'                                             485
            AG_EXNAME = 'POSITION'                                        485
            AG_PARLIST = 'NONE'                                           485
            CALL AXPOSITION                                               485
C       *                                                                 485
            ENDIF                                                         485
         GO TO 5271                                                       487
5272     CONTINUE                                                         487
C    Check SVTG_NLAYER<0                                                  488
         IF (SVTG_NLAYER.LT.0) THEN                                       488
C    use SVTL bank                                                        489
         BPATH_SVTL='SVTL*'                                               489
         IDNUM_SVTL(1)=0                                                  489
         SVTL_LAYER=-SVTG_NLAYER                                          489
         CALL AGDGETP('SVTTGEO7','*',-999,'SVTL',NSVTL_LAYER,             489
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       489
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                489
C    Create SLYD :                                                        490
C    Call To Local Procedure SLYD                                         490
         ASSIGN 5300 TO BLOCK_SLYD                                        490
         GO TO  5280                                                      490
5300     CONTINUE                                                         490
C    *                                                                    490
C    Geant POSITION : SLYD ===> inherited arguments used <===             491
         CALL AGSRESET                                                    491
         AG_TITLE = 'SLYD'                                                491
         AG_EXNAME = 'POSITION'                                           491
         AG_PARLIST = 'NONE'                                              491
         CALL AXPOSITION                                                  491
C    *                                                                    491
         ENDIF                                                            491
C    Create SISH :                                                        493
C    Call To Local Procedure SISH                                         493
         ASSIGN 5320 TO BLOCK_SISH                                        493
         GO TO  5310                                                      493
5320     CONTINUE                                                         493
C    *                                                                    493
C    Geant POSITION : SISH ===> inherited arguments used <===             494
         CALL AGSRESET                                                    494
         AG_TITLE = 'SISH'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'NONE'                                              494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Create SOSH :                                                        495
C    Call To Local Procedure SOSH                                         495
         ASSIGN 5340 TO BLOCK_SOSH                                        495
         GO TO  5330                                                      495
5340     CONTINUE                                                         495
C    *                                                                    495
C    Geant POSITION : SOSH ===> inherited arguments used <===             496
         CALL AGSRESET                                                    496
         AG_TITLE = 'SOSH'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'NONE'                                              496
         CALL AXPOSITION                                                  496
C    *                                                                    496
C    Check SVTG_NLAYER>6                                                  496
         IF (SVTG_NLAYER.GT.6) THEN                                       497
C    Create SSSH :                                                        498
C    Call To Local Procedure SSSH                                         498
         ASSIGN 5360 TO BLOCK_SSSH                                        498
         GO TO  5350                                                      498
5360     CONTINUE                                                         498
C    *                                                                    498
C    Geant POSITION : SSSH ===> inherited arguments used <===             499
         CALL AGSRESET                                                    499
         AG_TITLE = 'SSSH'                                                499
         AG_EXNAME = 'POSITION'                                           499
         AG_PARLIST = 'NONE'                                              499
         CALL AXPOSITION                                                  499
C    *                                                                    499
         ENDIF                                                            499
C    Create SALM :                                                        501
C    Call To Local Procedure SALM                                         501
         ASSIGN 5380 TO BLOCK_SALM                                        501
         GO TO  5370                                                      501
5380     CONTINUE                                                         501
C    *                                                                    501
C    Geant POSITION : SALM ===> Z=SSLD_ALMSHPOS-SSLD_ALMSHTHK/2 <===      502
         CALL AGSRESET                                                    502
         AG_Z = SSLD_ALMSHPOS-SSLD_ALMSHTHK/2                             502
         AG_TITLE = 'SALM'                                                502
         AG_EXNAME = 'POSITION'                                           502
         AG_PARLIST = 'Z'                                                 502
         CALL AXPOSITION                                                  502
C    *                                                                    502
C    Geant POSITION : SALM ===> Z=-(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2) <===   502
         CALL AGSRESET                                                    503
         AG_Z = -(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2)                          503
         AG_TITLE = 'SALM'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'Z'                                                 503
         CALL AXPOSITION                                                  503
C    *                                                                    503
C    Create SCBM :                                                        504
C    Call To Local Procedure SCBM                                         504
         ASSIGN 5400 TO BLOCK_SCBM                                        504
         GO TO  5390                                                      504
5400     CONTINUE                                                         504
C    *                                                                    504
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=SWAM_ZMIN-SCBP_LEN <===     505
         CALL AGSRESET                                                    505
         AG_X = 0.0                                                       505
         AG_Y = 0.0                                                       505
         AG_Z = SWAM_ZMIN-SCBP_LEN                                        505
         AG_TITLE = 'SCBM'                                                505
         AG_EXNAME = 'POSITION'                                           505
         AG_PARLIST = 'X_Y_Z'                                             505
         CALL AXPOSITION                                                  505
C    *                                                                    505
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=-(SWAM_ZMIN-SCBP_LEN) THET  505
C    AZ=180.0 <===                                                        505
         CALL AGSRESET                                                    506
         AG_X = 0.0                                                       506
         AG_Y = 0.0                                                       506
         AG_Z = -(SWAM_ZMIN-SCBP_LEN)                                     506
         AG_THETAZ = 180.0                                                506
         AG_TITLE = 'SCBM'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      506
         CALL AXPOSITION                                                  506
C    *                                                                    506
C    use SVTL bank                                                        506
         BPATH_SVTL='SVTL*'                                               507
         IDNUM_SVTL(1)=0                                                  507
         SVTL_LAYER=6                                                     507
         CALL AGDGETP('SVTTGEO7','*',-999,'SVTL',NSVTL_LAYER,             507
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       507
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                507
C    Create SXRL :                                                        508
C    Call To Local Procedure SXRL                                         508
         ASSIGN 5420 TO BLOCK_SXRL                                        508
         GO TO  5410                                                      508
5420     CONTINUE                                                         508
C    *                                                                    508
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  510
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  510
         CALL AGSRESET                                                    510
         AG_X = 0.0                                                       510
         AG_Y = 0.0                                                       510
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               510
         AG_KONLY = 'MANY'                                                510
         AG_TITLE = 'SXRL'                                                510
         AG_EXNAME = 'POSITION'                                           510
         AG_PARLIST = 'X_Y_Z_KONLY'                                       510
         CALL AXPOSITION                                                  510
C    *                                                                    510
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  510
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 510
         CALL AGSRESET                                                    512
         AG_X = 0.0                                                       512
         AG_Y = 0.0                                                       512
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              512
         AG_KONLY = 'MANY'                                                512
         AG_TITLE = 'SXRL'                                                512
         AG_EXNAME = 'POSITION'                                           512
         AG_PARLIST = 'X_Y_Z_KONLY'                                       512
         CALL AXPOSITION                                                  512
C    *                                                                    512
C    use SVTL bank                                                        512
         BPATH_SVTL='SVTL*'                                               513
         IDNUM_SVTL(1)=0                                                  513
         SVTL_LAYER=4                                                     513
         CALL AGDGETP('SVTTGEO7','*',-999,'SVTL',NSVTL_LAYER,             513
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       513
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                513
C    Create SXRL :                                                        514
C    Call To Local Procedure SXRL                                         514
         ASSIGN 5430 TO BLOCK_SXRL                                        514
         GO TO  5410                                                      514
5430     CONTINUE                                                         514
C    *                                                                    514
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  516
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  516
         CALL AGSRESET                                                    516
         AG_X = 0.0                                                       516
         AG_Y = 0.0                                                       516
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               516
         AG_KONLY = 'MANY'                                                516
         AG_TITLE = 'SXRL'                                                516
         AG_EXNAME = 'POSITION'                                           516
         AG_PARLIST = 'X_Y_Z_KONLY'                                       516
         CALL AXPOSITION                                                  516
C    *                                                                    516
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  516
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 516
         CALL AGSRESET                                                    518
         AG_X = 0.0                                                       518
         AG_Y = 0.0                                                       518
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              518
         AG_KONLY = 'MANY'                                                518
         AG_TITLE = 'SXRL'                                                518
         AG_EXNAME = 'POSITION'                                           518
         AG_PARLIST = 'X_Y_Z_KONLY'                                       518
         CALL AXPOSITION                                                  518
C    *                                                                    518
C    use SVTL bank                                                        518
         BPATH_SVTL='SVTL*'                                               519
         IDNUM_SVTL(1)=0                                                  519
         SVTL_LAYER=2                                                     519
         CALL AGDGETP('SVTTGEO7','*',-999,'SVTL',NSVTL_LAYER,             519
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       519
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                519
C    Create SXRL :                                                        520
C    Call To Local Procedure SXRL                                         520
         ASSIGN 5440 TO BLOCK_SXRL                                        520
         GO TO  5410                                                      520
5440     CONTINUE                                                         520
C    *                                                                    520
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  522
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  522
         CALL AGSRESET                                                    522
         AG_X = 0.0                                                       522
         AG_Y = 0.0                                                       522
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               522
         AG_KONLY = 'MANY'                                                522
         AG_TITLE = 'SXRL'                                                522
         AG_EXNAME = 'POSITION'                                           522
         AG_PARLIST = 'X_Y_Z_KONLY'                                       522
         CALL AXPOSITION                                                  522
C    *                                                                    522
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  522
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 522
         CALL AGSRESET                                                    524
         AG_X = 0.0                                                       524
         AG_Y = 0.0                                                       524
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              524
         AG_KONLY = 'MANY'                                                524
         AG_TITLE = 'SXRL'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'X_Y_Z_KONLY'                                       524
         CALL AXPOSITION                                                  524
C    *                                                                    524
C    Create SOUM :                                                        525
C    Call To Local Procedure SOUM                                         525
         ASSIGN 5460 TO BLOCK_SOUM                                        525
         GO TO  5450                                                      525
5460     CONTINUE                                                         525
C    *                                                                    525
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 <===                    526
         CALL AGSRESET                                                    526
         AG_X = 0.0                                                       526
         AG_Y = 0.0                                                       526
         AG_Z = 0.0                                                       526
         AG_TITLE = 'SOUM'                                                526
         AG_EXNAME = 'POSITION'                                           526
         AG_PARLIST = 'X_Y_Z'                                             526
         CALL AXPOSITION                                                  526
C    *                                                                    526
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 <===       526
         CALL AGSRESET                                                    527
         AG_X = 0.0                                                       527
         AG_Y = 0.0                                                       527
         AG_Z = 0.0                                                       527
         AG_ALPHAN=AG_ALPHAN+1                                            527
         AG_ALPHAK(AG_ALPHAN)=2                                           527
         AG_ALPHAL(AG_ALPHAN)=180.0                                       527
         AG_TITLE = 'SOUM'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      527
         CALL AXPOSITION                                                  527
C    *                                                                    527
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 ALPHAZ=18  527
C    0 <===                                                               527
         CALL AGSRESET                                                    528
         AG_X = 0.0                                                       528
         AG_Y = 0.0                                                       528
         AG_Z = 0.0                                                       528
         AG_ALPHAN=AG_ALPHAN+1                                            528
         AG_ALPHAK(AG_ALPHAN)=2                                           528
         AG_ALPHAL(AG_ALPHAN)=180.0                                       528
         AG_ALPHAN=AG_ALPHAN+1                                            528
         AG_ALPHAK(AG_ALPHAN)=3                                           528
         AG_ALPHAL(AG_ALPHAN)=180                                         528
         AG_TITLE = 'SOUM'                                                528
         AG_EXNAME = 'POSITION'                                           528
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               528
         CALL AXPOSITION                                                  528
C    *                                                                    528
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAZ=180 <===         528
         CALL AGSRESET                                                    529
         AG_X = 0.0                                                       529
         AG_Y = 0.0                                                       529
         AG_Z = 0.0                                                       529
         AG_ALPHAN=AG_ALPHAN+1                                            529
         AG_ALPHAK(AG_ALPHAN)=3                                           529
         AG_ALPHAL(AG_ALPHAN)=180                                         529
         AG_TITLE = 'SOUM'                                                529
         AG_EXNAME = 'POSITION'                                           529
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      529
         CALL AXPOSITION                                                  529
C    *                                                                    529
5021  CALL AGSPOPB                                                        530
      GO TO BLOCK_SVTT, (5020 ,5030,5040)                                 530
C End of Local Procedure                                                  530
C ----------------------------------------------------------------------  531
C ------------ SXRL                                                       531
C Begin Local Procedure SXRL                                              532
C SXRL IS THE MOTHER OF THE CIRCULAR WATER PIPES                          532
5410  CONTINUE                                                            532
         AG_TITLE='SXRL'                                                  532
         AG_MODULE=MODULE                                                 532
         AG_MARK='V00'                                                    532
         CALL AGSPUSH                                                     532
         AG_MODULE=MODULE                                                 532
C    *                                                                    532
C    Geant MATERIAL : AIR ===> inherited arguments used <===              533
         CALL AGSRESET                                                    533
         AG_TITLE = 'AIR'                                                 533
         AG_EXNAME = 'MATERIAL'                                           533
         AG_PARLIST = 'NONE'                                              533
         CALL AXMATERIAL                                                  533
C    *                                                                    533
C    Geant ATTRIBUTE : SXRL ===> SEEN=0 COLO=1 <===                       533
         CALL AGSRESET                                                    534
         AG_SEEN = 0                                                      534
         AG_COLO = 1                                                      534
         AG_TITLE = 'SXRL'                                                534
         AG_EXNAME = 'ATTRIBUTE'                                          534
         AG_PARLIST = 'SEEN_COLO'                                         534
         CALL AXATTRIBUTE                                                 534
C    *                                                                    534
C    Geant SHAPE : TUBE ===> RMIN=SWCX_ROFFSET+SVTL_RADIUS, RMAX=SWCX_RO  534
C    FFSET+SVTL_RADIUS+SWCX_DR DZ=SWCX_LENGTH/2.0 <===                    534
         CALL AGSRESET                                                    536
         AG_RMIN = SWCX_ROFFSET+SVTL_RADIUS                               536
         AG_RMAX = SWCX_ROFFSET+SVTL_RADIUS+SWCX_DR                       536
         AG_DZ = SWCX_LENGTH/2.0                                          536
         AG_TITLE = 'TUBE'                                                536
         AG_EXNAME = 'SHAPE'                                              536
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      536
         CALL AXSHAPE                                                     536
C    *                                                                    536
C    Create SWRP :                                                        537
C    Call To Local Procedure SWRP                                         537
         ASSIGN 5480 TO BLOCK_SWRP                                        537
         GO TO  5470                                                      537
5480     CONTINUE                                                         537
C    *                                                                    537
C    Create SYRU :                                                        538
C    Call To Local Procedure SYRU                                         538
         ASSIGN 5500 TO BLOCK_SYRU                                        538
         GO TO  5490                                                      538
5500     CONTINUE                                                         538
C    *                                                                    538
C    *                                                                    539
         I_PHI=1                                                          539
            GO TO 5513                                                    539
5511        I_PHI=I_PHI+1                                                 539
5513        IF(I_PHI-(4*SVTL_NLADDER).GT.0) GO TO 5512                    539
            TUBE_ANGLE=(PI/(2.0*SVTL_NLADDER))*(I_PHI-0.5)                540
C       Geant POSITION : SWRP ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  542
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  542
C       SWCX_DR/2.0) Z=0.0 <===                                           542
            CALL AGSRESET                                                 542
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     542
     *      2.0)                                                          542
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     542
     *      2.0)                                                          542
            AG_Z = 0.0                                                    542
            AG_TITLE = 'SWRP'                                             542
            AG_EXNAME = 'POSITION'                                        542
            AG_PARLIST = 'X_Y_Z'                                          542
            CALL AXPOSITION                                               542
C       *                                                                 542
C       Geant POSITION : SYRU ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  542
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  542
C       SWCX_DR/2.0) Z=0.0 <===                                           542
            CALL AGSRESET                                                 544
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     544
     *      2.0)                                                          544
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     544
     *      2.0)                                                          544
            AG_Z = 0.0                                                    544
            AG_TITLE = 'SYRU'                                             544
            AG_EXNAME = 'POSITION'                                        544
            AG_PARLIST = 'X_Y_Z'                                          544
            CALL AXPOSITION                                               544
C       *                                                                 544
         GO TO 5511                                                       545
5512     CONTINUE                                                         545
5411  CALL AGSPOPB                                                        546
      GO TO BLOCK_SXRL, (5410 ,5420,5430,5440)                            546
C End of Local Procedure                                                  546
C ----------------------------------------------------------------------  547
C ------------ SWRP                                                       547
C Begin Local Procedure SWRP                                              549
C SWRP IS AN APPROXIMATION OF WATER IN THE CIRCULAR PIPE, A RECTANGULAR,  549
C  ONE                                                                    549
5470  CONTINUE                                                            549
         AG_TITLE='SWRP'                                                  549
         AG_MODULE=MODULE                                                 549
         AG_MARK='V00'                                                    549
         CALL AGSPUSH                                                     549
         AG_MODULE=MODULE                                                 549
C    *                                                                    549
C    Geant MATERIAL : WATER ===> inherited arguments used <===            550
         CALL AGSRESET                                                    550
         AG_TITLE = 'WATER'                                               550
         AG_EXNAME = 'MATERIAL'                                           550
         AG_PARLIST = 'NONE'                                              550
         CALL AXMATERIAL                                                  550
C    *                                                                    550
C    Geant ATTRIBUTE : SWRP ===> SEEN=1 COLO=4 <===                       550
         CALL AGSRESET                                                    551
         AG_SEEN = 1                                                      551
         AG_COLO = 4                                                      551
         AG_TITLE = 'SWRP'                                                551
         AG_EXNAME = 'ATTRIBUTE'                                          551
         AG_PARLIST = 'SEEN_COLO'                                         551
         CALL AXATTRIBUTE                                                 551
C    *                                                                    551
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SWCX_RAD DZ=SWCX_LENGTH/2.0 <  551
C    ===                                                                  551
         CALL AGSRESET                                                    552
         AG_RMIN = 0.0                                                    552
         AG_RMAX = SWCX_RAD                                               552
         AG_DZ = SWCX_LENGTH/2.0                                          552
         AG_TITLE = 'TUBE'                                                552
         AG_EXNAME = 'SHAPE'                                              552
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      552
         CALL AXSHAPE                                                     552
C    *                                                                    552
5471  CALL AGSPOPB                                                        553
      GO TO BLOCK_SWRP, (5470 ,5480)                                      553
C End of Local Procedure                                                  553
C ----------------------------------------------------------------------  554
C ------------ SYRU                                                       554
C Begin Local Procedure SYRU                                              555
C SYRU IS THE WALL OF THE WATER PIPE                                      555
5490  CONTINUE                                                            555
         AG_TITLE='SYRU'                                                  555
         AG_MODULE=MODULE                                                 555
         AG_MARK='V00'                                                    555
         CALL AGSPUSH                                                     555
         AG_MODULE=MODULE                                                 555
C    *                                                                    555
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           556
         CALL AGSRESET                                                    556
         AG_A = 12                                                        556
         AG_Z = 6                                                         556
         AG_W = 1                                                         556
         AG_TITLE = 'C'                                                   556
         AG_EXNAME = 'COMPONENT'                                          556
         AG_PARLIST = 'A_Z_W'                                             556
         CALL AXCOMPONENT                                                 556
C    *                                                                    556
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           556
         CALL AGSRESET                                                    557
         AG_A = 1                                                         557
         AG_Z = 1                                                         557
         AG_W = 2                                                         557
         AG_TITLE = 'H2'                                                  557
         AG_EXNAME = 'COMPONENT'                                          557
         AG_PARLIST = 'A_Z_W'                                             557
         CALL AXCOMPONENT                                                 557
C    *                                                                    557
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             557
         CALL AGSRESET                                                    558
         AG_DENS = 0.935                                                  558
         AG_TITLE = 'CH2'                                                 558
         AG_EXNAME = 'MIXTURE'                                            558
         AG_PARLIST = 'DENS'                                              558
         CALL AXMIXTURE                                                   558
C    *                                                                    558
C    Geant ATTRIBUTE : SYRU ===> SEEN=1 COLO=4 <===                       558
         CALL AGSRESET                                                    559
         AG_SEEN = 1                                                      559
         AG_COLO = 4                                                      559
         AG_TITLE = 'SYRU'                                                559
         AG_EXNAME = 'ATTRIBUTE'                                          559
         AG_PARLIST = 'SEEN_COLO'                                         559
         CALL AXATTRIBUTE                                                 559
C    *                                                                    559
C    Geant SHAPE : TUBE ===> RMIN=SWCX_RAD RMAX=SWCX_RAD+SWCX_WALL, DZ=S  559
C    WCX_LENGTH/2.0 <===                                                  559
         CALL AGSRESET                                                    561
         AG_RMIN = SWCX_RAD                                               561
         AG_RMAX = SWCX_RAD+SWCX_WALL                                     561
         AG_DZ = SWCX_LENGTH/2.0                                          561
         AG_TITLE = 'TUBE'                                                561
         AG_EXNAME = 'SHAPE'                                              561
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      561
         CALL AXSHAPE                                                     561
C    *                                                                    561
5491  CALL AGSPOPB                                                        562
      GO TO BLOCK_SYRU, (5490 ,5500)                                      562
C End of Local Procedure                                                  562
C ----------------------------------------------------------------------  563
C ------------ SOUM                                                       563
C Begin Local Procedure SOUM                                              564
C SOUM IS THE MOTHER OF THE ARRAY OF THE OUTER SHILEDING SUPPORT TUBES    564
5450  CONTINUE                                                            564
         AG_TITLE='SOUM'                                                  564
         AG_MODULE=MODULE                                                 564
         AG_MARK='V00'                                                    564
         CALL AGSPUSH                                                     564
         AG_MODULE=MODULE                                                 564
C    *                                                                    564
C    Geant MATERIAL : AIR ===> inherited arguments used <===              565
         CALL AGSRESET                                                    565
         AG_TITLE = 'AIR'                                                 565
         AG_EXNAME = 'MATERIAL'                                           565
         AG_PARLIST = 'NONE'                                              565
         CALL AXMATERIAL                                                  565
C    *                                                                    565
C    Geant ATTRIBUTE : SOUM ===> SEEN=0 COLO=1 <===                       565
         CALL AGSRESET                                                    566
         AG_SEEN = 0                                                      566
         AG_COLO = 1                                                      566
         AG_TITLE = 'SOUM'                                                566
         AG_EXNAME = 'ATTRIBUTE'                                          566
         AG_PARLIST = 'SEEN_COLO'                                         566
         CALL AXATTRIBUTE                                                 566
C    *                                                                    566
C    Geant SHAPE : TUBS ===> RMIN=SOUP_ROUT-SOUP_DR RMAX=SOUP_ROUT, PHI1  566
C    =SOUP_PHI1 PHI2=SOUP_PHI2 DZ=SOUP_LENGTH/2.0 <===                    566
         CALL AGSRESET                                                    568
         AG_RMIN = SOUP_ROUT-SOUP_DR                                      568
         AG_RMAX = SOUP_ROUT                                              568
         AG_PHI1 = SOUP_PHI1                                              568
         AG_PHI2 = SOUP_PHI2                                              568
         AG_DZ = SOUP_LENGTH/2.0                                          568
         AG_TITLE = 'TUBS'                                                568
         AG_EXNAME = 'SHAPE'                                              568
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            568
         CALL AXSHAPE                                                     568
C    *                                                                    568
C    Create SOUR :                                                        569
C    Call To Local Procedure SOUR                                         569
         ASSIGN 5530 TO BLOCK_SOUR                                        569
         GO TO  5520                                                      569
5530     CONTINUE                                                         569
C    *                                                                    569
C    *                                                                    570
         I_PHI=1                                                          570
            GO TO 5543                                                    570
5541        I_PHI=I_PHI+1                                                 570
5543        IF(I_PHI-(4).GT.0) GO TO 5542                                 570
C       Geant POSITION : SOUR ===> X=COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SO  572
C       UP_ROUT-SOUP_DR/2.0) Y=SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROU  572
C       T-SOUP_DR/2.0) Z=0.0 <===                                         572
            CALL AGSRESET                                                 572
            AG_X = COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    572
     *      2.0)                                                          572
            AG_Y = SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    572
     *      2.0)                                                          572
            AG_Z = 0.0                                                    572
            AG_TITLE = 'SOUR'                                             572
            AG_EXNAME = 'POSITION'                                        572
            AG_PARLIST = 'X_Y_Z'                                          572
            CALL AXPOSITION                                               572
C       *                                                                 572
         GO TO 5541                                                       573
5542     CONTINUE                                                         573
5451  CALL AGSPOPB                                                        574
      GO TO BLOCK_SOUM, (5450 ,5460)                                      574
C End of Local Procedure                                                  574
C ----------------------------------------------------------------------  575
C ------------ SOUR                                                       575
C Begin Local Procedure SOUR                                              576
C SOUR IS THE OUTER SHILEDING SUPPORT TUBES (RODS)                        576
5520  CONTINUE                                                            576
         AG_TITLE='SOUR'                                                  576
         AG_MODULE=MODULE                                                 576
         AG_MARK='V00'                                                    576
         CALL AGSPUSH                                                     576
         AG_MODULE=MODULE                                                 576
C    *                                                                    576
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           577
         CALL AGSRESET                                                    577
         AG_TITLE = 'CARBON'                                              577
         AG_EXNAME = 'MATERIAL'                                           577
         AG_PARLIST = 'NONE'                                              577
         CALL AXMATERIAL                                                  577
C    *                                                                    577
C    Geant ATTRIBUTE : SOUM ===> SEEN=1 COLO=4 <===                       577
         CALL AGSRESET                                                    578
         AG_SEEN = 1                                                      578
         AG_COLO = 4                                                      578
         AG_TITLE = 'SOUM'                                                578
         AG_EXNAME = 'ATTRIBUTE'                                          578
         AG_PARLIST = 'SEEN_COLO'                                         578
         CALL AXATTRIBUTE                                                 578
C    *                                                                    578
C    Geant SHAPE : TUBE ===> RMIN=SOUP_DIAMIN/2.0 RMAX=SOUP_DIAMOUT/2.0,  578
C     DZ=SOUP_LENGTH/2.0 <===                                             578
         CALL AGSRESET                                                    580
         AG_RMIN = SOUP_DIAMIN/2.0                                        580
         AG_RMAX = SOUP_DIAMOUT/2.0                                       580
         AG_DZ = SOUP_LENGTH/2.0                                          580
         AG_TITLE = 'TUBE'                                                580
         AG_EXNAME = 'SHAPE'                                              580
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      580
         CALL AXSHAPE                                                     580
C    *                                                                    580
5521  CALL AGSPOPB                                                        581
      GO TO BLOCK_SOUR, (5520 ,5530)                                      581
C End of Local Procedure                                                  581
C ----------------------------------------------------------------------  582
C ------------ SLYD                                                       582
C Begin Local Procedure SLYD                                              583
C SLYD IS A SINGLE SVT LAYER                                              583
5280  CONTINUE                                                            583
         AG_TITLE='SLYD'                                                  583
         AG_MODULE=MODULE                                                 583
         AG_MARK='V00'                                                    583
         CALL AGSPUSH                                                     583
         AG_MODULE=MODULE                                                 583
C    *                                                                    583
         LADTHK = SWCA_WAFERTHK/2 + SWCA_ROHATHK + SWCA_WAFCARTH          584
         ELETHK=2.0*SELC_BETHK + SELC_WATTHK + SELC_BEOTHK +              585
     *   SELC_AGPDTHK                                                     585
         ELETHK=ELETHK + SELC_GLASSTHK + SELC_DYETHK                      586
         DEG=180.0/SVTL_NLADDER                                           587
         RAD=PI/SVTL_NLADDER                                              588
C    Geant MATERIAL : AIR ===> inherited arguments used <===              589
         CALL AGSRESET                                                    589
         AG_TITLE = 'AIR'                                                 589
         AG_EXNAME = 'MATERIAL'                                           589
         AG_PARLIST = 'NONE'                                              589
         CALL AXMATERIAL                                                  589
C    *                                                                    589
C    Geant ATTRIBUTE : SLYD ===> SEEN=0 COLO=1 <===                       589
         CALL AGSRESET                                                    590
         AG_SEEN = 0                                                      590
         AG_COLO = 1                                                      590
         AG_TITLE = 'SLYD'                                                590
         AG_EXNAME = 'ATTRIBUTE'                                          590
         AG_PARLIST = 'SEEN_COLO'                                         590
         CALL AXATTRIBUTE                                                 590
C    *                                                                    590
         RMIN=SVTL_RADIUS - LADTHK                                        590
         RMAX = SVTL_RADIUS*SVTL_RADIUS                                   592
         RMAX=RMAX+SWCA_WAFERWID*SWCA_WAFERWID/4.0                        593
         RMAX = SQRT(RMAX) + ELETHK                                       594
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   595
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    596
         CALL AGSRESET                                                    596
         AG_RMIN = RMIN                                                   596
         AG_RMAX = RMAX                                                   596
         AG_DZ = SWCA_LENGTH/2                                            596
         AG_TITLE = 'TUBE'                                                596
         AG_EXNAME = 'SHAPE'                                              596
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      596
         CALL AXSHAPE                                                     596
C    *                                                                    596
         C0=90 - DEG*MOD(ILAYER,2)                                        596
C    *                                                                    598
         IPOS=1                                                           598
            GO TO 5553                                                    598
5551        IPOS=IPOS+1                                                   598
5553        IF(IPOS-(SVTL_NLADDER).GT.0) GO TO 5552                       598
C       XD(ILAYER,IPOS)                                                   599
            XDISP=0.0                                                     599
C       YD(ILAYER,IPOS)                                                   600
            YDISP=0.0                                                     600
            ADISP=AD(ILAYER,IPOS)                                         601
            LSUB = MOD(ILAYER-1,2)                                        602
            CHECKREZ = SVTL_NLADDER*2.0-(IPOS-1)*2-LSUB                   603
            FOO = SVTL_NLADDER                                            604
C       Create SLSD :                                                     605
C       Call To Local Procedure SLSD                                      605
            ASSIGN 5570 TO BLOCK_SLSD                                     605
            GO TO  5560                                                   605
5570        CONTINUE                                                      605
C       *                                                                 605
C       Geant POSITION : SLSD ===> X=XDISP Y=YDISP ALPHAZ=C0+(360/SVTL_N  607
C       LADDER)*(IPOS-1)+(180/SVTL_NLADDER) KONLY='MANY' <===             607
            CALL AGSRESET                                                 607
            AG_X = XDISP                                                  607
            AG_Y = YDISP                                                  607
            AG_ALPHAN=AG_ALPHAN+1                                         607
            AG_ALPHAK(AG_ALPHAN)=3                                        607
            AG_ALPHAL(AG_ALPHAN)=C0+(360/SVTL_NLADDER)*(IPOS-1)+(180/     607
     *      SVTL_NLADDER)                                                 607
            AG_KONLY = 'MANY'                                             607
            AG_TITLE = 'SLSD'                                             607
            AG_EXNAME = 'POSITION'                                        607
            AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                               607
            CALL AXPOSITION                                               607
C       *                                                                 607
         GO TO 5551                                                       608
5552     CONTINUE                                                         608
5281  CALL AGSPOPB                                                        609
      GO TO BLOCK_SLYD, (5280 ,5290,5300)                                 609
C End of Local Procedure                                                  609
C ----------------------------------------------------------------------  610
C ------------ SLSD                                                       610
C Begin Local Procedure SLSD                                              611
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         611
5560  CONTINUE                                                            611
         AG_TITLE='SLSD'                                                  611
         AG_MODULE=MODULE                                                 611
         AG_MARK='V00'                                                    611
         CALL AGSPUSH                                                     611
         AG_MODULE=MODULE                                                 611
C    *                                                                    611
C    Geant ATTRIBUTE : SLSD ===> SEEN=1 COLO=1 <===                       612
         CALL AGSRESET                                                    612
         AG_SEEN = 1                                                      612
         AG_COLO = 1                                                      612
         AG_TITLE = 'SLSD'                                                612
         AG_EXNAME = 'ATTRIBUTE'                                          612
         AG_PARLIST = 'SEEN_COLO'                                         612
         CALL AXATTRIBUTE                                                 612
C    *                                                                    612
C    Geant ATTRIBUTE : SLSD ===> SERIAL=ILAYER*10+IPOS <===               612
         CALL AGSRESET                                                    613
         AG_SERIAL = ILAYER*10+IPOS                                       613
         AG_TITLE = 'SLSD'                                                613
         AG_EXNAME = 'ATTRIBUTE'                                          613
         AG_PARLIST = 'SERIAL'                                            613
         CALL AXATTRIBUTE                                                 613
C    *                                                                    613
C    Check SVTG_NLAYER<0                                                  613
         IF (SVTG_NLAYER.LT.0) THEN                                       614
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               615
         CALL AGSRESET                                                    615
         AG_NDIV = 1                                                      615
         AG_IAXIS = 2                                                     615
         AG_C0 = -90                                                      615
         AG_TITLE = 'DIVISION'                                            615
         AG_EXNAME = 'SHAPE'                                              615
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     615
         CALL AXSHAPE                                                     615
C    *                                                                    615
         ELSE                                                             615
C    Check SVTG_VERSION==1                                                617
         IF (SVTG_VERSION.EQ.1) THEN                                      617
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              618
         ELSE                                                             619
         C0=90-DEG*MOD(ILAYER,2)                                          620
         ENDIF                                                            621
C    Geant SHAPE : TUBS ===> RMIN=RMIN RMAX=RMAX PHI1=-180/SVTL_NLADDER,  623
C     PHI2=180/SVTL_NLADDER DZ=SWCA_LENGTH/2 <===                         623
         CALL AGSRESET                                                    623
         AG_RMIN = RMIN                                                   623
         AG_RMAX = RMAX                                                   623
         AG_PHI1 = -180/SVTL_NLADDER                                      623
         AG_PHI2 = 180/SVTL_NLADDER                                       623
         AG_DZ = SWCA_LENGTH/2                                            623
         AG_TITLE = 'TUBS'                                                623
         AG_EXNAME = 'SHAPE'                                              623
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            623
         CALL AXSHAPE                                                     623
C    *                                                                    623
         ENDIF                                                            623
C    Geant ATTRIBUTE : SLSD ===> SERIAL=0 <===                            625
         CALL AGSRESET                                                    625
         AG_SERIAL = 0                                                    625
         AG_TITLE = 'SLSD'                                                625
         AG_EXNAME = 'ATTRIBUTE'                                          625
         AG_PARLIST = 'SERIAL'                                            625
         CALL AXATTRIBUTE                                                 625
C    *                                                                    625
C    Create SLDI :                                                        626
C    Call To Local Procedure SLDI                                         626
         ASSIGN 5590 TO BLOCK_SLDI                                        626
         GO TO  5580                                                      626
5590     CONTINUE                                                         626
C    *                                                                    626
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ALPHAZ=ADISP ORT=YZX <===   627
         CALL AGSRESET                                                    627
         AG_X = SVTL_RADIUS                                               627
         AG_ALPHAN=AG_ALPHAN+1                                            627
         AG_ALPHAK(AG_ALPHAN)=3                                           627
         AG_ALPHAL(AG_ALPHAN)=ADISP                                       627
         AG_ALPHAN=AG_ALPHAN+1                                            627
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 627
         AG_TITLE = 'SLDI'                                                627
         AG_EXNAME = 'POSITION'                                           627
         AG_PARLIST = 'X_ALPHAZ_ORT'                                      627
         CALL AXPOSITION                                                  627
C    *                                                                    627
C    Create SELE :                                                        628
C    Call To Local Procedure SELE                                         628
         ASSIGN 5610 TO BLOCK_SELE                                        628
         GO TO  5600                                                      628
5610     CONTINUE                                                         628
C    *                                                                    628
         DEG=180.0/SVTL_NLADDER                                           629
         RAD=(TWOPI/2)/SVTL_NLADDER                                       630
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 631
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 632
C    *                                                                    633
         S=-1                                                             633
            GOTO 5623                                                     633
5621        S=S+(2)                                                       633
5623        IF((2)*(S-(1)).GT.0) GO TO 5622                               633
            CS=(S*DEG+ADISP)                                              634
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  636
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=CS <===            636
            CALL AGSRESET                                                 636
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                636
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               636
            AG_ALPHAN=AG_ALPHAN+1                                         636
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              636
            AG_ALPHAN=AG_ALPHAN+1                                         636
            AG_ALPHAK(AG_ALPHAN)=1                                        636
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 636
            AG_ALPHAN=AG_ALPHAN+1                                         636
            AG_ALPHAK(AG_ALPHAN)=3                                        636
            AG_ALPHAL(AG_ALPHAN)=CS                                       636
            AG_TITLE = 'SELE'                                             636
            AG_EXNAME = 'POSITION'                                        636
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          636
            CALL AXPOSITION                                               636
C       *                                                                 636
         GO TO 5621                                                       637
5622     CONTINUE                                                         637
5561  CALL AGSPOPB                                                        638
      GO TO BLOCK_SLSD, (5560 ,5570)                                      638
C End of Local Procedure                                                  638
C ----------------------------------------------------------------------  639
C ------------ SLDI                                                       639
C Begin Local Procedure SLDI                                              640
C SLDI IS A LADDER VOLUME                                                 640
5580  CONTINUE                                                            640
         AG_TITLE='SLDI'                                                  640
         AG_MODULE=MODULE                                                 640
         AG_MARK='V00'                                                    640
         CALL AGSPUSH                                                     640
         AG_MODULE=MODULE                                                 640
C    *                                                                    640
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       641
C    Geant MATERIAL : AIR ===> inherited arguments used <===              642
         CALL AGSRESET                                                    642
         AG_TITLE = 'AIR'                                                 642
         AG_EXNAME = 'MATERIAL'                                           642
         AG_PARLIST = 'NONE'                                              642
         CALL AXMATERIAL                                                  642
C    *                                                                    642
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 <===                       642
         CALL AGSRESET                                                    643
         AG_SEEN = 0                                                      643
         AG_COLO = 1                                                      643
         AG_TITLE = 'SLDI'                                                643
         AG_EXNAME = 'ATTRIBUTE'                                          643
         AG_PARLIST = 'SEEN_COLO'                                         643
         CALL AXATTRIBUTE                                                 643
C    *                                                                    643
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  643
C    HK <===                                                              643
         CALL AGSRESET                                                    645
         AG_DX = SWCA_WAFERWID/2                                          645
         AG_DY = SWCA_LENGTH/2                                            645
         AG_DZ = LADTHK                                                   645
         AG_TITLE = 'BOX'                                                 645
         AG_EXNAME = 'SHAPE'                                              645
         AG_PARLIST = 'DX_DY_DZ'                                          645
         CALL AXSHAPE                                                     645
C    *                                                                    645
C    Create STLI :                                                        646
C    Call To Local Procedure STLI                                         646
         ASSIGN 5640 TO BLOCK_STLI                                        646
         GO TO  5630                                                      646
5640     CONTINUE                                                         646
C    *                                                                    646
C    Geant POSITION : STLI ===> inherited arguments used <===             647
         CALL AGSRESET                                                    647
         AG_TITLE = 'STLI'                                                647
         AG_EXNAME = 'POSITION'                                           647
         AG_PARLIST = 'NONE'                                              647
         CALL AXPOSITION                                                  647
C    *                                                                    647
C    Create SBER :                                                        648
C    Call To Local Procedure SBER                                         648
         ASSIGN 5660 TO BLOCK_SBER                                        648
         GO TO  5650                                                      648
5660     CONTINUE                                                         648
C    *                                                                    648
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  649
C    DTHK+SWCA_WAFCARTH/2 <===                                            649
         CALL AGSRESET                                                    649
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          649
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   649
         AG_TITLE = 'SBER'                                                649
         AG_EXNAME = 'POSITION'                                           649
         AG_PARLIST = 'X_Z'                                               649
         CALL AXPOSITION                                                  649
C    *                                                                    649
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  649
C    DTHK+SWCA_WAFCARTH/2 <===                                            649
         CALL AGSRESET                                                    650
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          650
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   650
         AG_TITLE = 'SBER'                                                650
         AG_EXNAME = 'POSITION'                                           650
         AG_PARLIST = 'X_Z'                                               650
         CALL AXPOSITION                                                  650
C    *                                                                    650
C    Create STAB :                                                        651
C    Call To Local Procedure STAB                                         651
         ASSIGN 5680 TO BLOCK_STAB                                        651
         GO TO  5670                                                      651
5680     CONTINUE                                                         651
C    *                                                                    651
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  652
C    WAFCARTH/2 <===                                                      652
         CALL AGSRESET                                                    652
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    652
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   652
         AG_TITLE = 'STAB'                                                652
         AG_EXNAME = 'POSITION'                                           652
         AG_PARLIST = 'Y_Z'                                               652
         CALL AXPOSITION                                                  652
C    *                                                                    652
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  652
C    _WAFCARTH/2 <===                                                     652
         CALL AGSRESET                                                    653
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   653
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   653
         AG_TITLE = 'STAB'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'Y_Z'                                               653
         CALL AXPOSITION                                                  653
C    *                                                                    653
C    Create STRU :                                                        654
C    Call To Local Procedure STRU                                         654
         ASSIGN 5700 TO BLOCK_STRU                                        654
         GO TO  5690                                                      654
5700     CONTINUE                                                         654
C    *                                                                    654
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  657
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  657
C    =                                                                    657
         CALL AGSRESET                                                    657
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              657
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  657
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   657
         AG_TITLE = 'STRU'                                                657
         AG_EXNAME = 'POSITION'                                           657
         AG_PARLIST = 'Y_Z'                                               657
         CALL AXPOSITION                                                  657
C    *                                                                    657
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  657
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  657
C    ==                                                                   657
         CALL AGSRESET                                                    660
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             660
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  660
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   660
         AG_TITLE = 'STRU'                                                660
         AG_EXNAME = 'POSITION'                                           660
         AG_PARLIST = 'Y_Z'                                               660
         CALL AXPOSITION                                                  660
C    *                                                                    660
C    Create SRHC :                                                        661
C    Call To Local Procedure SRHC                                         661
         ASSIGN 5720 TO BLOCK_SRHC                                        661
         GO TO  5710                                                      661
5720     CONTINUE                                                         661
C    *                                                                    661
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  663
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         663
         CALL AGSRESET                                                    663
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          663
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                663
         AG_TITLE = 'SRHC'                                                663
         AG_EXNAME = 'POSITION'                                           663
         AG_PARLIST = 'X_Z'                                               663
         CALL AXPOSITION                                                  663
C    *                                                                    663
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  663
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         663
         CALL AGSRESET                                                    665
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          665
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                665
         AG_TITLE = 'SRHC'                                                665
         AG_EXNAME = 'POSITION'                                           665
         AG_PARLIST = 'X_Z'                                               665
         CALL AXPOSITION                                                  665
C    *                                                                    665
C    Create SPCB :                                                        666
C    Call To Local Procedure SPCB                                         666
         ASSIGN 5740 TO BLOCK_SPCB                                        666
         GO TO  5730                                                      666
5740     CONTINUE                                                         666
C    *                                                                    666
         YPCB = SVTL_NWAFER*(SWCA_WAFERLEN + SWCA_WAFERGAP)/2.0 +         667
     *   SVTL_PCBGAP + SVTL_PCBLEN/2.0                                    667
C    Geant POSITION : SPCB ===> X=0.0 Y=YPCB Z=SVTL_PCBTHK/2.0 <===       668
         CALL AGSRESET                                                    668
         AG_X = 0.0                                                       668
         AG_Y = YPCB                                                      668
         AG_Z = SVTL_PCBTHK/2.0                                           668
         AG_TITLE = 'SPCB'                                                668
         AG_EXNAME = 'POSITION'                                           668
         AG_PARLIST = 'X_Y_Z'                                             668
         CALL AXPOSITION                                                  668
C    *                                                                    668
C    Geant POSITION : SPCB ===> X=0.0 Y=-YPCB Z=SVTL_PCBTHK/2.0 ALPHAZ=1  668
C    80.0 <===                                                            668
         CALL AGSRESET                                                    669
         AG_X = 0.0                                                       669
         AG_Y = -YPCB                                                     669
         AG_Z = SVTL_PCBTHK/2.0                                           669
         AG_ALPHAN=AG_ALPHAN+1                                            669
         AG_ALPHAK(AG_ALPHAN)=3                                           669
         AG_ALPHAL(AG_ALPHAN)=180.0                                       669
         AG_TITLE = 'SPCB'                                                669
         AG_EXNAME = 'POSITION'                                           669
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      669
         CALL AXPOSITION                                                  669
C    *                                                                    669
5581  CALL AGSPOPB                                                        670
      GO TO BLOCK_SLDI, (5580 ,5590)                                      670
C End of Local Procedure                                                  670
C ----------------------------------------------------------------------  671
C ------------ SRHC                                                       671
C Begin Local Procedure SRHC                                              672
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     672
5710  CONTINUE                                                            672
         AG_TITLE='SRHC'                                                  672
         AG_MODULE=MODULE                                                 672
         AG_MARK='V00'                                                    672
         CALL AGSPUSH                                                     672
         AG_MODULE=MODULE                                                 672
C    *                                                                    672
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           673
         CALL AGSRESET                                                    673
         AG_A = 12                                                        673
         AG_Z = 6                                                         673
         AG_W = 1                                                         673
         AG_TITLE = 'C'                                                   673
         AG_EXNAME = 'COMPONENT'                                          673
         AG_PARLIST = 'A_Z_W'                                             673
         CALL AXCOMPONENT                                                 673
C    *                                                                    673
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           673
         CALL AGSRESET                                                    674
         AG_A = 1                                                         674
         AG_Z = 1                                                         674
         AG_W = 2                                                         674
         AG_TITLE = 'H2'                                                  674
         AG_EXNAME = 'COMPONENT'                                          674
         AG_PARLIST = 'A_Z_W'                                             674
         CALL AXCOMPONENT                                                 674
C    *                                                                    674
C    Geant MIXTURE : ROHA ===> DENS=0.0304 <===                           674
         CALL AGSRESET                                                    675
         AG_DENS = 0.0304                                                 675
         AG_TITLE = 'ROHA'                                                675
         AG_EXNAME = 'MIXTURE'                                            675
         AG_PARLIST = 'DENS'                                              675
         CALL AXMIXTURE                                                   675
C    *                                                                    675
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       675
         CALL AGSRESET                                                    676
         AG_SEEN = 1                                                      676
         AG_COLO = 3                                                      676
         AG_TITLE = 'SRHC'                                                676
         AG_EXNAME = 'ATTRIBUTE'                                          676
         AG_PARLIST = 'SEEN_COLO'                                         676
         CALL AXATTRIBUTE                                                 676
C    *                                                                    676
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  676
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        676
         CALL AGSRESET                                                    678
         AG_DX = SWCA_WAFCARWD/2                                          678
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              678
         AG_DZ = SWCA_ROHATHK/2                                           678
         AG_TITLE = 'BOX'                                                 678
         AG_EXNAME = 'SHAPE'                                              678
         AG_PARLIST = 'DX_DY_DZ'                                          678
         CALL AXSHAPE                                                     678
C    *                                                                    678
5711  CALL AGSPOPB                                                        679
      GO TO BLOCK_SRHC, (5710 ,5720)                                      679
C End of Local Procedure                                                  679
C ----------------------------------------------------------------------  680
C ------------ STLI                                                       680
C Begin Local Procedure STLI                                              681
C STLI IS THE WAFER PACK CONTAINER                                        681
5630  CONTINUE                                                            681
         AG_TITLE='STLI'                                                  681
         AG_MODULE=MODULE                                                 681
         AG_MARK='V00'                                                    681
         CALL AGSPUSH                                                     681
         AG_MODULE=MODULE                                                 681
C    *                                                                    681
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              682
         CALL AGSRESET                                                    682
         AG_SEEN = 0                                                      682
         AG_COLO = 1                                                      682
         AG_SERIAL = 0                                                    682
         AG_TITLE = 'STLI'                                                682
         AG_EXNAME = 'ATTRIBUTE'                                          682
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  682
         CALL AXATTRIBUTE                                                 682
C    *                                                                    682
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  682
C    P), DZ=SWCA_WAFERTHK/2 <===                                          682
         CALL AGSRESET                                                    684
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              684
         AG_DZ = SWCA_WAFERTHK/2                                          684
         AG_TITLE = 'BOX'                                                 684
         AG_EXNAME = 'SHAPE'                                              684
         AG_PARLIST = 'DY_DZ'                                             684
         CALL AXSHAPE                                                     684
C    *                                                                    684
C    Create STSI :                                                        685
C    Call To Local Procedure STSI                                         685
         ASSIGN 5760 TO BLOCK_STSI                                        685
         GO TO  5750                                                      685
5760     CONTINUE                                                         685
C    *                                                                    685
5631  CALL AGSPOPB                                                        686
      GO TO BLOCK_STLI, (5630 ,5640)                                      686
C End of Local Procedure                                                  686
C ----------------------------------------------------------------------  687
C ------------ STSI                                                       687
C Begin Local Procedure STSI                                              688
C STSI IS A SINGLE WAVER CONTAINER                                        688
5750  CONTINUE                                                            688
         AG_TITLE='STSI'                                                  688
         AG_MODULE=MODULE                                                 688
         AG_MARK='V00'                                                    688
         CALL AGSPUSH                                                     688
         AG_MODULE=MODULE                                                 688
C    *                                                                    688
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            689
         CALL AGSRESET                                                    689
         AG_NDIV = SVTL_NWAFER                                            689
         AG_IAXIS = 2                                                     689
         AG_TITLE = 'DIVISION'                                            689
         AG_EXNAME = 'SHAPE'                                              689
         AG_PARLIST = 'NDIV_IAXIS'                                        689
         CALL AXSHAPE                                                     689
C    *                                                                    689
C    Create SVTD :                                                        690
C    Call To Local Procedure SVTD                                         690
         ASSIGN 5780 TO BLOCK_SVTD                                        690
         GO TO  5770                                                      690
5780     CONTINUE                                                         690
C    *                                                                    690
C    Geant POSITION : SVTD ===> inherited arguments used <===             691
         CALL AGSRESET                                                    691
         AG_TITLE = 'SVTD'                                                691
         AG_EXNAME = 'POSITION'                                           691
         AG_PARLIST = 'NONE'                                              691
         CALL AXPOSITION                                                  691
C    *                                                                    691
5751  CALL AGSPOPB                                                        692
      GO TO BLOCK_STSI, (5750 ,5760)                                      692
C End of Local Procedure                                                  692
C ----------------------------------------------------------------------  693
C ------------ SVTD                                                       693
C Begin Local Procedure SVTD                                              694
C SVTD IS AN ACTIVE WAFER VOLUME                                          694
5770  CONTINUE                                                            694
         AG_TITLE='SVTD'                                                  694
         AG_MODULE=MODULE                                                 694
         AG_MARK='V00'                                                    694
         CALL AGSPUSH                                                     694
         AG_MODULE=MODULE                                                 694
C    *                                                                    694
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          695
         CALL AGSRESET                                                    695
         AG_TITLE = 'SILICON'                                             695
         AG_EXNAME = 'MATERIAL'                                           695
         AG_PARLIST = 'NONE'                                              695
         CALL AXMATERIAL                                                  695
C    *                                                                    695
C    Geant MATERIAL : SENSSI ===> ISVOL=1 <===                            695
         CALL AGSRESET                                                    696
         AG_ISVOL = 1                                                     696
         AG_TITLE = 'SENSSI'                                              696
         AG_EXNAME = 'MATERIAL'                                           696
         AG_PARLIST = 'ISVOL'                                             696
         CALL AXMATERIAL                                                  696
C    *                                                                    696
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       696
         CALL AGSRESET                                                    697
         AG_SEEN = 1                                                      697
         AG_COLO = 4                                                      697
         AG_TITLE = 'SVTD'                                                697
         AG_EXNAME = 'ATTRIBUTE'                                          697
         AG_PARLIST = 'SEEN_COLO'                                         697
         CALL AXATTRIBUTE                                                 697
C    *                                                                    697
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       697
         CALL AGSRESET                                                    698
         AG_DY = SWCA_WAFERLEN/2                                          698
         AG_TITLE = 'BOX'                                                 698
         AG_EXNAME = 'SHAPE'                                              698
         AG_PARLIST = 'DY'                                                698
         CALL AXSHAPE                                                     698
C    *                                                                    698
         TRAPY=SWCA_SENSWID/2.0 - (SWCA_SENSWID - SWCA_SENSGIR)/2.0       698
C    Create STRA :                                                        700
C    Call To Local Procedure STRA                                         700
         ASSIGN 5800 TO BLOCK_STRA                                        700
         GO TO  5790                                                      700
5800     CONTINUE                                                         700
C    *                                                                    700
C    Geant POSITION : STRA ===> Y=+TRAPY ALPHAZ=+90 ORT=YZX <===          701
         CALL AGSRESET                                                    701
         AG_Y = +TRAPY                                                    701
         AG_ALPHAN=AG_ALPHAN+1                                            701
         AG_ALPHAK(AG_ALPHAN)=3                                           701
         AG_ALPHAL(AG_ALPHAN)=+90                                         701
         AG_ALPHAN=AG_ALPHAN+1                                            701
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 701
         AG_TITLE = 'STRA'                                                701
         AG_EXNAME = 'POSITION'                                           701
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      701
         CALL AXPOSITION                                                  701
C    *                                                                    701
C    Geant POSITION : STRA ===> Y=-TRAPY ALPHAZ=-90 ORT=YZX <===          701
         CALL AGSRESET                                                    702
         AG_Y = -TRAPY                                                    702
         AG_ALPHAN=AG_ALPHAN+1                                            702
         AG_ALPHAK(AG_ALPHAN)=3                                           702
         AG_ALPHAL(AG_ALPHAN)=-90                                         702
         AG_ALPHAN=AG_ALPHAN+1                                            702
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 702
         AG_TITLE = 'STRA'                                                702
         AG_EXNAME = 'POSITION'                                           702
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      702
         CALL AXPOSITION                                                  702
C    *                                                                    702
         SSIDX=SWCA_WAFERLEN/2.0 - (SWCA_WAFERLEN - SWCA_SENSLEN)/4.0     702
         SSIRY=SWCA_WAFERWID/2.0 - (SWCA_WAFERWID - SWCA_SENSWID)/4.0     704
C    Create SSID :                                                        705
C    Call To Local Procedure SSID                                         705
         ASSIGN 5820 TO BLOCK_SSID                                        705
         GO TO  5810                                                      705
5820     CONTINUE                                                         705
C    *                                                                    705
C    Geant POSITION : SSID ===> X=+SSIDX <===                             706
         CALL AGSRESET                                                    706
         AG_X = +SSIDX                                                    706
         AG_TITLE = 'SSID'                                                706
         AG_EXNAME = 'POSITION'                                           706
         AG_PARLIST = 'X'                                                 706
         CALL AXPOSITION                                                  706
C    *                                                                    706
C    Geant POSITION : SSID ===> X=-SSIDX <===                             706
         CALL AGSRESET                                                    707
         AG_X = -SSIDX                                                    707
         AG_TITLE = 'SSID'                                                707
         AG_EXNAME = 'POSITION'                                           707
         AG_PARLIST = 'X'                                                 707
         CALL AXPOSITION                                                  707
C    *                                                                    707
C    Create SSIR :                                                        708
C    Call To Local Procedure SSIR                                         708
         ASSIGN 5840 TO BLOCK_SSIR                                        708
         GO TO  5830                                                      708
5840     CONTINUE                                                         708
C    *                                                                    708
C    Geant POSITION : SSIR ===> Y=+SSIRY <===                             709
         CALL AGSRESET                                                    709
         AG_Y = +SSIRY                                                    709
         AG_TITLE = 'SSIR'                                                709
         AG_EXNAME = 'POSITION'                                           709
         AG_PARLIST = 'Y'                                                 709
         CALL AXPOSITION                                                  709
C    *                                                                    709
C    Geant POSITION : SSIR ===> Y=-SSIRY <===                             709
         CALL AGSRESET                                                    710
         AG_Y = -SSIRY                                                    710
         AG_TITLE = 'SSIR'                                                710
         AG_EXNAME = 'POSITION'                                           710
         AG_PARLIST = 'Y'                                                 710
         CALL AXPOSITION                                                  710
C    *                                                                    710
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 710
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  713
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  713
C    .,0.01)                                                              713
         AG_TITLE = 'SVTD'                                                713
         AG_EXNAME = 'HITS'                                               713
         AG_TYPE=AG_MAGIC                                                 713
         CALL AGSDETV('SVTD')                                             713
C    *                                                                    713
         AG_PARLIST='Z'                                                   713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.001                                                      713
         AG_OPTION='S'                                                    713
         AG_FHMIN = AG_MAGIC                                              713
         AG_FHMAX = AG_MAGIC                                              713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='Y'                                                   713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.001                                                      713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='X'                                                   713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.001                                                      713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='PTOT'                                                713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 18                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0                                                     713
         AG_FHMAX = 100                                                   713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='CX'                                                  713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 10                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='CY'                                                  713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 10                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='CZ'                                                  713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 10                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='SLENG'                                               713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 16                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0                                                     713
         AG_FHMAX = 500                                                   713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='TOF'                                                 713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 16                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0                                                     713
         AG_FHMAX = 1.0E-6                                                713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='STEP'                                                713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.01                                                       713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='ELOSS'                                               713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 22                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0.                                                    713
         AG_FHMAX = 0.01                                                  713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         CALL AGSDETU('SVTD')                                             713
C    *                                                                    713
5771  CALL AGSPOPB                                                        714
      GO TO BLOCK_SVTD, (5770 ,5780)                                      714
C End of Local Procedure                                                  714
C ----------------------------------------------------------------------  715
C ------------ SSID                                                       715
C Begin Local Procedure SSID                                              716
C SSID IS A NON-SENSITIVE LEFT-RIGHT BORDER OF THE WAFER                  716
5810  CONTINUE                                                            716
         AG_TITLE='SSID'                                                  716
         AG_MODULE=MODULE                                                 716
         AG_MARK='V00'                                                    716
         CALL AGSPUSH                                                     716
         AG_MODULE=MODULE                                                 716
C    *                                                                    716
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          717
         CALL AGSRESET                                                    717
         AG_TITLE = 'SILICON'                                             717
         AG_EXNAME = 'MATERIAL'                                           717
         AG_PARLIST = 'NONE'                                              717
         CALL AXMATERIAL                                                  717
C    *                                                                    717
C    Geant MATERIAL : NONSENSSI ===> ISVOL=0 <===                         717
         CALL AGSRESET                                                    718
         AG_ISVOL = 0                                                     718
         AG_TITLE = 'NONSENSSI'                                           718
         AG_EXNAME = 'MATERIAL'                                           718
         AG_PARLIST = 'ISVOL'                                             718
         CALL AXMATERIAL                                                  718
C    *                                                                    718
C    Geant ATTRIBUTE : SSID ===> SEEN=1 COLO=1 <===                       718
         CALL AGSRESET                                                    719
         AG_SEEN = 1                                                      719
         AG_COLO = 1                                                      719
         AG_TITLE = 'SSID'                                                719
         AG_EXNAME = 'ATTRIBUTE'                                          719
         AG_PARLIST = 'SEEN_COLO'                                         719
         CALL AXATTRIBUTE                                                 719
C    *                                                                    719
C    Geant SHAPE : BOX ===> DX=(SWCA_WAFERLEN-SWCA_SENSLEN)/4.0, DY=SWCA  719
C    _WAFERWID/2.0 <===                                                   719
         CALL AGSRESET                                                    721
         AG_DX = (SWCA_WAFERLEN-SWCA_SENSLEN)/4.0                         721
         AG_DY = SWCA_WAFERWID/2.0                                        721
         AG_TITLE = 'BOX'                                                 721
         AG_EXNAME = 'SHAPE'                                              721
         AG_PARLIST = 'DX_DY'                                             721
         CALL AXSHAPE                                                     721
C    *                                                                    721
5811  CALL AGSPOPB                                                        722
      GO TO BLOCK_SSID, (5810 ,5820)                                      722
C End of Local Procedure                                                  722
C ----------------------------------------------------------------------  723
C ------------ SSIR                                                       723
C Begin Local Procedure SSIR                                              724
C SSIR IS A NON-SENSITIVE UP-DOWN BORDER OF THE WAFER                     724
5830  CONTINUE                                                            724
         AG_TITLE='SSIR'                                                  724
         AG_MODULE=MODULE                                                 724
         AG_MARK='V00'                                                    724
         CALL AGSPUSH                                                     724
         AG_MODULE=MODULE                                                 724
C    *                                                                    724
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          725
         CALL AGSRESET                                                    725
         AG_TITLE = 'SILICON'                                             725
         AG_EXNAME = 'MATERIAL'                                           725
         AG_PARLIST = 'NONE'                                              725
         CALL AXMATERIAL                                                  725
C    *                                                                    725
C    Geant MATERIAL : NONSENSSIL ===> ISVOL=0 <===                        725
         CALL AGSRESET                                                    726
         AG_ISVOL = 0                                                     726
         AG_TITLE = 'NONSENSSIL'                                          726
         AG_EXNAME = 'MATERIAL'                                           726
         AG_PARLIST = 'ISVOL'                                             726
         CALL AXMATERIAL                                                  726
C    *                                                                    726
C    Geant ATTRIBUTE : SSIR ===> SEEN=1 COLO=1 <===                       726
         CALL AGSRESET                                                    727
         AG_SEEN = 1                                                      727
         AG_COLO = 1                                                      727
         AG_TITLE = 'SSIR'                                                727
         AG_EXNAME = 'ATTRIBUTE'                                          727
         AG_PARLIST = 'SEEN_COLO'                                         727
         CALL AXATTRIBUTE                                                 727
C    *                                                                    727
C    Geant SHAPE : BOX ===> DX=SWCA_SENSLEN/2.0 DY=(SWCA_WAFERWID-SWCA_S  727
C    ENSWID)/4.0 <===                                                     727
         CALL AGSRESET                                                    728
         AG_DX = SWCA_SENSLEN/2.0                                         728
         AG_DY = (SWCA_WAFERWID-SWCA_SENSWID)/4.0                         728
         AG_TITLE = 'BOX'                                                 728
         AG_EXNAME = 'SHAPE'                                              728
         AG_PARLIST = 'DX_DY'                                             728
         CALL AXSHAPE                                                     728
C    *                                                                    728
5831  CALL AGSPOPB                                                        729
      GO TO BLOCK_SSIR, (5830 ,5840)                                      729
C End of Local Procedure                                                  729
C ----------------------------------------------------------------------  730
C ------------ STRA                                                       730
C Begin Local Procedure STRA                                              731
C STRA IS A TRAPEZOID OF TRIANGULAR SHAPE                                 731
5790  CONTINUE                                                            731
         AG_TITLE='STRA'                                                  731
         AG_MODULE=MODULE                                                 731
         AG_MARK='V00'                                                    731
         CALL AGSPUSH                                                     731
         AG_MODULE=MODULE                                                 731
C    *                                                                    731
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          732
         CALL AGSRESET                                                    732
         AG_TITLE = 'SILICON'                                             732
         AG_EXNAME = 'MATERIAL'                                           732
         AG_PARLIST = 'NONE'                                              732
         CALL AXMATERIAL                                                  732
C    *                                                                    732
C    Geant MATERIAL : NONSENSSILI ===> ISVOL=0 <===                       732
         CALL AGSRESET                                                    733
         AG_ISVOL = 0                                                     733
         AG_TITLE = 'NONSENSSILI'                                         733
         AG_EXNAME = 'MATERIAL'                                           733
         AG_PARLIST = 'ISVOL'                                             733
         CALL AXMATERIAL                                                  733
C    *                                                                    733
C    Geant ATTRIBUTE : STRA ===> SEEN=1 COLO=2 <===                       733
         CALL AGSRESET                                                    734
         AG_SEEN = 1                                                      734
         AG_COLO = 2                                                      734
         AG_TITLE = 'STRA'                                                734
         AG_EXNAME = 'ATTRIBUTE'                                          734
         AG_PARLIST = 'SEEN_COLO'                                         734
         CALL AXATTRIBUTE                                                 734
C    *                                                                    734
C    Geant SHAPE : TRD1 ===> DX1=0.0 DX2=SWCA_SENSWID/2.0, DY=SWCA_WAFER  734
C    THK/2.0 DZ=(SWCA_SENSLEN-SWCA_SENSGIR)/2.0 <===                      734
         CALL AGSRESET                                                    736
         AG_DX1 = 0.0                                                     736
         AG_DX2 = SWCA_SENSWID/2.0                                        736
         AG_DY = SWCA_WAFERTHK/2.0                                        736
         AG_DZ = (SWCA_SENSLEN-SWCA_SENSGIR)/2.0                          736
         AG_TITLE = 'TRD1'                                                736
         AG_EXNAME = 'SHAPE'                                              736
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     736
         CALL AXSHAPE                                                     736
C    *                                                                    736
5791  CALL AGSPOPB                                                        737
      GO TO BLOCK_STRA, (5790 ,5800)                                      737
C End of Local Procedure                                                  737
C ----------------------------------------------------------------------  738
C ------------ SBER                                                       738
C Begin Local Procedure SBER                                              739
C SBER ARE THE BERILLIUM WAFER CARRIER RAILS                              739
5650  CONTINUE                                                            739
         AG_TITLE='SBER'                                                  739
         AG_MODULE=MODULE                                                 739
         AG_MARK='V00'                                                    739
         CALL AGSPUSH                                                     739
         AG_MODULE=MODULE                                                 739
C    *                                                                    739
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        740
         CALL AGSRESET                                                    740
         AG_TITLE = 'BERILLIUM'                                           740
         AG_EXNAME = 'MATERIAL'                                           740
         AG_PARLIST = 'NONE'                                              740
         CALL AXMATERIAL                                                  740
C    *                                                                    740
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       740
         CALL AGSRESET                                                    741
         AG_SEEN = 1                                                      741
         AG_COLO = 2                                                      741
         AG_TITLE = 'SBER'                                                741
         AG_EXNAME = 'ATTRIBUTE'                                          741
         AG_PARLIST = 'SEEN_COLO'                                         741
         CALL AXATTRIBUTE                                                 741
C    *                                                                    741
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  741
C    _WAFCARTH/2 <===                                                     741
         CALL AGSRESET                                                    743
         AG_DX = SWCA_WAFCARWD/2                                          743
         AG_DY = SWCA_LENGTH/2                                            743
         AG_DZ = SWCA_WAFCARTH/2                                          743
         AG_TITLE = 'BOX'                                                 743
         AG_EXNAME = 'SHAPE'                                              743
         AG_PARLIST = 'DX_DY_DZ'                                          743
         CALL AXSHAPE                                                     743
C    *                                                                    743
5651  CALL AGSPOPB                                                        744
      GO TO BLOCK_SBER, (5650 ,5660)                                      744
C End of Local Procedure                                                  744
C ----------------------------------------------------------------------  745
C ------------ STAB                                                       745
C Begin Local Procedure STAB                                              746
C STAB ARE THE BERILLIUM WAFER CARRIER END TABS                           746
5670  CONTINUE                                                            746
         AG_TITLE='STAB'                                                  746
         AG_MODULE=MODULE                                                 746
         AG_MARK='V00'                                                    746
         CALL AGSPUSH                                                     746
         AG_MODULE=MODULE                                                 746
C    *                                                                    746
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        747
         CALL AGSRESET                                                    747
         AG_TITLE = 'BERILLIUM'                                           747
         AG_EXNAME = 'MATERIAL'                                           747
         AG_PARLIST = 'NONE'                                              747
         CALL AXMATERIAL                                                  747
C    *                                                                    747
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       747
         CALL AGSRESET                                                    748
         AG_SEEN = 1                                                      748
         AG_COLO = 2                                                      748
         AG_TITLE = 'STAB'                                                748
         AG_EXNAME = 'ATTRIBUTE'                                          748
         AG_PARLIST = 'SEEN_COLO'                                         748
         CALL AXATTRIBUTE                                                 748
C    *                                                                    748
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  748
C    2 DZ=SWCA_WAFCARTH/2 <===                                            748
         CALL AGSRESET                                                    750
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            750
         AG_DY = TABLEN/2                                                 750
         AG_DZ = SWCA_WAFCARTH/2                                          750
         AG_TITLE = 'BOX'                                                 750
         AG_EXNAME = 'SHAPE'                                              750
         AG_PARLIST = 'DX_DY_DZ'                                          750
         CALL AXSHAPE                                                     750
C    *                                                                    750
5671  CALL AGSPOPB                                                        751
      GO TO BLOCK_STAB, (5670 ,5680)                                      751
C End of Local Procedure                                                  751
C ----------------------------------------------------------------------  752
C ------------ STRU                                                       752
C Begin Local Procedure STRU                                              753
C STRU ARE THE BERILLIUM STRUTS BETWEEN THE WAFER CARRIER RAILS           753
5690  CONTINUE                                                            753
         AG_TITLE='STRU'                                                  753
         AG_MODULE=MODULE                                                 753
         AG_MARK='V00'                                                    753
         CALL AGSPUSH                                                     753
         AG_MODULE=MODULE                                                 753
C    *                                                                    753
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        754
         CALL AGSRESET                                                    754
         AG_TITLE = 'BERILLIUM'                                           754
         AG_EXNAME = 'MATERIAL'                                           754
         AG_PARLIST = 'NONE'                                              754
         CALL AXMATERIAL                                                  754
C    *                                                                    754
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       754
         CALL AGSRESET                                                    755
         AG_SEEN = 1                                                      755
         AG_COLO = 2                                                      755
         AG_TITLE = 'STRU'                                                755
         AG_EXNAME = 'ATTRIBUTE'                                          755
         AG_PARLIST = 'SEEN_COLO'                                         755
         CALL AXATTRIBUTE                                                 755
C    *                                                                    755
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  755
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     755
         CALL AGSRESET                                                    757
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            757
         AG_DY = SWCA_STRUTLEN/2                                          757
         AG_DZ = SWCA_WAFCARTH/2                                          757
         AG_TITLE = 'BOX'                                                 757
         AG_EXNAME = 'SHAPE'                                              757
         AG_PARLIST = 'DX_DY_DZ'                                          757
         CALL AXSHAPE                                                     757
C    *                                                                    757
5691  CALL AGSPOPB                                                        758
      GO TO BLOCK_STRU, (5690 ,5700)                                      758
C End of Local Procedure                                                  758
C ----------------------------------------------------------------------  759
C ------------ SPCB                                                       759
C Begin Local Procedure SPCB                                              760
C SPCB IS THE G10 PCB                                                     760
5730  CONTINUE                                                            760
         AG_TITLE='SPCB'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
C    Geant MATERIAL : G10 ===> inherited arguments used <===              761
         CALL AGSRESET                                                    761
         AG_TITLE = 'G10'                                                 761
         AG_EXNAME = 'MATERIAL'                                           761
         AG_PARLIST = 'NONE'                                              761
         CALL AXMATERIAL                                                  761
C    *                                                                    761
C    Geant ATTRIBUTE : SPCB ===> SEEN=1 COLO=5 <===                       761
         CALL AGSRESET                                                    762
         AG_SEEN = 1                                                      762
         AG_COLO = 5                                                      762
         AG_TITLE = 'SPCB'                                                762
         AG_EXNAME = 'ATTRIBUTE'                                          762
         AG_PARLIST = 'SEEN_COLO'                                         762
         CALL AXATTRIBUTE                                                 762
C    *                                                                    762
C    Geant SHAPE : BOX ===> DX=SVTL_PCBWIDTH/2.0 DY=SVTL_PCBLEN/2.0, DZ=  762
C    SVTL_PCBTHK/2.0 <===                                                 762
         CALL AGSRESET                                                    764
         AG_DX = SVTL_PCBWIDTH/2.0                                        764
         AG_DY = SVTL_PCBLEN/2.0                                          764
         AG_DZ = SVTL_PCBTHK/2.0                                          764
         AG_TITLE = 'BOX'                                                 764
         AG_EXNAME = 'SHAPE'                                              764
         AG_PARLIST = 'DX_DY_DZ'                                          764
         CALL AXSHAPE                                                     764
C    *                                                                    764
5731  CALL AGSPOPB                                                        765
      GO TO BLOCK_SPCB, (5730 ,5740)                                      765
C End of Local Procedure                                                  765
C ----------------------------------------------------------------------  766
C ------------ SCBM                                                       766
C Begin Local Procedure SCBM                                              767
C SCBM IS THE MOTHER FOR THE BUNDLE OF CABLES GOING FROM PCBS             767
5390  CONTINUE                                                            767
         AG_TITLE='SCBM'                                                  767
         AG_MODULE=MODULE                                                 767
         AG_MARK='V00'                                                    767
         CALL AGSPUSH                                                     767
         AG_MODULE=MODULE                                                 767
C    *                                                                    767
C    Geant MATERIAL : AIR ===> inherited arguments used <===              768
         CALL AGSRESET                                                    768
         AG_TITLE = 'AIR'                                                 768
         AG_EXNAME = 'MATERIAL'                                           768
         AG_PARLIST = 'NONE'                                              768
         CALL AXMATERIAL                                                  768
C    *                                                                    768
C    Geant ATTRIBUTE : SCBM ===> SEEN=0 COLO=1 <===                       768
         CALL AGSRESET                                                    769
         AG_SEEN = 0                                                      769
         AG_COLO = 1                                                      769
         AG_TITLE = 'SCBM'                                                769
         AG_EXNAME = 'ATTRIBUTE'                                          769
         AG_PARLIST = 'SEEN_COLO'                                         769
         CALL AXATTRIBUTE                                                 769
C    *                                                                    769
C    Geant SHAPE : TUBE ===> RMIN=RADII(1) RMAX=SWAM_RMAX, DZ=SCBP_LEN <  769
C    ===                                                                  769
         CALL AGSRESET                                                    771
         AG_RMIN = RADII(1)                                               771
         AG_RMAX = SWAM_RMAX                                              771
         AG_DZ = SCBP_LEN                                                 771
         AG_TITLE = 'TUBE'                                                771
         AG_EXNAME = 'SHAPE'                                              771
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      771
         CALL AXSHAPE                                                     771
C    *                                                                    771
C    *                                                                    771
         ILAYER=1                                                         772
            GO TO 5853                                                    772
5851        ILAYER=ILAYER+1                                               772
5853        IF(ILAYER-(4).GT.0) GO TO 5852                                772
C       use SCBP bank                                                     773
            BPATH_SCBP='SCBP*'                                            773
            IDNUM_SCBP(1)=0                                               773
            SCBP_LAYER=ILAYER                                             773
            CALL AGDGETP('SVTTGEO7','*',-999,'SCBP',NSCBP_LAYER,          773
     *      RE_SCBP_LAYER,BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,    773
     *      BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)             773
C       Create SCBL :                                                     774
C       Call To Local Procedure SCBL                                      774
            ASSIGN 5870 TO BLOCK_SCBL                                     774
            GO TO  5860                                                   774
5870        CONTINUE                                                      774
C       *                                                                 774
C       Geant POSITION : SCBL ===> X=0.0 Y=0.0 Z=0.0 <===                 775
            CALL AGSRESET                                                 775
            AG_X = 0.0                                                    775
            AG_Y = 0.0                                                    775
            AG_Z = 0.0                                                    775
            AG_TITLE = 'SCBL'                                             775
            AG_EXNAME = 'POSITION'                                        775
            AG_PARLIST = 'X_Y_Z'                                          775
            CALL AXPOSITION                                               775
C       *                                                                 775
         GO TO 5851                                                       776
5852     CONTINUE                                                         776
C    *                                                                    777
         ILAYER=1                                                         777
            GO TO 5883                                                    777
5881        ILAYER=ILAYER+1                                               777
5883        IF(ILAYER-(3).GT.0) GO TO 5882                                777
C       use SFEP bank                                                     778
            BPATH_SFEP='SFEP*'                                            778
            IDNUM_SFEP(1)=0                                               778
            SFEP_LAYER=ILAYER                                             778
            CALL AGDGETP('SVTTGEO7','*',-999,'SFEP',NSFEP_LAYER,          778
     *      RE_SFEP_LAYER,BPATH_SFEP,IDNUM_SFEP,1,MAP_SFEP,NAMES_SFEP,    778
     *      BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)             778
C       Create SFED :                                                     779
C       Call To Local Procedure SFED                                      779
            ASSIGN 5900 TO BLOCK_SFED                                     779
            GO TO  5890                                                   779
5900        CONTINUE                                                      779
C       *                                                                 779
C       Geant POSITION : SFED ===> X=0.0 Y=0.0 Z=0.0 <===                 780
            CALL AGSRESET                                                 780
            AG_X = 0.0                                                    780
            AG_Y = 0.0                                                    780
            AG_Z = 0.0                                                    780
            AG_TITLE = 'SFED'                                             780
            AG_EXNAME = 'POSITION'                                        780
            AG_PARLIST = 'X_Y_Z'                                          780
            CALL AXPOSITION                                               780
C       *                                                                 780
C       Create SPLS :                                                     781
C       Call To Local Procedure SPLS                                      781
            ASSIGN 5920 TO BLOCK_SPLS                                     781
            GO TO  5910                                                   781
5920        CONTINUE                                                      781
C       *                                                                 781
C       Geant POSITION : SPLS ===> X=0.0 Y=0.0 Z=0.0 <===                 782
            CALL AGSRESET                                                 782
            AG_X = 0.0                                                    782
            AG_Y = 0.0                                                    782
            AG_Z = 0.0                                                    782
            AG_TITLE = 'SPLS'                                             782
            AG_EXNAME = 'POSITION'                                        782
            AG_PARLIST = 'X_Y_Z'                                          782
            CALL AXPOSITION                                               782
C       *                                                                 782
         GO TO 5881                                                       783
5882     CONTINUE                                                         783
5391  CALL AGSPOPB                                                        784
      GO TO BLOCK_SCBM, (5390 ,5400)                                      784
C End of Local Procedure                                                  784
C ----------------------------------------------------------------------  785
C ------------ SCBL                                                       785
C Begin Local Procedure SCBL                                              786
C SCBL IS THE BUNDLE OF CABLES GOING FROM PCBS TO MANIFOLDS               786
5860  CONTINUE                                                            786
         AG_TITLE='SCBL'                                                  786
         AG_MODULE=MODULE                                                 786
         AG_MARK='V00'                                                    786
         CALL AGSPUSH                                                     786
         AG_MODULE=MODULE                                                 786
C    *                                                                    786
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           787
         CALL AGSRESET                                                    787
         AG_TITLE = 'COPPER'                                              787
         AG_EXNAME = 'MATERIAL'                                           787
         AG_PARLIST = 'NONE'                                              787
         CALL AXMATERIAL                                                  787
C    *                                                                    787
C    Geant ATTRIBUTE : SCBL ===> SEEN=1 COLO=3 <===                       787
         CALL AGSRESET                                                    788
         AG_SEEN = 1                                                      788
         AG_COLO = 3                                                      788
         AG_TITLE = 'SCBL'                                                788
         AG_EXNAME = 'ATTRIBUTE'                                          788
         AG_PARLIST = 'SEEN_COLO'                                         788
         CALL AXATTRIBUTE                                                 788
C    *                                                                    788
         SQ =(SCBP_LEN*SCBP_LEN)                                          788
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  790
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  791
         A = PI                                                           792
         A=A*(SCBP_RMIN1*SCBP_RMIN1 + SCBP_RMIN2*SCBP_RMIN2)              793
         A=A*SQRT(1 + SQ)                                                 794
         CUTHK=(SCBP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            795
C    Geant SHAPE : CONE ===> DZ=SCBP_LEN RMN1=SCBP_RMIN1 RMX1=SCBP_RMIN1  797
C    +CUTHK, RMN2=SCBP_RMIN2 RMX2=SCBP_RMIN2+CUTHK <===                   797
         CALL AGSRESET                                                    797
         AG_DZ = SCBP_LEN                                                 797
         AG_RMN1 = SCBP_RMIN1                                             797
         AG_RMX1 = SCBP_RMIN1+CUTHK                                       797
         AG_RMN2 = SCBP_RMIN2                                             797
         AG_RMX2 = SCBP_RMIN2+CUTHK                                       797
         AG_TITLE = 'CONE'                                                797
         AG_EXNAME = 'SHAPE'                                              797
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            797
         CALL AXSHAPE                                                     797
C    *                                                                    797
5861  CALL AGSPOPB                                                        798
      GO TO BLOCK_SCBL, (5860 ,5870)                                      798
C End of Local Procedure                                                  798
C ----------------------------------------------------------------------  799
C ------------ SFED                                                       799
C Begin Local Procedure SFED                                              800
C SFED IS THE WATRER IN THE BUNDLE OF PIPES GOING TO MANIFOLDS            800
5890  CONTINUE                                                            800
         AG_TITLE='SFED'                                                  800
         AG_MODULE=MODULE                                                 800
         AG_MARK='V00'                                                    800
         CALL AGSPUSH                                                     800
         AG_MODULE=MODULE                                                 800
C    *                                                                    800
C    Geant MATERIAL : WATER ===> inherited arguments used <===            801
         CALL AGSRESET                                                    801
         AG_TITLE = 'WATER'                                               801
         AG_EXNAME = 'MATERIAL'                                           801
         AG_PARLIST = 'NONE'                                              801
         CALL AXMATERIAL                                                  801
C    *                                                                    801
C    Geant ATTRIBUTE : SFED ===> SEEN=1 COLO=1 <===                       801
         CALL AGSRESET                                                    802
         AG_SEEN = 1                                                      802
         AG_COLO = 1                                                      802
         AG_TITLE = 'SFED'                                                802
         AG_EXNAME = 'ATTRIBUTE'                                          802
         AG_PARLIST = 'SEEN_COLO'                                         802
         CALL AXATTRIBUTE                                                 802
C    *                                                                    802
         SQ = SFEP_LEN*SFEP_LEN                                           802
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  804
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  805
         A = PI                                                           806
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            807
         A=A*SQRT(1 + SQ)                                                 808
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            809
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1 RMX1=SFEP_RMIN1  811
C    +CUTHK, RMN2=SFEP_RMIN2 RMX2=SFEP_RMIN2+CUTHK <===                   811
         CALL AGSRESET                                                    811
         AG_DZ = SFEP_LEN                                                 811
         AG_RMN1 = SFEP_RMIN1                                             811
         AG_RMX1 = SFEP_RMIN1+CUTHK                                       811
         AG_RMN2 = SFEP_RMIN2                                             811
         AG_RMX2 = SFEP_RMIN2+CUTHK                                       811
         AG_TITLE = 'CONE'                                                811
         AG_EXNAME = 'SHAPE'                                              811
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            811
         CALL AXSHAPE                                                     811
C    *                                                                    811
5891  CALL AGSPOPB                                                        812
      GO TO BLOCK_SFED, (5890 ,5900)                                      812
C End of Local Procedure                                                  812
C ----------------------------------------------------------------------  813
C ------------ SPLS                                                       813
C Begin Local Procedure SPLS                                              814
C SPLS IS THE PLASTIC WALLS OF THE BUNDLE OF PIPES GOING TO MANIFOLDS     814
5910  CONTINUE                                                            814
         AG_TITLE='SPLS'                                                  814
         AG_MODULE=MODULE                                                 814
         AG_MARK='V00'                                                    814
         CALL AGSPUSH                                                     814
         AG_MODULE=MODULE                                                 814
C    *                                                                    814
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           815
         CALL AGSRESET                                                    815
         AG_A = 12                                                        815
         AG_Z = 6                                                         815
         AG_W = 1                                                         815
         AG_TITLE = 'C'                                                   815
         AG_EXNAME = 'COMPONENT'                                          815
         AG_PARLIST = 'A_Z_W'                                             815
         CALL AXCOMPONENT                                                 815
C    *                                                                    815
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           815
         CALL AGSRESET                                                    816
         AG_A = 1                                                         816
         AG_Z = 1                                                         816
         AG_W = 2                                                         816
         AG_TITLE = 'H2'                                                  816
         AG_EXNAME = 'COMPONENT'                                          816
         AG_PARLIST = 'A_Z_W'                                             816
         CALL AXCOMPONENT                                                 816
C    *                                                                    816
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             816
         CALL AGSRESET                                                    817
         AG_DENS = 0.935                                                  817
         AG_TITLE = 'CH2'                                                 817
         AG_EXNAME = 'MIXTURE'                                            817
         AG_PARLIST = 'DENS'                                              817
         CALL AXMIXTURE                                                   817
C    *                                                                    817
C    Geant ATTRIBUTE : SPLS ===> SEEN=1 COLO=6 <===                       817
         CALL AGSRESET                                                    818
         AG_SEEN = 1                                                      818
         AG_COLO = 6                                                      818
         AG_TITLE = 'SPLS'                                                818
         AG_EXNAME = 'ATTRIBUTE'                                          818
         AG_PARLIST = 'SEEN_COLO'                                         818
         CALL AXATTRIBUTE                                                 818
C    *                                                                    818
         SQ = SFEP_LEN*SFEP_LEN                                           818
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  820
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  821
         A = PI                                                           822
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            823
         A=A*SQRT(1 + SQ)                                                 824
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            825
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1+0.2, RMX1=SFEP_  828
C    RMIN1+0.2+CUTHK RMN2=SFEP_RMIN2+0.2, RMX2=SFEP_RMIN2+0.2+CUTHK <===  828
         CALL AGSRESET                                                    828
         AG_DZ = SFEP_LEN                                                 828
         AG_RMN1 = SFEP_RMIN1+0.2                                         828
         AG_RMX1 = SFEP_RMIN1+0.2+CUTHK                                   828
         AG_RMN2 = SFEP_RMIN2+0.2                                         828
         AG_RMX2 = SFEP_RMIN2+0.2+CUTHK                                   828
         AG_TITLE = 'CONE'                                                828
         AG_EXNAME = 'SHAPE'                                              828
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            828
         CALL AXSHAPE                                                     828
C    *                                                                    828
5911  CALL AGSPOPB                                                        829
      GO TO BLOCK_SPLS, (5910 ,5920)                                      829
C End of Local Procedure                                                  829
C ----------------------------------------------------------------------  830
C ------------ SELE                                                       830
C Begin Local Procedure SELE                                              831
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   831
5600  CONTINUE                                                            831
         AG_TITLE='SELE'                                                  831
         AG_MODULE=MODULE                                                 831
         AG_MARK='V00'                                                    831
         CALL AGSPUSH                                                     831
         AG_MODULE=MODULE                                                 831
C    *                                                                    831
C    Geant MATERIAL : AIR ===> inherited arguments used <===              832
         CALL AGSRESET                                                    832
         AG_TITLE = 'AIR'                                                 832
         AG_EXNAME = 'MATERIAL'                                           832
         AG_PARLIST = 'NONE'                                              832
         CALL AXMATERIAL                                                  832
C    *                                                                    832
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 SERIAL=ILAYER <===         832
         CALL AGSRESET                                                    833
         AG_SEEN = 0                                                      833
         AG_COLO = 1                                                      833
         AG_SERIAL = ILAYER                                               833
         AG_TITLE = 'SELE'                                                833
         AG_EXNAME = 'ATTRIBUTE'                                          833
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  833
         CALL AXATTRIBUTE                                                 833
C    *                                                                    833
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  833
C    K/2 <===                                                             833
         CALL AGSRESET                                                    835
         AG_DX = SELC_ELCAWID/2                                           835
         AG_DY = SWCA_LENGTH/2                                            835
         AG_DZ = ELETHK/2                                                 835
         AG_TITLE = 'BOX'                                                 835
         AG_EXNAME = 'SHAPE'                                              835
         AG_PARLIST = 'DX_DY_DZ'                                          835
         CALL AXSHAPE                                                     835
C    *                                                                    835
C    Geant ATTRIBUTE : SELE ===> SERIAL=0 <===                            835
         CALL AGSRESET                                                    836
         AG_SERIAL = 0                                                    836
         AG_TITLE = 'SELE'                                                836
         AG_EXNAME = 'ATTRIBUTE'                                          836
         AG_PARLIST = 'SERIAL'                                            836
         CALL AXATTRIBUTE                                                 836
C    *                                                                    836
C    Create SWCH :                                                        837
C    Call To Local Procedure SWCH                                         837
         ASSIGN 5940 TO BLOCK_SWCH                                        837
         GO TO  5930                                                      837
5940     CONTINUE                                                         837
C    *                                                                    837
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             838
         CALL AGSRESET                                                    838
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    838
         AG_TITLE = 'SWCH'                                                838
         AG_EXNAME = 'POSITION'                                           838
         AG_PARLIST = 'Z'                                                 838
         CALL AXPOSITION                                                  838
C    *                                                                    838
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  838
C    BETHK/2 <===                                                         838
         CALL AGSRESET                                                    839
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             839
         AG_TITLE = 'SWCH'                                                839
         AG_EXNAME = 'POSITION'                                           839
         AG_PARLIST = 'Z'                                                 839
         CALL AXPOSITION                                                  839
C    *                                                                    839
C    Create SWCS :                                                        840
C    Call To Local Procedure SWCS                                         840
         ASSIGN 5960 TO BLOCK_SWCS                                        840
         GO TO  5950                                                      840
5960     CONTINUE                                                         840
C    *                                                                    840
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  842
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     842
         CALL AGSRESET                                                    842
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              842
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        842
         AG_TITLE = 'SWCS'                                                842
         AG_EXNAME = 'POSITION'                                           842
         AG_PARLIST = 'X_Z'                                               842
         CALL AXPOSITION                                                  842
C    *                                                                    842
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  842
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     842
         CALL AGSRESET                                                    844
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              844
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        844
         AG_TITLE = 'SWCS'                                                844
         AG_EXNAME = 'POSITION'                                           844
         AG_PARLIST = 'X_Z'                                               844
         CALL AXPOSITION                                                  844
C    *                                                                    844
C    Create SWCW :                                                        845
C    Call To Local Procedure SWCW                                         845
         ASSIGN 5980 TO BLOCK_SWCW                                        845
         GO TO  5970                                                      845
5980     CONTINUE                                                         845
C    *                                                                    845
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  846
C    =                                                                    846
         CALL AGSRESET                                                    846
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        846
         AG_TITLE = 'SWCW'                                                846
         AG_EXNAME = 'POSITION'                                           846
         AG_PARLIST = 'Z'                                                 846
         CALL AXPOSITION                                                  846
C    *                                                                    846
C    Create SBOI :                                                        847
C    Call To Local Procedure SBOI                                         847
         ASSIGN 6000 TO BLOCK_SBOI                                        847
         GO TO  5990                                                      847
6000     CONTINUE                                                         847
C    *                                                                    847
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  848
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       848
         CALL AGSRESET                                                    848
         AG_Z = (ELETHK/                                                  848
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          848
         AG_TITLE = 'SBOI'                                                848
         AG_EXNAME = 'POSITION'                                           848
         AG_PARLIST = 'Z'                                                 848
         CALL AXPOSITION                                                  848
C    *                                                                    848
C    Create SGLA :                                                        849
C    Call To Local Procedure SGLA                                         849
         ASSIGN 6020 TO BLOCK_SGLA                                        849
         GO TO  6010                                                      849
6020     CONTINUE                                                         849
C    *                                                                    849
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  850
C    _GLASSTHK/2 <===                                                     850
         CALL AGSRESET                                                    850
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         850
         AG_TITLE = 'SGLA'                                                850
         AG_EXNAME = 'POSITION'                                           850
         AG_PARLIST = 'Z'                                                 850
         CALL AXPOSITION                                                  850
C    *                                                                    850
C    Create SAGP :                                                        851
C    Call To Local Procedure SAGP                                         851
         ASSIGN 6040 TO BLOCK_SAGP                                        851
         GO TO  6030                                                      851
6040     CONTINUE                                                         851
C    *                                                                    851
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  852
C    ==                                                                   852
         CALL AGSRESET                                                    852
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       852
         AG_TITLE = 'SAGP'                                                852
         AG_EXNAME = 'POSITION'                                           852
         AG_PARLIST = 'Z'                                                 852
         CALL AXPOSITION                                                  852
C    *                                                                    852
C    Create SDYE :                                                        853
C    Call To Local Procedure SDYE                                         853
         ASSIGN 6060 TO BLOCK_SDYE                                        853
         GO TO  6050                                                      853
6060     CONTINUE                                                         853
C    *                                                                    853
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  855
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     855
         CALL AGSRESET                                                    855
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  855
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    855
         AG_TITLE = 'SDYE'                                                855
         AG_EXNAME = 'POSITION'                                           855
         AG_PARLIST = 'X_Z'                                               855
         CALL AXPOSITION                                                  855
C    *                                                                    855
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  855
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             855
         CALL AGSRESET                                                    857
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          857
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    857
         AG_TITLE = 'SDYE'                                                857
         AG_EXNAME = 'POSITION'                                           857
         AG_PARLIST = 'X_Z'                                               857
         CALL AXPOSITION                                                  857
C    *                                                                    857
C    Create SECA :                                                        858
C    Call To Local Procedure SECA                                         858
         ASSIGN 6080 TO BLOCK_SECA                                        858
         GO TO  6070                                                      858
6080     CONTINUE                                                         858
C    *                                                                    858
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  860
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    860
         CALL AGSRESET                                                    860
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             860
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        860
         AG_TITLE = 'SECA'                                                860
         AG_EXNAME = 'POSITION'                                           860
         AG_PARLIST = 'X_Z'                                               860
         CALL AXPOSITION                                                  860
C    *                                                                    860
5601  CALL AGSPOPB                                                        861
      GO TO BLOCK_SELE, (5600 ,5610)                                      861
C End of Local Procedure                                                  861
C ----------------------------------------------------------------------  862
C ------------ SDYE                                                       862
C Begin Local Procedure SDYE                                              863
C SDYE IS THE IC CHIP ON THE HYBRID                                       863
6050  CONTINUE                                                            863
         AG_TITLE='SDYE'                                                  863
         AG_MODULE=MODULE                                                 863
         AG_MARK='V00'                                                    863
         CALL AGSPUSH                                                     863
         AG_MODULE=MODULE                                                 863
C    *                                                                    863
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          864
         CALL AGSRESET                                                    864
         AG_TITLE = 'SILICON'                                             864
         AG_EXNAME = 'MATERIAL'                                           864
         AG_PARLIST = 'NONE'                                              864
         CALL AXMATERIAL                                                  864
C    *                                                                    864
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 <===                       864
         CALL AGSRESET                                                    865
         AG_SEEN = 1                                                      865
         AG_COLO = 6                                                      865
         AG_TITLE = 'SDYE'                                                865
         AG_EXNAME = 'ATTRIBUTE'                                          865
         AG_PARLIST = 'SEEN_COLO'                                         865
         CALL AXATTRIBUTE                                                 865
C    *                                                                    865
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  865
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           865
         CALL AGSRESET                                                    867
         AG_DX = SELC_DYEWID/2                                            867
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              867
         AG_DZ = SELC_DYETHK/2                                            867
         AG_TITLE = 'BOX'                                                 867
         AG_EXNAME = 'SHAPE'                                              867
         AG_PARLIST = 'DX_DY_DZ'                                          867
         CALL AXSHAPE                                                     867
C    *                                                                    867
6051  CALL AGSPOPB                                                        868
      GO TO BLOCK_SDYE, (6050 ,6060)                                      868
C End of Local Procedure                                                  868
C ----------------------------------------------------------------------  869
C ------------ SECA                                                       869
C Begin Local Procedure SECA                                              870
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            870
6070  CONTINUE                                                            870
         AG_TITLE='SECA'                                                  870
         AG_MODULE=MODULE                                                 870
         AG_MARK='V00'                                                    870
         CALL AGSPUSH                                                     870
         AG_MODULE=MODULE                                                 870
C    *                                                                    870
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           871
         CALL AGSRESET                                                    871
         AG_TITLE = 'COPPER'                                              871
         AG_EXNAME = 'MATERIAL'                                           871
         AG_PARLIST = 'NONE'                                              871
         CALL AXMATERIAL                                                  871
C    *                                                                    871
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 <===                       871
         CALL AGSRESET                                                    872
         AG_SEEN = 1                                                      872
         AG_COLO = 2                                                      872
         AG_TITLE = 'SECA'                                                872
         AG_EXNAME = 'ATTRIBUTE'                                          872
         AG_PARLIST = 'SEEN_COLO'                                         872
         CALL AXATTRIBUTE                                                 872
C    *                                                                    872
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  872
C    ABTHK/2 <===                                                         872
         CALL AGSRESET                                                    874
         AG_DX = SELC_CABWID/2                                            874
         AG_DY = SWCA_LENGTH/2                                            874
         AG_DZ = SELC_CABTHK/2                                            874
         AG_TITLE = 'BOX'                                                 874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'DX_DY_DZ'                                          874
         CALL AXSHAPE                                                     874
C    *                                                                    874
6071  CALL AGSPOPB                                                        875
      GO TO BLOCK_SECA, (6070 ,6080)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ SBOI                                                       876
C Begin Local Procedure SBOI                                              877
C SBOI IS THE BERILLIA LAYER                                              877
5990  CONTINUE                                                            877
         AG_TITLE='SBOI'                                                  877
         AG_MODULE=MODULE                                                 877
         AG_MARK='V00'                                                    877
         CALL AGSPUSH                                                     877
         AG_MODULE=MODULE                                                 877
C    *                                                                    877
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1 <===                           878
         CALL AGSRESET                                                    878
         AG_A = 9                                                         878
         AG_Z = 4                                                         878
         AG_W = 1                                                         878
         AG_TITLE = 'BE'                                                  878
         AG_EXNAME = 'COMPONENT'                                          878
         AG_PARLIST = 'A_Z_W'                                             878
         CALL AXCOMPONENT                                                 878
C    *                                                                    878
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           878
         CALL AGSRESET                                                    879
         AG_A = 16                                                        879
         AG_Z = 8                                                         879
         AG_W = 1                                                         879
         AG_TITLE = 'O'                                                   879
         AG_EXNAME = 'COMPONENT'                                          879
         AG_PARLIST = 'A_Z_W'                                             879
         CALL AXCOMPONENT                                                 879
C    *                                                                    879
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              879
         CALL AGSRESET                                                    880
         AG_DENS = 2.85                                                   880
         AG_TITLE = 'BEO'                                                 880
         AG_EXNAME = 'MIXTURE'                                            880
         AG_PARLIST = 'DENS'                                              880
         CALL AXMIXTURE                                                   880
C    *                                                                    880
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 <===                       880
         CALL AGSRESET                                                    881
         AG_SEEN = 1                                                      881
         AG_COLO = 6                                                      881
         AG_TITLE = 'SBOI'                                                881
         AG_EXNAME = 'ATTRIBUTE'                                          881
         AG_PARLIST = 'SEEN_COLO'                                         881
         CALL AXATTRIBUTE                                                 881
C    *                                                                    881
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  881
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          881
         CALL AGSRESET                                                    883
         AG_DX = SELC_ELCAWID/2                                           883
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              883
         AG_DZ = SELC_BEOTHK/2                                            883
         AG_TITLE = 'BOX'                                                 883
         AG_EXNAME = 'SHAPE'                                              883
         AG_PARLIST = 'DX_DY_DZ'                                          883
         CALL AXSHAPE                                                     883
C    *                                                                    883
5991  CALL AGSPOPB                                                        884
      GO TO BLOCK_SBOI, (5990 ,6000)                                      884
C End of Local Procedure                                                  884
C ----------------------------------------------------------------------  885
C ------------ SAGP                                                       885
C Begin Local Procedure SAGP                                              886
C SAGP IS THE SILVER-PALLADIUM LAYER                                      886
6030  CONTINUE                                                            886
         AG_TITLE='SAGP'                                                  886
         AG_MODULE=MODULE                                                 886
         AG_MARK='V00'                                                    886
         CALL AGSPUSH                                                     886
         AG_MODULE=MODULE                                                 886
C    *                                                                    886
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1 <===                        887
         CALL AGSRESET                                                    887
         AG_A = 108                                                       887
         AG_Z = 47                                                        887
         AG_W = 1                                                         887
         AG_TITLE = 'AG'                                                  887
         AG_EXNAME = 'COMPONENT'                                          887
         AG_PARLIST = 'A_Z_W'                                             887
         CALL AXCOMPONENT                                                 887
C    *                                                                    887
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1 <===                        887
         CALL AGSRESET                                                    888
         AG_A = 106                                                       888
         AG_Z = 46                                                        888
         AG_W = 1                                                         888
         AG_TITLE = 'PD'                                                  888
         AG_EXNAME = 'COMPONENT'                                          888
         AG_PARLIST = 'A_Z_W'                                             888
         CALL AXCOMPONENT                                                 888
C    *                                                                    888
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            888
         CALL AGSRESET                                                    889
         AG_DENS = 11.25                                                  889
         AG_TITLE = 'AGPD'                                                889
         AG_EXNAME = 'MIXTURE'                                            889
         AG_PARLIST = 'DENS'                                              889
         CALL AXMIXTURE                                                   889
C    *                                                                    889
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 <===                       889
         CALL AGSRESET                                                    890
         AG_SEEN = 1                                                      890
         AG_COLO = 2                                                      890
         AG_TITLE = 'SAGP'                                                890
         AG_EXNAME = 'ATTRIBUTE'                                          890
         AG_PARLIST = 'SEEN_COLO'                                         890
         CALL AXATTRIBUTE                                                 890
C    *                                                                    890
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  890
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         890
         CALL AGSRESET                                                    892
         AG_DX = SELC_ELCAWID/2                                           892
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              892
         AG_DZ = SELC_AGPDTHK/2                                           892
         AG_TITLE = 'BOX'                                                 892
         AG_EXNAME = 'SHAPE'                                              892
         AG_PARLIST = 'DX_DY_DZ'                                          892
         CALL AXSHAPE                                                     892
C    *                                                                    892
6031  CALL AGSPOPB                                                        893
      GO TO BLOCK_SAGP, (6030 ,6040)                                      893
C End of Local Procedure                                                  893
C ----------------------------------------------------------------------  894
C ------------ SGLA                                                       894
C Begin Local Procedure SGLA                                              895
C SGLA IS THE INSULATING GLASS LAYER                                      895
6010  CONTINUE                                                            895
         AG_TITLE='SGLA'                                                  895
         AG_MODULE=MODULE                                                 895
         AG_MARK='V00'                                                    895
         CALL AGSPUSH                                                     895
         AG_MODULE=MODULE                                                 895
C    *                                                                    895
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         896
         CALL AGSRESET                                                    896
         AG_A = 28                                                        896
         AG_Z = 14                                                        896
         AG_W = 1                                                         896
         AG_TITLE = 'SI'                                                  896
         AG_EXNAME = 'COMPONENT'                                          896
         AG_PARLIST = 'A_Z_W'                                             896
         CALL AXCOMPONENT                                                 896
C    *                                                                    896
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           896
         CALL AGSRESET                                                    897
         AG_A = 16                                                        897
         AG_Z = 8                                                         897
         AG_W = 2                                                         897
         AG_TITLE = 'O'                                                   897
         AG_EXNAME = 'COMPONENT'                                          897
         AG_PARLIST = 'A_Z_W'                                             897
         CALL AXCOMPONENT                                                 897
C    *                                                                    897
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             897
         CALL AGSRESET                                                    898
         AG_DENS = 2.2                                                    898
         AG_TITLE = 'GLASS'                                               898
         AG_EXNAME = 'MIXTURE'                                            898
         AG_PARLIST = 'DENS'                                              898
         CALL AXMIXTURE                                                   898
C    *                                                                    898
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 <===                       898
         CALL AGSRESET                                                    899
         AG_SEEN = 1                                                      899
         AG_COLO = 6                                                      899
         AG_TITLE = 'SGLA'                                                899
         AG_EXNAME = 'ATTRIBUTE'                                          899
         AG_PARLIST = 'SEEN_COLO'                                         899
         CALL AXATTRIBUTE                                                 899
C    *                                                                    899
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  899
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        899
         CALL AGSRESET                                                    901
         AG_DX = SELC_ELCAWID/2                                           901
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              901
         AG_DZ = SELC_GLASSTHK/2                                          901
         AG_TITLE = 'BOX'                                                 901
         AG_EXNAME = 'SHAPE'                                              901
         AG_PARLIST = 'DX_DY_DZ'                                          901
         CALL AXSHAPE                                                     901
C    *                                                                    901
6011  CALL AGSPOPB                                                        902
      GO TO BLOCK_SGLA, (6010 ,6020)                                      902
C End of Local Procedure                                                  902
C ----------------------------------------------------------------------  903
C ------------ SWCH                                                       903
C Begin Local Procedure SWCH                                              904
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      904
5930  CONTINUE                                                            904
         AG_TITLE='SWCH'                                                  904
         AG_MODULE=MODULE                                                 904
         AG_MARK='V00'                                                    904
         CALL AGSPUSH                                                     904
         AG_MODULE=MODULE                                                 904
C    *                                                                    904
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        905
         CALL AGSRESET                                                    905
         AG_TITLE = 'BERILLIUM'                                           905
         AG_EXNAME = 'MATERIAL'                                           905
         AG_PARLIST = 'NONE'                                              905
         CALL AXMATERIAL                                                  905
C    *                                                                    905
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 <===                       905
         CALL AGSRESET                                                    906
         AG_SEEN = 1                                                      906
         AG_COLO = 2                                                      906
         AG_TITLE = 'SWCH'                                                906
         AG_EXNAME = 'ATTRIBUTE'                                          906
         AG_PARLIST = 'SEEN_COLO'                                         906
         CALL AXATTRIBUTE                                                 906
C    *                                                                    906
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  906
C    BETHK/2 <===                                                         906
         CALL AGSRESET                                                    908
         AG_DX = SELC_ELCAWID/2                                           908
         AG_DY = SWCA_LENGTH/2                                            908
         AG_DZ = SELC_BETHK/2                                             908
         AG_TITLE = 'BOX'                                                 908
         AG_EXNAME = 'SHAPE'                                              908
         AG_PARLIST = 'DX_DY_DZ'                                          908
         CALL AXSHAPE                                                     908
C    *                                                                    908
5931  CALL AGSPOPB                                                        909
      GO TO BLOCK_SWCH, (5930 ,5940)                                      909
C End of Local Procedure                                                  909
C ----------------------------------------------------------------------  910
C ------------ SWCS                                                       910
C Begin Local Procedure SWCS                                              911
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                911
5950  CONTINUE                                                            911
         AG_TITLE='SWCS'                                                  911
         AG_MODULE=MODULE                                                 911
         AG_MARK='V00'                                                    911
         CALL AGSPUSH                                                     911
         AG_MODULE=MODULE                                                 911
C    *                                                                    911
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        912
         CALL AGSRESET                                                    912
         AG_TITLE = 'BERILLIUM'                                           912
         AG_EXNAME = 'MATERIAL'                                           912
         AG_PARLIST = 'NONE'                                              912
         CALL AXMATERIAL                                                  912
C    *                                                                    912
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 <===                       912
         CALL AGSRESET                                                    913
         AG_SEEN = 1                                                      913
         AG_COLO = 2                                                      913
         AG_TITLE = 'SWCS'                                                913
         AG_EXNAME = 'ATTRIBUTE'                                          913
         AG_PARLIST = 'SEEN_COLO'                                         913
         CALL AXATTRIBUTE                                                 913
C    *                                                                    913
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  913
C    TTHK/2 <===                                                          913
         CALL AGSRESET                                                    915
         AG_DX = SELC_BETHK/2                                             915
         AG_DY = SWCA_LENGTH/2                                            915
         AG_DZ = SELC_WATTHK/2                                            915
         AG_TITLE = 'BOX'                                                 915
         AG_EXNAME = 'SHAPE'                                              915
         AG_PARLIST = 'DX_DY_DZ'                                          915
         CALL AXSHAPE                                                     915
C    *                                                                    915
5951  CALL AGSPOPB                                                        916
      GO TO BLOCK_SWCS, (5950 ,5960)                                      916
C End of Local Procedure                                                  916
C ----------------------------------------------------------------------  917
C ------------ SWCW                                                       917
C Begin Local Procedure SWCW                                              918
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       918
5970  CONTINUE                                                            918
         AG_TITLE='SWCW'                                                  918
         AG_MODULE=MODULE                                                 918
         AG_MARK='V00'                                                    918
         CALL AGSPUSH                                                     918
         AG_MODULE=MODULE                                                 918
C    *                                                                    918
C    Geant MATERIAL : WATER ===> inherited arguments used <===            919
         CALL AGSRESET                                                    919
         AG_TITLE = 'WATER'                                               919
         AG_EXNAME = 'MATERIAL'                                           919
         AG_PARLIST = 'NONE'                                              919
         CALL AXMATERIAL                                                  919
C    *                                                                    919
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 <===                       919
         CALL AGSRESET                                                    920
         AG_SEEN = 1                                                      920
         AG_COLO = 6                                                      920
         AG_TITLE = 'SWCW'                                                920
         AG_EXNAME = 'ATTRIBUTE'                                          920
         AG_PARLIST = 'SEEN_COLO'                                         920
         CALL AXATTRIBUTE                                                 920
C    *                                                                    920
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  920
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     920
         CALL AGSRESET                                                    922
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          922
         AG_DY = SWCA_LENGTH/2                                            922
         AG_DZ = (SELC_WATTHK/2)                                          922
         AG_TITLE = 'BOX'                                                 922
         AG_EXNAME = 'SHAPE'                                              922
         AG_PARLIST = 'DX_DY_DZ'                                          922
         CALL AXSHAPE                                                     922
C    *                                                                    922
5971  CALL AGSPOPB                                                        923
      GO TO BLOCK_SWCW, (5970 ,5980)                                      923
C End of Local Procedure                                                  923
C ----------------------------------------------------------------------  924
C ------------ SIRT                                                       924
C Begin Local Procedure SIRT                                              925
C SIRT IS THE SVT INNER END RING TUBE                                     925
5070  CONTINUE                                                            925
         AG_TITLE='SIRT'                                                  925
         AG_MODULE=MODULE                                                 925
         AG_MARK='V00'                                                    925
         CALL AGSPUSH                                                     925
         AG_MODULE=MODULE                                                 925
C    *                                                                    925
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        926
         CALL AGSRESET                                                    926
         AG_TITLE = 'BERILLIUM'                                           926
         AG_EXNAME = 'MATERIAL'                                           926
         AG_PARLIST = 'NONE'                                              926
         CALL AXMATERIAL                                                  926
C    *                                                                    926
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       926
         CALL AGSRESET                                                    927
         AG_SEEN = 1                                                      927
         AG_COLO = 2                                                      927
         AG_TITLE = 'SIRT'                                                927
         AG_EXNAME = 'ATTRIBUTE'                                          927
         AG_PARLIST = 'SEEN_COLO'                                         927
         CALL AXATTRIBUTE                                                 927
C    *                                                                    927
         IR_RMIN = COS(PI/8.0)                                            927
         IR_RMIN=IR_RMIN+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/           929
     *   8.0)*SIN(PI/8.0))                                                929
         IR_RMIN=IR_RMIN*SERG_IRNGPRMN                                    930
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  932
C    DRNGTH/2 <===                                                        932
         CALL AGSRESET                                                    932
         AG_RMIN = IR_RMIN                                                932
         AG_RMAX = SERG_IRNGTRMX                                          932
         AG_DZ = SERG_ENDRNGTH/2                                          932
         AG_TITLE = 'TUBE'                                                932
         AG_EXNAME = 'SHAPE'                                              932
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      932
         CALL AXSHAPE                                                     932
C    *                                                                    932
5071  CALL AGSPOPB                                                        933
      GO TO BLOCK_SIRT, (5070 ,5080)                                      933
C End of Local Procedure                                                  933
C ----------------------------------------------------------------------  934
C ------------ SIRP                                                       934
C Begin Local Procedure SIRP                                              935
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 935
5050  CONTINUE                                                            935
         AG_TITLE='SIRP'                                                  935
         AG_MODULE=MODULE                                                 935
         AG_MARK='V00'                                                    935
         CALL AGSPUSH                                                     935
         AG_MODULE=MODULE                                                 935
C    *                                                                    935
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        936
         CALL AGSRESET                                                    936
         AG_TITLE = 'BERILLIUM'                                           936
         AG_EXNAME = 'MATERIAL'                                           936
         AG_PARLIST = 'NONE'                                              936
         CALL AXMATERIAL                                                  936
C    *                                                                    936
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       936
         CALL AGSRESET                                                    937
         AG_SEEN = 1                                                      937
         AG_COLO = 2                                                      937
         AG_TITLE = 'SIRP'                                                937
         AG_EXNAME = 'ATTRIBUTE'                                          937
         AG_PARLIST = 'SEEN_COLO'                                         937
         CALL AXATTRIBUTE                                                 937
C    *                                                                    937
         ROU = COS(PI/8.0)                                                937
         ROU=ROU+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/8.0)*SIN(PI/       939
     *   8.0))                                                            939
         ROU = SERG_IRNGTRMX/ROU                                          940
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  943
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  943
C    OU,ROU} <===                                                         943
         CALL AGSRESET                                                    943
         AG_PHI1 = 0                                                      943
         AG_DPHI = 360                                                    943
         AG_NPDIV = 8                                                     943
         AG_NZ = 2                                                        943
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        943
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        943
         AG_RMN(1)=SERG_IRNGPRMN                                          943
         AG_RMN(2)=SERG_IRNGPRMN                                          943
         AG_RMX(1)=ROU                                                    943
         AG_RMX(2)=ROU                                                    943
         AG_TITLE = 'PGON'                                                943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     943
         CALL AXSHAPE                                                     943
C    *                                                                    943
5051  CALL AGSPOPB                                                        944
      GO TO BLOCK_SIRP, (5050 ,5060)                                      944
C End of Local Procedure                                                  944
C ----------------------------------------------------------------------  945
C ------------ SOER                                                       945
C Begin Local Procedure SOER                                              946
C SOER IS THE SVT OUTER END RING                                          946
5090  CONTINUE                                                            946
         AG_TITLE='SOER'                                                  946
         AG_MODULE=MODULE                                                 946
         AG_MARK='V00'                                                    946
         CALL AGSPUSH                                                     946
         AG_MODULE=MODULE                                                 946
C    *                                                                    946
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        947
         CALL AGSRESET                                                    947
         AG_TITLE = 'BERILLIUM'                                           947
         AG_EXNAME = 'MATERIAL'                                           947
         AG_PARLIST = 'NONE'                                              947
         CALL AXMATERIAL                                                  947
C    *                                                                    947
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       947
         CALL AGSRESET                                                    948
         AG_SEEN = 1                                                      948
         AG_COLO = 2                                                      948
         AG_TITLE = 'SOER'                                                948
         AG_EXNAME = 'ATTRIBUTE'                                          948
         AG_PARLIST = 'SEEN_COLO'                                         948
         CALL AXATTRIBUTE                                                 948
C    *                                                                    948
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  948
C    ERG_ENDRNGTH/2 <===                                                  948
         CALL AGSRESET                                                    950
         AG_RMIN = SERG_ORNGRMIN                                          950
         AG_RMAX = SERG_ORNGRMAX                                          950
         AG_DZ = SERG_ENDRNGTH/2                                          950
         AG_TITLE = 'TUBE'                                                950
         AG_EXNAME = 'SHAPE'                                              950
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      950
         CALL AXSHAPE                                                     950
C    *                                                                    950
5091  CALL AGSPOPB                                                        951
      GO TO BLOCK_SOER, (5090 ,5100)                                      951
C End of Local Procedure                                                  951
C ----------------------------------------------------------------------  952
C ------------ SIES                                                       952
C Begin Local Procedure SIES                                              953
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         953
5150  CONTINUE                                                            953
         AG_TITLE='SIES'                                                  953
         AG_MODULE=MODULE                                                 953
         AG_MARK='V00'                                                    953
         CALL AGSPUSH                                                     953
         AG_MODULE=MODULE                                                 953
C    *                                                                    953
C    Geant MATERIAL : AIR ===> inherited arguments used <===              954
         CALL AGSRESET                                                    954
         AG_TITLE = 'AIR'                                                 954
         AG_EXNAME = 'MATERIAL'                                           954
         AG_PARLIST = 'NONE'                                              954
         CALL AXMATERIAL                                                  954
C    *                                                                    954
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       954
         CALL AGSRESET                                                    955
         AG_SEEN = 0                                                      955
         AG_COLO = 1                                                      955
         AG_TITLE = 'SIES'                                                955
         AG_EXNAME = 'ATTRIBUTE'                                          955
         AG_PARLIST = 'SEEN_COLO'                                         955
         CALL AXATTRIBUTE                                                 955
C    *                                                                    955
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  955
C    ACK_Z-ENDRNG_Z) <===                                                 955
         CALL AGSRESET                                                    957
         AG_RMIN = IR_RMIN                                                957
         AG_RMAX = SERG_IRNGTRMX                                          957
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   957
         AG_TITLE = 'TUBE'                                                957
         AG_EXNAME = 'SHAPE'                                              957
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      957
         CALL AXSHAPE                                                     957
C    *                                                                    957
C    Create SISM :                                                        958
C    Call To Local Procedure SISM                                         958
         ASSIGN 6100 TO BLOCK_SISM                                        958
         GO TO  6090                                                      958
6100     CONTINUE                                                         958
C    *                                                                    958
5151  CALL AGSPOPB                                                        959
      GO TO BLOCK_SIES, (5150 ,5160)                                      959
C End of Local Procedure                                                  959
C ----------------------------------------------------------------------  960
C ------------ SISM                                                       960
C Begin Local Procedure SISM                                              961
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        961
6090  CONTINUE                                                            961
         AG_TITLE='SISM'                                                  961
         AG_MODULE=MODULE                                                 961
         AG_MARK='V00'                                                    961
         CALL AGSPUSH                                                     961
         AG_MODULE=MODULE                                                 961
C    *                                                                    961
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       962
         CALL AGSRESET                                                    962
         AG_SEEN = 0                                                      962
         AG_COLO = 1                                                      962
         AG_TITLE = 'SISM'                                                962
         AG_EXNAME = 'ATTRIBUTE'                                          962
         AG_PARLIST = 'SEEN_COLO'                                         962
         CALL AXATTRIBUTE                                                 962
C    *                                                                    962
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                962
         CALL AGSRESET                                                    963
         AG_NDIV = 4                                                      963
         AG_IAXIS = 2                                                     963
         AG_C0 = 45                                                       963
         AG_TITLE = 'DIVISION'                                            963
         AG_EXNAME = 'SHAPE'                                              963
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     963
         CALL AXSHAPE                                                     963
C    *                                                                    963
C    Create SCRW :                                                        964
C    Call To Local Procedure SCRW                                         964
         ASSIGN 6120 TO BLOCK_SCRW                                        964
         GO TO  6110                                                      964
6120     CONTINUE                                                         964
C    *                                                                    964
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  965
C    ===                                                                  965
         CALL AGSRESET                                                    965
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      965
         AG_TITLE = 'SCRW'                                                965
         AG_EXNAME = 'POSITION'                                           965
         AG_PARLIST = 'X'                                                 965
         CALL AXPOSITION                                                  965
C    *                                                                    965
6091  CALL AGSPOPB                                                        966
      GO TO BLOCK_SISM, (6090 ,6100)                                      966
C End of Local Procedure                                                  966
C ----------------------------------------------------------------------  967
C ------------ SOES                                                       967
C Begin Local Procedure SOES                                              968
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         968
5130  CONTINUE                                                            968
         AG_TITLE='SOES'                                                  968
         AG_MODULE=MODULE                                                 968
         AG_MARK='V00'                                                    968
         CALL AGSPUSH                                                     968
         AG_MODULE=MODULE                                                 968
C    *                                                                    968
C    Geant MATERIAL : AIR ===> inherited arguments used <===              969
         CALL AGSRESET                                                    969
         AG_TITLE = 'AIR'                                                 969
         AG_EXNAME = 'MATERIAL'                                           969
         AG_PARLIST = 'NONE'                                              969
         CALL AXMATERIAL                                                  969
C    *                                                                    969
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       969
         CALL AGSRESET                                                    970
         AG_SEEN = 0                                                      970
         AG_COLO = 1                                                      970
         AG_TITLE = 'SOES'                                                970
         AG_EXNAME = 'ATTRIBUTE'                                          970
         AG_PARLIST = 'SEEN_COLO'                                         970
         CALL AXATTRIBUTE                                                 970
C    *                                                                    970
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  970
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           970
         CALL AGSRESET                                                    972
         AG_RMIN = SERG_ORNGRMIN                                          972
         AG_RMAX = SERG_ORNGRMAX                                          972
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   972
         AG_TITLE = 'TUBE'                                                972
         AG_EXNAME = 'SHAPE'                                              972
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      972
         CALL AXSHAPE                                                     972
C    *                                                                    972
C    Create SOSM :                                                        973
C    Call To Local Procedure SOSM                                         973
         ASSIGN 6140 TO BLOCK_SOSM                                        973
         GO TO  6130                                                      973
6140     CONTINUE                                                         973
C    *                                                                    973
5131  CALL AGSPOPB                                                        974
      GO TO BLOCK_SOES, (5130 ,5140)                                      974
C End of Local Procedure                                                  974
C ----------------------------------------------------------------------  975
C ------------ SOSM                                                       975
C Begin Local Procedure SOSM                                              976
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        976
6130  CONTINUE                                                            976
         AG_TITLE='SOSM'                                                  976
         AG_MODULE=MODULE                                                 976
         AG_MARK='V00'                                                    976
         CALL AGSPUSH                                                     976
         AG_MODULE=MODULE                                                 976
C    *                                                                    976
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       977
         CALL AGSRESET                                                    977
         AG_SEEN = 0                                                      977
         AG_COLO = 1                                                      977
         AG_TITLE = 'SOSM'                                                977
         AG_EXNAME = 'ATTRIBUTE'                                          977
         AG_PARLIST = 'SEEN_COLO'                                         977
         CALL AXATTRIBUTE                                                 977
C    *                                                                    977
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                977
         CALL AGSRESET                                                    978
         AG_NDIV = 4                                                      978
         AG_IAXIS = 2                                                     978
         AG_C0 = 45                                                       978
         AG_TITLE = 'DIVISION'                                            978
         AG_EXNAME = 'SHAPE'                                              978
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     978
         CALL AXSHAPE                                                     978
C    *                                                                    978
C    Create SCRW :                                                        979
C    Call To Local Procedure SCRW                                         979
         ASSIGN 6150 TO BLOCK_SCRW                                        979
         GO TO  6110                                                      979
6150     CONTINUE                                                         979
C    *                                                                    979
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  980
C    ===                                                                  980
         CALL AGSRESET                                                    980
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      980
         AG_TITLE = 'SCRW'                                                980
         AG_EXNAME = 'POSITION'                                           980
         AG_PARLIST = 'X'                                                 980
         CALL AXPOSITION                                                  980
C    *                                                                    980
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  980
C    ===                                                                  980
         CALL AGSRESET                                                    981
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      981
         AG_TITLE = 'SCRW'                                                981
         AG_EXNAME = 'POSITION'                                           981
         AG_PARLIST = 'X'                                                 981
         CALL AXPOSITION                                                  981
C    *                                                                    981
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  981
C    =SSUP_ERJ2Y <===                                                     981
         CALL AGSRESET                                                    982
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      982
         AG_Y = SSUP_ERJ2Y                                                982
         AG_TITLE = 'SCRW'                                                982
         AG_EXNAME = 'POSITION'                                           982
         AG_PARLIST = 'X_Y'                                               982
         CALL AXPOSITION                                                  982
C    *                                                                    982
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  982
C    =-SSUP_ERJ2Y <===                                                    982
         CALL AGSRESET                                                    983
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      983
         AG_Y = -SSUP_ERJ2Y                                               983
         AG_TITLE = 'SCRW'                                                983
         AG_EXNAME = 'POSITION'                                           983
         AG_PARLIST = 'X_Y'                                               983
         CALL AXPOSITION                                                  983
C    *                                                                    983
6131  CALL AGSPOPB                                                        984
      GO TO BLOCK_SOSM, (6130 ,6140)                                      984
C End of Local Procedure                                                  984
C ----------------------------------------------------------------------  985
C ------------ SBRG                                                       985
C Begin Local Procedure SBRG                                              986
C SBRG IS THE BRACKET JOINING THE END RINGS                               986
5110  CONTINUE                                                            986
         AG_TITLE='SBRG'                                                  986
         AG_MODULE=MODULE                                                 986
         AG_MARK='V00'                                                    986
         CALL AGSPUSH                                                     986
         AG_MODULE=MODULE                                                 986
C    *                                                                    986
         RIN=SERG_IRNGPRMN                                                987
         ROU=SERG_ORNGRMAX                                                988
C    Geant MATERIAL : AIR ===> inherited arguments used <===              989
         CALL AGSRESET                                                    989
         AG_TITLE = 'AIR'                                                 989
         AG_EXNAME = 'MATERIAL'                                           989
         AG_PARLIST = 'NONE'                                              989
         CALL AXMATERIAL                                                  989
C    *                                                                    989
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       989
         CALL AGSRESET                                                    990
         AG_SEEN = 0                                                      990
         AG_COLO = 1                                                      990
         AG_TITLE = 'SBRG'                                                990
         AG_EXNAME = 'ATTRIBUTE'                                          990
         AG_PARLIST = 'SEEN_COLO'                                         990
         CALL AXATTRIBUTE                                                 990
C    *                                                                    990
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      990
         CALL AGSRESET                                                    991
         AG_RMIN = RIN                                                    991
         AG_RMAX = ROU                                                    991
         AG_DZ = SSUP_ERJTHK/2                                            991
         AG_TITLE = 'TUBE'                                                991
         AG_EXNAME = 'SHAPE'                                              991
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      991
         CALL AXSHAPE                                                     991
C    *                                                                    991
C    Create SBRM :                                                        992
C    Call To Local Procedure SBRM                                         992
         ASSIGN 6170 TO BLOCK_SBRM                                        992
         GO TO  6160                                                      992
6170     CONTINUE                                                         992
C    *                                                                    992
5111  CALL AGSPOPB                                                        993
      GO TO BLOCK_SBRG, (5110 ,5120)                                      993
C End of Local Procedure                                                  993
C ----------------------------------------------------------------------  994
C ------------ SBRM                                                       994
C Begin Local Procedure SBRM                                              995
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          995
6160  CONTINUE                                                            995
         AG_TITLE='SBRM'                                                  995
         AG_MODULE=MODULE                                                 995
         AG_MARK='V00'                                                    995
         CALL AGSPUSH                                                     995
         AG_MODULE=MODULE                                                 995
C    *                                                                    995
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       996
         CALL AGSRESET                                                    996
         AG_SEEN = 0                                                      996
         AG_COLO = 1                                                      996
         AG_TITLE = 'SBRM'                                                996
         AG_EXNAME = 'ATTRIBUTE'                                          996
         AG_PARLIST = 'SEEN_COLO'                                         996
         CALL AXATTRIBUTE                                                 996
C    *                                                                    996
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                996
         CALL AGSRESET                                                    997
         AG_NDIV = 4                                                      997
         AG_IAXIS = 2                                                     997
         AG_C0 = 45                                                       997
         AG_TITLE = 'DIVISION'                                            997
         AG_EXNAME = 'SHAPE'                                              997
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     997
         CALL AXSHAPE                                                     997
C    *                                                                    997
C    Create SBRI :                                                        998
C    Call To Local Procedure SBRI                                         998
         ASSIGN 6190 TO BLOCK_SBRI                                        998
         GO TO  6180                                                      998
6190     CONTINUE                                                         998
C    *                                                                    998
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        999
         CALL AGSRESET                                                    999
         AG_X = SSUP_ERJRAD                                               999
         AG_TITLE = 'SBRI'                                                999
         AG_EXNAME = 'POSITION'                                           999
         AG_PARLIST = 'X'                                                 999
         CALL AXPOSITION                                                  999
C    *                                                                    999
6161  CALL AGSPOPB                                                        1000
      GO TO BLOCK_SBRM, (6160 ,6170)                                      1000
C End of Local Procedure                                                  1000
C ----------------------------------------------------------------------  1001
C ------------ SBRI                                                       1001
C Begin Local Procedure SBRI                                              1002
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               1002
6180  CONTINUE                                                            1002
         AG_TITLE='SBRI'                                                  1002
         AG_MODULE=MODULE                                                 1002
         AG_MARK='V00'                                                    1002
         CALL AGSPUSH                                                     1002
         AG_MODULE=MODULE                                                 1002
C    *                                                                    1002
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       1003
         CALL AGSRESET                                                    1003
         AG_SEEN = 1                                                      1003
         AG_COLO = 2                                                      1003
         AG_TITLE = 'SBRI'                                                1003
         AG_EXNAME = 'ATTRIBUTE'                                          1003
         AG_PARLIST = 'SEEN_COLO'                                         1003
         CALL AXATTRIBUTE                                                 1003
C    *                                                                    1003
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        1003
         CALL AGSRESET                                                    1004
         AG_TITLE = 'BERILLIUM'                                           1004
         AG_EXNAME = 'MATERIAL'                                           1004
         AG_PARLIST = 'NONE'                                              1004
         CALL AXMATERIAL                                                  1004
C    *                                                                    1004
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  1004
C    RJTHK/2 <===                                                         1004
         CALL AGSRESET                                                    1006
         AG_DX = SSUP_ERJLEN/2                                            1006
         AG_DY = SSUP_ERJWID/2                                            1006
         AG_DZ = SSUP_ERJTHK/2                                            1006
         AG_TITLE = 'BOX'                                                 1006
         AG_EXNAME = 'SHAPE'                                              1006
         AG_PARLIST = 'DX_DY_DZ'                                          1006
         CALL AXSHAPE                                                     1006
C    *                                                                    1006
6181  CALL AGSPOPB                                                        1007
      GO TO BLOCK_SBRI, (6180 ,6190)                                      1007
C End of Local Procedure                                                  1007
C ----------------------------------------------------------------------  1008
C ------------ SROD                                                       1008
C Begin Local Procedure SROD                                              1009
C SROD IS THE SVT CARBON COMPOSITE SUPPORT ROD                            1009
5250  CONTINUE                                                            1009
         AG_TITLE='SROD'                                                  1009
         AG_MODULE=MODULE                                                 1009
         AG_MARK='V00'                                                    1009
         CALL AGSPUSH                                                     1009
         AG_MODULE=MODULE                                                 1009
C    *                                                                    1009
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1010
         CALL AGSRESET                                                    1010
         AG_TITLE = 'CARBON'                                              1010
         AG_EXNAME = 'MATERIAL'                                           1010
         AG_PARLIST = 'NONE'                                              1010
         CALL AXMATERIAL                                                  1010
C    *                                                                    1010
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       1010
         CALL AGSRESET                                                    1011
         AG_SEEN = 1                                                      1011
         AG_COLO = 2                                                      1011
         AG_TITLE = 'SROD'                                                1011
         AG_EXNAME = 'ATTRIBUTE'                                          1011
         AG_PARLIST = 'SEEN_COLO'                                         1011
         CALL AXATTRIBUTE                                                 1011
C    *                                                                    1011
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  1011
C    P_RODLEN/2 <===                                                      1011
         CALL AGSRESET                                                    1013
         AG_RMIN = SSUP_RODID/2                                           1013
         AG_RMAX = SSUP_RODOD/2                                           1013
         AG_DZ = SSUP_RODLEN/2                                            1013
         AG_TITLE = 'TUBE'                                                1013
         AG_EXNAME = 'SHAPE'                                              1013
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1013
         CALL AXSHAPE                                                     1013
C    *                                                                    1013
5251  CALL AGSPOPB                                                        1014
      GO TO BLOCK_SROD, (5250 ,5260)                                      1014
C End of Local Procedure                                                  1014
C ----------------------------------------------------------------------  1015
C ------------ SCON                                                       1015
C Begin Local Procedure SCON                                              1016
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               1016
5210  CONTINUE                                                            1016
         AG_TITLE='SCON'                                                  1016
         AG_MODULE=MODULE                                                 1016
         AG_MARK='V00'                                                    1016
         CALL AGSPUSH                                                     1016
         AG_MODULE=MODULE                                                 1016
C    *                                                                    1016
         CONE_THK1=SSUP_GRPHTHK                                           1017
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                1018
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              1019
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1020
         CALL AGSRESET                                                    1020
         AG_TITLE = 'AIR'                                                 1020
         AG_EXNAME = 'MATERIAL'                                           1020
         AG_PARLIST = 'NONE'                                              1020
         CALL AXMATERIAL                                                  1020
C    *                                                                    1020
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       1020
         CALL AGSRESET                                                    1021
         AG_SEEN = 0                                                      1021
         AG_COLO = 1                                                      1021
         AG_TITLE = 'SCON'                                                1021
         AG_EXNAME = 'ATTRIBUTE'                                          1021
         AG_PARLIST = 'SEEN_COLO'                                         1021
         CALL AXATTRIBUTE                                                 1021
C    *                                                                    1021
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  1021
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  1021
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  1021
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  1021
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  1021
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  1021
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      1021
         CALL AGSRESET                                                    1031
         AG_PHI1 = 0                                                      1031
         AG_DPHI = 360                                                    1031
         AG_NZ = 7                                                        1031
         AG_ZI(1)=SSUP_CONE1ZMN                                           1031
         AG_ZI(2)=SSUP_RODLEN/2                                           1031
         AG_ZI(3)=SSUP_RODLEN/2                                           1031
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1031
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1031
         AG_ZI(6)=SSUP_CONE3ZMX                                           1031
         AG_ZI(7)=SSUP_CONE4ZMX                                           1031
         AG_RMN(1)=SSUP_CON1IDMN                                          1031
         AG_RMN(2)=SSUP_CON1IDMN                                          1031
         AG_RMN(3)=SSUP_CON1IDMN                                          1031
         AG_RMN(4)=SSUP_CON1IDMN                                          1031
         AG_RMN(5)=SSUP_CON3IDMN                                          1031
         AG_RMN(6)=SSUP_CON4IDMN                                          1031
         AG_RMN(7)=SSUP_CON4IDMX                                          1031
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                1031
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                1031
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                1031
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                1031
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                1031
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                1031
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                1031
         AG_TITLE = 'PCON'                                                1031
         AG_EXNAME = 'SHAPE'                                              1031
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1031
         CALL AXSHAPE                                                     1031
C    *                                                                    1031
C    Create SGRA :                                                        1032
C    Call To Local Procedure SGRA                                         1032
         ASSIGN 6210 TO BLOCK_SGRA                                        1032
         GO TO  6200                                                      1032
6210     CONTINUE                                                         1032
C    *                                                                    1032
C    Geant POSITION : SGRA ===> inherited arguments used <===             1033
         CALL AGSRESET                                                    1033
         AG_TITLE = 'SGRA'                                                1033
         AG_EXNAME = 'POSITION'                                           1033
         AG_PARLIST = 'NONE'                                              1033
         CALL AXPOSITION                                                  1033
C    *                                                                    1033
C    Create STAP :                                                        1034
C    Call To Local Procedure STAP                                         1034
         ASSIGN 6230 TO BLOCK_STAP                                        1034
         GO TO  6220                                                      1034
6230     CONTINUE                                                         1034
C    *                                                                    1034
C    Geant POSITION : STAP ===> inherited arguments used <===             1035
         CALL AGSRESET                                                    1035
         AG_TITLE = 'STAP'                                                1035
         AG_EXNAME = 'POSITION'                                           1035
         AG_PARLIST = 'NONE'                                              1035
         CALL AXPOSITION                                                  1035
C    *                                                                    1035
C    Create STAC :                                                        1036
C    Call To Local Procedure STAC                                         1036
         ASSIGN 6250 TO BLOCK_STAC                                        1036
         GO TO  6240                                                      1036
6250     CONTINUE                                                         1036
C    *                                                                    1036
C    Geant POSITION : STAC ===> inherited arguments used <===             1037
         CALL AGSRESET                                                    1037
         AG_TITLE = 'STAC'                                                1037
         AG_EXNAME = 'POSITION'                                           1037
         AG_PARLIST = 'NONE'                                              1037
         CALL AXPOSITION                                                  1037
C    *                                                                    1037
C    Create SHLA :                                                        1038
C    Call To Local Procedure SHLA                                         1038
         ASSIGN 6270 TO BLOCK_SHLA                                        1038
         GO TO  6260                                                      1038
6270     CONTINUE                                                         1038
C    *                                                                    1038
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  1040
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             1040
         CALL AGSRESET                                                    1040
         AG_Z = SSUP_RODLEN/                                              1040
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    1040
         AG_TITLE = 'SHLA'                                                1040
         AG_EXNAME = 'POSITION'                                           1040
         AG_PARLIST = 'Z'                                                 1040
         CALL AXPOSITION                                                  1040
C    *                                                                    1040
C    Create SHLB :                                                        1041
C    Call To Local Procedure SHLB                                         1041
         ASSIGN 6290 TO BLOCK_SHLB                                        1041
         GO TO  6280                                                      1041
6290     CONTINUE                                                         1041
C    *                                                                    1041
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  1042
C    CONE3ZMX) <===                                                       1042
         CALL AGSRESET                                                    1042
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           1042
         AG_TITLE = 'SHLB'                                                1042
         AG_EXNAME = 'POSITION'                                           1042
         AG_PARLIST = 'Z'                                                 1042
         CALL AXPOSITION                                                  1042
C    *                                                                    1042
C    Create SCMY :                                                        1043
C    Call To Local Procedure SCMY                                         1043
         ASSIGN 6310 TO BLOCK_SCMY                                        1043
         GO TO  6300                                                      1043
6310     CONTINUE                                                         1043
C    *                                                                    1043
C    Geant POSITION : SCMY ===> inherited arguments used <===             1044
         CALL AGSRESET                                                    1044
         AG_TITLE = 'SCMY'                                                1044
         AG_EXNAME = 'POSITION'                                           1044
         AG_PARLIST = 'NONE'                                              1044
         CALL AXPOSITION                                                  1044
C    *                                                                    1044
C    Create SCAL :                                                        1045
C    Call To Local Procedure SCAL                                         1045
         ASSIGN 6330 TO BLOCK_SCAL                                        1045
         GO TO  6320                                                      1045
6330     CONTINUE                                                         1045
C    *                                                                    1045
C    Geant POSITION : SCAL ===> inherited arguments used <===             1046
         CALL AGSRESET                                                    1046
         AG_TITLE = 'SCAL'                                                1046
         AG_EXNAME = 'POSITION'                                           1046
         AG_PARLIST = 'NONE'                                              1046
         CALL AXPOSITION                                                  1046
C    *                                                                    1046
5211  CALL AGSPOPB                                                        1047
      GO TO BLOCK_SCON, (5210 ,5220)                                      1047
C End of Local Procedure                                                  1047
C ----------------------------------------------------------------------  1048
C ------------ SGRA                                                       1048
C Begin Local Procedure SGRA                                              1049
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 1049
6200  CONTINUE                                                            1049
         AG_TITLE='SGRA'                                                  1049
         AG_MODULE=MODULE                                                 1049
         AG_MARK='V00'                                                    1049
         CALL AGSPUSH                                                     1049
         AG_MODULE=MODULE                                                 1049
C    *                                                                    1049
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1050
         CALL AGSRESET                                                    1050
         AG_TITLE = 'CARBON'                                              1050
         AG_EXNAME = 'MATERIAL'                                           1050
         AG_PARLIST = 'NONE'                                              1050
         CALL AXMATERIAL                                                  1050
C    *                                                                    1050
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       1050
         CALL AGSRESET                                                    1051
         AG_SEEN = 1                                                      1051
         AG_COLO = 6                                                      1051
         AG_TITLE = 'SGRA'                                                1051
         AG_EXNAME = 'ATTRIBUTE'                                          1051
         AG_PARLIST = 'SEEN_COLO'                                         1051
         CALL AXATTRIBUTE                                                 1051
C    *                                                                    1051
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  1051
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  1051
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  1051
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  1051
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  1051
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              1051
         CALL AGSRESET                                                    1059
         AG_PHI1 = 0                                                      1059
         AG_DPHI = 360                                                    1059
         AG_NZ = 5                                                        1059
         AG_ZI(1)=SSUP_RODLEN/2                                           1059
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1059
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1059
         AG_ZI(4)=SSUP_CONE3ZMX                                           1059
         AG_ZI(5)=SSUP_CONE4ZMX                                           1059
         AG_RMN(1)=SSUP_CON1IDMN                                          1059
         AG_RMN(2)=SSUP_CON1IDMN                                          1059
         AG_RMN(3)=SSUP_CON3IDMN                                          1059
         AG_RMN(4)=SSUP_CON4IDMN                                          1059
         AG_RMN(5)=SSUP_CON4IDMX                                          1059
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1059
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1059
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1059
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             1059
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             1059
         AG_TITLE = 'PCON'                                                1059
         AG_EXNAME = 'SHAPE'                                              1059
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1059
         CALL AXSHAPE                                                     1059
C    *                                                                    1059
6201  CALL AGSPOPB                                                        1060
      GO TO BLOCK_SGRA, (6200 ,6210)                                      1060
C End of Local Procedure                                                  1060
C ----------------------------------------------------------------------  1061
C ------------ STAP                                                       1061
C Begin Local Procedure STAP                                              1062
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  1062
C E)                                                                      1062
6220  CONTINUE                                                            1062
         AG_TITLE='STAP'                                                  1062
         AG_MODULE=MODULE                                                 1062
         AG_MARK='V00'                                                    1062
         CALL AGSPUSH                                                     1062
         AG_MODULE=MODULE                                                 1062
C    *                                                                    1062
         ROFFSET=SSUP_GRPHTHK                                             1063
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1064
         CALL AGSRESET                                                    1064
         AG_A = 12                                                        1064
         AG_Z = 6                                                         1064
         AG_W = 1                                                         1064
         AG_TITLE = 'C'                                                   1064
         AG_EXNAME = 'COMPONENT'                                          1064
         AG_PARLIST = 'A_Z_W'                                             1064
         CALL AXCOMPONENT                                                 1064
C    *                                                                    1064
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1064
         CALL AGSRESET                                                    1065
         AG_A = 1                                                         1065
         AG_Z = 1                                                         1065
         AG_W = 2                                                         1065
         AG_TITLE = 'H2'                                                  1065
         AG_EXNAME = 'COMPONENT'                                          1065
         AG_PARLIST = 'A_Z_W'                                             1065
         CALL AXCOMPONENT                                                 1065
C    *                                                                    1065
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1065
         CALL AGSRESET                                                    1066
         AG_DENS = 0.935                                                  1066
         AG_TITLE = 'CH2'                                                 1066
         AG_EXNAME = 'MIXTURE'                                            1066
         AG_PARLIST = 'DENS'                                              1066
         CALL AXMIXTURE                                                   1066
C    *                                                                    1066
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       1066
         CALL AGSRESET                                                    1067
         AG_SEEN = 1                                                      1067
         AG_COLO = 3                                                      1067
         AG_TITLE = 'STAP'                                                1067
         AG_EXNAME = 'ATTRIBUTE'                                          1067
         AG_PARLIST = 'SEEN_COLO'                                         1067
         CALL AXATTRIBUTE                                                 1067
C    *                                                                    1067
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1067
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1067
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1067
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1067
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1067
         CALL AGSRESET                                                    1073
         AG_PHI1 = 0                                                      1073
         AG_DPHI = 360                                                    1073
         AG_NZ = 3                                                        1073
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1073
         AG_ZI(2)=SSUP_CONE3ZMX                                           1073
         AG_ZI(3)=SSUP_CONE4ZMX                                           1073
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1073
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1073
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1073
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1073
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1073
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1073
         AG_TITLE = 'PCON'                                                1073
         AG_EXNAME = 'SHAPE'                                              1073
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1073
         CALL AXSHAPE                                                     1073
C    *                                                                    1073
6221  CALL AGSPOPB                                                        1074
      GO TO BLOCK_STAP, (6220 ,6230)                                      1074
C End of Local Procedure                                                  1074
C ----------------------------------------------------------------------  1075
C ------------ STAC                                                       1075
C Begin Local Procedure STAC                                              1076
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      1076
6240  CONTINUE                                                            1076
         AG_TITLE='STAC'                                                  1076
         AG_MODULE=MODULE                                                 1076
         AG_MARK='V00'                                                    1076
         CALL AGSPUSH                                                     1076
         AG_MODULE=MODULE                                                 1076
C    *                                                                    1076
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             1077
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           1078
         CALL AGSRESET                                                    1078
         AG_TITLE = 'COPPER'                                              1078
         AG_EXNAME = 'MATERIAL'                                           1078
         AG_PARLIST = 'NONE'                                              1078
         CALL AXMATERIAL                                                  1078
C    *                                                                    1078
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       1078
         CALL AGSRESET                                                    1079
         AG_SEEN = 1                                                      1079
         AG_COLO = 2                                                      1079
         AG_TITLE = 'STAC'                                                1079
         AG_EXNAME = 'ATTRIBUTE'                                          1079
         AG_PARLIST = 'SEEN_COLO'                                         1079
         CALL AXATTRIBUTE                                                 1079
C    *                                                                    1079
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1079
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1079
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1079
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1079
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1079
         CALL AGSRESET                                                    1085
         AG_PHI1 = 0                                                      1085
         AG_DPHI = 360                                                    1085
         AG_NZ = 3                                                        1085
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1085
         AG_ZI(2)=SSUP_CONE3ZMX                                           1085
         AG_ZI(3)=SSUP_CONE4ZMX                                           1085
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1085
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1085
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1085
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1085
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1085
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1085
         AG_TITLE = 'PCON'                                                1085
         AG_EXNAME = 'SHAPE'                                              1085
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1085
         CALL AXSHAPE                                                     1085
C    *                                                                    1085
6241  CALL AGSPOPB                                                        1086
      GO TO BLOCK_STAC, (6240 ,6250)                                      1086
C End of Local Procedure                                                  1086
C ----------------------------------------------------------------------  1087
C ------------ SHLA                                                       1087
C Begin Local Procedure SHLA                                              1088
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              1088
6260  CONTINUE                                                            1088
         AG_TITLE='SHLA'                                                  1088
         AG_MODULE=MODULE                                                 1088
         AG_MARK='V00'                                                    1088
         CALL AGSPUSH                                                     1088
         AG_MODULE=MODULE                                                 1088
C    *                                                                    1088
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1089
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1090
         CALL AGSRESET                                                    1090
         AG_TITLE = 'AIR'                                                 1090
         AG_EXNAME = 'MATERIAL'                                           1090
         AG_PARLIST = 'NONE'                                              1090
         CALL AXMATERIAL                                                  1090
C    *                                                                    1090
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       1090
         CALL AGSRESET                                                    1091
         AG_SEEN = 0                                                      1091
         AG_COLO = 1                                                      1091
         AG_TITLE = 'SHLA'                                                1091
         AG_EXNAME = 'ATTRIBUTE'                                          1091
         AG_PARLIST = 'SEEN_COLO'                                         1091
         CALL AXATTRIBUTE                                                 1091
C    *                                                                    1091
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  1091
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  1091
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  1091
C    .0*SSUP_HOSRMX <===                                                  1091
         CALL AGSRESET                                                    1095
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            1095
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  1095
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1095
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  1095
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1095
         AG_TITLE = 'CONE'                                                1095
         AG_EXNAME = 'SHAPE'                                              1095
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1095
         CALL AXSHAPE                                                     1095
C    *                                                                    1095
C    Create SHMA :                                                        1096
C    Call To Local Procedure SHMA                                         1096
         ASSIGN 6350 TO BLOCK_SHMA                                        1096
         GO TO  6340                                                      1096
6350     CONTINUE                                                         1096
C    *                                                                    1096
6261  CALL AGSPOPB                                                        1097
      GO TO BLOCK_SHLA, (6260 ,6270)                                      1097
C End of Local Procedure                                                  1097
C ----------------------------------------------------------------------  1098
C ------------ SHMA                                                       1098
C Begin Local Procedure SHMA                                              1099
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           1099
6340  CONTINUE                                                            1099
         AG_TITLE='SHMA'                                                  1099
         AG_MODULE=MODULE                                                 1099
         AG_MARK='V00'                                                    1099
         CALL AGSPUSH                                                     1099
         AG_MODULE=MODULE                                                 1099
C    *                                                                    1099
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1100
         RMIN=SSUP_CON3IDMN + ROFFSET                                     1101
         RMAX=SSUP_CON4IDMN + ROFFSET                                     1102
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                1103
         ZMAX=SSUP_CONE3ZMX                                               1104
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1105
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1106
         CONE_LEN = SQRT(CONE_LEN)                                        1107
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1108
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1109
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1110
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1111
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1112
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       1113
         CALL AGSRESET                                                    1113
         AG_SEEN = 0                                                      1113
         AG_COLO = 1                                                      1113
         AG_TITLE = 'SHMA'                                                1113
         AG_EXNAME = 'ATTRIBUTE'                                          1113
         AG_PARLIST = 'SEEN_COLO'                                         1113
         CALL AXATTRIBUTE                                                 1113
C    *                                                                    1113
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1113
         CALL AGSRESET                                                    1115
         AG_NDIV = SSUP_NHOSES                                            1115
         AG_IAXIS = 2                                                     1115
         AG_C0 = 0.0                                                      1115
         AG_TITLE = 'DIVISION'                                            1115
         AG_EXNAME = 'SHAPE'                                              1115
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1115
         CALL AXSHAPE                                                     1115
C    *                                                                    1115
C    Create SWHO :                                                        1116
C    Call To Local Procedure SWHO                                         1116
         ASSIGN 6370 TO BLOCK_SWHO                                        1116
         GO TO  6360                                                      1116
6370     CONTINUE                                                         1116
C    *                                                                    1116
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1117
         CALL AGSRESET                                                    1117
         AG_X = XPOS                                                      1117
         AG_Y = 0                                                         1117
         AG_Z = ZPOS                                                      1117
         AG_ALPHAN=AG_ALPHAN+1                                            1117
         AG_ALPHAK(AG_ALPHAN)=2                                           1117
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1117
         AG_TITLE = 'SWHO'                                                1117
         AG_EXNAME = 'POSITION'                                           1117
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1117
         CALL AXPOSITION                                                  1117
C    *                                                                    1117
6341  CALL AGSPOPB                                                        1118
      GO TO BLOCK_SHMA, (6340 ,6350)                                      1118
C End of Local Procedure                                                  1118
C ----------------------------------------------------------------------  1119
C ------------ SHLB                                                       1119
C Begin Local Procedure SHLB                                              1120
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               1120
6280  CONTINUE                                                            1120
         AG_TITLE='SHLB'                                                  1120
         AG_MODULE=MODULE                                                 1120
         AG_MARK='V00'                                                    1120
         CALL AGSPUSH                                                     1120
         AG_MODULE=MODULE                                                 1120
C    *                                                                    1120
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1121
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1122
         CALL AGSRESET                                                    1122
         AG_TITLE = 'AIR'                                                 1122
         AG_EXNAME = 'MATERIAL'                                           1122
         AG_PARLIST = 'NONE'                                              1122
         CALL AXMATERIAL                                                  1122
C    *                                                                    1122
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       1122
         CALL AGSRESET                                                    1123
         AG_SEEN = 0                                                      1123
         AG_COLO = 1                                                      1123
         AG_TITLE = 'SHLB'                                                1123
         AG_EXNAME = 'ATTRIBUTE'                                          1123
         AG_PARLIST = 'SEEN_COLO'                                         1123
         CALL AXATTRIBUTE                                                 1123
C    *                                                                    1123
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  1123
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  1123
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  1123
C    X <===                                                               1123
         CALL AGSRESET                                                    1127
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         1127
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  1127
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1127
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  1127
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  1127
         AG_TITLE = 'CONE'                                                1127
         AG_EXNAME = 'SHAPE'                                              1127
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1127
         CALL AXSHAPE                                                     1127
C    *                                                                    1127
C    Create SHMB :                                                        1128
C    Call To Local Procedure SHMB                                         1128
         ASSIGN 6390 TO BLOCK_SHMB                                        1128
         GO TO  6380                                                      1128
6390     CONTINUE                                                         1128
C    *                                                                    1128
6281  CALL AGSPOPB                                                        1129
      GO TO BLOCK_SHLB, (6280 ,6290)                                      1129
C End of Local Procedure                                                  1129
C ----------------------------------------------------------------------  1130
C ------------ SHMB                                                       1130
C Begin Local Procedure SHMB                                              1131
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           1131
6380  CONTINUE                                                            1131
         AG_TITLE='SHMB'                                                  1131
         AG_MODULE=MODULE                                                 1131
         AG_MARK='V00'                                                    1131
         CALL AGSPUSH                                                     1131
         AG_MODULE=MODULE                                                 1131
C    *                                                                    1131
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1132
         RMIN=SSUP_CON4IDMN + ROFFSET                                     1133
         RMAX=SSUP_CON4IDMX + ROFFSET                                     1134
         ZMIN=SSUP_CONE3ZMX                                               1135
         ZMAX=SSUP_CONE4ZMX                                               1136
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1137
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1138
         CONE_LEN = SQRT(CONE_LEN)                                        1139
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1140
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1141
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1142
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1143
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1144
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       1145
         CALL AGSRESET                                                    1145
         AG_SEEN = 0                                                      1145
         AG_COLO = 1                                                      1145
         AG_TITLE = 'SHMB'                                                1145
         AG_EXNAME = 'ATTRIBUTE'                                          1145
         AG_PARLIST = 'SEEN_COLO'                                         1145
         CALL AXATTRIBUTE                                                 1145
C    *                                                                    1145
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1145
         CALL AGSRESET                                                    1147
         AG_NDIV = SSUP_NHOSES                                            1147
         AG_IAXIS = 2                                                     1147
         AG_C0 = 0.0                                                      1147
         AG_TITLE = 'DIVISION'                                            1147
         AG_EXNAME = 'SHAPE'                                              1147
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1147
         CALL AXSHAPE                                                     1147
C    *                                                                    1147
C    Create SWHO :                                                        1148
C    Call To Local Procedure SWHO                                         1148
         ASSIGN 6400 TO BLOCK_SWHO                                        1148
         GO TO  6360                                                      1148
6400     CONTINUE                                                         1148
C    *                                                                    1148
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1149
         CALL AGSRESET                                                    1149
         AG_X = XPOS                                                      1149
         AG_Y = 0                                                         1149
         AG_Z = ZPOS                                                      1149
         AG_ALPHAN=AG_ALPHAN+1                                            1149
         AG_ALPHAK(AG_ALPHAN)=2                                           1149
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1149
         AG_TITLE = 'SWHO'                                                1149
         AG_EXNAME = 'POSITION'                                           1149
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1149
         CALL AXPOSITION                                                  1149
C    *                                                                    1149
6381  CALL AGSPOPB                                                        1150
      GO TO BLOCK_SHMB, (6380 ,6390)                                      1150
C End of Local Procedure                                                  1150
C ----------------------------------------------------------------------  1151
C ------------ SWHO                                                       1151
C Begin Local Procedure SWHO                                              1152
C SWHO IS A WATER HOSE                                                    1152
6360  CONTINUE                                                            1152
         AG_TITLE='SWHO'                                                  1152
         AG_MODULE=MODULE                                                 1152
         AG_MARK='V00'                                                    1152
         CALL AGSPUSH                                                     1152
         AG_MODULE=MODULE                                                 1152
C    *                                                                    1152
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1153
         CALL AGSRESET                                                    1153
         AG_SEEN = 1                                                      1153
         AG_COLO = 3                                                      1153
         AG_TITLE = 'SWHO'                                                1153
         AG_EXNAME = 'ATTRIBUTE'                                          1153
         AG_PARLIST = 'SEEN_COLO'                                         1153
         CALL AXATTRIBUTE                                                 1153
C    *                                                                    1153
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1153
         CALL AGSRESET                                                    1154
         AG_A = 12                                                        1154
         AG_Z = 6                                                         1154
         AG_W = 1                                                         1154
         AG_TITLE = 'C'                                                   1154
         AG_EXNAME = 'COMPONENT'                                          1154
         AG_PARLIST = 'A_Z_W'                                             1154
         CALL AXCOMPONENT                                                 1154
C    *                                                                    1154
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1154
         CALL AGSRESET                                                    1155
         AG_A = 1                                                         1155
         AG_Z = 1                                                         1155
         AG_W = 2                                                         1155
         AG_TITLE = 'H2'                                                  1155
         AG_EXNAME = 'COMPONENT'                                          1155
         AG_PARLIST = 'A_Z_W'                                             1155
         CALL AXCOMPONENT                                                 1155
C    *                                                                    1155
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1155
         CALL AGSRESET                                                    1156
         AG_DENS = 0.935                                                  1156
         AG_TITLE = 'CH2'                                                 1156
         AG_EXNAME = 'MIXTURE'                                            1156
         AG_PARLIST = 'DENS'                                              1156
         CALL AXMIXTURE                                                   1156
C    *                                                                    1156
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1156
         CALL AGSRESET                                                    1157
         AG_SEEN = 1                                                      1157
         AG_COLO = 3                                                      1157
         AG_TITLE = 'SWHO'                                                1157
         AG_EXNAME = 'ATTRIBUTE'                                          1157
         AG_PARLIST = 'SEEN_COLO'                                         1157
         CALL AXATTRIBUTE                                                 1157
C    *                                                                    1157
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  1157
C    =                                                                    1157
         CALL AGSRESET                                                    1158
         AG_RMIN = 0                                                      1158
         AG_RMAX = SSUP_HOSRMX                                            1158
         AG_DZ = 0.5*CONE_LEN                                             1158
         AG_TITLE = 'TUBE'                                                1158
         AG_EXNAME = 'SHAPE'                                              1158
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1158
         CALL AXSHAPE                                                     1158
C    *                                                                    1158
C    Create SHWA :                                                        1159
C    Call To Local Procedure SHWA                                         1159
         ASSIGN 6420 TO BLOCK_SHWA                                        1159
         GO TO  6410                                                      1159
6420     CONTINUE                                                         1159
C    *                                                                    1159
C    Geant POSITION : SHWA ===> inherited arguments used <===             1160
         CALL AGSRESET                                                    1160
         AG_TITLE = 'SHWA'                                                1160
         AG_EXNAME = 'POSITION'                                           1160
         AG_PARLIST = 'NONE'                                              1160
         CALL AXPOSITION                                                  1160
C    *                                                                    1160
6361  CALL AGSPOPB                                                        1161
      GO TO BLOCK_SWHO, (6360 ,6370,6400)                                 1161
C End of Local Procedure                                                  1161
C ----------------------------------------------------------------------  1162
C ------------ SHWA                                                       1162
C Begin Local Procedure SHWA                                              1163
C SHWA IS THE WATER IN THE HOSE                                           1163
6410  CONTINUE                                                            1163
         AG_TITLE='SHWA'                                                  1163
         AG_MODULE=MODULE                                                 1163
         AG_MARK='V00'                                                    1163
         CALL AGSPUSH                                                     1163
         AG_MODULE=MODULE                                                 1163
C    *                                                                    1163
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       1164
         CALL AGSRESET                                                    1164
         AG_SEEN = 1                                                      1164
         AG_COLO = 6                                                      1164
         AG_TITLE = 'SHWA'                                                1164
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
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        1165
         CALL AGSRESET                                                    1166
         AG_RMAX = SSUP_HOSRMN                                            1166
         AG_TITLE = 'TUBE'                                                1166
         AG_EXNAME = 'SHAPE'                                              1166
         AG_PARLIST = 'RMAX'                                              1166
         CALL AXSHAPE                                                     1166
C    *                                                                    1166
6411  CALL AGSPOPB                                                        1167
      GO TO BLOCK_SHWA, (6410 ,6420)                                      1167
C End of Local Procedure                                                  1167
C ----------------------------------------------------------------------  1168
C ------------ SCMY                                                       1168
C Begin Local Procedure SCMY                                              1169
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            1169
6300  CONTINUE                                                            1169
         AG_TITLE='SCMY'                                                  1169
         AG_MODULE=MODULE                                                 1169
         AG_MARK='V00'                                                    1169
         CALL AGSPUSH                                                     1169
         AG_MODULE=MODULE                                                 1169
C    *                                                                    1169
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             1170
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          1171
         CALL AGSRESET                                                    1171
         AG_A = 12                                                        1171
         AG_Z = 6                                                         1171
         AG_W = 5                                                         1171
         AG_TITLE = 'C5'                                                  1171
         AG_EXNAME = 'COMPONENT'                                          1171
         AG_PARLIST = 'A_Z_W'                                             1171
         CALL AXCOMPONENT                                                 1171
C    *                                                                    1171
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           1171
         CALL AGSRESET                                                    1172
         AG_A = 1                                                         1172
         AG_Z = 1                                                         1172
         AG_W = 4                                                         1172
         AG_TITLE = 'H4'                                                  1172
         AG_EXNAME = 'COMPONENT'                                          1172
         AG_PARLIST = 'A_Z_W'                                             1172
         CALL AXCOMPONENT                                                 1172
C    *                                                                    1172
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          1172
         CALL AGSRESET                                                    1173
         AG_A = 16                                                        1173
         AG_Z = 8                                                         1173
         AG_W = 2                                                         1173
         AG_TITLE = 'O2'                                                  1173
         AG_EXNAME = 'COMPONENT'                                          1173
         AG_PARLIST = 'A_Z_W'                                             1173
         CALL AXCOMPONENT                                                 1173
C    *                                                                    1173
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            1173
         CALL AGSRESET                                                    1174
         AG_DENS = 1.39                                                   1174
         AG_TITLE = 'MYLAR'                                               1174
         AG_EXNAME = 'MIXTURE'                                            1174
         AG_PARLIST = 'DENS'                                              1174
         CALL AXMIXTURE                                                   1174
C    *                                                                    1174
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       1174
         CALL AGSRESET                                                    1175
         AG_SEEN = 1                                                      1175
         AG_COLO = 3                                                      1175
         AG_TITLE = 'SCMY'                                                1175
         AG_EXNAME = 'ATTRIBUTE'                                          1175
         AG_PARLIST = 'SEEN_COLO'                                         1175
         CALL AXATTRIBUTE                                                 1175
C    *                                                                    1175
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1175
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1175
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1175
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  1175
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     1175
         CALL AGSRESET                                                    1181
         AG_PHI1 = 0                                                      1181
         AG_DPHI = 360                                                    1181
         AG_NZ = 3                                                        1181
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1181
         AG_ZI(2)=SSUP_CONE3ZMX                                           1181
         AG_ZI(3)=SSUP_CONE4ZMX                                           1181
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1181
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1181
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1181
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    1181
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    1181
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    1181
         AG_TITLE = 'PCON'                                                1181
         AG_EXNAME = 'SHAPE'                                              1181
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1181
         CALL AXSHAPE                                                     1181
C    *                                                                    1181
6301  CALL AGSPOPB                                                        1182
      GO TO BLOCK_SCMY, (6300 ,6310)                                      1182
C End of Local Procedure                                                  1182
C ----------------------------------------------------------------------  1183
C ------------ SCAL                                                       1183
C Begin Local Procedure SCAL                                              1184
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     1184
6320  CONTINUE                                                            1184
         AG_TITLE='SCAL'                                                  1184
         AG_MODULE=MODULE                                                 1184
         AG_MARK='V00'                                                    1184
         CALL AGSPUSH                                                     1184
         AG_MODULE=MODULE                                                 1184
C    *                                                                    1184
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           1185
     *   SSUP_WRPMYTHK                                                    1185
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1186
         CALL AGSRESET                                                    1186
         AG_TITLE = 'ALUMINIUM'                                           1186
         AG_EXNAME = 'MATERIAL'                                           1186
         AG_PARLIST = 'NONE'                                              1186
         CALL AXMATERIAL                                                  1186
C    *                                                                    1186
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       1186
         CALL AGSRESET                                                    1187
         AG_SEEN = 1                                                      1187
         AG_COLO = 2                                                      1187
         AG_TITLE = 'SCAL'                                                1187
         AG_EXNAME = 'ATTRIBUTE'                                          1187
         AG_PARLIST = 'SEEN_COLO'                                         1187
         CALL AXATTRIBUTE                                                 1187
C    *                                                                    1187
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1187
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1187
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1187
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  1187
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     1187
         CALL AGSRESET                                                    1193
         AG_PHI1 = 0                                                      1193
         AG_DPHI = 360                                                    1193
         AG_NZ = 3                                                        1193
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1193
         AG_ZI(2)=SSUP_CONE3ZMX                                           1193
         AG_ZI(3)=SSUP_CONE4ZMX                                           1193
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1193
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1193
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1193
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    1193
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    1193
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    1193
         AG_TITLE = 'PCON'                                                1193
         AG_EXNAME = 'SHAPE'                                              1193
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1193
         CALL AXSHAPE                                                     1193
C    *                                                                    1193
6321  CALL AGSPOPB                                                        1194
      GO TO BLOCK_SCAL, (6320 ,6330)                                      1194
C End of Local Procedure                                                  1194
C ----------------------------------------------------------------------  1195
C ------------ SWMM                                                       1195
C Begin Local Procedure SWMM                                              1196
C SWMM IS THE WATER MANIFOLD MOTHER                                       1196
5170  CONTINUE                                                            1196
         AG_TITLE='SWMM'                                                  1196
         AG_MODULE=MODULE                                                 1196
         AG_MARK='V00'                                                    1196
         CALL AGSPUSH                                                     1196
         AG_MODULE=MODULE                                                 1196
C    *                                                                    1196
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     1197
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     1198
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1199
         CALL AGSRESET                                                    1199
         AG_TITLE = 'AIR'                                                 1199
         AG_EXNAME = 'MATERIAL'                                           1199
         AG_PARLIST = 'NONE'                                              1199
         CALL AXMATERIAL                                                  1199
C    *                                                                    1199
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       1199
         CALL AGSRESET                                                    1200
         AG_SEEN = 0                                                      1200
         AG_COLO = 1                                                      1200
         AG_TITLE = 'SWMM'                                                1200
         AG_EXNAME = 'ATTRIBUTE'                                          1200
         AG_PARLIST = 'SEEN_COLO'                                         1200
         CALL AXATTRIBUTE                                                 1200
C    *                                                                    1200
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1200
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   1200
         CALL AGSRESET                                                    1203
         AG_PHI1 = 0                                                      1203
         AG_DPHI = 360                                                    1203
         AG_NPDIV = 18                                                    1203
         AG_NZ = 2                                                        1203
         AG_ZI(1)=-SWAM_LEN/2                                             1203
         AG_ZI(2)=+SWAM_LEN/2                                             1203
         AG_RMN(1)=RIN                                                    1203
         AG_RMN(2)=RIN                                                    1203
         AG_RMX(1)=ROU                                                    1203
         AG_RMX(2)=ROU                                                    1203
         AG_TITLE = 'PGON'                                                1203
         AG_EXNAME = 'SHAPE'                                              1203
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1203
         CALL AXSHAPE                                                     1203
C    *                                                                    1203
C    Create SWMB :                                                        1204
C    Call To Local Procedure SWMB                                         1204
         ASSIGN 6440 TO BLOCK_SWMB                                        1204
         GO TO  6430                                                      1204
6440     CONTINUE                                                         1204
C    *                                                                    1204
C    Geant POSITION : SWMB ===> inherited arguments used <===             1205
         CALL AGSRESET                                                    1205
         AG_TITLE = 'SWMB'                                                1205
         AG_EXNAME = 'POSITION'                                           1205
         AG_PARLIST = 'NONE'                                              1205
         CALL AXPOSITION                                                  1205
C    *                                                                    1205
C    Create SWMT :                                                        1206
C    Call To Local Procedure SWMT                                         1206
         ASSIGN 6460 TO BLOCK_SWMT                                        1206
         GO TO  6450                                                      1206
6460     CONTINUE                                                         1206
C    *                                                                    1206
C    Geant POSITION : SWMT ===> inherited arguments used <===             1207
         CALL AGSRESET                                                    1207
         AG_TITLE = 'SWMT'                                                1207
         AG_EXNAME = 'POSITION'                                           1207
         AG_PARLIST = 'NONE'                                              1207
         CALL AXPOSITION                                                  1207
C    *                                                                    1207
C    Create SWMS :                                                        1208
C    Call To Local Procedure SWMS                                         1208
         ASSIGN 6480 TO BLOCK_SWMS                                        1208
         GO TO  6470                                                      1208
6480     CONTINUE                                                         1208
C    *                                                                    1208
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         1209
         CALL AGSRESET                                                    1209
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                1209
         AG_TITLE = 'SWMS'                                                1209
         AG_EXNAME = 'POSITION'                                           1209
         AG_PARLIST = 'Z'                                                 1209
         CALL AXPOSITION                                                  1209
C    *                                                                    1209
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          1209
         CALL AGSRESET                                                    1210
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 1210
         AG_TITLE = 'SWMS'                                                1210
         AG_EXNAME = 'POSITION'                                           1210
         AG_PARLIST = 'Z'                                                 1210
         CALL AXPOSITION                                                  1210
C    *                                                                    1210
C    Create SWMW :                                                        1211
C    Call To Local Procedure SWMW                                         1211
         ASSIGN 6500 TO BLOCK_SWMW                                        1211
         GO TO  6490                                                      1211
6500     CONTINUE                                                         1211
C    *                                                                    1211
C    Geant POSITION : SWMW ===> inherited arguments used <===             1212
         CALL AGSRESET                                                    1212
         AG_TITLE = 'SWMW'                                                1212
         AG_EXNAME = 'POSITION'                                           1212
         AG_PARLIST = 'NONE'                                              1212
         CALL AXPOSITION                                                  1212
C    *                                                                    1212
C    Create SOTB :                                                        1213
C    Call To Local Procedure SOTB                                         1213
         ASSIGN 6520 TO BLOCK_SOTB                                        1213
         GO TO  6510                                                      1213
6520     CONTINUE                                                         1213
C    *                                                                    1213
C    Geant POSITION : SOTB ===> inherited arguments used <===             1214
         CALL AGSRESET                                                    1214
         AG_TITLE = 'SOTB'                                                1214
         AG_EXNAME = 'POSITION'                                           1214
         AG_PARLIST = 'NONE'                                              1214
         CALL AXPOSITION                                                  1214
C    *                                                                    1214
C    Create SITB :                                                        1215
C    Call To Local Procedure SITB                                         1215
         ASSIGN 6540 TO BLOCK_SITB                                        1215
         GO TO  6530                                                      1215
6540     CONTINUE                                                         1215
C    *                                                                    1215
C    Geant POSITION : SITB ===> inherited arguments used <===             1216
         CALL AGSRESET                                                    1216
         AG_TITLE = 'SITB'                                                1216
         AG_EXNAME = 'POSITION'                                           1216
         AG_PARLIST = 'NONE'                                              1216
         CALL AXPOSITION                                                  1216
C    *                                                                    1216
5171  CALL AGSPOPB                                                        1217
      GO TO BLOCK_SWMM, (5170 ,5180)                                      1217
C End of Local Procedure                                                  1217
C ----------------------------------------------------------------------  1218
C ------------ SWMB                                                       1218
C Begin Local Procedure SWMB                                              1219
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       1219
6430  CONTINUE                                                            1219
         AG_TITLE='SWMB'                                                  1219
         AG_MODULE=MODULE                                                 1219
         AG_MARK='V00'                                                    1219
         CALL AGSPUSH                                                     1219
         AG_MODULE=MODULE                                                 1219
C    *                                                                    1219
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1220
         CALL AGSRESET                                                    1220
         AG_TITLE = 'ALUMINIUM'                                           1220
         AG_EXNAME = 'MATERIAL'                                           1220
         AG_PARLIST = 'NONE'                                              1220
         CALL AXMATERIAL                                                  1220
C    *                                                                    1220
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       1220
         CALL AGSRESET                                                    1221
         AG_SEEN = 1                                                      1221
         AG_COLO = 2                                                      1221
         AG_TITLE = 'SWMB'                                                1221
         AG_EXNAME = 'ATTRIBUTE'                                          1221
         AG_PARLIST = 'SEEN_COLO'                                         1221
         CALL AXATTRIBUTE                                                 1221
C    *                                                                    1221
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1221
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  1221
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          1221
         CALL AGSRESET                                                    1225
         AG_PHI1 = 0                                                      1225
         AG_DPHI = 360                                                    1225
         AG_NPDIV = 18                                                    1225
         AG_NZ = 2                                                        1225
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1225
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1225
         AG_RMN(1)=SWAM_RMIN                                              1225
         AG_RMN(2)=SWAM_RMIN                                              1225
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 1225
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 1225
         AG_TITLE = 'PGON'                                                1225
         AG_EXNAME = 'SHAPE'                                              1225
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1225
         CALL AXSHAPE                                                     1225
C    *                                                                    1225
6431  CALL AGSPOPB                                                        1226
      GO TO BLOCK_SWMB, (6430 ,6440)                                      1226
C End of Local Procedure                                                  1226
C ----------------------------------------------------------------------  1227
C ------------ SWMT                                                       1227
C Begin Local Procedure SWMT                                              1228
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            1228
6450  CONTINUE                                                            1228
         AG_TITLE='SWMT'                                                  1228
         AG_MODULE=MODULE                                                 1228
         AG_MARK='V00'                                                    1228
         CALL AGSPUSH                                                     1228
         AG_MODULE=MODULE                                                 1228
C    *                                                                    1228
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1229
         CALL AGSRESET                                                    1229
         AG_TITLE = 'ALUMINIUM'                                           1229
         AG_EXNAME = 'MATERIAL'                                           1229
         AG_PARLIST = 'NONE'                                              1229
         CALL AXMATERIAL                                                  1229
C    *                                                                    1229
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       1229
         CALL AGSRESET                                                    1230
         AG_SEEN = 1                                                      1230
         AG_COLO = 2                                                      1230
         AG_TITLE = 'SWMT'                                                1230
         AG_EXNAME = 'ATTRIBUTE'                                          1230
         AG_PARLIST = 'SEEN_COLO'                                         1230
         CALL AXATTRIBUTE                                                 1230
C    *                                                                    1230
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1230
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  1230
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          1230
         CALL AGSRESET                                                    1234
         AG_PHI1 = 0                                                      1234
         AG_DPHI = 360                                                    1234
         AG_NPDIV = 18                                                    1234
         AG_NZ = 2                                                        1234
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1234
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1234
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 1234
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 1234
         AG_RMX(1)=SWAM_RMAX                                              1234
         AG_RMX(2)=SWAM_RMAX                                              1234
         AG_TITLE = 'PGON'                                                1234
         AG_EXNAME = 'SHAPE'                                              1234
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1234
         CALL AXSHAPE                                                     1234
C    *                                                                    1234
6451  CALL AGSPOPB                                                        1235
      GO TO BLOCK_SWMT, (6450 ,6460)                                      1235
C End of Local Procedure                                                  1235
C ----------------------------------------------------------------------  1236
C ------------ SWMS                                                       1236
C Begin Local Procedure SWMS                                              1237
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  1237
6470  CONTINUE                                                            1237
         AG_TITLE='SWMS'                                                  1237
         AG_MODULE=MODULE                                                 1237
         AG_MARK='V00'                                                    1237
         CALL AGSPUSH                                                     1237
         AG_MODULE=MODULE                                                 1237
C    *                                                                    1237
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1238
         CALL AGSRESET                                                    1238
         AG_TITLE = 'ALUMINIUM'                                           1238
         AG_EXNAME = 'MATERIAL'                                           1238
         AG_PARLIST = 'NONE'                                              1238
         CALL AXMATERIAL                                                  1238
C    *                                                                    1238
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       1238
         CALL AGSRESET                                                    1239
         AG_SEEN = 1                                                      1239
         AG_COLO = 2                                                      1239
         AG_TITLE = 'SWMS'                                                1239
         AG_EXNAME = 'ATTRIBUTE'                                          1239
         AG_PARLIST = 'SEEN_COLO'                                         1239
         CALL AXATTRIBUTE                                                 1239
C    *                                                                    1239
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  1239
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  1239
C    WAM_RMAX} <===                                                       1239
         CALL AGSRESET                                                    1242
         AG_PHI1 = 0                                                      1242
         AG_DPHI = 360                                                    1242
         AG_NPDIV = 18                                                    1242
         AG_NZ = 2                                                        1242
         AG_ZI(1)=-SWAM_WALLTHK/2                                         1242
         AG_ZI(2)=SWAM_WALLTHK/2                                          1242
         AG_RMN(1)=SWAM_RMIN                                              1242
         AG_RMN(2)=SWAM_RMIN                                              1242
         AG_RMX(1)=SWAM_RMAX                                              1242
         AG_RMX(2)=SWAM_RMAX                                              1242
         AG_TITLE = 'PGON'                                                1242
         AG_EXNAME = 'SHAPE'                                              1242
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1242
         CALL AXSHAPE                                                     1242
C    *                                                                    1242
6471  CALL AGSPOPB                                                        1243
      GO TO BLOCK_SWMS, (6470 ,6480)                                      1243
C End of Local Procedure                                                  1243
C ----------------------------------------------------------------------  1244
C ------------ SWMW                                                       1244
C Begin Local Procedure SWMW                                              1245
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 1245
6490  CONTINUE                                                            1245
         AG_TITLE='SWMW'                                                  1245
         AG_MODULE=MODULE                                                 1245
         AG_MARK='V00'                                                    1245
         CALL AGSPUSH                                                     1245
         AG_MODULE=MODULE                                                 1245
C    *                                                                    1245
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       1246
         CALL AGSRESET                                                    1246
         AG_SEEN = 1                                                      1246
         AG_COLO = 6                                                      1246
         AG_TITLE = 'SWMW'                                                1246
         AG_EXNAME = 'ATTRIBUTE'                                          1246
         AG_PARLIST = 'SEEN_COLO'                                         1246
         CALL AXATTRIBUTE                                                 1246
C    *                                                                    1246
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1246
         CALL AGSRESET                                                    1247
         AG_TITLE = 'WATER'                                               1247
         AG_EXNAME = 'MATERIAL'                                           1247
         AG_PARLIST = 'NONE'                                              1247
         CALL AXMATERIAL                                                  1247
C    *                                                                    1247
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1247
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  1247
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  1247
C    -SWAM_WALLTHK} <===                                                  1247
         CALL AGSRESET                                                    1252
         AG_PHI1 = 0                                                      1252
         AG_DPHI = 360                                                    1252
         AG_NPDIV = 18                                                    1252
         AG_NZ = 2                                                        1252
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1252
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1252
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 1252
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 1252
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 1252
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 1252
         AG_TITLE = 'PGON'                                                1252
         AG_EXNAME = 'SHAPE'                                              1252
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1252
         CALL AXSHAPE                                                     1252
C    *                                                                    1252
6491  CALL AGSPOPB                                                        1253
      GO TO BLOCK_SWMW, (6490 ,6500)                                      1253
C End of Local Procedure                                                  1253
C ----------------------------------------------------------------------  1254
C ------------ SOTB                                                       1254
C Begin Local Procedure SOTB                                              1255
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            1255
6510  CONTINUE                                                            1255
         AG_TITLE='SOTB'                                                  1255
         AG_MODULE=MODULE                                                 1255
         AG_MARK='V00'                                                    1255
         CALL AGSPUSH                                                     1255
         AG_MODULE=MODULE                                                 1255
C    *                                                                    1255
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1256
         CALL AGSRESET                                                    1256
         AG_TITLE = 'G10'                                                 1256
         AG_EXNAME = 'MATERIAL'                                           1256
         AG_PARLIST = 'NONE'                                              1256
         CALL AXMATERIAL                                                  1256
C    *                                                                    1256
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       1256
         CALL AGSRESET                                                    1257
         AG_SEEN = 1                                                      1257
         AG_COLO = 3                                                      1257
         AG_TITLE = 'SOTB'                                                1257
         AG_EXNAME = 'ATTRIBUTE'                                          1257
         AG_PARLIST = 'SEEN_COLO'                                         1257
         CALL AXATTRIBUTE                                                 1257
C    *                                                                    1257
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1257
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  1257
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    1257
         CALL AGSRESET                                                    1260
         AG_PHI1 = 0                                                      1260
         AG_DPHI = 360                                                    1260
         AG_NPDIV = 18                                                    1260
         AG_NZ = 2                                                        1260
         AG_ZI(1)=-SWAM_LEN/2                                             1260
         AG_ZI(2)=+SWAM_LEN/2                                             1260
         AG_RMN(1)=SWAM_RMAX                                              1260
         AG_RMN(2)=SWAM_RMAX                                              1260
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 1260
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 1260
         AG_TITLE = 'PGON'                                                1260
         AG_EXNAME = 'SHAPE'                                              1260
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1260
         CALL AXSHAPE                                                     1260
C    *                                                                    1260
6511  CALL AGSPOPB                                                        1261
      GO TO BLOCK_SOTB, (6510 ,6520)                                      1261
C End of Local Procedure                                                  1261
C ----------------------------------------------------------------------  1262
C ------------ SITB                                                       1262
C Begin Local Procedure SITB                                              1263
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            1263
6530  CONTINUE                                                            1263
         AG_TITLE='SITB'                                                  1263
         AG_MODULE=MODULE                                                 1263
         AG_MARK='V00'                                                    1263
         CALL AGSPUSH                                                     1263
         AG_MODULE=MODULE                                                 1263
C    *                                                                    1263
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1264
         CALL AGSRESET                                                    1264
         AG_A = 12                                                        1264
         AG_Z = 6                                                         1264
         AG_W = 1                                                         1264
         AG_TITLE = 'C'                                                   1264
         AG_EXNAME = 'COMPONENT'                                          1264
         AG_PARLIST = 'A_Z_W'                                             1264
         CALL AXCOMPONENT                                                 1264
C    *                                                                    1264
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1264
         CALL AGSRESET                                                    1265
         AG_A = 1                                                         1265
         AG_Z = 1                                                         1265
         AG_W = 2                                                         1265
         AG_TITLE = 'H2'                                                  1265
         AG_EXNAME = 'COMPONENT'                                          1265
         AG_PARLIST = 'A_Z_W'                                             1265
         CALL AXCOMPONENT                                                 1265
C    *                                                                    1265
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1265
         CALL AGSRESET                                                    1266
         AG_DENS = 0.935                                                  1266
         AG_TITLE = 'CH2'                                                 1266
         AG_EXNAME = 'MIXTURE'                                            1266
         AG_PARLIST = 'DENS'                                              1266
         CALL AXMIXTURE                                                   1266
C    *                                                                    1266
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       1266
         CALL AGSRESET                                                    1267
         AG_SEEN = 1                                                      1267
         AG_COLO = 3                                                      1267
         AG_TITLE = 'SITB'                                                1267
         AG_EXNAME = 'ATTRIBUTE'                                          1267
         AG_PARLIST = 'SEEN_COLO'                                         1267
         CALL AXATTRIBUTE                                                 1267
C    *                                                                    1267
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1267
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  1267
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   1267
         CALL AGSRESET                                                    1271
         AG_PHI1 = 0                                                      1271
         AG_DPHI = 360                                                    1271
         AG_NPDIV = 18                                                    1271
         AG_NZ = 2                                                        1271
         AG_ZI(1)=-SWAM_LEN/2                                             1271
         AG_ZI(2)=+SWAM_LEN/2                                             1271
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 1271
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 1271
         AG_RMX(1)=SWAM_RMIN                                              1271
         AG_RMX(2)=SWAM_RMIN                                              1271
         AG_TITLE = 'PGON'                                                1271
         AG_EXNAME = 'SHAPE'                                              1271
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1271
         CALL AXSHAPE                                                     1271
C    *                                                                    1271
6531  CALL AGSPOPB                                                        1272
      GO TO BLOCK_SITB, (6530 ,6540)                                      1272
C End of Local Procedure                                                  1272
C ----------------------------------------------------------------------  1273
C ------------ SBWC                                                       1273
C Begin Local Procedure SBWC                                              1274
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           1274
5190  CONTINUE                                                            1274
         AG_TITLE='SBWC'                                                  1274
         AG_MODULE=MODULE                                                 1274
         AG_MARK='V00'                                                    1274
         CALL AGSPUSH                                                     1274
         AG_MODULE=MODULE                                                 1274
C    *                                                                    1274
         RIN=SWAM_RMIN                                                    1275
         ROU=SSUP_CON1IDMN                                                1276
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1277
         CALL AGSRESET                                                    1277
         AG_TITLE = 'AIR'                                                 1277
         AG_EXNAME = 'MATERIAL'                                           1277
         AG_PARLIST = 'NONE'                                              1277
         CALL AXMATERIAL                                                  1277
C    *                                                                    1277
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       1277
         CALL AGSRESET                                                    1278
         AG_SEEN = 0                                                      1278
         AG_COLO = 1                                                      1278
         AG_TITLE = 'SBWC'                                                1278
         AG_EXNAME = 'ATTRIBUTE'                                          1278
         AG_PARLIST = 'SEEN_COLO'                                         1278
         CALL AXATTRIBUTE                                                 1278
C    *                                                                    1278
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  1278
C    MIN+SWAM_LEN))/2 <===                                                1278
         CALL AGSRESET                                                    1279
         AG_RMIN = RIN                                                    1279
         AG_RMAX = ROU                                                    1279
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   1279
         AG_TITLE = 'TUBE'                                                1279
         AG_EXNAME = 'SHAPE'                                              1279
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1279
         CALL AXSHAPE                                                     1279
C    *                                                                    1279
C    Create SWCM :                                                        1280
C    Call To Local Procedure SWCM                                         1280
         ASSIGN 6560 TO BLOCK_SWCM                                        1280
         GO TO  6550                                                      1280
6560     CONTINUE                                                         1280
C    *                                                                    1280
5191  CALL AGSPOPB                                                        1281
      GO TO BLOCK_SBWC, (5190 ,5200)                                      1281
C End of Local Procedure                                                  1281
C ----------------------------------------------------------------------  1282
C ------------ SWCM                                                       1282
C Begin Local Procedure SWCM                                              1283
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   1283
6550  CONTINUE                                                            1283
         AG_TITLE='SWCM'                                                  1283
         AG_MODULE=MODULE                                                 1283
         AG_MARK='V00'                                                    1283
         CALL AGSPUSH                                                     1283
         AG_MODULE=MODULE                                                 1283
C    *                                                                    1283
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       1284
         CALL AGSRESET                                                    1284
         AG_SEEN = 0                                                      1284
         AG_COLO = 1                                                      1284
         AG_TITLE = 'SWCM'                                                1284
         AG_EXNAME = 'ATTRIBUTE'                                          1284
         AG_PARLIST = 'SEEN_COLO'                                         1284
         CALL AXATTRIBUTE                                                 1284
C    *                                                                    1284
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 1284
         CALL AGSRESET                                                    1285
         AG_NDIV = 3                                                      1285
         AG_IAXIS = 2                                                     1285
         AG_C0 = 0                                                        1285
         AG_TITLE = 'DIVISION'                                            1285
         AG_EXNAME = 'SHAPE'                                              1285
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1285
         CALL AXSHAPE                                                     1285
C    *                                                                    1285
C    Create SXAI :                                                        1286
C    Call To Local Procedure SXAI                                         1286
         ASSIGN 6580 TO BLOCK_SXAI                                        1286
         GO TO  6570                                                      1286
6580     CONTINUE                                                         1286
C    *                                                                    1286
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  1287
C    /2+SSUP_BRATHK/2) <===                                               1287
         CALL AGSRESET                                                    1287
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   1287
         AG_TITLE = 'SXAI'                                                1287
         AG_EXNAME = 'POSITION'                                           1287
         AG_PARLIST = 'Z'                                                 1287
         CALL AXPOSITION                                                  1287
C    *                                                                    1287
C    Create SXBI :                                                        1288
C    Call To Local Procedure SXBI                                         1288
         ASSIGN 6600 TO BLOCK_SXBI                                        1288
         GO TO  6590                                                      1288
6600     CONTINUE                                                         1288
C    *                                                                    1288
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    1289
         CALL AGSRESET                                                    1289
         AG_Z = (SSUP_BRATHK/2)                                           1289
         AG_TITLE = 'SXBI'                                                1289
         AG_EXNAME = 'POSITION'                                           1289
         AG_PARLIST = 'Z'                                                 1289
         CALL AXPOSITION                                                  1289
C    *                                                                    1289
6551  CALL AGSPOPB                                                        1290
      GO TO BLOCK_SWCM, (6550 ,6560)                                      1290
C End of Local Procedure                                                  1290
C ----------------------------------------------------------------------  1291
C ------------ SXAI                                                       1291
C Begin Local Procedure SXAI                                              1292
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      1292
6570  CONTINUE                                                            1292
         AG_TITLE='SXAI'                                                  1292
         AG_MODULE=MODULE                                                 1292
         AG_MARK='V00'                                                    1292
         CALL AGSPUSH                                                     1292
         AG_MODULE=MODULE                                                 1292
C    *                                                                    1292
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       1293
         CALL AGSRESET                                                    1293
         AG_SEEN = 1                                                      1293
         AG_COLO = 2                                                      1293
         AG_TITLE = 'SXAI'                                                1293
         AG_EXNAME = 'ATTRIBUTE'                                          1293
         AG_PARLIST = 'SEEN_COLO'                                         1293
         CALL AXATTRIBUTE                                                 1293
C    *                                                                    1293
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1293
         CALL AGSRESET                                                    1294
         AG_TITLE = 'ALUMINIUM'                                           1294
         AG_EXNAME = 'MATERIAL'                                           1294
         AG_PARLIST = 'NONE'                                              1294
         CALL AXMATERIAL                                                  1294
C    *                                                                    1294
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   1294
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         1294
         CALL AGSRESET                                                    1296
         AG_RMIN = SWAM_RMIN                                              1296
         AG_RMAX = SSUP_CON1IDMN                                          1296
         AG_PHI1 = -5                                                     1296
         AG_PHI2 = 5                                                      1296
         AG_DZ = SSUP_BRATHK/2                                            1296
         AG_TITLE = 'TUBS'                                                1296
         AG_EXNAME = 'SHAPE'                                              1296
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1296
         CALL AXSHAPE                                                     1296
C    *                                                                    1296
6571  CALL AGSPOPB                                                        1297
      GO TO BLOCK_SXAI, (6570 ,6580)                                      1297
C End of Local Procedure                                                  1297
C ----------------------------------------------------------------------  1298
C ------------ SXBI                                                       1298
C Begin Local Procedure SXBI                                              1299
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     1299
6590  CONTINUE                                                            1299
         AG_TITLE='SXBI'                                                  1299
         AG_MODULE=MODULE                                                 1299
         AG_MARK='V00'                                                    1299
         CALL AGSPUSH                                                     1299
         AG_MODULE=MODULE                                                 1299
C    *                                                                    1299
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       1300
         CALL AGSRESET                                                    1300
         AG_SEEN = 1                                                      1300
         AG_COLO = 2                                                      1300
         AG_TITLE = 'SXBI'                                                1300
         AG_EXNAME = 'ATTRIBUTE'                                          1300
         AG_PARLIST = 'SEEN_COLO'                                         1300
         CALL AXATTRIBUTE                                                 1300
C    *                                                                    1300
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1300
         CALL AGSRESET                                                    1301
         AG_TITLE = 'ALUMINIUM'                                           1301
         AG_EXNAME = 'MATERIAL'                                           1301
         AG_PARLIST = 'NONE'                                              1301
         CALL AXMATERIAL                                                  1301
C    *                                                                    1301
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  1301
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  1301
C    _BRATHK)/2) <===                                                     1301
         CALL AGSRESET                                                    1303
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              1303
         AG_RMAX = SSUP_CON1IDMN                                          1303
         AG_PHI1 = -5                                                     1303
         AG_PHI2 = 5                                                      1303
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     1303
         AG_TITLE = 'TUBS'                                                1303
         AG_EXNAME = 'SHAPE'                                              1303
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1303
         CALL AXSHAPE                                                     1303
C    *                                                                    1303
6591  CALL AGSPOPB                                                        1304
      GO TO BLOCK_SXBI, (6590 ,6600)                                      1304
C End of Local Procedure                                                  1304
C ----------------------------------------------------------------------  1305
C ------------ SCRW                                                       1305
C Begin Local Procedure SCRW                                              1306
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   1306
6110  CONTINUE                                                            1306
         AG_TITLE='SCRW'                                                  1306
         AG_MODULE=MODULE                                                 1306
         AG_MARK='V00'                                                    1306
         CALL AGSPUSH                                                     1306
         AG_MODULE=MODULE                                                 1306
C    *                                                                    1306
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        1307
         CALL AGSRESET                                                    1307
         AG_TITLE = 'BERILLIUM'                                           1307
         AG_EXNAME = 'MATERIAL'                                           1307
         AG_PARLIST = 'NONE'                                              1307
         CALL AXMATERIAL                                                  1307
C    *                                                                    1307
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       1307
         CALL AGSRESET                                                    1308
         AG_SEEN = 1                                                      1308
         AG_COLO = 2                                                      1308
         AG_TITLE = 'SCRW'                                                1308
         AG_EXNAME = 'ATTRIBUTE'                                          1308
         AG_PARLIST = 'SEEN_COLO'                                         1308
         CALL AXATTRIBUTE                                                 1308
C    *                                                                    1308
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  1308
C    NDRNG_Z) <===                                                        1308
         CALL AGSRESET                                                    1309
         AG_RMIN = 0                                                      1309
         AG_RMAX = SSUP_ERJDIA/2                                          1309
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   1309
         AG_TITLE = 'TUBE'                                                1309
         AG_EXNAME = 'SHAPE'                                              1309
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1309
         CALL AXSHAPE                                                     1309
C    *                                                                    1309
6111  CALL AGSPOPB                                                        1310
      GO TO BLOCK_SCRW, (6110 ,6120,6150)                                 1310
C End of Local Procedure                                                  1310
C ----------------------------------------------------------------------  1311
C ------------ SBSP                                                       1311
C Begin Local Procedure SBSP                                              1312
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1312
5230  CONTINUE                                                            1312
         AG_TITLE='SBSP'                                                  1312
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
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1313
         CALL AGSRESET                                                    1314
         AG_SEEN = 0                                                      1314
         AG_COLO = 1                                                      1314
         AG_TITLE = 'SBSP'                                                1314
         AG_EXNAME = 'ATTRIBUTE'                                          1314
         AG_PARLIST = 'SEEN_COLO'                                         1314
         CALL AXATTRIBUTE                                                 1314
C    *                                                                    1314
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1314
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1314
         CALL AGSRESET                                                    1316
         AG_RMIN = SVTG_RSIZEMIN                                          1316
         AG_RMAX = SSUB_KMOUNTOD/2                                        1316
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1316
         AG_TITLE = 'TUBE'                                                1316
         AG_EXNAME = 'SHAPE'                                              1316
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1316
         CALL AXSHAPE                                                     1316
C    *                                                                    1316
C    Create SAKM :                                                        1317
C    Call To Local Procedure SAKM                                         1317
         ASSIGN 6620 TO BLOCK_SAKM                                        1317
         GO TO  6610                                                      1317
6620     CONTINUE                                                         1317
C    *                                                                    1317
C    Geant POSITION : SAKM ===> inherited arguments used <===             1318
         CALL AGSRESET                                                    1318
         AG_TITLE = 'SAKM'                                                1318
         AG_EXNAME = 'POSITION'                                           1318
         AG_PARLIST = 'NONE'                                              1318
         CALL AXPOSITION                                                  1318
C    *                                                                    1318
C    Create SBMM :                                                        1319
C    Call To Local Procedure SBMM                                         1319
         ASSIGN 6640 TO BLOCK_SBMM                                        1319
         GO TO  6630                                                      1319
6640     CONTINUE                                                         1319
C    *                                                                    1319
C    Create SBMM :                                                        1320
C    Call To Local Procedure SBMM                                         1320
         ASSIGN 6650 TO BLOCK_SBMM                                        1320
         GO TO  6630                                                      1320
6650     CONTINUE                                                         1320
C    *                                                                    1320
C    Create SBRL :                                                        1321
C    Call To Local Procedure SBRL                                         1321
         ASSIGN 6670 TO BLOCK_SBRL                                        1321
         GO TO  6660                                                      1321
6670     CONTINUE                                                         1321
C    *                                                                    1321
C    Create SBRX :                                                        1322
C    Call To Local Procedure SBRX                                         1322
         ASSIGN 6690 TO BLOCK_SBRX                                        1322
         GO TO  6680                                                      1322
6690     CONTINUE                                                         1322
C    *                                                                    1322
C    *                                                                    1323
         I=-1                                                             1323
            GOTO 6703                                                     1323
6701        I=I+(2)                                                       1323
6703        IF((2)*(I-(1)).GT.0) GO TO 6702                               1323
C       *                                                                 1324
            J=0                                                           1324
               GO TO 6713                                                 1324
6711           J=J+1                                                      1324
6713           IF(J-(1).GT.0) GO TO 6712                                  1324
               PHI=I*SSUB_SRCUTPHI+180*J                                  1325
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1326
     *         2.0)                                                       1326
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1328
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1328
C          ==                                                             1328
               CALL AGSRESET                                              1328
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1328
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1328
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1328
               AG_ALPHAN=AG_ALPHAN+1                                      1328
               AG_ALPHAK(AG_ALPHAN)=3                                     1328
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1328
               AG_TITLE = 'SBMM'                                          1328
               AG_EXNAME = 'POSITION'                                     1328
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1328
               CALL AXPOSITION                                            1328
C          *                                                              1328
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1328
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1331
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1331
C           ORT=ZXY <===                                                  1331
               CALL AGSRESET                                              1331
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1331
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1331
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1331
               AG_ALPHAN=AG_ALPHAN+1                                      1331
               AG_ALPHAK(AG_ALPHAN)=3                                     1331
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1331
               AG_ALPHAN=AG_ALPHAN+1                                      1331
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1331
               AG_TITLE = 'SBRL'                                          1331
               AG_EXNAME = 'POSITION'                                     1331
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1331
               CALL AXPOSITION                                            1331
C          *                                                              1331
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1331
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1331
C           ORT=ZXY <===                                                  1331
               CALL AGSRESET                                              1333
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1333
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1333
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1333
               AG_ALPHAN=AG_ALPHAN+1                                      1333
               AG_ALPHAK(AG_ALPHAN)=3                                     1333
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1333
               AG_ALPHAN=AG_ALPHAN+1                                      1333
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1333
               AG_TITLE = 'SBRX'                                          1333
               AG_EXNAME = 'POSITION'                                     1333
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1333
               CALL AXPOSITION                                            1333
C          *                                                              1333
            GO TO 6711                                                    1334
6712        CONTINUE                                                      1334
         GO TO 6701                                                       1335
6702     CONTINUE                                                         1335
C    Create SBSR :                                                        1336
C    Call To Local Procedure SBSR                                         1336
         ASSIGN 6730 TO BLOCK_SBSR                                        1336
         GO TO  6720                                                      1336
6730     CONTINUE                                                         1336
C    *                                                                    1336
C    Geant POSITION : SBSR ===> inherited arguments used <===             1337
         CALL AGSRESET                                                    1337
         AG_TITLE = 'SBSR'                                                1337
         AG_EXNAME = 'POSITION'                                           1337
         AG_PARLIST = 'NONE'                                              1337
         CALL AXPOSITION                                                  1337
C    *                                                                    1337
5231  CALL AGSPOPB                                                        1338
      GO TO BLOCK_SBSP, (5230 ,5240)                                      1338
C End of Local Procedure                                                  1338
C ----------------------------------------------------------------------  1339
C ------------ SAKM                                                       1339
C Begin Local Procedure SAKM                                              1340
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1340
6610  CONTINUE                                                            1340
         AG_TITLE='SAKM'                                                  1340
         AG_MODULE=MODULE                                                 1340
         AG_MARK='V00'                                                    1340
         CALL AGSPUSH                                                     1340
         AG_MODULE=MODULE                                                 1340
C    *                                                                    1340
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1341
         CALL AGSRESET                                                    1341
         AG_TITLE = 'ALUMINIUM'                                           1341
         AG_EXNAME = 'MATERIAL'                                           1341
         AG_PARLIST = 'NONE'                                              1341
         CALL AXMATERIAL                                                  1341
C    *                                                                    1341
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1341
         CALL AGSRESET                                                    1342
         AG_SEEN = 1                                                      1342
         AG_COLO = 2                                                      1342
         AG_TITLE = 'SAKM'                                                1342
         AG_EXNAME = 'ATTRIBUTE'                                          1342
         AG_PARLIST = 'SEEN_COLO'                                         1342
         CALL AXATTRIBUTE                                                 1342
C    *                                                                    1342
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1342
C    DZ=SSUB_KMNTTHK/2 <===                                               1342
         CALL AGSRESET                                                    1344
         AG_RMIN = SSUB_KMOUNTID/2                                        1344
         AG_RMAX = SSUB_KMOUNTOD/2                                        1344
         AG_DZ = SSUB_KMNTTHK/2                                           1344
         AG_TITLE = 'TUBE'                                                1344
         AG_EXNAME = 'SHAPE'                                              1344
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1344
         CALL AXSHAPE                                                     1344
C    *                                                                    1344
C    Create SCKM :                                                        1345
C    Call To Local Procedure SCKM                                         1345
         ASSIGN 6750 TO BLOCK_SCKM                                        1345
         GO TO  6740                                                      1345
6750     CONTINUE                                                         1345
C    *                                                                    1345
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1346
         CALL AGSRESET                                                    1346
         AG_Y = SSUB_KMCUTOFF                                             1346
         AG_KONLY = 'MANY'                                                1346
         AG_TITLE = 'SCKM'                                                1346
         AG_EXNAME = 'POSITION'                                           1346
         AG_PARLIST = 'Y_KONLY'                                           1346
         CALL AXPOSITION                                                  1346
C    *                                                                    1346
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1346
C     <===                                                                1346
         CALL AGSRESET                                                    1347
         AG_Y = -SSUB_KMCUTOFF                                            1347
         AG_ALPHAN=AG_ALPHAN+1                                            1347
         AG_ALPHAK(AG_ALPHAN)=3                                           1347
         AG_ALPHAL(AG_ALPHAN)=180                                         1347
         AG_KONLY = 'MANY'                                                1347
         AG_TITLE = 'SCKM'                                                1347
         AG_EXNAME = 'POSITION'                                           1347
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1347
         CALL AXPOSITION                                                  1347
C    *                                                                    1347
6611  CALL AGSPOPB                                                        1348
      GO TO BLOCK_SAKM, (6610 ,6620)                                      1348
C End of Local Procedure                                                  1348
C ----------------------------------------------------------------------  1349
C ------------ SCKM                                                       1349
C Begin Local Procedure SCKM                                              1350
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1350
6740  CONTINUE                                                            1350
         AG_TITLE='SCKM'                                                  1350
         AG_MODULE=MODULE                                                 1350
         AG_MARK='V00'                                                    1350
         CALL AGSPUSH                                                     1350
         AG_MODULE=MODULE                                                 1350
C    *                                                                    1350
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1351
         CALL AGSRESET                                                    1351
         AG_TITLE = 'AIR'                                                 1351
         AG_EXNAME = 'MATERIAL'                                           1351
         AG_PARLIST = 'NONE'                                              1351
         CALL AXMATERIAL                                                  1351
C    *                                                                    1351
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1351
         CALL AGSRESET                                                    1352
         AG_SEEN = 1                                                      1352
         AG_COLO = 1                                                      1352
         AG_TITLE = 'SCKM'                                                1352
         AG_EXNAME = 'ATTRIBUTE'                                          1352
         AG_PARLIST = 'SEEN_COLO'                                         1352
         CALL AXATTRIBUTE                                                 1352
C    *                                                                    1352
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1352
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1352
         CALL AGSRESET                                                    1355
         AG_RMIN = SSUB_KMCUTID/2                                         1355
         AG_RMAX = SSUB_KMCUTOD/2                                         1355
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1355
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1355
         AG_DZ = SSUB_KMNTTHK/2                                           1355
         AG_TITLE = 'TUBS'                                                1355
         AG_EXNAME = 'SHAPE'                                              1355
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1355
         CALL AXSHAPE                                                     1355
C    *                                                                    1355
6741  CALL AGSPOPB                                                        1356
      GO TO BLOCK_SCKM, (6740 ,6750)                                      1356
C End of Local Procedure                                                  1356
C ----------------------------------------------------------------------  1357
C ------------ SBSR                                                       1357
C Begin Local Procedure SBSR                                              1358
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1358
6720  CONTINUE                                                            1358
         AG_TITLE='SBSR'                                                  1358
         AG_MODULE=MODULE                                                 1358
         AG_MARK='V00'                                                    1358
         CALL AGSPUSH                                                     1358
         AG_MODULE=MODULE                                                 1358
C    *                                                                    1358
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1359
         CALL AGSRESET                                                    1359
         AG_TITLE = 'G10'                                                 1359
         AG_EXNAME = 'MATERIAL'                                           1359
         AG_PARLIST = 'NONE'                                              1359
         CALL AXMATERIAL                                                  1359
C    *                                                                    1359
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1359
         CALL AGSRESET                                                    1360
         AG_SEEN = 1                                                      1360
         AG_COLO = 3                                                      1360
         AG_TITLE = 'SBSR'                                                1360
         AG_EXNAME = 'ATTRIBUTE'                                          1360
         AG_PARLIST = 'SEEN_COLO'                                         1360
         CALL AXATTRIBUTE                                                 1360
C    *                                                                    1360
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1360
C    =SSUB_SRINGTHK/2 <===                                                1360
         CALL AGSRESET                                                    1362
         AG_RMIN = SSUB_SRINGID/2                                         1362
         AG_RMAX = SSUB_SRINGOD/2                                         1362
         AG_DZ = SSUB_SRINGTHK/2                                          1362
         AG_TITLE = 'TUBE'                                                1362
         AG_EXNAME = 'SHAPE'                                              1362
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1362
         CALL AXSHAPE                                                     1362
C    *                                                                    1362
C    Create SBCR :                                                        1363
C    Call To Local Procedure SBCR                                         1363
         ASSIGN 6770 TO BLOCK_SBCR                                        1363
         GO TO  6760                                                      1363
6770     CONTINUE                                                         1363
C    *                                                                    1363
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1364
C    *                                                                    1365
         I=-1                                                             1365
            GOTO 6783                                                     1365
6781        I=I+(2)                                                       1365
6783        IF((2)*(I-(1)).GT.0) GO TO 6782                               1365
C       *                                                                 1366
            J=0                                                           1366
               GO TO 6793                                                 1366
6791           J=J+1                                                      1366
6793           IF(J-(1).GT.0) GO TO 6792                                  1366
               PHI=I*SSUB_SRCUTPHI+180*J                                  1367
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1369
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1369
               CALL AGSRESET                                              1369
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1369
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1369
               AG_ALPHAN=AG_ALPHAN+1                                      1369
               AG_ALPHAK(AG_ALPHAN)=3                                     1369
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1369
               AG_KONLY = 'MANY'                                          1369
               AG_TITLE = 'SBCR'                                          1369
               AG_EXNAME = 'POSITION'                                     1369
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1369
               CALL AXPOSITION                                            1369
C          *                                                              1369
            GO TO 6791                                                    1370
6792        CONTINUE                                                      1370
         GO TO 6781                                                       1371
6782     CONTINUE                                                         1371
6721  CALL AGSPOPB                                                        1372
      GO TO BLOCK_SBSR, (6720 ,6730)                                      1372
C End of Local Procedure                                                  1372
C ----------------------------------------------------------------------  1373
C ------------ SBCR                                                       1373
C Begin Local Procedure SBCR                                              1374
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1374
6760  CONTINUE                                                            1374
         AG_TITLE='SBCR'                                                  1374
         AG_MODULE=MODULE                                                 1374
         AG_MARK='V00'                                                    1374
         CALL AGSPUSH                                                     1374
         AG_MODULE=MODULE                                                 1374
C    *                                                                    1374
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1375
         CALL AGSRESET                                                    1375
         AG_TITLE = 'AIR'                                                 1375
         AG_EXNAME = 'MATERIAL'                                           1375
         AG_PARLIST = 'NONE'                                              1375
         CALL AXMATERIAL                                                  1375
C    *                                                                    1375
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1375
         CALL AGSRESET                                                    1376
         AG_SEEN = 1                                                      1376
         AG_COLO = 1                                                      1376
         AG_TITLE = 'SBCR'                                                1376
         AG_EXNAME = 'ATTRIBUTE'                                          1376
         AG_PARLIST = 'SEEN_COLO'                                         1376
         CALL AXATTRIBUTE                                                 1376
C    *                                                                    1376
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1376
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1376
         CALL AGSRESET                                                    1378
         AG_DX = SSUB_SRCUTWID/2                                          1378
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1378
         AG_DZ = SSUB_SRINGTHK/2                                          1378
         AG_TITLE = 'BOX'                                                 1378
         AG_EXNAME = 'SHAPE'                                              1378
         AG_PARLIST = 'DX_DY_DZ'                                          1378
         CALL AXSHAPE                                                     1378
C    *                                                                    1378
6761  CALL AGSPOPB                                                        1379
      GO TO BLOCK_SBCR, (6760 ,6770)                                      1379
C End of Local Procedure                                                  1379
C ----------------------------------------------------------------------  1380
C ------------ SBRL                                                       1380
C Begin Local Procedure SBRL                                              1381
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1381
6660  CONTINUE                                                            1381
         AG_TITLE='SBRL'                                                  1381
         AG_MODULE=MODULE                                                 1381
         AG_MARK='V00'                                                    1381
         CALL AGSPUSH                                                     1381
         AG_MODULE=MODULE                                                 1381
C    *                                                                    1381
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1382
C    ADL=12.6 <===                                                        1382
         CALL AGSRESET                                                    1382
         AG_A = 20.719                                                    1382
         AG_Z = 10.307                                                    1382
         AG_DENS = 2.23                                                   1382
         AG_ABSL = 50.7                                                   1382
         AG_RADL = 12.6                                                   1382
         AG_TITLE = 'PYREX'                                               1382
         AG_EXNAME = 'MATERIAL'                                           1382
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1382
         CALL AXMATERIAL                                                  1382
C    *                                                                    1382
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1382
         CALL AGSRESET                                                    1383
         AG_SEEN = 1                                                      1383
         AG_COLO = 6                                                      1383
         AG_TITLE = 'SBRL'                                                1383
         AG_EXNAME = 'ATTRIBUTE'                                          1383
         AG_PARLIST = 'SEEN_COLO'                                         1383
         CALL AXATTRIBUTE                                                 1383
C    *                                                                    1383
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1383
C    =                                                                    1383
         CALL AGSRESET                                                    1384
         AG_RMIN = SSUB_SROLLID/2                                         1384
         AG_RMAX = SSUB_SROLLOD/2                                         1384
         AG_TITLE = 'TUBE'                                                1384
         AG_EXNAME = 'SHAPE'                                              1384
         AG_PARLIST = 'RMIN_RMAX'                                         1384
         CALL AXSHAPE                                                     1384
C    *                                                                    1384
6661  CALL AGSPOPB                                                        1385
      GO TO BLOCK_SBRL, (6660 ,6670)                                      1385
C End of Local Procedure                                                  1385
C ----------------------------------------------------------------------  1386
C ------------ SBRX                                                       1386
C Begin Local Procedure SBRX                                              1387
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1387
6680  CONTINUE                                                            1387
         AG_TITLE='SBRX'                                                  1387
         AG_MODULE=MODULE                                                 1387
         AG_MARK='V00'                                                    1387
         CALL AGSPUSH                                                     1387
         AG_MODULE=MODULE                                                 1387
C    *                                                                    1387
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1388
         CALL AGSRESET                                                    1388
         AG_TITLE = 'IRON'                                                1388
         AG_EXNAME = 'MATERIAL'                                           1388
         AG_PARLIST = 'NONE'                                              1388
         CALL AXMATERIAL                                                  1388
C    *                                                                    1388
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1388
         CALL AGSRESET                                                    1389
         AG_SEEN = 1                                                      1389
         AG_COLO = 2                                                      1389
         AG_TITLE = 'SBRX'                                                1389
         AG_EXNAME = 'ATTRIBUTE'                                          1389
         AG_PARLIST = 'SEEN_COLO'                                         1389
         CALL AXATTRIBUTE                                                 1389
C    *                                                                    1389
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1389
C    LEN/2 <===                                                           1389
         CALL AGSRESET                                                    1391
         AG_RMIN = 0.0                                                    1391
         AG_RMAX = SSUB_SROLLID/2                                         1391
         AG_DZ = SSUB_SWIRELEN/2                                          1391
         AG_TITLE = 'TUBE'                                                1391
         AG_EXNAME = 'SHAPE'                                              1391
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1391
         CALL AXSHAPE                                                     1391
C    *                                                                    1391
6681  CALL AGSPOPB                                                        1392
      GO TO BLOCK_SBRX, (6680 ,6690)                                      1392
C End of Local Procedure                                                  1392
C ----------------------------------------------------------------------  1393
C ------------ SBMM                                                       1393
C Begin Local Procedure SBMM                                              1394
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1394
6630  CONTINUE                                                            1394
         AG_TITLE='SBMM'                                                  1394
         AG_MODULE=MODULE                                                 1394
         AG_MARK='V00'                                                    1394
         CALL AGSPUSH                                                     1394
         AG_MODULE=MODULE                                                 1394
C    *                                                                    1394
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1395
         CALL AGSRESET                                                    1395
         AG_TITLE = 'AIR'                                                 1395
         AG_EXNAME = 'MATERIAL'                                           1395
         AG_PARLIST = 'NONE'                                              1395
         CALL AXMATERIAL                                                  1395
C    *                                                                    1395
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1395
         CALL AGSRESET                                                    1396
         AG_SEEN = 0                                                      1396
         AG_COLO = 1                                                      1396
         AG_TITLE = 'SBMM'                                                1396
         AG_EXNAME = 'ATTRIBUTE'                                          1396
         AG_PARLIST = 'SEEN_COLO'                                         1396
         CALL AXATTRIBUTE                                                 1396
C    *                                                                    1396
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1396
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1396
         CALL AGSRESET                                                    1398
         AG_DX = SSUB_MBLKIWID/2                                          1398
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1398
         AG_DZ = SSUB_MBLKHGH/2                                           1398
         AG_TITLE = 'BOX'                                                 1398
         AG_EXNAME = 'SHAPE'                                              1398
         AG_PARLIST = 'DX_DY_DZ'                                          1398
         CALL AXSHAPE                                                     1398
C    *                                                                    1398
C    Create SMRD :                                                        1399
C    Call To Local Procedure SMRD                                         1399
         ASSIGN 6810 TO BLOCK_SMRD                                        1399
         GO TO  6800                                                      1399
6810     CONTINUE                                                         1399
C    *                                                                    1399
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1400
     *   SSUB_SROLLOD                                                     1400
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1401
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1401
         CALL AGSRESET                                                    1401
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1401
         AG_ALPHAN=AG_ALPHAN+1                                            1401
         AG_ALPHAK(AG_ALPHAN)=1                                           1401
         AG_ALPHAL(AG_ALPHAN)=90                                          1401
         AG_TITLE = 'SMRD'                                                1401
         AG_EXNAME = 'POSITION'                                           1401
         AG_PARLIST = 'Y_ALPHAX'                                          1401
         CALL AXPOSITION                                                  1401
C    *                                                                    1401
C    Create SBMO :                                                        1402
C    Call To Local Procedure SBMO                                         1402
         ASSIGN 6830 TO BLOCK_SBMO                                        1402
         GO TO  6820                                                      1402
6830     CONTINUE                                                         1402
C    *                                                                    1402
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1403
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1404
C    UB_SROLLOD)/2 <===                                                   1404
         CALL AGSRESET                                                    1404
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1404
         AG_TITLE = 'SBMO'                                                1404
         AG_EXNAME = 'POSITION'                                           1404
         AG_PARLIST = 'Y'                                                 1404
         CALL AXPOSITION                                                  1404
C    *                                                                    1404
C    Create SBMI :                                                        1405
C    Call To Local Procedure SBMI                                         1405
         ASSIGN 6850 TO BLOCK_SBMI                                        1405
         GO TO  6840                                                      1405
6850     CONTINUE                                                         1405
C    *                                                                    1405
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1406
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1407
C    UB_SROLLOD)/2 <===                                                   1407
         CALL AGSRESET                                                    1407
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1407
         AG_TITLE = 'SBMI'                                                1407
         AG_EXNAME = 'POSITION'                                           1407
         AG_PARLIST = 'Y'                                                 1407
         CALL AXPOSITION                                                  1407
C    *                                                                    1407
6631  CALL AGSPOPB                                                        1408
      GO TO BLOCK_SBMM, (6630 ,6640,6650)                                 1408
C End of Local Procedure                                                  1408
C ----------------------------------------------------------------------  1409
C ------------ SMRD                                                       1409
C Begin Local Procedure SMRD                                              1410
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1410
6800  CONTINUE                                                            1410
         AG_TITLE='SMRD'                                                  1410
         AG_MODULE=MODULE                                                 1410
         AG_MARK='V00'                                                    1410
         CALL AGSPUSH                                                     1410
         AG_MODULE=MODULE                                                 1410
C    *                                                                    1410
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1411
         CALL AGSRESET                                                    1411
         AG_TITLE = 'ALUMINIUM'                                           1411
         AG_EXNAME = 'MATERIAL'                                           1411
         AG_PARLIST = 'NONE'                                              1411
         CALL AXMATERIAL                                                  1411
C    *                                                                    1411
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1411
         CALL AGSRESET                                                    1412
         AG_SEEN = 1                                                      1412
         AG_COLO = 2                                                      1412
         AG_TITLE = 'SMRD'                                                1412
         AG_EXNAME = 'ATTRIBUTE'                                          1412
         AG_PARLIST = 'SEEN_COLO'                                         1412
         CALL AXATTRIBUTE                                                 1412
C    *                                                                    1412
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1412
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1412
         CALL AGSRESET                                                    1414
         AG_RMIN = 0.0                                                    1414
         AG_RMAX = SSUB_MRODDIA/2                                         1414
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1414
         AG_TITLE = 'TUBE'                                                1414
         AG_EXNAME = 'SHAPE'                                              1414
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1414
         CALL AXSHAPE                                                     1414
C    *                                                                    1414
6801  CALL AGSPOPB                                                        1415
      GO TO BLOCK_SMRD, (6800 ,6810)                                      1415
C End of Local Procedure                                                  1415
C ----------------------------------------------------------------------  1416
C ------------ SBMO                                                       1416
C Begin Local Procedure SBMO                                              1417
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1417
6820  CONTINUE                                                            1417
         AG_TITLE='SBMO'                                                  1417
         AG_MODULE=MODULE                                                 1417
         AG_MARK='V00'                                                    1417
         CALL AGSPUSH                                                     1417
         AG_MODULE=MODULE                                                 1417
C    *                                                                    1417
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1418
         CALL AGSRESET                                                    1418
         AG_TITLE = 'G10'                                                 1418
         AG_EXNAME = 'MATERIAL'                                           1418
         AG_PARLIST = 'NONE'                                              1418
         CALL AXMATERIAL                                                  1418
C    *                                                                    1418
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1418
         CALL AGSRESET                                                    1419
         AG_SEEN = 1                                                      1419
         AG_COLO = 3                                                      1419
         AG_TITLE = 'SBMO'                                                1419
         AG_EXNAME = 'ATTRIBUTE'                                          1419
         AG_PARLIST = 'SEEN_COLO'                                         1419
         CALL AXATTRIBUTE                                                 1419
C    *                                                                    1419
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1419
C    UB_MBLKHGH/2 <===                                                    1419
         CALL AGSRESET                                                    1421
         AG_DX = SSUB_MBLKOWID/2                                          1421
         AG_DY = SSUB_MBLKOLEN/2                                          1421
         AG_DZ = SSUB_MBLKHGH/2                                           1421
         AG_TITLE = 'BOX'                                                 1421
         AG_EXNAME = 'SHAPE'                                              1421
         AG_PARLIST = 'DX_DY_DZ'                                          1421
         CALL AXSHAPE                                                     1421
C    *                                                                    1421
6821  CALL AGSPOPB                                                        1422
      GO TO BLOCK_SBMO, (6820 ,6830)                                      1422
C End of Local Procedure                                                  1422
C ----------------------------------------------------------------------  1423
C ------------ SBMI                                                       1423
C Begin Local Procedure SBMI                                              1424
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1424
6840  CONTINUE                                                            1424
         AG_TITLE='SBMI'                                                  1424
         AG_MODULE=MODULE                                                 1424
         AG_MARK='V00'                                                    1424
         CALL AGSPUSH                                                     1424
         AG_MODULE=MODULE                                                 1424
C    *                                                                    1424
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1425
         CALL AGSRESET                                                    1425
         AG_TITLE = 'G10'                                                 1425
         AG_EXNAME = 'MATERIAL'                                           1425
         AG_PARLIST = 'NONE'                                              1425
         CALL AXMATERIAL                                                  1425
C    *                                                                    1425
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1425
         CALL AGSRESET                                                    1426
         AG_SEEN = 1                                                      1426
         AG_COLO = 3                                                      1426
         AG_TITLE = 'SBMO'                                                1426
         AG_EXNAME = 'ATTRIBUTE'                                          1426
         AG_PARLIST = 'SEEN_COLO'                                         1426
         CALL AXATTRIBUTE                                                 1426
C    *                                                                    1426
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1426
C    UB_MBLKHGH/2 <===                                                    1426
         CALL AGSRESET                                                    1428
         AG_DX = SSUB_MBLKIWID/2                                          1428
         AG_DY = SSUB_MBLKILEN/2                                          1428
         AG_DZ = SSUB_MBLKHGH/2                                           1428
         AG_TITLE = 'BOX'                                                 1428
         AG_EXNAME = 'SHAPE'                                              1428
         AG_PARLIST = 'DX_DY_DZ'                                          1428
         CALL AXSHAPE                                                     1428
C    *                                                                    1428
6841  CALL AGSPOPB                                                        1429
      GO TO BLOCK_SBMI, (6840 ,6850)                                      1429
C End of Local Procedure                                                  1429
C ----------------------------------------------------------------------  1430
C ------------ SALM                                                       1430
C Begin Local Procedure SALM                                              1431
C SALM IS THE ALUMINUM SHIELD MESH                                        1431
5370  CONTINUE                                                            1431
         AG_TITLE='SALM'                                                  1431
         AG_MODULE=MODULE                                                 1431
         AG_MARK='V00'                                                    1431
         CALL AGSPUSH                                                     1431
         AG_MODULE=MODULE                                                 1431
C    *                                                                    1431
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1432
         CALL AGSRESET                                                    1432
         AG_TITLE = 'ALUMINIUM'                                           1432
         AG_EXNAME = 'MATERIAL'                                           1432
         AG_PARLIST = 'NONE'                                              1432
         CALL AXMATERIAL                                                  1432
C    *                                                                    1432
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1432
         CALL AGSRESET                                                    1433
         AG_SEEN = 1                                                      1433
         AG_COLO = 2                                                      1433
         AG_TITLE = 'SALM'                                                1433
         AG_EXNAME = 'ATTRIBUTE'                                          1433
         AG_PARLIST = 'SEEN_COLO'                                         1433
         CALL AXATTRIBUTE                                                 1433
C    *                                                                    1433
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1433
C    DZ=SSLD_ALMSHTHK/2 <===                                              1433
         CALL AGSRESET                                                    1435
         AG_RMIN = SSLD_ALMESHID/2                                        1435
         AG_RMAX = SSLD_ALMESHOD/2                                        1435
         AG_DZ = SSLD_ALMSHTHK/2                                          1435
         AG_TITLE = 'TUBE'                                                1435
         AG_EXNAME = 'SHAPE'                                              1435
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1435
         CALL AXSHAPE                                                     1435
C    *                                                                    1435
5371  CALL AGSPOPB                                                        1436
      GO TO BLOCK_SALM, (5370 ,5380)                                      1436
C End of Local Procedure                                                  1436
C ----------------------------------------------------------------------  1437
C ------------ SISH                                                       1437
C Begin Local Procedure SISH                                              1438
C SISH IS THE INNER SHIELD CYLINDER                                       1438
5310  CONTINUE                                                            1438
         AG_TITLE='SISH'                                                  1438
         AG_MODULE=MODULE                                                 1438
         AG_MARK='V00'                                                    1438
         CALL AGSPUSH                                                     1438
         AG_MODULE=MODULE                                                 1438
C    *                                                                    1438
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1439
         CALL AGSRESET                                                    1439
         AG_TITLE = 'ALKAP'                                               1439
         AG_EXNAME = 'MATERIAL'                                           1439
         AG_PARLIST = 'NONE'                                              1439
         CALL AXMATERIAL                                                  1439
C    *                                                                    1439
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1439
         CALL AGSRESET                                                    1440
         AG_SEEN = 1                                                      1440
         AG_COLO = 3                                                      1440
         AG_TITLE = 'SISH'                                                1440
         AG_EXNAME = 'ATTRIBUTE'                                          1440
         AG_PARLIST = 'SEEN_COLO'                                         1440
         CALL AXATTRIBUTE                                                 1440
C    *                                                                    1440
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1440
C    D_SINLEN <===                                                        1440
         CALL AGSRESET                                                    1442
         AG_RMIN = SSLD_SINRINN                                           1442
         AG_RMAX = SSLD_SINROUT                                           1442
         AG_DZ = SSLD_SINLEN                                              1442
         AG_TITLE = 'TUBE'                                                1442
         AG_EXNAME = 'SHAPE'                                              1442
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1442
         CALL AXSHAPE                                                     1442
C    *                                                                    1442
5311  CALL AGSPOPB                                                        1443
      GO TO BLOCK_SISH, (5310 ,5320)                                      1443
C End of Local Procedure                                                  1443
C ----------------------------------------------------------------------  1444
C ------------ SSSH                                                       1444
C Begin Local Procedure SSSH                                              1445
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1445
5350  CONTINUE                                                            1445
         AG_TITLE='SSSH'                                                  1445
         AG_MODULE=MODULE                                                 1445
         AG_MARK='V00'                                                    1445
         CALL AGSPUSH                                                     1445
         AG_MODULE=MODULE                                                 1445
C    *                                                                    1445
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1446
         CALL AGSRESET                                                    1446
         AG_TITLE = 'SSDALMY'                                             1446
         AG_EXNAME = 'MATERIAL'                                           1446
         AG_PARLIST = 'NONE'                                              1446
         CALL AXMATERIAL                                                  1446
C    *                                                                    1446
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1446
         CALL AGSRESET                                                    1447
         AG_SEEN = 1                                                      1447
         AG_COLO = 3                                                      1447
         AG_TITLE = 'SSSH'                                                1447
         AG_EXNAME = 'ATTRIBUTE'                                          1447
         AG_PARLIST = 'SEEN_COLO'                                         1447
         CALL AXATTRIBUTE                                                 1447
C    *                                                                    1447
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1447
C    SLD_SSEPLEN <===                                                     1447
         CALL AGSRESET                                                    1449
         AG_RMIN = SSLD_SSEPRINN                                          1449
         AG_RMAX = SSLD_SSEPROUT                                          1449
         AG_DZ = SSLD_SSEPLEN                                             1449
         AG_TITLE = 'TUBE'                                                1449
         AG_EXNAME = 'SHAPE'                                              1449
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1449
         CALL AXSHAPE                                                     1449
C    *                                                                    1449
5351  CALL AGSPOPB                                                        1450
      GO TO BLOCK_SSSH, (5350 ,5360)                                      1450
C End of Local Procedure                                                  1450
C ----------------------------------------------------------------------  1451
C ------------ SOSH                                                       1451
C Begin Local Procedure SOSH                                              1452
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1452
5330  CONTINUE                                                            1452
         AG_TITLE='SOSH'                                                  1452
         AG_MODULE=MODULE                                                 1452
         AG_MARK='V00'                                                    1452
         CALL AGSPUSH                                                     1452
         AG_MODULE=MODULE                                                 1452
C    *                                                                    1452
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1453
         CALL AGSRESET                                                    1453
         AG_TITLE = 'SSDALMY'                                             1453
         AG_EXNAME = 'MATERIAL'                                           1453
         AG_PARLIST = 'NONE'                                              1453
         CALL AXMATERIAL                                                  1453
C    *                                                                    1453
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1453
         CALL AGSRESET                                                    1454
         AG_SEEN = 1                                                      1454
         AG_COLO = 3                                                      1454
         AG_TITLE = 'SOSH'                                                1454
         AG_EXNAME = 'ATTRIBUTE'                                          1454
         AG_PARLIST = 'SEEN_COLO'                                         1454
         CALL AXATTRIBUTE                                                 1454
C    *                                                                    1454
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1454
C    SLD_SOUTLEN <===                                                     1454
         CALL AGSRESET                                                    1456
         AG_RMIN = SSLD_SOUTRINN                                          1456
         AG_RMAX = SSLD_SOUTROUT                                          1456
         AG_DZ = SSLD_SOUTLEN                                             1456
         AG_TITLE = 'TUBE'                                                1456
         AG_EXNAME = 'SHAPE'                                              1456
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1456
         CALL AXSHAPE                                                     1456
C    *                                                                    1456
5331  CALL AGSPOPB                                                        1457
      GO TO BLOCK_SOSH, (5330 ,5340)                                      1457
C End of Local Procedure                                                  1457
      WRITE(LOUT,6860)'===>>  [End AgML Module SVTTGEO7]  <<==='          1457
6860  FORMAT(A32,/,/)                                                     1457
C MODULE SVTTGEO7                                                         1459
      END                                                                 1459
