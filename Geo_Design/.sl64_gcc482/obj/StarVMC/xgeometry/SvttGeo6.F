C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO6                                                 1
C SVTTGEO6 IS THE SVT: CORRECTED WAFERS AND SENSITIVE VOLUMES             1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SVTTGEO6'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE SVT: CORRECTED WAFERS AND SENSITIVE           1
     *VOLUMES'/                                                           1
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
C CREATED 19 OCT 2005                                                     2
      CHARACTER*40 CREATED /'19 OCT 2005'/                                3
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
      INTEGER BLOCK_SLYD                                                  15
      INTEGER BLOCK_SLSD                                                  15
      INTEGER BLOCK_SLDI                                                  15
      INTEGER BLOCK_STLI                                                  15
      INTEGER BLOCK_STSI                                                  15
      INTEGER BLOCK_SVTD                                                  15
      INTEGER BLOCK_SBER                                                  15
      INTEGER BLOCK_STAB                                                  15
      INTEGER BLOCK_STRU                                                  15
      INTEGER BLOCK_SPCB                                                  15
      INTEGER BLOCK_SRHC                                                  15
      INTEGER BLOCK_SBWC                                                  15
      INTEGER BLOCK_SWCM                                                  15
      INTEGER BLOCK_SXAI                                                  15
      INTEGER BLOCK_SXBI                                                  15
      INTEGER BLOCK_STRA                                                  15
      INTEGER BLOCK_SSID                                                  15
      INTEGER BLOCK_SSIR                                                  15
      INTEGER BLOCK_SELE                                                  15
      INTEGER BLOCK_SWCH                                                  15
      INTEGER BLOCK_SWCW                                                  15
      INTEGER BLOCK_SWCS                                                  15
      INTEGER BLOCK_SBOI                                                  15
      INTEGER BLOCK_SAGP                                                  15
      INTEGER BLOCK_SDYE                                                  15
      INTEGER BLOCK_SECA                                                  15
      INTEGER BLOCK_SIRP                                                  15
      INTEGER BLOCK_SIRT                                                  15
      INTEGER BLOCK_SOER                                                  15
      INTEGER BLOCK_SCON                                                  15
      INTEGER BLOCK_SROD                                                  15
      INTEGER BLOCK_SGRA                                                  15
      INTEGER BLOCK_STAP                                                  15
      INTEGER BLOCK_STAC                                                  15
      INTEGER BLOCK_SHLA                                                  15
      INTEGER BLOCK_SHLB                                                  15
      INTEGER BLOCK_SHMA                                                  15
      INTEGER BLOCK_SHMB                                                  15
      INTEGER BLOCK_SWHO                                                  15
      INTEGER BLOCK_SHWA                                                  15
      INTEGER BLOCK_SCMY                                                  15
      INTEGER BLOCK_SCAL                                                  15
      INTEGER BLOCK_SWMM                                                  15
      INTEGER BLOCK_SWMB                                                  15
      INTEGER BLOCK_SWRP                                                  15
      INTEGER BLOCK_SXRL                                                  15
      INTEGER BLOCK_SYRU                                                  15
      INTEGER BLOCK_SWMT                                                  15
      INTEGER BLOCK_SWMS                                                  15
      INTEGER BLOCK_SWMW                                                  15
      INTEGER BLOCK_SOTB                                                  15
      INTEGER BLOCK_SITB                                                  15
      INTEGER BLOCK_SBRG                                                  15
      INTEGER BLOCK_SBRM                                                  15
      INTEGER BLOCK_SBRI                                                  15
      INTEGER BLOCK_SOES                                                  15
      INTEGER BLOCK_SIES                                                  15
      INTEGER BLOCK_SOSM                                                  15
      INTEGER BLOCK_SISM                                                  15
      INTEGER BLOCK_SCRW                                                  15
      INTEGER BLOCK_SGLA                                                  15
      INTEGER BLOCK_SBSP                                                  15
      INTEGER BLOCK_SAKM                                                  15
      INTEGER BLOCK_SCKM                                                  15
      INTEGER BLOCK_SBSR                                                  15
      INTEGER BLOCK_SBCR                                                  15
      INTEGER BLOCK_SBRX                                                  15
      INTEGER BLOCK_SBRL                                                  15
      INTEGER BLOCK_SBMM                                                  15
      INTEGER BLOCK_SBMO                                                  15
      INTEGER BLOCK_SBMI                                                  15
      INTEGER BLOCK_SMRD                                                  15
      INTEGER BLOCK_SALM                                                  15
      INTEGER BLOCK_SISH                                                  15
      INTEGER BLOCK_SSSH                                                  15
      INTEGER BLOCK_SOSH                                                  15
      INTEGER BLOCK_SCBM                                                  15
      INTEGER BLOCK_SCBL                                                  15
      INTEGER BLOCK_SFED                                                  15
      INTEGER BLOCK_SPLS                                                  15
      INTEGER BLOCK_SOUM                                                  15
      INTEGER BLOCK_SOUR                                                  15
C -                                                                       17
      REAL SVTG_VERSION                                                   17
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            17
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       17
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          17
     *MSVTG_VERSION)                                                      17
      CHARACTER CSVTG_VERSION*40/' '/                                     17
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             17
      INTEGER MSVTG_VERSION(2)/1,1/                                       17
      REAL RE_SVTG_VERSION                                                17
C -                                                                       17
      INTEGER SVTG_NLAYER                                                 17
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              17
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        17
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           17
     *MSVTG_NLAYER)                                                       17
      CHARACTER CSVTG_NLAYER*40/' '/                                      17
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               17
      INTEGER MSVTG_NLAYER(2)/1,1/                                        17
      REAL RE_SVTG_NLAYER                                                 17
C -                                                                       17
      REAL SVTG_RSIZEMIN                                                  17
      EQUIVALENCE (BANK_SVTG(5),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          17
     *(NAMES_SVTG(3),NSVTG_RSIZEMIN)                                      17
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_RSIZEMIN),(MAP_SVTG(1,3),         17
     *MSVTG_RSIZEMIN)                                                     17
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    17
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           17
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      17
      REAL RE_SVTG_RSIZEMIN                                               17
C -                                                                       17
      REAL SVTG_RSIZEMAX                                                  17
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          17
     *(NAMES_SVTG(4),NSVTG_RSIZEMAX)                                      17
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMAX),(MAP_SVTG(1,4),         17
     *MSVTG_RSIZEMAX)                                                     17
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    17
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           17
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      17
      REAL RE_SVTG_RSIZEMAX                                               17
C -                                                                       17
      REAL SVTG_ZSIZEMAX                                                  17
      EQUIVALENCE (BANK_SVTG(7),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          17
     *(NAMES_SVTG(5),NSVTG_ZSIZEMAX)                                      17
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_ZSIZEMAX),(MAP_SVTG(1,5),         17
     *MSVTG_ZSIZEMAX)                                                     17
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    17
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           17
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      17
      REAL RE_SVTG_ZSIZEMAX                                               17
C -                                                                       17
      REAL SVTG_ANGOFF                                                    17
      EQUIVALENCE (BANK_SVTG(8),SVTG_ANGOFF,RE_SVTG_ANGOFF),              17
     *(NAMES_SVTG(6),NSVTG_ANGOFF)                                        17
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ANGOFF),(MAP_SVTG(1,6),           17
     *MSVTG_ANGOFF)                                                       17
      CHARACTER CSVTG_ANGOFF*40/' '/                                      17
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               17
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        17
      REAL RE_SVTG_ANGOFF                                                 17
C -                                                                       17
      REAL SVTG_SUPPORTVER                                                17
      EQUIVALENCE (BANK_SVTG(9),SVTG_SUPPORTVER,RE_SVTG_SUPPORTVER),      17
     *(NAMES_SVTG(7),NSVTG_SUPPORTVER)                                    17
      EQUIVALENCE (COMMEN_SVTG(7),CSVTG_SUPPORTVER),(MAP_SVTG(1,7),       17
     *MSVTG_SUPPORTVER)                                                   17
      CHARACTER CSVTG_SUPPORTVER*40/' '/                                  17
      CHARACTER NSVTG_SUPPORTVER*12/'R.SUPPORTVER'/                       17
      INTEGER MSVTG_SUPPORTVER(2)/1,1/                                    17
      REAL RE_SVTG_SUPPORTVER                                             17
C -                                                                       17
      REAL SVTG_CONEVER                                                   17
      EQUIVALENCE (BANK_SVTG(10),SVTG_CONEVER,RE_SVTG_CONEVER),           17
     *(NAMES_SVTG(8),NSVTG_CONEVER)                                       17
      EQUIVALENCE (COMMEN_SVTG(8),CSVTG_CONEVER),(MAP_SVTG(1,8),          17
     *MSVTG_CONEVER)                                                      17
      CHARACTER CSVTG_CONEVER*40/' '/                                     17
      CHARACTER NSVTG_CONEVER*12/'R.CONEVER'/                             17
      INTEGER MSVTG_CONEVER(2)/1,1/                                       17
      REAL RE_SVTG_CONEVER                                                17
C -                                                                       17
      REAL SVTG_IFMANY                                                    17
      EQUIVALENCE (BANK_SVTG(11),SVTG_IFMANY,RE_SVTG_IFMANY),             17
     *(NAMES_SVTG(9),NSVTG_IFMANY)                                        17
      EQUIVALENCE (COMMEN_SVTG(9),CSVTG_IFMANY),(MAP_SVTG(1,9),           17
     *MSVTG_IFMANY)                                                       17
      CHARACTER CSVTG_IFMANY*40/' '/                                      17
      CHARACTER NSVTG_IFMANY*12/'R.IFMANY'/                               17
      INTEGER MSVTG_IFMANY(2)/1,1/                                        17
      REAL RE_SVTG_IFMANY                                                 17
C -                                                                       17
      REAL SVTG_NMIN                                                      17
      EQUIVALENCE (BANK_SVTG(12),SVTG_NMIN,RE_SVTG_NMIN),                 17
     *(NAMES_SVTG(10),NSVTG_NMIN)                                         17
      EQUIVALENCE (COMMEN_SVTG(10),CSVTG_NMIN),(MAP_SVTG(1,10),           17
     *MSVTG_NMIN)                                                         17
      CHARACTER CSVTG_NMIN*40/' '/                                        17
      CHARACTER NSVTG_NMIN*12/'R.NMIN'/                                   17
      INTEGER MSVTG_NMIN(2)/1,1/                                          17
      REAL RE_SVTG_NMIN                                                   17
C -                                                                       17
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                17
      PARAMETER (LEN_SVTG=12)                                             17
      PARAMETER (LLEN_SVTG=10)                                            17
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           17
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        17
      DATA IDNUM_SVTG/10*0/                                               17
      REAL BANK_SVTG(LEN_SVTG)                                            17
      CHARACTER*60 BPATH_SVTG/' '/                                        17
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          17
     *BPATH_SVTG                                                          17
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             17
C *                                                                       17
C -                                                                       19
      REAL SWCA_VERSION                                                   19
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            19
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       19
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          19
     *MSWCA_VERSION)                                                      19
      CHARACTER CSWCA_VERSION*40/' '/                                     19
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             19
      INTEGER MSWCA_VERSION(2)/1,1/                                       19
      REAL RE_SWCA_VERSION                                                19
C -                                                                       19
      REAL SWCA_LENGTH                                                    19
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              19
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        19
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           19
     *MSWCA_LENGTH)                                                       19
      CHARACTER CSWCA_LENGTH*40/' '/                                      19
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               19
      INTEGER MSWCA_LENGTH(2)/1,1/                                        19
      REAL RE_SWCA_LENGTH                                                 19
C -                                                                       19
      REAL SWCA_WAFERWID                                                  19
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          19
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      19
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         19
     *MSWCA_WAFERWID)                                                     19
      CHARACTER CSWCA_WAFERWID*40/' '/                                    19
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           19
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      19
      REAL RE_SWCA_WAFERWID                                               19
C -                                                                       19
      REAL SWCA_WAFERLEN                                                  19
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          19
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      19
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         19
     *MSWCA_WAFERLEN)                                                     19
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    19
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           19
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      19
      REAL RE_SWCA_WAFERLEN                                               19
C -                                                                       19
      REAL SWCA_WAFERTHK                                                  19
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          19
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      19
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         19
     *MSWCA_WAFERTHK)                                                     19
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    19
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           19
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      19
      REAL RE_SWCA_WAFERTHK                                               19
C -                                                                       19
      REAL SWCA_ROHATHK                                                   19
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            19
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       19
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          19
     *MSWCA_ROHATHK)                                                      19
      CHARACTER CSWCA_ROHATHK*40/' '/                                     19
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             19
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       19
      REAL RE_SWCA_ROHATHK                                                19
C -                                                                       19
      REAL SWCA_WAFCARWD                                                  19
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          19
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      19
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         19
     *MSWCA_WAFCARWD)                                                     19
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    19
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           19
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      19
      REAL RE_SWCA_WAFCARWD                                               19
C -                                                                       19
      REAL SWCA_WAFCARTH                                                  19
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         19
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      19
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         19
     *MSWCA_WAFCARTH)                                                     19
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    19
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           19
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      19
      REAL RE_SWCA_WAFCARTH                                               19
C -                                                                       19
      REAL SWCA_WAFERGAP                                                  19
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         19
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      19
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         19
     *MSWCA_WAFERGAP)                                                     19
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    19
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           19
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      19
      REAL RE_SWCA_WAFERGAP                                               19
C -                                                                       19
      REAL SWCA_DRIFT                                                     19
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               19
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        19
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          19
     *MSWCA_DRIFT)                                                        19
      CHARACTER CSWCA_DRIFT*40/' '/                                       19
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 19
      INTEGER MSWCA_DRIFT(2)/1,1/                                         19
      REAL RE_SWCA_DRIFT                                                  19
C -                                                                       19
      REAL SWCA_STRUTLEN                                                  19
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         19
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     19
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       19
     *MSWCA_STRUTLEN)                                                     19
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    19
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           19
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      19
      REAL RE_SWCA_STRUTLEN                                               19
C -                                                                       19
      REAL SWCA_SENSWID                                                   19
      EQUIVALENCE (BANK_SWCA(14),SWCA_SENSWID,RE_SWCA_SENSWID),           19
     *(NAMES_SWCA(12),NSWCA_SENSWID)                                      19
      EQUIVALENCE (COMMEN_SWCA(12),CSWCA_SENSWID),(MAP_SWCA(1,12),        19
     *MSWCA_SENSWID)                                                      19
      CHARACTER CSWCA_SENSWID*40/' '/                                     19
      CHARACTER NSWCA_SENSWID*12/'R.SENSWID'/                             19
      INTEGER MSWCA_SENSWID(2)/1,1/                                       19
      REAL RE_SWCA_SENSWID                                                19
C -                                                                       19
      REAL SWCA_SENSLEN                                                   19
      EQUIVALENCE (BANK_SWCA(15),SWCA_SENSLEN,RE_SWCA_SENSLEN),           19
     *(NAMES_SWCA(13),NSWCA_SENSLEN)                                      19
      EQUIVALENCE (COMMEN_SWCA(13),CSWCA_SENSLEN),(MAP_SWCA(1,13),        19
     *MSWCA_SENSLEN)                                                      19
      CHARACTER CSWCA_SENSLEN*40/' '/                                     19
      CHARACTER NSWCA_SENSLEN*12/'R.SENSLEN'/                             19
      INTEGER MSWCA_SENSLEN(2)/1,1/                                       19
      REAL RE_SWCA_SENSLEN                                                19
C -                                                                       19
      REAL SWCA_SENSGIR                                                   19
      EQUIVALENCE (BANK_SWCA(16),SWCA_SENSGIR,RE_SWCA_SENSGIR),           19
     *(NAMES_SWCA(14),NSWCA_SENSGIR)                                      19
      EQUIVALENCE (COMMEN_SWCA(14),CSWCA_SENSGIR),(MAP_SWCA(1,14),        19
     *MSWCA_SENSGIR)                                                      19
      CHARACTER CSWCA_SENSGIR*40/' '/                                     19
      CHARACTER NSWCA_SENSGIR*12/'R.SENSGIR'/                             19
      INTEGER MSWCA_SENSGIR(2)/1,1/                                       19
      REAL RE_SWCA_SENSGIR                                                19
C -                                                                       19
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                19
      PARAMETER (LEN_SWCA=16)                                             19
      PARAMETER (LLEN_SWCA=14)                                            19
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           19
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        19
      DATA IDNUM_SWCA/10*0/                                               19
      REAL BANK_SWCA(LEN_SWCA)                                            19
      CHARACTER*60 BPATH_SWCA/' '/                                        19
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          19
     *BPATH_SWCA                                                          19
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             19
C *                                                                       19
C -                                                                       22
      REAL SSUP_VERSION                                                   22
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            22
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       22
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          22
     *MSSUP_VERSION)                                                      22
      CHARACTER CSSUP_VERSION*40/' '/                                     22
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             22
      INTEGER MSSUP_VERSION(2)/1,1/                                       22
      REAL RE_SSUP_VERSION                                                22
C -                                                                       22
      REAL SSUP_CABTHK                                                    22
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              22
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        22
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           22
     *MSSUP_CABTHK)                                                       22
      CHARACTER CSSUP_CABTHK*40/' '/                                      22
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               22
      INTEGER MSSUP_CABTHK(2)/1,1/                                        22
      REAL RE_SSUP_CABTHK                                                 22
C -                                                                       22
      REAL SSUP_HOSRMN                                                    22
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              22
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        22
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           22
     *MSSUP_HOSRMN)                                                       22
      CHARACTER CSSUP_HOSRMN*40/' '/                                      22
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               22
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        22
      REAL RE_SSUP_HOSRMN                                                 22
C -                                                                       22
      REAL SSUP_HOSRMX                                                    22
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              22
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        22
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           22
     *MSSUP_HOSRMX)                                                       22
      CHARACTER CSSUP_HOSRMX*40/' '/                                      22
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               22
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        22
      REAL RE_SSUP_HOSRMX                                                 22
C -                                                                       22
      REAL SSUP_NHOSES                                                    22
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              22
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        22
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           22
     *MSSUP_NHOSES)                                                       22
      CHARACTER CSSUP_NHOSES*40/' '/                                      22
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               22
      INTEGER MSSUP_NHOSES(2)/1,1/                                        22
      REAL RE_SSUP_NHOSES                                                 22
C -                                                                       22
      REAL SSUP_WRPMYTHK                                                  22
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          22
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      22
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         22
     *MSSUP_WRPMYTHK)                                                     22
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    22
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           22
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      22
      REAL RE_SSUP_WRPMYTHK                                               22
C -                                                                       22
      REAL SSUP_WRPALTHK                                                  22
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          22
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      22
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         22
     *MSSUP_WRPALTHK)                                                     22
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    22
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           22
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      22
      REAL RE_SSUP_WRPALTHK                                               22
C -                                                                       22
      REAL SSUP_GRPHTHK                                                   22
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           22
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       22
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          22
     *MSSUP_GRPHTHK)                                                      22
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     22
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             22
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       22
      REAL RE_SSUP_GRPHTHK                                                22
C -                                                                       22
      REAL SSUP_CONE1ZMN                                                  22
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         22
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      22
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         22
     *MSSUP_CONE1ZMN)                                                     22
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    22
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           22
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      22
      REAL RE_SSUP_CONE1ZMN                                               22
C -                                                                       22
      REAL SSUP_RODLEN                                                    22
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             22
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       22
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         22
     *MSSUP_RODLEN)                                                       22
      CHARACTER CSSUP_RODLEN*40/' '/                                      22
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               22
      INTEGER MSSUP_RODLEN(2)/1,1/                                        22
      REAL RE_SSUP_RODLEN                                                 22
C -                                                                       22
      REAL SSUP_RODDIST                                                   22
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           22
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      22
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        22
     *MSSUP_RODDIST)                                                      22
      CHARACTER CSSUP_RODDIST*40/' '/                                     22
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             22
      INTEGER MSSUP_RODDIST(2)/1,1/                                       22
      REAL RE_SSUP_RODDIST                                                22
C -                                                                       22
      REAL SSUP_RODID                                                     22
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               22
     *(NAMES_SSUP(12),NSSUP_RODID)                                        22
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          22
     *MSSUP_RODID)                                                        22
      CHARACTER CSSUP_RODID*40/' '/                                       22
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 22
      INTEGER MSSUP_RODID(2)/1,1/                                         22
      REAL RE_SSUP_RODID                                                  22
C -                                                                       22
      REAL SSUP_RODOD                                                     22
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               22
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        22
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          22
     *MSSUP_RODOD)                                                        22
      CHARACTER CSSUP_RODOD*40/' '/                                       22
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 22
      INTEGER MSSUP_RODOD(2)/1,1/                                         22
      REAL RE_SSUP_RODOD                                                  22
C -                                                                       22
      REAL SSUP_CON1IDMN                                                  22
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         22
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     22
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       22
     *MSSUP_CON1IDMN)                                                     22
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    22
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           22
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      22
      REAL RE_SSUP_CON1IDMN                                               22
C -                                                                       22
      REAL SSUP_CON3IDMN                                                  22
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         22
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     22
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       22
     *MSSUP_CON3IDMN)                                                     22
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    22
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           22
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      22
      REAL RE_SSUP_CON3IDMN                                               22
C -                                                                       22
      REAL SSUP_CON4IDMN                                                  22
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         22
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     22
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       22
     *MSSUP_CON4IDMN)                                                     22
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    22
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           22
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      22
      REAL RE_SSUP_CON4IDMN                                               22
C -                                                                       22
      REAL SSUP_CON4IDMX                                                  22
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         22
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     22
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       22
     *MSSUP_CON4IDMX)                                                     22
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    22
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           22
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      22
      REAL RE_SSUP_CON4IDMX                                               22
C -                                                                       22
      REAL SSUP_CONE3ZMX                                                  22
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         22
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     22
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       22
     *MSSUP_CONE3ZMX)                                                     22
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    22
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           22
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      22
      REAL RE_SSUP_CONE3ZMX                                               22
C -                                                                       22
      REAL SSUP_CONE4ZMX                                                  22
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         22
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     22
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       22
     *MSSUP_CONE4ZMX)                                                     22
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    22
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           22
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      22
      REAL RE_SSUP_CONE4ZMX                                               22
C -                                                                       22
      REAL SSUP_BRATHK                                                    22
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             22
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       22
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         22
     *MSSUP_BRATHK)                                                       22
      CHARACTER CSSUP_BRATHK*40/' '/                                      22
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               22
      INTEGER MSSUP_BRATHK(2)/1,1/                                        22
      REAL RE_SSUP_BRATHK                                                 22
C -                                                                       22
      REAL SSUP_ERJTHK                                                    22
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             22
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       22
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         22
     *MSSUP_ERJTHK)                                                       22
      CHARACTER CSSUP_ERJTHK*40/' '/                                      22
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               22
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        22
      REAL RE_SSUP_ERJTHK                                                 22
C -                                                                       22
      REAL SSUP_ERJWID                                                    22
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             22
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       22
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         22
     *MSSUP_ERJWID)                                                       22
      CHARACTER CSSUP_ERJWID*40/' '/                                      22
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               22
      INTEGER MSSUP_ERJWID(2)/1,1/                                        22
      REAL RE_SSUP_ERJWID                                                 22
C -                                                                       22
      REAL SSUP_ERJLEN                                                    22
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             22
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       22
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         22
     *MSSUP_ERJLEN)                                                       22
      CHARACTER CSSUP_ERJLEN*40/' '/                                      22
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               22
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        22
      REAL RE_SSUP_ERJLEN                                                 22
C -                                                                       22
      REAL SSUP_ERJZDIS                                                   22
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           22
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      22
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        22
     *MSSUP_ERJZDIS)                                                      22
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     22
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             22
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       22
      REAL RE_SSUP_ERJZDIS                                                22
C -                                                                       22
      REAL SSUP_ERJ1X                                                     22
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               22
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        22
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          22
     *MSSUP_ERJ1X)                                                        22
      CHARACTER CSSUP_ERJ1X*40/' '/                                       22
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 22
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         22
      REAL RE_SSUP_ERJ1X                                                  22
C -                                                                       22
      REAL SSUP_ERJ2X                                                     22
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               22
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        22
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          22
     *MSSUP_ERJ2X)                                                        22
      CHARACTER CSSUP_ERJ2X*40/' '/                                       22
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 22
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         22
      REAL RE_SSUP_ERJ2X                                                  22
C -                                                                       22
      REAL SSUP_ERJ2Y                                                     22
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               22
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        22
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          22
     *MSSUP_ERJ2Y)                                                        22
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       22
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 22
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         22
      REAL RE_SSUP_ERJ2Y                                                  22
C -                                                                       22
      REAL SSUP_ERJRAD                                                    22
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             22
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       22
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         22
     *MSSUP_ERJRAD)                                                       22
      CHARACTER CSSUP_ERJRAD*40/' '/                                      22
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               22
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        22
      REAL RE_SSUP_ERJRAD                                                 22
C -                                                                       22
      REAL SSUP_ERJDIA                                                    22
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             22
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       22
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         22
     *MSSUP_ERJDIA)                                                       22
      CHARACTER CSSUP_ERJDIA*40/' '/                                      22
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               22
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        22
      REAL RE_SSUP_ERJDIA                                                 22
C -                                                                       22
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                22
      PARAMETER (LEN_SSUP=31)                                             22
      PARAMETER (LLEN_SSUP=29)                                            22
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           22
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        22
      DATA IDNUM_SSUP/10*0/                                               22
      REAL BANK_SSUP(LEN_SSUP)                                            22
      CHARACTER*60 BPATH_SSUP/' '/                                        22
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          22
     *BPATH_SSUP                                                          22
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             22
C *                                                                       22
C -                                                                       26
      REAL SSUB_VERSION                                                   26
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            26
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       26
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          26
     *MSSUB_VERSION)                                                      26
      CHARACTER CSSUB_VERSION*40/' '/                                     26
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             26
      INTEGER MSSUB_VERSION(2)/1,1/                                       26
      REAL RE_SSUB_VERSION                                                26
C -                                                                       26
      REAL SSUB_KMOUNTID                                                  26
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          26
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      26
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         26
     *MSSUB_KMOUNTID)                                                     26
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    26
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           26
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      26
      REAL RE_SSUB_KMOUNTID                                               26
C -                                                                       26
      REAL SSUB_KMOUNTOD                                                  26
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          26
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      26
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         26
     *MSSUB_KMOUNTOD)                                                     26
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    26
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           26
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      26
      REAL RE_SSUB_KMOUNTOD                                               26
C -                                                                       26
      REAL SSUB_KMNTTHK                                                   26
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            26
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       26
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          26
     *MSSUB_KMNTTHK)                                                      26
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     26
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             26
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       26
      REAL RE_SSUB_KMNTTHK                                                26
C -                                                                       26
      REAL SSUB_KMCUTOD                                                   26
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            26
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       26
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          26
     *MSSUB_KMCUTOD)                                                      26
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     26
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             26
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       26
      REAL RE_SSUB_KMCUTOD                                                26
C -                                                                       26
      REAL SSUB_KMCUTID                                                   26
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            26
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       26
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          26
     *MSSUB_KMCUTID)                                                      26
      CHARACTER CSSUB_KMCUTID*40/' '/                                     26
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             26
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       26
      REAL RE_SSUB_KMCUTID                                                26
C -                                                                       26
      REAL SSUB_KMCUTOA                                                   26
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            26
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       26
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          26
     *MSSUB_KMCUTOA)                                                      26
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     26
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             26
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       26
      REAL RE_SSUB_KMCUTOA                                                26
C -                                                                       26
      REAL SSUB_KMCUTOFF                                                  26
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         26
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      26
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         26
     *MSSUB_KMCUTOFF)                                                     26
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    26
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           26
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      26
      REAL RE_SSUB_KMCUTOFF                                               26
C -                                                                       26
      REAL SSUB_SRINGID                                                   26
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           26
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       26
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          26
     *MSSUB_SRINGID)                                                      26
      CHARACTER CSSUB_SRINGID*40/' '/                                     26
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             26
      INTEGER MSSUB_SRINGID(2)/1,1/                                       26
      REAL RE_SSUB_SRINGID                                                26
C -                                                                       26
      REAL SSUB_SRINGOD                                                   26
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           26
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      26
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        26
     *MSSUB_SRINGOD)                                                      26
      CHARACTER CSSUB_SRINGOD*40/' '/                                     26
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             26
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       26
      REAL RE_SSUB_SRINGOD                                                26
C -                                                                       26
      REAL SSUB_SRINGTHK                                                  26
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         26
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     26
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       26
     *MSSUB_SRINGTHK)                                                     26
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    26
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           26
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      26
      REAL RE_SSUB_SRINGTHK                                               26
C -                                                                       26
      REAL SSUB_SRCUTPHI                                                  26
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         26
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     26
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       26
     *MSSUB_SRCUTPHI)                                                     26
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    26
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           26
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      26
      REAL RE_SSUB_SRCUTPHI                                               26
C -                                                                       26
      REAL SSUB_SRCUTWID                                                  26
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         26
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     26
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       26
     *MSSUB_SRCUTWID)                                                     26
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    26
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           26
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      26
      REAL RE_SSUB_SRCUTWID                                               26
C -                                                                       26
      REAL SSUB_SRCUTOUT                                                  26
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         26
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     26
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       26
     *MSSUB_SRCUTOUT)                                                     26
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    26
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           26
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      26
      REAL RE_SSUB_SRCUTOUT                                               26
C -                                                                       26
      REAL SSUB_SRCUTIN                                                   26
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           26
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      26
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        26
     *MSSUB_SRCUTIN)                                                      26
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     26
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             26
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       26
      REAL RE_SSUB_SRCUTIN                                                26
C -                                                                       26
      REAL SSUB_SROLLID                                                   26
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           26
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      26
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        26
     *MSSUB_SROLLID)                                                      26
      CHARACTER CSSUB_SROLLID*40/' '/                                     26
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             26
      INTEGER MSSUB_SROLLID(2)/1,1/                                       26
      REAL RE_SSUB_SROLLID                                                26
C -                                                                       26
      REAL SSUB_SROLLOD                                                   26
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           26
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      26
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        26
     *MSSUB_SROLLOD)                                                      26
      CHARACTER CSSUB_SROLLOD*40/' '/                                     26
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             26
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       26
      REAL RE_SSUB_SROLLOD                                                26
C -                                                                       26
      REAL SSUB_SROLLLEN                                                  26
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         26
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     26
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       26
     *MSSUB_SROLLLEN)                                                     26
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    26
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           26
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      26
      REAL RE_SSUB_SROLLLEN                                               26
C -                                                                       26
      REAL SSUB_SWIRELEN                                                  26
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         26
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     26
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       26
     *MSSUB_SWIRELEN)                                                     26
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    26
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           26
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      26
      REAL RE_SSUB_SWIRELEN                                               26
C -                                                                       26
      REAL SSUB_MBLKHGH                                                   26
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           26
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      26
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        26
     *MSSUB_MBLKHGH)                                                      26
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     26
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             26
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       26
      REAL RE_SSUB_MBLKHGH                                                26
C -                                                                       26
      REAL SSUB_MBLKOWID                                                  26
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         26
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     26
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       26
     *MSSUB_MBLKOWID)                                                     26
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    26
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           26
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      26
      REAL RE_SSUB_MBLKOWID                                               26
C -                                                                       26
      REAL SSUB_MBLKOLEN                                                  26
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         26
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     26
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       26
     *MSSUB_MBLKOLEN)                                                     26
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    26
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           26
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      26
      REAL RE_SSUB_MBLKOLEN                                               26
C -                                                                       26
      REAL SSUB_MBLKIWID                                                  26
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         26
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     26
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       26
     *MSSUB_MBLKIWID)                                                     26
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    26
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           26
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      26
      REAL RE_SSUB_MBLKIWID                                               26
C -                                                                       26
      REAL SSUB_MBLKILEN                                                  26
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         26
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     26
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       26
     *MSSUB_MBLKILEN)                                                     26
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    26
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           26
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      26
      REAL RE_SSUB_MBLKILEN                                               26
C -                                                                       26
      REAL SSUB_MBLKORAD                                                  26
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         26
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     26
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       26
     *MSSUB_MBLKORAD)                                                     26
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    26
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           26
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      26
      REAL RE_SSUB_MBLKORAD                                               26
C -                                                                       26
      REAL SSUB_MBLKIRAD                                                  26
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         26
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     26
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       26
     *MSSUB_MBLKIRAD)                                                     26
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    26
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           26
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      26
      REAL RE_SSUB_MBLKIRAD                                               26
C -                                                                       26
      REAL SSUB_MRODDIA                                                   26
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           26
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      26
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        26
     *MSSUB_MRODDIA)                                                      26
      CHARACTER CSSUB_MRODDIA*40/' '/                                     26
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             26
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       26
      REAL RE_SSUB_MRODDIA                                                26
C -                                                                       26
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                26
      PARAMETER (LEN_SSUB=29)                                             26
      PARAMETER (LLEN_SSUB=27)                                            26
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           26
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        26
      DATA IDNUM_SSUB/10*0/                                               26
      REAL BANK_SSUB(LEN_SSUB)                                            26
      CHARACTER*60 BPATH_SSUB/' '/                                        26
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          26
     *BPATH_SSUB                                                          26
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             26
C *                                                                       26
C -                                                                       27
      REAL SWAM_VERSION                                                   27
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            27
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       27
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          27
     *MSWAM_VERSION)                                                      27
      CHARACTER CSWAM_VERSION*40/' '/                                     27
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             27
      INTEGER MSWAM_VERSION(2)/1,1/                                       27
      REAL RE_SWAM_VERSION                                                27
C -                                                                       27
      REAL SWAM_ZMIN                                                      27
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   27
     *NSWAM_ZMIN)                                                         27
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             27
     *MSWAM_ZMIN)                                                         27
      CHARACTER CSWAM_ZMIN*40/' '/                                        27
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   27
      INTEGER MSWAM_ZMIN(2)/1,1/                                          27
      REAL RE_SWAM_ZMIN                                                   27
C -                                                                       27
      REAL SWAM_LEN                                                       27
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     27
     *NSWAM_LEN)                                                          27
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    27
      CHARACTER CSWAM_LEN*40/' '/                                         27
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     27
      INTEGER MSWAM_LEN(2)/1,1/                                           27
      REAL RE_SWAM_LEN                                                    27
C -                                                                       27
      REAL SWAM_RMIN                                                      27
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   27
     *NSWAM_RMIN)                                                         27
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             27
     *MSWAM_RMIN)                                                         27
      CHARACTER CSWAM_RMIN*40/' '/                                        27
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   27
      INTEGER MSWAM_RMIN(2)/1,1/                                          27
      REAL RE_SWAM_RMIN                                                   27
C -                                                                       27
      REAL SWAM_RMAX                                                      27
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   27
     *NSWAM_RMAX)                                                         27
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             27
     *MSWAM_RMAX)                                                         27
      CHARACTER CSWAM_RMAX*40/' '/                                        27
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   27
      INTEGER MSWAM_RMAX(2)/1,1/                                          27
      REAL RE_SWAM_RMAX                                                   27
C -                                                                       27
      REAL SWAM_TBRDTHK                                                   27
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            27
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       27
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          27
     *MSWAM_TBRDTHK)                                                      27
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     27
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             27
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       27
      REAL RE_SWAM_TBRDTHK                                                27
C -                                                                       27
      REAL SWAM_WALLTHK                                                   27
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            27
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       27
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          27
     *MSWAM_WALLTHK)                                                      27
      CHARACTER CSWAM_WALLTHK*40/' '/                                     27
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             27
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       27
      REAL RE_SWAM_WALLTHK                                                27
C -                                                                       27
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                27
      PARAMETER (LEN_SWAM=9)                                              27
      PARAMETER (LLEN_SWAM=7)                                             27
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           27
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        27
      DATA IDNUM_SWAM/10*0/                                               27
      REAL BANK_SWAM(LEN_SWAM)                                            27
      CHARACTER*60 BPATH_SWAM/' '/                                        27
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          27
     *BPATH_SWAM                                                          27
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             27
C *                                                                       27
C -                                                                       28
      REAL SERG_VERSION                                                   28
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            28
     *(NAMES_SERG(1),NSERG_VERSION)                                       28
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          28
     *MSERG_VERSION)                                                      28
      CHARACTER CSERG_VERSION*40/' '/                                     28
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             28
      INTEGER MSERG_VERSION(2)/1,1/                                       28
      REAL RE_SERG_VERSION                                                28
C -                                                                       28
      REAL SERG_IRNGTRMX                                                  28
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          28
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      28
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         28
     *MSERG_IRNGTRMX)                                                     28
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    28
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           28
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      28
      REAL RE_SERG_IRNGTRMX                                               28
C -                                                                       28
      REAL SERG_IRNGPRMN                                                  28
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          28
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      28
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         28
     *MSERG_IRNGPRMN)                                                     28
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    28
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           28
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      28
      REAL RE_SERG_IRNGPRMN                                               28
C -                                                                       28
      REAL SERG_ORNGRMIN                                                  28
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          28
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      28
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         28
     *MSERG_ORNGRMIN)                                                     28
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    28
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           28
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      28
      REAL RE_SERG_ORNGRMIN                                               28
C -                                                                       28
      REAL SERG_ORNGRMAX                                                  28
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          28
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      28
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         28
     *MSERG_ORNGRMAX)                                                     28
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    28
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           28
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      28
      REAL RE_SERG_ORNGRMAX                                               28
C -                                                                       28
      REAL SERG_ENDRNGTH                                                  28
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          28
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      28
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         28
     *MSERG_ENDRNGTH)                                                     28
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    28
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           28
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      28
      REAL RE_SERG_ENDRNGTH                                               28
C -                                                                       28
      REAL SERG_ENDRNGZM                                                  28
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          28
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      28
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         28
     *MSERG_ENDRNGZM)                                                     28
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    28
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           28
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      28
      REAL RE_SERG_ENDRNGZM                                               28
C -                                                                       28
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                28
      PARAMETER (LEN_SERG=9)                                              28
      PARAMETER (LLEN_SERG=7)                                             28
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           28
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        28
      DATA IDNUM_SERG/10*0/                                               28
      REAL BANK_SERG(LEN_SERG)                                            28
      CHARACTER*60 BPATH_SERG/' '/                                        28
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          28
     *BPATH_SERG                                                          28
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             28
C *                                                                       28
C -                                                                       30
      REAL SELC_VERSION                                                   30
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            30
     *(NAMES_SELC(1),NSELC_VERSION)                                       30
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          30
     *MSELC_VERSION)                                                      30
      CHARACTER CSELC_VERSION*40/' '/                                     30
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             30
      INTEGER MSELC_VERSION(2)/1,1/                                       30
      REAL RE_SELC_VERSION                                                30
C -                                                                       30
      REAL SELC_BETHK                                                     30
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                30
     *(NAMES_SELC(2),NSELC_BETHK)                                         30
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            30
     *MSELC_BETHK)                                                        30
      CHARACTER CSELC_BETHK*40/' '/                                       30
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 30
      INTEGER MSELC_BETHK(2)/1,1/                                         30
      REAL RE_SELC_BETHK                                                  30
C -                                                                       30
      REAL SELC_WATTHK                                                    30
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              30
     *(NAMES_SELC(3),NSELC_WATTHK)                                        30
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           30
     *MSELC_WATTHK)                                                       30
      CHARACTER CSELC_WATTHK*40/' '/                                      30
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               30
      INTEGER MSELC_WATTHK(2)/1,1/                                        30
      REAL RE_SELC_WATTHK                                                 30
C -                                                                       30
      REAL SELC_BEOTHK                                                    30
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              30
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        30
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           30
     *MSELC_BEOTHK)                                                       30
      CHARACTER CSELC_BEOTHK*40/' '/                                      30
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               30
      INTEGER MSELC_BEOTHK(2)/1,1/                                        30
      REAL RE_SELC_BEOTHK                                                 30
C -                                                                       30
      REAL SELC_DYETHK                                                    30
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              30
     *(NAMES_SELC(5),NSELC_DYETHK)                                        30
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           30
     *MSELC_DYETHK)                                                       30
      CHARACTER CSELC_DYETHK*40/' '/                                      30
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               30
      INTEGER MSELC_DYETHK(2)/1,1/                                        30
      REAL RE_SELC_DYETHK                                                 30
C -                                                                       30
      REAL SELC_DYEWID                                                    30
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              30
     *(NAMES_SELC(6),NSELC_DYEWID)                                        30
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           30
     *MSELC_DYEWID)                                                       30
      CHARACTER CSELC_DYEWID*40/' '/                                      30
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               30
      INTEGER MSELC_DYEWID(2)/1,1/                                        30
      REAL RE_SELC_DYEWID                                                 30
C -                                                                       30
      REAL SELC_DYESPC                                                    30
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              30
     *(NAMES_SELC(7),NSELC_DYESPC)                                        30
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           30
     *MSELC_DYESPC)                                                       30
      CHARACTER CSELC_DYESPC*40/' '/                                      30
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               30
      INTEGER MSELC_DYESPC(2)/1,1/                                        30
      REAL RE_SELC_DYESPC                                                 30
C -                                                                       30
      REAL SELC_ELCAWID                                                   30
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           30
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       30
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          30
     *MSELC_ELCAWID)                                                      30
      CHARACTER CSELC_ELCAWID*40/' '/                                     30
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             30
      INTEGER MSELC_ELCAWID(2)/1,1/                                       30
      REAL RE_SELC_ELCAWID                                                30
C -                                                                       30
      REAL SELC_AGPDTHK                                                   30
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           30
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       30
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          30
     *MSELC_AGPDTHK)                                                      30
      CHARACTER CSELC_AGPDTHK*40/' '/                                     30
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             30
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       30
      REAL RE_SELC_AGPDTHK                                                30
C -                                                                       30
      REAL SELC_GLASSTHK                                                  30
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         30
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     30
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       30
     *MSELC_GLASSTHK)                                                     30
      CHARACTER CSELC_GLASSTHK*40/' '/                                    30
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           30
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      30
      REAL RE_SELC_GLASSTHK                                               30
C -                                                                       30
      REAL SELC_CABTHK                                                    30
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             30
     *(NAMES_SELC(11),NSELC_CABTHK)                                       30
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         30
     *MSELC_CABTHK)                                                       30
      CHARACTER CSELC_CABTHK*40/' '/                                      30
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               30
      INTEGER MSELC_CABTHK(2)/1,1/                                        30
      REAL RE_SELC_CABTHK                                                 30
C -                                                                       30
      REAL SELC_CABWID                                                    30
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             30
     *(NAMES_SELC(12),NSELC_CABWID)                                       30
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         30
     *MSELC_CABWID)                                                       30
      CHARACTER CSELC_CABWID*40/' '/                                      30
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               30
      INTEGER MSELC_CABWID(2)/1,1/                                        30
      REAL RE_SELC_CABWID                                                 30
C -                                                                       30
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                30
      PARAMETER (LEN_SELC=14)                                             30
      PARAMETER (LLEN_SELC=12)                                            30
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           30
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        30
      DATA IDNUM_SELC/10*0/                                               30
      REAL BANK_SELC(LEN_SELC)                                            30
      CHARACTER*60 BPATH_SELC/' '/                                        30
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          30
     *BPATH_SELC                                                          30
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             30
C *                                                                       30
C -                                                                       32
      INTEGER SVTL_LAYER                                                  32
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                32
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         32
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            32
     *MSVTL_LAYER)                                                        32
      CHARACTER CSVTL_LAYER*40/' '/                                       32
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 32
      INTEGER MSVTL_LAYER(2)/1,1/                                         32
      REAL RE_SVTL_LAYER                                                  32
C -                                                                       32
      REAL SVTL_NLADDER                                                   32
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            32
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       32
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          32
     *MSVTL_NLADDER)                                                      32
      CHARACTER CSVTL_NLADDER*40/' '/                                     32
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             32
      INTEGER MSVTL_NLADDER(2)/1,1/                                       32
      REAL RE_SVTL_NLADDER                                                32
C -                                                                       32
      REAL SVTL_NWAFER                                                    32
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              32
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        32
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           32
     *MSVTL_NWAFER)                                                       32
      CHARACTER CSVTL_NWAFER*40/' '/                                      32
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               32
      INTEGER MSVTL_NWAFER(2)/1,1/                                        32
      REAL RE_SVTL_NWAFER                                                 32
C -                                                                       32
      REAL SVTL_RADIUS                                                    32
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              32
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        32
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           32
     *MSVTL_RADIUS)                                                       32
      CHARACTER CSVTL_RADIUS*40/' '/                                      32
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               32
      INTEGER MSVTL_RADIUS(2)/1,1/                                        32
      REAL RE_SVTL_RADIUS                                                 32
C -                                                                       32
      REAL SVTL_BAREEDGE                                                  32
      EQUIVALENCE (BANK_SVTL(7),SVTL_BAREEDGE,RE_SVTL_BAREEDGE),          32
     *(NAMES_SVTL(5),NSVTL_BAREEDGE)                                      32
      EQUIVALENCE (COMMEN_SVTL(5),CSVTL_BAREEDGE),(MAP_SVTL(1,5),         32
     *MSVTL_BAREEDGE)                                                     32
      CHARACTER CSVTL_BAREEDGE*40/' '/                                    32
      CHARACTER NSVTL_BAREEDGE*12/'R.BAREEDGE'/                           32
      INTEGER MSVTL_BAREEDGE(2)/1,1/                                      32
      REAL RE_SVTL_BAREEDGE                                               32
C -                                                                       32
      REAL SVTL_PCBLEN                                                    32
      EQUIVALENCE (BANK_SVTL(8),SVTL_PCBLEN,RE_SVTL_PCBLEN),              32
     *(NAMES_SVTL(6),NSVTL_PCBLEN)                                        32
      EQUIVALENCE (COMMEN_SVTL(6),CSVTL_PCBLEN),(MAP_SVTL(1,6),           32
     *MSVTL_PCBLEN)                                                       32
      CHARACTER CSVTL_PCBLEN*40/' '/                                      32
      CHARACTER NSVTL_PCBLEN*12/'R.PCBLEN'/                               32
      INTEGER MSVTL_PCBLEN(2)/1,1/                                        32
      REAL RE_SVTL_PCBLEN                                                 32
C -                                                                       32
      REAL SVTL_PCBWIDTH                                                  32
      EQUIVALENCE (BANK_SVTL(9),SVTL_PCBWIDTH,RE_SVTL_PCBWIDTH),          32
     *(NAMES_SVTL(7),NSVTL_PCBWIDTH)                                      32
      EQUIVALENCE (COMMEN_SVTL(7),CSVTL_PCBWIDTH),(MAP_SVTL(1,7),         32
     *MSVTL_PCBWIDTH)                                                     32
      CHARACTER CSVTL_PCBWIDTH*40/' '/                                    32
      CHARACTER NSVTL_PCBWIDTH*12/'R.PCBWIDTH'/                           32
      INTEGER MSVTL_PCBWIDTH(2)/1,1/                                      32
      REAL RE_SVTL_PCBWIDTH                                               32
C -                                                                       32
      REAL SVTL_PCBTHK                                                    32
      EQUIVALENCE (BANK_SVTL(10),SVTL_PCBTHK,RE_SVTL_PCBTHK),             32
     *(NAMES_SVTL(8),NSVTL_PCBTHK)                                        32
      EQUIVALENCE (COMMEN_SVTL(8),CSVTL_PCBTHK),(MAP_SVTL(1,8),           32
     *MSVTL_PCBTHK)                                                       32
      CHARACTER CSVTL_PCBTHK*40/' '/                                      32
      CHARACTER NSVTL_PCBTHK*12/'R.PCBTHK'/                               32
      INTEGER MSVTL_PCBTHK(2)/1,1/                                        32
      REAL RE_SVTL_PCBTHK                                                 32
C -                                                                       32
      REAL SVTL_PCBGAP                                                    32
      EQUIVALENCE (BANK_SVTL(11),SVTL_PCBGAP,RE_SVTL_PCBGAP),             32
     *(NAMES_SVTL(9),NSVTL_PCBGAP)                                        32
      EQUIVALENCE (COMMEN_SVTL(9),CSVTL_PCBGAP),(MAP_SVTL(1,9),           32
     *MSVTL_PCBGAP)                                                       32
      CHARACTER CSVTL_PCBGAP*40/' '/                                      32
      CHARACTER NSVTL_PCBGAP*12/'R.PCBGAP'/                               32
      INTEGER MSVTL_PCBGAP(2)/1,1/                                        32
      REAL RE_SVTL_PCBGAP                                                 32
C -                                                                       32
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                32
      PARAMETER (LEN_SVTL=11)                                             32
      PARAMETER (LLEN_SVTL=9)                                             32
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           32
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        32
      DATA IDNUM_SVTL/10*0/                                               32
      REAL BANK_SVTL(LEN_SVTL)                                            32
      CHARACTER*60 BPATH_SVTL/' '/                                        32
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          32
     *BPATH_SVTL                                                          32
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             32
C *                                                                       32
C -                                                                       34
      REAL SSLD_VERSION                                                   34
      EQUIVALENCE (BANK_SSLD(3),SSLD_VERSION,RE_SSLD_VERSION),            34
     *(NAMES_SSLD(1),NSSLD_VERSION)                                       34
      EQUIVALENCE (COMMEN_SSLD(1),CSSLD_VERSION),(MAP_SSLD(1,1),          34
     *MSSLD_VERSION)                                                      34
      CHARACTER CSSLD_VERSION*40/' '/                                     34
      CHARACTER NSSLD_VERSION*12/'R.VERSION'/                             34
      INTEGER MSSLD_VERSION(2)/1,1/                                       34
      REAL RE_SSLD_VERSION                                                34
C -                                                                       34
      REAL SSLD_SINRINN                                                   34
      EQUIVALENCE (BANK_SSLD(4),SSLD_SINRINN,RE_SSLD_SINRINN),            34
     *(NAMES_SSLD(2),NSSLD_SINRINN)                                       34
      EQUIVALENCE (COMMEN_SSLD(2),CSSLD_SINRINN),(MAP_SSLD(1,2),          34
     *MSSLD_SINRINN)                                                      34
      CHARACTER CSSLD_SINRINN*40/' '/                                     34
      CHARACTER NSSLD_SINRINN*12/'R.SINRINN'/                             34
      INTEGER MSSLD_SINRINN(2)/1,1/                                       34
      REAL RE_SSLD_SINRINN                                                34
C -                                                                       34
      REAL SSLD_SINROUT                                                   34
      EQUIVALENCE (BANK_SSLD(5),SSLD_SINROUT,RE_SSLD_SINROUT),            34
     *(NAMES_SSLD(3),NSSLD_SINROUT)                                       34
      EQUIVALENCE (COMMEN_SSLD(3),CSSLD_SINROUT),(MAP_SSLD(1,3),          34
     *MSSLD_SINROUT)                                                      34
      CHARACTER CSSLD_SINROUT*40/' '/                                     34
      CHARACTER NSSLD_SINROUT*12/'R.SINROUT'/                             34
      INTEGER MSSLD_SINROUT(2)/1,1/                                       34
      REAL RE_SSLD_SINROUT                                                34
C -                                                                       34
      REAL SSLD_SINLEN                                                    34
      EQUIVALENCE (BANK_SSLD(6),SSLD_SINLEN,RE_SSLD_SINLEN),              34
     *(NAMES_SSLD(4),NSSLD_SINLEN)                                        34
      EQUIVALENCE (COMMEN_SSLD(4),CSSLD_SINLEN),(MAP_SSLD(1,4),           34
     *MSSLD_SINLEN)                                                       34
      CHARACTER CSSLD_SINLEN*40/' '/                                      34
      CHARACTER NSSLD_SINLEN*12/'R.SINLEN'/                               34
      INTEGER MSSLD_SINLEN(2)/1,1/                                        34
      REAL RE_SSLD_SINLEN                                                 34
C -                                                                       34
      REAL SSLD_SSEPRINN                                                  34
      EQUIVALENCE (BANK_SSLD(7),SSLD_SSEPRINN,RE_SSLD_SSEPRINN),          34
     *(NAMES_SSLD(5),NSSLD_SSEPRINN)                                      34
      EQUIVALENCE (COMMEN_SSLD(5),CSSLD_SSEPRINN),(MAP_SSLD(1,5),         34
     *MSSLD_SSEPRINN)                                                     34
      CHARACTER CSSLD_SSEPRINN*40/' '/                                    34
      CHARACTER NSSLD_SSEPRINN*12/'R.SSEPRINN'/                           34
      INTEGER MSSLD_SSEPRINN(2)/1,1/                                      34
      REAL RE_SSLD_SSEPRINN                                               34
C -                                                                       34
      REAL SSLD_SSEPROUT                                                  34
      EQUIVALENCE (BANK_SSLD(8),SSLD_SSEPROUT,RE_SSLD_SSEPROUT),          34
     *(NAMES_SSLD(6),NSSLD_SSEPROUT)                                      34
      EQUIVALENCE (COMMEN_SSLD(6),CSSLD_SSEPROUT),(MAP_SSLD(1,6),         34
     *MSSLD_SSEPROUT)                                                     34
      CHARACTER CSSLD_SSEPROUT*40/' '/                                    34
      CHARACTER NSSLD_SSEPROUT*12/'R.SSEPROUT'/                           34
      INTEGER MSSLD_SSEPROUT(2)/1,1/                                      34
      REAL RE_SSLD_SSEPROUT                                               34
C -                                                                       34
      REAL SSLD_SSEPLEN                                                   34
      EQUIVALENCE (BANK_SSLD(9),SSLD_SSEPLEN,RE_SSLD_SSEPLEN),            34
     *(NAMES_SSLD(7),NSSLD_SSEPLEN)                                       34
      EQUIVALENCE (COMMEN_SSLD(7),CSSLD_SSEPLEN),(MAP_SSLD(1,7),          34
     *MSSLD_SSEPLEN)                                                      34
      CHARACTER CSSLD_SSEPLEN*40/' '/                                     34
      CHARACTER NSSLD_SSEPLEN*12/'R.SSEPLEN'/                             34
      INTEGER MSSLD_SSEPLEN(2)/1,1/                                       34
      REAL RE_SSLD_SSEPLEN                                                34
C -                                                                       34
      REAL SSLD_SOUTRINN                                                  34
      EQUIVALENCE (BANK_SSLD(10),SSLD_SOUTRINN,RE_SSLD_SOUTRINN),         34
     *(NAMES_SSLD(8),NSSLD_SOUTRINN)                                      34
      EQUIVALENCE (COMMEN_SSLD(8),CSSLD_SOUTRINN),(MAP_SSLD(1,8),         34
     *MSSLD_SOUTRINN)                                                     34
      CHARACTER CSSLD_SOUTRINN*40/' '/                                    34
      CHARACTER NSSLD_SOUTRINN*12/'R.SOUTRINN'/                           34
      INTEGER MSSLD_SOUTRINN(2)/1,1/                                      34
      REAL RE_SSLD_SOUTRINN                                               34
C -                                                                       34
      REAL SSLD_SOUTROUT                                                  34
      EQUIVALENCE (BANK_SSLD(11),SSLD_SOUTROUT,RE_SSLD_SOUTROUT),         34
     *(NAMES_SSLD(9),NSSLD_SOUTROUT)                                      34
      EQUIVALENCE (COMMEN_SSLD(9),CSSLD_SOUTROUT),(MAP_SSLD(1,9),         34
     *MSSLD_SOUTROUT)                                                     34
      CHARACTER CSSLD_SOUTROUT*40/' '/                                    34
      CHARACTER NSSLD_SOUTROUT*12/'R.SOUTROUT'/                           34
      INTEGER MSSLD_SOUTROUT(2)/1,1/                                      34
      REAL RE_SSLD_SOUTROUT                                               34
C -                                                                       34
      REAL SSLD_SOUTLEN                                                   34
      EQUIVALENCE (BANK_SSLD(12),SSLD_SOUTLEN,RE_SSLD_SOUTLEN),           34
     *(NAMES_SSLD(10),NSSLD_SOUTLEN)                                      34
      EQUIVALENCE (COMMEN_SSLD(10),CSSLD_SOUTLEN),(MAP_SSLD(1,10),        34
     *MSSLD_SOUTLEN)                                                      34
      CHARACTER CSSLD_SOUTLEN*40/' '/                                     34
      CHARACTER NSSLD_SOUTLEN*12/'R.SOUTLEN'/                             34
      INTEGER MSSLD_SOUTLEN(2)/1,1/                                       34
      REAL RE_SSLD_SOUTLEN                                                34
C -                                                                       34
      REAL SSLD_ALMESHID                                                  34
      EQUIVALENCE (BANK_SSLD(13),SSLD_ALMESHID,RE_SSLD_ALMESHID),         34
     *(NAMES_SSLD(11),NSSLD_ALMESHID)                                     34
      EQUIVALENCE (COMMEN_SSLD(11),CSSLD_ALMESHID),(MAP_SSLD(1,11),       34
     *MSSLD_ALMESHID)                                                     34
      CHARACTER CSSLD_ALMESHID*40/' '/                                    34
      CHARACTER NSSLD_ALMESHID*12/'R.ALMESHID'/                           34
      INTEGER MSSLD_ALMESHID(2)/1,1/                                      34
      REAL RE_SSLD_ALMESHID                                               34
C -                                                                       34
      REAL SSLD_ALMESHOD                                                  34
      EQUIVALENCE (BANK_SSLD(14),SSLD_ALMESHOD,RE_SSLD_ALMESHOD),         34
     *(NAMES_SSLD(12),NSSLD_ALMESHOD)                                     34
      EQUIVALENCE (COMMEN_SSLD(12),CSSLD_ALMESHOD),(MAP_SSLD(1,12),       34
     *MSSLD_ALMESHOD)                                                     34
      CHARACTER CSSLD_ALMESHOD*40/' '/                                    34
      CHARACTER NSSLD_ALMESHOD*12/'R.ALMESHOD'/                           34
      INTEGER MSSLD_ALMESHOD(2)/1,1/                                      34
      REAL RE_SSLD_ALMESHOD                                               34
C -                                                                       34
      REAL SSLD_ALMSHTHK                                                  34
      EQUIVALENCE (BANK_SSLD(15),SSLD_ALMSHTHK,RE_SSLD_ALMSHTHK),         34
     *(NAMES_SSLD(13),NSSLD_ALMSHTHK)                                     34
      EQUIVALENCE (COMMEN_SSLD(13),CSSLD_ALMSHTHK),(MAP_SSLD(1,13),       34
     *MSSLD_ALMSHTHK)                                                     34
      CHARACTER CSSLD_ALMSHTHK*40/' '/                                    34
      CHARACTER NSSLD_ALMSHTHK*12/'R.ALMSHTHK'/                           34
      INTEGER MSSLD_ALMSHTHK(2)/1,1/                                      34
      REAL RE_SSLD_ALMSHTHK                                               34
C -                                                                       34
      REAL SSLD_ALMSHPOS                                                  34
      EQUIVALENCE (BANK_SSLD(16),SSLD_ALMSHPOS,RE_SSLD_ALMSHPOS),         34
     *(NAMES_SSLD(14),NSSLD_ALMSHPOS)                                     34
      EQUIVALENCE (COMMEN_SSLD(14),CSSLD_ALMSHPOS),(MAP_SSLD(1,14),       34
     *MSSLD_ALMSHPOS)                                                     34
      CHARACTER CSSLD_ALMSHPOS*40/' '/                                    34
      CHARACTER NSSLD_ALMSHPOS*12/'R.ALMSHPOS'/                           34
      INTEGER MSSLD_ALMSHPOS(2)/1,1/                                      34
      REAL RE_SSLD_ALMSHPOS                                               34
C -                                                                       34
      INTEGER LEN_SSLD,LLEN_SSLD,LINK_SSLD                                34
      PARAMETER (LEN_SSLD=16)                                             34
      PARAMETER (LLEN_SSLD=14)                                            34
      INTEGER FLAG_SSLD/0/,MAP_SSLD(2,LLEN_SSLD),IDNUM_SSLD(10)           34
      CHARACTER NAMES_SSLD(LLEN_SSLD)*12,COMMEN_SSLD(LLEN_SSLD)*40        34
      DATA IDNUM_SSLD/10*0/                                               34
      REAL BANK_SSLD(LEN_SSLD)                                            34
      CHARACTER*60 BPATH_SSLD/' '/                                        34
      SAVE FLAG_SSLD,MAP_SSLD,IDNUM_SSLD,NAMES_SSLD,COMMEN_SSLD,          34
     *BPATH_SSLD                                                          34
      DATA LINK_SSLD/0/,BANK_SSLD/LEN_SSLD*0/                             34
C *                                                                       34
C -                                                                       35
      INTEGER SCBP_LAYER                                                  35
      EQUIVALENCE (BANK_SCBP(3),SCBP_LAYER,RE_SCBP_LAYER),                35
     *(NAMES_SCBP(1),NSCBP_LAYER)                                         35
      EQUIVALENCE (COMMEN_SCBP(1),CSCBP_LAYER),(MAP_SCBP(1,1),            35
     *MSCBP_LAYER)                                                        35
      CHARACTER CSCBP_LAYER*40/' '/                                       35
      CHARACTER NSCBP_LAYER*12/'I.LAYER'/                                 35
      INTEGER MSCBP_LAYER(2)/1,1/                                         35
      REAL RE_SCBP_LAYER                                                  35
C -                                                                       35
      REAL SCBP_LEN                                                       35
      EQUIVALENCE (BANK_SCBP(4),SCBP_LEN,RE_SCBP_LEN),(NAMES_SCBP(2),     35
     *NSCBP_LEN)                                                          35
      EQUIVALENCE (COMMEN_SCBP(2),CSCBP_LEN),(MAP_SCBP(1,2),MSCBP_LEN)    35
      CHARACTER CSCBP_LEN*40/' '/                                         35
      CHARACTER NSCBP_LEN*12/'R.LEN'/                                     35
      INTEGER MSCBP_LEN(2)/1,1/                                           35
      REAL RE_SCBP_LEN                                                    35
C -                                                                       35
      REAL SCBP_RMIN1                                                     35
      EQUIVALENCE (BANK_SCBP(5),SCBP_RMIN1,RE_SCBP_RMIN1),                35
     *(NAMES_SCBP(3),NSCBP_RMIN1)                                         35
      EQUIVALENCE (COMMEN_SCBP(3),CSCBP_RMIN1),(MAP_SCBP(1,3),            35
     *MSCBP_RMIN1)                                                        35
      CHARACTER CSCBP_RMIN1*40/' '/                                       35
      CHARACTER NSCBP_RMIN1*12/'R.RMIN1'/                                 35
      INTEGER MSCBP_RMIN1(2)/1,1/                                         35
      REAL RE_SCBP_RMIN1                                                  35
C -                                                                       35
      REAL SCBP_RMAX1                                                     35
      EQUIVALENCE (BANK_SCBP(6),SCBP_RMAX1,RE_SCBP_RMAX1),                35
     *(NAMES_SCBP(4),NSCBP_RMAX1)                                         35
      EQUIVALENCE (COMMEN_SCBP(4),CSCBP_RMAX1),(MAP_SCBP(1,4),            35
     *MSCBP_RMAX1)                                                        35
      CHARACTER CSCBP_RMAX1*40/' '/                                       35
      CHARACTER NSCBP_RMAX1*12/'R.RMAX1'/                                 35
      INTEGER MSCBP_RMAX1(2)/1,1/                                         35
      REAL RE_SCBP_RMAX1                                                  35
C -                                                                       35
      REAL SCBP_RMIN2                                                     35
      EQUIVALENCE (BANK_SCBP(7),SCBP_RMIN2,RE_SCBP_RMIN2),                35
     *(NAMES_SCBP(5),NSCBP_RMIN2)                                         35
      EQUIVALENCE (COMMEN_SCBP(5),CSCBP_RMIN2),(MAP_SCBP(1,5),            35
     *MSCBP_RMIN2)                                                        35
      CHARACTER CSCBP_RMIN2*40/' '/                                       35
      CHARACTER NSCBP_RMIN2*12/'R.RMIN2'/                                 35
      INTEGER MSCBP_RMIN2(2)/1,1/                                         35
      REAL RE_SCBP_RMIN2                                                  35
C -                                                                       35
      REAL SCBP_RMAX2                                                     35
      EQUIVALENCE (BANK_SCBP(8),SCBP_RMAX2,RE_SCBP_RMAX2),                35
     *(NAMES_SCBP(6),NSCBP_RMAX2)                                         35
      EQUIVALENCE (COMMEN_SCBP(6),CSCBP_RMAX2),(MAP_SCBP(1,6),            35
     *MSCBP_RMAX2)                                                        35
      CHARACTER CSCBP_RMAX2*40/' '/                                       35
      CHARACTER NSCBP_RMAX2*12/'R.RMAX2'/                                 35
      INTEGER MSCBP_RMAX2(2)/1,1/                                         35
      REAL RE_SCBP_RMAX2                                                  35
C -                                                                       35
      REAL SCBP_VOL                                                       35
      EQUIVALENCE (BANK_SCBP(9),SCBP_VOL,RE_SCBP_VOL),(NAMES_SCBP(7),     35
     *NSCBP_VOL)                                                          35
      EQUIVALENCE (COMMEN_SCBP(7),CSCBP_VOL),(MAP_SCBP(1,7),MSCBP_VOL)    35
      CHARACTER CSCBP_VOL*40/' '/                                         35
      CHARACTER NSCBP_VOL*12/'R.VOL'/                                     35
      INTEGER MSCBP_VOL(2)/1,1/                                           35
      REAL RE_SCBP_VOL                                                    35
C -                                                                       35
      INTEGER LEN_SCBP,LLEN_SCBP,LINK_SCBP                                35
      PARAMETER (LEN_SCBP=9)                                              35
      PARAMETER (LLEN_SCBP=7)                                             35
      INTEGER FLAG_SCBP/0/,MAP_SCBP(2,LLEN_SCBP),IDNUM_SCBP(10)           35
      CHARACTER NAMES_SCBP(LLEN_SCBP)*12,COMMEN_SCBP(LLEN_SCBP)*40        35
      DATA IDNUM_SCBP/10*0/                                               35
      REAL BANK_SCBP(LEN_SCBP)                                            35
      CHARACTER*60 BPATH_SCBP/' '/                                        35
      SAVE FLAG_SCBP,MAP_SCBP,IDNUM_SCBP,NAMES_SCBP,COMMEN_SCBP,          35
     *BPATH_SCBP                                                          35
      DATA LINK_SCBP/0/,BANK_SCBP/LEN_SCBP*0/                             35
C *                                                                       35
C -                                                                       36
      INTEGER SFEP_LAYER                                                  36
      EQUIVALENCE (BANK_SFEP(3),SFEP_LAYER,RE_SFEP_LAYER),                36
     *(NAMES_SFEP(1),NSFEP_LAYER)                                         36
      EQUIVALENCE (COMMEN_SFEP(1),CSFEP_LAYER),(MAP_SFEP(1,1),            36
     *MSFEP_LAYER)                                                        36
      CHARACTER CSFEP_LAYER*40/' '/                                       36
      CHARACTER NSFEP_LAYER*12/'I.LAYER'/                                 36
      INTEGER MSFEP_LAYER(2)/1,1/                                         36
      REAL RE_SFEP_LAYER                                                  36
C -                                                                       36
      REAL SFEP_LEN                                                       36
      EQUIVALENCE (BANK_SFEP(4),SFEP_LEN,RE_SFEP_LEN),(NAMES_SFEP(2),     36
     *NSFEP_LEN)                                                          36
      EQUIVALENCE (COMMEN_SFEP(2),CSFEP_LEN),(MAP_SFEP(1,2),MSFEP_LEN)    36
      CHARACTER CSFEP_LEN*40/' '/                                         36
      CHARACTER NSFEP_LEN*12/'R.LEN'/                                     36
      INTEGER MSFEP_LEN(2)/1,1/                                           36
      REAL RE_SFEP_LEN                                                    36
C -                                                                       36
      REAL SFEP_RMIN1                                                     36
      EQUIVALENCE (BANK_SFEP(5),SFEP_RMIN1,RE_SFEP_RMIN1),                36
     *(NAMES_SFEP(3),NSFEP_RMIN1)                                         36
      EQUIVALENCE (COMMEN_SFEP(3),CSFEP_RMIN1),(MAP_SFEP(1,3),            36
     *MSFEP_RMIN1)                                                        36
      CHARACTER CSFEP_RMIN1*40/' '/                                       36
      CHARACTER NSFEP_RMIN1*12/'R.RMIN1'/                                 36
      INTEGER MSFEP_RMIN1(2)/1,1/                                         36
      REAL RE_SFEP_RMIN1                                                  36
C -                                                                       36
      REAL SFEP_RMAX1                                                     36
      EQUIVALENCE (BANK_SFEP(6),SFEP_RMAX1,RE_SFEP_RMAX1),                36
     *(NAMES_SFEP(4),NSFEP_RMAX1)                                         36
      EQUIVALENCE (COMMEN_SFEP(4),CSFEP_RMAX1),(MAP_SFEP(1,4),            36
     *MSFEP_RMAX1)                                                        36
      CHARACTER CSFEP_RMAX1*40/' '/                                       36
      CHARACTER NSFEP_RMAX1*12/'R.RMAX1'/                                 36
      INTEGER MSFEP_RMAX1(2)/1,1/                                         36
      REAL RE_SFEP_RMAX1                                                  36
C -                                                                       36
      REAL SFEP_RMIN2                                                     36
      EQUIVALENCE (BANK_SFEP(7),SFEP_RMIN2,RE_SFEP_RMIN2),                36
     *(NAMES_SFEP(5),NSFEP_RMIN2)                                         36
      EQUIVALENCE (COMMEN_SFEP(5),CSFEP_RMIN2),(MAP_SFEP(1,5),            36
     *MSFEP_RMIN2)                                                        36
      CHARACTER CSFEP_RMIN2*40/' '/                                       36
      CHARACTER NSFEP_RMIN2*12/'R.RMIN2'/                                 36
      INTEGER MSFEP_RMIN2(2)/1,1/                                         36
      REAL RE_SFEP_RMIN2                                                  36
C -                                                                       36
      REAL SFEP_RMAX2                                                     36
      EQUIVALENCE (BANK_SFEP(8),SFEP_RMAX2,RE_SFEP_RMAX2),                36
     *(NAMES_SFEP(6),NSFEP_RMAX2)                                         36
      EQUIVALENCE (COMMEN_SFEP(6),CSFEP_RMAX2),(MAP_SFEP(1,6),            36
     *MSFEP_RMAX2)                                                        36
      CHARACTER CSFEP_RMAX2*40/' '/                                       36
      CHARACTER NSFEP_RMAX2*12/'R.RMAX2'/                                 36
      INTEGER MSFEP_RMAX2(2)/1,1/                                         36
      REAL RE_SFEP_RMAX2                                                  36
C -                                                                       36
      REAL SFEP_VOL                                                       36
      EQUIVALENCE (BANK_SFEP(9),SFEP_VOL,RE_SFEP_VOL),(NAMES_SFEP(7),     36
     *NSFEP_VOL)                                                          36
      EQUIVALENCE (COMMEN_SFEP(7),CSFEP_VOL),(MAP_SFEP(1,7),MSFEP_VOL)    36
      CHARACTER CSFEP_VOL*40/' '/                                         36
      CHARACTER NSFEP_VOL*12/'R.VOL'/                                     36
      INTEGER MSFEP_VOL(2)/1,1/                                           36
      REAL RE_SFEP_VOL                                                    36
C -                                                                       36
      REAL SFEP_VOLPLAST                                                  36
      EQUIVALENCE (BANK_SFEP(10),SFEP_VOLPLAST,RE_SFEP_VOLPLAST),         36
     *(NAMES_SFEP(8),NSFEP_VOLPLAST)                                      36
      EQUIVALENCE (COMMEN_SFEP(8),CSFEP_VOLPLAST),(MAP_SFEP(1,8),         36
     *MSFEP_VOLPLAST)                                                     36
      CHARACTER CSFEP_VOLPLAST*40/' '/                                    36
      CHARACTER NSFEP_VOLPLAST*12/'R.VOLPLAST'/                           36
      INTEGER MSFEP_VOLPLAST(2)/1,1/                                      36
      REAL RE_SFEP_VOLPLAST                                               36
C -                                                                       36
      INTEGER LEN_SFEP,LLEN_SFEP,LINK_SFEP                                36
      PARAMETER (LEN_SFEP=10)                                             36
      PARAMETER (LLEN_SFEP=8)                                             36
      INTEGER FLAG_SFEP/0/,MAP_SFEP(2,LLEN_SFEP),IDNUM_SFEP(10)           36
      CHARACTER NAMES_SFEP(LLEN_SFEP)*12,COMMEN_SFEP(LLEN_SFEP)*40        36
      DATA IDNUM_SFEP/10*0/                                               36
      REAL BANK_SFEP(LEN_SFEP)                                            36
      CHARACTER*60 BPATH_SFEP/' '/                                        36
      SAVE FLAG_SFEP,MAP_SFEP,IDNUM_SFEP,NAMES_SFEP,COMMEN_SFEP,          36
     *BPATH_SFEP                                                          36
      DATA LINK_SFEP/0/,BANK_SFEP/LEN_SFEP*0/                             36
C *                                                                       36
C -                                                                       37
      INTEGER SWCX_LAYER                                                  37
      EQUIVALENCE (BANK_SWCX(3),SWCX_LAYER,RE_SWCX_LAYER),                37
     *(NAMES_SWCX(1),NSWCX_LAYER)                                         37
      EQUIVALENCE (COMMEN_SWCX(1),CSWCX_LAYER),(MAP_SWCX(1,1),            37
     *MSWCX_LAYER)                                                        37
      CHARACTER CSWCX_LAYER*40/' '/                                       37
      CHARACTER NSWCX_LAYER*12/'I.LAYER'/                                 37
      INTEGER MSWCX_LAYER(2)/1,1/                                         37
      REAL RE_SWCX_LAYER                                                  37
C -                                                                       37
      REAL SWCX_LENGTH                                                    37
      EQUIVALENCE (BANK_SWCX(4),SWCX_LENGTH,RE_SWCX_LENGTH),              37
     *(NAMES_SWCX(2),NSWCX_LENGTH)                                        37
      EQUIVALENCE (COMMEN_SWCX(2),CSWCX_LENGTH),(MAP_SWCX(1,2),           37
     *MSWCX_LENGTH)                                                       37
      CHARACTER CSWCX_LENGTH*40/' '/                                      37
      CHARACTER NSWCX_LENGTH*12/'R.LENGTH'/                               37
      INTEGER MSWCX_LENGTH(2)/1,1/                                        37
      REAL RE_SWCX_LENGTH                                                 37
C -                                                                       37
      REAL SWCX_DR                                                        37
      EQUIVALENCE (BANK_SWCX(5),SWCX_DR,RE_SWCX_DR),(NAMES_SWCX(3),       37
     *NSWCX_DR)                                                           37
      EQUIVALENCE (COMMEN_SWCX(3),CSWCX_DR),(MAP_SWCX(1,3),MSWCX_DR)      37
      CHARACTER CSWCX_DR*40/' '/                                          37
      CHARACTER NSWCX_DR*12/'R.DR'/                                       37
      INTEGER MSWCX_DR(2)/1,1/                                            37
      REAL RE_SWCX_DR                                                     37
C -                                                                       37
      REAL SWCX_OFFSET                                                    37
      EQUIVALENCE (BANK_SWCX(6),SWCX_OFFSET,RE_SWCX_OFFSET),              37
     *(NAMES_SWCX(4),NSWCX_OFFSET)                                        37
      EQUIVALENCE (COMMEN_SWCX(4),CSWCX_OFFSET),(MAP_SWCX(1,4),           37
     *MSWCX_OFFSET)                                                       37
      CHARACTER CSWCX_OFFSET*40/' '/                                      37
      CHARACTER NSWCX_OFFSET*12/'R.OFFSET'/                               37
      INTEGER MSWCX_OFFSET(2)/1,1/                                        37
      REAL RE_SWCX_OFFSET                                                 37
C -                                                                       37
      REAL SWCX_RAD                                                       37
      EQUIVALENCE (BANK_SWCX(7),SWCX_RAD,RE_SWCX_RAD),(NAMES_SWCX(5),     37
     *NSWCX_RAD)                                                          37
      EQUIVALENCE (COMMEN_SWCX(5),CSWCX_RAD),(MAP_SWCX(1,5),MSWCX_RAD)    37
      CHARACTER CSWCX_RAD*40/' '/                                         37
      CHARACTER NSWCX_RAD*12/'R.RAD'/                                     37
      INTEGER MSWCX_RAD(2)/1,1/                                           37
      REAL RE_SWCX_RAD                                                    37
C -                                                                       37
      REAL SWCX_WALL                                                      37
      EQUIVALENCE (BANK_SWCX(8),SWCX_WALL,RE_SWCX_WALL),(NAMES_SWCX(6),   37
     *NSWCX_WALL)                                                         37
      EQUIVALENCE (COMMEN_SWCX(6),CSWCX_WALL),(MAP_SWCX(1,6),             37
     *MSWCX_WALL)                                                         37
      CHARACTER CSWCX_WALL*40/' '/                                        37
      CHARACTER NSWCX_WALL*12/'R.WALL'/                                   37
      INTEGER MSWCX_WALL(2)/1,1/                                          37
      REAL RE_SWCX_WALL                                                   37
C -                                                                       37
      REAL SWCX_ROFFSET                                                   37
      EQUIVALENCE (BANK_SWCX(9),SWCX_ROFFSET,RE_SWCX_ROFFSET),            37
     *(NAMES_SWCX(7),NSWCX_ROFFSET)                                       37
      EQUIVALENCE (COMMEN_SWCX(7),CSWCX_ROFFSET),(MAP_SWCX(1,7),          37
     *MSWCX_ROFFSET)                                                      37
      CHARACTER CSWCX_ROFFSET*40/' '/                                     37
      CHARACTER NSWCX_ROFFSET*12/'R.ROFFSET'/                             37
      INTEGER MSWCX_ROFFSET(2)/1,1/                                       37
      REAL RE_SWCX_ROFFSET                                                37
C -                                                                       37
      INTEGER LEN_SWCX,LLEN_SWCX,LINK_SWCX                                37
      PARAMETER (LEN_SWCX=9)                                              37
      PARAMETER (LLEN_SWCX=7)                                             37
      INTEGER FLAG_SWCX/0/,MAP_SWCX(2,LLEN_SWCX),IDNUM_SWCX(10)           37
      CHARACTER NAMES_SWCX(LLEN_SWCX)*12,COMMEN_SWCX(LLEN_SWCX)*40        37
      DATA IDNUM_SWCX/10*0/                                               37
      REAL BANK_SWCX(LEN_SWCX)                                            37
      CHARACTER*60 BPATH_SWCX/' '/                                        37
      SAVE FLAG_SWCX,MAP_SWCX,IDNUM_SWCX,NAMES_SWCX,COMMEN_SWCX,          37
     *BPATH_SWCX                                                          37
      DATA LINK_SWCX/0/,BANK_SWCX/LEN_SWCX*0/                             37
C *                                                                       37
C -                                                                       38
      REAL SOUP_VERSION                                                   38
      EQUIVALENCE (BANK_SOUP(3),SOUP_VERSION,RE_SOUP_VERSION),            38
     *(NAMES_SOUP(1),NSOUP_VERSION)                                       38
      EQUIVALENCE (COMMEN_SOUP(1),CSOUP_VERSION),(MAP_SOUP(1,1),          38
     *MSOUP_VERSION)                                                      38
      CHARACTER CSOUP_VERSION*40/' '/                                     38
      CHARACTER NSOUP_VERSION*12/'R.VERSION'/                             38
      INTEGER MSOUP_VERSION(2)/1,1/                                       38
      REAL RE_SOUP_VERSION                                                38
C -                                                                       38
      REAL SOUP_LENGTH                                                    38
      EQUIVALENCE (BANK_SOUP(4),SOUP_LENGTH,RE_SOUP_LENGTH),              38
     *(NAMES_SOUP(2),NSOUP_LENGTH)                                        38
      EQUIVALENCE (COMMEN_SOUP(2),CSOUP_LENGTH),(MAP_SOUP(1,2),           38
     *MSOUP_LENGTH)                                                       38
      CHARACTER CSOUP_LENGTH*40/' '/                                      38
      CHARACTER NSOUP_LENGTH*12/'R.LENGTH'/                               38
      INTEGER MSOUP_LENGTH(2)/1,1/                                        38
      REAL RE_SOUP_LENGTH                                                 38
C -                                                                       38
      REAL SOUP_ROUT                                                      38
      EQUIVALENCE (BANK_SOUP(5),SOUP_ROUT,RE_SOUP_ROUT),(NAMES_SOUP(3),   38
     *NSOUP_ROUT)                                                         38
      EQUIVALENCE (COMMEN_SOUP(3),CSOUP_ROUT),(MAP_SOUP(1,3),             38
     *MSOUP_ROUT)                                                         38
      CHARACTER CSOUP_ROUT*40/' '/                                        38
      CHARACTER NSOUP_ROUT*12/'R.ROUT'/                                   38
      INTEGER MSOUP_ROUT(2)/1,1/                                          38
      REAL RE_SOUP_ROUT                                                   38
C -                                                                       38
      REAL SOUP_DR                                                        38
      EQUIVALENCE (BANK_SOUP(6),SOUP_DR,RE_SOUP_DR),(NAMES_SOUP(4),       38
     *NSOUP_DR)                                                           38
      EQUIVALENCE (COMMEN_SOUP(4),CSOUP_DR),(MAP_SOUP(1,4),MSOUP_DR)      38
      CHARACTER CSOUP_DR*40/' '/                                          38
      CHARACTER NSOUP_DR*12/'R.DR'/                                       38
      INTEGER MSOUP_DR(2)/1,1/                                            38
      REAL RE_SOUP_DR                                                     38
C -                                                                       38
      REAL SOUP_PHI1                                                      38
      EQUIVALENCE (BANK_SOUP(7),SOUP_PHI1,RE_SOUP_PHI1),(NAMES_SOUP(5),   38
     *NSOUP_PHI1)                                                         38
      EQUIVALENCE (COMMEN_SOUP(5),CSOUP_PHI1),(MAP_SOUP(1,5),             38
     *MSOUP_PHI1)                                                         38
      CHARACTER CSOUP_PHI1*40/' '/                                        38
      CHARACTER NSOUP_PHI1*12/'R.PHI1'/                                   38
      INTEGER MSOUP_PHI1(2)/1,1/                                          38
      REAL RE_SOUP_PHI1                                                   38
C -                                                                       38
      REAL SOUP_PHI2                                                      38
      EQUIVALENCE (BANK_SOUP(8),SOUP_PHI2,RE_SOUP_PHI2),(NAMES_SOUP(6),   38
     *NSOUP_PHI2)                                                         38
      EQUIVALENCE (COMMEN_SOUP(6),CSOUP_PHI2),(MAP_SOUP(1,6),             38
     *MSOUP_PHI2)                                                         38
      CHARACTER CSOUP_PHI2*40/' '/                                        38
      CHARACTER NSOUP_PHI2*12/'R.PHI2'/                                   38
      INTEGER MSOUP_PHI2(2)/1,1/                                          38
      REAL RE_SOUP_PHI2                                                   38
C -                                                                       38
      REAL SOUP_DIAMOUT                                                   38
      EQUIVALENCE (BANK_SOUP(9),SOUP_DIAMOUT,RE_SOUP_DIAMOUT),            38
     *(NAMES_SOUP(7),NSOUP_DIAMOUT)                                       38
      EQUIVALENCE (COMMEN_SOUP(7),CSOUP_DIAMOUT),(MAP_SOUP(1,7),          38
     *MSOUP_DIAMOUT)                                                      38
      CHARACTER CSOUP_DIAMOUT*40/' '/                                     38
      CHARACTER NSOUP_DIAMOUT*12/'R.DIAMOUT'/                             38
      INTEGER MSOUP_DIAMOUT(2)/1,1/                                       38
      REAL RE_SOUP_DIAMOUT                                                38
C -                                                                       38
      REAL SOUP_DIAMIN                                                    38
      EQUIVALENCE (BANK_SOUP(10),SOUP_DIAMIN,RE_SOUP_DIAMIN),             38
     *(NAMES_SOUP(8),NSOUP_DIAMIN)                                        38
      EQUIVALENCE (COMMEN_SOUP(8),CSOUP_DIAMIN),(MAP_SOUP(1,8),           38
     *MSOUP_DIAMIN)                                                       38
      CHARACTER CSOUP_DIAMIN*40/' '/                                      38
      CHARACTER NSOUP_DIAMIN*12/'R.DIAMIN'/                               38
      INTEGER MSOUP_DIAMIN(2)/1,1/                                        38
      REAL RE_SOUP_DIAMIN                                                 38
C -                                                                       38
      INTEGER LEN_SOUP,LLEN_SOUP,LINK_SOUP                                38
      PARAMETER (LEN_SOUP=10)                                             38
      PARAMETER (LLEN_SOUP=8)                                             38
      INTEGER FLAG_SOUP/0/,MAP_SOUP(2,LLEN_SOUP),IDNUM_SOUP(10)           38
      CHARACTER NAMES_SOUP(LLEN_SOUP)*12,COMMEN_SOUP(LLEN_SOUP)*40        38
      DATA IDNUM_SOUP/10*0/                                               38
      REAL BANK_SOUP(LEN_SOUP)                                            38
      CHARACTER*60 BPATH_SOUP/' '/                                        38
      SAVE FLAG_SOUP,MAP_SOUP,IDNUM_SOUP,NAMES_SOUP,COMMEN_SOUP,          38
     *BPATH_SOUP                                                          38
      DATA LINK_SOUP/0/,BANK_SOUP/LEN_SOUP*0/                             38
C *                                                                       38
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 39
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         41
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           43
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          45
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    47
      REAL XBUF,PHI,XBUF1,XBUF2                                           49
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     51
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          53
     *SHIELD_PHI(4)                                                       53
      REAL TRAPY,SSIDX,SSIRY                                              55
      INTEGER I_PHI                                                       57
      RADII(1)= 6.37                                                      59
      RADII(2)= 7.38                                                      60
      RADII(3)=10.38                                                      61
      RADII(4)=11.27                                                      62
      RADII(5)=14.19                                                      63
      RADII(6)=15.13                                                      64
      RAD_OFFSET=0.015                                                    65
      SHIELD_PHI(1)=9.0                                                   66
      SHIELD_PHI(2)=27.0                                                  67
      SHIELD_PHI(3)=45.0                                                  68
      SHIELD_PHI(4)=63.0                                                  69
C *                                                                       70
      CALL AGSBEGM(MODULE,IPRIN)                                          70
      IF (IPRIN.LT.0) RETURN                                              70
C Filling bank SVTG - BASIC SVT DIMENSIONS                                70
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   70
      BPATH_SVTG='SVTG*'                                                  70
      IDNUM_SVTG(1)=0                                                     71
C GEOMETRY VERSION                                                        71
      SVTG_VERSION=2                                                      71
      CSVTG_VERSION='GEOMETRY VERSION'                                    71
C NUMBER OF SVT LAYERS (WAS 7)                                            72
      SVTG_NLAYER=7                                                       72
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         72
C STV INNERMOST RADIUS                                                    73
      SVTG_RSIZEMIN=4.100                                                 73
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               73
C STV OUTERMOST RADIUS                                                    74
      SVTG_RSIZEMAX=46.107                                                74
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               74
C SVT+FTPC LENGTH                                                         75
      SVTG_ZSIZEMAX=270                                                   75
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    75
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          76
      SVTG_ANGOFF=0                                                       76
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       76
C VERSIONING OF THE SHIELD                                                77
      SVTG_SUPPORTVER=1                                                   77
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         77
C VERSIONING OF THE SUPPORT CONE                                          78
      SVTG_CONEVER=1                                                      78
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      78
C WHETHER WE USE THE GEANT MANY OPTION                                    79
      SVTG_IFMANY=0                                                       79
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 79
C THE INDEX OF THE INNERMOST LAYER                                        80
      SVTG_NMIN=1                                                         80
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       80
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    81
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           81
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   81
C *                                                                       81
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    81
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           81
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   81
C *                                                                       82
      CALL AGSBEGM(MODULE,IPRIN)                                          82
      IF (IPRIN.LT.0) RETURN                                              82
C Filling bank SWCA - WAFER CARRIER                                       82
      BANK_TITLE='WAFER CARRIER'                                          82
      BPATH_SWCA='SWCA*'                                                  82
      IDNUM_SWCA(1)=0                                                     83
C GEOMETRY VERSION                                                        83
      SWCA_VERSION=1                                                      83
      CSWCA_VERSION='GEOMETRY VERSION'                                    83
C LADDER LENGTH                                                           84
      SWCA_LENGTH=56.04                                                   84
      CSWCA_LENGTH='LADDER LENGTH'                                        84
C WAFER WIDTH                                                             85
      SWCA_WAFERWID=6.305                                                 85
      CSWCA_WAFERWID='WAFER WIDTH'                                        85
C WAFER LENGTH                                                            86
      SWCA_WAFERLEN=6.305                                                 86
      CSWCA_WAFERLEN='WAFER LENGTH'                                       86
C WAFER THICKNESS                                                         87
      SWCA_WAFERTHK=0.0300                                                87
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    87
C ROHA CELL PLUS GLUE THICKNESS                                           88
      SWCA_ROHATHK=0.0381                                                 88
      CSWCA_ROHATHK='ROHA CELL PLUS GLUE THICKNESS'                       88
C WAFER CARRIER RAILS WIDTH                                               89
      SWCA_WAFCARWD=1.5                                                   89
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          89
C WAFER CARRIER THICKNESS                                                 90
      SWCA_WAFCARTH=0.0300                                                90
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            90
C INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY                      91
      SWCA_WAFERGAP=0.0                                                   91
      CSWCA_WAFERGAP='INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY  91
     *'                                                                   91
C DRIFT DIRECTION                                                         92
      SWCA_DRIFT=1                                                        92
      CSWCA_DRIFT='DRIFT DIRECTION'                                       92
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        93
      SWCA_STRUTLEN=1.0                                                   93
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   93
C DEFINE THE SENSITIVE SQUARE WIDTH (NEW)                                 94
      SWCA_SENSWID=6.000                                                  94
      CSWCA_SENSWID='DEFINE THE SENSITIVE SQUARE WIDTH (NEW)'             94
C DEFINE THE SENSITIVE SQUARE LENGTH (NEW)                                95
      SWCA_SENSLEN=6.000                                                  95
      CSWCA_SENSLEN='DEFINE THE SENSITIVE SQUARE LENGTH (NEW)'            95
C GIRTH (FOR LACK OF A BETTER WORD)                                       96
      SWCA_SENSGIR=5.775                                                  96
      CSWCA_SENSGIR='GIRTH (FOR LACK OF A BETTER WORD)'                   96
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    97
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           97
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   97
C *                                                                       97
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    97
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           97
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   97
C *                                                                       98
      CALL AGSBEGM(MODULE,IPRIN)                                          98
      IF (IPRIN.LT.0) RETURN                                              98
C Filling bank SSUP - SUPPORT STRUCTURES                                  98
      BANK_TITLE='SUPPORT STRUCTURES'                                     98
      BPATH_SSUP='SSUP*'                                                  98
      IDNUM_SSUP(1)=0                                                     99
C GEOMETRY VERSION                                                        99
      SSUP_VERSION=1                                                      99
      CSSUP_VERSION='GEOMETRY VERSION'                                    99
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            100
      SSUP_CABTHK=0.05                                                    100
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         100
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             101
      SSUP_HOSRMN=0.75                                                    101
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          101
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             102
      SSUP_HOSRMX=0.95                                                    102
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          102
C NUMBER OF WATER HOSES                                                   103
      SSUP_NHOSES=10                                                      103
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                103
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             104
      SSUP_WRPMYTHK=0.10                                                  104
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        104
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   105
      SSUP_WRPALTHK=0.01                                                  105
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              105
C SUPPORT CONE THICKNESS                                                  106
      SSUP_GRPHTHK=0.16                                                   106
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              106
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              107
      SSUP_CONE1ZMN=52.23                                                 107
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         107
C LENGTH OF SUPPORT RODS                                                  108
      SSUP_RODLEN=110.8                                                   108
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               108
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               109
      SSUP_RODDIST=17.5                                                   109
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           109
C ID OF CARBON SUPPORT RODS (APPROX)                                      110
      SSUP_RODID=2.5                                                      110
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    110
C OD OF CARBON SUPPORT RODS (APPROX)                                      111
      SSUP_RODOD=3.05                                                     111
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    111
C MINIMUM ID OF CONE 1                                                    112
      SSUP_CON1IDMN=15.67                                                 112
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               112
C MINIMUM ID OF CONE 3 (TBD)                                              113
      SSUP_CON3IDMN=21.67                                                 113
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         113
C MINIMUM ID OF CONE 4 (TBD)                                              114
      SSUP_CON4IDMN=37.4                                                  114
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         114
C MAXIMUM ID OF CONE 4 (TBD)                                              115
      SSUP_CON4IDMX=37.4                                                  115
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         115
C MAXIMUM Z OF CONE 3 (TBD)                                               116
      SSUP_CONE3ZMX=150.0                                                 116
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          116
C MAXIMUM Z OF CONE 4 (TBD)                                               117
      SSUP_CONE4ZMX=229.36                                                117
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          117
C THICKNESS OF AL BRACKETS                                                118
      SSUP_BRATHK=.2                                                      118
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             118
C (Z) THICKNESS OF END RING JOINING BRACKETS                              119
      SSUP_ERJTHK=.1                                                      119
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           119
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          120
      SSUP_ERJWID=2.07                                                    120
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       120
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            121
      SSUP_ERJLEN=5.19                                                    121
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         121
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    122
      SSUP_ERJZDIS=2.0                                                    122
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                122
C ERJ SCREW 1 X POSITION (RADIAL)                                         123
      SSUP_ERJ1X=0.31                                                     123
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       123
C ERJ SCREW 2 X POSITION                                                  124
      SSUP_ERJ2X=1.15                                                     124
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                124
C ERJ SCREW 2 Y POSITION                                                  125
      SSUP_ERJ2Y=0.72                                                     125
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                125
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   126
      SSUP_ERJRAD=10.80                                                   126
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                126
C ERJ SCREW DIAMETER                                                      127
      SSUP_ERJDIA=0.17                                                    127
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   127
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    128
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           128
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   128
C *                                                                       128
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    128
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           128
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   128
C *                                                                       129
      CALL AGSBEGM(MODULE,IPRIN)                                          129
      IF (IPRIN.LT.0) RETURN                                              129
C Filling bank SSUP - SUPPORT STRUCTURES                                  129
      BANK_TITLE='SUPPORT STRUCTURES'                                     129
      BPATH_SSUP='SSUP*'                                                  129
      IDNUM_SSUP(1)=0                                                     130
C GEOMETRY VERSION                                                        130
      SSUP_VERSION=2                                                      130
      CSSUP_VERSION='GEOMETRY VERSION'                                    130
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            131
      SSUP_CABTHK=0.21                                                    131
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         131
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             132
      SSUP_HOSRMN=0.75                                                    132
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          132
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             133
      SSUP_HOSRMX=0.95                                                    133
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          133
C NUMBER OF WATER HOSES                                                   134
      SSUP_NHOSES=10                                                      134
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                134
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             135
      SSUP_WRPMYTHK=0.10                                                  135
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        135
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   136
      SSUP_WRPALTHK=0.01                                                  136
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              136
C SUPPORT CONE THICKNESS                                                  137
      SSUP_GRPHTHK=0.16                                                   137
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              137
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              138
      SSUP_CONE1ZMN=52.23                                                 138
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         138
C LENGTH OF SUPPORT RODS                                                  139
      SSUP_RODLEN=110.8                                                   139
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               139
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               140
      SSUP_RODDIST=17.5                                                   140
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           140
C ID OF CARBON SUPPORT RODS (APPROX)                                      141
      SSUP_RODID=2.5                                                      141
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    141
C OD OF CARBON SUPPORT RODS (APPROX)                                      142
      SSUP_RODOD=3.05                                                     142
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    142
C MINIMUM ID OF CONE 1                                                    143
      SSUP_CON1IDMN=15.67                                                 143
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               143
C MINIMUM ID OF CONE 3 (TBD)                                              144
      SSUP_CON3IDMN=21.67                                                 144
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         144
C MINIMUM ID OF CONE 4 (TBD)                                              145
      SSUP_CON4IDMN=37.4                                                  145
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         145
C MAXIMUM ID OF CONE 4 (TBD)                                              146
      SSUP_CON4IDMX=37.4                                                  146
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         146
C MAXIMUM Z OF CONE 3 (TBD)                                               147
      SSUP_CONE3ZMX=150.0                                                 147
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          147
C MAXIMUM Z OF CONE 4 (TBD)                                               148
      SSUP_CONE4ZMX=229.36                                                148
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          148
C THICKNESS OF AL BRACKETS                                                149
      SSUP_BRATHK=.2                                                      149
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             149
C (Z) THICKNESS OF END RING JOINING BRACKETS                              150
      SSUP_ERJTHK=.1                                                      150
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           150
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          151
      SSUP_ERJWID=2.07                                                    151
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       151
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            152
      SSUP_ERJLEN=5.19                                                    152
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         152
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    153
      SSUP_ERJZDIS=2.0                                                    153
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                153
C ERJ SCREW 1 X POSITION (RADIAL)                                         154
      SSUP_ERJ1X=0.31                                                     154
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       154
C ERJ SCREW 2 X POSITION                                                  155
      SSUP_ERJ2X=1.15                                                     155
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                155
C ERJ SCREW 2 Y POSITION                                                  156
      SSUP_ERJ2Y=0.72                                                     156
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                156
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   157
      SSUP_ERJRAD=10.80                                                   157
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                157
C ERJ SCREW DIAMETER                                                      158
      SSUP_ERJDIA=0.17                                                    158
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   158
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    159
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           159
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   159
C *                                                                       159
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    159
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           159
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   159
C *                                                                       160
      CALL AGSBEGM(MODULE,IPRIN)                                          160
      IF (IPRIN.LT.0) RETURN                                              160
C Filling bank SSUB - BEAMPIPE SUPPORT                                    160
      BANK_TITLE='BEAMPIPE SUPPORT'                                       160
      BPATH_SSUB='SSUB*'                                                  160
      IDNUM_SSUB(1)=0                                                     161
C GEOMETRY VERSION                                                        161
      SSUB_VERSION=1                                                      161
      CSSUB_VERSION='GEOMETRY VERSION'                                    161
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  162
      SSUB_KMOUNTID=31.34                                                 162
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             162
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  163
      SSUB_KMOUNTOD=38.96                                                 163
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             163
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    164
      SSUB_KMNTTHK=0.64                                                   164
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                164
C OD OF CUTOUT IN KINEMATIC MOUNT                                         165
      SSUB_KMCUTOD=18.31                                                  165
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     165
C ID OF CUTOUT IN KINEMATIC MOUNT                                         166
      SSUB_KMCUTID=14                                                     166
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     166
C OPENING ANGLE OF CUTOUT                                                 167
      SSUB_KMCUTOA=38                                                     167
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             167
C OFFSET OF CUTOUT CENTER FROM AXIS                                       168
      SSUB_KMCUTOFF=26.58                                                 168
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  168
C ID OF BEAMPIPE SUPPORT RING                                             169
      SSUB_SRINGID=8.47                                                   169
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         169
C OD OF BEAMPIPE SUPPORT RING                                             170
      SSUB_SRINGOD=12.78                                                  170
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         170
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     171
      SSUB_SRINGTHK=0.64                                                  171
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                171
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     172
      SSUB_SRCUTPHI=38                                                    172
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                172
C SUPPORT RING CUTOUT WIDTH                                               173
      SSUB_SRCUTWID=3.63                                                  173
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          173
C SUPPORT RING CUTOUT DEPTH                                               174
      SSUB_SRCUTOUT=5.08                                                  174
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          174
C SUPPORT RING CUTOUT START                                               175
      SSUB_SRCUTIN=3.5                                                    175
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           175
C SUPPORT ROLLER ID                                                       176
      SSUB_SROLLID=0.2                                                    176
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   176
C SUPPORT ROLLER OD                                                       177
      SSUB_SROLLOD=0.62                                                   177
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   177
C SUPPORT ROLLER LENGTH                                                   178
      SSUB_SROLLLEN=2.54                                                  178
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              178
C SUPPORT ROLLER AXIS LENGTH                                              179
      SSUB_SWIRELEN=5.08                                                  179
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         179
C MOUNTING BLOCK HEIGHT                                                   180
      SSUB_MBLKHGH=0.97                                                   180
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               180
C OUTER MOUNTING BLOCK WIDTH                                              181
      SSUB_MBLKOWID=2.54                                                  181
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         181
C OUTER MOUNTING BLOCK LENGTH                                             182
      SSUB_MBLKOLEN=1.27                                                  182
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        182
C INNER MOUNTING BLOCK WIDTH                                              183
      SSUB_MBLKIWID=3.175                                                 183
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         183
C INNER MOUNTING BLOCK LENGTH                                             184
      SSUB_MBLKILEN=1.27                                                  184
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        184
C OUTER MOUNTING BLOCK AT RADIUS                                          185
      SSUB_MBLKORAD=17.4                                                  185
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     185
C INNER MOUNTING BLOCK AT RADIUS                                          186
      SSUB_MBLKIRAD=5.42                                                  186
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     186
C MOUNTING SUPPORT ROD DIAMETER                                           187
      SSUB_MRODDIA=0.32                                                   187
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       187
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    188
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           188
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   188
C *                                                                       188
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    188
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           188
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   188
C *                                                                       189
      CALL AGSBEGM(MODULE,IPRIN)                                          189
      IF (IPRIN.LT.0) RETURN                                              189
C Filling bank SERG - END RINGS                                           189
      BANK_TITLE='END RINGS'                                              189
      BPATH_SERG='SERG*'                                                  189
      IDNUM_SERG(1)=0                                                     190
C GEOMETRY VERSION                                                        190
      SERG_VERSION=1                                                      190
      CSERG_VERSION='GEOMETRY VERSION'                                    190
C INNER END RING TUBE MAXIMUM RADIUS                                      191
      SERG_IRNGTRMX=9.703                                                 191
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 191
C INNER END RING POLYGON MINIMUM RADIUS                                   192
      SERG_IRNGPRMN=7.671                                                 192
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              192
C OUTER END RING MINIMUM RADIUS                                           193
      SERG_ORNGRMIN=11.900                                                193
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      193
C OUTER END RING MAXIMUM RADIUS                                           194
      SERG_ORNGRMAX=13.805                                                194
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      194
C END RING THICKNESS                                                      195
      SERG_ENDRNGTH=0.2                                                   195
      CSERG_ENDRNGTH='END RING THICKNESS'                                 195
C MINIMUM Z FOR END RINGS                                                 196
      SERG_ENDRNGZM=23.01                                                 196
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            196
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    197
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           197
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   197
C *                                                                       197
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    197
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           197
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   197
C *                                                                       198
      CALL AGSBEGM(MODULE,IPRIN)                                          198
      IF (IPRIN.LT.0) RETURN                                              198
C Filling bank SWAM - WATER MANIFOLD                                      198
      BANK_TITLE='WATER MANIFOLD'                                         198
      BPATH_SWAM='SWAM*'                                                  198
      IDNUM_SWAM(1)=0                                                     199
C GEOMETRY VERSION                                                        199
      SWAM_VERSION=1                                                      199
      CSWAM_VERSION='GEOMETRY VERSION'                                    199
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                200
      SWAM_ZMIN=33.9                                                      200
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               200
C WATER MANIFOLD FULL LENGTH                                              201
      SWAM_LEN=16.0                                                       201
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              201
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         202
      SWAM_RMIN=15.24                                                     202
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        202
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         203
      SWAM_RMAX=16.83                                                     203
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        203
C TRANSITION BOARD THICKNESS                                              204
      SWAM_TBRDTHK=0.1                                                    204
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          204
C WATER MANIFOLD WALL THICKNESS                                           205
      SWAM_WALLTHK=0.1                                                    205
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       205
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    206
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           206
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   206
C *                                                                       206
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    206
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           206
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   206
C *                                                                       207
      CALL AGSBEGM(MODULE,IPRIN)                                          207
      IF (IPRIN.LT.0) RETURN                                              207
C Filling bank SELC - ELECTRONICS CARRIER DATA                            207
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               207
      BPATH_SELC='SELC*'                                                  207
      IDNUM_SELC(1)=0                                                     208
C GEOMETRY VERSION                                                        208
      SELC_VERSION=1                                                      208
      CSELC_VERSION='GEOMETRY VERSION'                                    208
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              209
      SELC_BETHK=0.0500                                                   209
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            209
C THICKNESS OF WATER CHANNEL                                              210
      SELC_WATTHK=0.0750                                                  210
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           210
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    211
      SELC_BEOTHK=0.0500                                                  211
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 211
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                212
      SELC_DYETHK=0.0340                                                  212
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             212
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                213
      SELC_DYEWID=0.3                                                     213
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             213
C SPACE IC CHIPS                                                          214
      SELC_DYESPC=0.1                                                     214
      CSELC_DYESPC='SPACE IC CHIPS'                                       214
C ELECTRONICS CARRIER WIDTH                                               215
      SELC_ELCAWID=2.0                                                    215
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           215
C THICKNESS OF AG-PD CONDUCTOR                                            216
      SELC_AGPDTHK=0.0030                                                 216
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        216
C THICKNESS OF GLASS INSULATOR                                            217
      SELC_GLASSTHK=0.0150                                                217
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       217
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        218
      SELC_CABTHK=0.0033                                                  218
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     218
C CABLE WIDTH                                                             219
      SELC_CABWID=0.6                                                     219
      CSELC_CABWID='CABLE WIDTH'                                          219
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    220
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           220
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   220
C *                                                                       220
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    220
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           220
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   220
C *                                                                       221
      CALL AGSBEGM(MODULE,IPRIN)                                          221
      IF (IPRIN.LT.0) RETURN                                              221
C Filling bank SVTL - SINGLE LAYER DATA                                   221
      BANK_TITLE='SINGLE LAYER DATA'                                      221
      BPATH_SVTL='SVTL*'                                                  221
      IDNUM_SVTL(1)=0                                                     222
C LAYER NUMBER                                                            222
      SVTL_LAYER=1                                                        222
      CSVTL_LAYER='LAYER NUMBER'                                          222
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 223
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     223
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              223
C NUMBER OF LADDER                                                        224
      SVTL_NLADDER=4                                                      224
      CSVTL_NLADDER='NUMBER OF LADDER'                                    224
C NUMBER OF WAFERS                                                        225
      SVTL_NWAFER=4                                                       225
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     225
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        226
      SVTL_BAREEDGE=1.0                                                   226
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   226
C PCB LENGTH                                                              227
      SVTL_PCBLEN=14.9                                                    227
      CSVTL_PCBLEN='PCB LENGTH'                                           227
C PCB WIDTH                                                               228
      SVTL_PCBWIDTH=6.3                                                   228
      CSVTL_PCBWIDTH='PCB WIDTH'                                          228
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            229
      SVTL_PCBTHK=0.1                                                     229
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  229
     *R COPPER'                                                           229
C GAP BETWEEN THE PCB AND THE SI WAFER                                    230
      SVTL_PCBGAP=0.2                                                     230
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 230
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    231
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           231
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   231
C *                                                                       231
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    231
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           231
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   231
C *                                                                       232
      CALL AGSBEGM(MODULE,IPRIN)                                          232
      IF (IPRIN.LT.0) RETURN                                              232
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             232
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                232
      BPATH_SVTL='SVTL*'                                                  232
      IDNUM_SVTL(1)=0                                                     233
C LAYER NUMBER                                                            233
      SVTL_LAYER=2                                                        233
      CSVTL_LAYER='LAYER NUMBER'                                          233
C LAYER RADIUS                                                            234
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     234
      CSVTL_RADIUS='LAYER RADIUS'                                         234
C PCB LENGTH                                                              235
      SVTL_PCBLEN=14.9                                                    235
      CSVTL_PCBLEN='PCB LENGTH'                                           235
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    236
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           236
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   236
C *                                                                       236
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    236
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           236
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   236
C *                                                                       237
      CALL AGSBEGM(MODULE,IPRIN)                                          237
      IF (IPRIN.LT.0) RETURN                                              237
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             237
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                237
      BPATH_SVTL='SVTL*'                                                  237
      IDNUM_SVTL(1)=0                                                     238
C LAYER NUMBER                                                            238
      SVTL_LAYER=3                                                        238
      CSVTL_LAYER='LAYER NUMBER'                                          238
C LAYER RADIUS                                                            239
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     239
      CSVTL_RADIUS='LAYER RADIUS'                                         239
C NUMBER OF LADDER                                                        240
      SVTL_NLADDER=6                                                      240
      CSVTL_NLADDER='NUMBER OF LADDER'                                    240
C NUMBER OF WAFERS                                                        241
      SVTL_NWAFER=6                                                       241
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     241
C PCB LENGTH                                                              242
      SVTL_PCBLEN=7.5                                                     242
      CSVTL_PCBLEN='PCB LENGTH'                                           242
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    243
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           243
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   243
C *                                                                       243
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    243
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           243
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   243
C *                                                                       244
      CALL AGSBEGM(MODULE,IPRIN)                                          244
      IF (IPRIN.LT.0) RETURN                                              244
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             244
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                244
      BPATH_SVTL='SVTL*'                                                  244
      IDNUM_SVTL(1)=0                                                     245
C LAYER NUMBER                                                            245
      SVTL_LAYER=4                                                        245
      CSVTL_LAYER='LAYER NUMBER'                                          245
C LAYER RADIUS                                                            246
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     246
      CSVTL_RADIUS='LAYER RADIUS'                                         246
C PCB LENGTH                                                              247
      SVTL_PCBLEN=7.5                                                     247
      CSVTL_PCBLEN='PCB LENGTH'                                           247
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    248
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           248
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   248
C *                                                                       248
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    248
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           248
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   248
C *                                                                       249
      CALL AGSBEGM(MODULE,IPRIN)                                          249
      IF (IPRIN.LT.0) RETURN                                              249
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             249
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                249
      BPATH_SVTL='SVTL*'                                                  249
      IDNUM_SVTL(1)=0                                                     250
C LAYER NUMBER                                                            250
      SVTL_LAYER=5                                                        250
      CSVTL_LAYER='LAYER NUMBER'                                          250
C LAYER RADIUS                                                            251
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     251
      CSVTL_RADIUS='LAYER RADIUS'                                         251
C NUMBER OF LADDER                                                        252
      SVTL_NLADDER=8                                                      252
      CSVTL_NLADDER='NUMBER OF LADDER'                                    252
C NUMBER OF WAFERS                                                        253
      SVTL_NWAFER=7                                                       253
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     253
C PCB LENGTH                                                              254
      SVTL_PCBLEN=4.4                                                     254
      CSVTL_PCBLEN='PCB LENGTH'                                           254
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    255
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           255
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   255
C *                                                                       255
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    255
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           255
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   255
C *                                                                       256
      CALL AGSBEGM(MODULE,IPRIN)                                          256
      IF (IPRIN.LT.0) RETURN                                              256
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             256
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                256
      BPATH_SVTL='SVTL*'                                                  256
      IDNUM_SVTL(1)=0                                                     257
C LAYER NUMBER                                                            257
      SVTL_LAYER=6                                                        257
      CSVTL_LAYER='LAYER NUMBER'                                          257
C LAYER RADIUS                                                            258
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     258
      CSVTL_RADIUS='LAYER RADIUS'                                         258
C PCB LENGTH                                                              259
      SVTL_PCBLEN=4.4                                                     259
      CSVTL_PCBLEN='PCB LENGTH'                                           259
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    260
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           260
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   260
C *                                                                       260
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    260
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           260
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   260
C *                                                                       261
      CALL AGSBEGM(MODULE,IPRIN)                                          261
      IF (IPRIN.LT.0) RETURN                                              261
C Filling bank SSLD - SHIELDING PARAMETERS                                261
      BANK_TITLE='SHIELDING PARAMETERS'                                   261
      BPATH_SSLD='SSLD*'                                                  261
      IDNUM_SSLD(1)=0                                                     262
C GEOMETRY VERSION                                                        262
      SSLD_VERSION=1                                                      262
      CSSLD_VERSION='GEOMETRY VERSION'                                    262
C INNER SHIELD CYLINDER, INNER RADIUS                                     263
      SSLD_SINRINN=5                                                      263
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 263
C INNER SHIELD CYLINDER, OUTER RADIUS                                     264
      SSLD_SINROUT=5.008                                                  264
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 264
C INNER SHIELD CYLINDER, HALF LENGTH                                      265
      SSLD_SINLEN=53.5                                                    265
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   265
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                266
      SSLD_SSEPRINN=22                                                    266
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           266
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                267
      SSLD_SSEPROUT=22.018                                                267
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           267
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 268
      SSLD_SSEPLEN=55.4                                                   268
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             268
C OUTER SHIELD CYLINDER, INNER RADIUS                                     269
      SSLD_SOUTRINN=29.5                                                  269
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                269
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     270
      SSLD_SOUTROUT=29.52                                                 270
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                270
C OUTER SHIELD CYLINDER, HALF LENGTH                                      271
      SSLD_SOUTLEN=65.4                                                   271
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  271
C ALUMINUM SHIELD MESH INNER DIAMETER                                     272
      SSLD_ALMESHID=9.7                                                   272
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                272
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     273
      SSLD_ALMESHOD=44                                                    273
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                273
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                274
      SSLD_ALMSHTHK=0.03                                                  274
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           274
C ALUMINUM SHIELD MESH Z POSITION                                         275
      SSLD_ALMSHPOS=53.5                                                  275
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    275
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    276
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           276
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   276
C *                                                                       276
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    276
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           276
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   276
C *                                                                       277
      CALL AGSBEGM(MODULE,IPRIN)                                          277
      IF (IPRIN.LT.0) RETURN                                              277
C Filling bank SSLD - SHIELDING PARAMETERS                                277
      BANK_TITLE='SHIELDING PARAMETERS'                                   277
      BPATH_SSLD='SSLD*'                                                  277
      IDNUM_SSLD(1)=0                                                     278
C GEOMETRY VERSION                                                        278
      SSLD_VERSION=2                                                      278
      CSSLD_VERSION='GEOMETRY VERSION'                                    278
C INNER SHIELD CYLINDER, INNER RADIUS                                     279
      SSLD_SINRINN=5.9                                                    279
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 279
C INNER SHIELD CYLINDER, OUTER RADIUS                                     280
      SSLD_SINROUT=5.908                                                  280
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 280
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    281
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           281
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   281
C *                                                                       281
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    281
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           281
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   281
C *                                                                       282
      CALL AGSBEGM(MODULE,IPRIN)                                          282
      IF (IPRIN.LT.0) RETURN                                              282
C Filling bank SSLD - SHIELDING PARAMETERS                                282
      BANK_TITLE='SHIELDING PARAMETERS'                                   282
      BPATH_SSLD='SSLD*'                                                  282
      IDNUM_SSLD(1)=0                                                     283
C GEOMETRY VERSION                                                        283
      SSLD_VERSION=3                                                      283
      CSSLD_VERSION='GEOMETRY VERSION'                                    283
C INNER SHIELD CYLINDER, INNER RADIUS                                     284
      SSLD_SINRINN=5                                                      284
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 284
C INNER SHIELD CYLINDER, OUTER RADIUS                                     285
      SSLD_SINROUT=5.008                                                  285
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 285
C INNER SHIELD CYLINDER, HALF LENGTH                                      286
      SSLD_SINLEN=53.5                                                    286
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   286
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                287
      SSLD_SSEPRINN=22                                                    287
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           287
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                288
      SSLD_SSEPROUT=22.018                                                288
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           288
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 289
      SSLD_SSEPLEN=55.4                                                   289
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             289
C OUTER SHIELD CYLINDER, INNER RADIUS                                     290
      SSLD_SOUTRINN=31.80                                                 290
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                290
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     291
      SSLD_SOUTROUT=31.82                                                 291
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                291
C OUTER SHIELD CYLINDER, HALF LENGTH                                      292
      SSLD_SOUTLEN=65.4                                                   292
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  292
C ALUMINUM SHIELD MESH INNER DIAMETER                                     293
      SSLD_ALMESHID=9.7                                                   293
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                293
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     294
      SSLD_ALMESHOD=44                                                    294
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                294
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                295
      SSLD_ALMSHTHK=0.03                                                  295
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           295
C ALUMINUM SHIELD MESH Z POSITION                                         296
      SSLD_ALMSHPOS=53.5                                                  296
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    296
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    297
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           297
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   297
C *                                                                       297
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    297
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           297
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   297
C *                                                                       298
      DO 5011 I=1,4                                                       298
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 299
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 300
5011  CONTINUE                                                            301
5012  CONTINUE                                                            301
C *                                                                       302
      CALL AGSBEGM(MODULE,IPRIN)                                          302
      IF (IPRIN.LT.0) RETURN                                              302
C Filling bank SCBP - CABLING                                             302
      BANK_TITLE='CABLING'                                                302
      BPATH_SCBP='SCBP*'                                                  302
      IDNUM_SCBP(1)=0                                                     303
C LAYER                                                                   303
      SCBP_LAYER=1                                                        303
      CSCBP_LAYER='LAYER'                                                 303
C LENGTH                                                                  304
      SCBP_LEN=1.85                                                       304
      CSCBP_LEN='LENGTH'                                                  304
C MIN RADIUS CLOSER TO WAFERS                                             305
      SCBP_RMIN1=RAD_CONES_IN(1)                                          305
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           305
C MIN RADIUS FURTHER FROM WAFERS                                          306
      SCBP_RMIN2=RAD_CONES_OUT(1)                                         306
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        306
C VOLUME OF COPPER, LV+HV CABLES                                          307
      SCBP_VOL=7.24+3.21                                                  307
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          307
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    308
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           308
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   308
C *                                                                       308
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    308
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           308
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   308
C *                                                                       309
      CALL AGSBEGM(MODULE,IPRIN)                                          309
      IF (IPRIN.LT.0) RETURN                                              309
C Filling bank SCBP - CABLING                                             309
      BANK_TITLE='CABLING'                                                309
      BPATH_SCBP='SCBP*'                                                  309
      IDNUM_SCBP(1)=0                                                     310
C LAYER                                                                   310
      SCBP_LAYER=2                                                        310
      CSCBP_LAYER='LAYER'                                                 310
C MIN RADIUS CLOSER TO WAFERS                                             311
      SCBP_RMIN1=RAD_CONES_IN(2)                                          311
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           311
C MIN RADIUS FURTHER FROM WAFERS                                          312
      SCBP_RMIN2=RAD_CONES_OUT(2)                                         312
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        312
C VOLUME OF COPPER, LV+HV CABLES                                          313
      SCBP_VOL=15.54+5.7                                                  313
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          313
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    314
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           314
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   314
C *                                                                       314
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    314
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           314
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   314
C *                                                                       315
      CALL AGSBEGM(MODULE,IPRIN)                                          315
      IF (IPRIN.LT.0) RETURN                                              315
C Filling bank SCBP - CABLING                                             315
      BANK_TITLE='CABLING'                                                315
      BPATH_SCBP='SCBP*'                                                  315
      IDNUM_SCBP(1)=0                                                     316
C LAYER                                                                   316
      SCBP_LAYER=3                                                        316
      CSCBP_LAYER='LAYER'                                                 316
C MIN RADIUS CLOSER TO WAFERS                                             317
      SCBP_RMIN1=RAD_CONES_IN(3)                                          317
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           317
C MIN RADIUS FURTHER FROM WAFERS                                          318
      SCBP_RMIN2=RAD_CONES_OUT(3)                                         318
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        318
C VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE                   319
      SCBP_VOL=4.05+2.02+3.67+1.69                                        319
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE'   319
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    320
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           320
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   320
C *                                                                       320
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    320
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           320
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   320
C *                                                                       321
      CALL AGSBEGM(MODULE,IPRIN)                                          321
      IF (IPRIN.LT.0) RETURN                                              321
C Filling bank SCBP - CABLING                                             321
      BANK_TITLE='CABLING'                                                321
      BPATH_SCBP='SCBP*'                                                  321
      IDNUM_SCBP(1)=0                                                     322
C LAYER (FORMER 5TH)                                                      322
      SCBP_LAYER=4                                                        322
      CSCBP_LAYER='LAYER (FORMER 5TH)'                                    322
C MIN RADIUS CLOSER TO WAFERS                                             323
      SCBP_RMIN1=RAD_CONES_IN(4)                                          323
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           323
C MIN RADIUS FURTHER FROM WAFERS                                          324
      SCBP_RMIN2=RAD_CONES_OUT(4)                                         324
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        324
C VOLUME OF COPPER, LV+HV CABLES                                          325
      SCBP_VOL=6.95+2.43                                                  325
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          325
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    326
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           326
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   326
C *                                                                       326
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    326
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           326
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   326
C *                                                                       327
      CALL AGSBEGM(MODULE,IPRIN)                                          327
      IF (IPRIN.LT.0) RETURN                                              327
C Filling bank SFEP - WATER FEED                                          327
      BANK_TITLE='WATER FEED'                                             327
      BPATH_SFEP='SFEP*'                                                  327
      IDNUM_SFEP(1)=0                                                     328
C LAYER                                                                   328
      SFEP_LAYER=1                                                        328
      CSFEP_LAYER='LAYER'                                                 328
C LENGTH                                                                  329
      SFEP_LEN=1.85                                                       329
      CSFEP_LEN='LENGTH'                                                  329
C MIN RADIUS CLOSER TO WAFERS                                             330
      SFEP_RMIN1=0.5*(RAD_CONES_IN(1) +RAD_CONES_IN(2))                   330
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           330
C MIN RADIUS FURTHER FROM WAFERS                                          331
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(1)+RAD_CONES_OUT(2))                  331
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        331
C VOLUME OF WATER                                                         332
      SFEP_VOL=16.0                                                       332
      CSFEP_VOL='VOLUME OF WATER'                                         332
C VOLUME OF PLASTIC                                                       333
      SFEP_VOLPLAST=38.4                                                  333
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  333
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    334
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           334
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   334
C *                                                                       334
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    334
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           334
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   334
C *                                                                       335
      CALL AGSBEGM(MODULE,IPRIN)                                          335
      IF (IPRIN.LT.0) RETURN                                              335
C Filling bank SFEP - WATER FEED                                          335
      BANK_TITLE='WATER FEED'                                             335
      BPATH_SFEP='SFEP*'                                                  335
      IDNUM_SFEP(1)=0                                                     336
C LAYER                                                                   336
      SFEP_LAYER=2                                                        336
      CSFEP_LAYER='LAYER'                                                 336
C MIN RADIUS CLOSER TO WAFERS                                             337
      SFEP_RMIN1=0.5*(RAD_CONES_IN(2) +RAD_CONES_IN(3))                   337
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           337
C MIN RADIUS FURTHER FROM WAFERS                                          338
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(2)+RAD_CONES_OUT(3))                  338
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        338
C VOLUME OF WATER                                                         339
      SFEP_VOL=24.0                                                       339
      CSFEP_VOL='VOLUME OF WATER'                                         339
C VOLUME OF PLASTIC                                                       340
      SFEP_VOLPLAST=57.6                                                  340
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  340
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    341
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           341
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   341
C *                                                                       341
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    341
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           341
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   341
C *                                                                       342
      CALL AGSBEGM(MODULE,IPRIN)                                          342
      IF (IPRIN.LT.0) RETURN                                              342
C Filling bank SFEP - WATER FEED                                          342
      BANK_TITLE='WATER FEED'                                             342
      BPATH_SFEP='SFEP*'                                                  342
      IDNUM_SFEP(1)=0                                                     343
C LAYER                                                                   343
      SFEP_LAYER=3                                                        343
      CSFEP_LAYER='LAYER'                                                 343
C MIN RADIUS CLOSER TO WAFERS                                             344
      SFEP_RMIN1=0.5*(RAD_CONES_IN(3) +RAD_CONES_IN(4))                   344
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           344
C MIN RADIUS FURTHER FROM WAFERS                                          345
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(3)+RAD_CONES_OUT(4))                  345
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        345
C VOLUME OF WATER                                                         346
      SFEP_VOL=32                                                         346
      CSFEP_VOL='VOLUME OF WATER'                                         346
C VOLUME OF PLASTIC                                                       347
      SFEP_VOLPLAST=76.8                                                  347
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  347
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    348
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           348
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   348
C *                                                                       348
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    348
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           348
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   348
C *                                                                       349
      CALL AGSBEGM(MODULE,IPRIN)                                          349
      IF (IPRIN.LT.0) RETURN                                              349
C Filling bank SWCX - SEGMENTS OF THE WATER DISTRIBUTION PIPES            349
      BANK_TITLE='SEGMENTS OF THE WATER DISTRIBUTION PIPES'               349
      BPATH_SWCX='SWCX*'                                                  349
      IDNUM_SWCX(1)=0                                                     350
C VERSION                                                                 350
      SWCX_LAYER=1                                                        350
      CSWCX_LAYER='VERSION'                                               350
C OF THE RING IN THE Z DIRECTION                                          351
      SWCX_LENGTH=2.8                                                     351
      CSWCX_LENGTH='OF THE RING IN THE Z DIRECTION'                       351
C THICKNESS OF THE MOTHER LAYER                                           352
      SWCX_DR=0.72                                                        352
      CSWCX_DR='THICKNESS OF THE MOTHER LAYER'                            352
C INNER PLASTIC TUBE RADIUS                                               353
      SWCX_RAD=0.2                                                        353
      CSWCX_RAD='INNER PLASTIC TUBE RADIUS'                               353
C FROM THE EDGE OF THE LADDER SUPPORT, INWARD                             354
      SWCX_OFFSET=-2.0                                                    354
      CSWCX_OFFSET='FROM THE EDGE OF THE LADDER SUPPORT, INWARD'          354
C RADIAL OFFSET                                                           355
      SWCX_ROFFSET=1.0                                                    355
      CSWCX_ROFFSET='RADIAL OFFSET'                                       355
C THICKNESS OF THE PLASTIC PIPE WALL                                      356
      SWCX_WALL=0.16                                                      356
      CSWCX_WALL='THICKNESS OF THE PLASTIC PIPE WALL'                     356
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    357
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           357
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   357
C *                                                                       357
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    357
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           357
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   357
C *                                                                       358
      CALL AGSBEGM(MODULE,IPRIN)                                          358
      IF (IPRIN.LT.0) RETURN                                              358
C Filling bank SOUP - MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS      358
      BANK_TITLE='MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS'         358
      BPATH_SOUP='SOUP*'                                                  358
      IDNUM_SOUP(1)=0                                                     359
C VERSION                                                                 359
      SOUP_VERSION=1                                                      359
      CSOUP_VERSION='VERSION'                                             359
C LENGTH                                                                  360
      SOUP_LENGTH=82.5                                                    360
      CSOUP_LENGTH='LENGTH'                                               360
C OUTER RADIUS OF THE SHIELD                                              361
      SOUP_ROUT=19.41                                                     361
      CSOUP_ROUT='OUTER RADIUS OF THE SHIELD'                             361
C DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF THE MOTHER)     362
      SOUP_DR=0.711                                                       362
      CSOUP_DR='DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF   362
     *THE MOTHER)'                                                        362
C STARTING ANGLE OF THE MOTHER                                            363
      SOUP_PHI1=0.0                                                       363
      CSOUP_PHI1='STARTING ANGLE OF THE MOTHER'                           363
C FINISHING ANGLE OF THE MOTHER                                           364
      SOUP_PHI2=70.0                                                      364
      CSOUP_PHI2='FINISHING ANGLE OF THE MOTHER'                          364
C OUTER DIAM OF THE CARBON TUBE                                           365
      SOUP_DIAMOUT=0.711                                                  365
      CSOUP_DIAMOUT='OUTER DIAM OF THE CARBON TUBE'                       365
C INNER DIAM OF THE CARBON TUBE                                           366
      SOUP_DIAMIN=0.620                                                   366
      CSOUP_DIAMIN='INNER DIAM OF THE CARBON TUBE'                        366
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    367
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           367
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   367
C *                                                                       367
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    367
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           367
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   367
C use default SVTG bank                                                   368
      BPATH_SVTG='SVTG*'                                                  368
      IDNUM_SVTG(1)=0                                                     368
      CALL AGDGETP('SVTTGEO6','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,  368
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      368
     *LINK_SVTG,FLAG_SVTG)                                                368
C use default SWCA bank                                                   369
      BPATH_SWCA='SWCA*'                                                  369
      IDNUM_SWCA(1)=0                                                     369
      CALL AGDGETP('SVTTGEO6','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,  369
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      369
     *LINK_SWCA,FLAG_SWCA)                                                369
C use default SELC bank                                                   370
      BPATH_SELC='SELC*'                                                  370
      IDNUM_SELC(1)=0                                                     370
      CALL AGDGETP('SVTTGEO6','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  370
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      370
     *LINK_SELC,FLAG_SELC)                                                370
C use SSUP bank                                                           371
      BPATH_SSUP='SSUP*'                                                  371
      IDNUM_SSUP(1)=0                                                     371
      SSUP_VERSION=SVTG_CONEVER                                           371
      CALL AGDGETP('SVTTGEO6','*',-999,'SSUP',NSSUP_VERSION,              371
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        371
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   371
C use default SSUB bank                                                   372
      BPATH_SSUB='SSUB*'                                                  372
      IDNUM_SSUB(1)=0                                                     372
      CALL AGDGETP('SVTTGEO6','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,  372
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      372
     *LINK_SSUB,FLAG_SSUB)                                                372
C use default SERG bank                                                   373
      BPATH_SERG='SERG*'                                                  373
      IDNUM_SERG(1)=0                                                     373
      CALL AGDGETP('SVTTGEO6','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,  373
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      373
     *LINK_SERG,FLAG_SERG)                                                373
C use default SWAM bank                                                   374
      BPATH_SWAM='SWAM*'                                                  374
      IDNUM_SWAM(1)=0                                                     374
      CALL AGDGETP('SVTTGEO6','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,  374
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      374
     *LINK_SWAM,FLAG_SWAM)                                                374
C use default SELC bank                                                   375
      BPATH_SELC='SELC*'                                                  375
      IDNUM_SELC(1)=0                                                     375
      CALL AGDGETP('SVTTGEO6','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  375
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      375
     *LINK_SELC,FLAG_SELC)                                                375
C use SSLD bank                                                           376
      BPATH_SSLD='SSLD*'                                                  376
      IDNUM_SSLD(1)=0                                                     376
      SSLD_VERSION=SVTG_SUPPORTVER                                        376
      CALL AGDGETP('SVTTGEO6','*',-999,'SSLD',NSSLD_VERSION,              376
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        376
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   376
C use default SCBP bank                                                   377
      BPATH_SCBP='SCBP*'                                                  377
      IDNUM_SCBP(1)=0                                                     377
      CALL AGDGETP('SVTTGEO6','*',-999,'SCBP',' ',BANK_SCBP, BPATH_SCBP,  377
     *IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,LLEN_SCBP,      377
     *LINK_SCBP,FLAG_SCBP)                                                377
C use default SWCX bank                                                   378
      BPATH_SWCX='SWCX*'                                                  378
      IDNUM_SWCX(1)=0                                                     378
      CALL AGDGETP('SVTTGEO6','*',-999,'SWCX',' ',BANK_SWCX, BPATH_SWCX,  378
     *IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,LLEN_SWCX,      378
     *LINK_SWCX,FLAG_SWCX)                                                378
C use default SOUP bank                                                   379
      BPATH_SOUP='SOUP*'                                                  379
      IDNUM_SOUP(1)=0                                                     379
      CALL AGDGETP('SVTTGEO6','*',-999,'SOUP',' ',BANK_SOUP, BPATH_SOUP,  379
     *IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,LLEN_SOUP,      379
     *LINK_SOUP,FLAG_SOUP)                                                379
C Check SVTG_SUPPORTVER==2                                                380
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      380
      SVTG_RSIZEMIN = 6.0                                                 381
      ENDIF                                                               382
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             383
      CALL AGSRESET                                                       383
      AG_A = 28.08                                                        383
      AG_Z = 14                                                           383
      AG_W = 0.6*1*28./60.                                                383
      AG_TITLE = 'SI'                                                     383
      AG_EXNAME = 'COMPONENT'                                             383
      AG_PARLIST = 'A_Z_W'                                                383
      CALL AXCOMPONENT                                                    383
C *                                                                       383
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  383
C =                                                                       383
      CALL AGSRESET                                                       384
      AG_A = 16                                                           384
      AG_Z = 8                                                            384
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               384
      AG_TITLE = 'O'                                                      384
      AG_EXNAME = 'COMPONENT'                                             384
      AG_PARLIST = 'A_Z_W'                                                384
      CALL AXCOMPONENT                                                    384
C *                                                                       384
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 384
      CALL AGSRESET                                                       385
      AG_A = 12                                                           385
      AG_Z = 6                                                            385
      AG_W = 0.4*8*12./174.                                               385
      AG_TITLE = 'C'                                                      385
      AG_EXNAME = 'COMPONENT'                                             385
      AG_PARLIST = 'A_Z_W'                                                385
      CALL AXCOMPONENT                                                    385
C *                                                                       385
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  385
      CALL AGSRESET                                                       386
      AG_A = 1                                                            386
      AG_Z = 1                                                            386
      AG_W = 0.4*14*1./174.                                               386
      AG_TITLE = 'H'                                                      386
      AG_EXNAME = 'COMPONENT'                                             386
      AG_PARLIST = 'A_Z_W'                                                386
      CALL AXCOMPONENT                                                    386
C *                                                                       386
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  386
      CALL AGSRESET                                                       387
      AG_DENS = 1.7                                                       387
      AG_TITLE = 'G10'                                                    387
      AG_EXNAME = 'MIXTURE'                                               387
      AG_PARLIST = 'DENS'                                                 387
      CALL AXMIXTURE                                                      387
C *                                                                       387
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       387
      CALL AGSRESET                                                       388
      AG_A = 12                                                           388
      AG_Z = 6                                                            388
      AG_W = 10000*5                                                      388
      AG_TITLE = 'C5'                                                     388
      AG_EXNAME = 'COMPONENT'                                             388
      AG_PARLIST = 'A_Z_W'                                                388
      CALL AXCOMPONENT                                                    388
C *                                                                       388
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        388
      CALL AGSRESET                                                       389
      AG_A = 1                                                            389
      AG_Z = 1                                                            389
      AG_W = 10000*4                                                      389
      AG_TITLE = 'H4'                                                     389
      AG_EXNAME = 'COMPONENT'                                             389
      AG_PARLIST = 'A_Z_W'                                                389
      CALL AXCOMPONENT                                                    389
C *                                                                       389
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       389
      CALL AGSRESET                                                       390
      AG_A = 16                                                           390
      AG_Z = 8                                                            390
      AG_W = 10000*2                                                      390
      AG_TITLE = 'O2'                                                     390
      AG_EXNAME = 'COMPONENT'                                             390
      AG_PARLIST = 'A_Z_W'                                                390
      CALL AXCOMPONENT                                                    390
C *                                                                       390
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 390
      CALL AGSRESET                                                       391
      AG_A = 27                                                           391
      AG_Z = 13                                                           391
      AG_W = 10000*0.2302                                                 391
      AG_TITLE = 'AL'                                                     391
      AG_EXNAME = 'COMPONENT'                                             391
      AG_PARLIST = 'A_Z_W'                                                391
      CALL AXCOMPONENT                                                    391
C *                                                                       391
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              391
      CALL AGSRESET                                                       392
      AG_DENS = 1.432                                                     392
      AG_TITLE = 'ALKAP'                                                  392
      AG_EXNAME = 'MIXTURE'                                               392
      AG_PARLIST = 'DENS'                                                 392
      CALL AXMIXTURE                                                      392
C *                                                                       392
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       392
      CALL AGSRESET                                                       393
      AG_A = 12                                                           393
      AG_Z = 6                                                            393
      AG_W = 10000*5                                                      393
      AG_TITLE = 'C5'                                                     393
      AG_EXNAME = 'COMPONENT'                                             393
      AG_PARLIST = 'A_Z_W'                                                393
      CALL AXCOMPONENT                                                    393
C *                                                                       393
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        393
      CALL AGSRESET                                                       394
      AG_A = 1                                                            394
      AG_Z = 1                                                            394
      AG_W = 10000*4                                                      394
      AG_TITLE = 'H4'                                                     394
      AG_EXNAME = 'COMPONENT'                                             394
      AG_PARLIST = 'A_Z_W'                                                394
      CALL AXCOMPONENT                                                    394
C *                                                                       394
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       394
      CALL AGSRESET                                                       395
      AG_A = 16                                                           395
      AG_Z = 8                                                            395
      AG_W = 10000*2                                                      395
      AG_TITLE = 'O2'                                                     395
      AG_EXNAME = 'COMPONENT'                                             395
      AG_PARLIST = 'A_Z_W'                                                395
      CALL AXCOMPONENT                                                    395
C *                                                                       395
C Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                          395
      CALL AGSRESET                                                       396
      AG_A = 27                                                           396
      AG_Z = 13                                                           396
      AG_W = 986                                                          396
      AG_TITLE = 'AL'                                                     396
      AG_EXNAME = 'COMPONENT'                                             396
      AG_PARLIST = 'A_Z_W'                                                396
      CALL AXCOMPONENT                                                    396
C *                                                                       396
C Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                          396
      CALL AGSRESET                                                       397
      AG_DENS = 1.40845                                                   397
      AG_TITLE = 'SSDALMY'                                                397
      AG_EXNAME = 'MIXTURE'                                               397
      AG_PARLIST = 'DENS'                                                 397
      CALL AXMIXTURE                                                      397
C *                                                                       397
C Check SWAM_LEN>0                                                        397
      IF (SWAM_LEN.GT.0) THEN                                             398
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              399
      CALL AGSRESET                                                       399
      AG_A = 1                                                            399
      AG_Z = 1                                                            399
      AG_W = 2                                                            399
      AG_TITLE = 'H2'                                                     399
      AG_EXNAME = 'COMPONENT'                                             399
      AG_PARLIST = 'A_Z_W'                                                399
      CALL AXCOMPONENT                                                    399
C *                                                                       399
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              399
      CALL AGSRESET                                                       400
      AG_A = 16                                                           400
      AG_Z = 8                                                            400
      AG_W = 1                                                            400
      AG_TITLE = 'O'                                                      400
      AG_EXNAME = 'COMPONENT'                                             400
      AG_PARLIST = 'A_Z_W'                                                400
      CALL AXCOMPONENT                                                    400
C *                                                                       400
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                400
      CALL AGSRESET                                                       401
      AG_DENS = 1.0                                                       401
      AG_TITLE = 'WATER'                                                  401
      AG_EXNAME = 'MIXTURE'                                               401
      AG_PARLIST = 'DENS'                                                 401
      CALL AXMIXTURE                                                      401
C *                                                                       401
      ELSE                                                                401
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             403
      CALL AGSRESET                                                       403
      AG_DENS = 0.0009                                                    403
      AG_TITLE = 'WATER'                                                  403
      AG_EXNAME = 'MIXTURE'                                               403
      AG_PARLIST = 'DENS'                                                 403
      CALL AXMIXTURE                                                      403
C *                                                                       403
      ENDIF                                                               403
C Check SVTG_IFMANY>0                                                     405
      IF (SVTG_IFMANY.GT.0) THEN                                          405
      CALL AGSBEGM(MODULE,IPRIN)                                          406
C Check IPRIN<0                                                           406
      IF (IPRIN.LT.0) RETURN                                              406
C Create SVTT :                                                           406
C Call To Local Procedure SVTT                                            406
      ASSIGN 5030 TO BLOCK_SVTT                                           406
      GO TO  5020                                                         406
5030  CONTINUE                                                            406
C *                                                                       406
C Geant POSITION : SVTT ===> MOTHER='CAVE' KONLY='MANY' <===              407
      CALL AGSRESET                                                       407
      AG_MOTHER = 'CAVE'                                                  407
      AG_KONLY = 'MANY'                                                   407
      AG_TITLE = 'SVTT'                                                   407
      AG_EXNAME = 'POSITION'                                              407
      AG_PARLIST = 'MOTHER_KONLY'                                         407
      CALL AXPOSITION                                                     407
C *                                                                       407
      ELSE                                                                407
      CALL AGSBEGM(MODULE,IPRIN)                                          409
C Check IPRIN<0                                                           409
      IF (IPRIN.LT.0) RETURN                                              409
C Create SVTT :                                                           409
C Call To Local Procedure SVTT                                            409
      ASSIGN 5040 TO BLOCK_SVTT                                           409
      GO TO  5020                                                         409
5040  CONTINUE                                                            409
C *                                                                       409
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           410
      CALL AGSRESET                                                       410
      AG_MOTHER = 'CAVE'                                                  410
      AG_TITLE = 'SVTT'                                                   410
      AG_EXNAME = 'POSITION'                                              410
      AG_PARLIST = 'MOTHER'                                               410
      CALL AXPOSITION                                                     410
C *                                                                       410
      ENDIF                                                               410
C ----------------------------------------------------------------------  412
C ------------ SVTT                                                       412
      CALL AGSENDM                                                        413
      RETURN                                                              413
C Begin Local Procedure SVTT                                              413
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   413
5020  CONTINUE                                                            413
         AG_TITLE='SVTT'                                                  413
         AG_MODULE=MODULE                                                 413
         AG_MARK='V00'                                                    413
         CALL AGSPUSH                                                     413
         AG_MODULE=MODULE                                                 413
C    *                                                                    413
         RSIZEMAX=SSUP_CON4IDMX                                           414
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 415
     *   2.0*SSUP_HOSRMX                                                  415
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                416
         CLEARANCE=SVTG_RSIZEMAX - RSIZEMAX                               417
C    Check (CLEARANCE<0)                                                  418
         IF ((CLEARANCE.LT.0)) THEN                                       418
         ENDIF                                                            419
C    Geant MATERIAL : AIR ===> inherited arguments used <===              420
         CALL AGSRESET                                                    420
         AG_TITLE = 'AIR'                                                 420
         AG_EXNAME = 'MATERIAL'                                           420
         AG_PARLIST = 'NONE'                                              420
         CALL AXMATERIAL                                                  420
C    *                                                                    420
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       420
         CALL AGSRESET                                                    421
         AG_SEEN = 0                                                      421
         AG_COLO = 1                                                      421
         AG_TITLE = 'SVTT'                                                421
         AG_EXNAME = 'ATTRIBUTE'                                          421
         AG_PARLIST = 'SEEN_COLO'                                         421
         CALL AXATTRIBUTE                                                 421
C    *                                                                    421
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  421
C    VTG_ZSIZEMAX <===                                                    421
         CALL AGSRESET                                                    423
         AG_RMIN = SVTG_RSIZEMIN                                          423
         AG_RMAX = SVTG_RSIZEMAX                                          423
         AG_DZ = SVTG_ZSIZEMAX                                            423
         AG_TITLE = 'TUBE'                                                423
         AG_EXNAME = 'SHAPE'                                              423
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      423
         CALL AXSHAPE                                                     423
C    *                                                                    423
C    Create SIRP :                                                        424
C    Call To Local Procedure SIRP                                         424
         ASSIGN 5060 TO BLOCK_SIRP                                        424
         GO TO  5050                                                      424
5060     CONTINUE                                                         424
C    *                                                                    424
C    Geant POSITION : SIRP ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  425
C    2.5 <===                                                             425
         CALL AGSRESET                                                    425
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             425
         AG_ALPHAN=AG_ALPHAN+1                                            425
         AG_ALPHAK(AG_ALPHAN)=3                                           425
         AG_ALPHAL(AG_ALPHAN)=22.5                                        425
         AG_TITLE = 'SIRP'                                                425
         AG_EXNAME = 'POSITION'                                           425
         AG_PARLIST = 'Z_ALPHAZ'                                          425
         CALL AXPOSITION                                                  425
C    *                                                                    425
C    Geant POSITION : SIRP ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  425
C    22.5 <===                                                            425
         CALL AGSRESET                                                    426
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            426
         AG_ALPHAN=AG_ALPHAN+1                                            426
         AG_ALPHAK(AG_ALPHAN)=3                                           426
         AG_ALPHAL(AG_ALPHAN)=22.5                                        426
         AG_TITLE = 'SIRP'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'Z_ALPHAZ'                                          426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Create SIRT :                                                        427
C    Call To Local Procedure SIRT                                         427
         ASSIGN 5080 TO BLOCK_SIRT                                        427
         GO TO  5070                                                      427
5080     CONTINUE                                                         427
C    *                                                                    427
C    Geant POSITION : SIRT ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  428
C    2.5 <===                                                             428
         CALL AGSRESET                                                    428
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             428
         AG_ALPHAN=AG_ALPHAN+1                                            428
         AG_ALPHAK(AG_ALPHAN)=3                                           428
         AG_ALPHAL(AG_ALPHAN)=22.5                                        428
         AG_TITLE = 'SIRT'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'Z_ALPHAZ'                                          428
         CALL AXPOSITION                                                  428
C    *                                                                    428
C    Geant POSITION : SIRT ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  428
C    22.5 <===                                                            428
         CALL AGSRESET                                                    429
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            429
         AG_ALPHAN=AG_ALPHAN+1                                            429
         AG_ALPHAK(AG_ALPHAN)=3                                           429
         AG_ALPHAL(AG_ALPHAN)=22.5                                        429
         AG_TITLE = 'SIRT'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'Z_ALPHAZ'                                          429
         CALL AXPOSITION                                                  429
C    *                                                                    429
C    Create SOER :                                                        430
C    Call To Local Procedure SOER                                         430
         ASSIGN 5100 TO BLOCK_SOER                                        430
         GO TO  5090                                                      430
5100     CONTINUE                                                         430
C    *                                                                    430
C    Geant POSITION : SOER ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 <===      431
         CALL AGSRESET                                                    431
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             431
         AG_TITLE = 'SOER'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'Z'                                                 431
         CALL AXPOSITION                                                  431
C    *                                                                    431
C    Geant POSITION : SOER ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 <===     431
         CALL AGSRESET                                                    432
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            432
         AG_TITLE = 'SOER'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'Z'                                                 432
         CALL AXPOSITION                                                  432
C    *                                                                    432
C    Create SBRG :                                                        433
C    Call To Local Procedure SBRG                                         433
         ASSIGN 5120 TO BLOCK_SBRG                                        433
         GO TO  5110                                                      433
5120     CONTINUE                                                         433
C    *                                                                    433
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  434
C    /2 <===                                                              434
         CALL AGSRESET                                                    434
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  434
         AG_TITLE = 'SBRG'                                                434
         AG_EXNAME = 'POSITION'                                           434
         AG_PARLIST = 'Z'                                                 434
         CALL AXPOSITION                                                  434
C    *                                                                    434
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  434
C    K/2 <===                                                             434
         CALL AGSRESET                                                    435
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 435
         AG_TITLE = 'SBRG'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'Z'                                                 435
         CALL AXPOSITION                                                  435
C    *                                                                    435
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           435
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             437
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      438
C    Create SOES :                                                        439
C    Call To Local Procedure SOES                                         439
         ASSIGN 5140 TO BLOCK_SOES                                        439
         GO TO  5130                                                      439
5140     CONTINUE                                                         439
C    *                                                                    439
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               440
         CALL AGSRESET                                                    440
         AG_Z = SCREW_Z                                                   440
         AG_KONLY = 'MANY'                                                440
         AG_TITLE = 'SOES'                                                440
         AG_EXNAME = 'POSITION'                                           440
         AG_PARLIST = 'Z_KONLY'                                           440
         CALL AXPOSITION                                                  440
C    *                                                                    440
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              440
         CALL AGSRESET                                                    441
         AG_Z = -SCREW_Z                                                  441
         AG_KONLY = 'MANY'                                                441
         AG_TITLE = 'SOES'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'Z_KONLY'                                           441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Create SIES :                                                        442
C    Call To Local Procedure SIES                                         442
         ASSIGN 5160 TO BLOCK_SIES                                        442
         GO TO  5150                                                      442
5160     CONTINUE                                                         442
C    *                                                                    442
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               443
         CALL AGSRESET                                                    443
         AG_Z = SCREW_Z                                                   443
         AG_KONLY = 'MANY'                                                443
         AG_TITLE = 'SIES'                                                443
         AG_EXNAME = 'POSITION'                                           443
         AG_PARLIST = 'Z_KONLY'                                           443
         CALL AXPOSITION                                                  443
C    *                                                                    443
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              443
         CALL AGSRESET                                                    444
         AG_Z = -SCREW_Z                                                  444
         AG_KONLY = 'MANY'                                                444
         AG_TITLE = 'SIES'                                                444
         AG_EXNAME = 'POSITION'                                           444
         AG_PARLIST = 'Z_KONLY'                                           444
         CALL AXPOSITION                                                  444
C    *                                                                    444
C    Check SWAM_LEN>0                                                     444
         IF (SWAM_LEN.GT.0) THEN                                          445
C    Create SWMM :                                                        446
C    Call To Local Procedure SWMM                                         446
         ASSIGN 5180 TO BLOCK_SWMM                                        446
         GO TO  5170                                                      446
5180     CONTINUE                                                         446
C    *                                                                    446
C    Geant POSITION : SWMM ===> Z=SWAM_ZMIN+SWAM_LEN/2 <===               447
         CALL AGSRESET                                                    447
         AG_Z = SWAM_ZMIN+SWAM_LEN/2                                      447
         AG_TITLE = 'SWMM'                                                447
         AG_EXNAME = 'POSITION'                                           447
         AG_PARLIST = 'Z'                                                 447
         CALL AXPOSITION                                                  447
C    *                                                                    447
C    Geant POSITION : SWMM ===> Z=-SWAM_ZMIN-SWAM_LEN/2 <===              447
         CALL AGSRESET                                                    448
         AG_Z = -SWAM_ZMIN-SWAM_LEN/2                                     448
         AG_TITLE = 'SWMM'                                                448
         AG_EXNAME = 'POSITION'                                           448
         AG_PARLIST = 'Z'                                                 448
         CALL AXPOSITION                                                  448
C    *                                                                    448
         ENDIF                                                            448
C    Check SWAM_LEN>0                                                     450
         IF (SWAM_LEN.GT.0) THEN                                          450
C    Create SBWC :                                                        451
C    Call To Local Procedure SBWC                                         451
         ASSIGN 5200 TO BLOCK_SBWC                                        451
         GO TO  5190                                                      451
5200     CONTINUE                                                         451
C    *                                                                    451
C    Geant POSITION : SBWC ===> Z=(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SW  452
C    AM_ZMIN+SWAM_LEN))/2) <===                                           452
         CALL AGSRESET                                                    452
         AG_Z =                                                           452
     *   (SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)      452
         AG_TITLE = 'SBWC'                                                452
         AG_EXNAME = 'POSITION'                                           452
         AG_PARLIST = 'Z'                                                 452
         CALL AXPOSITION                                                  452
C    *                                                                    452
C    Geant POSITION : SBWC ===> Z=-(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(S  452
C    WAM_ZMIN+SWAM_LEN))/2) THETAZ=180 <===                               452
         CALL AGSRESET                                                    454
         AG_Z =                                                           454
     *   -(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)     454
         AG_THETAZ = 180                                                  454
         AG_TITLE = 'SBWC'                                                454
         AG_EXNAME = 'POSITION'                                           454
         AG_PARLIST = 'Z_THETAZ'                                          454
         CALL AXPOSITION                                                  454
C    *                                                                    454
         ENDIF                                                            454
C    Create SCON :                                                        456
C    Call To Local Procedure SCON                                         456
         ASSIGN 5220 TO BLOCK_SCON                                        456
         GO TO  5210                                                      456
5220     CONTINUE                                                         456
C    *                                                                    456
C    Geant POSITION : SCON ===> inherited arguments used <===             457
         CALL AGSRESET                                                    457
         AG_TITLE = 'SCON'                                                457
         AG_EXNAME = 'POSITION'                                           457
         AG_PARLIST = 'NONE'                                              457
         CALL AXPOSITION                                                  457
C    *                                                                    457
C    Geant POSITION : SCON ===> THETAZ=180 <===                           457
         CALL AGSRESET                                                    458
         AG_THETAZ = 180                                                  458
         AG_TITLE = 'SCON'                                                458
         AG_EXNAME = 'POSITION'                                           458
         AG_PARLIST = 'THETAZ'                                            458
         CALL AXPOSITION                                                  458
C    *                                                                    458
C    Create SBSP :                                                        459
C    Call To Local Procedure SBSP                                         459
         ASSIGN 5240 TO BLOCK_SBSP                                        459
         GO TO  5230                                                      459
5240     CONTINUE                                                         459
C    *                                                                    459
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     460
         CALL AGSRESET                                                    460
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            460
         AG_TITLE = 'SBSP'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'Z'                                                 460
         CALL AXPOSITION                                                  460
C    *                                                                    460
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  460
C    =180 <===                                                            460
         CALL AGSRESET                                                    461
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           461
         AG_THETAZ = 180                                                  461
         AG_TITLE = 'SBSP'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'Z_THETAZ'                                          461
         CALL AXPOSITION                                                  461
C    *                                                                    461
C    Create SROD :                                                        462
C    Call To Local Procedure SROD                                         462
         ASSIGN 5260 TO BLOCK_SROD                                        462
         GO TO  5250                                                      462
5260     CONTINUE                                                         462
C    *                                                                    462
C    Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===          463
         CALL AGSRESET                                                    463
         AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                 463
         AG_TITLE = 'SROD'                                                463
         AG_EXNAME = 'POSITION'                                           463
         AG_PARLIST = 'Y'                                                 463
         CALL AXPOSITION                                                  463
C    *                                                                    463
C    Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===         463
         CALL AGSRESET                                                    464
         AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                464
         AG_TITLE = 'SROD'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'Y'                                                 464
         CALL AXPOSITION                                                  464
C    *                                                                    464
         RADMAX=SVTG_RSIZEMAX                                             464
C    *                                                                    466
         ILAYER=SVTG_NMIN                                                 466
            GO TO 5273                                                    466
5271        ILAYER=ILAYER+1                                               466
5273        IF(ILAYER-(MIN(6,SVTG_NLAYER)).GT.0) GO TO 5272               466
C       Check ILAYER<6                                                    467
            IF (ILAYER.LT.6) THEN                                         467
C       use SVTL bank                                                     468
            BPATH_SVTL='SVTL*'                                            468
            IDNUM_SVTL(1)=0                                               468
            SVTL_LAYER=ILAYER+1                                           468
            CALL AGDGETP('SVTTGEO6','*',-999,'SVTL',NSVTL_LAYER,          468
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    468
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             468
            RADMAX=SVTL_RADIUS                                            469
            ELSE                                                          470
            RADMAX=SVTG_RSIZEMAX                                          471
            ENDIF                                                         472
C       use SVTL bank                                                     473
            BPATH_SVTL='SVTL*'                                            473
            IDNUM_SVTL(1)=0                                               473
            SVTL_LAYER=ILAYER                                             473
            CALL AGDGETP('SVTTGEO6','*',-999,'SVTL',NSVTL_LAYER,          473
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    473
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             473
C       Create SLYD :                                                     474
C       Call To Local Procedure SLYD                                      474
            ASSIGN 5290 TO BLOCK_SLYD                                     474
            GO TO  5280                                                   474
5290        CONTINUE                                                      474
C       *                                                                 474
C       Check ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6                   475
            IF (ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6) THEN           475
C       Geant POSITION : SLYD ===> KONLY='MANY' <===                      476
            CALL AGSRESET                                                 476
            AG_KONLY = 'MANY'                                             476
            AG_TITLE = 'SLYD'                                             476
            AG_EXNAME = 'POSITION'                                        476
            AG_PARLIST = 'KONLY'                                          476
            CALL AXPOSITION                                               476
C       *                                                                 476
            ELSE                                                          476
C       Geant POSITION : SLYD ===> inherited arguments used <===          478
            CALL AGSRESET                                                 478
            AG_TITLE = 'SLYD'                                             478
            AG_EXNAME = 'POSITION'                                        478
            AG_PARLIST = 'NONE'                                           478
            CALL AXPOSITION                                               478
C       *                                                                 478
            ENDIF                                                         478
         GO TO 5271                                                       480
5272     CONTINUE                                                         480
C    Check SVTG_NLAYER<0                                                  481
         IF (SVTG_NLAYER.LT.0) THEN                                       481
C    use SVTL bank                                                        482
         BPATH_SVTL='SVTL*'                                               482
         IDNUM_SVTL(1)=0                                                  482
         SVTL_LAYER=-SVTG_NLAYER                                          482
         CALL AGDGETP('SVTTGEO6','*',-999,'SVTL',NSVTL_LAYER,             482
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       482
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                482
C    Create SLYD :                                                        483
C    Call To Local Procedure SLYD                                         483
         ASSIGN 5300 TO BLOCK_SLYD                                        483
         GO TO  5280                                                      483
5300     CONTINUE                                                         483
C    *                                                                    483
C    Geant POSITION : SLYD ===> inherited arguments used <===             484
         CALL AGSRESET                                                    484
         AG_TITLE = 'SLYD'                                                484
         AG_EXNAME = 'POSITION'                                           484
         AG_PARLIST = 'NONE'                                              484
         CALL AXPOSITION                                                  484
C    *                                                                    484
         ENDIF                                                            484
C    Create SISH :                                                        486
C    Call To Local Procedure SISH                                         486
         ASSIGN 5320 TO BLOCK_SISH                                        486
         GO TO  5310                                                      486
5320     CONTINUE                                                         486
C    *                                                                    486
C    Geant POSITION : SISH ===> inherited arguments used <===             487
         CALL AGSRESET                                                    487
         AG_TITLE = 'SISH'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'NONE'                                              487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Create SOSH :                                                        488
C    Call To Local Procedure SOSH                                         488
         ASSIGN 5340 TO BLOCK_SOSH                                        488
         GO TO  5330                                                      488
5340     CONTINUE                                                         488
C    *                                                                    488
C    Geant POSITION : SOSH ===> inherited arguments used <===             489
         CALL AGSRESET                                                    489
         AG_TITLE = 'SOSH'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'NONE'                                              489
         CALL AXPOSITION                                                  489
C    *                                                                    489
C    Check SVTG_NLAYER>6                                                  489
         IF (SVTG_NLAYER.GT.6) THEN                                       490
C    Create SSSH :                                                        491
C    Call To Local Procedure SSSH                                         491
         ASSIGN 5360 TO BLOCK_SSSH                                        491
         GO TO  5350                                                      491
5360     CONTINUE                                                         491
C    *                                                                    491
C    Geant POSITION : SSSH ===> inherited arguments used <===             492
         CALL AGSRESET                                                    492
         AG_TITLE = 'SSSH'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'NONE'                                              492
         CALL AXPOSITION                                                  492
C    *                                                                    492
         ENDIF                                                            492
C    Create SALM :                                                        494
C    Call To Local Procedure SALM                                         494
         ASSIGN 5380 TO BLOCK_SALM                                        494
         GO TO  5370                                                      494
5380     CONTINUE                                                         494
C    *                                                                    494
C    Geant POSITION : SALM ===> Z=SSLD_ALMSHPOS-SSLD_ALMSHTHK/2 <===      495
         CALL AGSRESET                                                    495
         AG_Z = SSLD_ALMSHPOS-SSLD_ALMSHTHK/2                             495
         AG_TITLE = 'SALM'                                                495
         AG_EXNAME = 'POSITION'                                           495
         AG_PARLIST = 'Z'                                                 495
         CALL AXPOSITION                                                  495
C    *                                                                    495
C    Geant POSITION : SALM ===> Z=-(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2) <===   495
         CALL AGSRESET                                                    496
         AG_Z = -(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2)                          496
         AG_TITLE = 'SALM'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'Z'                                                 496
         CALL AXPOSITION                                                  496
C    *                                                                    496
C    Create SCBM :                                                        497
C    Call To Local Procedure SCBM                                         497
         ASSIGN 5400 TO BLOCK_SCBM                                        497
         GO TO  5390                                                      497
5400     CONTINUE                                                         497
C    *                                                                    497
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=SWAM_ZMIN-SCBP_LEN <===     498
         CALL AGSRESET                                                    498
         AG_X = 0.0                                                       498
         AG_Y = 0.0                                                       498
         AG_Z = SWAM_ZMIN-SCBP_LEN                                        498
         AG_TITLE = 'SCBM'                                                498
         AG_EXNAME = 'POSITION'                                           498
         AG_PARLIST = 'X_Y_Z'                                             498
         CALL AXPOSITION                                                  498
C    *                                                                    498
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=-(SWAM_ZMIN-SCBP_LEN) THET  498
C    AZ=180.0 <===                                                        498
         CALL AGSRESET                                                    499
         AG_X = 0.0                                                       499
         AG_Y = 0.0                                                       499
         AG_Z = -(SWAM_ZMIN-SCBP_LEN)                                     499
         AG_THETAZ = 180.0                                                499
         AG_TITLE = 'SCBM'                                                499
         AG_EXNAME = 'POSITION'                                           499
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      499
         CALL AXPOSITION                                                  499
C    *                                                                    499
C    use SVTL bank                                                        499
         BPATH_SVTL='SVTL*'                                               500
         IDNUM_SVTL(1)=0                                                  500
         SVTL_LAYER=6                                                     500
         CALL AGDGETP('SVTTGEO6','*',-999,'SVTL',NSVTL_LAYER,             500
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       500
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                500
C    Create SXRL :                                                        501
C    Call To Local Procedure SXRL                                         501
         ASSIGN 5420 TO BLOCK_SXRL                                        501
         GO TO  5410                                                      501
5420     CONTINUE                                                         501
C    *                                                                    501
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  503
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  503
         CALL AGSRESET                                                    503
         AG_X = 0.0                                                       503
         AG_Y = 0.0                                                       503
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               503
         AG_KONLY = 'MANY'                                                503
         AG_TITLE = 'SXRL'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_Y_Z_KONLY'                                       503
         CALL AXPOSITION                                                  503
C    *                                                                    503
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  503
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 503
         CALL AGSRESET                                                    505
         AG_X = 0.0                                                       505
         AG_Y = 0.0                                                       505
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              505
         AG_KONLY = 'MANY'                                                505
         AG_TITLE = 'SXRL'                                                505
         AG_EXNAME = 'POSITION'                                           505
         AG_PARLIST = 'X_Y_Z_KONLY'                                       505
         CALL AXPOSITION                                                  505
C    *                                                                    505
C    use SVTL bank                                                        505
         BPATH_SVTL='SVTL*'                                               506
         IDNUM_SVTL(1)=0                                                  506
         SVTL_LAYER=4                                                     506
         CALL AGDGETP('SVTTGEO6','*',-999,'SVTL',NSVTL_LAYER,             506
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       506
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                506
C    Create SXRL :                                                        507
C    Call To Local Procedure SXRL                                         507
         ASSIGN 5430 TO BLOCK_SXRL                                        507
         GO TO  5410                                                      507
5430     CONTINUE                                                         507
C    *                                                                    507
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  509
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  509
         CALL AGSRESET                                                    509
         AG_X = 0.0                                                       509
         AG_Y = 0.0                                                       509
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               509
         AG_KONLY = 'MANY'                                                509
         AG_TITLE = 'SXRL'                                                509
         AG_EXNAME = 'POSITION'                                           509
         AG_PARLIST = 'X_Y_Z_KONLY'                                       509
         CALL AXPOSITION                                                  509
C    *                                                                    509
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  509
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 509
         CALL AGSRESET                                                    511
         AG_X = 0.0                                                       511
         AG_Y = 0.0                                                       511
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              511
         AG_KONLY = 'MANY'                                                511
         AG_TITLE = 'SXRL'                                                511
         AG_EXNAME = 'POSITION'                                           511
         AG_PARLIST = 'X_Y_Z_KONLY'                                       511
         CALL AXPOSITION                                                  511
C    *                                                                    511
C    use SVTL bank                                                        511
         BPATH_SVTL='SVTL*'                                               512
         IDNUM_SVTL(1)=0                                                  512
         SVTL_LAYER=2                                                     512
         CALL AGDGETP('SVTTGEO6','*',-999,'SVTL',NSVTL_LAYER,             512
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       512
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                512
C    Create SXRL :                                                        513
C    Call To Local Procedure SXRL                                         513
         ASSIGN 5440 TO BLOCK_SXRL                                        513
         GO TO  5410                                                      513
5440     CONTINUE                                                         513
C    *                                                                    513
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  515
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  515
         CALL AGSRESET                                                    515
         AG_X = 0.0                                                       515
         AG_Y = 0.0                                                       515
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               515
         AG_KONLY = 'MANY'                                                515
         AG_TITLE = 'SXRL'                                                515
         AG_EXNAME = 'POSITION'                                           515
         AG_PARLIST = 'X_Y_Z_KONLY'                                       515
         CALL AXPOSITION                                                  515
C    *                                                                    515
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  515
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 515
         CALL AGSRESET                                                    517
         AG_X = 0.0                                                       517
         AG_Y = 0.0                                                       517
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              517
         AG_KONLY = 'MANY'                                                517
         AG_TITLE = 'SXRL'                                                517
         AG_EXNAME = 'POSITION'                                           517
         AG_PARLIST = 'X_Y_Z_KONLY'                                       517
         CALL AXPOSITION                                                  517
C    *                                                                    517
C    Create SOUM :                                                        518
C    Call To Local Procedure SOUM                                         518
         ASSIGN 5460 TO BLOCK_SOUM                                        518
         GO TO  5450                                                      518
5460     CONTINUE                                                         518
C    *                                                                    518
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 <===                    519
         CALL AGSRESET                                                    519
         AG_X = 0.0                                                       519
         AG_Y = 0.0                                                       519
         AG_Z = 0.0                                                       519
         AG_TITLE = 'SOUM'                                                519
         AG_EXNAME = 'POSITION'                                           519
         AG_PARLIST = 'X_Y_Z'                                             519
         CALL AXPOSITION                                                  519
C    *                                                                    519
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 <===       519
         CALL AGSRESET                                                    520
         AG_X = 0.0                                                       520
         AG_Y = 0.0                                                       520
         AG_Z = 0.0                                                       520
         AG_ALPHAN=AG_ALPHAN+1                                            520
         AG_ALPHAK(AG_ALPHAN)=2                                           520
         AG_ALPHAL(AG_ALPHAN)=180.0                                       520
         AG_TITLE = 'SOUM'                                                520
         AG_EXNAME = 'POSITION'                                           520
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      520
         CALL AXPOSITION                                                  520
C    *                                                                    520
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 ALPHAZ=18  520
C    0 <===                                                               520
         CALL AGSRESET                                                    521
         AG_X = 0.0                                                       521
         AG_Y = 0.0                                                       521
         AG_Z = 0.0                                                       521
         AG_ALPHAN=AG_ALPHAN+1                                            521
         AG_ALPHAK(AG_ALPHAN)=2                                           521
         AG_ALPHAL(AG_ALPHAN)=180.0                                       521
         AG_ALPHAN=AG_ALPHAN+1                                            521
         AG_ALPHAK(AG_ALPHAN)=3                                           521
         AG_ALPHAL(AG_ALPHAN)=180                                         521
         AG_TITLE = 'SOUM'                                                521
         AG_EXNAME = 'POSITION'                                           521
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               521
         CALL AXPOSITION                                                  521
C    *                                                                    521
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAZ=180 <===         521
         CALL AGSRESET                                                    522
         AG_X = 0.0                                                       522
         AG_Y = 0.0                                                       522
         AG_Z = 0.0                                                       522
         AG_ALPHAN=AG_ALPHAN+1                                            522
         AG_ALPHAK(AG_ALPHAN)=3                                           522
         AG_ALPHAL(AG_ALPHAN)=180                                         522
         AG_TITLE = 'SOUM'                                                522
         AG_EXNAME = 'POSITION'                                           522
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      522
         CALL AXPOSITION                                                  522
C    *                                                                    522
5021  CALL AGSPOPB                                                        523
      GO TO BLOCK_SVTT, (5020 ,5030,5040)                                 523
C End of Local Procedure                                                  523
C ----------------------------------------------------------------------  524
C ------------ SXRL                                                       524
C Begin Local Procedure SXRL                                              525
C SXRL IS THE MOTHER OF THE CIRCULAR WATER PIPES                          525
5410  CONTINUE                                                            525
         AG_TITLE='SXRL'                                                  525
         AG_MODULE=MODULE                                                 525
         AG_MARK='V00'                                                    525
         CALL AGSPUSH                                                     525
         AG_MODULE=MODULE                                                 525
C    *                                                                    525
C    Geant MATERIAL : AIR ===> inherited arguments used <===              526
         CALL AGSRESET                                                    526
         AG_TITLE = 'AIR'                                                 526
         AG_EXNAME = 'MATERIAL'                                           526
         AG_PARLIST = 'NONE'                                              526
         CALL AXMATERIAL                                                  526
C    *                                                                    526
C    Geant ATTRIBUTE : SXRL ===> SEEN=0 COLO=1 <===                       526
         CALL AGSRESET                                                    527
         AG_SEEN = 0                                                      527
         AG_COLO = 1                                                      527
         AG_TITLE = 'SXRL'                                                527
         AG_EXNAME = 'ATTRIBUTE'                                          527
         AG_PARLIST = 'SEEN_COLO'                                         527
         CALL AXATTRIBUTE                                                 527
C    *                                                                    527
C    Geant SHAPE : TUBE ===> RMIN=SWCX_ROFFSET+SVTL_RADIUS, RMAX=SWCX_RO  527
C    FFSET+SVTL_RADIUS+SWCX_DR DZ=SWCX_LENGTH/2.0 <===                    527
         CALL AGSRESET                                                    529
         AG_RMIN = SWCX_ROFFSET+SVTL_RADIUS                               529
         AG_RMAX = SWCX_ROFFSET+SVTL_RADIUS+SWCX_DR                       529
         AG_DZ = SWCX_LENGTH/2.0                                          529
         AG_TITLE = 'TUBE'                                                529
         AG_EXNAME = 'SHAPE'                                              529
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      529
         CALL AXSHAPE                                                     529
C    *                                                                    529
C    Create SWRP :                                                        530
C    Call To Local Procedure SWRP                                         530
         ASSIGN 5480 TO BLOCK_SWRP                                        530
         GO TO  5470                                                      530
5480     CONTINUE                                                         530
C    *                                                                    530
C    Create SYRU :                                                        531
C    Call To Local Procedure SYRU                                         531
         ASSIGN 5500 TO BLOCK_SYRU                                        531
         GO TO  5490                                                      531
5500     CONTINUE                                                         531
C    *                                                                    531
C    *                                                                    532
         I_PHI=1                                                          532
            GO TO 5513                                                    532
5511        I_PHI=I_PHI+1                                                 532
5513        IF(I_PHI-(4*SVTL_NLADDER).GT.0) GO TO 5512                    532
            TUBE_ANGLE=(PI/(2.0*SVTL_NLADDER))*(I_PHI-0.5)                533
C       Geant POSITION : SWRP ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  535
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  535
C       SWCX_DR/2.0) Z=0.0 <===                                           535
            CALL AGSRESET                                                 535
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     535
     *      2.0)                                                          535
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     535
     *      2.0)                                                          535
            AG_Z = 0.0                                                    535
            AG_TITLE = 'SWRP'                                             535
            AG_EXNAME = 'POSITION'                                        535
            AG_PARLIST = 'X_Y_Z'                                          535
            CALL AXPOSITION                                               535
C       *                                                                 535
C       Geant POSITION : SYRU ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  535
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  535
C       SWCX_DR/2.0) Z=0.0 <===                                           535
            CALL AGSRESET                                                 537
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     537
     *      2.0)                                                          537
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     537
     *      2.0)                                                          537
            AG_Z = 0.0                                                    537
            AG_TITLE = 'SYRU'                                             537
            AG_EXNAME = 'POSITION'                                        537
            AG_PARLIST = 'X_Y_Z'                                          537
            CALL AXPOSITION                                               537
C       *                                                                 537
         GO TO 5511                                                       538
5512     CONTINUE                                                         538
5411  CALL AGSPOPB                                                        539
      GO TO BLOCK_SXRL, (5410 ,5420,5430,5440)                            539
C End of Local Procedure                                                  539
C ----------------------------------------------------------------------  540
C ------------ SWRP                                                       540
C Begin Local Procedure SWRP                                              542
C SWRP IS AN APPROXIMATION OF WATER IN THE CIRCULAR PIPE, A RECTANGULAR,  542
C  ONE                                                                    542
5470  CONTINUE                                                            542
         AG_TITLE='SWRP'                                                  542
         AG_MODULE=MODULE                                                 542
         AG_MARK='V00'                                                    542
         CALL AGSPUSH                                                     542
         AG_MODULE=MODULE                                                 542
C    *                                                                    542
C    Geant MATERIAL : WATER ===> inherited arguments used <===            543
         CALL AGSRESET                                                    543
         AG_TITLE = 'WATER'                                               543
         AG_EXNAME = 'MATERIAL'                                           543
         AG_PARLIST = 'NONE'                                              543
         CALL AXMATERIAL                                                  543
C    *                                                                    543
C    Geant ATTRIBUTE : SWRP ===> SEEN=1 COLO=4 <===                       543
         CALL AGSRESET                                                    544
         AG_SEEN = 1                                                      544
         AG_COLO = 4                                                      544
         AG_TITLE = 'SWRP'                                                544
         AG_EXNAME = 'ATTRIBUTE'                                          544
         AG_PARLIST = 'SEEN_COLO'                                         544
         CALL AXATTRIBUTE                                                 544
C    *                                                                    544
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SWCX_RAD DZ=SWCX_LENGTH/2.0 <  544
C    ===                                                                  544
         CALL AGSRESET                                                    545
         AG_RMIN = 0.0                                                    545
         AG_RMAX = SWCX_RAD                                               545
         AG_DZ = SWCX_LENGTH/2.0                                          545
         AG_TITLE = 'TUBE'                                                545
         AG_EXNAME = 'SHAPE'                                              545
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      545
         CALL AXSHAPE                                                     545
C    *                                                                    545
5471  CALL AGSPOPB                                                        546
      GO TO BLOCK_SWRP, (5470 ,5480)                                      546
C End of Local Procedure                                                  546
C ----------------------------------------------------------------------  547
C ------------ SYRU                                                       547
C Begin Local Procedure SYRU                                              548
C SYRU IS THE WALL OF THE WATER PIPE                                      548
5490  CONTINUE                                                            548
         AG_TITLE='SYRU'                                                  548
         AG_MODULE=MODULE                                                 548
         AG_MARK='V00'                                                    548
         CALL AGSPUSH                                                     548
         AG_MODULE=MODULE                                                 548
C    *                                                                    548
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           549
         CALL AGSRESET                                                    549
         AG_A = 12                                                        549
         AG_Z = 6                                                         549
         AG_W = 1                                                         549
         AG_TITLE = 'C'                                                   549
         AG_EXNAME = 'COMPONENT'                                          549
         AG_PARLIST = 'A_Z_W'                                             549
         CALL AXCOMPONENT                                                 549
C    *                                                                    549
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           549
         CALL AGSRESET                                                    550
         AG_A = 1                                                         550
         AG_Z = 1                                                         550
         AG_W = 2                                                         550
         AG_TITLE = 'H2'                                                  550
         AG_EXNAME = 'COMPONENT'                                          550
         AG_PARLIST = 'A_Z_W'                                             550
         CALL AXCOMPONENT                                                 550
C    *                                                                    550
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             550
         CALL AGSRESET                                                    551
         AG_DENS = 0.935                                                  551
         AG_TITLE = 'CH2'                                                 551
         AG_EXNAME = 'MIXTURE'                                            551
         AG_PARLIST = 'DENS'                                              551
         CALL AXMIXTURE                                                   551
C    *                                                                    551
C    Geant ATTRIBUTE : SYRU ===> SEEN=1 COLO=4 <===                       551
         CALL AGSRESET                                                    552
         AG_SEEN = 1                                                      552
         AG_COLO = 4                                                      552
         AG_TITLE = 'SYRU'                                                552
         AG_EXNAME = 'ATTRIBUTE'                                          552
         AG_PARLIST = 'SEEN_COLO'                                         552
         CALL AXATTRIBUTE                                                 552
C    *                                                                    552
C    Geant SHAPE : TUBE ===> RMIN=SWCX_RAD RMAX=SWCX_RAD+SWCX_WALL, DZ=S  552
C    WCX_LENGTH/2.0 <===                                                  552
         CALL AGSRESET                                                    554
         AG_RMIN = SWCX_RAD                                               554
         AG_RMAX = SWCX_RAD+SWCX_WALL                                     554
         AG_DZ = SWCX_LENGTH/2.0                                          554
         AG_TITLE = 'TUBE'                                                554
         AG_EXNAME = 'SHAPE'                                              554
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      554
         CALL AXSHAPE                                                     554
C    *                                                                    554
5491  CALL AGSPOPB                                                        555
      GO TO BLOCK_SYRU, (5490 ,5500)                                      555
C End of Local Procedure                                                  555
C ----------------------------------------------------------------------  556
C ------------ SOUM                                                       556
C Begin Local Procedure SOUM                                              557
C SOUM IS THE MOTHER OF THE ARRAY OF THE OUTER SHILEDING SUPPORT TUBES    557
5450  CONTINUE                                                            557
         AG_TITLE='SOUM'                                                  557
         AG_MODULE=MODULE                                                 557
         AG_MARK='V00'                                                    557
         CALL AGSPUSH                                                     557
         AG_MODULE=MODULE                                                 557
C    *                                                                    557
C    Geant MATERIAL : AIR ===> inherited arguments used <===              558
         CALL AGSRESET                                                    558
         AG_TITLE = 'AIR'                                                 558
         AG_EXNAME = 'MATERIAL'                                           558
         AG_PARLIST = 'NONE'                                              558
         CALL AXMATERIAL                                                  558
C    *                                                                    558
C    Geant ATTRIBUTE : SOUM ===> SEEN=0 COLO=1 <===                       558
         CALL AGSRESET                                                    559
         AG_SEEN = 0                                                      559
         AG_COLO = 1                                                      559
         AG_TITLE = 'SOUM'                                                559
         AG_EXNAME = 'ATTRIBUTE'                                          559
         AG_PARLIST = 'SEEN_COLO'                                         559
         CALL AXATTRIBUTE                                                 559
C    *                                                                    559
C    Geant SHAPE : TUBS ===> RMIN=SOUP_ROUT-SOUP_DR RMAX=SOUP_ROUT, PHI1  559
C    =SOUP_PHI1 PHI2=SOUP_PHI2 DZ=SOUP_LENGTH/2.0 <===                    559
         CALL AGSRESET                                                    561
         AG_RMIN = SOUP_ROUT-SOUP_DR                                      561
         AG_RMAX = SOUP_ROUT                                              561
         AG_PHI1 = SOUP_PHI1                                              561
         AG_PHI2 = SOUP_PHI2                                              561
         AG_DZ = SOUP_LENGTH/2.0                                          561
         AG_TITLE = 'TUBS'                                                561
         AG_EXNAME = 'SHAPE'                                              561
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            561
         CALL AXSHAPE                                                     561
C    *                                                                    561
C    Create SOUR :                                                        562
C    Call To Local Procedure SOUR                                         562
         ASSIGN 5530 TO BLOCK_SOUR                                        562
         GO TO  5520                                                      562
5530     CONTINUE                                                         562
C    *                                                                    562
C    *                                                                    563
         I_PHI=1                                                          563
            GO TO 5543                                                    563
5541        I_PHI=I_PHI+1                                                 563
5543        IF(I_PHI-(4).GT.0) GO TO 5542                                 563
C       Geant POSITION : SOUR ===> X=COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SO  565
C       UP_ROUT-SOUP_DR/2.0) Y=SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROU  565
C       T-SOUP_DR/2.0) Z=0.0 <===                                         565
            CALL AGSRESET                                                 565
            AG_X = COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    565
     *      2.0)                                                          565
            AG_Y = SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    565
     *      2.0)                                                          565
            AG_Z = 0.0                                                    565
            AG_TITLE = 'SOUR'                                             565
            AG_EXNAME = 'POSITION'                                        565
            AG_PARLIST = 'X_Y_Z'                                          565
            CALL AXPOSITION                                               565
C       *                                                                 565
         GO TO 5541                                                       566
5542     CONTINUE                                                         566
5451  CALL AGSPOPB                                                        567
      GO TO BLOCK_SOUM, (5450 ,5460)                                      567
C End of Local Procedure                                                  567
C ----------------------------------------------------------------------  568
C ------------ SOUR                                                       568
C Begin Local Procedure SOUR                                              569
C SOUR IS THE OUTER SHILEDING SUPPORT TUBES (RODS)                        569
5520  CONTINUE                                                            569
         AG_TITLE='SOUR'                                                  569
         AG_MODULE=MODULE                                                 569
         AG_MARK='V00'                                                    569
         CALL AGSPUSH                                                     569
         AG_MODULE=MODULE                                                 569
C    *                                                                    569
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           570
         CALL AGSRESET                                                    570
         AG_TITLE = 'CARBON'                                              570
         AG_EXNAME = 'MATERIAL'                                           570
         AG_PARLIST = 'NONE'                                              570
         CALL AXMATERIAL                                                  570
C    *                                                                    570
C    Geant ATTRIBUTE : SOUM ===> SEEN=1 COLO=4 <===                       570
         CALL AGSRESET                                                    571
         AG_SEEN = 1                                                      571
         AG_COLO = 4                                                      571
         AG_TITLE = 'SOUM'                                                571
         AG_EXNAME = 'ATTRIBUTE'                                          571
         AG_PARLIST = 'SEEN_COLO'                                         571
         CALL AXATTRIBUTE                                                 571
C    *                                                                    571
C    Geant SHAPE : TUBE ===> RMIN=SOUP_DIAMIN/2.0 RMAX=SOUP_DIAMOUT/2.0,  571
C     DZ=SOUP_LENGTH/2.0 <===                                             571
         CALL AGSRESET                                                    573
         AG_RMIN = SOUP_DIAMIN/2.0                                        573
         AG_RMAX = SOUP_DIAMOUT/2.0                                       573
         AG_DZ = SOUP_LENGTH/2.0                                          573
         AG_TITLE = 'TUBE'                                                573
         AG_EXNAME = 'SHAPE'                                              573
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      573
         CALL AXSHAPE                                                     573
C    *                                                                    573
5521  CALL AGSPOPB                                                        574
      GO TO BLOCK_SOUR, (5520 ,5530)                                      574
C End of Local Procedure                                                  574
C ----------------------------------------------------------------------  575
C ------------ SLYD                                                       575
C Begin Local Procedure SLYD                                              576
C SLYD IS A SINGLE SVT LAYER                                              576
5280  CONTINUE                                                            576
         AG_TITLE='SLYD'                                                  576
         AG_MODULE=MODULE                                                 576
         AG_MARK='V00'                                                    576
         CALL AGSPUSH                                                     576
         AG_MODULE=MODULE                                                 576
C    *                                                                    576
         LADTHK = SWCA_WAFERTHK/2 + SWCA_ROHATHK + SWCA_WAFCARTH          577
         ELETHK=2.0*SELC_BETHK + SELC_WATTHK + SELC_BEOTHK +              578
     *   SELC_AGPDTHK                                                     578
         ELETHK=ELETHK + SELC_GLASSTHK + SELC_DYETHK                      579
         DEG=180.0/SVTL_NLADDER                                           580
         RAD=PI/SVTL_NLADDER                                              581
C    Geant MATERIAL : AIR ===> inherited arguments used <===              582
         CALL AGSRESET                                                    582
         AG_TITLE = 'AIR'                                                 582
         AG_EXNAME = 'MATERIAL'                                           582
         AG_PARLIST = 'NONE'                                              582
         CALL AXMATERIAL                                                  582
C    *                                                                    582
C    Geant ATTRIBUTE : SLYD ===> SEEN=0 COLO=1 <===                       582
         CALL AGSRESET                                                    583
         AG_SEEN = 0                                                      583
         AG_COLO = 1                                                      583
         AG_TITLE = 'SLYD'                                                583
         AG_EXNAME = 'ATTRIBUTE'                                          583
         AG_PARLIST = 'SEEN_COLO'                                         583
         CALL AXATTRIBUTE                                                 583
C    *                                                                    583
         RMIN=SVTL_RADIUS - LADTHK                                        583
         RMAX = SVTL_RADIUS*SVTL_RADIUS                                   585
         RMAX=RMAX+SWCA_WAFERWID*SWCA_WAFERWID/4.0                        586
         RMAX = SQRT(RMAX) + ELETHK                                       587
C    Check (RMAX>RADMAX-LADTHK)                                           588
         IF ((RMAX.GT.RADMAX-LADTHK)) THEN                                588
         ENDIF                                                            589
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   590
C    Check (RMAX<RMIN)                                                    591
         IF ((RMAX.LT.RMIN)) THEN                                         591
         ENDIF                                                            592
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    593
         CALL AGSRESET                                                    593
         AG_RMIN = RMIN                                                   593
         AG_RMAX = RMAX                                                   593
         AG_DZ = SWCA_LENGTH/2                                            593
         AG_TITLE = 'TUBE'                                                593
         AG_EXNAME = 'SHAPE'                                              593
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      593
         CALL AXSHAPE                                                     593
C    *                                                                    593
C    Create SLSD :                                                        594
C    Call To Local Procedure SLSD                                         594
         ASSIGN 5560 TO BLOCK_SLSD                                        594
         GO TO  5550                                                      594
5560     CONTINUE                                                         594
C    *                                                                    594
5281  CALL AGSPOPB                                                        595
      GO TO BLOCK_SLYD, (5280 ,5290,5300)                                 595
C End of Local Procedure                                                  595
C ----------------------------------------------------------------------  596
C ------------ SLSD                                                       596
C Begin Local Procedure SLSD                                              597
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         597
5550  CONTINUE                                                            597
         AG_TITLE='SLSD'                                                  597
         AG_MODULE=MODULE                                                 597
         AG_MARK='V00'                                                    597
         CALL AGSPUSH                                                     597
         AG_MODULE=MODULE                                                 597
C    *                                                                    597
C    Geant ATTRIBUTE : SLSD ===> SEEN=0 COLO=1 <===                       598
         CALL AGSRESET                                                    598
         AG_SEEN = 0                                                      598
         AG_COLO = 1                                                      598
         AG_TITLE = 'SLSD'                                                598
         AG_EXNAME = 'ATTRIBUTE'                                          598
         AG_PARLIST = 'SEEN_COLO'                                         598
         CALL AXATTRIBUTE                                                 598
C    *                                                                    598
C    Check SVTG_NLAYER<0                                                  598
         IF (SVTG_NLAYER.LT.0) THEN                                       599
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               600
         CALL AGSRESET                                                    600
         AG_NDIV = 1                                                      600
         AG_IAXIS = 2                                                     600
         AG_C0 = -90                                                      600
         AG_TITLE = 'DIVISION'                                            600
         AG_EXNAME = 'SHAPE'                                              600
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     600
         CALL AXSHAPE                                                     600
C    *                                                                    600
         ELSE                                                             600
C    Check SVTG_VERSION==1                                                602
         IF (SVTG_VERSION.EQ.1) THEN                                      602
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              603
         ELSE                                                             604
         C0=90-DEG*MOD(ILAYER,2)                                          605
         ENDIF                                                            606
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NLADDER IAXIS=2, C0=C0 <===    608
         CALL AGSRESET                                                    608
         AG_NDIV = SVTL_NLADDER                                           608
         AG_IAXIS = 2                                                     608
         AG_C0 = C0                                                       608
         AG_TITLE = 'DIVISION'                                            608
         AG_EXNAME = 'SHAPE'                                              608
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     608
         CALL AXSHAPE                                                     608
C    *                                                                    608
         ENDIF                                                            608
C    Create SLDI :                                                        610
C    Call To Local Procedure SLDI                                         610
         ASSIGN 5580 TO BLOCK_SLDI                                        610
         GO TO  5570                                                      610
5580     CONTINUE                                                         610
C    *                                                                    610
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ORT=YZX <===                611
         CALL AGSRESET                                                    611
         AG_X = SVTL_RADIUS                                               611
         AG_ALPHAN=AG_ALPHAN+1                                            611
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 611
         AG_TITLE = 'SLDI'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'X_ORT'                                             611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Create SELE :                                                        612
C    Call To Local Procedure SELE                                         612
         ASSIGN 5600 TO BLOCK_SELE                                        612
         GO TO  5590                                                      612
5600     CONTINUE                                                         612
C    *                                                                    612
         DEG=180.0/SVTL_NLADDER                                           613
         RAD=(TWOPI/2)/SVTL_NLADDER                                       614
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 615
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 616
C    *                                                                    617
         S=-1                                                             617
            GOTO 5613                                                     617
5611        S=S+(2)                                                       617
5613        IF((2)*(S-(1)).GT.0) GO TO 5612                               617
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  619
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=S*DEG <===         619
            CALL AGSRESET                                                 619
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                619
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               619
            AG_ALPHAN=AG_ALPHAN+1                                         619
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              619
            AG_ALPHAN=AG_ALPHAN+1                                         619
            AG_ALPHAK(AG_ALPHAN)=1                                        619
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 619
            AG_ALPHAN=AG_ALPHAN+1                                         619
            AG_ALPHAK(AG_ALPHAN)=3                                        619
            AG_ALPHAL(AG_ALPHAN)=S*DEG                                    619
            AG_TITLE = 'SELE'                                             619
            AG_EXNAME = 'POSITION'                                        619
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          619
            CALL AXPOSITION                                               619
C       *                                                                 619
         GO TO 5611                                                       620
5612     CONTINUE                                                         620
5551  CALL AGSPOPB                                                        621
      GO TO BLOCK_SLSD, (5550 ,5560)                                      621
C End of Local Procedure                                                  621
C ----------------------------------------------------------------------  622
C ------------ SLDI                                                       622
C Begin Local Procedure SLDI                                              623
C SLDI IS A LADDER VOLUME                                                 623
5570  CONTINUE                                                            623
         AG_TITLE='SLDI'                                                  623
         AG_MODULE=MODULE                                                 623
         AG_MARK='V00'                                                    623
         CALL AGSPUSH                                                     623
         AG_MODULE=MODULE                                                 623
C    *                                                                    623
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       624
C    Geant MATERIAL : AIR ===> inherited arguments used <===              625
         CALL AGSRESET                                                    625
         AG_TITLE = 'AIR'                                                 625
         AG_EXNAME = 'MATERIAL'                                           625
         AG_PARLIST = 'NONE'                                              625
         CALL AXMATERIAL                                                  625
C    *                                                                    625
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 SERIAL=SVTL_NWAFER <===    625
         CALL AGSRESET                                                    626
         AG_SEEN = 0                                                      626
         AG_COLO = 1                                                      626
         AG_SERIAL = SVTL_NWAFER                                          626
         AG_TITLE = 'SLDI'                                                626
         AG_EXNAME = 'ATTRIBUTE'                                          626
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  626
         CALL AXATTRIBUTE                                                 626
C    *                                                                    626
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  626
C    HK <===                                                              626
         CALL AGSRESET                                                    628
         AG_DX = SWCA_WAFERWID/2                                          628
         AG_DY = SWCA_LENGTH/2                                            628
         AG_DZ = LADTHK                                                   628
         AG_TITLE = 'BOX'                                                 628
         AG_EXNAME = 'SHAPE'                                              628
         AG_PARLIST = 'DX_DY_DZ'                                          628
         CALL AXSHAPE                                                     628
C    *                                                                    628
C    Create STLI :                                                        629
C    Call To Local Procedure STLI                                         629
         ASSIGN 5630 TO BLOCK_STLI                                        629
         GO TO  5620                                                      629
5630     CONTINUE                                                         629
C    *                                                                    629
C    Geant POSITION : STLI ===> inherited arguments used <===             630
         CALL AGSRESET                                                    630
         AG_TITLE = 'STLI'                                                630
         AG_EXNAME = 'POSITION'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXPOSITION                                                  630
C    *                                                                    630
C    Create SBER :                                                        631
C    Call To Local Procedure SBER                                         631
         ASSIGN 5650 TO BLOCK_SBER                                        631
         GO TO  5640                                                      631
5650     CONTINUE                                                         631
C    *                                                                    631
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  632
C    DTHK+SWCA_WAFCARTH/2 <===                                            632
         CALL AGSRESET                                                    632
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          632
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   632
         AG_TITLE = 'SBER'                                                632
         AG_EXNAME = 'POSITION'                                           632
         AG_PARLIST = 'X_Z'                                               632
         CALL AXPOSITION                                                  632
C    *                                                                    632
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  632
C    DTHK+SWCA_WAFCARTH/2 <===                                            632
         CALL AGSRESET                                                    633
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          633
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   633
         AG_TITLE = 'SBER'                                                633
         AG_EXNAME = 'POSITION'                                           633
         AG_PARLIST = 'X_Z'                                               633
         CALL AXPOSITION                                                  633
C    *                                                                    633
C    Create STAB :                                                        634
C    Call To Local Procedure STAB                                         634
         ASSIGN 5670 TO BLOCK_STAB                                        634
         GO TO  5660                                                      634
5670     CONTINUE                                                         634
C    *                                                                    634
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  635
C    WAFCARTH/2 <===                                                      635
         CALL AGSRESET                                                    635
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    635
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   635
         AG_TITLE = 'STAB'                                                635
         AG_EXNAME = 'POSITION'                                           635
         AG_PARLIST = 'Y_Z'                                               635
         CALL AXPOSITION                                                  635
C    *                                                                    635
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  635
C    _WAFCARTH/2 <===                                                     635
         CALL AGSRESET                                                    636
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   636
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   636
         AG_TITLE = 'STAB'                                                636
         AG_EXNAME = 'POSITION'                                           636
         AG_PARLIST = 'Y_Z'                                               636
         CALL AXPOSITION                                                  636
C    *                                                                    636
C    Create STRU :                                                        637
C    Call To Local Procedure STRU                                         637
         ASSIGN 5690 TO BLOCK_STRU                                        637
         GO TO  5680                                                      637
5690     CONTINUE                                                         637
C    *                                                                    637
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  640
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  640
C    =                                                                    640
         CALL AGSRESET                                                    640
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              640
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  640
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   640
         AG_TITLE = 'STRU'                                                640
         AG_EXNAME = 'POSITION'                                           640
         AG_PARLIST = 'Y_Z'                                               640
         CALL AXPOSITION                                                  640
C    *                                                                    640
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  640
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  640
C    ==                                                                   640
         CALL AGSRESET                                                    643
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             643
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  643
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   643
         AG_TITLE = 'STRU'                                                643
         AG_EXNAME = 'POSITION'                                           643
         AG_PARLIST = 'Y_Z'                                               643
         CALL AXPOSITION                                                  643
C    *                                                                    643
C    Create SRHC :                                                        644
C    Call To Local Procedure SRHC                                         644
         ASSIGN 5710 TO BLOCK_SRHC                                        644
         GO TO  5700                                                      644
5710     CONTINUE                                                         644
C    *                                                                    644
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  646
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         646
         CALL AGSRESET                                                    646
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          646
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                646
         AG_TITLE = 'SRHC'                                                646
         AG_EXNAME = 'POSITION'                                           646
         AG_PARLIST = 'X_Z'                                               646
         CALL AXPOSITION                                                  646
C    *                                                                    646
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  646
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         646
         CALL AGSRESET                                                    648
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          648
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                648
         AG_TITLE = 'SRHC'                                                648
         AG_EXNAME = 'POSITION'                                           648
         AG_PARLIST = 'X_Z'                                               648
         CALL AXPOSITION                                                  648
C    *                                                                    648
C    Create SPCB :                                                        649
C    Call To Local Procedure SPCB                                         649
         ASSIGN 5730 TO BLOCK_SPCB                                        649
         GO TO  5720                                                      649
5730     CONTINUE                                                         649
C    *                                                                    649
         YPCB = SVTL_NWAFER*(SWCA_WAFERLEN + SWCA_WAFERGAP)/2.0 +         650
     *   SVTL_PCBGAP + SVTL_PCBLEN/2.0                                    650
C    Geant POSITION : SPCB ===> X=0.0 Y=YPCB Z=SVTL_PCBTHK/2.0 <===       651
         CALL AGSRESET                                                    651
         AG_X = 0.0                                                       651
         AG_Y = YPCB                                                      651
         AG_Z = SVTL_PCBTHK/2.0                                           651
         AG_TITLE = 'SPCB'                                                651
         AG_EXNAME = 'POSITION'                                           651
         AG_PARLIST = 'X_Y_Z'                                             651
         CALL AXPOSITION                                                  651
C    *                                                                    651
C    Geant POSITION : SPCB ===> X=0.0 Y=-YPCB Z=SVTL_PCBTHK/2.0 ALPHAZ=1  651
C    80.0 <===                                                            651
         CALL AGSRESET                                                    652
         AG_X = 0.0                                                       652
         AG_Y = -YPCB                                                     652
         AG_Z = SVTL_PCBTHK/2.0                                           652
         AG_ALPHAN=AG_ALPHAN+1                                            652
         AG_ALPHAK(AG_ALPHAN)=3                                           652
         AG_ALPHAL(AG_ALPHAN)=180.0                                       652
         AG_TITLE = 'SPCB'                                                652
         AG_EXNAME = 'POSITION'                                           652
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      652
         CALL AXPOSITION                                                  652
C    *                                                                    652
5571  CALL AGSPOPB                                                        653
      GO TO BLOCK_SLDI, (5570 ,5580)                                      653
C End of Local Procedure                                                  653
C ----------------------------------------------------------------------  654
C ------------ SRHC                                                       654
C Begin Local Procedure SRHC                                              655
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     655
5700  CONTINUE                                                            655
         AG_TITLE='SRHC'                                                  655
         AG_MODULE=MODULE                                                 655
         AG_MARK='V00'                                                    655
         CALL AGSPUSH                                                     655
         AG_MODULE=MODULE                                                 655
C    *                                                                    655
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           656
         CALL AGSRESET                                                    656
         AG_A = 12                                                        656
         AG_Z = 6                                                         656
         AG_W = 1                                                         656
         AG_TITLE = 'C'                                                   656
         AG_EXNAME = 'COMPONENT'                                          656
         AG_PARLIST = 'A_Z_W'                                             656
         CALL AXCOMPONENT                                                 656
C    *                                                                    656
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           656
         CALL AGSRESET                                                    657
         AG_A = 1                                                         657
         AG_Z = 1                                                         657
         AG_W = 2                                                         657
         AG_TITLE = 'H2'                                                  657
         AG_EXNAME = 'COMPONENT'                                          657
         AG_PARLIST = 'A_Z_W'                                             657
         CALL AXCOMPONENT                                                 657
C    *                                                                    657
C    Geant MIXTURE : ROHA ===> DENS=0.0304 <===                           657
         CALL AGSRESET                                                    658
         AG_DENS = 0.0304                                                 658
         AG_TITLE = 'ROHA'                                                658
         AG_EXNAME = 'MIXTURE'                                            658
         AG_PARLIST = 'DENS'                                              658
         CALL AXMIXTURE                                                   658
C    *                                                                    658
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       658
         CALL AGSRESET                                                    659
         AG_SEEN = 1                                                      659
         AG_COLO = 3                                                      659
         AG_TITLE = 'SRHC'                                                659
         AG_EXNAME = 'ATTRIBUTE'                                          659
         AG_PARLIST = 'SEEN_COLO'                                         659
         CALL AXATTRIBUTE                                                 659
C    *                                                                    659
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  659
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        659
         CALL AGSRESET                                                    661
         AG_DX = SWCA_WAFCARWD/2                                          661
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              661
         AG_DZ = SWCA_ROHATHK/2                                           661
         AG_TITLE = 'BOX'                                                 661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'DX_DY_DZ'                                          661
         CALL AXSHAPE                                                     661
C    *                                                                    661
5701  CALL AGSPOPB                                                        662
      GO TO BLOCK_SRHC, (5700 ,5710)                                      662
C End of Local Procedure                                                  662
C ----------------------------------------------------------------------  663
C ------------ STLI                                                       663
C Begin Local Procedure STLI                                              664
C STLI IS THE WAFER PACK CONTAINER                                        664
5620  CONTINUE                                                            664
         AG_TITLE='STLI'                                                  664
         AG_MODULE=MODULE                                                 664
         AG_MARK='V00'                                                    664
         CALL AGSPUSH                                                     664
         AG_MODULE=MODULE                                                 664
C    *                                                                    664
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              665
         CALL AGSRESET                                                    665
         AG_SEEN = 0                                                      665
         AG_COLO = 1                                                      665
         AG_SERIAL = 0                                                    665
         AG_TITLE = 'STLI'                                                665
         AG_EXNAME = 'ATTRIBUTE'                                          665
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  665
         CALL AXATTRIBUTE                                                 665
C    *                                                                    665
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  665
C    P), DZ=SWCA_WAFERTHK/2 <===                                          665
         CALL AGSRESET                                                    667
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              667
         AG_DZ = SWCA_WAFERTHK/2                                          667
         AG_TITLE = 'BOX'                                                 667
         AG_EXNAME = 'SHAPE'                                              667
         AG_PARLIST = 'DY_DZ'                                             667
         CALL AXSHAPE                                                     667
C    *                                                                    667
C    Create STSI :                                                        668
C    Call To Local Procedure STSI                                         668
         ASSIGN 5750 TO BLOCK_STSI                                        668
         GO TO  5740                                                      668
5750     CONTINUE                                                         668
C    *                                                                    668
5621  CALL AGSPOPB                                                        669
      GO TO BLOCK_STLI, (5620 ,5630)                                      669
C End of Local Procedure                                                  669
C ----------------------------------------------------------------------  670
C ------------ STSI                                                       670
C Begin Local Procedure STSI                                              671
C STSI IS A SINGLE WAVER CONTAINER                                        671
5740  CONTINUE                                                            671
         AG_TITLE='STSI'                                                  671
         AG_MODULE=MODULE                                                 671
         AG_MARK='V00'                                                    671
         CALL AGSPUSH                                                     671
         AG_MODULE=MODULE                                                 671
C    *                                                                    671
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            672
         CALL AGSRESET                                                    672
         AG_NDIV = SVTL_NWAFER                                            672
         AG_IAXIS = 2                                                     672
         AG_TITLE = 'DIVISION'                                            672
         AG_EXNAME = 'SHAPE'                                              672
         AG_PARLIST = 'NDIV_IAXIS'                                        672
         CALL AXSHAPE                                                     672
C    *                                                                    672
C    Create SVTD :                                                        673
C    Call To Local Procedure SVTD                                         673
         ASSIGN 5770 TO BLOCK_SVTD                                        673
         GO TO  5760                                                      673
5770     CONTINUE                                                         673
C    *                                                                    673
C    Geant POSITION : SVTD ===> inherited arguments used <===             674
         CALL AGSRESET                                                    674
         AG_TITLE = 'SVTD'                                                674
         AG_EXNAME = 'POSITION'                                           674
         AG_PARLIST = 'NONE'                                              674
         CALL AXPOSITION                                                  674
C    *                                                                    674
5741  CALL AGSPOPB                                                        675
      GO TO BLOCK_STSI, (5740 ,5750)                                      675
C End of Local Procedure                                                  675
C ----------------------------------------------------------------------  676
C ------------ SVTD                                                       676
C Begin Local Procedure SVTD                                              677
C SVTD IS AN ACTIVE WAFER VOLUME                                          677
5760  CONTINUE                                                            677
         AG_TITLE='SVTD'                                                  677
         AG_MODULE=MODULE                                                 677
         AG_MARK='V00'                                                    677
         CALL AGSPUSH                                                     677
         AG_MODULE=MODULE                                                 677
C    *                                                                    677
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          678
         CALL AGSRESET                                                    678
         AG_TITLE = 'SILICON'                                             678
         AG_EXNAME = 'MATERIAL'                                           678
         AG_PARLIST = 'NONE'                                              678
         CALL AXMATERIAL                                                  678
C    *                                                                    678
C    Geant MATERIAL : SENSSI ===> ISVOL=1 <===                            678
         CALL AGSRESET                                                    679
         AG_ISVOL = 1                                                     679
         AG_TITLE = 'SENSSI'                                              679
         AG_EXNAME = 'MATERIAL'                                           679
         AG_PARLIST = 'ISVOL'                                             679
         CALL AXMATERIAL                                                  679
C    *                                                                    679
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       679
         CALL AGSRESET                                                    680
         AG_SEEN = 1                                                      680
         AG_COLO = 4                                                      680
         AG_TITLE = 'SVTD'                                                680
         AG_EXNAME = 'ATTRIBUTE'                                          680
         AG_PARLIST = 'SEEN_COLO'                                         680
         CALL AXATTRIBUTE                                                 680
C    *                                                                    680
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       680
         CALL AGSRESET                                                    681
         AG_DY = SWCA_WAFERLEN/2                                          681
         AG_TITLE = 'BOX'                                                 681
         AG_EXNAME = 'SHAPE'                                              681
         AG_PARLIST = 'DY'                                                681
         CALL AXSHAPE                                                     681
C    *                                                                    681
         TRAPY=SWCA_SENSWID/2.0 - (SWCA_SENSWID - SWCA_SENSGIR)/2.0       681
C    Create STRA :                                                        683
C    Call To Local Procedure STRA                                         683
         ASSIGN 5790 TO BLOCK_STRA                                        683
         GO TO  5780                                                      683
5790     CONTINUE                                                         683
C    *                                                                    683
C    Geant POSITION : STRA ===> Y=+TRAPY ALPHAZ=+90 ORT=YZX <===          684
         CALL AGSRESET                                                    684
         AG_Y = +TRAPY                                                    684
         AG_ALPHAN=AG_ALPHAN+1                                            684
         AG_ALPHAK(AG_ALPHAN)=3                                           684
         AG_ALPHAL(AG_ALPHAN)=+90                                         684
         AG_ALPHAN=AG_ALPHAN+1                                            684
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 684
         AG_TITLE = 'STRA'                                                684
         AG_EXNAME = 'POSITION'                                           684
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      684
         CALL AXPOSITION                                                  684
C    *                                                                    684
C    Geant POSITION : STRA ===> Y=-TRAPY ALPHAZ=-90 ORT=YZX <===          684
         CALL AGSRESET                                                    685
         AG_Y = -TRAPY                                                    685
         AG_ALPHAN=AG_ALPHAN+1                                            685
         AG_ALPHAK(AG_ALPHAN)=3                                           685
         AG_ALPHAL(AG_ALPHAN)=-90                                         685
         AG_ALPHAN=AG_ALPHAN+1                                            685
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 685
         AG_TITLE = 'STRA'                                                685
         AG_EXNAME = 'POSITION'                                           685
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      685
         CALL AXPOSITION                                                  685
C    *                                                                    685
         SSIDX=SWCA_WAFERLEN/2.0 - (SWCA_WAFERLEN - SWCA_SENSLEN)/4.0     685
         SSIRY=SWCA_WAFERWID/2.0 - (SWCA_WAFERWID - SWCA_SENSWID)/4.0     687
C    Create SSID :                                                        688
C    Call To Local Procedure SSID                                         688
         ASSIGN 5810 TO BLOCK_SSID                                        688
         GO TO  5800                                                      688
5810     CONTINUE                                                         688
C    *                                                                    688
C    Geant POSITION : SSID ===> X=+SSIDX <===                             689
         CALL AGSRESET                                                    689
         AG_X = +SSIDX                                                    689
         AG_TITLE = 'SSID'                                                689
         AG_EXNAME = 'POSITION'                                           689
         AG_PARLIST = 'X'                                                 689
         CALL AXPOSITION                                                  689
C    *                                                                    689
C    Geant POSITION : SSID ===> X=-SSIDX <===                             689
         CALL AGSRESET                                                    690
         AG_X = -SSIDX                                                    690
         AG_TITLE = 'SSID'                                                690
         AG_EXNAME = 'POSITION'                                           690
         AG_PARLIST = 'X'                                                 690
         CALL AXPOSITION                                                  690
C    *                                                                    690
C    Create SSIR :                                                        691
C    Call To Local Procedure SSIR                                         691
         ASSIGN 5830 TO BLOCK_SSIR                                        691
         GO TO  5820                                                      691
5830     CONTINUE                                                         691
C    *                                                                    691
C    Geant POSITION : SSIR ===> Y=+SSIRY <===                             692
         CALL AGSRESET                                                    692
         AG_Y = +SSIRY                                                    692
         AG_TITLE = 'SSIR'                                                692
         AG_EXNAME = 'POSITION'                                           692
         AG_PARLIST = 'Y'                                                 692
         CALL AXPOSITION                                                  692
C    *                                                                    692
C    Geant POSITION : SSIR ===> Y=-SSIRY <===                             692
         CALL AGSRESET                                                    693
         AG_Y = -SSIRY                                                    693
         AG_TITLE = 'SSIR'                                                693
         AG_EXNAME = 'POSITION'                                           693
         AG_PARLIST = 'Y'                                                 693
         CALL AXPOSITION                                                  693
C    *                                                                    693
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 693
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  696
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  696
C    .,0.01)                                                              696
         AG_TITLE = 'SVTD'                                                696
         AG_EXNAME = 'HITS'                                               696
         AG_TYPE=AG_MAGIC                                                 696
         CALL AGSDETV('SVTD')                                             696
C    *                                                                    696
         AG_PARLIST='Z'                                                   696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = AG_MAGIC                                              696
         AG_BIN=.001                                                      696
         AG_OPTION='S'                                                    696
         AG_FHMIN = AG_MAGIC                                              696
         AG_FHMAX = AG_MAGIC                                              696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='Y'                                                   696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = AG_MAGIC                                              696
         AG_BIN=.001                                                      696
         AG_FHMIN=AG_MAGIC                                                696
         AG_FHMAX=AG_MAGIC                                                696
         AG_OPTION='*'                                                    696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='X'                                                   696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = AG_MAGIC                                              696
         AG_BIN=.001                                                      696
         AG_FHMIN=AG_MAGIC                                                696
         AG_FHMAX=AG_MAGIC                                                696
         AG_OPTION='*'                                                    696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='PTOT'                                                696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = 18                                                    696
         AG_BIN=AG_MAGIC                                                  696
         AG_OPTION='*'                                                    696
         AG_FHMIN = 0                                                     696
         AG_FHMAX = 100                                                   696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='CX'                                                  696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = 10                                                    696
         AG_BIN=AG_MAGIC                                                  696
         AG_FHMIN=AG_MAGIC                                                696
         AG_FHMAX=AG_MAGIC                                                696
         AG_OPTION='*'                                                    696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='CY'                                                  696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = 10                                                    696
         AG_BIN=AG_MAGIC                                                  696
         AG_FHMIN=AG_MAGIC                                                696
         AG_FHMAX=AG_MAGIC                                                696
         AG_OPTION='*'                                                    696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='CZ'                                                  696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = 10                                                    696
         AG_BIN=AG_MAGIC                                                  696
         AG_FHMIN=AG_MAGIC                                                696
         AG_FHMAX=AG_MAGIC                                                696
         AG_OPTION='*'                                                    696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='SLENG'                                               696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = 16                                                    696
         AG_BIN=AG_MAGIC                                                  696
         AG_OPTION='*'                                                    696
         AG_FHMIN = 0                                                     696
         AG_FHMAX = 500                                                   696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='TOF'                                                 696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = 16                                                    696
         AG_BIN=AG_MAGIC                                                  696
         AG_OPTION='*'                                                    696
         AG_FHMIN = 0                                                     696
         AG_FHMAX = 1.0E-6                                                696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='STEP'                                                696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = AG_MAGIC                                              696
         AG_BIN=.01                                                       696
         AG_FHMIN=AG_MAGIC                                                696
         AG_FHMAX=AG_MAGIC                                                696
         AG_OPTION='*'                                                    696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         AG_PARLIST='ELOSS'                                               696
         AG_TITLE = 'DETH'                                                696
         AG_NBITS = 22                                                    696
         AG_BIN=AG_MAGIC                                                  696
         AG_OPTION='*'                                                    696
         AG_FHMIN = 0.                                                    696
         AG_FHMAX = 0.01                                                  696
         CALL AGSDETH (0)                                                 696
C    *                                                                    696
         CALL AGSDETU('SVTD')                                             696
C    *                                                                    696
5761  CALL AGSPOPB                                                        697
      GO TO BLOCK_SVTD, (5760 ,5770)                                      697
C End of Local Procedure                                                  697
C ----------------------------------------------------------------------  698
C ------------ SSID                                                       698
C Begin Local Procedure SSID                                              699
C SSID IS A NON-SENSITIVE LEFT-RIGHT BORDER OF THE WAFER                  699
5800  CONTINUE                                                            699
         AG_TITLE='SSID'                                                  699
         AG_MODULE=MODULE                                                 699
         AG_MARK='V00'                                                    699
         CALL AGSPUSH                                                     699
         AG_MODULE=MODULE                                                 699
C    *                                                                    699
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          700
         CALL AGSRESET                                                    700
         AG_TITLE = 'SILICON'                                             700
         AG_EXNAME = 'MATERIAL'                                           700
         AG_PARLIST = 'NONE'                                              700
         CALL AXMATERIAL                                                  700
C    *                                                                    700
C    Geant MATERIAL : NONSENSSI ===> ISVOL=0 <===                         700
         CALL AGSRESET                                                    701
         AG_ISVOL = 0                                                     701
         AG_TITLE = 'NONSENSSI'                                           701
         AG_EXNAME = 'MATERIAL'                                           701
         AG_PARLIST = 'ISVOL'                                             701
         CALL AXMATERIAL                                                  701
C    *                                                                    701
C    Geant ATTRIBUTE : SSID ===> SEEN=1 COLO=1 <===                       701
         CALL AGSRESET                                                    702
         AG_SEEN = 1                                                      702
         AG_COLO = 1                                                      702
         AG_TITLE = 'SSID'                                                702
         AG_EXNAME = 'ATTRIBUTE'                                          702
         AG_PARLIST = 'SEEN_COLO'                                         702
         CALL AXATTRIBUTE                                                 702
C    *                                                                    702
C    Geant SHAPE : BOX ===> DX=(SWCA_WAFERLEN-SWCA_SENSLEN)/4.0, DY=SWCA  702
C    _WAFERWID/2.0 <===                                                   702
         CALL AGSRESET                                                    704
         AG_DX = (SWCA_WAFERLEN-SWCA_SENSLEN)/4.0                         704
         AG_DY = SWCA_WAFERWID/2.0                                        704
         AG_TITLE = 'BOX'                                                 704
         AG_EXNAME = 'SHAPE'                                              704
         AG_PARLIST = 'DX_DY'                                             704
         CALL AXSHAPE                                                     704
C    *                                                                    704
5801  CALL AGSPOPB                                                        705
      GO TO BLOCK_SSID, (5800 ,5810)                                      705
C End of Local Procedure                                                  705
C ----------------------------------------------------------------------  706
C ------------ SSIR                                                       706
C Begin Local Procedure SSIR                                              707
C SSIR IS A NON-SENSITIVE UP-DOWN BORDER OF THE WAFER                     707
5820  CONTINUE                                                            707
         AG_TITLE='SSIR'                                                  707
         AG_MODULE=MODULE                                                 707
         AG_MARK='V00'                                                    707
         CALL AGSPUSH                                                     707
         AG_MODULE=MODULE                                                 707
C    *                                                                    707
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          708
         CALL AGSRESET                                                    708
         AG_TITLE = 'SILICON'                                             708
         AG_EXNAME = 'MATERIAL'                                           708
         AG_PARLIST = 'NONE'                                              708
         CALL AXMATERIAL                                                  708
C    *                                                                    708
C    Geant MATERIAL : NONSENSSIL ===> ISVOL=0 <===                        708
         CALL AGSRESET                                                    709
         AG_ISVOL = 0                                                     709
         AG_TITLE = 'NONSENSSIL'                                          709
         AG_EXNAME = 'MATERIAL'                                           709
         AG_PARLIST = 'ISVOL'                                             709
         CALL AXMATERIAL                                                  709
C    *                                                                    709
C    Geant ATTRIBUTE : SSIR ===> SEEN=1 COLO=1 <===                       709
         CALL AGSRESET                                                    710
         AG_SEEN = 1                                                      710
         AG_COLO = 1                                                      710
         AG_TITLE = 'SSIR'                                                710
         AG_EXNAME = 'ATTRIBUTE'                                          710
         AG_PARLIST = 'SEEN_COLO'                                         710
         CALL AXATTRIBUTE                                                 710
C    *                                                                    710
C    Geant SHAPE : BOX ===> DX=SWCA_SENSLEN/2.0 DY=(SWCA_WAFERWID-SWCA_S  710
C    ENSWID)/4.0 <===                                                     710
         CALL AGSRESET                                                    711
         AG_DX = SWCA_SENSLEN/2.0                                         711
         AG_DY = (SWCA_WAFERWID-SWCA_SENSWID)/4.0                         711
         AG_TITLE = 'BOX'                                                 711
         AG_EXNAME = 'SHAPE'                                              711
         AG_PARLIST = 'DX_DY'                                             711
         CALL AXSHAPE                                                     711
C    *                                                                    711
5821  CALL AGSPOPB                                                        712
      GO TO BLOCK_SSIR, (5820 ,5830)                                      712
C End of Local Procedure                                                  712
C ----------------------------------------------------------------------  713
C ------------ STRA                                                       713
C Begin Local Procedure STRA                                              714
C STRA IS A TRAPEZOID OF TRIANGULAR SHAPE                                 714
5780  CONTINUE                                                            714
         AG_TITLE='STRA'                                                  714
         AG_MODULE=MODULE                                                 714
         AG_MARK='V00'                                                    714
         CALL AGSPUSH                                                     714
         AG_MODULE=MODULE                                                 714
C    *                                                                    714
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          715
         CALL AGSRESET                                                    715
         AG_TITLE = 'SILICON'                                             715
         AG_EXNAME = 'MATERIAL'                                           715
         AG_PARLIST = 'NONE'                                              715
         CALL AXMATERIAL                                                  715
C    *                                                                    715
C    Geant MATERIAL : NONSENSSILI ===> ISVOL=0 <===                       715
         CALL AGSRESET                                                    716
         AG_ISVOL = 0                                                     716
         AG_TITLE = 'NONSENSSILI'                                         716
         AG_EXNAME = 'MATERIAL'                                           716
         AG_PARLIST = 'ISVOL'                                             716
         CALL AXMATERIAL                                                  716
C    *                                                                    716
C    Geant ATTRIBUTE : STRA ===> SEEN=1 COLO=2 <===                       716
         CALL AGSRESET                                                    717
         AG_SEEN = 1                                                      717
         AG_COLO = 2                                                      717
         AG_TITLE = 'STRA'                                                717
         AG_EXNAME = 'ATTRIBUTE'                                          717
         AG_PARLIST = 'SEEN_COLO'                                         717
         CALL AXATTRIBUTE                                                 717
C    *                                                                    717
C    Geant SHAPE : TRD1 ===> DX1=0.0 DX2=SWCA_SENSWID/2.0, DY=SWCA_WAFER  717
C    THK/2.0 DZ=(SWCA_SENSLEN-SWCA_SENSGIR)/2.0 <===                      717
         CALL AGSRESET                                                    719
         AG_DX1 = 0.0                                                     719
         AG_DX2 = SWCA_SENSWID/2.0                                        719
         AG_DY = SWCA_WAFERTHK/2.0                                        719
         AG_DZ = (SWCA_SENSLEN-SWCA_SENSGIR)/2.0                          719
         AG_TITLE = 'TRD1'                                                719
         AG_EXNAME = 'SHAPE'                                              719
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     719
         CALL AXSHAPE                                                     719
C    *                                                                    719
5781  CALL AGSPOPB                                                        720
      GO TO BLOCK_STRA, (5780 ,5790)                                      720
C End of Local Procedure                                                  720
C ----------------------------------------------------------------------  721
C ------------ SBER                                                       721
C Begin Local Procedure SBER                                              722
C SBER ARE THE BERILLIUM WAFER CARRIER RAILS                              722
5640  CONTINUE                                                            722
         AG_TITLE='SBER'                                                  722
         AG_MODULE=MODULE                                                 722
         AG_MARK='V00'                                                    722
         CALL AGSPUSH                                                     722
         AG_MODULE=MODULE                                                 722
C    *                                                                    722
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        723
         CALL AGSRESET                                                    723
         AG_TITLE = 'BERILLIUM'                                           723
         AG_EXNAME = 'MATERIAL'                                           723
         AG_PARLIST = 'NONE'                                              723
         CALL AXMATERIAL                                                  723
C    *                                                                    723
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       723
         CALL AGSRESET                                                    724
         AG_SEEN = 1                                                      724
         AG_COLO = 2                                                      724
         AG_TITLE = 'SBER'                                                724
         AG_EXNAME = 'ATTRIBUTE'                                          724
         AG_PARLIST = 'SEEN_COLO'                                         724
         CALL AXATTRIBUTE                                                 724
C    *                                                                    724
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  724
C    _WAFCARTH/2 <===                                                     724
         CALL AGSRESET                                                    726
         AG_DX = SWCA_WAFCARWD/2                                          726
         AG_DY = SWCA_LENGTH/2                                            726
         AG_DZ = SWCA_WAFCARTH/2                                          726
         AG_TITLE = 'BOX'                                                 726
         AG_EXNAME = 'SHAPE'                                              726
         AG_PARLIST = 'DX_DY_DZ'                                          726
         CALL AXSHAPE                                                     726
C    *                                                                    726
5641  CALL AGSPOPB                                                        727
      GO TO BLOCK_SBER, (5640 ,5650)                                      727
C End of Local Procedure                                                  727
C ----------------------------------------------------------------------  728
C ------------ STAB                                                       728
C Begin Local Procedure STAB                                              729
C STAB ARE THE BERILLIUM WAFER CARRIER END TABS                           729
5660  CONTINUE                                                            729
         AG_TITLE='STAB'                                                  729
         AG_MODULE=MODULE                                                 729
         AG_MARK='V00'                                                    729
         CALL AGSPUSH                                                     729
         AG_MODULE=MODULE                                                 729
C    *                                                                    729
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        730
         CALL AGSRESET                                                    730
         AG_TITLE = 'BERILLIUM'                                           730
         AG_EXNAME = 'MATERIAL'                                           730
         AG_PARLIST = 'NONE'                                              730
         CALL AXMATERIAL                                                  730
C    *                                                                    730
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       730
         CALL AGSRESET                                                    731
         AG_SEEN = 1                                                      731
         AG_COLO = 2                                                      731
         AG_TITLE = 'STAB'                                                731
         AG_EXNAME = 'ATTRIBUTE'                                          731
         AG_PARLIST = 'SEEN_COLO'                                         731
         CALL AXATTRIBUTE                                                 731
C    *                                                                    731
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  731
C    2 DZ=SWCA_WAFCARTH/2 <===                                            731
         CALL AGSRESET                                                    733
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            733
         AG_DY = TABLEN/2                                                 733
         AG_DZ = SWCA_WAFCARTH/2                                          733
         AG_TITLE = 'BOX'                                                 733
         AG_EXNAME = 'SHAPE'                                              733
         AG_PARLIST = 'DX_DY_DZ'                                          733
         CALL AXSHAPE                                                     733
C    *                                                                    733
5661  CALL AGSPOPB                                                        734
      GO TO BLOCK_STAB, (5660 ,5670)                                      734
C End of Local Procedure                                                  734
C ----------------------------------------------------------------------  735
C ------------ STRU                                                       735
C Begin Local Procedure STRU                                              736
C STRU ARE THE BERILLIUM STRUTS BETWEEN THE WAFER CARRIER RAILS           736
5680  CONTINUE                                                            736
         AG_TITLE='STRU'                                                  736
         AG_MODULE=MODULE                                                 736
         AG_MARK='V00'                                                    736
         CALL AGSPUSH                                                     736
         AG_MODULE=MODULE                                                 736
C    *                                                                    736
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        737
         CALL AGSRESET                                                    737
         AG_TITLE = 'BERILLIUM'                                           737
         AG_EXNAME = 'MATERIAL'                                           737
         AG_PARLIST = 'NONE'                                              737
         CALL AXMATERIAL                                                  737
C    *                                                                    737
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       737
         CALL AGSRESET                                                    738
         AG_SEEN = 1                                                      738
         AG_COLO = 2                                                      738
         AG_TITLE = 'STRU'                                                738
         AG_EXNAME = 'ATTRIBUTE'                                          738
         AG_PARLIST = 'SEEN_COLO'                                         738
         CALL AXATTRIBUTE                                                 738
C    *                                                                    738
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  738
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     738
         CALL AGSRESET                                                    740
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            740
         AG_DY = SWCA_STRUTLEN/2                                          740
         AG_DZ = SWCA_WAFCARTH/2                                          740
         AG_TITLE = 'BOX'                                                 740
         AG_EXNAME = 'SHAPE'                                              740
         AG_PARLIST = 'DX_DY_DZ'                                          740
         CALL AXSHAPE                                                     740
C    *                                                                    740
5681  CALL AGSPOPB                                                        741
      GO TO BLOCK_STRU, (5680 ,5690)                                      741
C End of Local Procedure                                                  741
C ----------------------------------------------------------------------  742
C ------------ SPCB                                                       742
C Begin Local Procedure SPCB                                              743
C SPCB IS THE G10 PCB                                                     743
5720  CONTINUE                                                            743
         AG_TITLE='SPCB'                                                  743
         AG_MODULE=MODULE                                                 743
         AG_MARK='V00'                                                    743
         CALL AGSPUSH                                                     743
         AG_MODULE=MODULE                                                 743
C    *                                                                    743
C    Geant MATERIAL : G10 ===> inherited arguments used <===              744
         CALL AGSRESET                                                    744
         AG_TITLE = 'G10'                                                 744
         AG_EXNAME = 'MATERIAL'                                           744
         AG_PARLIST = 'NONE'                                              744
         CALL AXMATERIAL                                                  744
C    *                                                                    744
C    Geant ATTRIBUTE : SPCB ===> SEEN=1 COLO=5 <===                       744
         CALL AGSRESET                                                    745
         AG_SEEN = 1                                                      745
         AG_COLO = 5                                                      745
         AG_TITLE = 'SPCB'                                                745
         AG_EXNAME = 'ATTRIBUTE'                                          745
         AG_PARLIST = 'SEEN_COLO'                                         745
         CALL AXATTRIBUTE                                                 745
C    *                                                                    745
C    Geant SHAPE : BOX ===> DX=SVTL_PCBWIDTH/2.0 DY=SVTL_PCBLEN/2.0, DZ=  745
C    SVTL_PCBTHK/2.0 <===                                                 745
         CALL AGSRESET                                                    747
         AG_DX = SVTL_PCBWIDTH/2.0                                        747
         AG_DY = SVTL_PCBLEN/2.0                                          747
         AG_DZ = SVTL_PCBTHK/2.0                                          747
         AG_TITLE = 'BOX'                                                 747
         AG_EXNAME = 'SHAPE'                                              747
         AG_PARLIST = 'DX_DY_DZ'                                          747
         CALL AXSHAPE                                                     747
C    *                                                                    747
5721  CALL AGSPOPB                                                        748
      GO TO BLOCK_SPCB, (5720 ,5730)                                      748
C End of Local Procedure                                                  748
C ----------------------------------------------------------------------  749
C ------------ SCBM                                                       749
C Begin Local Procedure SCBM                                              750
C SCBM IS THE MOTHER FOR THE BUNDLE OF CABLES GOING FROM PCBS             750
5390  CONTINUE                                                            750
         AG_TITLE='SCBM'                                                  750
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
C    Geant ATTRIBUTE : SCBM ===> SEEN=0 COLO=1 <===                       751
         CALL AGSRESET                                                    752
         AG_SEEN = 0                                                      752
         AG_COLO = 1                                                      752
         AG_TITLE = 'SCBM'                                                752
         AG_EXNAME = 'ATTRIBUTE'                                          752
         AG_PARLIST = 'SEEN_COLO'                                         752
         CALL AXATTRIBUTE                                                 752
C    *                                                                    752
C    Geant SHAPE : TUBE ===> RMIN=RADII(1) RMAX=SWAM_RMAX, DZ=SCBP_LEN <  752
C    ===                                                                  752
         CALL AGSRESET                                                    754
         AG_RMIN = RADII(1)                                               754
         AG_RMAX = SWAM_RMAX                                              754
         AG_DZ = SCBP_LEN                                                 754
         AG_TITLE = 'TUBE'                                                754
         AG_EXNAME = 'SHAPE'                                              754
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      754
         CALL AXSHAPE                                                     754
C    *                                                                    754
C    *                                                                    754
         ILAYER=1                                                         755
            GO TO 5843                                                    755
5841        ILAYER=ILAYER+1                                               755
5843        IF(ILAYER-(4).GT.0) GO TO 5842                                755
C       use SCBP bank                                                     756
            BPATH_SCBP='SCBP*'                                            756
            IDNUM_SCBP(1)=0                                               756
            SCBP_LAYER=ILAYER                                             756
            CALL AGDGETP('SVTTGEO6','*',-999,'SCBP',NSCBP_LAYER,          756
     *      RE_SCBP_LAYER,BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,    756
     *      BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)             756
C       Create SCBL :                                                     757
C       Call To Local Procedure SCBL                                      757
            ASSIGN 5860 TO BLOCK_SCBL                                     757
            GO TO  5850                                                   757
5860        CONTINUE                                                      757
C       *                                                                 757
C       Geant POSITION : SCBL ===> X=0.0 Y=0.0 Z=0.0 <===                 758
            CALL AGSRESET                                                 758
            AG_X = 0.0                                                    758
            AG_Y = 0.0                                                    758
            AG_Z = 0.0                                                    758
            AG_TITLE = 'SCBL'                                             758
            AG_EXNAME = 'POSITION'                                        758
            AG_PARLIST = 'X_Y_Z'                                          758
            CALL AXPOSITION                                               758
C       *                                                                 758
         GO TO 5841                                                       759
5842     CONTINUE                                                         759
C    *                                                                    760
         ILAYER=1                                                         760
            GO TO 5873                                                    760
5871        ILAYER=ILAYER+1                                               760
5873        IF(ILAYER-(3).GT.0) GO TO 5872                                760
C       use SFEP bank                                                     761
            BPATH_SFEP='SFEP*'                                            761
            IDNUM_SFEP(1)=0                                               761
            SFEP_LAYER=ILAYER                                             761
            CALL AGDGETP('SVTTGEO6','*',-999,'SFEP',NSFEP_LAYER,          761
     *      RE_SFEP_LAYER,BPATH_SFEP,IDNUM_SFEP,1,MAP_SFEP,NAMES_SFEP,    761
     *      BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)             761
C       Create SFED :                                                     762
C       Call To Local Procedure SFED                                      762
            ASSIGN 5890 TO BLOCK_SFED                                     762
            GO TO  5880                                                   762
5890        CONTINUE                                                      762
C       *                                                                 762
C       Geant POSITION : SFED ===> X=0.0 Y=0.0 Z=0.0 <===                 763
            CALL AGSRESET                                                 763
            AG_X = 0.0                                                    763
            AG_Y = 0.0                                                    763
            AG_Z = 0.0                                                    763
            AG_TITLE = 'SFED'                                             763
            AG_EXNAME = 'POSITION'                                        763
            AG_PARLIST = 'X_Y_Z'                                          763
            CALL AXPOSITION                                               763
C       *                                                                 763
C       Create SPLS :                                                     764
C       Call To Local Procedure SPLS                                      764
            ASSIGN 5910 TO BLOCK_SPLS                                     764
            GO TO  5900                                                   764
5910        CONTINUE                                                      764
C       *                                                                 764
C       Geant POSITION : SPLS ===> X=0.0 Y=0.0 Z=0.0 <===                 765
            CALL AGSRESET                                                 765
            AG_X = 0.0                                                    765
            AG_Y = 0.0                                                    765
            AG_Z = 0.0                                                    765
            AG_TITLE = 'SPLS'                                             765
            AG_EXNAME = 'POSITION'                                        765
            AG_PARLIST = 'X_Y_Z'                                          765
            CALL AXPOSITION                                               765
C       *                                                                 765
         GO TO 5871                                                       766
5872     CONTINUE                                                         766
5391  CALL AGSPOPB                                                        767
      GO TO BLOCK_SCBM, (5390 ,5400)                                      767
C End of Local Procedure                                                  767
C ----------------------------------------------------------------------  768
C ------------ SCBL                                                       768
C Begin Local Procedure SCBL                                              769
C SCBL IS THE BUNDLE OF CABLES GOING FROM PCBS TO MANIFOLDS               769
5850  CONTINUE                                                            769
         AG_TITLE='SCBL'                                                  769
         AG_MODULE=MODULE                                                 769
         AG_MARK='V00'                                                    769
         CALL AGSPUSH                                                     769
         AG_MODULE=MODULE                                                 769
C    *                                                                    769
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           770
         CALL AGSRESET                                                    770
         AG_TITLE = 'COPPER'                                              770
         AG_EXNAME = 'MATERIAL'                                           770
         AG_PARLIST = 'NONE'                                              770
         CALL AXMATERIAL                                                  770
C    *                                                                    770
C    Geant ATTRIBUTE : SCBL ===> SEEN=1 COLO=3 <===                       770
         CALL AGSRESET                                                    771
         AG_SEEN = 1                                                      771
         AG_COLO = 3                                                      771
         AG_TITLE = 'SCBL'                                                771
         AG_EXNAME = 'ATTRIBUTE'                                          771
         AG_PARLIST = 'SEEN_COLO'                                         771
         CALL AXATTRIBUTE                                                 771
C    *                                                                    771
         SQ =(SCBP_LEN*SCBP_LEN)                                          771
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  773
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  774
         A = PI                                                           775
         A=A*(SCBP_RMIN1*SCBP_RMIN1 + SCBP_RMIN2*SCBP_RMIN2)              776
         A=A*SQRT(1 + SQ)                                                 777
         CUTHK=(SCBP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            778
C    Geant SHAPE : CONE ===> DZ=SCBP_LEN RMN1=SCBP_RMIN1 RMX1=SCBP_RMIN1  780
C    +CUTHK, RMN2=SCBP_RMIN2 RMX2=SCBP_RMIN2+CUTHK <===                   780
         CALL AGSRESET                                                    780
         AG_DZ = SCBP_LEN                                                 780
         AG_RMN1 = SCBP_RMIN1                                             780
         AG_RMX1 = SCBP_RMIN1+CUTHK                                       780
         AG_RMN2 = SCBP_RMIN2                                             780
         AG_RMX2 = SCBP_RMIN2+CUTHK                                       780
         AG_TITLE = 'CONE'                                                780
         AG_EXNAME = 'SHAPE'                                              780
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            780
         CALL AXSHAPE                                                     780
C    *                                                                    780
5851  CALL AGSPOPB                                                        781
      GO TO BLOCK_SCBL, (5850 ,5860)                                      781
C End of Local Procedure                                                  781
C ----------------------------------------------------------------------  782
C ------------ SFED                                                       782
C Begin Local Procedure SFED                                              783
C SFED IS THE WATRER IN THE BUNDLE OF PIPES GOING TO MANIFOLDS            783
5880  CONTINUE                                                            783
         AG_TITLE='SFED'                                                  783
         AG_MODULE=MODULE                                                 783
         AG_MARK='V00'                                                    783
         CALL AGSPUSH                                                     783
         AG_MODULE=MODULE                                                 783
C    *                                                                    783
C    Geant MATERIAL : WATER ===> inherited arguments used <===            784
         CALL AGSRESET                                                    784
         AG_TITLE = 'WATER'                                               784
         AG_EXNAME = 'MATERIAL'                                           784
         AG_PARLIST = 'NONE'                                              784
         CALL AXMATERIAL                                                  784
C    *                                                                    784
C    Geant ATTRIBUTE : SFED ===> SEEN=1 COLO=1 <===                       784
         CALL AGSRESET                                                    785
         AG_SEEN = 1                                                      785
         AG_COLO = 1                                                      785
         AG_TITLE = 'SFED'                                                785
         AG_EXNAME = 'ATTRIBUTE'                                          785
         AG_PARLIST = 'SEEN_COLO'                                         785
         CALL AXATTRIBUTE                                                 785
C    *                                                                    785
         SQ = SFEP_LEN*SFEP_LEN                                           785
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  787
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  788
         A = PI                                                           789
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            790
         A=A*SQRT(1 + SQ)                                                 791
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            792
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1 RMX1=SFEP_RMIN1  794
C    +CUTHK, RMN2=SFEP_RMIN2 RMX2=SFEP_RMIN2+CUTHK <===                   794
         CALL AGSRESET                                                    794
         AG_DZ = SFEP_LEN                                                 794
         AG_RMN1 = SFEP_RMIN1                                             794
         AG_RMX1 = SFEP_RMIN1+CUTHK                                       794
         AG_RMN2 = SFEP_RMIN2                                             794
         AG_RMX2 = SFEP_RMIN2+CUTHK                                       794
         AG_TITLE = 'CONE'                                                794
         AG_EXNAME = 'SHAPE'                                              794
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            794
         CALL AXSHAPE                                                     794
C    *                                                                    794
5881  CALL AGSPOPB                                                        795
      GO TO BLOCK_SFED, (5880 ,5890)                                      795
C End of Local Procedure                                                  795
C ----------------------------------------------------------------------  796
C ------------ SPLS                                                       796
C Begin Local Procedure SPLS                                              797
C SPLS IS THE PLASTIC WALLS OF THE BUNDLE OF PIPES GOING TO MANIFOLDS     797
5900  CONTINUE                                                            797
         AG_TITLE='SPLS'                                                  797
         AG_MODULE=MODULE                                                 797
         AG_MARK='V00'                                                    797
         CALL AGSPUSH                                                     797
         AG_MODULE=MODULE                                                 797
C    *                                                                    797
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           798
         CALL AGSRESET                                                    798
         AG_A = 12                                                        798
         AG_Z = 6                                                         798
         AG_W = 1                                                         798
         AG_TITLE = 'C'                                                   798
         AG_EXNAME = 'COMPONENT'                                          798
         AG_PARLIST = 'A_Z_W'                                             798
         CALL AXCOMPONENT                                                 798
C    *                                                                    798
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           798
         CALL AGSRESET                                                    799
         AG_A = 1                                                         799
         AG_Z = 1                                                         799
         AG_W = 2                                                         799
         AG_TITLE = 'H2'                                                  799
         AG_EXNAME = 'COMPONENT'                                          799
         AG_PARLIST = 'A_Z_W'                                             799
         CALL AXCOMPONENT                                                 799
C    *                                                                    799
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             799
         CALL AGSRESET                                                    800
         AG_DENS = 0.935                                                  800
         AG_TITLE = 'CH2'                                                 800
         AG_EXNAME = 'MIXTURE'                                            800
         AG_PARLIST = 'DENS'                                              800
         CALL AXMIXTURE                                                   800
C    *                                                                    800
C    Geant ATTRIBUTE : SPLS ===> SEEN=1 COLO=6 <===                       800
         CALL AGSRESET                                                    801
         AG_SEEN = 1                                                      801
         AG_COLO = 6                                                      801
         AG_TITLE = 'SPLS'                                                801
         AG_EXNAME = 'ATTRIBUTE'                                          801
         AG_PARLIST = 'SEEN_COLO'                                         801
         CALL AXATTRIBUTE                                                 801
C    *                                                                    801
         SQ = SFEP_LEN*SFEP_LEN                                           801
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  803
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  804
         A = PI                                                           805
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            806
         A=A*SQRT(1 + SQ)                                                 807
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            808
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1+0.2, RMX1=SFEP_  811
C    RMIN1+0.2+CUTHK RMN2=SFEP_RMIN2+0.2, RMX2=SFEP_RMIN2+0.2+CUTHK <===  811
         CALL AGSRESET                                                    811
         AG_DZ = SFEP_LEN                                                 811
         AG_RMN1 = SFEP_RMIN1+0.2                                         811
         AG_RMX1 = SFEP_RMIN1+0.2+CUTHK                                   811
         AG_RMN2 = SFEP_RMIN2+0.2                                         811
         AG_RMX2 = SFEP_RMIN2+0.2+CUTHK                                   811
         AG_TITLE = 'CONE'                                                811
         AG_EXNAME = 'SHAPE'                                              811
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            811
         CALL AXSHAPE                                                     811
C    *                                                                    811
5901  CALL AGSPOPB                                                        812
      GO TO BLOCK_SPLS, (5900 ,5910)                                      812
C End of Local Procedure                                                  812
C ----------------------------------------------------------------------  813
C ------------ SELE                                                       813
C Begin Local Procedure SELE                                              814
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   814
5590  CONTINUE                                                            814
         AG_TITLE='SELE'                                                  814
         AG_MODULE=MODULE                                                 814
         AG_MARK='V00'                                                    814
         CALL AGSPUSH                                                     814
         AG_MODULE=MODULE                                                 814
C    *                                                                    814
C    Geant MATERIAL : AIR ===> inherited arguments used <===              815
         CALL AGSRESET                                                    815
         AG_TITLE = 'AIR'                                                 815
         AG_EXNAME = 'MATERIAL'                                           815
         AG_PARLIST = 'NONE'                                              815
         CALL AXMATERIAL                                                  815
C    *                                                                    815
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 SERIAL=ILAYER <===         815
         CALL AGSRESET                                                    816
         AG_SEEN = 0                                                      816
         AG_COLO = 1                                                      816
         AG_SERIAL = ILAYER                                               816
         AG_TITLE = 'SELE'                                                816
         AG_EXNAME = 'ATTRIBUTE'                                          816
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  816
         CALL AXATTRIBUTE                                                 816
C    *                                                                    816
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  816
C    K/2 <===                                                             816
         CALL AGSRESET                                                    818
         AG_DX = SELC_ELCAWID/2                                           818
         AG_DY = SWCA_LENGTH/2                                            818
         AG_DZ = ELETHK/2                                                 818
         AG_TITLE = 'BOX'                                                 818
         AG_EXNAME = 'SHAPE'                                              818
         AG_PARLIST = 'DX_DY_DZ'                                          818
         CALL AXSHAPE                                                     818
C    *                                                                    818
C    Create SWCH :                                                        819
C    Call To Local Procedure SWCH                                         819
         ASSIGN 5930 TO BLOCK_SWCH                                        819
         GO TO  5920                                                      819
5930     CONTINUE                                                         819
C    *                                                                    819
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             820
         CALL AGSRESET                                                    820
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    820
         AG_TITLE = 'SWCH'                                                820
         AG_EXNAME = 'POSITION'                                           820
         AG_PARLIST = 'Z'                                                 820
         CALL AXPOSITION                                                  820
C    *                                                                    820
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  820
C    BETHK/2 <===                                                         820
         CALL AGSRESET                                                    821
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             821
         AG_TITLE = 'SWCH'                                                821
         AG_EXNAME = 'POSITION'                                           821
         AG_PARLIST = 'Z'                                                 821
         CALL AXPOSITION                                                  821
C    *                                                                    821
C    Create SWCS :                                                        822
C    Call To Local Procedure SWCS                                         822
         ASSIGN 5950 TO BLOCK_SWCS                                        822
         GO TO  5940                                                      822
5950     CONTINUE                                                         822
C    *                                                                    822
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  824
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     824
         CALL AGSRESET                                                    824
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              824
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        824
         AG_TITLE = 'SWCS'                                                824
         AG_EXNAME = 'POSITION'                                           824
         AG_PARLIST = 'X_Z'                                               824
         CALL AXPOSITION                                                  824
C    *                                                                    824
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  824
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     824
         CALL AGSRESET                                                    826
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              826
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        826
         AG_TITLE = 'SWCS'                                                826
         AG_EXNAME = 'POSITION'                                           826
         AG_PARLIST = 'X_Z'                                               826
         CALL AXPOSITION                                                  826
C    *                                                                    826
C    Create SWCW :                                                        827
C    Call To Local Procedure SWCW                                         827
         ASSIGN 5970 TO BLOCK_SWCW                                        827
         GO TO  5960                                                      827
5970     CONTINUE                                                         827
C    *                                                                    827
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  828
C    =                                                                    828
         CALL AGSRESET                                                    828
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        828
         AG_TITLE = 'SWCW'                                                828
         AG_EXNAME = 'POSITION'                                           828
         AG_PARLIST = 'Z'                                                 828
         CALL AXPOSITION                                                  828
C    *                                                                    828
C    Create SBOI :                                                        829
C    Call To Local Procedure SBOI                                         829
         ASSIGN 5990 TO BLOCK_SBOI                                        829
         GO TO  5980                                                      829
5990     CONTINUE                                                         829
C    *                                                                    829
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  830
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       830
         CALL AGSRESET                                                    830
         AG_Z = (ELETHK/                                                  830
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          830
         AG_TITLE = 'SBOI'                                                830
         AG_EXNAME = 'POSITION'                                           830
         AG_PARLIST = 'Z'                                                 830
         CALL AXPOSITION                                                  830
C    *                                                                    830
C    Create SGLA :                                                        831
C    Call To Local Procedure SGLA                                         831
         ASSIGN 6010 TO BLOCK_SGLA                                        831
         GO TO  6000                                                      831
6010     CONTINUE                                                         831
C    *                                                                    831
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  832
C    _GLASSTHK/2 <===                                                     832
         CALL AGSRESET                                                    832
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         832
         AG_TITLE = 'SGLA'                                                832
         AG_EXNAME = 'POSITION'                                           832
         AG_PARLIST = 'Z'                                                 832
         CALL AXPOSITION                                                  832
C    *                                                                    832
C    Create SAGP :                                                        833
C    Call To Local Procedure SAGP                                         833
         ASSIGN 6030 TO BLOCK_SAGP                                        833
         GO TO  6020                                                      833
6030     CONTINUE                                                         833
C    *                                                                    833
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  834
C    ==                                                                   834
         CALL AGSRESET                                                    834
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       834
         AG_TITLE = 'SAGP'                                                834
         AG_EXNAME = 'POSITION'                                           834
         AG_PARLIST = 'Z'                                                 834
         CALL AXPOSITION                                                  834
C    *                                                                    834
C    Create SDYE :                                                        835
C    Call To Local Procedure SDYE                                         835
         ASSIGN 6050 TO BLOCK_SDYE                                        835
         GO TO  6040                                                      835
6050     CONTINUE                                                         835
C    *                                                                    835
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  837
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     837
         CALL AGSRESET                                                    837
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  837
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    837
         AG_TITLE = 'SDYE'                                                837
         AG_EXNAME = 'POSITION'                                           837
         AG_PARLIST = 'X_Z'                                               837
         CALL AXPOSITION                                                  837
C    *                                                                    837
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  837
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             837
         CALL AGSRESET                                                    839
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          839
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    839
         AG_TITLE = 'SDYE'                                                839
         AG_EXNAME = 'POSITION'                                           839
         AG_PARLIST = 'X_Z'                                               839
         CALL AXPOSITION                                                  839
C    *                                                                    839
C    Create SECA :                                                        840
C    Call To Local Procedure SECA                                         840
         ASSIGN 6070 TO BLOCK_SECA                                        840
         GO TO  6060                                                      840
6070     CONTINUE                                                         840
C    *                                                                    840
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  842
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    842
         CALL AGSRESET                                                    842
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             842
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        842
         AG_TITLE = 'SECA'                                                842
         AG_EXNAME = 'POSITION'                                           842
         AG_PARLIST = 'X_Z'                                               842
         CALL AXPOSITION                                                  842
C    *                                                                    842
5591  CALL AGSPOPB                                                        843
      GO TO BLOCK_SELE, (5590 ,5600)                                      843
C End of Local Procedure                                                  843
C ----------------------------------------------------------------------  844
C ------------ SDYE                                                       844
C Begin Local Procedure SDYE                                              845
C SDYE IS THE IC CHIP ON THE HYBRID                                       845
6040  CONTINUE                                                            845
         AG_TITLE='SDYE'                                                  845
         AG_MODULE=MODULE                                                 845
         AG_MARK='V00'                                                    845
         CALL AGSPUSH                                                     845
         AG_MODULE=MODULE                                                 845
C    *                                                                    845
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          846
         CALL AGSRESET                                                    846
         AG_TITLE = 'SILICON'                                             846
         AG_EXNAME = 'MATERIAL'                                           846
         AG_PARLIST = 'NONE'                                              846
         CALL AXMATERIAL                                                  846
C    *                                                                    846
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 SERIAL=0 <===              846
         CALL AGSRESET                                                    847
         AG_SEEN = 1                                                      847
         AG_COLO = 6                                                      847
         AG_SERIAL = 0                                                    847
         AG_TITLE = 'SDYE'                                                847
         AG_EXNAME = 'ATTRIBUTE'                                          847
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  847
         CALL AXATTRIBUTE                                                 847
C    *                                                                    847
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  847
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           847
         CALL AGSRESET                                                    849
         AG_DX = SELC_DYEWID/2                                            849
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              849
         AG_DZ = SELC_DYETHK/2                                            849
         AG_TITLE = 'BOX'                                                 849
         AG_EXNAME = 'SHAPE'                                              849
         AG_PARLIST = 'DX_DY_DZ'                                          849
         CALL AXSHAPE                                                     849
C    *                                                                    849
6041  CALL AGSPOPB                                                        850
      GO TO BLOCK_SDYE, (6040 ,6050)                                      850
C End of Local Procedure                                                  850
C ----------------------------------------------------------------------  851
C ------------ SECA                                                       851
C Begin Local Procedure SECA                                              852
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            852
6060  CONTINUE                                                            852
         AG_TITLE='SECA'                                                  852
         AG_MODULE=MODULE                                                 852
         AG_MARK='V00'                                                    852
         CALL AGSPUSH                                                     852
         AG_MODULE=MODULE                                                 852
C    *                                                                    852
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           853
         CALL AGSRESET                                                    853
         AG_TITLE = 'COPPER'                                              853
         AG_EXNAME = 'MATERIAL'                                           853
         AG_PARLIST = 'NONE'                                              853
         CALL AXMATERIAL                                                  853
C    *                                                                    853
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 SERIAL=0 <===              853
         CALL AGSRESET                                                    854
         AG_SEEN = 1                                                      854
         AG_COLO = 2                                                      854
         AG_SERIAL = 0                                                    854
         AG_TITLE = 'SECA'                                                854
         AG_EXNAME = 'ATTRIBUTE'                                          854
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  854
         CALL AXATTRIBUTE                                                 854
C    *                                                                    854
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  854
C    ABTHK/2 <===                                                         854
         CALL AGSRESET                                                    856
         AG_DX = SELC_CABWID/2                                            856
         AG_DY = SWCA_LENGTH/2                                            856
         AG_DZ = SELC_CABTHK/2                                            856
         AG_TITLE = 'BOX'                                                 856
         AG_EXNAME = 'SHAPE'                                              856
         AG_PARLIST = 'DX_DY_DZ'                                          856
         CALL AXSHAPE                                                     856
C    *                                                                    856
6061  CALL AGSPOPB                                                        857
      GO TO BLOCK_SECA, (6060 ,6070)                                      857
C End of Local Procedure                                                  857
C ----------------------------------------------------------------------  858
C ------------ SBOI                                                       858
C Begin Local Procedure SBOI                                              859
C SBOI IS THE BERILLIA LAYER                                              859
5980  CONTINUE                                                            859
         AG_TITLE='SBOI'                                                  859
         AG_MODULE=MODULE                                                 859
         AG_MARK='V00'                                                    859
         CALL AGSPUSH                                                     859
         AG_MODULE=MODULE                                                 859
C    *                                                                    859
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1.0 <===                         860
         CALL AGSRESET                                                    860
         AG_A = 9                                                         860
         AG_Z = 4                                                         860
         AG_W = 1.0                                                       860
         AG_TITLE = 'BE'                                                  860
         AG_EXNAME = 'COMPONENT'                                          860
         AG_PARLIST = 'A_Z_W'                                             860
         CALL AXCOMPONENT                                                 860
C    *                                                                    860
C    Geant COMPONENT : O ===> A=16 Z=8 W=1.0 <===                         860
         CALL AGSRESET                                                    861
         AG_A = 16                                                        861
         AG_Z = 8                                                         861
         AG_W = 1.0                                                       861
         AG_TITLE = 'O'                                                   861
         AG_EXNAME = 'COMPONENT'                                          861
         AG_PARLIST = 'A_Z_W'                                             861
         CALL AXCOMPONENT                                                 861
C    *                                                                    861
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              861
         CALL AGSRESET                                                    862
         AG_DENS = 2.85                                                   862
         AG_TITLE = 'BEO'                                                 862
         AG_EXNAME = 'MIXTURE'                                            862
         AG_PARLIST = 'DENS'                                              862
         CALL AXMIXTURE                                                   862
C    *                                                                    862
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 SERIAL=0 <===              862
         CALL AGSRESET                                                    863
         AG_SEEN = 1                                                      863
         AG_COLO = 6                                                      863
         AG_SERIAL = 0                                                    863
         AG_TITLE = 'SBOI'                                                863
         AG_EXNAME = 'ATTRIBUTE'                                          863
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  863
         CALL AXATTRIBUTE                                                 863
C    *                                                                    863
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  863
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          863
         CALL AGSRESET                                                    865
         AG_DX = SELC_ELCAWID/2                                           865
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              865
         AG_DZ = SELC_BEOTHK/2                                            865
         AG_TITLE = 'BOX'                                                 865
         AG_EXNAME = 'SHAPE'                                              865
         AG_PARLIST = 'DX_DY_DZ'                                          865
         CALL AXSHAPE                                                     865
C    *                                                                    865
5981  CALL AGSPOPB                                                        866
      GO TO BLOCK_SBOI, (5980 ,5990)                                      866
C End of Local Procedure                                                  866
C ----------------------------------------------------------------------  867
C ------------ SAGP                                                       867
C Begin Local Procedure SAGP                                              868
C SAGP IS THE SILVER-PALLADIUM LAYER                                      868
6020  CONTINUE                                                            868
         AG_TITLE='SAGP'                                                  868
         AG_MODULE=MODULE                                                 868
         AG_MARK='V00'                                                    868
         CALL AGSPUSH                                                     868
         AG_MODULE=MODULE                                                 868
C    *                                                                    868
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1.0 <===                      869
         CALL AGSRESET                                                    869
         AG_A = 108                                                       869
         AG_Z = 47                                                        869
         AG_W = 1.0                                                       869
         AG_TITLE = 'AG'                                                  869
         AG_EXNAME = 'COMPONENT'                                          869
         AG_PARLIST = 'A_Z_W'                                             869
         CALL AXCOMPONENT                                                 869
C    *                                                                    869
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1.0 <===                      869
         CALL AGSRESET                                                    870
         AG_A = 106                                                       870
         AG_Z = 46                                                        870
         AG_W = 1.0                                                       870
         AG_TITLE = 'PD'                                                  870
         AG_EXNAME = 'COMPONENT'                                          870
         AG_PARLIST = 'A_Z_W'                                             870
         CALL AXCOMPONENT                                                 870
C    *                                                                    870
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            870
         CALL AGSRESET                                                    871
         AG_DENS = 11.25                                                  871
         AG_TITLE = 'AGPD'                                                871
         AG_EXNAME = 'MIXTURE'                                            871
         AG_PARLIST = 'DENS'                                              871
         CALL AXMIXTURE                                                   871
C    *                                                                    871
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 SERIAL=0 <===              871
         CALL AGSRESET                                                    872
         AG_SEEN = 1                                                      872
         AG_COLO = 2                                                      872
         AG_SERIAL = 0                                                    872
         AG_TITLE = 'SAGP'                                                872
         AG_EXNAME = 'ATTRIBUTE'                                          872
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  872
         CALL AXATTRIBUTE                                                 872
C    *                                                                    872
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  872
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         872
         CALL AGSRESET                                                    874
         AG_DX = SELC_ELCAWID/2                                           874
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              874
         AG_DZ = SELC_AGPDTHK/2                                           874
         AG_TITLE = 'BOX'                                                 874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'DX_DY_DZ'                                          874
         CALL AXSHAPE                                                     874
C    *                                                                    874
6021  CALL AGSPOPB                                                        875
      GO TO BLOCK_SAGP, (6020 ,6030)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ SGLA                                                       876
C Begin Local Procedure SGLA                                              877
C SGLA IS THE INSULATING GLASS LAYER                                      877
6000  CONTINUE                                                            877
         AG_TITLE='SGLA'                                                  877
         AG_MODULE=MODULE                                                 877
         AG_MARK='V00'                                                    877
         CALL AGSPUSH                                                     877
         AG_MODULE=MODULE                                                 877
C    *                                                                    877
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         878
         CALL AGSRESET                                                    878
         AG_A = 28                                                        878
         AG_Z = 14                                                        878
         AG_W = 1                                                         878
         AG_TITLE = 'SI'                                                  878
         AG_EXNAME = 'COMPONENT'                                          878
         AG_PARLIST = 'A_Z_W'                                             878
         CALL AXCOMPONENT                                                 878
C    *                                                                    878
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           878
         CALL AGSRESET                                                    879
         AG_A = 16                                                        879
         AG_Z = 8                                                         879
         AG_W = 2                                                         879
         AG_TITLE = 'O'                                                   879
         AG_EXNAME = 'COMPONENT'                                          879
         AG_PARLIST = 'A_Z_W'                                             879
         CALL AXCOMPONENT                                                 879
C    *                                                                    879
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             879
         CALL AGSRESET                                                    880
         AG_DENS = 2.2                                                    880
         AG_TITLE = 'GLASS'                                               880
         AG_EXNAME = 'MIXTURE'                                            880
         AG_PARLIST = 'DENS'                                              880
         CALL AXMIXTURE                                                   880
C    *                                                                    880
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 SERIAL=0 <===              880
         CALL AGSRESET                                                    881
         AG_SEEN = 1                                                      881
         AG_COLO = 6                                                      881
         AG_SERIAL = 0                                                    881
         AG_TITLE = 'SGLA'                                                881
         AG_EXNAME = 'ATTRIBUTE'                                          881
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  881
         CALL AXATTRIBUTE                                                 881
C    *                                                                    881
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  881
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        881
         CALL AGSRESET                                                    883
         AG_DX = SELC_ELCAWID/2                                           883
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              883
         AG_DZ = SELC_GLASSTHK/2                                          883
         AG_TITLE = 'BOX'                                                 883
         AG_EXNAME = 'SHAPE'                                              883
         AG_PARLIST = 'DX_DY_DZ'                                          883
         CALL AXSHAPE                                                     883
C    *                                                                    883
6001  CALL AGSPOPB                                                        884
      GO TO BLOCK_SGLA, (6000 ,6010)                                      884
C End of Local Procedure                                                  884
C ----------------------------------------------------------------------  885
C ------------ SWCH                                                       885
C Begin Local Procedure SWCH                                              886
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      886
5920  CONTINUE                                                            886
         AG_TITLE='SWCH'                                                  886
         AG_MODULE=MODULE                                                 886
         AG_MARK='V00'                                                    886
         CALL AGSPUSH                                                     886
         AG_MODULE=MODULE                                                 886
C    *                                                                    886
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        887
         CALL AGSRESET                                                    887
         AG_TITLE = 'BERILLIUM'                                           887
         AG_EXNAME = 'MATERIAL'                                           887
         AG_PARLIST = 'NONE'                                              887
         CALL AXMATERIAL                                                  887
C    *                                                                    887
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 SERIAL=0 <===              887
         CALL AGSRESET                                                    888
         AG_SEEN = 1                                                      888
         AG_COLO = 2                                                      888
         AG_SERIAL = 0                                                    888
         AG_TITLE = 'SWCH'                                                888
         AG_EXNAME = 'ATTRIBUTE'                                          888
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  888
         CALL AXATTRIBUTE                                                 888
C    *                                                                    888
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  888
C    BETHK/2 <===                                                         888
         CALL AGSRESET                                                    890
         AG_DX = SELC_ELCAWID/2                                           890
         AG_DY = SWCA_LENGTH/2                                            890
         AG_DZ = SELC_BETHK/2                                             890
         AG_TITLE = 'BOX'                                                 890
         AG_EXNAME = 'SHAPE'                                              890
         AG_PARLIST = 'DX_DY_DZ'                                          890
         CALL AXSHAPE                                                     890
C    *                                                                    890
5921  CALL AGSPOPB                                                        891
      GO TO BLOCK_SWCH, (5920 ,5930)                                      891
C End of Local Procedure                                                  891
C ----------------------------------------------------------------------  892
C ------------ SWCS                                                       892
C Begin Local Procedure SWCS                                              893
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                893
5940  CONTINUE                                                            893
         AG_TITLE='SWCS'                                                  893
         AG_MODULE=MODULE                                                 893
         AG_MARK='V00'                                                    893
         CALL AGSPUSH                                                     893
         AG_MODULE=MODULE                                                 893
C    *                                                                    893
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        894
         CALL AGSRESET                                                    894
         AG_TITLE = 'BERILLIUM'                                           894
         AG_EXNAME = 'MATERIAL'                                           894
         AG_PARLIST = 'NONE'                                              894
         CALL AXMATERIAL                                                  894
C    *                                                                    894
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 SERIAL=0 <===              894
         CALL AGSRESET                                                    895
         AG_SEEN = 1                                                      895
         AG_COLO = 2                                                      895
         AG_SERIAL = 0                                                    895
         AG_TITLE = 'SWCS'                                                895
         AG_EXNAME = 'ATTRIBUTE'                                          895
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  895
         CALL AXATTRIBUTE                                                 895
C    *                                                                    895
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  895
C    TTHK/2 <===                                                          895
         CALL AGSRESET                                                    897
         AG_DX = SELC_BETHK/2                                             897
         AG_DY = SWCA_LENGTH/2                                            897
         AG_DZ = SELC_WATTHK/2                                            897
         AG_TITLE = 'BOX'                                                 897
         AG_EXNAME = 'SHAPE'                                              897
         AG_PARLIST = 'DX_DY_DZ'                                          897
         CALL AXSHAPE                                                     897
C    *                                                                    897
5941  CALL AGSPOPB                                                        898
      GO TO BLOCK_SWCS, (5940 ,5950)                                      898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ SWCW                                                       899
C Begin Local Procedure SWCW                                              900
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       900
5960  CONTINUE                                                            900
         AG_TITLE='SWCW'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant MATERIAL : WATER ===> inherited arguments used <===            901
         CALL AGSRESET                                                    901
         AG_TITLE = 'WATER'                                               901
         AG_EXNAME = 'MATERIAL'                                           901
         AG_PARLIST = 'NONE'                                              901
         CALL AXMATERIAL                                                  901
C    *                                                                    901
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 SERIAL=0 <===              901
         CALL AGSRESET                                                    902
         AG_SEEN = 1                                                      902
         AG_COLO = 6                                                      902
         AG_SERIAL = 0                                                    902
         AG_TITLE = 'SWCW'                                                902
         AG_EXNAME = 'ATTRIBUTE'                                          902
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  902
         CALL AXATTRIBUTE                                                 902
C    *                                                                    902
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  902
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     902
         CALL AGSRESET                                                    904
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          904
         AG_DY = SWCA_LENGTH/2                                            904
         AG_DZ = (SELC_WATTHK/2)                                          904
         AG_TITLE = 'BOX'                                                 904
         AG_EXNAME = 'SHAPE'                                              904
         AG_PARLIST = 'DX_DY_DZ'                                          904
         CALL AXSHAPE                                                     904
C    *                                                                    904
5961  CALL AGSPOPB                                                        905
      GO TO BLOCK_SWCW, (5960 ,5970)                                      905
C End of Local Procedure                                                  905
C ----------------------------------------------------------------------  906
C ------------ SIRT                                                       906
C Begin Local Procedure SIRT                                              907
C SIRT IS THE SVT INNER END RING TUBE                                     907
5070  CONTINUE                                                            907
         AG_TITLE='SIRT'                                                  907
         AG_MODULE=MODULE                                                 907
         AG_MARK='V00'                                                    907
         CALL AGSPUSH                                                     907
         AG_MODULE=MODULE                                                 907
C    *                                                                    907
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        908
         CALL AGSRESET                                                    908
         AG_TITLE = 'BERILLIUM'                                           908
         AG_EXNAME = 'MATERIAL'                                           908
         AG_PARLIST = 'NONE'                                              908
         CALL AXMATERIAL                                                  908
C    *                                                                    908
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       908
         CALL AGSRESET                                                    909
         AG_SEEN = 1                                                      909
         AG_COLO = 2                                                      909
         AG_TITLE = 'SIRT'                                                909
         AG_EXNAME = 'ATTRIBUTE'                                          909
         AG_PARLIST = 'SEEN_COLO'                                         909
         CALL AXATTRIBUTE                                                 909
C    *                                                                    909
         IR_RMIN = COS(PI/8.0)                                            909
         IR_RMIN=IR_RMIN+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/           911
     *   8.0)*SIN(PI/8.0))                                                911
         IR_RMIN=IR_RMIN*SERG_IRNGPRMN                                    912
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  914
C    DRNGTH/2 <===                                                        914
         CALL AGSRESET                                                    914
         AG_RMIN = IR_RMIN                                                914
         AG_RMAX = SERG_IRNGTRMX                                          914
         AG_DZ = SERG_ENDRNGTH/2                                          914
         AG_TITLE = 'TUBE'                                                914
         AG_EXNAME = 'SHAPE'                                              914
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      914
         CALL AXSHAPE                                                     914
C    *                                                                    914
5071  CALL AGSPOPB                                                        915
      GO TO BLOCK_SIRT, (5070 ,5080)                                      915
C End of Local Procedure                                                  915
C ----------------------------------------------------------------------  916
C ------------ SIRP                                                       916
C Begin Local Procedure SIRP                                              917
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 917
5050  CONTINUE                                                            917
         AG_TITLE='SIRP'                                                  917
         AG_MODULE=MODULE                                                 917
         AG_MARK='V00'                                                    917
         CALL AGSPUSH                                                     917
         AG_MODULE=MODULE                                                 917
C    *                                                                    917
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        918
         CALL AGSRESET                                                    918
         AG_TITLE = 'BERILLIUM'                                           918
         AG_EXNAME = 'MATERIAL'                                           918
         AG_PARLIST = 'NONE'                                              918
         CALL AXMATERIAL                                                  918
C    *                                                                    918
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       918
         CALL AGSRESET                                                    919
         AG_SEEN = 1                                                      919
         AG_COLO = 2                                                      919
         AG_TITLE = 'SIRP'                                                919
         AG_EXNAME = 'ATTRIBUTE'                                          919
         AG_PARLIST = 'SEEN_COLO'                                         919
         CALL AXATTRIBUTE                                                 919
C    *                                                                    919
         ROU = COS(PI/8.0)                                                919
         ROU=ROU+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/8.0)*SIN(PI/       921
     *   8.0))                                                            921
         ROU = SERG_IRNGTRMX/ROU                                          922
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  925
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  925
C    OU,ROU} <===                                                         925
         CALL AGSRESET                                                    925
         AG_PHI1 = 0                                                      925
         AG_DPHI = 360                                                    925
         AG_NPDIV = 8                                                     925
         AG_NZ = 2                                                        925
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        925
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        925
         AG_RMN(1)=SERG_IRNGPRMN                                          925
         AG_RMN(2)=SERG_IRNGPRMN                                          925
         AG_RMX(1)=ROU                                                    925
         AG_RMX(2)=ROU                                                    925
         AG_TITLE = 'PGON'                                                925
         AG_EXNAME = 'SHAPE'                                              925
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     925
         CALL AXSHAPE                                                     925
C    *                                                                    925
5051  CALL AGSPOPB                                                        926
      GO TO BLOCK_SIRP, (5050 ,5060)                                      926
C End of Local Procedure                                                  926
C ----------------------------------------------------------------------  927
C ------------ SOER                                                       927
C Begin Local Procedure SOER                                              928
C SOER IS THE SVT OUTER END RING                                          928
5090  CONTINUE                                                            928
         AG_TITLE='SOER'                                                  928
         AG_MODULE=MODULE                                                 928
         AG_MARK='V00'                                                    928
         CALL AGSPUSH                                                     928
         AG_MODULE=MODULE                                                 928
C    *                                                                    928
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        929
         CALL AGSRESET                                                    929
         AG_TITLE = 'BERILLIUM'                                           929
         AG_EXNAME = 'MATERIAL'                                           929
         AG_PARLIST = 'NONE'                                              929
         CALL AXMATERIAL                                                  929
C    *                                                                    929
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       929
         CALL AGSRESET                                                    930
         AG_SEEN = 1                                                      930
         AG_COLO = 2                                                      930
         AG_TITLE = 'SOER'                                                930
         AG_EXNAME = 'ATTRIBUTE'                                          930
         AG_PARLIST = 'SEEN_COLO'                                         930
         CALL AXATTRIBUTE                                                 930
C    *                                                                    930
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  930
C    ERG_ENDRNGTH/2 <===                                                  930
         CALL AGSRESET                                                    932
         AG_RMIN = SERG_ORNGRMIN                                          932
         AG_RMAX = SERG_ORNGRMAX                                          932
         AG_DZ = SERG_ENDRNGTH/2                                          932
         AG_TITLE = 'TUBE'                                                932
         AG_EXNAME = 'SHAPE'                                              932
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      932
         CALL AXSHAPE                                                     932
C    *                                                                    932
5091  CALL AGSPOPB                                                        933
      GO TO BLOCK_SOER, (5090 ,5100)                                      933
C End of Local Procedure                                                  933
C ----------------------------------------------------------------------  934
C ------------ SIES                                                       934
C Begin Local Procedure SIES                                              935
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         935
5150  CONTINUE                                                            935
         AG_TITLE='SIES'                                                  935
         AG_MODULE=MODULE                                                 935
         AG_MARK='V00'                                                    935
         CALL AGSPUSH                                                     935
         AG_MODULE=MODULE                                                 935
C    *                                                                    935
C    Geant MATERIAL : AIR ===> inherited arguments used <===              936
         CALL AGSRESET                                                    936
         AG_TITLE = 'AIR'                                                 936
         AG_EXNAME = 'MATERIAL'                                           936
         AG_PARLIST = 'NONE'                                              936
         CALL AXMATERIAL                                                  936
C    *                                                                    936
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       936
         CALL AGSRESET                                                    937
         AG_SEEN = 0                                                      937
         AG_COLO = 1                                                      937
         AG_TITLE = 'SIES'                                                937
         AG_EXNAME = 'ATTRIBUTE'                                          937
         AG_PARLIST = 'SEEN_COLO'                                         937
         CALL AXATTRIBUTE                                                 937
C    *                                                                    937
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  937
C    ACK_Z-ENDRNG_Z) <===                                                 937
         CALL AGSRESET                                                    939
         AG_RMIN = IR_RMIN                                                939
         AG_RMAX = SERG_IRNGTRMX                                          939
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   939
         AG_TITLE = 'TUBE'                                                939
         AG_EXNAME = 'SHAPE'                                              939
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      939
         CALL AXSHAPE                                                     939
C    *                                                                    939
C    Create SISM :                                                        940
C    Call To Local Procedure SISM                                         940
         ASSIGN 6090 TO BLOCK_SISM                                        940
         GO TO  6080                                                      940
6090     CONTINUE                                                         940
C    *                                                                    940
5151  CALL AGSPOPB                                                        941
      GO TO BLOCK_SIES, (5150 ,5160)                                      941
C End of Local Procedure                                                  941
C ----------------------------------------------------------------------  942
C ------------ SISM                                                       942
C Begin Local Procedure SISM                                              943
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        943
6080  CONTINUE                                                            943
         AG_TITLE='SISM'                                                  943
         AG_MODULE=MODULE                                                 943
         AG_MARK='V00'                                                    943
         CALL AGSPUSH                                                     943
         AG_MODULE=MODULE                                                 943
C    *                                                                    943
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       944
         CALL AGSRESET                                                    944
         AG_SEEN = 0                                                      944
         AG_COLO = 1                                                      944
         AG_TITLE = 'SISM'                                                944
         AG_EXNAME = 'ATTRIBUTE'                                          944
         AG_PARLIST = 'SEEN_COLO'                                         944
         CALL AXATTRIBUTE                                                 944
C    *                                                                    944
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                944
         CALL AGSRESET                                                    945
         AG_NDIV = 4                                                      945
         AG_IAXIS = 2                                                     945
         AG_C0 = 45                                                       945
         AG_TITLE = 'DIVISION'                                            945
         AG_EXNAME = 'SHAPE'                                              945
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     945
         CALL AXSHAPE                                                     945
C    *                                                                    945
C    Create SCRW :                                                        946
C    Call To Local Procedure SCRW                                         946
         ASSIGN 6110 TO BLOCK_SCRW                                        946
         GO TO  6100                                                      946
6110     CONTINUE                                                         946
C    *                                                                    946
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  947
C    ===                                                                  947
         CALL AGSRESET                                                    947
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      947
         AG_TITLE = 'SCRW'                                                947
         AG_EXNAME = 'POSITION'                                           947
         AG_PARLIST = 'X'                                                 947
         CALL AXPOSITION                                                  947
C    *                                                                    947
6081  CALL AGSPOPB                                                        948
      GO TO BLOCK_SISM, (6080 ,6090)                                      948
C End of Local Procedure                                                  948
C ----------------------------------------------------------------------  949
C ------------ SOES                                                       949
C Begin Local Procedure SOES                                              950
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         950
5130  CONTINUE                                                            950
         AG_TITLE='SOES'                                                  950
         AG_MODULE=MODULE                                                 950
         AG_MARK='V00'                                                    950
         CALL AGSPUSH                                                     950
         AG_MODULE=MODULE                                                 950
C    *                                                                    950
C    Geant MATERIAL : AIR ===> inherited arguments used <===              951
         CALL AGSRESET                                                    951
         AG_TITLE = 'AIR'                                                 951
         AG_EXNAME = 'MATERIAL'                                           951
         AG_PARLIST = 'NONE'                                              951
         CALL AXMATERIAL                                                  951
C    *                                                                    951
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       951
         CALL AGSRESET                                                    952
         AG_SEEN = 0                                                      952
         AG_COLO = 1                                                      952
         AG_TITLE = 'SOES'                                                952
         AG_EXNAME = 'ATTRIBUTE'                                          952
         AG_PARLIST = 'SEEN_COLO'                                         952
         CALL AXATTRIBUTE                                                 952
C    *                                                                    952
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  952
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           952
         CALL AGSRESET                                                    954
         AG_RMIN = SERG_ORNGRMIN                                          954
         AG_RMAX = SERG_ORNGRMAX                                          954
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   954
         AG_TITLE = 'TUBE'                                                954
         AG_EXNAME = 'SHAPE'                                              954
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      954
         CALL AXSHAPE                                                     954
C    *                                                                    954
C    Create SOSM :                                                        955
C    Call To Local Procedure SOSM                                         955
         ASSIGN 6130 TO BLOCK_SOSM                                        955
         GO TO  6120                                                      955
6130     CONTINUE                                                         955
C    *                                                                    955
5131  CALL AGSPOPB                                                        956
      GO TO BLOCK_SOES, (5130 ,5140)                                      956
C End of Local Procedure                                                  956
C ----------------------------------------------------------------------  957
C ------------ SOSM                                                       957
C Begin Local Procedure SOSM                                              958
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        958
6120  CONTINUE                                                            958
         AG_TITLE='SOSM'                                                  958
         AG_MODULE=MODULE                                                 958
         AG_MARK='V00'                                                    958
         CALL AGSPUSH                                                     958
         AG_MODULE=MODULE                                                 958
C    *                                                                    958
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       959
         CALL AGSRESET                                                    959
         AG_SEEN = 0                                                      959
         AG_COLO = 1                                                      959
         AG_TITLE = 'SOSM'                                                959
         AG_EXNAME = 'ATTRIBUTE'                                          959
         AG_PARLIST = 'SEEN_COLO'                                         959
         CALL AXATTRIBUTE                                                 959
C    *                                                                    959
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                959
         CALL AGSRESET                                                    960
         AG_NDIV = 4                                                      960
         AG_IAXIS = 2                                                     960
         AG_C0 = 45                                                       960
         AG_TITLE = 'DIVISION'                                            960
         AG_EXNAME = 'SHAPE'                                              960
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     960
         CALL AXSHAPE                                                     960
C    *                                                                    960
C    Create SCRW :                                                        961
C    Call To Local Procedure SCRW                                         961
         ASSIGN 6140 TO BLOCK_SCRW                                        961
         GO TO  6100                                                      961
6140     CONTINUE                                                         961
C    *                                                                    961
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  962
C    ===                                                                  962
         CALL AGSRESET                                                    962
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      962
         AG_TITLE = 'SCRW'                                                962
         AG_EXNAME = 'POSITION'                                           962
         AG_PARLIST = 'X'                                                 962
         CALL AXPOSITION                                                  962
C    *                                                                    962
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  962
C    ===                                                                  962
         CALL AGSRESET                                                    963
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      963
         AG_TITLE = 'SCRW'                                                963
         AG_EXNAME = 'POSITION'                                           963
         AG_PARLIST = 'X'                                                 963
         CALL AXPOSITION                                                  963
C    *                                                                    963
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  963
C    =SSUP_ERJ2Y <===                                                     963
         CALL AGSRESET                                                    964
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      964
         AG_Y = SSUP_ERJ2Y                                                964
         AG_TITLE = 'SCRW'                                                964
         AG_EXNAME = 'POSITION'                                           964
         AG_PARLIST = 'X_Y'                                               964
         CALL AXPOSITION                                                  964
C    *                                                                    964
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  964
C    =-SSUP_ERJ2Y <===                                                    964
         CALL AGSRESET                                                    965
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      965
         AG_Y = -SSUP_ERJ2Y                                               965
         AG_TITLE = 'SCRW'                                                965
         AG_EXNAME = 'POSITION'                                           965
         AG_PARLIST = 'X_Y'                                               965
         CALL AXPOSITION                                                  965
C    *                                                                    965
6121  CALL AGSPOPB                                                        966
      GO TO BLOCK_SOSM, (6120 ,6130)                                      966
C End of Local Procedure                                                  966
C ----------------------------------------------------------------------  967
C ------------ SBRG                                                       967
C Begin Local Procedure SBRG                                              968
C SBRG IS THE BRACKET JOINING THE END RINGS                               968
5110  CONTINUE                                                            968
         AG_TITLE='SBRG'                                                  968
         AG_MODULE=MODULE                                                 968
         AG_MARK='V00'                                                    968
         CALL AGSPUSH                                                     968
         AG_MODULE=MODULE                                                 968
C    *                                                                    968
         RIN=SERG_IRNGPRMN                                                969
         ROU=SERG_ORNGRMAX                                                970
C    Geant MATERIAL : AIR ===> inherited arguments used <===              971
         CALL AGSRESET                                                    971
         AG_TITLE = 'AIR'                                                 971
         AG_EXNAME = 'MATERIAL'                                           971
         AG_PARLIST = 'NONE'                                              971
         CALL AXMATERIAL                                                  971
C    *                                                                    971
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       971
         CALL AGSRESET                                                    972
         AG_SEEN = 0                                                      972
         AG_COLO = 1                                                      972
         AG_TITLE = 'SBRG'                                                972
         AG_EXNAME = 'ATTRIBUTE'                                          972
         AG_PARLIST = 'SEEN_COLO'                                         972
         CALL AXATTRIBUTE                                                 972
C    *                                                                    972
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      972
         CALL AGSRESET                                                    973
         AG_RMIN = RIN                                                    973
         AG_RMAX = ROU                                                    973
         AG_DZ = SSUP_ERJTHK/2                                            973
         AG_TITLE = 'TUBE'                                                973
         AG_EXNAME = 'SHAPE'                                              973
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      973
         CALL AXSHAPE                                                     973
C    *                                                                    973
C    Create SBRM :                                                        974
C    Call To Local Procedure SBRM                                         974
         ASSIGN 6160 TO BLOCK_SBRM                                        974
         GO TO  6150                                                      974
6160     CONTINUE                                                         974
C    *                                                                    974
5111  CALL AGSPOPB                                                        975
      GO TO BLOCK_SBRG, (5110 ,5120)                                      975
C End of Local Procedure                                                  975
C ----------------------------------------------------------------------  976
C ------------ SBRM                                                       976
C Begin Local Procedure SBRM                                              977
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          977
6150  CONTINUE                                                            977
         AG_TITLE='SBRM'                                                  977
         AG_MODULE=MODULE                                                 977
         AG_MARK='V00'                                                    977
         CALL AGSPUSH                                                     977
         AG_MODULE=MODULE                                                 977
C    *                                                                    977
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       978
         CALL AGSRESET                                                    978
         AG_SEEN = 0                                                      978
         AG_COLO = 1                                                      978
         AG_TITLE = 'SBRM'                                                978
         AG_EXNAME = 'ATTRIBUTE'                                          978
         AG_PARLIST = 'SEEN_COLO'                                         978
         CALL AXATTRIBUTE                                                 978
C    *                                                                    978
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                978
         CALL AGSRESET                                                    979
         AG_NDIV = 4                                                      979
         AG_IAXIS = 2                                                     979
         AG_C0 = 45                                                       979
         AG_TITLE = 'DIVISION'                                            979
         AG_EXNAME = 'SHAPE'                                              979
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     979
         CALL AXSHAPE                                                     979
C    *                                                                    979
C    Create SBRI :                                                        980
C    Call To Local Procedure SBRI                                         980
         ASSIGN 6180 TO BLOCK_SBRI                                        980
         GO TO  6170                                                      980
6180     CONTINUE                                                         980
C    *                                                                    980
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        981
         CALL AGSRESET                                                    981
         AG_X = SSUP_ERJRAD                                               981
         AG_TITLE = 'SBRI'                                                981
         AG_EXNAME = 'POSITION'                                           981
         AG_PARLIST = 'X'                                                 981
         CALL AXPOSITION                                                  981
C    *                                                                    981
6151  CALL AGSPOPB                                                        982
      GO TO BLOCK_SBRM, (6150 ,6160)                                      982
C End of Local Procedure                                                  982
C ----------------------------------------------------------------------  983
C ------------ SBRI                                                       983
C Begin Local Procedure SBRI                                              984
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               984
6170  CONTINUE                                                            984
         AG_TITLE='SBRI'                                                  984
         AG_MODULE=MODULE                                                 984
         AG_MARK='V00'                                                    984
         CALL AGSPUSH                                                     984
         AG_MODULE=MODULE                                                 984
C    *                                                                    984
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       985
         CALL AGSRESET                                                    985
         AG_SEEN = 1                                                      985
         AG_COLO = 2                                                      985
         AG_TITLE = 'SBRI'                                                985
         AG_EXNAME = 'ATTRIBUTE'                                          985
         AG_PARLIST = 'SEEN_COLO'                                         985
         CALL AXATTRIBUTE                                                 985
C    *                                                                    985
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        985
         CALL AGSRESET                                                    986
         AG_TITLE = 'BERILLIUM'                                           986
         AG_EXNAME = 'MATERIAL'                                           986
         AG_PARLIST = 'NONE'                                              986
         CALL AXMATERIAL                                                  986
C    *                                                                    986
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  986
C    RJTHK/2 <===                                                         986
         CALL AGSRESET                                                    988
         AG_DX = SSUP_ERJLEN/2                                            988
         AG_DY = SSUP_ERJWID/2                                            988
         AG_DZ = SSUP_ERJTHK/2                                            988
         AG_TITLE = 'BOX'                                                 988
         AG_EXNAME = 'SHAPE'                                              988
         AG_PARLIST = 'DX_DY_DZ'                                          988
         CALL AXSHAPE                                                     988
C    *                                                                    988
6171  CALL AGSPOPB                                                        989
      GO TO BLOCK_SBRI, (6170 ,6180)                                      989
C End of Local Procedure                                                  989
C ----------------------------------------------------------------------  990
C ------------ SROD                                                       990
C Begin Local Procedure SROD                                              991
C SROD IS THE SVT CARBON COMPOSITE SUPPORT ROD                            991
5250  CONTINUE                                                            991
         AG_TITLE='SROD'                                                  991
         AG_MODULE=MODULE                                                 991
         AG_MARK='V00'                                                    991
         CALL AGSPUSH                                                     991
         AG_MODULE=MODULE                                                 991
C    *                                                                    991
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           992
         CALL AGSRESET                                                    992
         AG_TITLE = 'CARBON'                                              992
         AG_EXNAME = 'MATERIAL'                                           992
         AG_PARLIST = 'NONE'                                              992
         CALL AXMATERIAL                                                  992
C    *                                                                    992
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       992
         CALL AGSRESET                                                    993
         AG_SEEN = 1                                                      993
         AG_COLO = 2                                                      993
         AG_TITLE = 'SROD'                                                993
         AG_EXNAME = 'ATTRIBUTE'                                          993
         AG_PARLIST = 'SEEN_COLO'                                         993
         CALL AXATTRIBUTE                                                 993
C    *                                                                    993
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  993
C    P_RODLEN/2 <===                                                      993
         CALL AGSRESET                                                    995
         AG_RMIN = SSUP_RODID/2                                           995
         AG_RMAX = SSUP_RODOD/2                                           995
         AG_DZ = SSUP_RODLEN/2                                            995
         AG_TITLE = 'TUBE'                                                995
         AG_EXNAME = 'SHAPE'                                              995
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      995
         CALL AXSHAPE                                                     995
C    *                                                                    995
5251  CALL AGSPOPB                                                        996
      GO TO BLOCK_SROD, (5250 ,5260)                                      996
C End of Local Procedure                                                  996
C ----------------------------------------------------------------------  997
C ------------ SCON                                                       997
C Begin Local Procedure SCON                                              998
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               998
5210  CONTINUE                                                            998
         AG_TITLE='SCON'                                                  998
         AG_MODULE=MODULE                                                 998
         AG_MARK='V00'                                                    998
         CALL AGSPUSH                                                     998
         AG_MODULE=MODULE                                                 998
C    *                                                                    998
         CONE_THK1=SSUP_GRPHTHK                                           999
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                1000
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              1001
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1002
         CALL AGSRESET                                                    1002
         AG_TITLE = 'AIR'                                                 1002
         AG_EXNAME = 'MATERIAL'                                           1002
         AG_PARLIST = 'NONE'                                              1002
         CALL AXMATERIAL                                                  1002
C    *                                                                    1002
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       1002
         CALL AGSRESET                                                    1003
         AG_SEEN = 0                                                      1003
         AG_COLO = 1                                                      1003
         AG_TITLE = 'SCON'                                                1003
         AG_EXNAME = 'ATTRIBUTE'                                          1003
         AG_PARLIST = 'SEEN_COLO'                                         1003
         CALL AXATTRIBUTE                                                 1003
C    *                                                                    1003
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  1003
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  1003
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  1003
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  1003
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  1003
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  1003
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      1003
         CALL AGSRESET                                                    1013
         AG_PHI1 = 0                                                      1013
         AG_DPHI = 360                                                    1013
         AG_NZ = 7                                                        1013
         AG_ZI(1)=SSUP_CONE1ZMN                                           1013
         AG_ZI(2)=SSUP_RODLEN/2                                           1013
         AG_ZI(3)=SSUP_RODLEN/2                                           1013
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1013
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1013
         AG_ZI(6)=SSUP_CONE3ZMX                                           1013
         AG_ZI(7)=SSUP_CONE4ZMX                                           1013
         AG_RMN(1)=SSUP_CON1IDMN                                          1013
         AG_RMN(2)=SSUP_CON1IDMN                                          1013
         AG_RMN(3)=SSUP_CON1IDMN                                          1013
         AG_RMN(4)=SSUP_CON1IDMN                                          1013
         AG_RMN(5)=SSUP_CON3IDMN                                          1013
         AG_RMN(6)=SSUP_CON4IDMN                                          1013
         AG_RMN(7)=SSUP_CON4IDMX                                          1013
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                1013
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                1013
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                1013
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                1013
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                1013
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                1013
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                1013
         AG_TITLE = 'PCON'                                                1013
         AG_EXNAME = 'SHAPE'                                              1013
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1013
         CALL AXSHAPE                                                     1013
C    *                                                                    1013
C    Create SGRA :                                                        1014
C    Call To Local Procedure SGRA                                         1014
         ASSIGN 6200 TO BLOCK_SGRA                                        1014
         GO TO  6190                                                      1014
6200     CONTINUE                                                         1014
C    *                                                                    1014
C    Geant POSITION : SGRA ===> inherited arguments used <===             1015
         CALL AGSRESET                                                    1015
         AG_TITLE = 'SGRA'                                                1015
         AG_EXNAME = 'POSITION'                                           1015
         AG_PARLIST = 'NONE'                                              1015
         CALL AXPOSITION                                                  1015
C    *                                                                    1015
C    Create STAP :                                                        1016
C    Call To Local Procedure STAP                                         1016
         ASSIGN 6220 TO BLOCK_STAP                                        1016
         GO TO  6210                                                      1016
6220     CONTINUE                                                         1016
C    *                                                                    1016
C    Geant POSITION : STAP ===> inherited arguments used <===             1017
         CALL AGSRESET                                                    1017
         AG_TITLE = 'STAP'                                                1017
         AG_EXNAME = 'POSITION'                                           1017
         AG_PARLIST = 'NONE'                                              1017
         CALL AXPOSITION                                                  1017
C    *                                                                    1017
C    Create STAC :                                                        1018
C    Call To Local Procedure STAC                                         1018
         ASSIGN 6240 TO BLOCK_STAC                                        1018
         GO TO  6230                                                      1018
6240     CONTINUE                                                         1018
C    *                                                                    1018
C    Geant POSITION : STAC ===> inherited arguments used <===             1019
         CALL AGSRESET                                                    1019
         AG_TITLE = 'STAC'                                                1019
         AG_EXNAME = 'POSITION'                                           1019
         AG_PARLIST = 'NONE'                                              1019
         CALL AXPOSITION                                                  1019
C    *                                                                    1019
C    Create SHLA :                                                        1020
C    Call To Local Procedure SHLA                                         1020
         ASSIGN 6260 TO BLOCK_SHLA                                        1020
         GO TO  6250                                                      1020
6260     CONTINUE                                                         1020
C    *                                                                    1020
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  1022
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             1022
         CALL AGSRESET                                                    1022
         AG_Z = SSUP_RODLEN/                                              1022
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    1022
         AG_TITLE = 'SHLA'                                                1022
         AG_EXNAME = 'POSITION'                                           1022
         AG_PARLIST = 'Z'                                                 1022
         CALL AXPOSITION                                                  1022
C    *                                                                    1022
C    Create SHLB :                                                        1023
C    Call To Local Procedure SHLB                                         1023
         ASSIGN 6280 TO BLOCK_SHLB                                        1023
         GO TO  6270                                                      1023
6280     CONTINUE                                                         1023
C    *                                                                    1023
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  1024
C    CONE3ZMX) <===                                                       1024
         CALL AGSRESET                                                    1024
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           1024
         AG_TITLE = 'SHLB'                                                1024
         AG_EXNAME = 'POSITION'                                           1024
         AG_PARLIST = 'Z'                                                 1024
         CALL AXPOSITION                                                  1024
C    *                                                                    1024
C    Create SCMY :                                                        1025
C    Call To Local Procedure SCMY                                         1025
         ASSIGN 6300 TO BLOCK_SCMY                                        1025
         GO TO  6290                                                      1025
6300     CONTINUE                                                         1025
C    *                                                                    1025
C    Geant POSITION : SCMY ===> inherited arguments used <===             1026
         CALL AGSRESET                                                    1026
         AG_TITLE = 'SCMY'                                                1026
         AG_EXNAME = 'POSITION'                                           1026
         AG_PARLIST = 'NONE'                                              1026
         CALL AXPOSITION                                                  1026
C    *                                                                    1026
C    Create SCAL :                                                        1027
C    Call To Local Procedure SCAL                                         1027
         ASSIGN 6320 TO BLOCK_SCAL                                        1027
         GO TO  6310                                                      1027
6320     CONTINUE                                                         1027
C    *                                                                    1027
C    Geant POSITION : SCAL ===> inherited arguments used <===             1028
         CALL AGSRESET                                                    1028
         AG_TITLE = 'SCAL'                                                1028
         AG_EXNAME = 'POSITION'                                           1028
         AG_PARLIST = 'NONE'                                              1028
         CALL AXPOSITION                                                  1028
C    *                                                                    1028
5211  CALL AGSPOPB                                                        1029
      GO TO BLOCK_SCON, (5210 ,5220)                                      1029
C End of Local Procedure                                                  1029
C ----------------------------------------------------------------------  1030
C ------------ SGRA                                                       1030
C Begin Local Procedure SGRA                                              1031
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 1031
6190  CONTINUE                                                            1031
         AG_TITLE='SGRA'                                                  1031
         AG_MODULE=MODULE                                                 1031
         AG_MARK='V00'                                                    1031
         CALL AGSPUSH                                                     1031
         AG_MODULE=MODULE                                                 1031
C    *                                                                    1031
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1032
         CALL AGSRESET                                                    1032
         AG_TITLE = 'CARBON'                                              1032
         AG_EXNAME = 'MATERIAL'                                           1032
         AG_PARLIST = 'NONE'                                              1032
         CALL AXMATERIAL                                                  1032
C    *                                                                    1032
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       1032
         CALL AGSRESET                                                    1033
         AG_SEEN = 1                                                      1033
         AG_COLO = 6                                                      1033
         AG_TITLE = 'SGRA'                                                1033
         AG_EXNAME = 'ATTRIBUTE'                                          1033
         AG_PARLIST = 'SEEN_COLO'                                         1033
         CALL AXATTRIBUTE                                                 1033
C    *                                                                    1033
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  1033
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  1033
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  1033
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  1033
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  1033
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              1033
         CALL AGSRESET                                                    1041
         AG_PHI1 = 0                                                      1041
         AG_DPHI = 360                                                    1041
         AG_NZ = 5                                                        1041
         AG_ZI(1)=SSUP_RODLEN/2                                           1041
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1041
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1041
         AG_ZI(4)=SSUP_CONE3ZMX                                           1041
         AG_ZI(5)=SSUP_CONE4ZMX                                           1041
         AG_RMN(1)=SSUP_CON1IDMN                                          1041
         AG_RMN(2)=SSUP_CON1IDMN                                          1041
         AG_RMN(3)=SSUP_CON3IDMN                                          1041
         AG_RMN(4)=SSUP_CON4IDMN                                          1041
         AG_RMN(5)=SSUP_CON4IDMX                                          1041
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1041
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1041
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1041
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             1041
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             1041
         AG_TITLE = 'PCON'                                                1041
         AG_EXNAME = 'SHAPE'                                              1041
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1041
         CALL AXSHAPE                                                     1041
C    *                                                                    1041
6191  CALL AGSPOPB                                                        1042
      GO TO BLOCK_SGRA, (6190 ,6200)                                      1042
C End of Local Procedure                                                  1042
C ----------------------------------------------------------------------  1043
C ------------ STAP                                                       1043
C Begin Local Procedure STAP                                              1044
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  1044
C E)                                                                      1044
6210  CONTINUE                                                            1044
         AG_TITLE='STAP'                                                  1044
         AG_MODULE=MODULE                                                 1044
         AG_MARK='V00'                                                    1044
         CALL AGSPUSH                                                     1044
         AG_MODULE=MODULE                                                 1044
C    *                                                                    1044
         ROFFSET=SSUP_GRPHTHK                                             1045
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1046
         CALL AGSRESET                                                    1046
         AG_A = 12                                                        1046
         AG_Z = 6                                                         1046
         AG_W = 1                                                         1046
         AG_TITLE = 'C'                                                   1046
         AG_EXNAME = 'COMPONENT'                                          1046
         AG_PARLIST = 'A_Z_W'                                             1046
         CALL AXCOMPONENT                                                 1046
C    *                                                                    1046
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1046
         CALL AGSRESET                                                    1047
         AG_A = 1                                                         1047
         AG_Z = 1                                                         1047
         AG_W = 2                                                         1047
         AG_TITLE = 'H2'                                                  1047
         AG_EXNAME = 'COMPONENT'                                          1047
         AG_PARLIST = 'A_Z_W'                                             1047
         CALL AXCOMPONENT                                                 1047
C    *                                                                    1047
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1047
         CALL AGSRESET                                                    1048
         AG_DENS = 0.935                                                  1048
         AG_TITLE = 'CH2'                                                 1048
         AG_EXNAME = 'MIXTURE'                                            1048
         AG_PARLIST = 'DENS'                                              1048
         CALL AXMIXTURE                                                   1048
C    *                                                                    1048
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       1048
         CALL AGSRESET                                                    1049
         AG_SEEN = 1                                                      1049
         AG_COLO = 3                                                      1049
         AG_TITLE = 'STAP'                                                1049
         AG_EXNAME = 'ATTRIBUTE'                                          1049
         AG_PARLIST = 'SEEN_COLO'                                         1049
         CALL AXATTRIBUTE                                                 1049
C    *                                                                    1049
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1049
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1049
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1049
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1049
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1049
         CALL AGSRESET                                                    1055
         AG_PHI1 = 0                                                      1055
         AG_DPHI = 360                                                    1055
         AG_NZ = 3                                                        1055
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1055
         AG_ZI(2)=SSUP_CONE3ZMX                                           1055
         AG_ZI(3)=SSUP_CONE4ZMX                                           1055
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1055
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1055
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1055
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1055
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1055
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1055
         AG_TITLE = 'PCON'                                                1055
         AG_EXNAME = 'SHAPE'                                              1055
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1055
         CALL AXSHAPE                                                     1055
C    *                                                                    1055
6211  CALL AGSPOPB                                                        1056
      GO TO BLOCK_STAP, (6210 ,6220)                                      1056
C End of Local Procedure                                                  1056
C ----------------------------------------------------------------------  1057
C ------------ STAC                                                       1057
C Begin Local Procedure STAC                                              1058
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      1058
6230  CONTINUE                                                            1058
         AG_TITLE='STAC'                                                  1058
         AG_MODULE=MODULE                                                 1058
         AG_MARK='V00'                                                    1058
         CALL AGSPUSH                                                     1058
         AG_MODULE=MODULE                                                 1058
C    *                                                                    1058
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             1059
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           1060
         CALL AGSRESET                                                    1060
         AG_TITLE = 'COPPER'                                              1060
         AG_EXNAME = 'MATERIAL'                                           1060
         AG_PARLIST = 'NONE'                                              1060
         CALL AXMATERIAL                                                  1060
C    *                                                                    1060
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       1060
         CALL AGSRESET                                                    1061
         AG_SEEN = 1                                                      1061
         AG_COLO = 2                                                      1061
         AG_TITLE = 'STAC'                                                1061
         AG_EXNAME = 'ATTRIBUTE'                                          1061
         AG_PARLIST = 'SEEN_COLO'                                         1061
         CALL AXATTRIBUTE                                                 1061
C    *                                                                    1061
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1061
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1061
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1061
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1061
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1061
         CALL AGSRESET                                                    1067
         AG_PHI1 = 0                                                      1067
         AG_DPHI = 360                                                    1067
         AG_NZ = 3                                                        1067
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1067
         AG_ZI(2)=SSUP_CONE3ZMX                                           1067
         AG_ZI(3)=SSUP_CONE4ZMX                                           1067
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1067
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1067
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1067
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1067
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1067
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1067
         AG_TITLE = 'PCON'                                                1067
         AG_EXNAME = 'SHAPE'                                              1067
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1067
         CALL AXSHAPE                                                     1067
C    *                                                                    1067
6231  CALL AGSPOPB                                                        1068
      GO TO BLOCK_STAC, (6230 ,6240)                                      1068
C End of Local Procedure                                                  1068
C ----------------------------------------------------------------------  1069
C ------------ SHLA                                                       1069
C Begin Local Procedure SHLA                                              1070
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              1070
6250  CONTINUE                                                            1070
         AG_TITLE='SHLA'                                                  1070
         AG_MODULE=MODULE                                                 1070
         AG_MARK='V00'                                                    1070
         CALL AGSPUSH                                                     1070
         AG_MODULE=MODULE                                                 1070
C    *                                                                    1070
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1071
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1072
         CALL AGSRESET                                                    1072
         AG_TITLE = 'AIR'                                                 1072
         AG_EXNAME = 'MATERIAL'                                           1072
         AG_PARLIST = 'NONE'                                              1072
         CALL AXMATERIAL                                                  1072
C    *                                                                    1072
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       1072
         CALL AGSRESET                                                    1073
         AG_SEEN = 0                                                      1073
         AG_COLO = 1                                                      1073
         AG_TITLE = 'SHLA'                                                1073
         AG_EXNAME = 'ATTRIBUTE'                                          1073
         AG_PARLIST = 'SEEN_COLO'                                         1073
         CALL AXATTRIBUTE                                                 1073
C    *                                                                    1073
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  1073
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  1073
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  1073
C    .0*SSUP_HOSRMX <===                                                  1073
         CALL AGSRESET                                                    1077
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            1077
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  1077
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1077
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  1077
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1077
         AG_TITLE = 'CONE'                                                1077
         AG_EXNAME = 'SHAPE'                                              1077
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1077
         CALL AXSHAPE                                                     1077
C    *                                                                    1077
C    Create SHMA :                                                        1078
C    Call To Local Procedure SHMA                                         1078
         ASSIGN 6340 TO BLOCK_SHMA                                        1078
         GO TO  6330                                                      1078
6340     CONTINUE                                                         1078
C    *                                                                    1078
6251  CALL AGSPOPB                                                        1079
      GO TO BLOCK_SHLA, (6250 ,6260)                                      1079
C End of Local Procedure                                                  1079
C ----------------------------------------------------------------------  1080
C ------------ SHMA                                                       1080
C Begin Local Procedure SHMA                                              1081
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           1081
6330  CONTINUE                                                            1081
         AG_TITLE='SHMA'                                                  1081
         AG_MODULE=MODULE                                                 1081
         AG_MARK='V00'                                                    1081
         CALL AGSPUSH                                                     1081
         AG_MODULE=MODULE                                                 1081
C    *                                                                    1081
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1082
         RMIN=SSUP_CON3IDMN + ROFFSET                                     1083
         RMAX=SSUP_CON4IDMN + ROFFSET                                     1084
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                1085
         ZMAX=SSUP_CONE3ZMX                                               1086
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1087
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1088
         CONE_LEN = SQRT(CONE_LEN)                                        1089
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1090
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1091
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1092
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1093
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1094
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       1095
         CALL AGSRESET                                                    1095
         AG_SEEN = 0                                                      1095
         AG_COLO = 1                                                      1095
         AG_TITLE = 'SHMA'                                                1095
         AG_EXNAME = 'ATTRIBUTE'                                          1095
         AG_PARLIST = 'SEEN_COLO'                                         1095
         CALL AXATTRIBUTE                                                 1095
C    *                                                                    1095
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1095
         CALL AGSRESET                                                    1097
         AG_NDIV = SSUP_NHOSES                                            1097
         AG_IAXIS = 2                                                     1097
         AG_C0 = 0.0                                                      1097
         AG_TITLE = 'DIVISION'                                            1097
         AG_EXNAME = 'SHAPE'                                              1097
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1097
         CALL AXSHAPE                                                     1097
C    *                                                                    1097
C    Create SWHO :                                                        1098
C    Call To Local Procedure SWHO                                         1098
         ASSIGN 6360 TO BLOCK_SWHO                                        1098
         GO TO  6350                                                      1098
6360     CONTINUE                                                         1098
C    *                                                                    1098
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1099
         CALL AGSRESET                                                    1099
         AG_X = XPOS                                                      1099
         AG_Y = 0                                                         1099
         AG_Z = ZPOS                                                      1099
         AG_ALPHAN=AG_ALPHAN+1                                            1099
         AG_ALPHAK(AG_ALPHAN)=2                                           1099
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1099
         AG_TITLE = 'SWHO'                                                1099
         AG_EXNAME = 'POSITION'                                           1099
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1099
         CALL AXPOSITION                                                  1099
C    *                                                                    1099
6331  CALL AGSPOPB                                                        1100
      GO TO BLOCK_SHMA, (6330 ,6340)                                      1100
C End of Local Procedure                                                  1100
C ----------------------------------------------------------------------  1101
C ------------ SHLB                                                       1101
C Begin Local Procedure SHLB                                              1102
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               1102
6270  CONTINUE                                                            1102
         AG_TITLE='SHLB'                                                  1102
         AG_MODULE=MODULE                                                 1102
         AG_MARK='V00'                                                    1102
         CALL AGSPUSH                                                     1102
         AG_MODULE=MODULE                                                 1102
C    *                                                                    1102
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1103
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1104
         CALL AGSRESET                                                    1104
         AG_TITLE = 'AIR'                                                 1104
         AG_EXNAME = 'MATERIAL'                                           1104
         AG_PARLIST = 'NONE'                                              1104
         CALL AXMATERIAL                                                  1104
C    *                                                                    1104
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       1104
         CALL AGSRESET                                                    1105
         AG_SEEN = 0                                                      1105
         AG_COLO = 1                                                      1105
         AG_TITLE = 'SHLB'                                                1105
         AG_EXNAME = 'ATTRIBUTE'                                          1105
         AG_PARLIST = 'SEEN_COLO'                                         1105
         CALL AXATTRIBUTE                                                 1105
C    *                                                                    1105
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  1105
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  1105
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  1105
C    X <===                                                               1105
         CALL AGSRESET                                                    1109
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         1109
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  1109
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1109
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  1109
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  1109
         AG_TITLE = 'CONE'                                                1109
         AG_EXNAME = 'SHAPE'                                              1109
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1109
         CALL AXSHAPE                                                     1109
C    *                                                                    1109
C    Create SHMB :                                                        1110
C    Call To Local Procedure SHMB                                         1110
         ASSIGN 6380 TO BLOCK_SHMB                                        1110
         GO TO  6370                                                      1110
6380     CONTINUE                                                         1110
C    *                                                                    1110
6271  CALL AGSPOPB                                                        1111
      GO TO BLOCK_SHLB, (6270 ,6280)                                      1111
C End of Local Procedure                                                  1111
C ----------------------------------------------------------------------  1112
C ------------ SHMB                                                       1112
C Begin Local Procedure SHMB                                              1113
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           1113
6370  CONTINUE                                                            1113
         AG_TITLE='SHMB'                                                  1113
         AG_MODULE=MODULE                                                 1113
         AG_MARK='V00'                                                    1113
         CALL AGSPUSH                                                     1113
         AG_MODULE=MODULE                                                 1113
C    *                                                                    1113
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1114
         RMIN=SSUP_CON4IDMN + ROFFSET                                     1115
         RMAX=SSUP_CON4IDMX + ROFFSET                                     1116
         ZMIN=SSUP_CONE3ZMX                                               1117
         ZMAX=SSUP_CONE4ZMX                                               1118
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1119
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1120
         CONE_LEN = SQRT(CONE_LEN)                                        1121
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1122
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1123
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1124
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1125
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1126
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       1127
         CALL AGSRESET                                                    1127
         AG_SEEN = 0                                                      1127
         AG_COLO = 1                                                      1127
         AG_TITLE = 'SHMB'                                                1127
         AG_EXNAME = 'ATTRIBUTE'                                          1127
         AG_PARLIST = 'SEEN_COLO'                                         1127
         CALL AXATTRIBUTE                                                 1127
C    *                                                                    1127
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1127
         CALL AGSRESET                                                    1129
         AG_NDIV = SSUP_NHOSES                                            1129
         AG_IAXIS = 2                                                     1129
         AG_C0 = 0.0                                                      1129
         AG_TITLE = 'DIVISION'                                            1129
         AG_EXNAME = 'SHAPE'                                              1129
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1129
         CALL AXSHAPE                                                     1129
C    *                                                                    1129
C    Create SWHO :                                                        1130
C    Call To Local Procedure SWHO                                         1130
         ASSIGN 6390 TO BLOCK_SWHO                                        1130
         GO TO  6350                                                      1130
6390     CONTINUE                                                         1130
C    *                                                                    1130
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1131
         CALL AGSRESET                                                    1131
         AG_X = XPOS                                                      1131
         AG_Y = 0                                                         1131
         AG_Z = ZPOS                                                      1131
         AG_ALPHAN=AG_ALPHAN+1                                            1131
         AG_ALPHAK(AG_ALPHAN)=2                                           1131
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1131
         AG_TITLE = 'SWHO'                                                1131
         AG_EXNAME = 'POSITION'                                           1131
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1131
         CALL AXPOSITION                                                  1131
C    *                                                                    1131
6371  CALL AGSPOPB                                                        1132
      GO TO BLOCK_SHMB, (6370 ,6380)                                      1132
C End of Local Procedure                                                  1132
C ----------------------------------------------------------------------  1133
C ------------ SWHO                                                       1133
C Begin Local Procedure SWHO                                              1134
C SWHO IS A WATER HOSE                                                    1134
6350  CONTINUE                                                            1134
         AG_TITLE='SWHO'                                                  1134
         AG_MODULE=MODULE                                                 1134
         AG_MARK='V00'                                                    1134
         CALL AGSPUSH                                                     1134
         AG_MODULE=MODULE                                                 1134
C    *                                                                    1134
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1135
         CALL AGSRESET                                                    1135
         AG_SEEN = 1                                                      1135
         AG_COLO = 3                                                      1135
         AG_TITLE = 'SWHO'                                                1135
         AG_EXNAME = 'ATTRIBUTE'                                          1135
         AG_PARLIST = 'SEEN_COLO'                                         1135
         CALL AXATTRIBUTE                                                 1135
C    *                                                                    1135
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1135
         CALL AGSRESET                                                    1136
         AG_A = 12                                                        1136
         AG_Z = 6                                                         1136
         AG_W = 1                                                         1136
         AG_TITLE = 'C'                                                   1136
         AG_EXNAME = 'COMPONENT'                                          1136
         AG_PARLIST = 'A_Z_W'                                             1136
         CALL AXCOMPONENT                                                 1136
C    *                                                                    1136
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1136
         CALL AGSRESET                                                    1137
         AG_A = 1                                                         1137
         AG_Z = 1                                                         1137
         AG_W = 2                                                         1137
         AG_TITLE = 'H2'                                                  1137
         AG_EXNAME = 'COMPONENT'                                          1137
         AG_PARLIST = 'A_Z_W'                                             1137
         CALL AXCOMPONENT                                                 1137
C    *                                                                    1137
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1137
         CALL AGSRESET                                                    1138
         AG_DENS = 0.935                                                  1138
         AG_TITLE = 'CH2'                                                 1138
         AG_EXNAME = 'MIXTURE'                                            1138
         AG_PARLIST = 'DENS'                                              1138
         CALL AXMIXTURE                                                   1138
C    *                                                                    1138
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1138
         CALL AGSRESET                                                    1139
         AG_SEEN = 1                                                      1139
         AG_COLO = 3                                                      1139
         AG_TITLE = 'SWHO'                                                1139
         AG_EXNAME = 'ATTRIBUTE'                                          1139
         AG_PARLIST = 'SEEN_COLO'                                         1139
         CALL AXATTRIBUTE                                                 1139
C    *                                                                    1139
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  1139
C    =                                                                    1139
         CALL AGSRESET                                                    1140
         AG_RMIN = 0                                                      1140
         AG_RMAX = SSUP_HOSRMX                                            1140
         AG_DZ = 0.5*CONE_LEN                                             1140
         AG_TITLE = 'TUBE'                                                1140
         AG_EXNAME = 'SHAPE'                                              1140
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1140
         CALL AXSHAPE                                                     1140
C    *                                                                    1140
C    Create SHWA :                                                        1141
C    Call To Local Procedure SHWA                                         1141
         ASSIGN 6410 TO BLOCK_SHWA                                        1141
         GO TO  6400                                                      1141
6410     CONTINUE                                                         1141
C    *                                                                    1141
C    Geant POSITION : SHWA ===> inherited arguments used <===             1142
         CALL AGSRESET                                                    1142
         AG_TITLE = 'SHWA'                                                1142
         AG_EXNAME = 'POSITION'                                           1142
         AG_PARLIST = 'NONE'                                              1142
         CALL AXPOSITION                                                  1142
C    *                                                                    1142
6351  CALL AGSPOPB                                                        1143
      GO TO BLOCK_SWHO, (6350 ,6360,6390)                                 1143
C End of Local Procedure                                                  1143
C ----------------------------------------------------------------------  1144
C ------------ SHWA                                                       1144
C Begin Local Procedure SHWA                                              1145
C SHWA IS THE WATER IN THE HOSE                                           1145
6400  CONTINUE                                                            1145
         AG_TITLE='SHWA'                                                  1145
         AG_MODULE=MODULE                                                 1145
         AG_MARK='V00'                                                    1145
         CALL AGSPUSH                                                     1145
         AG_MODULE=MODULE                                                 1145
C    *                                                                    1145
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       1146
         CALL AGSRESET                                                    1146
         AG_SEEN = 1                                                      1146
         AG_COLO = 6                                                      1146
         AG_TITLE = 'SHWA'                                                1146
         AG_EXNAME = 'ATTRIBUTE'                                          1146
         AG_PARLIST = 'SEEN_COLO'                                         1146
         CALL AXATTRIBUTE                                                 1146
C    *                                                                    1146
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1146
         CALL AGSRESET                                                    1147
         AG_TITLE = 'WATER'                                               1147
         AG_EXNAME = 'MATERIAL'                                           1147
         AG_PARLIST = 'NONE'                                              1147
         CALL AXMATERIAL                                                  1147
C    *                                                                    1147
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        1147
         CALL AGSRESET                                                    1148
         AG_RMAX = SSUP_HOSRMN                                            1148
         AG_TITLE = 'TUBE'                                                1148
         AG_EXNAME = 'SHAPE'                                              1148
         AG_PARLIST = 'RMAX'                                              1148
         CALL AXSHAPE                                                     1148
C    *                                                                    1148
6401  CALL AGSPOPB                                                        1149
      GO TO BLOCK_SHWA, (6400 ,6410)                                      1149
C End of Local Procedure                                                  1149
C ----------------------------------------------------------------------  1150
C ------------ SCMY                                                       1150
C Begin Local Procedure SCMY                                              1151
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            1151
6290  CONTINUE                                                            1151
         AG_TITLE='SCMY'                                                  1151
         AG_MODULE=MODULE                                                 1151
         AG_MARK='V00'                                                    1151
         CALL AGSPUSH                                                     1151
         AG_MODULE=MODULE                                                 1151
C    *                                                                    1151
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             1152
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          1153
         CALL AGSRESET                                                    1153
         AG_A = 12                                                        1153
         AG_Z = 6                                                         1153
         AG_W = 5                                                         1153
         AG_TITLE = 'C5'                                                  1153
         AG_EXNAME = 'COMPONENT'                                          1153
         AG_PARLIST = 'A_Z_W'                                             1153
         CALL AXCOMPONENT                                                 1153
C    *                                                                    1153
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           1153
         CALL AGSRESET                                                    1154
         AG_A = 1                                                         1154
         AG_Z = 1                                                         1154
         AG_W = 4                                                         1154
         AG_TITLE = 'H4'                                                  1154
         AG_EXNAME = 'COMPONENT'                                          1154
         AG_PARLIST = 'A_Z_W'                                             1154
         CALL AXCOMPONENT                                                 1154
C    *                                                                    1154
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          1154
         CALL AGSRESET                                                    1155
         AG_A = 16                                                        1155
         AG_Z = 8                                                         1155
         AG_W = 2                                                         1155
         AG_TITLE = 'O2'                                                  1155
         AG_EXNAME = 'COMPONENT'                                          1155
         AG_PARLIST = 'A_Z_W'                                             1155
         CALL AXCOMPONENT                                                 1155
C    *                                                                    1155
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            1155
         CALL AGSRESET                                                    1156
         AG_DENS = 1.39                                                   1156
         AG_TITLE = 'MYLAR'                                               1156
         AG_EXNAME = 'MIXTURE'                                            1156
         AG_PARLIST = 'DENS'                                              1156
         CALL AXMIXTURE                                                   1156
C    *                                                                    1156
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       1156
         CALL AGSRESET                                                    1157
         AG_SEEN = 1                                                      1157
         AG_COLO = 3                                                      1157
         AG_TITLE = 'SCMY'                                                1157
         AG_EXNAME = 'ATTRIBUTE'                                          1157
         AG_PARLIST = 'SEEN_COLO'                                         1157
         CALL AXATTRIBUTE                                                 1157
C    *                                                                    1157
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1157
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1157
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1157
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  1157
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     1157
         CALL AGSRESET                                                    1163
         AG_PHI1 = 0                                                      1163
         AG_DPHI = 360                                                    1163
         AG_NZ = 3                                                        1163
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1163
         AG_ZI(2)=SSUP_CONE3ZMX                                           1163
         AG_ZI(3)=SSUP_CONE4ZMX                                           1163
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1163
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1163
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1163
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    1163
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    1163
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    1163
         AG_TITLE = 'PCON'                                                1163
         AG_EXNAME = 'SHAPE'                                              1163
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1163
         CALL AXSHAPE                                                     1163
C    *                                                                    1163
6291  CALL AGSPOPB                                                        1164
      GO TO BLOCK_SCMY, (6290 ,6300)                                      1164
C End of Local Procedure                                                  1164
C ----------------------------------------------------------------------  1165
C ------------ SCAL                                                       1165
C Begin Local Procedure SCAL                                              1166
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     1166
6310  CONTINUE                                                            1166
         AG_TITLE='SCAL'                                                  1166
         AG_MODULE=MODULE                                                 1166
         AG_MARK='V00'                                                    1166
         CALL AGSPUSH                                                     1166
         AG_MODULE=MODULE                                                 1166
C    *                                                                    1166
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           1167
     *   SSUP_WRPMYTHK                                                    1167
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1168
         CALL AGSRESET                                                    1168
         AG_TITLE = 'ALUMINIUM'                                           1168
         AG_EXNAME = 'MATERIAL'                                           1168
         AG_PARLIST = 'NONE'                                              1168
         CALL AXMATERIAL                                                  1168
C    *                                                                    1168
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       1168
         CALL AGSRESET                                                    1169
         AG_SEEN = 1                                                      1169
         AG_COLO = 2                                                      1169
         AG_TITLE = 'SCAL'                                                1169
         AG_EXNAME = 'ATTRIBUTE'                                          1169
         AG_PARLIST = 'SEEN_COLO'                                         1169
         CALL AXATTRIBUTE                                                 1169
C    *                                                                    1169
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1169
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1169
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1169
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  1169
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     1169
         CALL AGSRESET                                                    1175
         AG_PHI1 = 0                                                      1175
         AG_DPHI = 360                                                    1175
         AG_NZ = 3                                                        1175
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1175
         AG_ZI(2)=SSUP_CONE3ZMX                                           1175
         AG_ZI(3)=SSUP_CONE4ZMX                                           1175
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1175
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1175
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1175
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    1175
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    1175
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    1175
         AG_TITLE = 'PCON'                                                1175
         AG_EXNAME = 'SHAPE'                                              1175
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1175
         CALL AXSHAPE                                                     1175
C    *                                                                    1175
6311  CALL AGSPOPB                                                        1176
      GO TO BLOCK_SCAL, (6310 ,6320)                                      1176
C End of Local Procedure                                                  1176
C ----------------------------------------------------------------------  1177
C ------------ SWMM                                                       1177
C Begin Local Procedure SWMM                                              1178
C SWMM IS THE WATER MANIFOLD MOTHER                                       1178
5170  CONTINUE                                                            1178
         AG_TITLE='SWMM'                                                  1178
         AG_MODULE=MODULE                                                 1178
         AG_MARK='V00'                                                    1178
         CALL AGSPUSH                                                     1178
         AG_MODULE=MODULE                                                 1178
C    *                                                                    1178
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     1179
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     1180
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1181
         CALL AGSRESET                                                    1181
         AG_TITLE = 'AIR'                                                 1181
         AG_EXNAME = 'MATERIAL'                                           1181
         AG_PARLIST = 'NONE'                                              1181
         CALL AXMATERIAL                                                  1181
C    *                                                                    1181
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       1181
         CALL AGSRESET                                                    1182
         AG_SEEN = 0                                                      1182
         AG_COLO = 1                                                      1182
         AG_TITLE = 'SWMM'                                                1182
         AG_EXNAME = 'ATTRIBUTE'                                          1182
         AG_PARLIST = 'SEEN_COLO'                                         1182
         CALL AXATTRIBUTE                                                 1182
C    *                                                                    1182
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1182
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   1182
         CALL AGSRESET                                                    1185
         AG_PHI1 = 0                                                      1185
         AG_DPHI = 360                                                    1185
         AG_NPDIV = 18                                                    1185
         AG_NZ = 2                                                        1185
         AG_ZI(1)=-SWAM_LEN/2                                             1185
         AG_ZI(2)=+SWAM_LEN/2                                             1185
         AG_RMN(1)=RIN                                                    1185
         AG_RMN(2)=RIN                                                    1185
         AG_RMX(1)=ROU                                                    1185
         AG_RMX(2)=ROU                                                    1185
         AG_TITLE = 'PGON'                                                1185
         AG_EXNAME = 'SHAPE'                                              1185
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1185
         CALL AXSHAPE                                                     1185
C    *                                                                    1185
C    Create SWMB :                                                        1186
C    Call To Local Procedure SWMB                                         1186
         ASSIGN 6430 TO BLOCK_SWMB                                        1186
         GO TO  6420                                                      1186
6430     CONTINUE                                                         1186
C    *                                                                    1186
C    Geant POSITION : SWMB ===> inherited arguments used <===             1187
         CALL AGSRESET                                                    1187
         AG_TITLE = 'SWMB'                                                1187
         AG_EXNAME = 'POSITION'                                           1187
         AG_PARLIST = 'NONE'                                              1187
         CALL AXPOSITION                                                  1187
C    *                                                                    1187
C    Create SWMT :                                                        1188
C    Call To Local Procedure SWMT                                         1188
         ASSIGN 6450 TO BLOCK_SWMT                                        1188
         GO TO  6440                                                      1188
6450     CONTINUE                                                         1188
C    *                                                                    1188
C    Geant POSITION : SWMT ===> inherited arguments used <===             1189
         CALL AGSRESET                                                    1189
         AG_TITLE = 'SWMT'                                                1189
         AG_EXNAME = 'POSITION'                                           1189
         AG_PARLIST = 'NONE'                                              1189
         CALL AXPOSITION                                                  1189
C    *                                                                    1189
C    Create SWMS :                                                        1190
C    Call To Local Procedure SWMS                                         1190
         ASSIGN 6470 TO BLOCK_SWMS                                        1190
         GO TO  6460                                                      1190
6470     CONTINUE                                                         1190
C    *                                                                    1190
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         1191
         CALL AGSRESET                                                    1191
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                1191
         AG_TITLE = 'SWMS'                                                1191
         AG_EXNAME = 'POSITION'                                           1191
         AG_PARLIST = 'Z'                                                 1191
         CALL AXPOSITION                                                  1191
C    *                                                                    1191
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          1191
         CALL AGSRESET                                                    1192
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 1192
         AG_TITLE = 'SWMS'                                                1192
         AG_EXNAME = 'POSITION'                                           1192
         AG_PARLIST = 'Z'                                                 1192
         CALL AXPOSITION                                                  1192
C    *                                                                    1192
C    Create SWMW :                                                        1193
C    Call To Local Procedure SWMW                                         1193
         ASSIGN 6490 TO BLOCK_SWMW                                        1193
         GO TO  6480                                                      1193
6490     CONTINUE                                                         1193
C    *                                                                    1193
C    Geant POSITION : SWMW ===> inherited arguments used <===             1194
         CALL AGSRESET                                                    1194
         AG_TITLE = 'SWMW'                                                1194
         AG_EXNAME = 'POSITION'                                           1194
         AG_PARLIST = 'NONE'                                              1194
         CALL AXPOSITION                                                  1194
C    *                                                                    1194
C    Create SOTB :                                                        1195
C    Call To Local Procedure SOTB                                         1195
         ASSIGN 6510 TO BLOCK_SOTB                                        1195
         GO TO  6500                                                      1195
6510     CONTINUE                                                         1195
C    *                                                                    1195
C    Geant POSITION : SOTB ===> inherited arguments used <===             1196
         CALL AGSRESET                                                    1196
         AG_TITLE = 'SOTB'                                                1196
         AG_EXNAME = 'POSITION'                                           1196
         AG_PARLIST = 'NONE'                                              1196
         CALL AXPOSITION                                                  1196
C    *                                                                    1196
C    Create SITB :                                                        1197
C    Call To Local Procedure SITB                                         1197
         ASSIGN 6530 TO BLOCK_SITB                                        1197
         GO TO  6520                                                      1197
6530     CONTINUE                                                         1197
C    *                                                                    1197
C    Geant POSITION : SITB ===> inherited arguments used <===             1198
         CALL AGSRESET                                                    1198
         AG_TITLE = 'SITB'                                                1198
         AG_EXNAME = 'POSITION'                                           1198
         AG_PARLIST = 'NONE'                                              1198
         CALL AXPOSITION                                                  1198
C    *                                                                    1198
5171  CALL AGSPOPB                                                        1199
      GO TO BLOCK_SWMM, (5170 ,5180)                                      1199
C End of Local Procedure                                                  1199
C ----------------------------------------------------------------------  1200
C ------------ SWMB                                                       1200
C Begin Local Procedure SWMB                                              1201
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       1201
6420  CONTINUE                                                            1201
         AG_TITLE='SWMB'                                                  1201
         AG_MODULE=MODULE                                                 1201
         AG_MARK='V00'                                                    1201
         CALL AGSPUSH                                                     1201
         AG_MODULE=MODULE                                                 1201
C    *                                                                    1201
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1202
         CALL AGSRESET                                                    1202
         AG_TITLE = 'ALUMINIUM'                                           1202
         AG_EXNAME = 'MATERIAL'                                           1202
         AG_PARLIST = 'NONE'                                              1202
         CALL AXMATERIAL                                                  1202
C    *                                                                    1202
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       1202
         CALL AGSRESET                                                    1203
         AG_SEEN = 1                                                      1203
         AG_COLO = 2                                                      1203
         AG_TITLE = 'SWMB'                                                1203
         AG_EXNAME = 'ATTRIBUTE'                                          1203
         AG_PARLIST = 'SEEN_COLO'                                         1203
         CALL AXATTRIBUTE                                                 1203
C    *                                                                    1203
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1203
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  1203
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          1203
         CALL AGSRESET                                                    1207
         AG_PHI1 = 0                                                      1207
         AG_DPHI = 360                                                    1207
         AG_NPDIV = 18                                                    1207
         AG_NZ = 2                                                        1207
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1207
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1207
         AG_RMN(1)=SWAM_RMIN                                              1207
         AG_RMN(2)=SWAM_RMIN                                              1207
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 1207
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 1207
         AG_TITLE = 'PGON'                                                1207
         AG_EXNAME = 'SHAPE'                                              1207
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1207
         CALL AXSHAPE                                                     1207
C    *                                                                    1207
6421  CALL AGSPOPB                                                        1208
      GO TO BLOCK_SWMB, (6420 ,6430)                                      1208
C End of Local Procedure                                                  1208
C ----------------------------------------------------------------------  1209
C ------------ SWMT                                                       1209
C Begin Local Procedure SWMT                                              1210
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            1210
6440  CONTINUE                                                            1210
         AG_TITLE='SWMT'                                                  1210
         AG_MODULE=MODULE                                                 1210
         AG_MARK='V00'                                                    1210
         CALL AGSPUSH                                                     1210
         AG_MODULE=MODULE                                                 1210
C    *                                                                    1210
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1211
         CALL AGSRESET                                                    1211
         AG_TITLE = 'ALUMINIUM'                                           1211
         AG_EXNAME = 'MATERIAL'                                           1211
         AG_PARLIST = 'NONE'                                              1211
         CALL AXMATERIAL                                                  1211
C    *                                                                    1211
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       1211
         CALL AGSRESET                                                    1212
         AG_SEEN = 1                                                      1212
         AG_COLO = 2                                                      1212
         AG_TITLE = 'SWMT'                                                1212
         AG_EXNAME = 'ATTRIBUTE'                                          1212
         AG_PARLIST = 'SEEN_COLO'                                         1212
         CALL AXATTRIBUTE                                                 1212
C    *                                                                    1212
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1212
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  1212
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          1212
         CALL AGSRESET                                                    1216
         AG_PHI1 = 0                                                      1216
         AG_DPHI = 360                                                    1216
         AG_NPDIV = 18                                                    1216
         AG_NZ = 2                                                        1216
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1216
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1216
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 1216
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 1216
         AG_RMX(1)=SWAM_RMAX                                              1216
         AG_RMX(2)=SWAM_RMAX                                              1216
         AG_TITLE = 'PGON'                                                1216
         AG_EXNAME = 'SHAPE'                                              1216
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1216
         CALL AXSHAPE                                                     1216
C    *                                                                    1216
6441  CALL AGSPOPB                                                        1217
      GO TO BLOCK_SWMT, (6440 ,6450)                                      1217
C End of Local Procedure                                                  1217
C ----------------------------------------------------------------------  1218
C ------------ SWMS                                                       1218
C Begin Local Procedure SWMS                                              1219
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  1219
6460  CONTINUE                                                            1219
         AG_TITLE='SWMS'                                                  1219
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
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       1220
         CALL AGSRESET                                                    1221
         AG_SEEN = 1                                                      1221
         AG_COLO = 2                                                      1221
         AG_TITLE = 'SWMS'                                                1221
         AG_EXNAME = 'ATTRIBUTE'                                          1221
         AG_PARLIST = 'SEEN_COLO'                                         1221
         CALL AXATTRIBUTE                                                 1221
C    *                                                                    1221
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  1221
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  1221
C    WAM_RMAX} <===                                                       1221
         CALL AGSRESET                                                    1224
         AG_PHI1 = 0                                                      1224
         AG_DPHI = 360                                                    1224
         AG_NPDIV = 18                                                    1224
         AG_NZ = 2                                                        1224
         AG_ZI(1)=-SWAM_WALLTHK/2                                         1224
         AG_ZI(2)=SWAM_WALLTHK/2                                          1224
         AG_RMN(1)=SWAM_RMIN                                              1224
         AG_RMN(2)=SWAM_RMIN                                              1224
         AG_RMX(1)=SWAM_RMAX                                              1224
         AG_RMX(2)=SWAM_RMAX                                              1224
         AG_TITLE = 'PGON'                                                1224
         AG_EXNAME = 'SHAPE'                                              1224
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1224
         CALL AXSHAPE                                                     1224
C    *                                                                    1224
6461  CALL AGSPOPB                                                        1225
      GO TO BLOCK_SWMS, (6460 ,6470)                                      1225
C End of Local Procedure                                                  1225
C ----------------------------------------------------------------------  1226
C ------------ SWMW                                                       1226
C Begin Local Procedure SWMW                                              1227
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 1227
6480  CONTINUE                                                            1227
         AG_TITLE='SWMW'                                                  1227
         AG_MODULE=MODULE                                                 1227
         AG_MARK='V00'                                                    1227
         CALL AGSPUSH                                                     1227
         AG_MODULE=MODULE                                                 1227
C    *                                                                    1227
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       1228
         CALL AGSRESET                                                    1228
         AG_SEEN = 1                                                      1228
         AG_COLO = 6                                                      1228
         AG_TITLE = 'SWMW'                                                1228
         AG_EXNAME = 'ATTRIBUTE'                                          1228
         AG_PARLIST = 'SEEN_COLO'                                         1228
         CALL AXATTRIBUTE                                                 1228
C    *                                                                    1228
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1228
         CALL AGSRESET                                                    1229
         AG_TITLE = 'WATER'                                               1229
         AG_EXNAME = 'MATERIAL'                                           1229
         AG_PARLIST = 'NONE'                                              1229
         CALL AXMATERIAL                                                  1229
C    *                                                                    1229
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1229
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  1229
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  1229
C    -SWAM_WALLTHK} <===                                                  1229
         CALL AGSRESET                                                    1234
         AG_PHI1 = 0                                                      1234
         AG_DPHI = 360                                                    1234
         AG_NPDIV = 18                                                    1234
         AG_NZ = 2                                                        1234
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1234
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1234
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 1234
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 1234
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 1234
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 1234
         AG_TITLE = 'PGON'                                                1234
         AG_EXNAME = 'SHAPE'                                              1234
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1234
         CALL AXSHAPE                                                     1234
C    *                                                                    1234
6481  CALL AGSPOPB                                                        1235
      GO TO BLOCK_SWMW, (6480 ,6490)                                      1235
C End of Local Procedure                                                  1235
C ----------------------------------------------------------------------  1236
C ------------ SOTB                                                       1236
C Begin Local Procedure SOTB                                              1237
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            1237
6500  CONTINUE                                                            1237
         AG_TITLE='SOTB'                                                  1237
         AG_MODULE=MODULE                                                 1237
         AG_MARK='V00'                                                    1237
         CALL AGSPUSH                                                     1237
         AG_MODULE=MODULE                                                 1237
C    *                                                                    1237
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1238
         CALL AGSRESET                                                    1238
         AG_TITLE = 'G10'                                                 1238
         AG_EXNAME = 'MATERIAL'                                           1238
         AG_PARLIST = 'NONE'                                              1238
         CALL AXMATERIAL                                                  1238
C    *                                                                    1238
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       1238
         CALL AGSRESET                                                    1239
         AG_SEEN = 1                                                      1239
         AG_COLO = 3                                                      1239
         AG_TITLE = 'SOTB'                                                1239
         AG_EXNAME = 'ATTRIBUTE'                                          1239
         AG_PARLIST = 'SEEN_COLO'                                         1239
         CALL AXATTRIBUTE                                                 1239
C    *                                                                    1239
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1239
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  1239
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    1239
         CALL AGSRESET                                                    1242
         AG_PHI1 = 0                                                      1242
         AG_DPHI = 360                                                    1242
         AG_NPDIV = 18                                                    1242
         AG_NZ = 2                                                        1242
         AG_ZI(1)=-SWAM_LEN/2                                             1242
         AG_ZI(2)=+SWAM_LEN/2                                             1242
         AG_RMN(1)=SWAM_RMAX                                              1242
         AG_RMN(2)=SWAM_RMAX                                              1242
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 1242
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 1242
         AG_TITLE = 'PGON'                                                1242
         AG_EXNAME = 'SHAPE'                                              1242
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1242
         CALL AXSHAPE                                                     1242
C    *                                                                    1242
6501  CALL AGSPOPB                                                        1243
      GO TO BLOCK_SOTB, (6500 ,6510)                                      1243
C End of Local Procedure                                                  1243
C ----------------------------------------------------------------------  1244
C ------------ SITB                                                       1244
C Begin Local Procedure SITB                                              1245
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            1245
6520  CONTINUE                                                            1245
         AG_TITLE='SITB'                                                  1245
         AG_MODULE=MODULE                                                 1245
         AG_MARK='V00'                                                    1245
         CALL AGSPUSH                                                     1245
         AG_MODULE=MODULE                                                 1245
C    *                                                                    1245
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1246
         CALL AGSRESET                                                    1246
         AG_A = 12                                                        1246
         AG_Z = 6                                                         1246
         AG_W = 1                                                         1246
         AG_TITLE = 'C'                                                   1246
         AG_EXNAME = 'COMPONENT'                                          1246
         AG_PARLIST = 'A_Z_W'                                             1246
         CALL AXCOMPONENT                                                 1246
C    *                                                                    1246
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1246
         CALL AGSRESET                                                    1247
         AG_A = 1                                                         1247
         AG_Z = 1                                                         1247
         AG_W = 2                                                         1247
         AG_TITLE = 'H2'                                                  1247
         AG_EXNAME = 'COMPONENT'                                          1247
         AG_PARLIST = 'A_Z_W'                                             1247
         CALL AXCOMPONENT                                                 1247
C    *                                                                    1247
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1247
         CALL AGSRESET                                                    1248
         AG_DENS = 0.935                                                  1248
         AG_TITLE = 'CH2'                                                 1248
         AG_EXNAME = 'MIXTURE'                                            1248
         AG_PARLIST = 'DENS'                                              1248
         CALL AXMIXTURE                                                   1248
C    *                                                                    1248
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       1248
         CALL AGSRESET                                                    1249
         AG_SEEN = 1                                                      1249
         AG_COLO = 3                                                      1249
         AG_TITLE = 'SITB'                                                1249
         AG_EXNAME = 'ATTRIBUTE'                                          1249
         AG_PARLIST = 'SEEN_COLO'                                         1249
         CALL AXATTRIBUTE                                                 1249
C    *                                                                    1249
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1249
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  1249
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   1249
         CALL AGSRESET                                                    1253
         AG_PHI1 = 0                                                      1253
         AG_DPHI = 360                                                    1253
         AG_NPDIV = 18                                                    1253
         AG_NZ = 2                                                        1253
         AG_ZI(1)=-SWAM_LEN/2                                             1253
         AG_ZI(2)=+SWAM_LEN/2                                             1253
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 1253
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 1253
         AG_RMX(1)=SWAM_RMIN                                              1253
         AG_RMX(2)=SWAM_RMIN                                              1253
         AG_TITLE = 'PGON'                                                1253
         AG_EXNAME = 'SHAPE'                                              1253
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1253
         CALL AXSHAPE                                                     1253
C    *                                                                    1253
6521  CALL AGSPOPB                                                        1254
      GO TO BLOCK_SITB, (6520 ,6530)                                      1254
C End of Local Procedure                                                  1254
C ----------------------------------------------------------------------  1255
C ------------ SBWC                                                       1255
C Begin Local Procedure SBWC                                              1256
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           1256
5190  CONTINUE                                                            1256
         AG_TITLE='SBWC'                                                  1256
         AG_MODULE=MODULE                                                 1256
         AG_MARK='V00'                                                    1256
         CALL AGSPUSH                                                     1256
         AG_MODULE=MODULE                                                 1256
C    *                                                                    1256
         RIN=SWAM_RMIN                                                    1257
         ROU=SSUP_CON1IDMN                                                1258
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1259
         CALL AGSRESET                                                    1259
         AG_TITLE = 'AIR'                                                 1259
         AG_EXNAME = 'MATERIAL'                                           1259
         AG_PARLIST = 'NONE'                                              1259
         CALL AXMATERIAL                                                  1259
C    *                                                                    1259
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       1259
         CALL AGSRESET                                                    1260
         AG_SEEN = 0                                                      1260
         AG_COLO = 1                                                      1260
         AG_TITLE = 'SBWC'                                                1260
         AG_EXNAME = 'ATTRIBUTE'                                          1260
         AG_PARLIST = 'SEEN_COLO'                                         1260
         CALL AXATTRIBUTE                                                 1260
C    *                                                                    1260
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  1260
C    MIN+SWAM_LEN))/2 <===                                                1260
         CALL AGSRESET                                                    1261
         AG_RMIN = RIN                                                    1261
         AG_RMAX = ROU                                                    1261
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   1261
         AG_TITLE = 'TUBE'                                                1261
         AG_EXNAME = 'SHAPE'                                              1261
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1261
         CALL AXSHAPE                                                     1261
C    *                                                                    1261
C    Create SWCM :                                                        1262
C    Call To Local Procedure SWCM                                         1262
         ASSIGN 6550 TO BLOCK_SWCM                                        1262
         GO TO  6540                                                      1262
6550     CONTINUE                                                         1262
C    *                                                                    1262
5191  CALL AGSPOPB                                                        1263
      GO TO BLOCK_SBWC, (5190 ,5200)                                      1263
C End of Local Procedure                                                  1263
C ----------------------------------------------------------------------  1264
C ------------ SWCM                                                       1264
C Begin Local Procedure SWCM                                              1265
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   1265
6540  CONTINUE                                                            1265
         AG_TITLE='SWCM'                                                  1265
         AG_MODULE=MODULE                                                 1265
         AG_MARK='V00'                                                    1265
         CALL AGSPUSH                                                     1265
         AG_MODULE=MODULE                                                 1265
C    *                                                                    1265
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       1266
         CALL AGSRESET                                                    1266
         AG_SEEN = 0                                                      1266
         AG_COLO = 1                                                      1266
         AG_TITLE = 'SWCM'                                                1266
         AG_EXNAME = 'ATTRIBUTE'                                          1266
         AG_PARLIST = 'SEEN_COLO'                                         1266
         CALL AXATTRIBUTE                                                 1266
C    *                                                                    1266
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 1266
         CALL AGSRESET                                                    1267
         AG_NDIV = 3                                                      1267
         AG_IAXIS = 2                                                     1267
         AG_C0 = 0                                                        1267
         AG_TITLE = 'DIVISION'                                            1267
         AG_EXNAME = 'SHAPE'                                              1267
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1267
         CALL AXSHAPE                                                     1267
C    *                                                                    1267
C    Create SXAI :                                                        1268
C    Call To Local Procedure SXAI                                         1268
         ASSIGN 6570 TO BLOCK_SXAI                                        1268
         GO TO  6560                                                      1268
6570     CONTINUE                                                         1268
C    *                                                                    1268
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  1269
C    /2+SSUP_BRATHK/2) <===                                               1269
         CALL AGSRESET                                                    1269
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   1269
         AG_TITLE = 'SXAI'                                                1269
         AG_EXNAME = 'POSITION'                                           1269
         AG_PARLIST = 'Z'                                                 1269
         CALL AXPOSITION                                                  1269
C    *                                                                    1269
C    Create SXBI :                                                        1270
C    Call To Local Procedure SXBI                                         1270
         ASSIGN 6590 TO BLOCK_SXBI                                        1270
         GO TO  6580                                                      1270
6590     CONTINUE                                                         1270
C    *                                                                    1270
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    1271
         CALL AGSRESET                                                    1271
         AG_Z = (SSUP_BRATHK/2)                                           1271
         AG_TITLE = 'SXBI'                                                1271
         AG_EXNAME = 'POSITION'                                           1271
         AG_PARLIST = 'Z'                                                 1271
         CALL AXPOSITION                                                  1271
C    *                                                                    1271
6541  CALL AGSPOPB                                                        1272
      GO TO BLOCK_SWCM, (6540 ,6550)                                      1272
C End of Local Procedure                                                  1272
C ----------------------------------------------------------------------  1273
C ------------ SXAI                                                       1273
C Begin Local Procedure SXAI                                              1274
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      1274
6560  CONTINUE                                                            1274
         AG_TITLE='SXAI'                                                  1274
         AG_MODULE=MODULE                                                 1274
         AG_MARK='V00'                                                    1274
         CALL AGSPUSH                                                     1274
         AG_MODULE=MODULE                                                 1274
C    *                                                                    1274
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       1275
         CALL AGSRESET                                                    1275
         AG_SEEN = 1                                                      1275
         AG_COLO = 2                                                      1275
         AG_TITLE = 'SXAI'                                                1275
         AG_EXNAME = 'ATTRIBUTE'                                          1275
         AG_PARLIST = 'SEEN_COLO'                                         1275
         CALL AXATTRIBUTE                                                 1275
C    *                                                                    1275
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1275
         CALL AGSRESET                                                    1276
         AG_TITLE = 'ALUMINIUM'                                           1276
         AG_EXNAME = 'MATERIAL'                                           1276
         AG_PARLIST = 'NONE'                                              1276
         CALL AXMATERIAL                                                  1276
C    *                                                                    1276
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   1276
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         1276
         CALL AGSRESET                                                    1278
         AG_RMIN = SWAM_RMIN                                              1278
         AG_RMAX = SSUP_CON1IDMN                                          1278
         AG_PHI1 = -5                                                     1278
         AG_PHI2 = 5                                                      1278
         AG_DZ = SSUP_BRATHK/2                                            1278
         AG_TITLE = 'TUBS'                                                1278
         AG_EXNAME = 'SHAPE'                                              1278
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1278
         CALL AXSHAPE                                                     1278
C    *                                                                    1278
6561  CALL AGSPOPB                                                        1279
      GO TO BLOCK_SXAI, (6560 ,6570)                                      1279
C End of Local Procedure                                                  1279
C ----------------------------------------------------------------------  1280
C ------------ SXBI                                                       1280
C Begin Local Procedure SXBI                                              1281
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     1281
6580  CONTINUE                                                            1281
         AG_TITLE='SXBI'                                                  1281
         AG_MODULE=MODULE                                                 1281
         AG_MARK='V00'                                                    1281
         CALL AGSPUSH                                                     1281
         AG_MODULE=MODULE                                                 1281
C    *                                                                    1281
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       1282
         CALL AGSRESET                                                    1282
         AG_SEEN = 1                                                      1282
         AG_COLO = 2                                                      1282
         AG_TITLE = 'SXBI'                                                1282
         AG_EXNAME = 'ATTRIBUTE'                                          1282
         AG_PARLIST = 'SEEN_COLO'                                         1282
         CALL AXATTRIBUTE                                                 1282
C    *                                                                    1282
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1282
         CALL AGSRESET                                                    1283
         AG_TITLE = 'ALUMINIUM'                                           1283
         AG_EXNAME = 'MATERIAL'                                           1283
         AG_PARLIST = 'NONE'                                              1283
         CALL AXMATERIAL                                                  1283
C    *                                                                    1283
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  1283
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  1283
C    _BRATHK)/2) <===                                                     1283
         CALL AGSRESET                                                    1285
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              1285
         AG_RMAX = SSUP_CON1IDMN                                          1285
         AG_PHI1 = -5                                                     1285
         AG_PHI2 = 5                                                      1285
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     1285
         AG_TITLE = 'TUBS'                                                1285
         AG_EXNAME = 'SHAPE'                                              1285
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1285
         CALL AXSHAPE                                                     1285
C    *                                                                    1285
6581  CALL AGSPOPB                                                        1286
      GO TO BLOCK_SXBI, (6580 ,6590)                                      1286
C End of Local Procedure                                                  1286
C ----------------------------------------------------------------------  1287
C ------------ SCRW                                                       1287
C Begin Local Procedure SCRW                                              1288
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   1288
6100  CONTINUE                                                            1288
         AG_TITLE='SCRW'                                                  1288
         AG_MODULE=MODULE                                                 1288
         AG_MARK='V00'                                                    1288
         CALL AGSPUSH                                                     1288
         AG_MODULE=MODULE                                                 1288
C    *                                                                    1288
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        1289
         CALL AGSRESET                                                    1289
         AG_TITLE = 'BERILLIUM'                                           1289
         AG_EXNAME = 'MATERIAL'                                           1289
         AG_PARLIST = 'NONE'                                              1289
         CALL AXMATERIAL                                                  1289
C    *                                                                    1289
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       1289
         CALL AGSRESET                                                    1290
         AG_SEEN = 1                                                      1290
         AG_COLO = 2                                                      1290
         AG_TITLE = 'SCRW'                                                1290
         AG_EXNAME = 'ATTRIBUTE'                                          1290
         AG_PARLIST = 'SEEN_COLO'                                         1290
         CALL AXATTRIBUTE                                                 1290
C    *                                                                    1290
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  1290
C    NDRNG_Z) <===                                                        1290
         CALL AGSRESET                                                    1291
         AG_RMIN = 0                                                      1291
         AG_RMAX = SSUP_ERJDIA/2                                          1291
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   1291
         AG_TITLE = 'TUBE'                                                1291
         AG_EXNAME = 'SHAPE'                                              1291
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1291
         CALL AXSHAPE                                                     1291
C    *                                                                    1291
6101  CALL AGSPOPB                                                        1292
      GO TO BLOCK_SCRW, (6100 ,6110,6140)                                 1292
C End of Local Procedure                                                  1292
C ----------------------------------------------------------------------  1293
C ------------ SBSP                                                       1293
C Begin Local Procedure SBSP                                              1294
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1294
5230  CONTINUE                                                            1294
         AG_TITLE='SBSP'                                                  1294
         AG_MODULE=MODULE                                                 1294
         AG_MARK='V00'                                                    1294
         CALL AGSPUSH                                                     1294
         AG_MODULE=MODULE                                                 1294
C    *                                                                    1294
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1295
         CALL AGSRESET                                                    1295
         AG_TITLE = 'AIR'                                                 1295
         AG_EXNAME = 'MATERIAL'                                           1295
         AG_PARLIST = 'NONE'                                              1295
         CALL AXMATERIAL                                                  1295
C    *                                                                    1295
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1295
         CALL AGSRESET                                                    1296
         AG_SEEN = 0                                                      1296
         AG_COLO = 1                                                      1296
         AG_TITLE = 'SBSP'                                                1296
         AG_EXNAME = 'ATTRIBUTE'                                          1296
         AG_PARLIST = 'SEEN_COLO'                                         1296
         CALL AXATTRIBUTE                                                 1296
C    *                                                                    1296
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1296
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1296
         CALL AGSRESET                                                    1298
         AG_RMIN = SVTG_RSIZEMIN                                          1298
         AG_RMAX = SSUB_KMOUNTOD/2                                        1298
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1298
         AG_TITLE = 'TUBE'                                                1298
         AG_EXNAME = 'SHAPE'                                              1298
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1298
         CALL AXSHAPE                                                     1298
C    *                                                                    1298
C    Create SAKM :                                                        1299
C    Call To Local Procedure SAKM                                         1299
         ASSIGN 6610 TO BLOCK_SAKM                                        1299
         GO TO  6600                                                      1299
6610     CONTINUE                                                         1299
C    *                                                                    1299
C    Geant POSITION : SAKM ===> inherited arguments used <===             1300
         CALL AGSRESET                                                    1300
         AG_TITLE = 'SAKM'                                                1300
         AG_EXNAME = 'POSITION'                                           1300
         AG_PARLIST = 'NONE'                                              1300
         CALL AXPOSITION                                                  1300
C    *                                                                    1300
C    Create SBMM :                                                        1301
C    Call To Local Procedure SBMM                                         1301
         ASSIGN 6630 TO BLOCK_SBMM                                        1301
         GO TO  6620                                                      1301
6630     CONTINUE                                                         1301
C    *                                                                    1301
C    Create SBMM :                                                        1302
C    Call To Local Procedure SBMM                                         1302
         ASSIGN 6640 TO BLOCK_SBMM                                        1302
         GO TO  6620                                                      1302
6640     CONTINUE                                                         1302
C    *                                                                    1302
C    Create SBRL :                                                        1303
C    Call To Local Procedure SBRL                                         1303
         ASSIGN 6660 TO BLOCK_SBRL                                        1303
         GO TO  6650                                                      1303
6660     CONTINUE                                                         1303
C    *                                                                    1303
C    Create SBRX :                                                        1304
C    Call To Local Procedure SBRX                                         1304
         ASSIGN 6680 TO BLOCK_SBRX                                        1304
         GO TO  6670                                                      1304
6680     CONTINUE                                                         1304
C    *                                                                    1304
C    *                                                                    1305
         I=-1                                                             1305
            GOTO 6693                                                     1305
6691        I=I+(2)                                                       1305
6693        IF((2)*(I-(1)).GT.0) GO TO 6692                               1305
C       *                                                                 1306
            J=0                                                           1306
               GO TO 6703                                                 1306
6701           J=J+1                                                      1306
6703           IF(J-(1).GT.0) GO TO 6702                                  1306
               PHI=I*SSUB_SRCUTPHI+180*J                                  1307
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1308
     *         2.0)                                                       1308
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1310
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1310
C          ==                                                             1310
               CALL AGSRESET                                              1310
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1310
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1310
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1310
               AG_ALPHAN=AG_ALPHAN+1                                      1310
               AG_ALPHAK(AG_ALPHAN)=3                                     1310
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1310
               AG_TITLE = 'SBMM'                                          1310
               AG_EXNAME = 'POSITION'                                     1310
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1310
               CALL AXPOSITION                                            1310
C          *                                                              1310
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1310
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1313
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1313
C           ORT=ZXY <===                                                  1313
               CALL AGSRESET                                              1313
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1313
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1313
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1313
               AG_ALPHAN=AG_ALPHAN+1                                      1313
               AG_ALPHAK(AG_ALPHAN)=3                                     1313
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1313
               AG_ALPHAN=AG_ALPHAN+1                                      1313
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1313
               AG_TITLE = 'SBRL'                                          1313
               AG_EXNAME = 'POSITION'                                     1313
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1313
               CALL AXPOSITION                                            1313
C          *                                                              1313
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1313
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1313
C           ORT=ZXY <===                                                  1313
               CALL AGSRESET                                              1315
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1315
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1315
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1315
               AG_ALPHAN=AG_ALPHAN+1                                      1315
               AG_ALPHAK(AG_ALPHAN)=3                                     1315
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1315
               AG_ALPHAN=AG_ALPHAN+1                                      1315
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1315
               AG_TITLE = 'SBRX'                                          1315
               AG_EXNAME = 'POSITION'                                     1315
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1315
               CALL AXPOSITION                                            1315
C          *                                                              1315
            GO TO 6701                                                    1316
6702        CONTINUE                                                      1316
         GO TO 6691                                                       1317
6692     CONTINUE                                                         1317
C    Create SBSR :                                                        1318
C    Call To Local Procedure SBSR                                         1318
         ASSIGN 6720 TO BLOCK_SBSR                                        1318
         GO TO  6710                                                      1318
6720     CONTINUE                                                         1318
C    *                                                                    1318
C    Geant POSITION : SBSR ===> inherited arguments used <===             1319
         CALL AGSRESET                                                    1319
         AG_TITLE = 'SBSR'                                                1319
         AG_EXNAME = 'POSITION'                                           1319
         AG_PARLIST = 'NONE'                                              1319
         CALL AXPOSITION                                                  1319
C    *                                                                    1319
5231  CALL AGSPOPB                                                        1320
      GO TO BLOCK_SBSP, (5230 ,5240)                                      1320
C End of Local Procedure                                                  1320
C ----------------------------------------------------------------------  1321
C ------------ SAKM                                                       1321
C Begin Local Procedure SAKM                                              1322
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1322
6600  CONTINUE                                                            1322
         AG_TITLE='SAKM'                                                  1322
         AG_MODULE=MODULE                                                 1322
         AG_MARK='V00'                                                    1322
         CALL AGSPUSH                                                     1322
         AG_MODULE=MODULE                                                 1322
C    *                                                                    1322
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1323
         CALL AGSRESET                                                    1323
         AG_TITLE = 'ALUMINIUM'                                           1323
         AG_EXNAME = 'MATERIAL'                                           1323
         AG_PARLIST = 'NONE'                                              1323
         CALL AXMATERIAL                                                  1323
C    *                                                                    1323
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1323
         CALL AGSRESET                                                    1324
         AG_SEEN = 1                                                      1324
         AG_COLO = 2                                                      1324
         AG_TITLE = 'SAKM'                                                1324
         AG_EXNAME = 'ATTRIBUTE'                                          1324
         AG_PARLIST = 'SEEN_COLO'                                         1324
         CALL AXATTRIBUTE                                                 1324
C    *                                                                    1324
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1324
C    DZ=SSUB_KMNTTHK/2 <===                                               1324
         CALL AGSRESET                                                    1326
         AG_RMIN = SSUB_KMOUNTID/2                                        1326
         AG_RMAX = SSUB_KMOUNTOD/2                                        1326
         AG_DZ = SSUB_KMNTTHK/2                                           1326
         AG_TITLE = 'TUBE'                                                1326
         AG_EXNAME = 'SHAPE'                                              1326
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1326
         CALL AXSHAPE                                                     1326
C    *                                                                    1326
C    Create SCKM :                                                        1327
C    Call To Local Procedure SCKM                                         1327
         ASSIGN 6740 TO BLOCK_SCKM                                        1327
         GO TO  6730                                                      1327
6740     CONTINUE                                                         1327
C    *                                                                    1327
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1328
         CALL AGSRESET                                                    1328
         AG_Y = SSUB_KMCUTOFF                                             1328
         AG_KONLY = 'MANY'                                                1328
         AG_TITLE = 'SCKM'                                                1328
         AG_EXNAME = 'POSITION'                                           1328
         AG_PARLIST = 'Y_KONLY'                                           1328
         CALL AXPOSITION                                                  1328
C    *                                                                    1328
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1328
C     <===                                                                1328
         CALL AGSRESET                                                    1329
         AG_Y = -SSUB_KMCUTOFF                                            1329
         AG_ALPHAN=AG_ALPHAN+1                                            1329
         AG_ALPHAK(AG_ALPHAN)=3                                           1329
         AG_ALPHAL(AG_ALPHAN)=180                                         1329
         AG_KONLY = 'MANY'                                                1329
         AG_TITLE = 'SCKM'                                                1329
         AG_EXNAME = 'POSITION'                                           1329
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1329
         CALL AXPOSITION                                                  1329
C    *                                                                    1329
6601  CALL AGSPOPB                                                        1330
      GO TO BLOCK_SAKM, (6600 ,6610)                                      1330
C End of Local Procedure                                                  1330
C ----------------------------------------------------------------------  1331
C ------------ SCKM                                                       1331
C Begin Local Procedure SCKM                                              1332
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1332
6730  CONTINUE                                                            1332
         AG_TITLE='SCKM'                                                  1332
         AG_MODULE=MODULE                                                 1332
         AG_MARK='V00'                                                    1332
         CALL AGSPUSH                                                     1332
         AG_MODULE=MODULE                                                 1332
C    *                                                                    1332
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1333
         CALL AGSRESET                                                    1333
         AG_TITLE = 'AIR'                                                 1333
         AG_EXNAME = 'MATERIAL'                                           1333
         AG_PARLIST = 'NONE'                                              1333
         CALL AXMATERIAL                                                  1333
C    *                                                                    1333
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1333
         CALL AGSRESET                                                    1334
         AG_SEEN = 1                                                      1334
         AG_COLO = 1                                                      1334
         AG_TITLE = 'SCKM'                                                1334
         AG_EXNAME = 'ATTRIBUTE'                                          1334
         AG_PARLIST = 'SEEN_COLO'                                         1334
         CALL AXATTRIBUTE                                                 1334
C    *                                                                    1334
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1334
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1334
         CALL AGSRESET                                                    1337
         AG_RMIN = SSUB_KMCUTID/2                                         1337
         AG_RMAX = SSUB_KMCUTOD/2                                         1337
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1337
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1337
         AG_DZ = SSUB_KMNTTHK/2                                           1337
         AG_TITLE = 'TUBS'                                                1337
         AG_EXNAME = 'SHAPE'                                              1337
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1337
         CALL AXSHAPE                                                     1337
C    *                                                                    1337
6731  CALL AGSPOPB                                                        1338
      GO TO BLOCK_SCKM, (6730 ,6740)                                      1338
C End of Local Procedure                                                  1338
C ----------------------------------------------------------------------  1339
C ------------ SBSR                                                       1339
C Begin Local Procedure SBSR                                              1340
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1340
6710  CONTINUE                                                            1340
         AG_TITLE='SBSR'                                                  1340
         AG_MODULE=MODULE                                                 1340
         AG_MARK='V00'                                                    1340
         CALL AGSPUSH                                                     1340
         AG_MODULE=MODULE                                                 1340
C    *                                                                    1340
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1341
         CALL AGSRESET                                                    1341
         AG_TITLE = 'G10'                                                 1341
         AG_EXNAME = 'MATERIAL'                                           1341
         AG_PARLIST = 'NONE'                                              1341
         CALL AXMATERIAL                                                  1341
C    *                                                                    1341
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1341
         CALL AGSRESET                                                    1342
         AG_SEEN = 1                                                      1342
         AG_COLO = 3                                                      1342
         AG_TITLE = 'SBSR'                                                1342
         AG_EXNAME = 'ATTRIBUTE'                                          1342
         AG_PARLIST = 'SEEN_COLO'                                         1342
         CALL AXATTRIBUTE                                                 1342
C    *                                                                    1342
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1342
C    =SSUB_SRINGTHK/2 <===                                                1342
         CALL AGSRESET                                                    1344
         AG_RMIN = SSUB_SRINGID/2                                         1344
         AG_RMAX = SSUB_SRINGOD/2                                         1344
         AG_DZ = SSUB_SRINGTHK/2                                          1344
         AG_TITLE = 'TUBE'                                                1344
         AG_EXNAME = 'SHAPE'                                              1344
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1344
         CALL AXSHAPE                                                     1344
C    *                                                                    1344
C    Create SBCR :                                                        1345
C    Call To Local Procedure SBCR                                         1345
         ASSIGN 6760 TO BLOCK_SBCR                                        1345
         GO TO  6750                                                      1345
6760     CONTINUE                                                         1345
C    *                                                                    1345
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1346
C    *                                                                    1347
         I=-1                                                             1347
            GOTO 6773                                                     1347
6771        I=I+(2)                                                       1347
6773        IF((2)*(I-(1)).GT.0) GO TO 6772                               1347
C       *                                                                 1348
            J=0                                                           1348
               GO TO 6783                                                 1348
6781           J=J+1                                                      1348
6783           IF(J-(1).GT.0) GO TO 6782                                  1348
               PHI=I*SSUB_SRCUTPHI+180*J                                  1349
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1351
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1351
               CALL AGSRESET                                              1351
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1351
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1351
               AG_ALPHAN=AG_ALPHAN+1                                      1351
               AG_ALPHAK(AG_ALPHAN)=3                                     1351
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1351
               AG_KONLY = 'MANY'                                          1351
               AG_TITLE = 'SBCR'                                          1351
               AG_EXNAME = 'POSITION'                                     1351
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1351
               CALL AXPOSITION                                            1351
C          *                                                              1351
            GO TO 6781                                                    1352
6782        CONTINUE                                                      1352
         GO TO 6771                                                       1353
6772     CONTINUE                                                         1353
6711  CALL AGSPOPB                                                        1354
      GO TO BLOCK_SBSR, (6710 ,6720)                                      1354
C End of Local Procedure                                                  1354
C ----------------------------------------------------------------------  1355
C ------------ SBCR                                                       1355
C Begin Local Procedure SBCR                                              1356
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1356
6750  CONTINUE                                                            1356
         AG_TITLE='SBCR'                                                  1356
         AG_MODULE=MODULE                                                 1356
         AG_MARK='V00'                                                    1356
         CALL AGSPUSH                                                     1356
         AG_MODULE=MODULE                                                 1356
C    *                                                                    1356
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1357
         CALL AGSRESET                                                    1357
         AG_TITLE = 'AIR'                                                 1357
         AG_EXNAME = 'MATERIAL'                                           1357
         AG_PARLIST = 'NONE'                                              1357
         CALL AXMATERIAL                                                  1357
C    *                                                                    1357
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1357
         CALL AGSRESET                                                    1358
         AG_SEEN = 1                                                      1358
         AG_COLO = 1                                                      1358
         AG_TITLE = 'SBCR'                                                1358
         AG_EXNAME = 'ATTRIBUTE'                                          1358
         AG_PARLIST = 'SEEN_COLO'                                         1358
         CALL AXATTRIBUTE                                                 1358
C    *                                                                    1358
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1358
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1358
         CALL AGSRESET                                                    1360
         AG_DX = SSUB_SRCUTWID/2                                          1360
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1360
         AG_DZ = SSUB_SRINGTHK/2                                          1360
         AG_TITLE = 'BOX'                                                 1360
         AG_EXNAME = 'SHAPE'                                              1360
         AG_PARLIST = 'DX_DY_DZ'                                          1360
         CALL AXSHAPE                                                     1360
C    *                                                                    1360
6751  CALL AGSPOPB                                                        1361
      GO TO BLOCK_SBCR, (6750 ,6760)                                      1361
C End of Local Procedure                                                  1361
C ----------------------------------------------------------------------  1362
C ------------ SBRL                                                       1362
C Begin Local Procedure SBRL                                              1363
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1363
6650  CONTINUE                                                            1363
         AG_TITLE='SBRL'                                                  1363
         AG_MODULE=MODULE                                                 1363
         AG_MARK='V00'                                                    1363
         CALL AGSPUSH                                                     1363
         AG_MODULE=MODULE                                                 1363
C    *                                                                    1363
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1364
C    ADL=12.6 <===                                                        1364
         CALL AGSRESET                                                    1364
         AG_A = 20.719                                                    1364
         AG_Z = 10.307                                                    1364
         AG_DENS = 2.23                                                   1364
         AG_ABSL = 50.7                                                   1364
         AG_RADL = 12.6                                                   1364
         AG_TITLE = 'PYREX'                                               1364
         AG_EXNAME = 'MATERIAL'                                           1364
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1364
         CALL AXMATERIAL                                                  1364
C    *                                                                    1364
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1364
         CALL AGSRESET                                                    1365
         AG_SEEN = 1                                                      1365
         AG_COLO = 6                                                      1365
         AG_TITLE = 'SBRL'                                                1365
         AG_EXNAME = 'ATTRIBUTE'                                          1365
         AG_PARLIST = 'SEEN_COLO'                                         1365
         CALL AXATTRIBUTE                                                 1365
C    *                                                                    1365
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1365
C    =                                                                    1365
         CALL AGSRESET                                                    1366
         AG_RMIN = SSUB_SROLLID/2                                         1366
         AG_RMAX = SSUB_SROLLOD/2                                         1366
         AG_TITLE = 'TUBE'                                                1366
         AG_EXNAME = 'SHAPE'                                              1366
         AG_PARLIST = 'RMIN_RMAX'                                         1366
         CALL AXSHAPE                                                     1366
C    *                                                                    1366
6651  CALL AGSPOPB                                                        1367
      GO TO BLOCK_SBRL, (6650 ,6660)                                      1367
C End of Local Procedure                                                  1367
C ----------------------------------------------------------------------  1368
C ------------ SBRX                                                       1368
C Begin Local Procedure SBRX                                              1369
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1369
6670  CONTINUE                                                            1369
         AG_TITLE='SBRX'                                                  1369
         AG_MODULE=MODULE                                                 1369
         AG_MARK='V00'                                                    1369
         CALL AGSPUSH                                                     1369
         AG_MODULE=MODULE                                                 1369
C    *                                                                    1369
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1370
         CALL AGSRESET                                                    1370
         AG_TITLE = 'IRON'                                                1370
         AG_EXNAME = 'MATERIAL'                                           1370
         AG_PARLIST = 'NONE'                                              1370
         CALL AXMATERIAL                                                  1370
C    *                                                                    1370
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1370
         CALL AGSRESET                                                    1371
         AG_SEEN = 1                                                      1371
         AG_COLO = 2                                                      1371
         AG_TITLE = 'SBRX'                                                1371
         AG_EXNAME = 'ATTRIBUTE'                                          1371
         AG_PARLIST = 'SEEN_COLO'                                         1371
         CALL AXATTRIBUTE                                                 1371
C    *                                                                    1371
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1371
C    LEN/2 <===                                                           1371
         CALL AGSRESET                                                    1373
         AG_RMIN = 0.0                                                    1373
         AG_RMAX = SSUB_SROLLID/2                                         1373
         AG_DZ = SSUB_SWIRELEN/2                                          1373
         AG_TITLE = 'TUBE'                                                1373
         AG_EXNAME = 'SHAPE'                                              1373
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1373
         CALL AXSHAPE                                                     1373
C    *                                                                    1373
6671  CALL AGSPOPB                                                        1374
      GO TO BLOCK_SBRX, (6670 ,6680)                                      1374
C End of Local Procedure                                                  1374
C ----------------------------------------------------------------------  1375
C ------------ SBMM                                                       1375
C Begin Local Procedure SBMM                                              1376
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1376
6620  CONTINUE                                                            1376
         AG_TITLE='SBMM'                                                  1376
         AG_MODULE=MODULE                                                 1376
         AG_MARK='V00'                                                    1376
         CALL AGSPUSH                                                     1376
         AG_MODULE=MODULE                                                 1376
C    *                                                                    1376
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1377
         CALL AGSRESET                                                    1377
         AG_TITLE = 'AIR'                                                 1377
         AG_EXNAME = 'MATERIAL'                                           1377
         AG_PARLIST = 'NONE'                                              1377
         CALL AXMATERIAL                                                  1377
C    *                                                                    1377
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1377
         CALL AGSRESET                                                    1378
         AG_SEEN = 0                                                      1378
         AG_COLO = 1                                                      1378
         AG_TITLE = 'SBMM'                                                1378
         AG_EXNAME = 'ATTRIBUTE'                                          1378
         AG_PARLIST = 'SEEN_COLO'                                         1378
         CALL AXATTRIBUTE                                                 1378
C    *                                                                    1378
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1378
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1378
         CALL AGSRESET                                                    1380
         AG_DX = SSUB_MBLKIWID/2                                          1380
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1380
         AG_DZ = SSUB_MBLKHGH/2                                           1380
         AG_TITLE = 'BOX'                                                 1380
         AG_EXNAME = 'SHAPE'                                              1380
         AG_PARLIST = 'DX_DY_DZ'                                          1380
         CALL AXSHAPE                                                     1380
C    *                                                                    1380
C    Create SMRD :                                                        1381
C    Call To Local Procedure SMRD                                         1381
         ASSIGN 6800 TO BLOCK_SMRD                                        1381
         GO TO  6790                                                      1381
6800     CONTINUE                                                         1381
C    *                                                                    1381
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1382
     *   SSUB_SROLLOD                                                     1382
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1383
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1383
         CALL AGSRESET                                                    1383
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1383
         AG_ALPHAN=AG_ALPHAN+1                                            1383
         AG_ALPHAK(AG_ALPHAN)=1                                           1383
         AG_ALPHAL(AG_ALPHAN)=90                                          1383
         AG_TITLE = 'SMRD'                                                1383
         AG_EXNAME = 'POSITION'                                           1383
         AG_PARLIST = 'Y_ALPHAX'                                          1383
         CALL AXPOSITION                                                  1383
C    *                                                                    1383
C    Create SBMO :                                                        1384
C    Call To Local Procedure SBMO                                         1384
         ASSIGN 6820 TO BLOCK_SBMO                                        1384
         GO TO  6810                                                      1384
6820     CONTINUE                                                         1384
C    *                                                                    1384
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1385
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1386
C    UB_SROLLOD)/2 <===                                                   1386
         CALL AGSRESET                                                    1386
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1386
         AG_TITLE = 'SBMO'                                                1386
         AG_EXNAME = 'POSITION'                                           1386
         AG_PARLIST = 'Y'                                                 1386
         CALL AXPOSITION                                                  1386
C    *                                                                    1386
C    Create SBMI :                                                        1387
C    Call To Local Procedure SBMI                                         1387
         ASSIGN 6840 TO BLOCK_SBMI                                        1387
         GO TO  6830                                                      1387
6840     CONTINUE                                                         1387
C    *                                                                    1387
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1388
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1389
C    UB_SROLLOD)/2 <===                                                   1389
         CALL AGSRESET                                                    1389
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1389
         AG_TITLE = 'SBMI'                                                1389
         AG_EXNAME = 'POSITION'                                           1389
         AG_PARLIST = 'Y'                                                 1389
         CALL AXPOSITION                                                  1389
C    *                                                                    1389
6621  CALL AGSPOPB                                                        1390
      GO TO BLOCK_SBMM, (6620 ,6630,6640)                                 1390
C End of Local Procedure                                                  1390
C ----------------------------------------------------------------------  1391
C ------------ SMRD                                                       1391
C Begin Local Procedure SMRD                                              1392
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1392
6790  CONTINUE                                                            1392
         AG_TITLE='SMRD'                                                  1392
         AG_MODULE=MODULE                                                 1392
         AG_MARK='V00'                                                    1392
         CALL AGSPUSH                                                     1392
         AG_MODULE=MODULE                                                 1392
C    *                                                                    1392
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1393
         CALL AGSRESET                                                    1393
         AG_TITLE = 'ALUMINIUM'                                           1393
         AG_EXNAME = 'MATERIAL'                                           1393
         AG_PARLIST = 'NONE'                                              1393
         CALL AXMATERIAL                                                  1393
C    *                                                                    1393
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1393
         CALL AGSRESET                                                    1394
         AG_SEEN = 1                                                      1394
         AG_COLO = 2                                                      1394
         AG_TITLE = 'SMRD'                                                1394
         AG_EXNAME = 'ATTRIBUTE'                                          1394
         AG_PARLIST = 'SEEN_COLO'                                         1394
         CALL AXATTRIBUTE                                                 1394
C    *                                                                    1394
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1394
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1394
         CALL AGSRESET                                                    1396
         AG_RMIN = 0.0                                                    1396
         AG_RMAX = SSUB_MRODDIA/2                                         1396
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1396
         AG_TITLE = 'TUBE'                                                1396
         AG_EXNAME = 'SHAPE'                                              1396
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1396
         CALL AXSHAPE                                                     1396
C    *                                                                    1396
6791  CALL AGSPOPB                                                        1397
      GO TO BLOCK_SMRD, (6790 ,6800)                                      1397
C End of Local Procedure                                                  1397
C ----------------------------------------------------------------------  1398
C ------------ SBMO                                                       1398
C Begin Local Procedure SBMO                                              1399
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1399
6810  CONTINUE                                                            1399
         AG_TITLE='SBMO'                                                  1399
         AG_MODULE=MODULE                                                 1399
         AG_MARK='V00'                                                    1399
         CALL AGSPUSH                                                     1399
         AG_MODULE=MODULE                                                 1399
C    *                                                                    1399
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1400
         CALL AGSRESET                                                    1400
         AG_TITLE = 'G10'                                                 1400
         AG_EXNAME = 'MATERIAL'                                           1400
         AG_PARLIST = 'NONE'                                              1400
         CALL AXMATERIAL                                                  1400
C    *                                                                    1400
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1400
         CALL AGSRESET                                                    1401
         AG_SEEN = 1                                                      1401
         AG_COLO = 3                                                      1401
         AG_TITLE = 'SBMO'                                                1401
         AG_EXNAME = 'ATTRIBUTE'                                          1401
         AG_PARLIST = 'SEEN_COLO'                                         1401
         CALL AXATTRIBUTE                                                 1401
C    *                                                                    1401
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1401
C    UB_MBLKHGH/2 <===                                                    1401
         CALL AGSRESET                                                    1403
         AG_DX = SSUB_MBLKOWID/2                                          1403
         AG_DY = SSUB_MBLKOLEN/2                                          1403
         AG_DZ = SSUB_MBLKHGH/2                                           1403
         AG_TITLE = 'BOX'                                                 1403
         AG_EXNAME = 'SHAPE'                                              1403
         AG_PARLIST = 'DX_DY_DZ'                                          1403
         CALL AXSHAPE                                                     1403
C    *                                                                    1403
6811  CALL AGSPOPB                                                        1404
      GO TO BLOCK_SBMO, (6810 ,6820)                                      1404
C End of Local Procedure                                                  1404
C ----------------------------------------------------------------------  1405
C ------------ SBMI                                                       1405
C Begin Local Procedure SBMI                                              1406
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1406
6830  CONTINUE                                                            1406
         AG_TITLE='SBMI'                                                  1406
         AG_MODULE=MODULE                                                 1406
         AG_MARK='V00'                                                    1406
         CALL AGSPUSH                                                     1406
         AG_MODULE=MODULE                                                 1406
C    *                                                                    1406
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1407
         CALL AGSRESET                                                    1407
         AG_TITLE = 'G10'                                                 1407
         AG_EXNAME = 'MATERIAL'                                           1407
         AG_PARLIST = 'NONE'                                              1407
         CALL AXMATERIAL                                                  1407
C    *                                                                    1407
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1407
         CALL AGSRESET                                                    1408
         AG_SEEN = 1                                                      1408
         AG_COLO = 3                                                      1408
         AG_TITLE = 'SBMO'                                                1408
         AG_EXNAME = 'ATTRIBUTE'                                          1408
         AG_PARLIST = 'SEEN_COLO'                                         1408
         CALL AXATTRIBUTE                                                 1408
C    *                                                                    1408
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1408
C    UB_MBLKHGH/2 <===                                                    1408
         CALL AGSRESET                                                    1410
         AG_DX = SSUB_MBLKIWID/2                                          1410
         AG_DY = SSUB_MBLKILEN/2                                          1410
         AG_DZ = SSUB_MBLKHGH/2                                           1410
         AG_TITLE = 'BOX'                                                 1410
         AG_EXNAME = 'SHAPE'                                              1410
         AG_PARLIST = 'DX_DY_DZ'                                          1410
         CALL AXSHAPE                                                     1410
C    *                                                                    1410
6831  CALL AGSPOPB                                                        1411
      GO TO BLOCK_SBMI, (6830 ,6840)                                      1411
C End of Local Procedure                                                  1411
C ----------------------------------------------------------------------  1412
C ------------ SALM                                                       1412
C Begin Local Procedure SALM                                              1413
C SALM IS THE ALUMINUM SHIELD MESH                                        1413
5370  CONTINUE                                                            1413
         AG_TITLE='SALM'                                                  1413
         AG_MODULE=MODULE                                                 1413
         AG_MARK='V00'                                                    1413
         CALL AGSPUSH                                                     1413
         AG_MODULE=MODULE                                                 1413
C    *                                                                    1413
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1414
         CALL AGSRESET                                                    1414
         AG_TITLE = 'ALUMINIUM'                                           1414
         AG_EXNAME = 'MATERIAL'                                           1414
         AG_PARLIST = 'NONE'                                              1414
         CALL AXMATERIAL                                                  1414
C    *                                                                    1414
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1414
         CALL AGSRESET                                                    1415
         AG_SEEN = 1                                                      1415
         AG_COLO = 2                                                      1415
         AG_TITLE = 'SALM'                                                1415
         AG_EXNAME = 'ATTRIBUTE'                                          1415
         AG_PARLIST = 'SEEN_COLO'                                         1415
         CALL AXATTRIBUTE                                                 1415
C    *                                                                    1415
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1415
C    DZ=SSLD_ALMSHTHK/2 <===                                              1415
         CALL AGSRESET                                                    1417
         AG_RMIN = SSLD_ALMESHID/2                                        1417
         AG_RMAX = SSLD_ALMESHOD/2                                        1417
         AG_DZ = SSLD_ALMSHTHK/2                                          1417
         AG_TITLE = 'TUBE'                                                1417
         AG_EXNAME = 'SHAPE'                                              1417
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1417
         CALL AXSHAPE                                                     1417
C    *                                                                    1417
5371  CALL AGSPOPB                                                        1418
      GO TO BLOCK_SALM, (5370 ,5380)                                      1418
C End of Local Procedure                                                  1418
C ----------------------------------------------------------------------  1419
C ------------ SISH                                                       1419
C Begin Local Procedure SISH                                              1420
C SISH IS THE INNER SHIELD CYLINDER                                       1420
5310  CONTINUE                                                            1420
         AG_TITLE='SISH'                                                  1420
         AG_MODULE=MODULE                                                 1420
         AG_MARK='V00'                                                    1420
         CALL AGSPUSH                                                     1420
         AG_MODULE=MODULE                                                 1420
C    *                                                                    1420
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1421
         CALL AGSRESET                                                    1421
         AG_TITLE = 'ALKAP'                                               1421
         AG_EXNAME = 'MATERIAL'                                           1421
         AG_PARLIST = 'NONE'                                              1421
         CALL AXMATERIAL                                                  1421
C    *                                                                    1421
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1421
         CALL AGSRESET                                                    1422
         AG_SEEN = 1                                                      1422
         AG_COLO = 3                                                      1422
         AG_TITLE = 'SISH'                                                1422
         AG_EXNAME = 'ATTRIBUTE'                                          1422
         AG_PARLIST = 'SEEN_COLO'                                         1422
         CALL AXATTRIBUTE                                                 1422
C    *                                                                    1422
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1422
C    D_SINLEN <===                                                        1422
         CALL AGSRESET                                                    1424
         AG_RMIN = SSLD_SINRINN                                           1424
         AG_RMAX = SSLD_SINROUT                                           1424
         AG_DZ = SSLD_SINLEN                                              1424
         AG_TITLE = 'TUBE'                                                1424
         AG_EXNAME = 'SHAPE'                                              1424
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1424
         CALL AXSHAPE                                                     1424
C    *                                                                    1424
5311  CALL AGSPOPB                                                        1425
      GO TO BLOCK_SISH, (5310 ,5320)                                      1425
C End of Local Procedure                                                  1425
C ----------------------------------------------------------------------  1426
C ------------ SSSH                                                       1426
C Begin Local Procedure SSSH                                              1427
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1427
5350  CONTINUE                                                            1427
         AG_TITLE='SSSH'                                                  1427
         AG_MODULE=MODULE                                                 1427
         AG_MARK='V00'                                                    1427
         CALL AGSPUSH                                                     1427
         AG_MODULE=MODULE                                                 1427
C    *                                                                    1427
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1428
         CALL AGSRESET                                                    1428
         AG_TITLE = 'SSDALMY'                                             1428
         AG_EXNAME = 'MATERIAL'                                           1428
         AG_PARLIST = 'NONE'                                              1428
         CALL AXMATERIAL                                                  1428
C    *                                                                    1428
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1428
         CALL AGSRESET                                                    1429
         AG_SEEN = 1                                                      1429
         AG_COLO = 3                                                      1429
         AG_TITLE = 'SSSH'                                                1429
         AG_EXNAME = 'ATTRIBUTE'                                          1429
         AG_PARLIST = 'SEEN_COLO'                                         1429
         CALL AXATTRIBUTE                                                 1429
C    *                                                                    1429
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1429
C    SLD_SSEPLEN <===                                                     1429
         CALL AGSRESET                                                    1431
         AG_RMIN = SSLD_SSEPRINN                                          1431
         AG_RMAX = SSLD_SSEPROUT                                          1431
         AG_DZ = SSLD_SSEPLEN                                             1431
         AG_TITLE = 'TUBE'                                                1431
         AG_EXNAME = 'SHAPE'                                              1431
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1431
         CALL AXSHAPE                                                     1431
C    *                                                                    1431
5351  CALL AGSPOPB                                                        1432
      GO TO BLOCK_SSSH, (5350 ,5360)                                      1432
C End of Local Procedure                                                  1432
C ----------------------------------------------------------------------  1433
C ------------ SOSH                                                       1433
C Begin Local Procedure SOSH                                              1434
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1434
5330  CONTINUE                                                            1434
         AG_TITLE='SOSH'                                                  1434
         AG_MODULE=MODULE                                                 1434
         AG_MARK='V00'                                                    1434
         CALL AGSPUSH                                                     1434
         AG_MODULE=MODULE                                                 1434
C    *                                                                    1434
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1435
         CALL AGSRESET                                                    1435
         AG_TITLE = 'SSDALMY'                                             1435
         AG_EXNAME = 'MATERIAL'                                           1435
         AG_PARLIST = 'NONE'                                              1435
         CALL AXMATERIAL                                                  1435
C    *                                                                    1435
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1435
         CALL AGSRESET                                                    1436
         AG_SEEN = 1                                                      1436
         AG_COLO = 3                                                      1436
         AG_TITLE = 'SOSH'                                                1436
         AG_EXNAME = 'ATTRIBUTE'                                          1436
         AG_PARLIST = 'SEEN_COLO'                                         1436
         CALL AXATTRIBUTE                                                 1436
C    *                                                                    1436
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1436
C    SLD_SOUTLEN <===                                                     1436
         CALL AGSRESET                                                    1438
         AG_RMIN = SSLD_SOUTRINN                                          1438
         AG_RMAX = SSLD_SOUTROUT                                          1438
         AG_DZ = SSLD_SOUTLEN                                             1438
         AG_TITLE = 'TUBE'                                                1438
         AG_EXNAME = 'SHAPE'                                              1438
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1438
         CALL AXSHAPE                                                     1438
C    *                                                                    1438
5331  CALL AGSPOPB                                                        1439
      GO TO BLOCK_SOSH, (5330 ,5340)                                      1439
C End of Local Procedure                                                  1439
      WRITE(LOUT,6850)'===>>  [End AgML Module SVTTGEO6]  <<==='          1439
6850  FORMAT(A32,/,/)                                                     1439
C MODULE SVTTGEO6                                                         1441
      END                                                                 1441
