C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO11                                                2
C SVTTGEO11 IS EVOLUTION OF SVTTGEO6 WITH CARBON REPLACING BE AS PER, DA  2
C VE LYNN: BASED ON THE SVTTGEO6 CODE                                     2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'SVTTGEO11'/, BANK_TITLE/'*'/                           2
      DATA MCOMMENT/'IS EVOLUTION OF SVTTGEO6 WITH CARBON REPLACING BE A  2
     *S PER, DAVE LYNN: BASED ON THE SVTTGEO6 CODE'/                      2
      LOGICAL FIRST                                                       2
      DATA FIRST/.TRUE./                                                  2
      SAVE FIRST                                                          2
      INTEGER JUMPAD                                                      2
      EXTERNAL JUMPAD                                                     2
      INTEGER OK                                                          2
      PARAMETER (OK=0)                                                    2
      INTEGER AG_I,AG_J                                                   2
      SAVE AG_I,AG_J                                                      2
C AUTHOR MAXIM POTEKHIN                                                   2
      DATA AG_I/0/,AG_J/0/                                                3
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C CREATED 15 AUG 2007                                                     3
      CHARACTER*40 CREATED /'15 AUG 2007'/                                4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
C - Advanced Geant interface                                              5
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      5
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       5
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         5
     *AG_OPTION,AG_ORT,AG_MARK                                            5
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         5
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     5
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     5
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    5
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          5
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   5
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     5
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        5
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   5
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 5
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     5
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       5
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        5
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       5
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       5
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       5
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         5
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     5
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       5
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   5
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       5
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     5
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          5
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         5
     *-696969,AG_LDETU=250)                                               5
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    5
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  5
     *AG_IMFLD,AG_ISIMU                                                   5
C Inherited variables a: saved during internal calls                      5
C b: reset at the entry in a block and then saved                         5
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        5
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       5
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         5
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     5
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       5
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         5
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           5
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        5
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        5
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            5
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           5
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    5
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      5
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           5
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          5
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                5
C local variables valid inside same block                                 5
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       5
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  5
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         5
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    5
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          5
     *AG_OPTION                                                           5
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        5
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             5
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              5
C temporarly                                                              5
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           5
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    5
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       5
     *AG_HY)                                                              5
      REAL MIL_P,INCH_P                                                   5
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   6
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        6
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       6
C - GEANT description of logical units                                    6
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            7
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      7
      COMMON/GCMAIL/ CHMAIL                                               7
      CHARACTER*132 CHMAIL                                                7
      INTEGER BLOCK_SVTT                                                  7
      INTEGER BLOCK_SLYD                                                  16
      INTEGER BLOCK_SLSD                                                  16
      INTEGER BLOCK_SLDI                                                  16
      INTEGER BLOCK_STLI                                                  16
      INTEGER BLOCK_STSI                                                  16
      INTEGER BLOCK_SVTD                                                  16
      INTEGER BLOCK_SBER                                                  16
      INTEGER BLOCK_STAB                                                  16
      INTEGER BLOCK_STRU                                                  16
      INTEGER BLOCK_SPCB                                                  16
      INTEGER BLOCK_SRHC                                                  16
      INTEGER BLOCK_SBWC                                                  16
      INTEGER BLOCK_SWCM                                                  16
      INTEGER BLOCK_SXAI                                                  16
      INTEGER BLOCK_SXBI                                                  16
      INTEGER BLOCK_STRA                                                  16
      INTEGER BLOCK_SSID                                                  16
      INTEGER BLOCK_SSIR                                                  16
      INTEGER BLOCK_SELE                                                  16
      INTEGER BLOCK_SWCH                                                  16
      INTEGER BLOCK_SWCW                                                  16
      INTEGER BLOCK_SWCS                                                  16
      INTEGER BLOCK_SBOI                                                  16
      INTEGER BLOCK_SAGP                                                  16
      INTEGER BLOCK_SDYE                                                  16
      INTEGER BLOCK_SECA                                                  16
      INTEGER BLOCK_SIRP                                                  16
      INTEGER BLOCK_SIRT                                                  16
      INTEGER BLOCK_SOER                                                  16
      INTEGER BLOCK_SCON                                                  16
      INTEGER BLOCK_SROD                                                  16
      INTEGER BLOCK_SGRA                                                  16
      INTEGER BLOCK_STAP                                                  16
      INTEGER BLOCK_STAC                                                  16
      INTEGER BLOCK_SHLA                                                  16
      INTEGER BLOCK_SHLB                                                  16
      INTEGER BLOCK_SHMA                                                  16
      INTEGER BLOCK_SHMB                                                  16
      INTEGER BLOCK_SWHO                                                  16
      INTEGER BLOCK_SHWA                                                  16
      INTEGER BLOCK_SCMY                                                  16
      INTEGER BLOCK_SCAL                                                  16
      INTEGER BLOCK_SWMM                                                  16
      INTEGER BLOCK_SWMB                                                  16
      INTEGER BLOCK_SWRP                                                  16
      INTEGER BLOCK_SXRL                                                  16
      INTEGER BLOCK_SYRU                                                  16
      INTEGER BLOCK_SWMT                                                  16
      INTEGER BLOCK_SWMS                                                  16
      INTEGER BLOCK_SWMW                                                  16
      INTEGER BLOCK_SOTB                                                  16
      INTEGER BLOCK_SITB                                                  16
      INTEGER BLOCK_SBRG                                                  16
      INTEGER BLOCK_SBRM                                                  16
      INTEGER BLOCK_SBRI                                                  16
      INTEGER BLOCK_SOES                                                  16
      INTEGER BLOCK_SIES                                                  16
      INTEGER BLOCK_SOSM                                                  16
      INTEGER BLOCK_SISM                                                  16
      INTEGER BLOCK_SCRW                                                  16
      INTEGER BLOCK_SGLA                                                  16
      INTEGER BLOCK_SBSP                                                  16
      INTEGER BLOCK_SAKM                                                  16
      INTEGER BLOCK_SCKM                                                  16
      INTEGER BLOCK_SBSR                                                  16
      INTEGER BLOCK_SBCR                                                  16
      INTEGER BLOCK_SBRX                                                  16
      INTEGER BLOCK_SBRL                                                  16
      INTEGER BLOCK_SBMM                                                  16
      INTEGER BLOCK_SBMO                                                  16
      INTEGER BLOCK_SBMI                                                  16
      INTEGER BLOCK_SMRD                                                  16
      INTEGER BLOCK_SALM                                                  16
      INTEGER BLOCK_SISH                                                  16
      INTEGER BLOCK_SSSH                                                  16
      INTEGER BLOCK_SOSH                                                  16
      INTEGER BLOCK_SCBM                                                  16
      INTEGER BLOCK_SCBL                                                  16
      INTEGER BLOCK_SFED                                                  16
      INTEGER BLOCK_SPLS                                                  16
      INTEGER BLOCK_SOUM                                                  16
      INTEGER BLOCK_SOUR                                                  16
C -                                                                       18
      REAL SVTG_VERSION                                                   18
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            18
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       18
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          18
     *MSVTG_VERSION)                                                      18
      CHARACTER CSVTG_VERSION*40/' '/                                     18
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             18
      INTEGER MSVTG_VERSION(2)/1,1/                                       18
      REAL RE_SVTG_VERSION                                                18
C -                                                                       18
      REAL SVTG_CONFIG                                                    18
      EQUIVALENCE (BANK_SVTG(4),SVTG_CONFIG,RE_SVTG_CONFIG),              18
     *(NAMES_SVTG(2),NSVTG_CONFIG)                                        18
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_CONFIG),(MAP_SVTG(1,2),           18
     *MSVTG_CONFIG)                                                       18
      CHARACTER CSVTG_CONFIG*40/' '/                                      18
      CHARACTER NSVTG_CONFIG*12/'R.CONFIG'/                               18
      INTEGER MSVTG_CONFIG(2)/1,1/                                        18
      REAL RE_SVTG_CONFIG                                                 18
C -                                                                       18
      INTEGER SVTG_NLAYER                                                 18
      EQUIVALENCE (BANK_SVTG(5),SVTG_NLAYER,RE_SVTG_NLAYER),              18
     *(NAMES_SVTG(3),NSVTG_NLAYER)                                        18
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_NLAYER),(MAP_SVTG(1,3),           18
     *MSVTG_NLAYER)                                                       18
      CHARACTER CSVTG_NLAYER*40/' '/                                      18
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               18
      INTEGER MSVTG_NLAYER(2)/1,1/                                        18
      REAL RE_SVTG_NLAYER                                                 18
C -                                                                       18
      REAL SVTG_RSIZEMIN                                                  18
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          18
     *(NAMES_SVTG(4),NSVTG_RSIZEMIN)                                      18
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMIN),(MAP_SVTG(1,4),         18
     *MSVTG_RSIZEMIN)                                                     18
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    18
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           18
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      18
      REAL RE_SVTG_RSIZEMIN                                               18
C -                                                                       18
      REAL SVTG_RSIZEMAX                                                  18
      EQUIVALENCE (BANK_SVTG(7),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          18
     *(NAMES_SVTG(5),NSVTG_RSIZEMAX)                                      18
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_RSIZEMAX),(MAP_SVTG(1,5),         18
     *MSVTG_RSIZEMAX)                                                     18
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    18
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           18
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      18
      REAL RE_SVTG_RSIZEMAX                                               18
C -                                                                       18
      REAL SVTG_ZSIZEMAX                                                  18
      EQUIVALENCE (BANK_SVTG(8),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          18
     *(NAMES_SVTG(6),NSVTG_ZSIZEMAX)                                      18
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ZSIZEMAX),(MAP_SVTG(1,6),         18
     *MSVTG_ZSIZEMAX)                                                     18
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    18
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           18
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      18
      REAL RE_SVTG_ZSIZEMAX                                               18
C -                                                                       18
      REAL SVTG_ANGOFF                                                    18
      EQUIVALENCE (BANK_SVTG(9),SVTG_ANGOFF,RE_SVTG_ANGOFF),              18
     *(NAMES_SVTG(7),NSVTG_ANGOFF)                                        18
      EQUIVALENCE (COMMEN_SVTG(7),CSVTG_ANGOFF),(MAP_SVTG(1,7),           18
     *MSVTG_ANGOFF)                                                       18
      CHARACTER CSVTG_ANGOFF*40/' '/                                      18
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               18
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        18
      REAL RE_SVTG_ANGOFF                                                 18
C -                                                                       18
      REAL SVTG_SUPPORTVER                                                18
      EQUIVALENCE (BANK_SVTG(10),SVTG_SUPPORTVER,RE_SVTG_SUPPORTVER),     18
     *(NAMES_SVTG(8),NSVTG_SUPPORTVER)                                    18
      EQUIVALENCE (COMMEN_SVTG(8),CSVTG_SUPPORTVER),(MAP_SVTG(1,8),       18
     *MSVTG_SUPPORTVER)                                                   18
      CHARACTER CSVTG_SUPPORTVER*40/' '/                                  18
      CHARACTER NSVTG_SUPPORTVER*12/'R.SUPPORTVER'/                       18
      INTEGER MSVTG_SUPPORTVER(2)/1,1/                                    18
      REAL RE_SVTG_SUPPORTVER                                             18
C -                                                                       18
      REAL SVTG_CONEVER                                                   18
      EQUIVALENCE (BANK_SVTG(11),SVTG_CONEVER,RE_SVTG_CONEVER),           18
     *(NAMES_SVTG(9),NSVTG_CONEVER)                                       18
      EQUIVALENCE (COMMEN_SVTG(9),CSVTG_CONEVER),(MAP_SVTG(1,9),          18
     *MSVTG_CONEVER)                                                      18
      CHARACTER CSVTG_CONEVER*40/' '/                                     18
      CHARACTER NSVTG_CONEVER*12/'R.CONEVER'/                             18
      INTEGER MSVTG_CONEVER(2)/1,1/                                       18
      REAL RE_SVTG_CONEVER                                                18
C -                                                                       18
      REAL SVTG_IFMANY                                                    18
      EQUIVALENCE (BANK_SVTG(12),SVTG_IFMANY,RE_SVTG_IFMANY),             18
     *(NAMES_SVTG(10),NSVTG_IFMANY)                                       18
      EQUIVALENCE (COMMEN_SVTG(10),CSVTG_IFMANY),(MAP_SVTG(1,10),         18
     *MSVTG_IFMANY)                                                       18
      CHARACTER CSVTG_IFMANY*40/' '/                                      18
      CHARACTER NSVTG_IFMANY*12/'R.IFMANY'/                               18
      INTEGER MSVTG_IFMANY(2)/1,1/                                        18
      REAL RE_SVTG_IFMANY                                                 18
C -                                                                       18
      REAL SVTG_NMIN                                                      18
      EQUIVALENCE (BANK_SVTG(13),SVTG_NMIN,RE_SVTG_NMIN),                 18
     *(NAMES_SVTG(11),NSVTG_NMIN)                                         18
      EQUIVALENCE (COMMEN_SVTG(11),CSVTG_NMIN),(MAP_SVTG(1,11),           18
     *MSVTG_NMIN)                                                         18
      CHARACTER CSVTG_NMIN*40/' '/                                        18
      CHARACTER NSVTG_NMIN*12/'R.NMIN'/                                   18
      INTEGER MSVTG_NMIN(2)/1,1/                                          18
      REAL RE_SVTG_NMIN                                                   18
C -                                                                       18
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                18
      PARAMETER (LEN_SVTG=13)                                             18
      PARAMETER (LLEN_SVTG=11)                                            18
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           18
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        18
      DATA IDNUM_SVTG/10*0/                                               18
      REAL BANK_SVTG(LEN_SVTG)                                            18
      CHARACTER*60 BPATH_SVTG/' '/                                        18
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          18
     *BPATH_SVTG                                                          18
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             18
C *                                                                       18
C -                                                                       20
      REAL SWCA_VERSION                                                   20
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            20
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       20
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          20
     *MSWCA_VERSION)                                                      20
      CHARACTER CSWCA_VERSION*40/' '/                                     20
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             20
      INTEGER MSWCA_VERSION(2)/1,1/                                       20
      REAL RE_SWCA_VERSION                                                20
C -                                                                       20
      REAL SWCA_LENGTH                                                    20
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              20
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        20
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           20
     *MSWCA_LENGTH)                                                       20
      CHARACTER CSWCA_LENGTH*40/' '/                                      20
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               20
      INTEGER MSWCA_LENGTH(2)/1,1/                                        20
      REAL RE_SWCA_LENGTH                                                 20
C -                                                                       20
      REAL SWCA_WAFERWID                                                  20
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          20
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      20
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         20
     *MSWCA_WAFERWID)                                                     20
      CHARACTER CSWCA_WAFERWID*40/' '/                                    20
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           20
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      20
      REAL RE_SWCA_WAFERWID                                               20
C -                                                                       20
      REAL SWCA_WAFERLEN                                                  20
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          20
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      20
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         20
     *MSWCA_WAFERLEN)                                                     20
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    20
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           20
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      20
      REAL RE_SWCA_WAFERLEN                                               20
C -                                                                       20
      REAL SWCA_WAFERTHK                                                  20
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          20
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      20
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         20
     *MSWCA_WAFERTHK)                                                     20
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    20
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           20
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      20
      REAL RE_SWCA_WAFERTHK                                               20
C -                                                                       20
      REAL SWCA_ROHATHK                                                   20
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            20
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       20
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          20
     *MSWCA_ROHATHK)                                                      20
      CHARACTER CSWCA_ROHATHK*40/' '/                                     20
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             20
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       20
      REAL RE_SWCA_ROHATHK                                                20
C -                                                                       20
      REAL SWCA_WAFCARWD                                                  20
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          20
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      20
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         20
     *MSWCA_WAFCARWD)                                                     20
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    20
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           20
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      20
      REAL RE_SWCA_WAFCARWD                                               20
C -                                                                       20
      REAL SWCA_WAFCARTH                                                  20
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         20
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      20
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         20
     *MSWCA_WAFCARTH)                                                     20
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    20
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           20
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      20
      REAL RE_SWCA_WAFCARTH                                               20
C -                                                                       20
      REAL SWCA_WAFERGAP                                                  20
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         20
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      20
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         20
     *MSWCA_WAFERGAP)                                                     20
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    20
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           20
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      20
      REAL RE_SWCA_WAFERGAP                                               20
C -                                                                       20
      REAL SWCA_DRIFT                                                     20
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               20
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        20
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          20
     *MSWCA_DRIFT)                                                        20
      CHARACTER CSWCA_DRIFT*40/' '/                                       20
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 20
      INTEGER MSWCA_DRIFT(2)/1,1/                                         20
      REAL RE_SWCA_DRIFT                                                  20
C -                                                                       20
      REAL SWCA_STRUTLEN                                                  20
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         20
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     20
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       20
     *MSWCA_STRUTLEN)                                                     20
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    20
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           20
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      20
      REAL RE_SWCA_STRUTLEN                                               20
C -                                                                       20
      REAL SWCA_SENSWID                                                   20
      EQUIVALENCE (BANK_SWCA(14),SWCA_SENSWID,RE_SWCA_SENSWID),           20
     *(NAMES_SWCA(12),NSWCA_SENSWID)                                      20
      EQUIVALENCE (COMMEN_SWCA(12),CSWCA_SENSWID),(MAP_SWCA(1,12),        20
     *MSWCA_SENSWID)                                                      20
      CHARACTER CSWCA_SENSWID*40/' '/                                     20
      CHARACTER NSWCA_SENSWID*12/'R.SENSWID'/                             20
      INTEGER MSWCA_SENSWID(2)/1,1/                                       20
      REAL RE_SWCA_SENSWID                                                20
C -                                                                       20
      REAL SWCA_SENSLEN                                                   20
      EQUIVALENCE (BANK_SWCA(15),SWCA_SENSLEN,RE_SWCA_SENSLEN),           20
     *(NAMES_SWCA(13),NSWCA_SENSLEN)                                      20
      EQUIVALENCE (COMMEN_SWCA(13),CSWCA_SENSLEN),(MAP_SWCA(1,13),        20
     *MSWCA_SENSLEN)                                                      20
      CHARACTER CSWCA_SENSLEN*40/' '/                                     20
      CHARACTER NSWCA_SENSLEN*12/'R.SENSLEN'/                             20
      INTEGER MSWCA_SENSLEN(2)/1,1/                                       20
      REAL RE_SWCA_SENSLEN                                                20
C -                                                                       20
      REAL SWCA_SENSGIR                                                   20
      EQUIVALENCE (BANK_SWCA(16),SWCA_SENSGIR,RE_SWCA_SENSGIR),           20
     *(NAMES_SWCA(14),NSWCA_SENSGIR)                                      20
      EQUIVALENCE (COMMEN_SWCA(14),CSWCA_SENSGIR),(MAP_SWCA(1,14),        20
     *MSWCA_SENSGIR)                                                      20
      CHARACTER CSWCA_SENSGIR*40/' '/                                     20
      CHARACTER NSWCA_SENSGIR*12/'R.SENSGIR'/                             20
      INTEGER MSWCA_SENSGIR(2)/1,1/                                       20
      REAL RE_SWCA_SENSGIR                                                20
C -                                                                       20
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                20
      PARAMETER (LEN_SWCA=16)                                             20
      PARAMETER (LLEN_SWCA=14)                                            20
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           20
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        20
      DATA IDNUM_SWCA/10*0/                                               20
      REAL BANK_SWCA(LEN_SWCA)                                            20
      CHARACTER*60 BPATH_SWCA/' '/                                        20
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          20
     *BPATH_SWCA                                                          20
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             20
C *                                                                       20
C -                                                                       23
      REAL SSUP_VERSION                                                   23
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            23
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       23
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          23
     *MSSUP_VERSION)                                                      23
      CHARACTER CSSUP_VERSION*40/' '/                                     23
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             23
      INTEGER MSSUP_VERSION(2)/1,1/                                       23
      REAL RE_SSUP_VERSION                                                23
C -                                                                       23
      REAL SSUP_CABTHK                                                    23
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              23
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        23
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           23
     *MSSUP_CABTHK)                                                       23
      CHARACTER CSSUP_CABTHK*40/' '/                                      23
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               23
      INTEGER MSSUP_CABTHK(2)/1,1/                                        23
      REAL RE_SSUP_CABTHK                                                 23
C -                                                                       23
      REAL SSUP_HOSRMN                                                    23
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              23
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        23
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           23
     *MSSUP_HOSRMN)                                                       23
      CHARACTER CSSUP_HOSRMN*40/' '/                                      23
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               23
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        23
      REAL RE_SSUP_HOSRMN                                                 23
C -                                                                       23
      REAL SSUP_HOSRMX                                                    23
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              23
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        23
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           23
     *MSSUP_HOSRMX)                                                       23
      CHARACTER CSSUP_HOSRMX*40/' '/                                      23
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               23
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        23
      REAL RE_SSUP_HOSRMX                                                 23
C -                                                                       23
      REAL SSUP_NHOSES                                                    23
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              23
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        23
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           23
     *MSSUP_NHOSES)                                                       23
      CHARACTER CSSUP_NHOSES*40/' '/                                      23
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               23
      INTEGER MSSUP_NHOSES(2)/1,1/                                        23
      REAL RE_SSUP_NHOSES                                                 23
C -                                                                       23
      REAL SSUP_WRPMYTHK                                                  23
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          23
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      23
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         23
     *MSSUP_WRPMYTHK)                                                     23
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    23
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           23
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      23
      REAL RE_SSUP_WRPMYTHK                                               23
C -                                                                       23
      REAL SSUP_WRPALTHK                                                  23
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          23
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      23
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         23
     *MSSUP_WRPALTHK)                                                     23
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    23
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           23
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      23
      REAL RE_SSUP_WRPALTHK                                               23
C -                                                                       23
      REAL SSUP_GRPHTHK                                                   23
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           23
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       23
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          23
     *MSSUP_GRPHTHK)                                                      23
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     23
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             23
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       23
      REAL RE_SSUP_GRPHTHK                                                23
C -                                                                       23
      REAL SSUP_CONE1ZMN                                                  23
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         23
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      23
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         23
     *MSSUP_CONE1ZMN)                                                     23
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    23
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           23
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      23
      REAL RE_SSUP_CONE1ZMN                                               23
C -                                                                       23
      REAL SSUP_RODLEN                                                    23
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             23
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       23
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         23
     *MSSUP_RODLEN)                                                       23
      CHARACTER CSSUP_RODLEN*40/' '/                                      23
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               23
      INTEGER MSSUP_RODLEN(2)/1,1/                                        23
      REAL RE_SSUP_RODLEN                                                 23
C -                                                                       23
      REAL SSUP_RODDIST                                                   23
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           23
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      23
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        23
     *MSSUP_RODDIST)                                                      23
      CHARACTER CSSUP_RODDIST*40/' '/                                     23
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             23
      INTEGER MSSUP_RODDIST(2)/1,1/                                       23
      REAL RE_SSUP_RODDIST                                                23
C -                                                                       23
      REAL SSUP_RODID                                                     23
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               23
     *(NAMES_SSUP(12),NSSUP_RODID)                                        23
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          23
     *MSSUP_RODID)                                                        23
      CHARACTER CSSUP_RODID*40/' '/                                       23
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 23
      INTEGER MSSUP_RODID(2)/1,1/                                         23
      REAL RE_SSUP_RODID                                                  23
C -                                                                       23
      REAL SSUP_RODOD                                                     23
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               23
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        23
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          23
     *MSSUP_RODOD)                                                        23
      CHARACTER CSSUP_RODOD*40/' '/                                       23
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 23
      INTEGER MSSUP_RODOD(2)/1,1/                                         23
      REAL RE_SSUP_RODOD                                                  23
C -                                                                       23
      REAL SSUP_CON1IDMN                                                  23
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         23
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     23
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       23
     *MSSUP_CON1IDMN)                                                     23
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    23
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           23
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      23
      REAL RE_SSUP_CON1IDMN                                               23
C -                                                                       23
      REAL SSUP_CON3IDMN                                                  23
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         23
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     23
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       23
     *MSSUP_CON3IDMN)                                                     23
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    23
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           23
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      23
      REAL RE_SSUP_CON3IDMN                                               23
C -                                                                       23
      REAL SSUP_CON4IDMN                                                  23
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         23
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     23
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       23
     *MSSUP_CON4IDMN)                                                     23
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    23
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           23
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      23
      REAL RE_SSUP_CON4IDMN                                               23
C -                                                                       23
      REAL SSUP_CON4IDMX                                                  23
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         23
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     23
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       23
     *MSSUP_CON4IDMX)                                                     23
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    23
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           23
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      23
      REAL RE_SSUP_CON4IDMX                                               23
C -                                                                       23
      REAL SSUP_CONE3ZMX                                                  23
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         23
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     23
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       23
     *MSSUP_CONE3ZMX)                                                     23
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    23
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           23
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      23
      REAL RE_SSUP_CONE3ZMX                                               23
C -                                                                       23
      REAL SSUP_CONE4ZMX                                                  23
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         23
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     23
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       23
     *MSSUP_CONE4ZMX)                                                     23
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    23
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           23
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      23
      REAL RE_SSUP_CONE4ZMX                                               23
C -                                                                       23
      REAL SSUP_BRATHK                                                    23
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             23
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       23
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         23
     *MSSUP_BRATHK)                                                       23
      CHARACTER CSSUP_BRATHK*40/' '/                                      23
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               23
      INTEGER MSSUP_BRATHK(2)/1,1/                                        23
      REAL RE_SSUP_BRATHK                                                 23
C -                                                                       23
      REAL SSUP_ERJTHK                                                    23
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             23
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       23
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         23
     *MSSUP_ERJTHK)                                                       23
      CHARACTER CSSUP_ERJTHK*40/' '/                                      23
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               23
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        23
      REAL RE_SSUP_ERJTHK                                                 23
C -                                                                       23
      REAL SSUP_ERJWID                                                    23
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             23
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       23
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         23
     *MSSUP_ERJWID)                                                       23
      CHARACTER CSSUP_ERJWID*40/' '/                                      23
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               23
      INTEGER MSSUP_ERJWID(2)/1,1/                                        23
      REAL RE_SSUP_ERJWID                                                 23
C -                                                                       23
      REAL SSUP_ERJLEN                                                    23
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             23
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       23
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         23
     *MSSUP_ERJLEN)                                                       23
      CHARACTER CSSUP_ERJLEN*40/' '/                                      23
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               23
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        23
      REAL RE_SSUP_ERJLEN                                                 23
C -                                                                       23
      REAL SSUP_ERJZDIS                                                   23
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           23
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      23
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        23
     *MSSUP_ERJZDIS)                                                      23
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     23
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             23
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       23
      REAL RE_SSUP_ERJZDIS                                                23
C -                                                                       23
      REAL SSUP_ERJ1X                                                     23
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               23
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        23
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          23
     *MSSUP_ERJ1X)                                                        23
      CHARACTER CSSUP_ERJ1X*40/' '/                                       23
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 23
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         23
      REAL RE_SSUP_ERJ1X                                                  23
C -                                                                       23
      REAL SSUP_ERJ2X                                                     23
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               23
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        23
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          23
     *MSSUP_ERJ2X)                                                        23
      CHARACTER CSSUP_ERJ2X*40/' '/                                       23
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 23
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         23
      REAL RE_SSUP_ERJ2X                                                  23
C -                                                                       23
      REAL SSUP_ERJ2Y                                                     23
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               23
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        23
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          23
     *MSSUP_ERJ2Y)                                                        23
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       23
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 23
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         23
      REAL RE_SSUP_ERJ2Y                                                  23
C -                                                                       23
      REAL SSUP_ERJRAD                                                    23
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             23
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       23
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         23
     *MSSUP_ERJRAD)                                                       23
      CHARACTER CSSUP_ERJRAD*40/' '/                                      23
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               23
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        23
      REAL RE_SSUP_ERJRAD                                                 23
C -                                                                       23
      REAL SSUP_ERJDIA                                                    23
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             23
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       23
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         23
     *MSSUP_ERJDIA)                                                       23
      CHARACTER CSSUP_ERJDIA*40/' '/                                      23
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               23
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        23
      REAL RE_SSUP_ERJDIA                                                 23
C -                                                                       23
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                23
      PARAMETER (LEN_SSUP=31)                                             23
      PARAMETER (LLEN_SSUP=29)                                            23
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           23
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        23
      DATA IDNUM_SSUP/10*0/                                               23
      REAL BANK_SSUP(LEN_SSUP)                                            23
      CHARACTER*60 BPATH_SSUP/' '/                                        23
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          23
     *BPATH_SSUP                                                          23
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             23
C *                                                                       23
C -                                                                       27
      REAL SSUB_VERSION                                                   27
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            27
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       27
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          27
     *MSSUB_VERSION)                                                      27
      CHARACTER CSSUB_VERSION*40/' '/                                     27
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             27
      INTEGER MSSUB_VERSION(2)/1,1/                                       27
      REAL RE_SSUB_VERSION                                                27
C -                                                                       27
      REAL SSUB_KMOUNTID                                                  27
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          27
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      27
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         27
     *MSSUB_KMOUNTID)                                                     27
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    27
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           27
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      27
      REAL RE_SSUB_KMOUNTID                                               27
C -                                                                       27
      REAL SSUB_KMOUNTOD                                                  27
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          27
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      27
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         27
     *MSSUB_KMOUNTOD)                                                     27
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    27
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           27
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      27
      REAL RE_SSUB_KMOUNTOD                                               27
C -                                                                       27
      REAL SSUB_KMNTTHK                                                   27
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            27
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       27
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          27
     *MSSUB_KMNTTHK)                                                      27
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     27
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             27
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       27
      REAL RE_SSUB_KMNTTHK                                                27
C -                                                                       27
      REAL SSUB_KMCUTOD                                                   27
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            27
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       27
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          27
     *MSSUB_KMCUTOD)                                                      27
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     27
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             27
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       27
      REAL RE_SSUB_KMCUTOD                                                27
C -                                                                       27
      REAL SSUB_KMCUTID                                                   27
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            27
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       27
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          27
     *MSSUB_KMCUTID)                                                      27
      CHARACTER CSSUB_KMCUTID*40/' '/                                     27
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             27
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       27
      REAL RE_SSUB_KMCUTID                                                27
C -                                                                       27
      REAL SSUB_KMCUTOA                                                   27
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            27
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       27
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          27
     *MSSUB_KMCUTOA)                                                      27
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     27
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             27
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       27
      REAL RE_SSUB_KMCUTOA                                                27
C -                                                                       27
      REAL SSUB_KMCUTOFF                                                  27
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         27
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      27
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         27
     *MSSUB_KMCUTOFF)                                                     27
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    27
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           27
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      27
      REAL RE_SSUB_KMCUTOFF                                               27
C -                                                                       27
      REAL SSUB_SRINGID                                                   27
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           27
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       27
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          27
     *MSSUB_SRINGID)                                                      27
      CHARACTER CSSUB_SRINGID*40/' '/                                     27
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             27
      INTEGER MSSUB_SRINGID(2)/1,1/                                       27
      REAL RE_SSUB_SRINGID                                                27
C -                                                                       27
      REAL SSUB_SRINGOD                                                   27
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           27
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      27
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        27
     *MSSUB_SRINGOD)                                                      27
      CHARACTER CSSUB_SRINGOD*40/' '/                                     27
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             27
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       27
      REAL RE_SSUB_SRINGOD                                                27
C -                                                                       27
      REAL SSUB_SRINGTHK                                                  27
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         27
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     27
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       27
     *MSSUB_SRINGTHK)                                                     27
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    27
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           27
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      27
      REAL RE_SSUB_SRINGTHK                                               27
C -                                                                       27
      REAL SSUB_SRCUTPHI                                                  27
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         27
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     27
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       27
     *MSSUB_SRCUTPHI)                                                     27
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    27
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           27
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      27
      REAL RE_SSUB_SRCUTPHI                                               27
C -                                                                       27
      REAL SSUB_SRCUTWID                                                  27
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         27
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     27
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       27
     *MSSUB_SRCUTWID)                                                     27
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    27
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           27
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      27
      REAL RE_SSUB_SRCUTWID                                               27
C -                                                                       27
      REAL SSUB_SRCUTOUT                                                  27
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         27
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     27
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       27
     *MSSUB_SRCUTOUT)                                                     27
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    27
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           27
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      27
      REAL RE_SSUB_SRCUTOUT                                               27
C -                                                                       27
      REAL SSUB_SRCUTIN                                                   27
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           27
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      27
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        27
     *MSSUB_SRCUTIN)                                                      27
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     27
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             27
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       27
      REAL RE_SSUB_SRCUTIN                                                27
C -                                                                       27
      REAL SSUB_SROLLID                                                   27
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           27
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      27
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        27
     *MSSUB_SROLLID)                                                      27
      CHARACTER CSSUB_SROLLID*40/' '/                                     27
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             27
      INTEGER MSSUB_SROLLID(2)/1,1/                                       27
      REAL RE_SSUB_SROLLID                                                27
C -                                                                       27
      REAL SSUB_SROLLOD                                                   27
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           27
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      27
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        27
     *MSSUB_SROLLOD)                                                      27
      CHARACTER CSSUB_SROLLOD*40/' '/                                     27
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             27
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       27
      REAL RE_SSUB_SROLLOD                                                27
C -                                                                       27
      REAL SSUB_SROLLLEN                                                  27
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         27
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     27
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       27
     *MSSUB_SROLLLEN)                                                     27
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    27
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           27
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      27
      REAL RE_SSUB_SROLLLEN                                               27
C -                                                                       27
      REAL SSUB_SWIRELEN                                                  27
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         27
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     27
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       27
     *MSSUB_SWIRELEN)                                                     27
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    27
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           27
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      27
      REAL RE_SSUB_SWIRELEN                                               27
C -                                                                       27
      REAL SSUB_MBLKHGH                                                   27
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           27
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      27
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        27
     *MSSUB_MBLKHGH)                                                      27
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     27
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             27
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       27
      REAL RE_SSUB_MBLKHGH                                                27
C -                                                                       27
      REAL SSUB_MBLKOWID                                                  27
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         27
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     27
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       27
     *MSSUB_MBLKOWID)                                                     27
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    27
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           27
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      27
      REAL RE_SSUB_MBLKOWID                                               27
C -                                                                       27
      REAL SSUB_MBLKOLEN                                                  27
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         27
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     27
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       27
     *MSSUB_MBLKOLEN)                                                     27
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    27
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           27
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      27
      REAL RE_SSUB_MBLKOLEN                                               27
C -                                                                       27
      REAL SSUB_MBLKIWID                                                  27
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         27
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     27
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       27
     *MSSUB_MBLKIWID)                                                     27
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    27
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           27
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      27
      REAL RE_SSUB_MBLKIWID                                               27
C -                                                                       27
      REAL SSUB_MBLKILEN                                                  27
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         27
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     27
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       27
     *MSSUB_MBLKILEN)                                                     27
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    27
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           27
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      27
      REAL RE_SSUB_MBLKILEN                                               27
C -                                                                       27
      REAL SSUB_MBLKORAD                                                  27
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         27
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     27
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       27
     *MSSUB_MBLKORAD)                                                     27
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    27
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           27
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      27
      REAL RE_SSUB_MBLKORAD                                               27
C -                                                                       27
      REAL SSUB_MBLKIRAD                                                  27
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         27
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     27
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       27
     *MSSUB_MBLKIRAD)                                                     27
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    27
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           27
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      27
      REAL RE_SSUB_MBLKIRAD                                               27
C -                                                                       27
      REAL SSUB_MRODDIA                                                   27
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           27
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      27
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        27
     *MSSUB_MRODDIA)                                                      27
      CHARACTER CSSUB_MRODDIA*40/' '/                                     27
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             27
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       27
      REAL RE_SSUB_MRODDIA                                                27
C -                                                                       27
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                27
      PARAMETER (LEN_SSUB=29)                                             27
      PARAMETER (LLEN_SSUB=27)                                            27
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           27
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        27
      DATA IDNUM_SSUB/10*0/                                               27
      REAL BANK_SSUB(LEN_SSUB)                                            27
      CHARACTER*60 BPATH_SSUB/' '/                                        27
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          27
     *BPATH_SSUB                                                          27
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             27
C *                                                                       27
C -                                                                       28
      REAL SWAM_VERSION                                                   28
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            28
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       28
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          28
     *MSWAM_VERSION)                                                      28
      CHARACTER CSWAM_VERSION*40/' '/                                     28
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             28
      INTEGER MSWAM_VERSION(2)/1,1/                                       28
      REAL RE_SWAM_VERSION                                                28
C -                                                                       28
      REAL SWAM_ZMIN                                                      28
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   28
     *NSWAM_ZMIN)                                                         28
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             28
     *MSWAM_ZMIN)                                                         28
      CHARACTER CSWAM_ZMIN*40/' '/                                        28
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   28
      INTEGER MSWAM_ZMIN(2)/1,1/                                          28
      REAL RE_SWAM_ZMIN                                                   28
C -                                                                       28
      REAL SWAM_LEN                                                       28
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     28
     *NSWAM_LEN)                                                          28
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    28
      CHARACTER CSWAM_LEN*40/' '/                                         28
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     28
      INTEGER MSWAM_LEN(2)/1,1/                                           28
      REAL RE_SWAM_LEN                                                    28
C -                                                                       28
      REAL SWAM_RMIN                                                      28
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   28
     *NSWAM_RMIN)                                                         28
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             28
     *MSWAM_RMIN)                                                         28
      CHARACTER CSWAM_RMIN*40/' '/                                        28
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   28
      INTEGER MSWAM_RMIN(2)/1,1/                                          28
      REAL RE_SWAM_RMIN                                                   28
C -                                                                       28
      REAL SWAM_RMAX                                                      28
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   28
     *NSWAM_RMAX)                                                         28
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             28
     *MSWAM_RMAX)                                                         28
      CHARACTER CSWAM_RMAX*40/' '/                                        28
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   28
      INTEGER MSWAM_RMAX(2)/1,1/                                          28
      REAL RE_SWAM_RMAX                                                   28
C -                                                                       28
      REAL SWAM_TBRDTHK                                                   28
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            28
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       28
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          28
     *MSWAM_TBRDTHK)                                                      28
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     28
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             28
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       28
      REAL RE_SWAM_TBRDTHK                                                28
C -                                                                       28
      REAL SWAM_WALLTHK                                                   28
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            28
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       28
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          28
     *MSWAM_WALLTHK)                                                      28
      CHARACTER CSWAM_WALLTHK*40/' '/                                     28
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             28
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       28
      REAL RE_SWAM_WALLTHK                                                28
C -                                                                       28
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                28
      PARAMETER (LEN_SWAM=9)                                              28
      PARAMETER (LLEN_SWAM=7)                                             28
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           28
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        28
      DATA IDNUM_SWAM/10*0/                                               28
      REAL BANK_SWAM(LEN_SWAM)                                            28
      CHARACTER*60 BPATH_SWAM/' '/                                        28
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          28
     *BPATH_SWAM                                                          28
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             28
C *                                                                       28
C -                                                                       29
      REAL SERG_VERSION                                                   29
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            29
     *(NAMES_SERG(1),NSERG_VERSION)                                       29
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          29
     *MSERG_VERSION)                                                      29
      CHARACTER CSERG_VERSION*40/' '/                                     29
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             29
      INTEGER MSERG_VERSION(2)/1,1/                                       29
      REAL RE_SERG_VERSION                                                29
C -                                                                       29
      REAL SERG_IRNGTRMX                                                  29
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          29
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      29
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         29
     *MSERG_IRNGTRMX)                                                     29
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    29
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           29
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      29
      REAL RE_SERG_IRNGTRMX                                               29
C -                                                                       29
      REAL SERG_IRNGPRMN                                                  29
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          29
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      29
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         29
     *MSERG_IRNGPRMN)                                                     29
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    29
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           29
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      29
      REAL RE_SERG_IRNGPRMN                                               29
C -                                                                       29
      REAL SERG_ORNGRMIN                                                  29
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          29
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      29
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         29
     *MSERG_ORNGRMIN)                                                     29
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    29
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           29
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      29
      REAL RE_SERG_ORNGRMIN                                               29
C -                                                                       29
      REAL SERG_ORNGRMAX                                                  29
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          29
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      29
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         29
     *MSERG_ORNGRMAX)                                                     29
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    29
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           29
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      29
      REAL RE_SERG_ORNGRMAX                                               29
C -                                                                       29
      REAL SERG_ENDRNGTH                                                  29
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          29
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      29
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         29
     *MSERG_ENDRNGTH)                                                     29
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    29
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           29
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      29
      REAL RE_SERG_ENDRNGTH                                               29
C -                                                                       29
      REAL SERG_ENDRNGZM                                                  29
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          29
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      29
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         29
     *MSERG_ENDRNGZM)                                                     29
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    29
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           29
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      29
      REAL RE_SERG_ENDRNGZM                                               29
C -                                                                       29
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                29
      PARAMETER (LEN_SERG=9)                                              29
      PARAMETER (LLEN_SERG=7)                                             29
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           29
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        29
      DATA IDNUM_SERG/10*0/                                               29
      REAL BANK_SERG(LEN_SERG)                                            29
      CHARACTER*60 BPATH_SERG/' '/                                        29
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          29
     *BPATH_SERG                                                          29
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             29
C *                                                                       29
C -                                                                       31
      REAL SELC_VERSION                                                   31
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            31
     *(NAMES_SELC(1),NSELC_VERSION)                                       31
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          31
     *MSELC_VERSION)                                                      31
      CHARACTER CSELC_VERSION*40/' '/                                     31
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             31
      INTEGER MSELC_VERSION(2)/1,1/                                       31
      REAL RE_SELC_VERSION                                                31
C -                                                                       31
      REAL SELC_BETHK                                                     31
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                31
     *(NAMES_SELC(2),NSELC_BETHK)                                         31
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            31
     *MSELC_BETHK)                                                        31
      CHARACTER CSELC_BETHK*40/' '/                                       31
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 31
      INTEGER MSELC_BETHK(2)/1,1/                                         31
      REAL RE_SELC_BETHK                                                  31
C -                                                                       31
      REAL SELC_WATTHK                                                    31
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              31
     *(NAMES_SELC(3),NSELC_WATTHK)                                        31
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           31
     *MSELC_WATTHK)                                                       31
      CHARACTER CSELC_WATTHK*40/' '/                                      31
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               31
      INTEGER MSELC_WATTHK(2)/1,1/                                        31
      REAL RE_SELC_WATTHK                                                 31
C -                                                                       31
      REAL SELC_BEOTHK                                                    31
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              31
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        31
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           31
     *MSELC_BEOTHK)                                                       31
      CHARACTER CSELC_BEOTHK*40/' '/                                      31
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               31
      INTEGER MSELC_BEOTHK(2)/1,1/                                        31
      REAL RE_SELC_BEOTHK                                                 31
C -                                                                       31
      REAL SELC_DYETHK                                                    31
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              31
     *(NAMES_SELC(5),NSELC_DYETHK)                                        31
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           31
     *MSELC_DYETHK)                                                       31
      CHARACTER CSELC_DYETHK*40/' '/                                      31
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               31
      INTEGER MSELC_DYETHK(2)/1,1/                                        31
      REAL RE_SELC_DYETHK                                                 31
C -                                                                       31
      REAL SELC_DYEWID                                                    31
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              31
     *(NAMES_SELC(6),NSELC_DYEWID)                                        31
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           31
     *MSELC_DYEWID)                                                       31
      CHARACTER CSELC_DYEWID*40/' '/                                      31
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               31
      INTEGER MSELC_DYEWID(2)/1,1/                                        31
      REAL RE_SELC_DYEWID                                                 31
C -                                                                       31
      REAL SELC_DYESPC                                                    31
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              31
     *(NAMES_SELC(7),NSELC_DYESPC)                                        31
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           31
     *MSELC_DYESPC)                                                       31
      CHARACTER CSELC_DYESPC*40/' '/                                      31
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               31
      INTEGER MSELC_DYESPC(2)/1,1/                                        31
      REAL RE_SELC_DYESPC                                                 31
C -                                                                       31
      REAL SELC_ELCAWID                                                   31
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           31
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       31
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          31
     *MSELC_ELCAWID)                                                      31
      CHARACTER CSELC_ELCAWID*40/' '/                                     31
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             31
      INTEGER MSELC_ELCAWID(2)/1,1/                                       31
      REAL RE_SELC_ELCAWID                                                31
C -                                                                       31
      REAL SELC_AGPDTHK                                                   31
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           31
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       31
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          31
     *MSELC_AGPDTHK)                                                      31
      CHARACTER CSELC_AGPDTHK*40/' '/                                     31
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             31
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       31
      REAL RE_SELC_AGPDTHK                                                31
C -                                                                       31
      REAL SELC_GLASSTHK                                                  31
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         31
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     31
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       31
     *MSELC_GLASSTHK)                                                     31
      CHARACTER CSELC_GLASSTHK*40/' '/                                    31
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           31
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      31
      REAL RE_SELC_GLASSTHK                                               31
C -                                                                       31
      REAL SELC_CABTHK                                                    31
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             31
     *(NAMES_SELC(11),NSELC_CABTHK)                                       31
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         31
     *MSELC_CABTHK)                                                       31
      CHARACTER CSELC_CABTHK*40/' '/                                      31
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               31
      INTEGER MSELC_CABTHK(2)/1,1/                                        31
      REAL RE_SELC_CABTHK                                                 31
C -                                                                       31
      REAL SELC_CABWID                                                    31
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             31
     *(NAMES_SELC(12),NSELC_CABWID)                                       31
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         31
     *MSELC_CABWID)                                                       31
      CHARACTER CSELC_CABWID*40/' '/                                      31
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               31
      INTEGER MSELC_CABWID(2)/1,1/                                        31
      REAL RE_SELC_CABWID                                                 31
C -                                                                       31
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                31
      PARAMETER (LEN_SELC=14)                                             31
      PARAMETER (LLEN_SELC=12)                                            31
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           31
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        31
      DATA IDNUM_SELC/10*0/                                               31
      REAL BANK_SELC(LEN_SELC)                                            31
      CHARACTER*60 BPATH_SELC/' '/                                        31
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          31
     *BPATH_SELC                                                          31
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             31
C *                                                                       31
C -                                                                       33
      INTEGER SVTL_LAYER                                                  33
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                33
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         33
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            33
     *MSVTL_LAYER)                                                        33
      CHARACTER CSVTL_LAYER*40/' '/                                       33
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 33
      INTEGER MSVTL_LAYER(2)/1,1/                                         33
      REAL RE_SVTL_LAYER                                                  33
C -                                                                       33
      REAL SVTL_NLADDER                                                   33
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            33
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       33
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          33
     *MSVTL_NLADDER)                                                      33
      CHARACTER CSVTL_NLADDER*40/' '/                                     33
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             33
      INTEGER MSVTL_NLADDER(2)/1,1/                                       33
      REAL RE_SVTL_NLADDER                                                33
C -                                                                       33
      REAL SVTL_NWAFER                                                    33
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              33
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        33
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           33
     *MSVTL_NWAFER)                                                       33
      CHARACTER CSVTL_NWAFER*40/' '/                                      33
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               33
      INTEGER MSVTL_NWAFER(2)/1,1/                                        33
      REAL RE_SVTL_NWAFER                                                 33
C -                                                                       33
      REAL SVTL_RADIUS                                                    33
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              33
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        33
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           33
     *MSVTL_RADIUS)                                                       33
      CHARACTER CSVTL_RADIUS*40/' '/                                      33
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               33
      INTEGER MSVTL_RADIUS(2)/1,1/                                        33
      REAL RE_SVTL_RADIUS                                                 33
C -                                                                       33
      REAL SVTL_BAREEDGE                                                  33
      EQUIVALENCE (BANK_SVTL(7),SVTL_BAREEDGE,RE_SVTL_BAREEDGE),          33
     *(NAMES_SVTL(5),NSVTL_BAREEDGE)                                      33
      EQUIVALENCE (COMMEN_SVTL(5),CSVTL_BAREEDGE),(MAP_SVTL(1,5),         33
     *MSVTL_BAREEDGE)                                                     33
      CHARACTER CSVTL_BAREEDGE*40/' '/                                    33
      CHARACTER NSVTL_BAREEDGE*12/'R.BAREEDGE'/                           33
      INTEGER MSVTL_BAREEDGE(2)/1,1/                                      33
      REAL RE_SVTL_BAREEDGE                                               33
C -                                                                       33
      REAL SVTL_PCBLEN                                                    33
      EQUIVALENCE (BANK_SVTL(8),SVTL_PCBLEN,RE_SVTL_PCBLEN),              33
     *(NAMES_SVTL(6),NSVTL_PCBLEN)                                        33
      EQUIVALENCE (COMMEN_SVTL(6),CSVTL_PCBLEN),(MAP_SVTL(1,6),           33
     *MSVTL_PCBLEN)                                                       33
      CHARACTER CSVTL_PCBLEN*40/' '/                                      33
      CHARACTER NSVTL_PCBLEN*12/'R.PCBLEN'/                               33
      INTEGER MSVTL_PCBLEN(2)/1,1/                                        33
      REAL RE_SVTL_PCBLEN                                                 33
C -                                                                       33
      REAL SVTL_PCBWIDTH                                                  33
      EQUIVALENCE (BANK_SVTL(9),SVTL_PCBWIDTH,RE_SVTL_PCBWIDTH),          33
     *(NAMES_SVTL(7),NSVTL_PCBWIDTH)                                      33
      EQUIVALENCE (COMMEN_SVTL(7),CSVTL_PCBWIDTH),(MAP_SVTL(1,7),         33
     *MSVTL_PCBWIDTH)                                                     33
      CHARACTER CSVTL_PCBWIDTH*40/' '/                                    33
      CHARACTER NSVTL_PCBWIDTH*12/'R.PCBWIDTH'/                           33
      INTEGER MSVTL_PCBWIDTH(2)/1,1/                                      33
      REAL RE_SVTL_PCBWIDTH                                               33
C -                                                                       33
      REAL SVTL_PCBTHK                                                    33
      EQUIVALENCE (BANK_SVTL(10),SVTL_PCBTHK,RE_SVTL_PCBTHK),             33
     *(NAMES_SVTL(8),NSVTL_PCBTHK)                                        33
      EQUIVALENCE (COMMEN_SVTL(8),CSVTL_PCBTHK),(MAP_SVTL(1,8),           33
     *MSVTL_PCBTHK)                                                       33
      CHARACTER CSVTL_PCBTHK*40/' '/                                      33
      CHARACTER NSVTL_PCBTHK*12/'R.PCBTHK'/                               33
      INTEGER MSVTL_PCBTHK(2)/1,1/                                        33
      REAL RE_SVTL_PCBTHK                                                 33
C -                                                                       33
      REAL SVTL_PCBGAP                                                    33
      EQUIVALENCE (BANK_SVTL(11),SVTL_PCBGAP,RE_SVTL_PCBGAP),             33
     *(NAMES_SVTL(9),NSVTL_PCBGAP)                                        33
      EQUIVALENCE (COMMEN_SVTL(9),CSVTL_PCBGAP),(MAP_SVTL(1,9),           33
     *MSVTL_PCBGAP)                                                       33
      CHARACTER CSVTL_PCBGAP*40/' '/                                      33
      CHARACTER NSVTL_PCBGAP*12/'R.PCBGAP'/                               33
      INTEGER MSVTL_PCBGAP(2)/1,1/                                        33
      REAL RE_SVTL_PCBGAP                                                 33
C -                                                                       33
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                33
      PARAMETER (LEN_SVTL=11)                                             33
      PARAMETER (LLEN_SVTL=9)                                             33
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           33
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        33
      DATA IDNUM_SVTL/10*0/                                               33
      REAL BANK_SVTL(LEN_SVTL)                                            33
      CHARACTER*60 BPATH_SVTL/' '/                                        33
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          33
     *BPATH_SVTL                                                          33
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             33
C *                                                                       33
C -                                                                       35
      REAL SSLD_VERSION                                                   35
      EQUIVALENCE (BANK_SSLD(3),SSLD_VERSION,RE_SSLD_VERSION),            35
     *(NAMES_SSLD(1),NSSLD_VERSION)                                       35
      EQUIVALENCE (COMMEN_SSLD(1),CSSLD_VERSION),(MAP_SSLD(1,1),          35
     *MSSLD_VERSION)                                                      35
      CHARACTER CSSLD_VERSION*40/' '/                                     35
      CHARACTER NSSLD_VERSION*12/'R.VERSION'/                             35
      INTEGER MSSLD_VERSION(2)/1,1/                                       35
      REAL RE_SSLD_VERSION                                                35
C -                                                                       35
      REAL SSLD_SINRINN                                                   35
      EQUIVALENCE (BANK_SSLD(4),SSLD_SINRINN,RE_SSLD_SINRINN),            35
     *(NAMES_SSLD(2),NSSLD_SINRINN)                                       35
      EQUIVALENCE (COMMEN_SSLD(2),CSSLD_SINRINN),(MAP_SSLD(1,2),          35
     *MSSLD_SINRINN)                                                      35
      CHARACTER CSSLD_SINRINN*40/' '/                                     35
      CHARACTER NSSLD_SINRINN*12/'R.SINRINN'/                             35
      INTEGER MSSLD_SINRINN(2)/1,1/                                       35
      REAL RE_SSLD_SINRINN                                                35
C -                                                                       35
      REAL SSLD_SINROUT                                                   35
      EQUIVALENCE (BANK_SSLD(5),SSLD_SINROUT,RE_SSLD_SINROUT),            35
     *(NAMES_SSLD(3),NSSLD_SINROUT)                                       35
      EQUIVALENCE (COMMEN_SSLD(3),CSSLD_SINROUT),(MAP_SSLD(1,3),          35
     *MSSLD_SINROUT)                                                      35
      CHARACTER CSSLD_SINROUT*40/' '/                                     35
      CHARACTER NSSLD_SINROUT*12/'R.SINROUT'/                             35
      INTEGER MSSLD_SINROUT(2)/1,1/                                       35
      REAL RE_SSLD_SINROUT                                                35
C -                                                                       35
      REAL SSLD_SINLEN                                                    35
      EQUIVALENCE (BANK_SSLD(6),SSLD_SINLEN,RE_SSLD_SINLEN),              35
     *(NAMES_SSLD(4),NSSLD_SINLEN)                                        35
      EQUIVALENCE (COMMEN_SSLD(4),CSSLD_SINLEN),(MAP_SSLD(1,4),           35
     *MSSLD_SINLEN)                                                       35
      CHARACTER CSSLD_SINLEN*40/' '/                                      35
      CHARACTER NSSLD_SINLEN*12/'R.SINLEN'/                               35
      INTEGER MSSLD_SINLEN(2)/1,1/                                        35
      REAL RE_SSLD_SINLEN                                                 35
C -                                                                       35
      REAL SSLD_SSEPRINN                                                  35
      EQUIVALENCE (BANK_SSLD(7),SSLD_SSEPRINN,RE_SSLD_SSEPRINN),          35
     *(NAMES_SSLD(5),NSSLD_SSEPRINN)                                      35
      EQUIVALENCE (COMMEN_SSLD(5),CSSLD_SSEPRINN),(MAP_SSLD(1,5),         35
     *MSSLD_SSEPRINN)                                                     35
      CHARACTER CSSLD_SSEPRINN*40/' '/                                    35
      CHARACTER NSSLD_SSEPRINN*12/'R.SSEPRINN'/                           35
      INTEGER MSSLD_SSEPRINN(2)/1,1/                                      35
      REAL RE_SSLD_SSEPRINN                                               35
C -                                                                       35
      REAL SSLD_SSEPROUT                                                  35
      EQUIVALENCE (BANK_SSLD(8),SSLD_SSEPROUT,RE_SSLD_SSEPROUT),          35
     *(NAMES_SSLD(6),NSSLD_SSEPROUT)                                      35
      EQUIVALENCE (COMMEN_SSLD(6),CSSLD_SSEPROUT),(MAP_SSLD(1,6),         35
     *MSSLD_SSEPROUT)                                                     35
      CHARACTER CSSLD_SSEPROUT*40/' '/                                    35
      CHARACTER NSSLD_SSEPROUT*12/'R.SSEPROUT'/                           35
      INTEGER MSSLD_SSEPROUT(2)/1,1/                                      35
      REAL RE_SSLD_SSEPROUT                                               35
C -                                                                       35
      REAL SSLD_SSEPLEN                                                   35
      EQUIVALENCE (BANK_SSLD(9),SSLD_SSEPLEN,RE_SSLD_SSEPLEN),            35
     *(NAMES_SSLD(7),NSSLD_SSEPLEN)                                       35
      EQUIVALENCE (COMMEN_SSLD(7),CSSLD_SSEPLEN),(MAP_SSLD(1,7),          35
     *MSSLD_SSEPLEN)                                                      35
      CHARACTER CSSLD_SSEPLEN*40/' '/                                     35
      CHARACTER NSSLD_SSEPLEN*12/'R.SSEPLEN'/                             35
      INTEGER MSSLD_SSEPLEN(2)/1,1/                                       35
      REAL RE_SSLD_SSEPLEN                                                35
C -                                                                       35
      REAL SSLD_SOUTRINN                                                  35
      EQUIVALENCE (BANK_SSLD(10),SSLD_SOUTRINN,RE_SSLD_SOUTRINN),         35
     *(NAMES_SSLD(8),NSSLD_SOUTRINN)                                      35
      EQUIVALENCE (COMMEN_SSLD(8),CSSLD_SOUTRINN),(MAP_SSLD(1,8),         35
     *MSSLD_SOUTRINN)                                                     35
      CHARACTER CSSLD_SOUTRINN*40/' '/                                    35
      CHARACTER NSSLD_SOUTRINN*12/'R.SOUTRINN'/                           35
      INTEGER MSSLD_SOUTRINN(2)/1,1/                                      35
      REAL RE_SSLD_SOUTRINN                                               35
C -                                                                       35
      REAL SSLD_SOUTROUT                                                  35
      EQUIVALENCE (BANK_SSLD(11),SSLD_SOUTROUT,RE_SSLD_SOUTROUT),         35
     *(NAMES_SSLD(9),NSSLD_SOUTROUT)                                      35
      EQUIVALENCE (COMMEN_SSLD(9),CSSLD_SOUTROUT),(MAP_SSLD(1,9),         35
     *MSSLD_SOUTROUT)                                                     35
      CHARACTER CSSLD_SOUTROUT*40/' '/                                    35
      CHARACTER NSSLD_SOUTROUT*12/'R.SOUTROUT'/                           35
      INTEGER MSSLD_SOUTROUT(2)/1,1/                                      35
      REAL RE_SSLD_SOUTROUT                                               35
C -                                                                       35
      REAL SSLD_SOUTLEN                                                   35
      EQUIVALENCE (BANK_SSLD(12),SSLD_SOUTLEN,RE_SSLD_SOUTLEN),           35
     *(NAMES_SSLD(10),NSSLD_SOUTLEN)                                      35
      EQUIVALENCE (COMMEN_SSLD(10),CSSLD_SOUTLEN),(MAP_SSLD(1,10),        35
     *MSSLD_SOUTLEN)                                                      35
      CHARACTER CSSLD_SOUTLEN*40/' '/                                     35
      CHARACTER NSSLD_SOUTLEN*12/'R.SOUTLEN'/                             35
      INTEGER MSSLD_SOUTLEN(2)/1,1/                                       35
      REAL RE_SSLD_SOUTLEN                                                35
C -                                                                       35
      REAL SSLD_ALMESHID                                                  35
      EQUIVALENCE (BANK_SSLD(13),SSLD_ALMESHID,RE_SSLD_ALMESHID),         35
     *(NAMES_SSLD(11),NSSLD_ALMESHID)                                     35
      EQUIVALENCE (COMMEN_SSLD(11),CSSLD_ALMESHID),(MAP_SSLD(1,11),       35
     *MSSLD_ALMESHID)                                                     35
      CHARACTER CSSLD_ALMESHID*40/' '/                                    35
      CHARACTER NSSLD_ALMESHID*12/'R.ALMESHID'/                           35
      INTEGER MSSLD_ALMESHID(2)/1,1/                                      35
      REAL RE_SSLD_ALMESHID                                               35
C -                                                                       35
      REAL SSLD_ALMESHOD                                                  35
      EQUIVALENCE (BANK_SSLD(14),SSLD_ALMESHOD,RE_SSLD_ALMESHOD),         35
     *(NAMES_SSLD(12),NSSLD_ALMESHOD)                                     35
      EQUIVALENCE (COMMEN_SSLD(12),CSSLD_ALMESHOD),(MAP_SSLD(1,12),       35
     *MSSLD_ALMESHOD)                                                     35
      CHARACTER CSSLD_ALMESHOD*40/' '/                                    35
      CHARACTER NSSLD_ALMESHOD*12/'R.ALMESHOD'/                           35
      INTEGER MSSLD_ALMESHOD(2)/1,1/                                      35
      REAL RE_SSLD_ALMESHOD                                               35
C -                                                                       35
      REAL SSLD_ALMSHTHK                                                  35
      EQUIVALENCE (BANK_SSLD(15),SSLD_ALMSHTHK,RE_SSLD_ALMSHTHK),         35
     *(NAMES_SSLD(13),NSSLD_ALMSHTHK)                                     35
      EQUIVALENCE (COMMEN_SSLD(13),CSSLD_ALMSHTHK),(MAP_SSLD(1,13),       35
     *MSSLD_ALMSHTHK)                                                     35
      CHARACTER CSSLD_ALMSHTHK*40/' '/                                    35
      CHARACTER NSSLD_ALMSHTHK*12/'R.ALMSHTHK'/                           35
      INTEGER MSSLD_ALMSHTHK(2)/1,1/                                      35
      REAL RE_SSLD_ALMSHTHK                                               35
C -                                                                       35
      REAL SSLD_ALMSHPOS                                                  35
      EQUIVALENCE (BANK_SSLD(16),SSLD_ALMSHPOS,RE_SSLD_ALMSHPOS),         35
     *(NAMES_SSLD(14),NSSLD_ALMSHPOS)                                     35
      EQUIVALENCE (COMMEN_SSLD(14),CSSLD_ALMSHPOS),(MAP_SSLD(1,14),       35
     *MSSLD_ALMSHPOS)                                                     35
      CHARACTER CSSLD_ALMSHPOS*40/' '/                                    35
      CHARACTER NSSLD_ALMSHPOS*12/'R.ALMSHPOS'/                           35
      INTEGER MSSLD_ALMSHPOS(2)/1,1/                                      35
      REAL RE_SSLD_ALMSHPOS                                               35
C -                                                                       35
      INTEGER LEN_SSLD,LLEN_SSLD,LINK_SSLD                                35
      PARAMETER (LEN_SSLD=16)                                             35
      PARAMETER (LLEN_SSLD=14)                                            35
      INTEGER FLAG_SSLD/0/,MAP_SSLD(2,LLEN_SSLD),IDNUM_SSLD(10)           35
      CHARACTER NAMES_SSLD(LLEN_SSLD)*12,COMMEN_SSLD(LLEN_SSLD)*40        35
      DATA IDNUM_SSLD/10*0/                                               35
      REAL BANK_SSLD(LEN_SSLD)                                            35
      CHARACTER*60 BPATH_SSLD/' '/                                        35
      SAVE FLAG_SSLD,MAP_SSLD,IDNUM_SSLD,NAMES_SSLD,COMMEN_SSLD,          35
     *BPATH_SSLD                                                          35
      DATA LINK_SSLD/0/,BANK_SSLD/LEN_SSLD*0/                             35
C *                                                                       35
C -                                                                       36
      INTEGER SCBP_LAYER                                                  36
      EQUIVALENCE (BANK_SCBP(3),SCBP_LAYER,RE_SCBP_LAYER),                36
     *(NAMES_SCBP(1),NSCBP_LAYER)                                         36
      EQUIVALENCE (COMMEN_SCBP(1),CSCBP_LAYER),(MAP_SCBP(1,1),            36
     *MSCBP_LAYER)                                                        36
      CHARACTER CSCBP_LAYER*40/' '/                                       36
      CHARACTER NSCBP_LAYER*12/'I.LAYER'/                                 36
      INTEGER MSCBP_LAYER(2)/1,1/                                         36
      REAL RE_SCBP_LAYER                                                  36
C -                                                                       36
      REAL SCBP_LEN                                                       36
      EQUIVALENCE (BANK_SCBP(4),SCBP_LEN,RE_SCBP_LEN),(NAMES_SCBP(2),     36
     *NSCBP_LEN)                                                          36
      EQUIVALENCE (COMMEN_SCBP(2),CSCBP_LEN),(MAP_SCBP(1,2),MSCBP_LEN)    36
      CHARACTER CSCBP_LEN*40/' '/                                         36
      CHARACTER NSCBP_LEN*12/'R.LEN'/                                     36
      INTEGER MSCBP_LEN(2)/1,1/                                           36
      REAL RE_SCBP_LEN                                                    36
C -                                                                       36
      REAL SCBP_RMIN1                                                     36
      EQUIVALENCE (BANK_SCBP(5),SCBP_RMIN1,RE_SCBP_RMIN1),                36
     *(NAMES_SCBP(3),NSCBP_RMIN1)                                         36
      EQUIVALENCE (COMMEN_SCBP(3),CSCBP_RMIN1),(MAP_SCBP(1,3),            36
     *MSCBP_RMIN1)                                                        36
      CHARACTER CSCBP_RMIN1*40/' '/                                       36
      CHARACTER NSCBP_RMIN1*12/'R.RMIN1'/                                 36
      INTEGER MSCBP_RMIN1(2)/1,1/                                         36
      REAL RE_SCBP_RMIN1                                                  36
C -                                                                       36
      REAL SCBP_RMAX1                                                     36
      EQUIVALENCE (BANK_SCBP(6),SCBP_RMAX1,RE_SCBP_RMAX1),                36
     *(NAMES_SCBP(4),NSCBP_RMAX1)                                         36
      EQUIVALENCE (COMMEN_SCBP(4),CSCBP_RMAX1),(MAP_SCBP(1,4),            36
     *MSCBP_RMAX1)                                                        36
      CHARACTER CSCBP_RMAX1*40/' '/                                       36
      CHARACTER NSCBP_RMAX1*12/'R.RMAX1'/                                 36
      INTEGER MSCBP_RMAX1(2)/1,1/                                         36
      REAL RE_SCBP_RMAX1                                                  36
C -                                                                       36
      REAL SCBP_RMIN2                                                     36
      EQUIVALENCE (BANK_SCBP(7),SCBP_RMIN2,RE_SCBP_RMIN2),                36
     *(NAMES_SCBP(5),NSCBP_RMIN2)                                         36
      EQUIVALENCE (COMMEN_SCBP(5),CSCBP_RMIN2),(MAP_SCBP(1,5),            36
     *MSCBP_RMIN2)                                                        36
      CHARACTER CSCBP_RMIN2*40/' '/                                       36
      CHARACTER NSCBP_RMIN2*12/'R.RMIN2'/                                 36
      INTEGER MSCBP_RMIN2(2)/1,1/                                         36
      REAL RE_SCBP_RMIN2                                                  36
C -                                                                       36
      REAL SCBP_RMAX2                                                     36
      EQUIVALENCE (BANK_SCBP(8),SCBP_RMAX2,RE_SCBP_RMAX2),                36
     *(NAMES_SCBP(6),NSCBP_RMAX2)                                         36
      EQUIVALENCE (COMMEN_SCBP(6),CSCBP_RMAX2),(MAP_SCBP(1,6),            36
     *MSCBP_RMAX2)                                                        36
      CHARACTER CSCBP_RMAX2*40/' '/                                       36
      CHARACTER NSCBP_RMAX2*12/'R.RMAX2'/                                 36
      INTEGER MSCBP_RMAX2(2)/1,1/                                         36
      REAL RE_SCBP_RMAX2                                                  36
C -                                                                       36
      REAL SCBP_VOL                                                       36
      EQUIVALENCE (BANK_SCBP(9),SCBP_VOL,RE_SCBP_VOL),(NAMES_SCBP(7),     36
     *NSCBP_VOL)                                                          36
      EQUIVALENCE (COMMEN_SCBP(7),CSCBP_VOL),(MAP_SCBP(1,7),MSCBP_VOL)    36
      CHARACTER CSCBP_VOL*40/' '/                                         36
      CHARACTER NSCBP_VOL*12/'R.VOL'/                                     36
      INTEGER MSCBP_VOL(2)/1,1/                                           36
      REAL RE_SCBP_VOL                                                    36
C -                                                                       36
      INTEGER LEN_SCBP,LLEN_SCBP,LINK_SCBP                                36
      PARAMETER (LEN_SCBP=9)                                              36
      PARAMETER (LLEN_SCBP=7)                                             36
      INTEGER FLAG_SCBP/0/,MAP_SCBP(2,LLEN_SCBP),IDNUM_SCBP(10)           36
      CHARACTER NAMES_SCBP(LLEN_SCBP)*12,COMMEN_SCBP(LLEN_SCBP)*40        36
      DATA IDNUM_SCBP/10*0/                                               36
      REAL BANK_SCBP(LEN_SCBP)                                            36
      CHARACTER*60 BPATH_SCBP/' '/                                        36
      SAVE FLAG_SCBP,MAP_SCBP,IDNUM_SCBP,NAMES_SCBP,COMMEN_SCBP,          36
     *BPATH_SCBP                                                          36
      DATA LINK_SCBP/0/,BANK_SCBP/LEN_SCBP*0/                             36
C *                                                                       36
C -                                                                       37
      INTEGER SFEP_LAYER                                                  37
      EQUIVALENCE (BANK_SFEP(3),SFEP_LAYER,RE_SFEP_LAYER),                37
     *(NAMES_SFEP(1),NSFEP_LAYER)                                         37
      EQUIVALENCE (COMMEN_SFEP(1),CSFEP_LAYER),(MAP_SFEP(1,1),            37
     *MSFEP_LAYER)                                                        37
      CHARACTER CSFEP_LAYER*40/' '/                                       37
      CHARACTER NSFEP_LAYER*12/'I.LAYER'/                                 37
      INTEGER MSFEP_LAYER(2)/1,1/                                         37
      REAL RE_SFEP_LAYER                                                  37
C -                                                                       37
      REAL SFEP_LEN                                                       37
      EQUIVALENCE (BANK_SFEP(4),SFEP_LEN,RE_SFEP_LEN),(NAMES_SFEP(2),     37
     *NSFEP_LEN)                                                          37
      EQUIVALENCE (COMMEN_SFEP(2),CSFEP_LEN),(MAP_SFEP(1,2),MSFEP_LEN)    37
      CHARACTER CSFEP_LEN*40/' '/                                         37
      CHARACTER NSFEP_LEN*12/'R.LEN'/                                     37
      INTEGER MSFEP_LEN(2)/1,1/                                           37
      REAL RE_SFEP_LEN                                                    37
C -                                                                       37
      REAL SFEP_RMIN1                                                     37
      EQUIVALENCE (BANK_SFEP(5),SFEP_RMIN1,RE_SFEP_RMIN1),                37
     *(NAMES_SFEP(3),NSFEP_RMIN1)                                         37
      EQUIVALENCE (COMMEN_SFEP(3),CSFEP_RMIN1),(MAP_SFEP(1,3),            37
     *MSFEP_RMIN1)                                                        37
      CHARACTER CSFEP_RMIN1*40/' '/                                       37
      CHARACTER NSFEP_RMIN1*12/'R.RMIN1'/                                 37
      INTEGER MSFEP_RMIN1(2)/1,1/                                         37
      REAL RE_SFEP_RMIN1                                                  37
C -                                                                       37
      REAL SFEP_RMAX1                                                     37
      EQUIVALENCE (BANK_SFEP(6),SFEP_RMAX1,RE_SFEP_RMAX1),                37
     *(NAMES_SFEP(4),NSFEP_RMAX1)                                         37
      EQUIVALENCE (COMMEN_SFEP(4),CSFEP_RMAX1),(MAP_SFEP(1,4),            37
     *MSFEP_RMAX1)                                                        37
      CHARACTER CSFEP_RMAX1*40/' '/                                       37
      CHARACTER NSFEP_RMAX1*12/'R.RMAX1'/                                 37
      INTEGER MSFEP_RMAX1(2)/1,1/                                         37
      REAL RE_SFEP_RMAX1                                                  37
C -                                                                       37
      REAL SFEP_RMIN2                                                     37
      EQUIVALENCE (BANK_SFEP(7),SFEP_RMIN2,RE_SFEP_RMIN2),                37
     *(NAMES_SFEP(5),NSFEP_RMIN2)                                         37
      EQUIVALENCE (COMMEN_SFEP(5),CSFEP_RMIN2),(MAP_SFEP(1,5),            37
     *MSFEP_RMIN2)                                                        37
      CHARACTER CSFEP_RMIN2*40/' '/                                       37
      CHARACTER NSFEP_RMIN2*12/'R.RMIN2'/                                 37
      INTEGER MSFEP_RMIN2(2)/1,1/                                         37
      REAL RE_SFEP_RMIN2                                                  37
C -                                                                       37
      REAL SFEP_RMAX2                                                     37
      EQUIVALENCE (BANK_SFEP(8),SFEP_RMAX2,RE_SFEP_RMAX2),                37
     *(NAMES_SFEP(6),NSFEP_RMAX2)                                         37
      EQUIVALENCE (COMMEN_SFEP(6),CSFEP_RMAX2),(MAP_SFEP(1,6),            37
     *MSFEP_RMAX2)                                                        37
      CHARACTER CSFEP_RMAX2*40/' '/                                       37
      CHARACTER NSFEP_RMAX2*12/'R.RMAX2'/                                 37
      INTEGER MSFEP_RMAX2(2)/1,1/                                         37
      REAL RE_SFEP_RMAX2                                                  37
C -                                                                       37
      REAL SFEP_VOL                                                       37
      EQUIVALENCE (BANK_SFEP(9),SFEP_VOL,RE_SFEP_VOL),(NAMES_SFEP(7),     37
     *NSFEP_VOL)                                                          37
      EQUIVALENCE (COMMEN_SFEP(7),CSFEP_VOL),(MAP_SFEP(1,7),MSFEP_VOL)    37
      CHARACTER CSFEP_VOL*40/' '/                                         37
      CHARACTER NSFEP_VOL*12/'R.VOL'/                                     37
      INTEGER MSFEP_VOL(2)/1,1/                                           37
      REAL RE_SFEP_VOL                                                    37
C -                                                                       37
      REAL SFEP_VOLPLAST                                                  37
      EQUIVALENCE (BANK_SFEP(10),SFEP_VOLPLAST,RE_SFEP_VOLPLAST),         37
     *(NAMES_SFEP(8),NSFEP_VOLPLAST)                                      37
      EQUIVALENCE (COMMEN_SFEP(8),CSFEP_VOLPLAST),(MAP_SFEP(1,8),         37
     *MSFEP_VOLPLAST)                                                     37
      CHARACTER CSFEP_VOLPLAST*40/' '/                                    37
      CHARACTER NSFEP_VOLPLAST*12/'R.VOLPLAST'/                           37
      INTEGER MSFEP_VOLPLAST(2)/1,1/                                      37
      REAL RE_SFEP_VOLPLAST                                               37
C -                                                                       37
      INTEGER LEN_SFEP,LLEN_SFEP,LINK_SFEP                                37
      PARAMETER (LEN_SFEP=10)                                             37
      PARAMETER (LLEN_SFEP=8)                                             37
      INTEGER FLAG_SFEP/0/,MAP_SFEP(2,LLEN_SFEP),IDNUM_SFEP(10)           37
      CHARACTER NAMES_SFEP(LLEN_SFEP)*12,COMMEN_SFEP(LLEN_SFEP)*40        37
      DATA IDNUM_SFEP/10*0/                                               37
      REAL BANK_SFEP(LEN_SFEP)                                            37
      CHARACTER*60 BPATH_SFEP/' '/                                        37
      SAVE FLAG_SFEP,MAP_SFEP,IDNUM_SFEP,NAMES_SFEP,COMMEN_SFEP,          37
     *BPATH_SFEP                                                          37
      DATA LINK_SFEP/0/,BANK_SFEP/LEN_SFEP*0/                             37
C *                                                                       37
C -                                                                       38
      INTEGER SWCX_LAYER                                                  38
      EQUIVALENCE (BANK_SWCX(3),SWCX_LAYER,RE_SWCX_LAYER),                38
     *(NAMES_SWCX(1),NSWCX_LAYER)                                         38
      EQUIVALENCE (COMMEN_SWCX(1),CSWCX_LAYER),(MAP_SWCX(1,1),            38
     *MSWCX_LAYER)                                                        38
      CHARACTER CSWCX_LAYER*40/' '/                                       38
      CHARACTER NSWCX_LAYER*12/'I.LAYER'/                                 38
      INTEGER MSWCX_LAYER(2)/1,1/                                         38
      REAL RE_SWCX_LAYER                                                  38
C -                                                                       38
      REAL SWCX_LENGTH                                                    38
      EQUIVALENCE (BANK_SWCX(4),SWCX_LENGTH,RE_SWCX_LENGTH),              38
     *(NAMES_SWCX(2),NSWCX_LENGTH)                                        38
      EQUIVALENCE (COMMEN_SWCX(2),CSWCX_LENGTH),(MAP_SWCX(1,2),           38
     *MSWCX_LENGTH)                                                       38
      CHARACTER CSWCX_LENGTH*40/' '/                                      38
      CHARACTER NSWCX_LENGTH*12/'R.LENGTH'/                               38
      INTEGER MSWCX_LENGTH(2)/1,1/                                        38
      REAL RE_SWCX_LENGTH                                                 38
C -                                                                       38
      REAL SWCX_DR                                                        38
      EQUIVALENCE (BANK_SWCX(5),SWCX_DR,RE_SWCX_DR),(NAMES_SWCX(3),       38
     *NSWCX_DR)                                                           38
      EQUIVALENCE (COMMEN_SWCX(3),CSWCX_DR),(MAP_SWCX(1,3),MSWCX_DR)      38
      CHARACTER CSWCX_DR*40/' '/                                          38
      CHARACTER NSWCX_DR*12/'R.DR'/                                       38
      INTEGER MSWCX_DR(2)/1,1/                                            38
      REAL RE_SWCX_DR                                                     38
C -                                                                       38
      REAL SWCX_OFFSET                                                    38
      EQUIVALENCE (BANK_SWCX(6),SWCX_OFFSET,RE_SWCX_OFFSET),              38
     *(NAMES_SWCX(4),NSWCX_OFFSET)                                        38
      EQUIVALENCE (COMMEN_SWCX(4),CSWCX_OFFSET),(MAP_SWCX(1,4),           38
     *MSWCX_OFFSET)                                                       38
      CHARACTER CSWCX_OFFSET*40/' '/                                      38
      CHARACTER NSWCX_OFFSET*12/'R.OFFSET'/                               38
      INTEGER MSWCX_OFFSET(2)/1,1/                                        38
      REAL RE_SWCX_OFFSET                                                 38
C -                                                                       38
      REAL SWCX_RAD                                                       38
      EQUIVALENCE (BANK_SWCX(7),SWCX_RAD,RE_SWCX_RAD),(NAMES_SWCX(5),     38
     *NSWCX_RAD)                                                          38
      EQUIVALENCE (COMMEN_SWCX(5),CSWCX_RAD),(MAP_SWCX(1,5),MSWCX_RAD)    38
      CHARACTER CSWCX_RAD*40/' '/                                         38
      CHARACTER NSWCX_RAD*12/'R.RAD'/                                     38
      INTEGER MSWCX_RAD(2)/1,1/                                           38
      REAL RE_SWCX_RAD                                                    38
C -                                                                       38
      REAL SWCX_WALL                                                      38
      EQUIVALENCE (BANK_SWCX(8),SWCX_WALL,RE_SWCX_WALL),(NAMES_SWCX(6),   38
     *NSWCX_WALL)                                                         38
      EQUIVALENCE (COMMEN_SWCX(6),CSWCX_WALL),(MAP_SWCX(1,6),             38
     *MSWCX_WALL)                                                         38
      CHARACTER CSWCX_WALL*40/' '/                                        38
      CHARACTER NSWCX_WALL*12/'R.WALL'/                                   38
      INTEGER MSWCX_WALL(2)/1,1/                                          38
      REAL RE_SWCX_WALL                                                   38
C -                                                                       38
      REAL SWCX_ROFFSET                                                   38
      EQUIVALENCE (BANK_SWCX(9),SWCX_ROFFSET,RE_SWCX_ROFFSET),            38
     *(NAMES_SWCX(7),NSWCX_ROFFSET)                                       38
      EQUIVALENCE (COMMEN_SWCX(7),CSWCX_ROFFSET),(MAP_SWCX(1,7),          38
     *MSWCX_ROFFSET)                                                      38
      CHARACTER CSWCX_ROFFSET*40/' '/                                     38
      CHARACTER NSWCX_ROFFSET*12/'R.ROFFSET'/                             38
      INTEGER MSWCX_ROFFSET(2)/1,1/                                       38
      REAL RE_SWCX_ROFFSET                                                38
C -                                                                       38
      INTEGER LEN_SWCX,LLEN_SWCX,LINK_SWCX                                38
      PARAMETER (LEN_SWCX=9)                                              38
      PARAMETER (LLEN_SWCX=7)                                             38
      INTEGER FLAG_SWCX/0/,MAP_SWCX(2,LLEN_SWCX),IDNUM_SWCX(10)           38
      CHARACTER NAMES_SWCX(LLEN_SWCX)*12,COMMEN_SWCX(LLEN_SWCX)*40        38
      DATA IDNUM_SWCX/10*0/                                               38
      REAL BANK_SWCX(LEN_SWCX)                                            38
      CHARACTER*60 BPATH_SWCX/' '/                                        38
      SAVE FLAG_SWCX,MAP_SWCX,IDNUM_SWCX,NAMES_SWCX,COMMEN_SWCX,          38
     *BPATH_SWCX                                                          38
      DATA LINK_SWCX/0/,BANK_SWCX/LEN_SWCX*0/                             38
C *                                                                       38
C -                                                                       39
      REAL SOUP_VERSION                                                   39
      EQUIVALENCE (BANK_SOUP(3),SOUP_VERSION,RE_SOUP_VERSION),            39
     *(NAMES_SOUP(1),NSOUP_VERSION)                                       39
      EQUIVALENCE (COMMEN_SOUP(1),CSOUP_VERSION),(MAP_SOUP(1,1),          39
     *MSOUP_VERSION)                                                      39
      CHARACTER CSOUP_VERSION*40/' '/                                     39
      CHARACTER NSOUP_VERSION*12/'R.VERSION'/                             39
      INTEGER MSOUP_VERSION(2)/1,1/                                       39
      REAL RE_SOUP_VERSION                                                39
C -                                                                       39
      REAL SOUP_LENGTH                                                    39
      EQUIVALENCE (BANK_SOUP(4),SOUP_LENGTH,RE_SOUP_LENGTH),              39
     *(NAMES_SOUP(2),NSOUP_LENGTH)                                        39
      EQUIVALENCE (COMMEN_SOUP(2),CSOUP_LENGTH),(MAP_SOUP(1,2),           39
     *MSOUP_LENGTH)                                                       39
      CHARACTER CSOUP_LENGTH*40/' '/                                      39
      CHARACTER NSOUP_LENGTH*12/'R.LENGTH'/                               39
      INTEGER MSOUP_LENGTH(2)/1,1/                                        39
      REAL RE_SOUP_LENGTH                                                 39
C -                                                                       39
      REAL SOUP_ROUT                                                      39
      EQUIVALENCE (BANK_SOUP(5),SOUP_ROUT,RE_SOUP_ROUT),(NAMES_SOUP(3),   39
     *NSOUP_ROUT)                                                         39
      EQUIVALENCE (COMMEN_SOUP(3),CSOUP_ROUT),(MAP_SOUP(1,3),             39
     *MSOUP_ROUT)                                                         39
      CHARACTER CSOUP_ROUT*40/' '/                                        39
      CHARACTER NSOUP_ROUT*12/'R.ROUT'/                                   39
      INTEGER MSOUP_ROUT(2)/1,1/                                          39
      REAL RE_SOUP_ROUT                                                   39
C -                                                                       39
      REAL SOUP_DR                                                        39
      EQUIVALENCE (BANK_SOUP(6),SOUP_DR,RE_SOUP_DR),(NAMES_SOUP(4),       39
     *NSOUP_DR)                                                           39
      EQUIVALENCE (COMMEN_SOUP(4),CSOUP_DR),(MAP_SOUP(1,4),MSOUP_DR)      39
      CHARACTER CSOUP_DR*40/' '/                                          39
      CHARACTER NSOUP_DR*12/'R.DR'/                                       39
      INTEGER MSOUP_DR(2)/1,1/                                            39
      REAL RE_SOUP_DR                                                     39
C -                                                                       39
      REAL SOUP_PHI1                                                      39
      EQUIVALENCE (BANK_SOUP(7),SOUP_PHI1,RE_SOUP_PHI1),(NAMES_SOUP(5),   39
     *NSOUP_PHI1)                                                         39
      EQUIVALENCE (COMMEN_SOUP(5),CSOUP_PHI1),(MAP_SOUP(1,5),             39
     *MSOUP_PHI1)                                                         39
      CHARACTER CSOUP_PHI1*40/' '/                                        39
      CHARACTER NSOUP_PHI1*12/'R.PHI1'/                                   39
      INTEGER MSOUP_PHI1(2)/1,1/                                          39
      REAL RE_SOUP_PHI1                                                   39
C -                                                                       39
      REAL SOUP_PHI2                                                      39
      EQUIVALENCE (BANK_SOUP(8),SOUP_PHI2,RE_SOUP_PHI2),(NAMES_SOUP(6),   39
     *NSOUP_PHI2)                                                         39
      EQUIVALENCE (COMMEN_SOUP(6),CSOUP_PHI2),(MAP_SOUP(1,6),             39
     *MSOUP_PHI2)                                                         39
      CHARACTER CSOUP_PHI2*40/' '/                                        39
      CHARACTER NSOUP_PHI2*12/'R.PHI2'/                                   39
      INTEGER MSOUP_PHI2(2)/1,1/                                          39
      REAL RE_SOUP_PHI2                                                   39
C -                                                                       39
      REAL SOUP_DIAMOUT                                                   39
      EQUIVALENCE (BANK_SOUP(9),SOUP_DIAMOUT,RE_SOUP_DIAMOUT),            39
     *(NAMES_SOUP(7),NSOUP_DIAMOUT)                                       39
      EQUIVALENCE (COMMEN_SOUP(7),CSOUP_DIAMOUT),(MAP_SOUP(1,7),          39
     *MSOUP_DIAMOUT)                                                      39
      CHARACTER CSOUP_DIAMOUT*40/' '/                                     39
      CHARACTER NSOUP_DIAMOUT*12/'R.DIAMOUT'/                             39
      INTEGER MSOUP_DIAMOUT(2)/1,1/                                       39
      REAL RE_SOUP_DIAMOUT                                                39
C -                                                                       39
      REAL SOUP_DIAMIN                                                    39
      EQUIVALENCE (BANK_SOUP(10),SOUP_DIAMIN,RE_SOUP_DIAMIN),             39
     *(NAMES_SOUP(8),NSOUP_DIAMIN)                                        39
      EQUIVALENCE (COMMEN_SOUP(8),CSOUP_DIAMIN),(MAP_SOUP(1,8),           39
     *MSOUP_DIAMIN)                                                       39
      CHARACTER CSOUP_DIAMIN*40/' '/                                      39
      CHARACTER NSOUP_DIAMIN*12/'R.DIAMIN'/                               39
      INTEGER MSOUP_DIAMIN(2)/1,1/                                        39
      REAL RE_SOUP_DIAMIN                                                 39
C -                                                                       39
      INTEGER LEN_SOUP,LLEN_SOUP,LINK_SOUP                                39
      PARAMETER (LEN_SOUP=10)                                             39
      PARAMETER (LLEN_SOUP=8)                                             39
      INTEGER FLAG_SOUP/0/,MAP_SOUP(2,LLEN_SOUP),IDNUM_SOUP(10)           39
      CHARACTER NAMES_SOUP(LLEN_SOUP)*12,COMMEN_SOUP(LLEN_SOUP)*40        39
      DATA IDNUM_SOUP/10*0/                                               39
      REAL BANK_SOUP(LEN_SOUP)                                            39
      CHARACTER*60 BPATH_SOUP/' '/                                        39
      SAVE FLAG_SOUP,MAP_SOUP,IDNUM_SOUP,NAMES_SOUP,COMMEN_SOUP,          39
     *BPATH_SOUP                                                          39
      DATA LINK_SOUP/0/,BANK_SOUP/LEN_SOUP*0/                             39
C *                                                                       39
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 40
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         42
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           44
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          46
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    48
      REAL XBUF,PHI,XBUF1,XBUF2                                           50
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     52
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          54
     *SHIELD_PHI(4)                                                       54
      REAL TRAPY,SSIDX,SSIRY                                              56
      INTEGER I_PHI                                                       58
      RADII(1)= 6.37                                                      60
      RADII(2)= 7.38                                                      61
      RADII(3)=10.38                                                      62
      RADII(4)=11.27                                                      63
      RADII(5)=14.19                                                      64
      RADII(6)=15.13                                                      65
      RAD_OFFSET=0.015                                                    66
      SHIELD_PHI(1)=9.0                                                   67
      SHIELD_PHI(2)=27.0                                                  68
      SHIELD_PHI(3)=45.0                                                  69
      SHIELD_PHI(4)=63.0                                                  70
C *                                                                       71
      CALL AGSBEGM(MODULE,IPRIN)                                          71
      IF (IPRIN.LT.0) RETURN                                              71
C Filling bank SVTG - BASIC SVT DIMENSIONS                                71
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   71
      BPATH_SVTG='SVTG*'                                                  71
      IDNUM_SVTG(1)=0                                                     72
C GEOMETRY VERSION                                                        72
      SVTG_VERSION=2                                                      72
      CSVTG_VERSION='GEOMETRY VERSION'                                    72
C DEFAULT ONE FOR SVTTGEO11.G                                             73
      SVTG_CONFIG=11                                                      73
      CSVTG_CONFIG='DEFAULT ONE FOR SVTTGEO11.G'                          73
C NUMBER OF SVT LAYERS (WAS 7)                                            74
      SVTG_NLAYER=7                                                       74
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         74
C STV INNERMOST RADIUS                                                    75
      SVTG_RSIZEMIN=4.100                                                 75
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               75
C STV OUTERMOST RADIUS                                                    76
      SVTG_RSIZEMAX=46.107                                                76
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               76
C SVT+FTPC LENGTH                                                         77
      SVTG_ZSIZEMAX=270                                                   77
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    77
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          78
      SVTG_ANGOFF=0                                                       78
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       78
C VERSIONING OF THE SHIELD                                                79
      SVTG_SUPPORTVER=1                                                   79
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         79
C VERSIONING OF THE SUPPORT CONE                                          80
      SVTG_CONEVER=1                                                      80
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      80
C WHETHER WE USE THE GEANT MANY OPTION                                    81
      SVTG_IFMANY=0                                                       81
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 81
C THE INDEX OF THE INNERMOST LAYER                                        82
      SVTG_NMIN=1                                                         82
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       82
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    83
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           83
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   83
C *                                                                       83
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    83
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           83
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   83
C *                                                                       84
      CALL AGSBEGM(MODULE,IPRIN)                                          84
      IF (IPRIN.LT.0) RETURN                                              84
C Filling bank SWCA - WAFER CARRIER                                       84
      BANK_TITLE='WAFER CARRIER'                                          84
      BPATH_SWCA='SWCA*'                                                  84
      IDNUM_SWCA(1)=0                                                     85
C GEOMETRY VERSION                                                        85
      SWCA_VERSION=1                                                      85
      CSWCA_VERSION='GEOMETRY VERSION'                                    85
C LADDER LENGTH                                                           86
      SWCA_LENGTH=56.04                                                   86
      CSWCA_LENGTH='LADDER LENGTH'                                        86
C WAFER WIDTH                                                             87
      SWCA_WAFERWID=6.305                                                 87
      CSWCA_WAFERWID='WAFER WIDTH'                                        87
C WAFER LENGTH                                                            88
      SWCA_WAFERLEN=6.305                                                 88
      CSWCA_WAFERLEN='WAFER LENGTH'                                       88
C WAFER THICKNESS                                                         89
      SWCA_WAFERTHK=0.0300                                                89
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    89
C 05/02/08 | 0.2 ! YF 05/01.08 0.0381 ! ROHA CELL PLUS GLUE THICKNESS     90
      SWCA_ROHATHK=0.0750                                                 90
      CSWCA_ROHATHK='05/02/08 | 0.2 ! YF 05/01.08 0.0381 ! ROHA CELL PLU  90
     *S GLUE THICKNESS'                                                   90
C WAFER CARRIER RAILS WIDTH                                               91
      SWCA_WAFCARWD=1.5                                                   91
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          91
C WAFER CARRIER THICKNESS                                                 92
      SWCA_WAFCARTH=0.0300                                                92
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            92
C INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY                      93
      SWCA_WAFERGAP=0.0                                                   93
      CSWCA_WAFERGAP='INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY  93
     *'                                                                   93
C DRIFT DIRECTION                                                         94
      SWCA_DRIFT=1                                                        94
      CSWCA_DRIFT='DRIFT DIRECTION'                                       94
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        95
      SWCA_STRUTLEN=1.0                                                   95
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   95
C DEFINE THE SENSITIVE SQUARE WIDTH (NEW)                                 96
      SWCA_SENSWID=6.000                                                  96
      CSWCA_SENSWID='DEFINE THE SENSITIVE SQUARE WIDTH (NEW)'             96
C DEFINE THE SENSITIVE SQUARE LENGTH (NEW)                                97
      SWCA_SENSLEN=6.000                                                  97
      CSWCA_SENSLEN='DEFINE THE SENSITIVE SQUARE LENGTH (NEW)'            97
C GIRTH (FOR LACK OF A BETTER WORD)                                       98
      SWCA_SENSGIR=5.775                                                  98
      CSWCA_SENSGIR='GIRTH (FOR LACK OF A BETTER WORD)'                   98
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    99
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           99
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   99
C *                                                                       99
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    99
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           99
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   99
C *                                                                       100
      CALL AGSBEGM(MODULE,IPRIN)                                          100
      IF (IPRIN.LT.0) RETURN                                              100
C Filling bank SSUP - SUPPORT STRUCTURES                                  100
      BANK_TITLE='SUPPORT STRUCTURES'                                     100
      BPATH_SSUP='SSUP*'                                                  100
      IDNUM_SSUP(1)=0                                                     101
C GEOMETRY VERSION                                                        101
      SSUP_VERSION=1                                                      101
      CSSUP_VERSION='GEOMETRY VERSION'                                    101
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            102
      SSUP_CABTHK=0.05                                                    102
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         102
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             103
      SSUP_HOSRMN=0.75                                                    103
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          103
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             104
      SSUP_HOSRMX=0.95                                                    104
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          104
C NUMBER OF WATER HOSES                                                   105
      SSUP_NHOSES=10                                                      105
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                105
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             106
      SSUP_WRPMYTHK=0.10                                                  106
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        106
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   107
      SSUP_WRPALTHK=0.01                                                  107
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              107
C SUPPORT CONE THICKNESS                                                  108
      SSUP_GRPHTHK=0.16                                                   108
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              108
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              109
      SSUP_CONE1ZMN=52.23                                                 109
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         109
C LENGTH OF SUPPORT RODS                                                  110
      SSUP_RODLEN=110.8                                                   110
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               110
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               111
      SSUP_RODDIST=17.5                                                   111
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           111
C ID OF CARBON SUPPORT RODS (APPROX)                                      112
      SSUP_RODID=2.5                                                      112
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    112
C OD OF CARBON SUPPORT RODS (APPROX)                                      113
      SSUP_RODOD=3.05                                                     113
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    113
C MINIMUM ID OF CONE 1                                                    114
      SSUP_CON1IDMN=15.67                                                 114
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               114
C MINIMUM ID OF CONE 3 (TBD)                                              115
      SSUP_CON3IDMN=21.67                                                 115
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         115
C MINIMUM ID OF CONE 4 (TBD)                                              116
      SSUP_CON4IDMN=37.4                                                  116
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         116
C MAXIMUM ID OF CONE 4 (TBD)                                              117
      SSUP_CON4IDMX=37.4                                                  117
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         117
C MAXIMUM Z OF CONE 3 (TBD)                                               118
      SSUP_CONE3ZMX=150.0                                                 118
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          118
C MAXIMUM Z OF CONE 4 (TBD)                                               119
      SSUP_CONE4ZMX=229.36                                                119
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          119
C THICKNESS OF AL BRACKETS                                                120
      SSUP_BRATHK=.2                                                      120
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             120
C (Z) THICKNESS OF END RING JOINING BRACKETS                              121
      SSUP_ERJTHK=.1                                                      121
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           121
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          122
      SSUP_ERJWID=2.07                                                    122
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       122
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            123
      SSUP_ERJLEN=5.19                                                    123
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         123
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    124
      SSUP_ERJZDIS=2.0                                                    124
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                124
C ERJ SCREW 1 X POSITION (RADIAL)                                         125
      SSUP_ERJ1X=0.31                                                     125
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       125
C ERJ SCREW 2 X POSITION                                                  126
      SSUP_ERJ2X=1.15                                                     126
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                126
C ERJ SCREW 2 Y POSITION                                                  127
      SSUP_ERJ2Y=0.72                                                     127
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                127
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   128
      SSUP_ERJRAD=10.80                                                   128
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                128
C ERJ SCREW DIAMETER                                                      129
      SSUP_ERJDIA=0.17                                                    129
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   129
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    130
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           130
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   130
C *                                                                       130
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    130
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           130
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   130
C *                                                                       131
      CALL AGSBEGM(MODULE,IPRIN)                                          131
      IF (IPRIN.LT.0) RETURN                                              131
C Filling bank SSUP - SUPPORT STRUCTURES                                  131
      BANK_TITLE='SUPPORT STRUCTURES'                                     131
      BPATH_SSUP='SSUP*'                                                  131
      IDNUM_SSUP(1)=0                                                     132
C GEOMETRY VERSION                                                        132
      SSUP_VERSION=2                                                      132
      CSSUP_VERSION='GEOMETRY VERSION'                                    132
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            133
      SSUP_CABTHK=0.21                                                    133
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         133
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             134
      SSUP_HOSRMN=0.75                                                    134
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          134
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             135
      SSUP_HOSRMX=0.95                                                    135
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          135
C NUMBER OF WATER HOSES                                                   136
      SSUP_NHOSES=10                                                      136
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                136
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             137
      SSUP_WRPMYTHK=0.10                                                  137
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        137
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   138
      SSUP_WRPALTHK=0.01                                                  138
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              138
C SUPPORT CONE THICKNESS                                                  139
      SSUP_GRPHTHK=0.16                                                   139
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              139
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              140
      SSUP_CONE1ZMN=52.23                                                 140
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         140
C LENGTH OF SUPPORT RODS                                                  141
      SSUP_RODLEN=110.8                                                   141
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               141
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               142
      SSUP_RODDIST=17.5                                                   142
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           142
C ID OF CARBON SUPPORT RODS (APPROX)                                      143
      SSUP_RODID=2.5                                                      143
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    143
C OD OF CARBON SUPPORT RODS (APPROX)                                      144
      SSUP_RODOD=3.05                                                     144
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    144
C MINIMUM ID OF CONE 1                                                    145
      SSUP_CON1IDMN=15.67                                                 145
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               145
C MINIMUM ID OF CONE 3 (TBD)                                              146
      SSUP_CON3IDMN=21.67                                                 146
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         146
C MINIMUM ID OF CONE 4 (TBD)                                              147
      SSUP_CON4IDMN=37.4                                                  147
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         147
C MAXIMUM ID OF CONE 4 (TBD)                                              148
      SSUP_CON4IDMX=37.4                                                  148
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         148
C MAXIMUM Z OF CONE 3 (TBD)                                               149
      SSUP_CONE3ZMX=150.0                                                 149
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          149
C MAXIMUM Z OF CONE 4 (TBD)                                               150
      SSUP_CONE4ZMX=229.36                                                150
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          150
C THICKNESS OF AL BRACKETS                                                151
      SSUP_BRATHK=.2                                                      151
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             151
C (Z) THICKNESS OF END RING JOINING BRACKETS                              152
      SSUP_ERJTHK=.1                                                      152
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           152
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          153
      SSUP_ERJWID=2.07                                                    153
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       153
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            154
      SSUP_ERJLEN=5.19                                                    154
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         154
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    155
      SSUP_ERJZDIS=2.0                                                    155
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                155
C ERJ SCREW 1 X POSITION (RADIAL)                                         156
      SSUP_ERJ1X=0.31                                                     156
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       156
C ERJ SCREW 2 X POSITION                                                  157
      SSUP_ERJ2X=1.15                                                     157
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                157
C ERJ SCREW 2 Y POSITION                                                  158
      SSUP_ERJ2Y=0.72                                                     158
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                158
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   159
      SSUP_ERJRAD=10.80                                                   159
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                159
C ERJ SCREW DIAMETER                                                      160
      SSUP_ERJDIA=0.17                                                    160
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   160
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    161
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           161
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   161
C *                                                                       161
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    161
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           161
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   161
C *                                                                       162
      CALL AGSBEGM(MODULE,IPRIN)                                          162
      IF (IPRIN.LT.0) RETURN                                              162
C Filling bank SSUB - BEAMPIPE SUPPORT                                    162
      BANK_TITLE='BEAMPIPE SUPPORT'                                       162
      BPATH_SSUB='SSUB*'                                                  162
      IDNUM_SSUB(1)=0                                                     163
C GEOMETRY VERSION                                                        163
      SSUB_VERSION=1                                                      163
      CSSUB_VERSION='GEOMETRY VERSION'                                    163
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  164
      SSUB_KMOUNTID=31.34                                                 164
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             164
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  165
      SSUB_KMOUNTOD=38.96                                                 165
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             165
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    166
      SSUB_KMNTTHK=0.64                                                   166
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                166
C OD OF CUTOUT IN KINEMATIC MOUNT                                         167
      SSUB_KMCUTOD=18.31                                                  167
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     167
C ID OF CUTOUT IN KINEMATIC MOUNT                                         168
      SSUB_KMCUTID=14                                                     168
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     168
C OPENING ANGLE OF CUTOUT                                                 169
      SSUB_KMCUTOA=38                                                     169
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             169
C OFFSET OF CUTOUT CENTER FROM AXIS                                       170
      SSUB_KMCUTOFF=26.58                                                 170
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  170
C ID OF BEAMPIPE SUPPORT RING                                             171
      SSUB_SRINGID=8.47                                                   171
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         171
C OD OF BEAMPIPE SUPPORT RING                                             172
      SSUB_SRINGOD=12.78                                                  172
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         172
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     173
      SSUB_SRINGTHK=0.64                                                  173
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                173
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     174
      SSUB_SRCUTPHI=38                                                    174
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                174
C SUPPORT RING CUTOUT WIDTH                                               175
      SSUB_SRCUTWID=3.63                                                  175
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          175
C SUPPORT RING CUTOUT DEPTH                                               176
      SSUB_SRCUTOUT=5.08                                                  176
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          176
C SUPPORT RING CUTOUT START                                               177
      SSUB_SRCUTIN=3.5                                                    177
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           177
C SUPPORT ROLLER ID                                                       178
      SSUB_SROLLID=0.2                                                    178
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   178
C SUPPORT ROLLER OD                                                       179
      SSUB_SROLLOD=0.62                                                   179
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   179
C SUPPORT ROLLER LENGTH                                                   180
      SSUB_SROLLLEN=2.54                                                  180
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              180
C SUPPORT ROLLER AXIS LENGTH                                              181
      SSUB_SWIRELEN=5.08                                                  181
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         181
C MOUNTING BLOCK HEIGHT                                                   182
      SSUB_MBLKHGH=0.97                                                   182
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               182
C OUTER MOUNTING BLOCK WIDTH                                              183
      SSUB_MBLKOWID=2.54                                                  183
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         183
C OUTER MOUNTING BLOCK LENGTH                                             184
      SSUB_MBLKOLEN=1.27                                                  184
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        184
C INNER MOUNTING BLOCK WIDTH                                              185
      SSUB_MBLKIWID=3.175                                                 185
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         185
C INNER MOUNTING BLOCK LENGTH                                             186
      SSUB_MBLKILEN=1.27                                                  186
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        186
C OUTER MOUNTING BLOCK AT RADIUS                                          187
      SSUB_MBLKORAD=17.4                                                  187
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     187
C INNER MOUNTING BLOCK AT RADIUS                                          188
      SSUB_MBLKIRAD=5.42                                                  188
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     188
C MOUNTING SUPPORT ROD DIAMETER                                           189
      SSUB_MRODDIA=0.32                                                   189
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       189
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    190
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           190
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   190
C *                                                                       190
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    190
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           190
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   190
C *                                                                       191
      CALL AGSBEGM(MODULE,IPRIN)                                          191
      IF (IPRIN.LT.0) RETURN                                              191
C Filling bank SERG - END RINGS                                           191
      BANK_TITLE='END RINGS'                                              191
      BPATH_SERG='SERG*'                                                  191
      IDNUM_SERG(1)=0                                                     192
C GEOMETRY VERSION                                                        192
      SERG_VERSION=1                                                      192
      CSERG_VERSION='GEOMETRY VERSION'                                    192
C INNER END RING TUBE MAXIMUM RADIUS                                      193
      SERG_IRNGTRMX=9.703                                                 193
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 193
C INNER END RING POLYGON MINIMUM RADIUS                                   194
      SERG_IRNGPRMN=7.671                                                 194
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              194
C OUTER END RING MINIMUM RADIUS                                           195
      SERG_ORNGRMIN=11.900                                                195
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      195
C OUTER END RING MAXIMUM RADIUS                                           196
      SERG_ORNGRMAX=13.805                                                196
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      196
C END RING THICKNESS                                                      197
      SERG_ENDRNGTH=0.2                                                   197
      CSERG_ENDRNGTH='END RING THICKNESS'                                 197
C MINIMUM Z FOR END RINGS                                                 198
      SERG_ENDRNGZM=23.01                                                 198
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            198
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    199
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           199
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   199
C *                                                                       199
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    199
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           199
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   199
C *                                                                       200
      CALL AGSBEGM(MODULE,IPRIN)                                          200
      IF (IPRIN.LT.0) RETURN                                              200
C Filling bank SWAM - WATER MANIFOLD                                      200
      BANK_TITLE='WATER MANIFOLD'                                         200
      BPATH_SWAM='SWAM*'                                                  200
      IDNUM_SWAM(1)=0                                                     201
C GEOMETRY VERSION                                                        201
      SWAM_VERSION=1                                                      201
      CSWAM_VERSION='GEOMETRY VERSION'                                    201
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                202
      SWAM_ZMIN=33.9                                                      202
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               202
C WATER MANIFOLD FULL LENGTH                                              203
      SWAM_LEN=16.0                                                       203
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              203
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         204
      SWAM_RMIN=15.24                                                     204
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        204
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         205
      SWAM_RMAX=16.83                                                     205
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        205
C TRANSITION BOARD THICKNESS                                              206
      SWAM_TBRDTHK=0.1                                                    206
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          206
C WATER MANIFOLD WALL THICKNESS                                           207
      SWAM_WALLTHK=0.1                                                    207
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       207
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    208
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           208
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   208
C *                                                                       208
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    208
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           208
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   208
C *                                                                       209
      CALL AGSBEGM(MODULE,IPRIN)                                          209
      IF (IPRIN.LT.0) RETURN                                              209
C Filling bank SELC - ELECTRONICS CARRIER DATA                            209
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               209
      BPATH_SELC='SELC*'                                                  209
      IDNUM_SELC(1)=0                                                     210
C GEOMETRY VERSION                                                        210
      SELC_VERSION=1                                                      210
      CSELC_VERSION='GEOMETRY VERSION'                                    210
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              211
      SELC_BETHK=0.0500                                                   211
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            211
C THICKNESS OF WATER CHANNEL                                              212
      SELC_WATTHK=0.0750                                                  212
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           212
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    213
      SELC_BEOTHK=0.0500                                                  213
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 213
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                214
      SELC_DYETHK=0.0340                                                  214
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             214
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                215
      SELC_DYEWID=0.3                                                     215
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             215
C SPACE IC CHIPS                                                          216
      SELC_DYESPC=0.1                                                     216
      CSELC_DYESPC='SPACE IC CHIPS'                                       216
C ELECTRONICS CARRIER WIDTH                                               217
      SELC_ELCAWID=2.0                                                    217
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           217
C THICKNESS OF AG-PD CONDUCTOR                                            218
      SELC_AGPDTHK=0.0030                                                 218
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        218
C 05/02/08 ! 0.1500 ! YF 05/01/08 0.0150 ! THICKNESS OF GLASS INSULATOR   219
      SELC_GLASSTHK=0.2000                                                219
      CSELC_GLASSTHK='05/02/08 ! 0.1500 ! YF 05/01/08 0.0150 ! THICKNESS  219
     * OF GLASS INSULATOR'                                                219
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        220
      SELC_CABTHK=0.0033                                                  220
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     220
C CABLE WIDTH                                                             221
      SELC_CABWID=0.6                                                     221
      CSELC_CABWID='CABLE WIDTH'                                          221
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    222
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           222
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   222
C *                                                                       222
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    222
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           222
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   222
C *                                                                       223
      CALL AGSBEGM(MODULE,IPRIN)                                          223
      IF (IPRIN.LT.0) RETURN                                              223
C Filling bank SVTL - SINGLE LAYER DATA                                   223
      BANK_TITLE='SINGLE LAYER DATA'                                      223
      BPATH_SVTL='SVTL*'                                                  223
      IDNUM_SVTL(1)=0                                                     224
C LAYER NUMBER                                                            224
      SVTL_LAYER=1                                                        224
      CSVTL_LAYER='LAYER NUMBER'                                          224
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 225
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     225
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              225
C NUMBER OF LADDER                                                        226
      SVTL_NLADDER=4                                                      226
      CSVTL_NLADDER='NUMBER OF LADDER'                                    226
C NUMBER OF WAFERS                                                        227
      SVTL_NWAFER=4                                                       227
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     227
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        228
      SVTL_BAREEDGE=1.0                                                   228
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   228
C PCB LENGTH                                                              229
      SVTL_PCBLEN=14.9                                                    229
      CSVTL_PCBLEN='PCB LENGTH'                                           229
C PCB WIDTH                                                               230
      SVTL_PCBWIDTH=6.3                                                   230
      CSVTL_PCBWIDTH='PCB WIDTH'                                          230
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            231
      SVTL_PCBTHK=0.1                                                     231
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  231
     *R COPPER'                                                           231
C GAP BETWEEN THE PCB AND THE SI WAFER                                    232
      SVTL_PCBGAP=0.2                                                     232
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 232
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    233
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           233
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   233
C *                                                                       233
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    233
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           233
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   233
C *                                                                       234
      CALL AGSBEGM(MODULE,IPRIN)                                          234
      IF (IPRIN.LT.0) RETURN                                              234
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             234
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                234
      BPATH_SVTL='SVTL*'                                                  234
      IDNUM_SVTL(1)=0                                                     235
C LAYER NUMBER                                                            235
      SVTL_LAYER=2                                                        235
      CSVTL_LAYER='LAYER NUMBER'                                          235
C LAYER RADIUS                                                            236
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     236
      CSVTL_RADIUS='LAYER RADIUS'                                         236
C PCB LENGTH                                                              237
      SVTL_PCBLEN=14.9                                                    237
      CSVTL_PCBLEN='PCB LENGTH'                                           237
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    238
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           238
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   238
C *                                                                       238
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    238
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           238
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   238
C *                                                                       239
      CALL AGSBEGM(MODULE,IPRIN)                                          239
      IF (IPRIN.LT.0) RETURN                                              239
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             239
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                239
      BPATH_SVTL='SVTL*'                                                  239
      IDNUM_SVTL(1)=0                                                     240
C LAYER NUMBER                                                            240
      SVTL_LAYER=3                                                        240
      CSVTL_LAYER='LAYER NUMBER'                                          240
C LAYER RADIUS                                                            241
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     241
      CSVTL_RADIUS='LAYER RADIUS'                                         241
C NUMBER OF LADDER                                                        242
      SVTL_NLADDER=6                                                      242
      CSVTL_NLADDER='NUMBER OF LADDER'                                    242
C NUMBER OF WAFERS                                                        243
      SVTL_NWAFER=6                                                       243
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     243
C PCB LENGTH                                                              244
      SVTL_PCBLEN=7.5                                                     244
      CSVTL_PCBLEN='PCB LENGTH'                                           244
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    245
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           245
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   245
C *                                                                       245
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    245
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           245
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   245
C *                                                                       246
      CALL AGSBEGM(MODULE,IPRIN)                                          246
      IF (IPRIN.LT.0) RETURN                                              246
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             246
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                246
      BPATH_SVTL='SVTL*'                                                  246
      IDNUM_SVTL(1)=0                                                     247
C LAYER NUMBER                                                            247
      SVTL_LAYER=4                                                        247
      CSVTL_LAYER='LAYER NUMBER'                                          247
C LAYER RADIUS                                                            248
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     248
      CSVTL_RADIUS='LAYER RADIUS'                                         248
C PCB LENGTH                                                              249
      SVTL_PCBLEN=7.5                                                     249
      CSVTL_PCBLEN='PCB LENGTH'                                           249
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    250
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           250
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   250
C *                                                                       250
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    250
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           250
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   250
C *                                                                       251
      CALL AGSBEGM(MODULE,IPRIN)                                          251
      IF (IPRIN.LT.0) RETURN                                              251
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             251
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                251
      BPATH_SVTL='SVTL*'                                                  251
      IDNUM_SVTL(1)=0                                                     252
C LAYER NUMBER                                                            252
      SVTL_LAYER=5                                                        252
      CSVTL_LAYER='LAYER NUMBER'                                          252
C LAYER RADIUS                                                            253
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     253
      CSVTL_RADIUS='LAYER RADIUS'                                         253
C NUMBER OF LADDER                                                        254
      SVTL_NLADDER=8                                                      254
      CSVTL_NLADDER='NUMBER OF LADDER'                                    254
C NUMBER OF WAFERS                                                        255
      SVTL_NWAFER=7                                                       255
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     255
C PCB LENGTH                                                              256
      SVTL_PCBLEN=4.4                                                     256
      CSVTL_PCBLEN='PCB LENGTH'                                           256
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    257
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           257
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   257
C *                                                                       257
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    257
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           257
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   257
C *                                                                       258
      CALL AGSBEGM(MODULE,IPRIN)                                          258
      IF (IPRIN.LT.0) RETURN                                              258
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             258
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                258
      BPATH_SVTL='SVTL*'                                                  258
      IDNUM_SVTL(1)=0                                                     259
C LAYER NUMBER                                                            259
      SVTL_LAYER=6                                                        259
      CSVTL_LAYER='LAYER NUMBER'                                          259
C LAYER RADIUS                                                            260
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     260
      CSVTL_RADIUS='LAYER RADIUS'                                         260
C PCB LENGTH                                                              261
      SVTL_PCBLEN=4.4                                                     261
      CSVTL_PCBLEN='PCB LENGTH'                                           261
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    262
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           262
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   262
C *                                                                       262
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    262
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           262
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   262
C *                                                                       263
      CALL AGSBEGM(MODULE,IPRIN)                                          263
      IF (IPRIN.LT.0) RETURN                                              263
C Filling bank SSLD - SHIELDING PARAMETERS                                263
      BANK_TITLE='SHIELDING PARAMETERS'                                   263
      BPATH_SSLD='SSLD*'                                                  263
      IDNUM_SSLD(1)=0                                                     264
C GEOMETRY VERSION                                                        264
      SSLD_VERSION=1                                                      264
      CSSLD_VERSION='GEOMETRY VERSION'                                    264
C INNER SHIELD CYLINDER, INNER RADIUS                                     265
      SSLD_SINRINN=5                                                      265
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 265
C INNER SHIELD CYLINDER, OUTER RADIUS                                     266
      SSLD_SINROUT=5.008                                                  266
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 266
C INNER SHIELD CYLINDER, HALF LENGTH                                      267
      SSLD_SINLEN=53.5                                                    267
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   267
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                268
      SSLD_SSEPRINN=22                                                    268
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           268
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                269
      SSLD_SSEPROUT=22.018                                                269
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           269
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 270
      SSLD_SSEPLEN=55.4                                                   270
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             270
C OUTER SHIELD CYLINDER, INNER RADIUS                                     271
      SSLD_SOUTRINN=29.5                                                  271
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                271
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     272
      SSLD_SOUTROUT=29.52                                                 272
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                272
C OUTER SHIELD CYLINDER, HALF LENGTH                                      273
      SSLD_SOUTLEN=65.4                                                   273
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  273
C ALUMINUM SHIELD MESH INNER DIAMETER                                     274
      SSLD_ALMESHID=9.7                                                   274
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                274
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     275
      SSLD_ALMESHOD=44                                                    275
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                275
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                276
      SSLD_ALMSHTHK=0.03                                                  276
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           276
C ALUMINUM SHIELD MESH Z POSITION                                         277
      SSLD_ALMSHPOS=53.5                                                  277
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    277
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    278
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           278
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   278
C *                                                                       278
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    278
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           278
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   278
C *                                                                       279
      CALL AGSBEGM(MODULE,IPRIN)                                          279
      IF (IPRIN.LT.0) RETURN                                              279
C Filling bank SSLD - SHIELDING PARAMETERS                                279
      BANK_TITLE='SHIELDING PARAMETERS'                                   279
      BPATH_SSLD='SSLD*'                                                  279
      IDNUM_SSLD(1)=0                                                     280
C GEOMETRY VERSION                                                        280
      SSLD_VERSION=2                                                      280
      CSSLD_VERSION='GEOMETRY VERSION'                                    280
C INNER SHIELD CYLINDER, INNER RADIUS                                     281
      SSLD_SINRINN=5.9                                                    281
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 281
C INNER SHIELD CYLINDER, OUTER RADIUS                                     282
      SSLD_SINROUT=5.908                                                  282
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 282
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    283
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           283
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   283
C *                                                                       283
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    283
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           283
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   283
C *                                                                       284
      CALL AGSBEGM(MODULE,IPRIN)                                          284
      IF (IPRIN.LT.0) RETURN                                              284
C Filling bank SSLD - SHIELDING PARAMETERS                                284
      BANK_TITLE='SHIELDING PARAMETERS'                                   284
      BPATH_SSLD='SSLD*'                                                  284
      IDNUM_SSLD(1)=0                                                     285
C GEOMETRY VERSION                                                        285
      SSLD_VERSION=3                                                      285
      CSSLD_VERSION='GEOMETRY VERSION'                                    285
C INNER SHIELD CYLINDER, INNER RADIUS                                     286
      SSLD_SINRINN=5                                                      286
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 286
C INNER SHIELD CYLINDER, OUTER RADIUS                                     287
      SSLD_SINROUT=5.008                                                  287
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 287
C INNER SHIELD CYLINDER, HALF LENGTH                                      288
      SSLD_SINLEN=53.5                                                    288
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   288
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                289
      SSLD_SSEPRINN=22                                                    289
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           289
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                290
      SSLD_SSEPROUT=22.018                                                290
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           290
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 291
      SSLD_SSEPLEN=55.4                                                   291
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             291
C OUTER SHIELD CYLINDER, INNER RADIUS                                     292
      SSLD_SOUTRINN=31.80                                                 292
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                292
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     293
      SSLD_SOUTROUT=31.82                                                 293
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                293
C OUTER SHIELD CYLINDER, HALF LENGTH                                      294
      SSLD_SOUTLEN=65.4                                                   294
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  294
C ALUMINUM SHIELD MESH INNER DIAMETER                                     295
      SSLD_ALMESHID=9.7                                                   295
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                295
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     296
      SSLD_ALMESHOD=44                                                    296
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                296
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                297
      SSLD_ALMSHTHK=0.03                                                  297
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           297
C ALUMINUM SHIELD MESH Z POSITION                                         298
      SSLD_ALMSHPOS=53.5                                                  298
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    298
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    299
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           299
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   299
C *                                                                       299
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    299
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           299
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   299
C *                                                                       300
      DO 5011 I=1,4                                                       300
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 301
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 302
5011  CONTINUE                                                            303
5012  CONTINUE                                                            303
C *                                                                       304
      CALL AGSBEGM(MODULE,IPRIN)                                          304
      IF (IPRIN.LT.0) RETURN                                              304
C Filling bank SCBP - CABLING                                             304
      BANK_TITLE='CABLING'                                                304
      BPATH_SCBP='SCBP*'                                                  304
      IDNUM_SCBP(1)=0                                                     305
C LAYER                                                                   305
      SCBP_LAYER=1                                                        305
      CSCBP_LAYER='LAYER'                                                 305
C LENGTH                                                                  306
      SCBP_LEN=1.85                                                       306
      CSCBP_LEN='LENGTH'                                                  306
C MIN RADIUS CLOSER TO WAFERS                                             307
      SCBP_RMIN1=RAD_CONES_IN(1)                                          307
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           307
C MIN RADIUS FURTHER FROM WAFERS                                          308
      SCBP_RMIN2=RAD_CONES_OUT(1)                                         308
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        308
C VOLUME OF COPPER, LV+HV CABLES                                          309
      SCBP_VOL=7.24+3.21                                                  309
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          309
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    310
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           310
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   310
C *                                                                       310
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    310
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           310
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   310
C *                                                                       311
      CALL AGSBEGM(MODULE,IPRIN)                                          311
      IF (IPRIN.LT.0) RETURN                                              311
C Filling bank SCBP - CABLING                                             311
      BANK_TITLE='CABLING'                                                311
      BPATH_SCBP='SCBP*'                                                  311
      IDNUM_SCBP(1)=0                                                     312
C LAYER                                                                   312
      SCBP_LAYER=2                                                        312
      CSCBP_LAYER='LAYER'                                                 312
C MIN RADIUS CLOSER TO WAFERS                                             313
      SCBP_RMIN1=RAD_CONES_IN(2)                                          313
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           313
C MIN RADIUS FURTHER FROM WAFERS                                          314
      SCBP_RMIN2=RAD_CONES_OUT(2)                                         314
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        314
C VOLUME OF COPPER, LV+HV CABLES                                          315
      SCBP_VOL=15.54+5.7                                                  315
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          315
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    316
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           316
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   316
C *                                                                       316
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    316
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           316
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   316
C *                                                                       317
      CALL AGSBEGM(MODULE,IPRIN)                                          317
      IF (IPRIN.LT.0) RETURN                                              317
C Filling bank SCBP - CABLING                                             317
      BANK_TITLE='CABLING'                                                317
      BPATH_SCBP='SCBP*'                                                  317
      IDNUM_SCBP(1)=0                                                     318
C LAYER                                                                   318
      SCBP_LAYER=3                                                        318
      CSCBP_LAYER='LAYER'                                                 318
C MIN RADIUS CLOSER TO WAFERS                                             319
      SCBP_RMIN1=RAD_CONES_IN(3)                                          319
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           319
C MIN RADIUS FURTHER FROM WAFERS                                          320
      SCBP_RMIN2=RAD_CONES_OUT(3)                                         320
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        320
C VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE                   321
      SCBP_VOL=4.05+2.02+3.67+1.69                                        321
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE'   321
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    322
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           322
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   322
C *                                                                       322
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    322
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           322
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   322
C *                                                                       323
      CALL AGSBEGM(MODULE,IPRIN)                                          323
      IF (IPRIN.LT.0) RETURN                                              323
C Filling bank SCBP - CABLING                                             323
      BANK_TITLE='CABLING'                                                323
      BPATH_SCBP='SCBP*'                                                  323
      IDNUM_SCBP(1)=0                                                     324
C LAYER (FORMER 5TH)                                                      324
      SCBP_LAYER=4                                                        324
      CSCBP_LAYER='LAYER (FORMER 5TH)'                                    324
C MIN RADIUS CLOSER TO WAFERS                                             325
      SCBP_RMIN1=RAD_CONES_IN(4)                                          325
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           325
C MIN RADIUS FURTHER FROM WAFERS                                          326
      SCBP_RMIN2=RAD_CONES_OUT(4)                                         326
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        326
C VOLUME OF COPPER, LV+HV CABLES                                          327
      SCBP_VOL=6.95+2.43                                                  327
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          327
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    328
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           328
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   328
C *                                                                       328
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    328
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           328
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   328
C *                                                                       329
      CALL AGSBEGM(MODULE,IPRIN)                                          329
      IF (IPRIN.LT.0) RETURN                                              329
C Filling bank SFEP - WATER FEED                                          329
      BANK_TITLE='WATER FEED'                                             329
      BPATH_SFEP='SFEP*'                                                  329
      IDNUM_SFEP(1)=0                                                     330
C LAYER                                                                   330
      SFEP_LAYER=1                                                        330
      CSFEP_LAYER='LAYER'                                                 330
C LENGTH                                                                  331
      SFEP_LEN=1.85                                                       331
      CSFEP_LEN='LENGTH'                                                  331
C MIN RADIUS CLOSER TO WAFERS                                             332
      SFEP_RMIN1=0.5*(RAD_CONES_IN(1) +RAD_CONES_IN(2))                   332
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           332
C MIN RADIUS FURTHER FROM WAFERS                                          333
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(1)+RAD_CONES_OUT(2))                  333
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        333
C VOLUME OF WATER                                                         334
      SFEP_VOL=16.0                                                       334
      CSFEP_VOL='VOLUME OF WATER'                                         334
C VOLUME OF PLASTIC                                                       335
      SFEP_VOLPLAST=38.4                                                  335
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  335
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    336
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           336
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   336
C *                                                                       336
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    336
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           336
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   336
C *                                                                       337
      CALL AGSBEGM(MODULE,IPRIN)                                          337
      IF (IPRIN.LT.0) RETURN                                              337
C Filling bank SFEP - WATER FEED                                          337
      BANK_TITLE='WATER FEED'                                             337
      BPATH_SFEP='SFEP*'                                                  337
      IDNUM_SFEP(1)=0                                                     338
C LAYER                                                                   338
      SFEP_LAYER=2                                                        338
      CSFEP_LAYER='LAYER'                                                 338
C MIN RADIUS CLOSER TO WAFERS                                             339
      SFEP_RMIN1=0.5*(RAD_CONES_IN(2) +RAD_CONES_IN(3))                   339
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           339
C MIN RADIUS FURTHER FROM WAFERS                                          340
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(2)+RAD_CONES_OUT(3))                  340
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        340
C VOLUME OF WATER                                                         341
      SFEP_VOL=24.0                                                       341
      CSFEP_VOL='VOLUME OF WATER'                                         341
C VOLUME OF PLASTIC                                                       342
      SFEP_VOLPLAST=57.6                                                  342
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
      SFEP_LAYER=3                                                        345
      CSFEP_LAYER='LAYER'                                                 345
C MIN RADIUS CLOSER TO WAFERS                                             346
      SFEP_RMIN1=0.5*(RAD_CONES_IN(3) +RAD_CONES_IN(4))                   346
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           346
C MIN RADIUS FURTHER FROM WAFERS                                          347
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(3)+RAD_CONES_OUT(4))                  347
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        347
C VOLUME OF WATER                                                         348
      SFEP_VOL=32                                                         348
      CSFEP_VOL='VOLUME OF WATER'                                         348
C VOLUME OF PLASTIC                                                       349
      SFEP_VOLPLAST=76.8                                                  349
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
C Filling bank SWCX - SEGMENTS OF THE WATER DISTRIBUTION PIPES            351
      BANK_TITLE='SEGMENTS OF THE WATER DISTRIBUTION PIPES'               351
      BPATH_SWCX='SWCX*'                                                  351
      IDNUM_SWCX(1)=0                                                     352
C VERSION                                                                 352
      SWCX_LAYER=1                                                        352
      CSWCX_LAYER='VERSION'                                               352
C OF THE RING IN THE Z DIRECTION                                          353
      SWCX_LENGTH=2.8                                                     353
      CSWCX_LENGTH='OF THE RING IN THE Z DIRECTION'                       353
C THICKNESS OF THE MOTHER LAYER                                           354
      SWCX_DR=0.72                                                        354
      CSWCX_DR='THICKNESS OF THE MOTHER LAYER'                            354
C INNER PLASTIC TUBE RADIUS                                               355
      SWCX_RAD=0.2                                                        355
      CSWCX_RAD='INNER PLASTIC TUBE RADIUS'                               355
C FROM THE EDGE OF THE LADDER SUPPORT, INWARD                             356
      SWCX_OFFSET=-2.0                                                    356
      CSWCX_OFFSET='FROM THE EDGE OF THE LADDER SUPPORT, INWARD'          356
C RADIAL OFFSET                                                           357
      SWCX_ROFFSET=1.0                                                    357
      CSWCX_ROFFSET='RADIAL OFFSET'                                       357
C THICKNESS OF THE PLASTIC PIPE WALL                                      358
      SWCX_WALL=0.16                                                      358
      CSWCX_WALL='THICKNESS OF THE PLASTIC PIPE WALL'                     358
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    359
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           359
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   359
C *                                                                       359
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    359
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           359
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   359
C *                                                                       360
      CALL AGSBEGM(MODULE,IPRIN)                                          360
      IF (IPRIN.LT.0) RETURN                                              360
C Filling bank SOUP - MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS      360
      BANK_TITLE='MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS'         360
      BPATH_SOUP='SOUP*'                                                  360
      IDNUM_SOUP(1)=0                                                     361
C VERSION                                                                 361
      SOUP_VERSION=1                                                      361
      CSOUP_VERSION='VERSION'                                             361
C LENGTH                                                                  362
      SOUP_LENGTH=82.5                                                    362
      CSOUP_LENGTH='LENGTH'                                               362
C OUTER RADIUS OF THE SHIELD                                              363
      SOUP_ROUT=19.41                                                     363
      CSOUP_ROUT='OUTER RADIUS OF THE SHIELD'                             363
C DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF THE MOTHER)     364
      SOUP_DR=0.711                                                       364
      CSOUP_DR='DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF   364
     *THE MOTHER)'                                                        364
C STARTING ANGLE OF THE MOTHER                                            365
      SOUP_PHI1=0.0                                                       365
      CSOUP_PHI1='STARTING ANGLE OF THE MOTHER'                           365
C FINISHING ANGLE OF THE MOTHER                                           366
      SOUP_PHI2=70.0                                                      366
      CSOUP_PHI2='FINISHING ANGLE OF THE MOTHER'                          366
C OUTER DIAM OF THE CARBON TUBE                                           367
      SOUP_DIAMOUT=0.711                                                  367
      CSOUP_DIAMOUT='OUTER DIAM OF THE CARBON TUBE'                       367
C INNER DIAM OF THE CARBON TUBE                                           368
      SOUP_DIAMIN=0.620                                                   368
      CSOUP_DIAMIN='INNER DIAM OF THE CARBON TUBE'                        368
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    369
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           369
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   369
C *                                                                       369
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    369
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           369
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   369
C use default SVTG bank                                                   370
      BPATH_SVTG='SVTG*'                                                  370
      IDNUM_SVTG(1)=0                                                     370
      CALL AGDGETP('SVTTGEO11','*',-999,'SVTG',' ',BANK_SVTG,             370
     *BPATH_SVTG,IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,     370
     *LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                                      370
C use default SWCA bank                                                   371
      BPATH_SWCA='SWCA*'                                                  371
      IDNUM_SWCA(1)=0                                                     371
      CALL AGDGETP('SVTTGEO11','*',-999,'SWCA',' ',BANK_SWCA,             371
     *BPATH_SWCA,IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,     371
     *LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                                      371
C use default SELC bank                                                   372
      BPATH_SELC='SELC*'                                                  372
      IDNUM_SELC(1)=0                                                     372
      CALL AGDGETP('SVTTGEO11','*',-999,'SELC',' ',BANK_SELC,             372
     *BPATH_SELC,IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,     372
     *LLEN_SELC,LINK_SELC,FLAG_SELC)                                      372
C use SSUP bank                                                           373
      BPATH_SSUP='SSUP*'                                                  373
      IDNUM_SSUP(1)=0                                                     373
      SSUP_VERSION=SVTG_CONEVER                                           373
      CALL AGDGETP('SVTTGEO11','*',-999,'SSUP',NSSUP_VERSION,             373
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        373
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   373
C use default SSUB bank                                                   374
      BPATH_SSUB='SSUB*'                                                  374
      IDNUM_SSUB(1)=0                                                     374
      CALL AGDGETP('SVTTGEO11','*',-999,'SSUB',' ',BANK_SSUB,             374
     *BPATH_SSUB,IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,     374
     *LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                                      374
C use default SERG bank                                                   375
      BPATH_SERG='SERG*'                                                  375
      IDNUM_SERG(1)=0                                                     375
      CALL AGDGETP('SVTTGEO11','*',-999,'SERG',' ',BANK_SERG,             375
     *BPATH_SERG,IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,     375
     *LLEN_SERG,LINK_SERG,FLAG_SERG)                                      375
C use default SWAM bank                                                   376
      BPATH_SWAM='SWAM*'                                                  376
      IDNUM_SWAM(1)=0                                                     376
      CALL AGDGETP('SVTTGEO11','*',-999,'SWAM',' ',BANK_SWAM,             376
     *BPATH_SWAM,IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,     376
     *LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                                      376
C use default SELC bank                                                   377
      BPATH_SELC='SELC*'                                                  377
      IDNUM_SELC(1)=0                                                     377
      CALL AGDGETP('SVTTGEO11','*',-999,'SELC',' ',BANK_SELC,             377
     *BPATH_SELC,IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,     377
     *LLEN_SELC,LINK_SELC,FLAG_SELC)                                      377
C use SSLD bank                                                           378
      BPATH_SSLD='SSLD*'                                                  378
      IDNUM_SSLD(1)=0                                                     378
      SSLD_VERSION=SVTG_SUPPORTVER                                        378
      CALL AGDGETP('SVTTGEO11','*',-999,'SSLD',NSSLD_VERSION,             378
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        378
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   378
C use default SCBP bank                                                   379
      BPATH_SCBP='SCBP*'                                                  379
      IDNUM_SCBP(1)=0                                                     379
      CALL AGDGETP('SVTTGEO11','*',-999,'SCBP',' ',BANK_SCBP,             379
     *BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,     379
     *LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                                      379
C use default SWCX bank                                                   380
      BPATH_SWCX='SWCX*'                                                  380
      IDNUM_SWCX(1)=0                                                     380
      CALL AGDGETP('SVTTGEO11','*',-999,'SWCX',' ',BANK_SWCX,             380
     *BPATH_SWCX,IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,     380
     *LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                                      380
C use default SOUP bank                                                   381
      BPATH_SOUP='SOUP*'                                                  381
      IDNUM_SOUP(1)=0                                                     381
      CALL AGDGETP('SVTTGEO11','*',-999,'SOUP',' ',BANK_SOUP,             381
     *BPATH_SOUP,IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,     381
     *LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                                      381
C Check SVTG_SUPPORTVER==2                                                382
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      382
      SVTG_RSIZEMIN = 6.0                                                 383
      ENDIF                                                               384
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             385
      CALL AGSRESET                                                       385
      AG_A = 28.08                                                        385
      AG_Z = 14                                                           385
      AG_W = 0.6*1*28./60.                                                385
      AG_TITLE = 'SI'                                                     385
      AG_EXNAME = 'COMPONENT'                                             385
      AG_PARLIST = 'A_Z_W'                                                385
      CALL AXCOMPONENT                                                    385
C *                                                                       385
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  385
C =                                                                       385
      CALL AGSRESET                                                       386
      AG_A = 16                                                           386
      AG_Z = 8                                                            386
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               386
      AG_TITLE = 'O'                                                      386
      AG_EXNAME = 'COMPONENT'                                             386
      AG_PARLIST = 'A_Z_W'                                                386
      CALL AXCOMPONENT                                                    386
C *                                                                       386
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 386
      CALL AGSRESET                                                       387
      AG_A = 12                                                           387
      AG_Z = 6                                                            387
      AG_W = 0.4*8*12./174.                                               387
      AG_TITLE = 'C'                                                      387
      AG_EXNAME = 'COMPONENT'                                             387
      AG_PARLIST = 'A_Z_W'                                                387
      CALL AXCOMPONENT                                                    387
C *                                                                       387
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  387
      CALL AGSRESET                                                       388
      AG_A = 1                                                            388
      AG_Z = 1                                                            388
      AG_W = 0.4*14*1./174.                                               388
      AG_TITLE = 'H'                                                      388
      AG_EXNAME = 'COMPONENT'                                             388
      AG_PARLIST = 'A_Z_W'                                                388
      CALL AXCOMPONENT                                                    388
C *                                                                       388
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  388
      CALL AGSRESET                                                       389
      AG_DENS = 1.7                                                       389
      AG_TITLE = 'G10'                                                    389
      AG_EXNAME = 'MIXTURE'                                               389
      AG_PARLIST = 'DENS'                                                 389
      CALL AXMIXTURE                                                      389
C *                                                                       389
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       389
      CALL AGSRESET                                                       390
      AG_A = 12                                                           390
      AG_Z = 6                                                            390
      AG_W = 10000*5                                                      390
      AG_TITLE = 'C5'                                                     390
      AG_EXNAME = 'COMPONENT'                                             390
      AG_PARLIST = 'A_Z_W'                                                390
      CALL AXCOMPONENT                                                    390
C *                                                                       390
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        390
      CALL AGSRESET                                                       391
      AG_A = 1                                                            391
      AG_Z = 1                                                            391
      AG_W = 10000*4                                                      391
      AG_TITLE = 'H4'                                                     391
      AG_EXNAME = 'COMPONENT'                                             391
      AG_PARLIST = 'A_Z_W'                                                391
      CALL AXCOMPONENT                                                    391
C *                                                                       391
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       391
      CALL AGSRESET                                                       392
      AG_A = 16                                                           392
      AG_Z = 8                                                            392
      AG_W = 10000*2                                                      392
      AG_TITLE = 'O2'                                                     392
      AG_EXNAME = 'COMPONENT'                                             392
      AG_PARLIST = 'A_Z_W'                                                392
      CALL AXCOMPONENT                                                    392
C *                                                                       392
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 392
      CALL AGSRESET                                                       393
      AG_A = 27                                                           393
      AG_Z = 13                                                           393
      AG_W = 10000*0.2302                                                 393
      AG_TITLE = 'AL'                                                     393
      AG_EXNAME = 'COMPONENT'                                             393
      AG_PARLIST = 'A_Z_W'                                                393
      CALL AXCOMPONENT                                                    393
C *                                                                       393
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              393
      CALL AGSRESET                                                       394
      AG_DENS = 1.432                                                     394
      AG_TITLE = 'ALKAP'                                                  394
      AG_EXNAME = 'MIXTURE'                                               394
      AG_PARLIST = 'DENS'                                                 394
      CALL AXMIXTURE                                                      394
C *                                                                       394
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       394
      CALL AGSRESET                                                       395
      AG_A = 12                                                           395
      AG_Z = 6                                                            395
      AG_W = 10000*5                                                      395
      AG_TITLE = 'C5'                                                     395
      AG_EXNAME = 'COMPONENT'                                             395
      AG_PARLIST = 'A_Z_W'                                                395
      CALL AXCOMPONENT                                                    395
C *                                                                       395
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        395
      CALL AGSRESET                                                       396
      AG_A = 1                                                            396
      AG_Z = 1                                                            396
      AG_W = 10000*4                                                      396
      AG_TITLE = 'H4'                                                     396
      AG_EXNAME = 'COMPONENT'                                             396
      AG_PARLIST = 'A_Z_W'                                                396
      CALL AXCOMPONENT                                                    396
C *                                                                       396
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       396
      CALL AGSRESET                                                       397
      AG_A = 16                                                           397
      AG_Z = 8                                                            397
      AG_W = 10000*2                                                      397
      AG_TITLE = 'O2'                                                     397
      AG_EXNAME = 'COMPONENT'                                             397
      AG_PARLIST = 'A_Z_W'                                                397
      CALL AXCOMPONENT                                                    397
C *                                                                       397
C Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                          397
      CALL AGSRESET                                                       398
      AG_A = 27                                                           398
      AG_Z = 13                                                           398
      AG_W = 986                                                          398
      AG_TITLE = 'AL'                                                     398
      AG_EXNAME = 'COMPONENT'                                             398
      AG_PARLIST = 'A_Z_W'                                                398
      CALL AXCOMPONENT                                                    398
C *                                                                       398
C Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                          398
      CALL AGSRESET                                                       399
      AG_DENS = 1.40845                                                   399
      AG_TITLE = 'SSDALMY'                                                399
      AG_EXNAME = 'MIXTURE'                                               399
      AG_PARLIST = 'DENS'                                                 399
      CALL AXMIXTURE                                                      399
C *                                                                       399
C Check SWAM_LEN>0                                                        399
      IF (SWAM_LEN.GT.0) THEN                                             400
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              401
      CALL AGSRESET                                                       401
      AG_A = 1                                                            401
      AG_Z = 1                                                            401
      AG_W = 2                                                            401
      AG_TITLE = 'H2'                                                     401
      AG_EXNAME = 'COMPONENT'                                             401
      AG_PARLIST = 'A_Z_W'                                                401
      CALL AXCOMPONENT                                                    401
C *                                                                       401
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              401
      CALL AGSRESET                                                       402
      AG_A = 16                                                           402
      AG_Z = 8                                                            402
      AG_W = 1                                                            402
      AG_TITLE = 'O'                                                      402
      AG_EXNAME = 'COMPONENT'                                             402
      AG_PARLIST = 'A_Z_W'                                                402
      CALL AXCOMPONENT                                                    402
C *                                                                       402
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                402
      CALL AGSRESET                                                       403
      AG_DENS = 1.0                                                       403
      AG_TITLE = 'WATER'                                                  403
      AG_EXNAME = 'MIXTURE'                                               403
      AG_PARLIST = 'DENS'                                                 403
      CALL AXMIXTURE                                                      403
C *                                                                       403
      ELSE                                                                403
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             405
      CALL AGSRESET                                                       405
      AG_DENS = 0.0009                                                    405
      AG_TITLE = 'WATER'                                                  405
      AG_EXNAME = 'MIXTURE'                                               405
      AG_PARLIST = 'DENS'                                                 405
      CALL AXMIXTURE                                                      405
C *                                                                       405
      ENDIF                                                               405
C Check SVTG_IFMANY>0                                                     407
      IF (SVTG_IFMANY.GT.0) THEN                                          407
      CALL AGSBEGM(MODULE,IPRIN)                                          408
C Check IPRIN<0                                                           408
      IF (IPRIN.LT.0) RETURN                                              408
C Create SVTT :                                                           408
C Call To Local Procedure SVTT                                            408
      ASSIGN 5030 TO BLOCK_SVTT                                           408
      GO TO  5020                                                         408
5030  CONTINUE                                                            408
C *                                                                       408
C Geant POSITION : SVTT ===> MOTHER='CAVE' KONLY='MANY' <===              409
      CALL AGSRESET                                                       409
      AG_MOTHER = 'CAVE'                                                  409
      AG_KONLY = 'MANY'                                                   409
      AG_TITLE = 'SVTT'                                                   409
      AG_EXNAME = 'POSITION'                                              409
      AG_PARLIST = 'MOTHER_KONLY'                                         409
      CALL AXPOSITION                                                     409
C *                                                                       409
      ELSE                                                                409
      CALL AGSBEGM(MODULE,IPRIN)                                          411
C Check IPRIN<0                                                           411
      IF (IPRIN.LT.0) RETURN                                              411
C Create SVTT :                                                           411
C Call To Local Procedure SVTT                                            411
      ASSIGN 5040 TO BLOCK_SVTT                                           411
      GO TO  5020                                                         411
5040  CONTINUE                                                            411
C *                                                                       411
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           412
      CALL AGSRESET                                                       412
      AG_MOTHER = 'CAVE'                                                  412
      AG_TITLE = 'SVTT'                                                   412
      AG_EXNAME = 'POSITION'                                              412
      AG_PARLIST = 'MOTHER'                                               412
      CALL AXPOSITION                                                     412
C *                                                                       412
      ENDIF                                                               412
C ----------------------------------------------------------------------  414
C ------------ SVTT                                                       414
      CALL AGSENDM                                                        415
      RETURN                                                              415
C Begin Local Procedure SVTT                                              415
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   415
5020  CONTINUE                                                            415
         AG_TITLE='SVTT'                                                  415
         AG_MODULE=MODULE                                                 415
         AG_MARK='V00'                                                    415
         CALL AGSPUSH                                                     415
         AG_MODULE=MODULE                                                 415
C    *                                                                    415
         RSIZEMAX=SSUP_CON4IDMX                                           416
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 417
     *   2.0*SSUP_HOSRMX                                                  417
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                418
         CLEARANCE=SVTG_RSIZEMAX - RSIZEMAX                               419
C    Check (CLEARANCE<0)                                                  420
         IF ((CLEARANCE.LT.0)) THEN                                       420
         ENDIF                                                            421
C    Geant MATERIAL : AIR ===> inherited arguments used <===              422
         CALL AGSRESET                                                    422
         AG_TITLE = 'AIR'                                                 422
         AG_EXNAME = 'MATERIAL'                                           422
         AG_PARLIST = 'NONE'                                              422
         CALL AXMATERIAL                                                  422
C    *                                                                    422
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       422
         CALL AGSRESET                                                    423
         AG_SEEN = 0                                                      423
         AG_COLO = 1                                                      423
         AG_TITLE = 'SVTT'                                                423
         AG_EXNAME = 'ATTRIBUTE'                                          423
         AG_PARLIST = 'SEEN_COLO'                                         423
         CALL AXATTRIBUTE                                                 423
C    *                                                                    423
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  423
C    VTG_ZSIZEMAX <===                                                    423
         CALL AGSRESET                                                    425
         AG_RMIN = SVTG_RSIZEMIN                                          425
         AG_RMAX = SVTG_RSIZEMAX                                          425
         AG_DZ = SVTG_ZSIZEMAX                                            425
         AG_TITLE = 'TUBE'                                                425
         AG_EXNAME = 'SHAPE'                                              425
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      425
         CALL AXSHAPE                                                     425
C    *                                                                    425
C    Create SIRP :                                                        426
C    Call To Local Procedure SIRP                                         426
         ASSIGN 5060 TO BLOCK_SIRP                                        426
         GO TO  5050                                                      426
5060     CONTINUE                                                         426
C    *                                                                    426
C    Geant POSITION : SIRP ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  427
C    2.5 <===                                                             427
         CALL AGSRESET                                                    427
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             427
         AG_ALPHAN=AG_ALPHAN+1                                            427
         AG_ALPHAK(AG_ALPHAN)=3                                           427
         AG_ALPHAL(AG_ALPHAN)=22.5                                        427
         AG_TITLE = 'SIRP'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'Z_ALPHAZ'                                          427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Geant POSITION : SIRP ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  427
C    22.5 <===                                                            427
         CALL AGSRESET                                                    428
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            428
         AG_ALPHAN=AG_ALPHAN+1                                            428
         AG_ALPHAK(AG_ALPHAN)=3                                           428
         AG_ALPHAL(AG_ALPHAN)=22.5                                        428
         AG_TITLE = 'SIRP'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'Z_ALPHAZ'                                          428
         CALL AXPOSITION                                                  428
C    *                                                                    428
C    Create SIRT :                                                        429
C    Call To Local Procedure SIRT                                         429
         ASSIGN 5080 TO BLOCK_SIRT                                        429
         GO TO  5070                                                      429
5080     CONTINUE                                                         429
C    *                                                                    429
C    Geant POSITION : SIRT ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  430
C    2.5 <===                                                             430
         CALL AGSRESET                                                    430
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             430
         AG_ALPHAN=AG_ALPHAN+1                                            430
         AG_ALPHAK(AG_ALPHAN)=3                                           430
         AG_ALPHAL(AG_ALPHAN)=22.5                                        430
         AG_TITLE = 'SIRT'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'Z_ALPHAZ'                                          430
         CALL AXPOSITION                                                  430
C    *                                                                    430
C    Geant POSITION : SIRT ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  430
C    22.5 <===                                                            430
         CALL AGSRESET                                                    431
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            431
         AG_ALPHAN=AG_ALPHAN+1                                            431
         AG_ALPHAK(AG_ALPHAN)=3                                           431
         AG_ALPHAL(AG_ALPHAN)=22.5                                        431
         AG_TITLE = 'SIRT'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'Z_ALPHAZ'                                          431
         CALL AXPOSITION                                                  431
C    *                                                                    431
C    Create SOER :                                                        432
C    Call To Local Procedure SOER                                         432
         ASSIGN 5100 TO BLOCK_SOER                                        432
         GO TO  5090                                                      432
5100     CONTINUE                                                         432
C    *                                                                    432
C    Geant POSITION : SOER ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 <===      433
         CALL AGSRESET                                                    433
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             433
         AG_TITLE = 'SOER'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'Z'                                                 433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Geant POSITION : SOER ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 <===     433
         CALL AGSRESET                                                    434
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            434
         AG_TITLE = 'SOER'                                                434
         AG_EXNAME = 'POSITION'                                           434
         AG_PARLIST = 'Z'                                                 434
         CALL AXPOSITION                                                  434
C    *                                                                    434
C    Create SBRG :                                                        435
C    Call To Local Procedure SBRG                                         435
         ASSIGN 5120 TO BLOCK_SBRG                                        435
         GO TO  5110                                                      435
5120     CONTINUE                                                         435
C    *                                                                    435
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  436
C    /2 <===                                                              436
         CALL AGSRESET                                                    436
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  436
         AG_TITLE = 'SBRG'                                                436
         AG_EXNAME = 'POSITION'                                           436
         AG_PARLIST = 'Z'                                                 436
         CALL AXPOSITION                                                  436
C    *                                                                    436
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  436
C    K/2 <===                                                             436
         CALL AGSRESET                                                    437
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 437
         AG_TITLE = 'SBRG'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'Z'                                                 437
         CALL AXPOSITION                                                  437
C    *                                                                    437
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           437
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             439
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      440
C    Create SOES :                                                        441
C    Call To Local Procedure SOES                                         441
         ASSIGN 5140 TO BLOCK_SOES                                        441
         GO TO  5130                                                      441
5140     CONTINUE                                                         441
C    *                                                                    441
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               442
         CALL AGSRESET                                                    442
         AG_Z = SCREW_Z                                                   442
         AG_KONLY = 'MANY'                                                442
         AG_TITLE = 'SOES'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'Z_KONLY'                                           442
         CALL AXPOSITION                                                  442
C    *                                                                    442
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              442
         CALL AGSRESET                                                    443
         AG_Z = -SCREW_Z                                                  443
         AG_KONLY = 'MANY'                                                443
         AG_TITLE = 'SOES'                                                443
         AG_EXNAME = 'POSITION'                                           443
         AG_PARLIST = 'Z_KONLY'                                           443
         CALL AXPOSITION                                                  443
C    *                                                                    443
C    Create SIES :                                                        444
C    Call To Local Procedure SIES                                         444
         ASSIGN 5160 TO BLOCK_SIES                                        444
         GO TO  5150                                                      444
5160     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               445
         CALL AGSRESET                                                    445
         AG_Z = SCREW_Z                                                   445
         AG_KONLY = 'MANY'                                                445
         AG_TITLE = 'SIES'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'Z_KONLY'                                           445
         CALL AXPOSITION                                                  445
C    *                                                                    445
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              445
         CALL AGSRESET                                                    446
         AG_Z = -SCREW_Z                                                  446
         AG_KONLY = 'MANY'                                                446
         AG_TITLE = 'SIES'                                                446
         AG_EXNAME = 'POSITION'                                           446
         AG_PARLIST = 'Z_KONLY'                                           446
         CALL AXPOSITION                                                  446
C    *                                                                    446
C    Check SWAM_LEN>0                                                     446
         IF (SWAM_LEN.GT.0) THEN                                          447
C    Create SWMM :                                                        448
C    Call To Local Procedure SWMM                                         448
         ASSIGN 5180 TO BLOCK_SWMM                                        448
         GO TO  5170                                                      448
5180     CONTINUE                                                         448
C    *                                                                    448
C    Geant POSITION : SWMM ===> Z=SWAM_ZMIN+SWAM_LEN/2 <===               449
         CALL AGSRESET                                                    449
         AG_Z = SWAM_ZMIN+SWAM_LEN/2                                      449
         AG_TITLE = 'SWMM'                                                449
         AG_EXNAME = 'POSITION'                                           449
         AG_PARLIST = 'Z'                                                 449
         CALL AXPOSITION                                                  449
C    *                                                                    449
C    Geant POSITION : SWMM ===> Z=-SWAM_ZMIN-SWAM_LEN/2 <===              449
         CALL AGSRESET                                                    450
         AG_Z = -SWAM_ZMIN-SWAM_LEN/2                                     450
         AG_TITLE = 'SWMM'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'Z'                                                 450
         CALL AXPOSITION                                                  450
C    *                                                                    450
         ENDIF                                                            450
C    Check SWAM_LEN>0                                                     452
         IF (SWAM_LEN.GT.0) THEN                                          452
C    Create SBWC :                                                        453
C    Call To Local Procedure SBWC                                         453
         ASSIGN 5200 TO BLOCK_SBWC                                        453
         GO TO  5190                                                      453
5200     CONTINUE                                                         453
C    *                                                                    453
C    Geant POSITION : SBWC ===> Z=(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SW  454
C    AM_ZMIN+SWAM_LEN))/2) <===                                           454
         CALL AGSRESET                                                    454
         AG_Z =                                                           454
     *   (SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)      454
         AG_TITLE = 'SBWC'                                                454
         AG_EXNAME = 'POSITION'                                           454
         AG_PARLIST = 'Z'                                                 454
         CALL AXPOSITION                                                  454
C    *                                                                    454
C    Geant POSITION : SBWC ===> Z=-(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(S  454
C    WAM_ZMIN+SWAM_LEN))/2) THETAZ=180 <===                               454
         CALL AGSRESET                                                    456
         AG_Z =                                                           456
     *   -(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)     456
         AG_THETAZ = 180                                                  456
         AG_TITLE = 'SBWC'                                                456
         AG_EXNAME = 'POSITION'                                           456
         AG_PARLIST = 'Z_THETAZ'                                          456
         CALL AXPOSITION                                                  456
C    *                                                                    456
         ENDIF                                                            456
C    Create SCON :                                                        458
C    Call To Local Procedure SCON                                         458
         ASSIGN 5220 TO BLOCK_SCON                                        458
         GO TO  5210                                                      458
5220     CONTINUE                                                         458
C    *                                                                    458
C    Geant POSITION : SCON ===> inherited arguments used <===             459
         CALL AGSRESET                                                    459
         AG_TITLE = 'SCON'                                                459
         AG_EXNAME = 'POSITION'                                           459
         AG_PARLIST = 'NONE'                                              459
         CALL AXPOSITION                                                  459
C    *                                                                    459
C    Geant POSITION : SCON ===> THETAZ=180 <===                           459
         CALL AGSRESET                                                    460
         AG_THETAZ = 180                                                  460
         AG_TITLE = 'SCON'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'THETAZ'                                            460
         CALL AXPOSITION                                                  460
C    *                                                                    460
C    Create SBSP :                                                        461
C    Call To Local Procedure SBSP                                         461
         ASSIGN 5240 TO BLOCK_SBSP                                        461
         GO TO  5230                                                      461
5240     CONTINUE                                                         461
C    *                                                                    461
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     462
         CALL AGSRESET                                                    462
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            462
         AG_TITLE = 'SBSP'                                                462
         AG_EXNAME = 'POSITION'                                           462
         AG_PARLIST = 'Z'                                                 462
         CALL AXPOSITION                                                  462
C    *                                                                    462
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  462
C    =180 <===                                                            462
         CALL AGSRESET                                                    463
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           463
         AG_THETAZ = 180                                                  463
         AG_TITLE = 'SBSP'                                                463
         AG_EXNAME = 'POSITION'                                           463
         AG_PARLIST = 'Z_THETAZ'                                          463
         CALL AXPOSITION                                                  463
C    *                                                                    463
C    Create SROD :                                                        464
C    Call To Local Procedure SROD                                         464
         ASSIGN 5260 TO BLOCK_SROD                                        464
         GO TO  5250                                                      464
5260     CONTINUE                                                         464
C    *                                                                    464
C    Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===          465
         CALL AGSRESET                                                    465
         AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                 465
         AG_TITLE = 'SROD'                                                465
         AG_EXNAME = 'POSITION'                                           465
         AG_PARLIST = 'Y'                                                 465
         CALL AXPOSITION                                                  465
C    *                                                                    465
C    Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===         465
         CALL AGSRESET                                                    466
         AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                466
         AG_TITLE = 'SROD'                                                466
         AG_EXNAME = 'POSITION'                                           466
         AG_PARLIST = 'Y'                                                 466
         CALL AXPOSITION                                                  466
C    *                                                                    466
         RADMAX=SVTG_RSIZEMAX                                             466
C    *                                                                    468
         ILAYER=SVTG_NMIN                                                 468
            GO TO 5273                                                    468
5271        ILAYER=ILAYER+1                                               468
5273        IF(ILAYER-(MIN(6,SVTG_NLAYER)).GT.0) GO TO 5272               468
C       Check ILAYER<6                                                    469
            IF (ILAYER.LT.6) THEN                                         469
C       use SVTL bank                                                     470
            BPATH_SVTL='SVTL*'                                            470
            IDNUM_SVTL(1)=0                                               470
            SVTL_LAYER=ILAYER+1                                           470
            CALL AGDGETP('SVTTGEO11','*',-999,'SVTL',NSVTL_LAYER,         470
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    470
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             470
            RADMAX=SVTL_RADIUS                                            471
            ELSE                                                          472
            RADMAX=SVTG_RSIZEMAX                                          473
            ENDIF                                                         474
C       use SVTL bank                                                     475
            BPATH_SVTL='SVTL*'                                            475
            IDNUM_SVTL(1)=0                                               475
            SVTL_LAYER=ILAYER                                             475
            CALL AGDGETP('SVTTGEO11','*',-999,'SVTL',NSVTL_LAYER,         475
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    475
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             475
C       Create SLYD :                                                     476
C       Call To Local Procedure SLYD                                      476
            ASSIGN 5290 TO BLOCK_SLYD                                     476
            GO TO  5280                                                   476
5290        CONTINUE                                                      476
C       *                                                                 476
C       Check ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6                   477
            IF (ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6) THEN           477
C       Geant POSITION : SLYD ===> KONLY='MANY' <===                      478
            CALL AGSRESET                                                 478
            AG_KONLY = 'MANY'                                             478
            AG_TITLE = 'SLYD'                                             478
            AG_EXNAME = 'POSITION'                                        478
            AG_PARLIST = 'KONLY'                                          478
            CALL AXPOSITION                                               478
C       *                                                                 478
            ELSE                                                          478
C       Geant POSITION : SLYD ===> inherited arguments used <===          480
            CALL AGSRESET                                                 480
            AG_TITLE = 'SLYD'                                             480
            AG_EXNAME = 'POSITION'                                        480
            AG_PARLIST = 'NONE'                                           480
            CALL AXPOSITION                                               480
C       *                                                                 480
            ENDIF                                                         480
         GO TO 5271                                                       482
5272     CONTINUE                                                         482
C    Check SVTG_NLAYER<0                                                  483
         IF (SVTG_NLAYER.LT.0) THEN                                       483
C    use SVTL bank                                                        484
         BPATH_SVTL='SVTL*'                                               484
         IDNUM_SVTL(1)=0                                                  484
         SVTL_LAYER=-SVTG_NLAYER                                          484
         CALL AGDGETP('SVTTGEO11','*',-999,'SVTL',NSVTL_LAYER,            484
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       484
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                484
C    Create SLYD :                                                        485
C    Call To Local Procedure SLYD                                         485
         ASSIGN 5300 TO BLOCK_SLYD                                        485
         GO TO  5280                                                      485
5300     CONTINUE                                                         485
C    *                                                                    485
C    Geant POSITION : SLYD ===> inherited arguments used <===             486
         CALL AGSRESET                                                    486
         AG_TITLE = 'SLYD'                                                486
         AG_EXNAME = 'POSITION'                                           486
         AG_PARLIST = 'NONE'                                              486
         CALL AXPOSITION                                                  486
C    *                                                                    486
         ENDIF                                                            486
C    Create SISH :                                                        488
C    Call To Local Procedure SISH                                         488
         ASSIGN 5320 TO BLOCK_SISH                                        488
         GO TO  5310                                                      488
5320     CONTINUE                                                         488
C    *                                                                    488
C    Geant POSITION : SISH ===> inherited arguments used <===             489
         CALL AGSRESET                                                    489
         AG_TITLE = 'SISH'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'NONE'                                              489
         CALL AXPOSITION                                                  489
C    *                                                                    489
C    Check (SVTG_CONFIG<=11)                                              489
         IF ((SVTG_CONFIG.LE.11)) THEN                                    490
C    Create SOSH :                                                        491
C    Call To Local Procedure SOSH                                         491
         ASSIGN 5340 TO BLOCK_SOSH                                        491
         GO TO  5330                                                      491
5340     CONTINUE                                                         491
C    *                                                                    491
C    Geant POSITION : SOSH ===> inherited arguments used <===             492
         CALL AGSRESET                                                    492
         AG_TITLE = 'SOSH'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'NONE'                                              492
         CALL AXPOSITION                                                  492
C    *                                                                    492
         ENDIF                                                            492
C    Check SVTG_NLAYER>6                                                  494
         IF (SVTG_NLAYER.GT.6) THEN                                       494
C    Create SSSH :                                                        495
C    Call To Local Procedure SSSH                                         495
         ASSIGN 5360 TO BLOCK_SSSH                                        495
         GO TO  5350                                                      495
5360     CONTINUE                                                         495
C    *                                                                    495
C    Geant POSITION : SSSH ===> inherited arguments used <===             496
         CALL AGSRESET                                                    496
         AG_TITLE = 'SSSH'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'NONE'                                              496
         CALL AXPOSITION                                                  496
C    *                                                                    496
         ENDIF                                                            496
C    Create SALM :                                                        498
C    Call To Local Procedure SALM                                         498
         ASSIGN 5380 TO BLOCK_SALM                                        498
         GO TO  5370                                                      498
5380     CONTINUE                                                         498
C    *                                                                    498
C    Geant POSITION : SALM ===> Z=SSLD_ALMSHPOS-SSLD_ALMSHTHK/2 <===      499
         CALL AGSRESET                                                    499
         AG_Z = SSLD_ALMSHPOS-SSLD_ALMSHTHK/2                             499
         AG_TITLE = 'SALM'                                                499
         AG_EXNAME = 'POSITION'                                           499
         AG_PARLIST = 'Z'                                                 499
         CALL AXPOSITION                                                  499
C    *                                                                    499
C    Geant POSITION : SALM ===> Z=-(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2) <===   499
         CALL AGSRESET                                                    500
         AG_Z = -(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2)                          500
         AG_TITLE = 'SALM'                                                500
         AG_EXNAME = 'POSITION'                                           500
         AG_PARLIST = 'Z'                                                 500
         CALL AXPOSITION                                                  500
C    *                                                                    500
C    Create SCBM :                                                        501
C    Call To Local Procedure SCBM                                         501
         ASSIGN 5400 TO BLOCK_SCBM                                        501
         GO TO  5390                                                      501
5400     CONTINUE                                                         501
C    *                                                                    501
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=SWAM_ZMIN-SCBP_LEN <===     502
         CALL AGSRESET                                                    502
         AG_X = 0.0                                                       502
         AG_Y = 0.0                                                       502
         AG_Z = SWAM_ZMIN-SCBP_LEN                                        502
         AG_TITLE = 'SCBM'                                                502
         AG_EXNAME = 'POSITION'                                           502
         AG_PARLIST = 'X_Y_Z'                                             502
         CALL AXPOSITION                                                  502
C    *                                                                    502
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=-(SWAM_ZMIN-SCBP_LEN) THET  502
C    AZ=180.0 <===                                                        502
         CALL AGSRESET                                                    503
         AG_X = 0.0                                                       503
         AG_Y = 0.0                                                       503
         AG_Z = -(SWAM_ZMIN-SCBP_LEN)                                     503
         AG_THETAZ = 180.0                                                503
         AG_TITLE = 'SCBM'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      503
         CALL AXPOSITION                                                  503
C    *                                                                    503
C    use SVTL bank                                                        503
         BPATH_SVTL='SVTL*'                                               504
         IDNUM_SVTL(1)=0                                                  504
         SVTL_LAYER=6                                                     504
         CALL AGDGETP('SVTTGEO11','*',-999,'SVTL',NSVTL_LAYER,            504
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       504
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                504
C    Create SXRL :                                                        505
C    Call To Local Procedure SXRL                                         505
         ASSIGN 5420 TO BLOCK_SXRL                                        505
         GO TO  5410                                                      505
5420     CONTINUE                                                         505
C    *                                                                    505
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  507
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  507
         CALL AGSRESET                                                    507
         AG_X = 0.0                                                       507
         AG_Y = 0.0                                                       507
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               507
         AG_KONLY = 'MANY'                                                507
         AG_TITLE = 'SXRL'                                                507
         AG_EXNAME = 'POSITION'                                           507
         AG_PARLIST = 'X_Y_Z_KONLY'                                       507
         CALL AXPOSITION                                                  507
C    *                                                                    507
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  507
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 507
         CALL AGSRESET                                                    509
         AG_X = 0.0                                                       509
         AG_Y = 0.0                                                       509
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              509
         AG_KONLY = 'MANY'                                                509
         AG_TITLE = 'SXRL'                                                509
         AG_EXNAME = 'POSITION'                                           509
         AG_PARLIST = 'X_Y_Z_KONLY'                                       509
         CALL AXPOSITION                                                  509
C    *                                                                    509
C    use SVTL bank                                                        509
         BPATH_SVTL='SVTL*'                                               510
         IDNUM_SVTL(1)=0                                                  510
         SVTL_LAYER=4                                                     510
         CALL AGDGETP('SVTTGEO11','*',-999,'SVTL',NSVTL_LAYER,            510
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       510
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                510
C    Create SXRL :                                                        511
C    Call To Local Procedure SXRL                                         511
         ASSIGN 5430 TO BLOCK_SXRL                                        511
         GO TO  5410                                                      511
5430     CONTINUE                                                         511
C    *                                                                    511
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  513
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  513
         CALL AGSRESET                                                    513
         AG_X = 0.0                                                       513
         AG_Y = 0.0                                                       513
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               513
         AG_KONLY = 'MANY'                                                513
         AG_TITLE = 'SXRL'                                                513
         AG_EXNAME = 'POSITION'                                           513
         AG_PARLIST = 'X_Y_Z_KONLY'                                       513
         CALL AXPOSITION                                                  513
C    *                                                                    513
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  513
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 513
         CALL AGSRESET                                                    515
         AG_X = 0.0                                                       515
         AG_Y = 0.0                                                       515
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              515
         AG_KONLY = 'MANY'                                                515
         AG_TITLE = 'SXRL'                                                515
         AG_EXNAME = 'POSITION'                                           515
         AG_PARLIST = 'X_Y_Z_KONLY'                                       515
         CALL AXPOSITION                                                  515
C    *                                                                    515
C    use SVTL bank                                                        515
         BPATH_SVTL='SVTL*'                                               516
         IDNUM_SVTL(1)=0                                                  516
         SVTL_LAYER=2                                                     516
         CALL AGDGETP('SVTTGEO11','*',-999,'SVTL',NSVTL_LAYER,            516
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       516
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                516
C    Create SXRL :                                                        517
C    Call To Local Procedure SXRL                                         517
         ASSIGN 5440 TO BLOCK_SXRL                                        517
         GO TO  5410                                                      517
5440     CONTINUE                                                         517
C    *                                                                    517
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  519
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  519
         CALL AGSRESET                                                    519
         AG_X = 0.0                                                       519
         AG_Y = 0.0                                                       519
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               519
         AG_KONLY = 'MANY'                                                519
         AG_TITLE = 'SXRL'                                                519
         AG_EXNAME = 'POSITION'                                           519
         AG_PARLIST = 'X_Y_Z_KONLY'                                       519
         CALL AXPOSITION                                                  519
C    *                                                                    519
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  519
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 519
         CALL AGSRESET                                                    521
         AG_X = 0.0                                                       521
         AG_Y = 0.0                                                       521
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              521
         AG_KONLY = 'MANY'                                                521
         AG_TITLE = 'SXRL'                                                521
         AG_EXNAME = 'POSITION'                                           521
         AG_PARLIST = 'X_Y_Z_KONLY'                                       521
         CALL AXPOSITION                                                  521
C    *                                                                    521
C    Create SOUM :                                                        522
C    Call To Local Procedure SOUM                                         522
         ASSIGN 5460 TO BLOCK_SOUM                                        522
         GO TO  5450                                                      522
5460     CONTINUE                                                         522
C    *                                                                    522
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 <===                    523
         CALL AGSRESET                                                    523
         AG_X = 0.0                                                       523
         AG_Y = 0.0                                                       523
         AG_Z = 0.0                                                       523
         AG_TITLE = 'SOUM'                                                523
         AG_EXNAME = 'POSITION'                                           523
         AG_PARLIST = 'X_Y_Z'                                             523
         CALL AXPOSITION                                                  523
C    *                                                                    523
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 <===       523
         CALL AGSRESET                                                    524
         AG_X = 0.0                                                       524
         AG_Y = 0.0                                                       524
         AG_Z = 0.0                                                       524
         AG_ALPHAN=AG_ALPHAN+1                                            524
         AG_ALPHAK(AG_ALPHAN)=2                                           524
         AG_ALPHAL(AG_ALPHAN)=180.0                                       524
         AG_TITLE = 'SOUM'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      524
         CALL AXPOSITION                                                  524
C    *                                                                    524
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 ALPHAZ=18  524
C    0 <===                                                               524
         CALL AGSRESET                                                    525
         AG_X = 0.0                                                       525
         AG_Y = 0.0                                                       525
         AG_Z = 0.0                                                       525
         AG_ALPHAN=AG_ALPHAN+1                                            525
         AG_ALPHAK(AG_ALPHAN)=2                                           525
         AG_ALPHAL(AG_ALPHAN)=180.0                                       525
         AG_ALPHAN=AG_ALPHAN+1                                            525
         AG_ALPHAK(AG_ALPHAN)=3                                           525
         AG_ALPHAL(AG_ALPHAN)=180                                         525
         AG_TITLE = 'SOUM'                                                525
         AG_EXNAME = 'POSITION'                                           525
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               525
         CALL AXPOSITION                                                  525
C    *                                                                    525
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAZ=180 <===         525
         CALL AGSRESET                                                    526
         AG_X = 0.0                                                       526
         AG_Y = 0.0                                                       526
         AG_Z = 0.0                                                       526
         AG_ALPHAN=AG_ALPHAN+1                                            526
         AG_ALPHAK(AG_ALPHAN)=3                                           526
         AG_ALPHAL(AG_ALPHAN)=180                                         526
         AG_TITLE = 'SOUM'                                                526
         AG_EXNAME = 'POSITION'                                           526
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      526
         CALL AXPOSITION                                                  526
C    *                                                                    526
5021  CALL AGSPOPB                                                        527
      GO TO BLOCK_SVTT, (5020 ,5030,5040)                                 527
C End of Local Procedure                                                  527
C ----------------------------------------------------------------------  528
C ------------ SXRL                                                       528
C Begin Local Procedure SXRL                                              529
C SXRL IS THE MOTHER OF THE CIRCULAR WATER PIPES                          529
5410  CONTINUE                                                            529
         AG_TITLE='SXRL'                                                  529
         AG_MODULE=MODULE                                                 529
         AG_MARK='V00'                                                    529
         CALL AGSPUSH                                                     529
         AG_MODULE=MODULE                                                 529
C    *                                                                    529
C    Geant MATERIAL : AIR ===> inherited arguments used <===              530
         CALL AGSRESET                                                    530
         AG_TITLE = 'AIR'                                                 530
         AG_EXNAME = 'MATERIAL'                                           530
         AG_PARLIST = 'NONE'                                              530
         CALL AXMATERIAL                                                  530
C    *                                                                    530
C    Geant ATTRIBUTE : SXRL ===> SEEN=0 COLO=1 <===                       530
         CALL AGSRESET                                                    531
         AG_SEEN = 0                                                      531
         AG_COLO = 1                                                      531
         AG_TITLE = 'SXRL'                                                531
         AG_EXNAME = 'ATTRIBUTE'                                          531
         AG_PARLIST = 'SEEN_COLO'                                         531
         CALL AXATTRIBUTE                                                 531
C    *                                                                    531
C    Geant SHAPE : TUBE ===> RMIN=SWCX_ROFFSET+SVTL_RADIUS, RMAX=SWCX_RO  531
C    FFSET+SVTL_RADIUS+SWCX_DR DZ=SWCX_LENGTH/2.0 <===                    531
         CALL AGSRESET                                                    533
         AG_RMIN = SWCX_ROFFSET+SVTL_RADIUS                               533
         AG_RMAX = SWCX_ROFFSET+SVTL_RADIUS+SWCX_DR                       533
         AG_DZ = SWCX_LENGTH/2.0                                          533
         AG_TITLE = 'TUBE'                                                533
         AG_EXNAME = 'SHAPE'                                              533
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      533
         CALL AXSHAPE                                                     533
C    *                                                                    533
C    Create SWRP :                                                        534
C    Call To Local Procedure SWRP                                         534
         ASSIGN 5480 TO BLOCK_SWRP                                        534
         GO TO  5470                                                      534
5480     CONTINUE                                                         534
C    *                                                                    534
C    Create SYRU :                                                        535
C    Call To Local Procedure SYRU                                         535
         ASSIGN 5500 TO BLOCK_SYRU                                        535
         GO TO  5490                                                      535
5500     CONTINUE                                                         535
C    *                                                                    535
C    *                                                                    536
         I_PHI=1                                                          536
            GO TO 5513                                                    536
5511        I_PHI=I_PHI+1                                                 536
5513        IF(I_PHI-(4*SVTL_NLADDER).GT.0) GO TO 5512                    536
            TUBE_ANGLE=(PI/(2.0*SVTL_NLADDER))*(I_PHI-0.5)                537
C       Geant POSITION : SWRP ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  539
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  539
C       SWCX_DR/2.0) Z=0.0 <===                                           539
            CALL AGSRESET                                                 539
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     539
     *      2.0)                                                          539
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     539
     *      2.0)                                                          539
            AG_Z = 0.0                                                    539
            AG_TITLE = 'SWRP'                                             539
            AG_EXNAME = 'POSITION'                                        539
            AG_PARLIST = 'X_Y_Z'                                          539
            CALL AXPOSITION                                               539
C       *                                                                 539
C       Geant POSITION : SYRU ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  539
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  539
C       SWCX_DR/2.0) Z=0.0 <===                                           539
            CALL AGSRESET                                                 541
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     541
     *      2.0)                                                          541
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     541
     *      2.0)                                                          541
            AG_Z = 0.0                                                    541
            AG_TITLE = 'SYRU'                                             541
            AG_EXNAME = 'POSITION'                                        541
            AG_PARLIST = 'X_Y_Z'                                          541
            CALL AXPOSITION                                               541
C       *                                                                 541
         GO TO 5511                                                       542
5512     CONTINUE                                                         542
5411  CALL AGSPOPB                                                        543
      GO TO BLOCK_SXRL, (5410 ,5420,5430,5440)                            543
C End of Local Procedure                                                  543
C ----------------------------------------------------------------------  544
C ------------ SWRP                                                       544
C Begin Local Procedure SWRP                                              546
C SWRP IS AN APPROXIMATION OF WATER IN THE CIRCULAR PIPE, A RECTANGULAR,  546
C  ONE                                                                    546
5470  CONTINUE                                                            546
         AG_TITLE='SWRP'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant MATERIAL : WATER ===> inherited arguments used <===            547
         CALL AGSRESET                                                    547
         AG_TITLE = 'WATER'                                               547
         AG_EXNAME = 'MATERIAL'                                           547
         AG_PARLIST = 'NONE'                                              547
         CALL AXMATERIAL                                                  547
C    *                                                                    547
C    Geant ATTRIBUTE : SWRP ===> SEEN=1 COLO=4 <===                       547
         CALL AGSRESET                                                    548
         AG_SEEN = 1                                                      548
         AG_COLO = 4                                                      548
         AG_TITLE = 'SWRP'                                                548
         AG_EXNAME = 'ATTRIBUTE'                                          548
         AG_PARLIST = 'SEEN_COLO'                                         548
         CALL AXATTRIBUTE                                                 548
C    *                                                                    548
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SWCX_RAD DZ=SWCX_LENGTH/2.0 <  548
C    ===                                                                  548
         CALL AGSRESET                                                    549
         AG_RMIN = 0.0                                                    549
         AG_RMAX = SWCX_RAD                                               549
         AG_DZ = SWCX_LENGTH/2.0                                          549
         AG_TITLE = 'TUBE'                                                549
         AG_EXNAME = 'SHAPE'                                              549
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      549
         CALL AXSHAPE                                                     549
C    *                                                                    549
5471  CALL AGSPOPB                                                        550
      GO TO BLOCK_SWRP, (5470 ,5480)                                      550
C End of Local Procedure                                                  550
C ----------------------------------------------------------------------  551
C ------------ SYRU                                                       551
C Begin Local Procedure SYRU                                              552
C SYRU IS THE WALL OF THE WATER PIPE                                      552
5490  CONTINUE                                                            552
         AG_TITLE='SYRU'                                                  552
         AG_MODULE=MODULE                                                 552
         AG_MARK='V00'                                                    552
         CALL AGSPUSH                                                     552
         AG_MODULE=MODULE                                                 552
C    *                                                                    552
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           553
         CALL AGSRESET                                                    553
         AG_A = 12                                                        553
         AG_Z = 6                                                         553
         AG_W = 1                                                         553
         AG_TITLE = 'C'                                                   553
         AG_EXNAME = 'COMPONENT'                                          553
         AG_PARLIST = 'A_Z_W'                                             553
         CALL AXCOMPONENT                                                 553
C    *                                                                    553
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           553
         CALL AGSRESET                                                    554
         AG_A = 1                                                         554
         AG_Z = 1                                                         554
         AG_W = 2                                                         554
         AG_TITLE = 'H2'                                                  554
         AG_EXNAME = 'COMPONENT'                                          554
         AG_PARLIST = 'A_Z_W'                                             554
         CALL AXCOMPONENT                                                 554
C    *                                                                    554
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             554
         CALL AGSRESET                                                    555
         AG_DENS = 0.935                                                  555
         AG_TITLE = 'CH2'                                                 555
         AG_EXNAME = 'MIXTURE'                                            555
         AG_PARLIST = 'DENS'                                              555
         CALL AXMIXTURE                                                   555
C    *                                                                    555
C    Geant ATTRIBUTE : SYRU ===> SEEN=1 COLO=4 <===                       555
         CALL AGSRESET                                                    556
         AG_SEEN = 1                                                      556
         AG_COLO = 4                                                      556
         AG_TITLE = 'SYRU'                                                556
         AG_EXNAME = 'ATTRIBUTE'                                          556
         AG_PARLIST = 'SEEN_COLO'                                         556
         CALL AXATTRIBUTE                                                 556
C    *                                                                    556
C    Geant SHAPE : TUBE ===> RMIN=SWCX_RAD RMAX=SWCX_RAD+SWCX_WALL, DZ=S  556
C    WCX_LENGTH/2.0 <===                                                  556
         CALL AGSRESET                                                    558
         AG_RMIN = SWCX_RAD                                               558
         AG_RMAX = SWCX_RAD+SWCX_WALL                                     558
         AG_DZ = SWCX_LENGTH/2.0                                          558
         AG_TITLE = 'TUBE'                                                558
         AG_EXNAME = 'SHAPE'                                              558
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      558
         CALL AXSHAPE                                                     558
C    *                                                                    558
5491  CALL AGSPOPB                                                        559
      GO TO BLOCK_SYRU, (5490 ,5500)                                      559
C End of Local Procedure                                                  559
C ----------------------------------------------------------------------  560
C ------------ SOUM                                                       560
C Begin Local Procedure SOUM                                              561
C SOUM IS THE MOTHER OF THE ARRAY OF THE OUTER SHILEDING SUPPORT TUBES    561
5450  CONTINUE                                                            561
         AG_TITLE='SOUM'                                                  561
         AG_MODULE=MODULE                                                 561
         AG_MARK='V00'                                                    561
         CALL AGSPUSH                                                     561
         AG_MODULE=MODULE                                                 561
C    *                                                                    561
C    Geant MATERIAL : AIR ===> inherited arguments used <===              562
         CALL AGSRESET                                                    562
         AG_TITLE = 'AIR'                                                 562
         AG_EXNAME = 'MATERIAL'                                           562
         AG_PARLIST = 'NONE'                                              562
         CALL AXMATERIAL                                                  562
C    *                                                                    562
C    Geant ATTRIBUTE : SOUM ===> SEEN=0 COLO=1 <===                       562
         CALL AGSRESET                                                    563
         AG_SEEN = 0                                                      563
         AG_COLO = 1                                                      563
         AG_TITLE = 'SOUM'                                                563
         AG_EXNAME = 'ATTRIBUTE'                                          563
         AG_PARLIST = 'SEEN_COLO'                                         563
         CALL AXATTRIBUTE                                                 563
C    *                                                                    563
C    Geant SHAPE : TUBS ===> RMIN=SOUP_ROUT-SOUP_DR RMAX=SOUP_ROUT, PHI1  563
C    =SOUP_PHI1 PHI2=SOUP_PHI2 DZ=SOUP_LENGTH/2.0 <===                    563
         CALL AGSRESET                                                    565
         AG_RMIN = SOUP_ROUT-SOUP_DR                                      565
         AG_RMAX = SOUP_ROUT                                              565
         AG_PHI1 = SOUP_PHI1                                              565
         AG_PHI2 = SOUP_PHI2                                              565
         AG_DZ = SOUP_LENGTH/2.0                                          565
         AG_TITLE = 'TUBS'                                                565
         AG_EXNAME = 'SHAPE'                                              565
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            565
         CALL AXSHAPE                                                     565
C    *                                                                    565
C    Create SOUR :                                                        566
C    Call To Local Procedure SOUR                                         566
         ASSIGN 5530 TO BLOCK_SOUR                                        566
         GO TO  5520                                                      566
5530     CONTINUE                                                         566
C    *                                                                    566
C    *                                                                    567
         I_PHI=1                                                          567
            GO TO 5543                                                    567
5541        I_PHI=I_PHI+1                                                 567
5543        IF(I_PHI-(4).GT.0) GO TO 5542                                 567
C       Geant POSITION : SOUR ===> X=COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SO  569
C       UP_ROUT-SOUP_DR/2.0) Y=SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROU  569
C       T-SOUP_DR/2.0) Z=0.0 <===                                         569
            CALL AGSRESET                                                 569
            AG_X = COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    569
     *      2.0)                                                          569
            AG_Y = SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    569
     *      2.0)                                                          569
            AG_Z = 0.0                                                    569
            AG_TITLE = 'SOUR'                                             569
            AG_EXNAME = 'POSITION'                                        569
            AG_PARLIST = 'X_Y_Z'                                          569
            CALL AXPOSITION                                               569
C       *                                                                 569
         GO TO 5541                                                       570
5542     CONTINUE                                                         570
5451  CALL AGSPOPB                                                        571
      GO TO BLOCK_SOUM, (5450 ,5460)                                      571
C End of Local Procedure                                                  571
C ----------------------------------------------------------------------  572
C ------------ SOUR                                                       572
C Begin Local Procedure SOUR                                              573
C SOUR IS THE OUTER SHILEDING SUPPORT TUBES (RODS)                        573
5520  CONTINUE                                                            573
         AG_TITLE='SOUR'                                                  573
         AG_MODULE=MODULE                                                 573
         AG_MARK='V00'                                                    573
         CALL AGSPUSH                                                     573
         AG_MODULE=MODULE                                                 573
C    *                                                                    573
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           574
         CALL AGSRESET                                                    574
         AG_TITLE = 'CARBON'                                              574
         AG_EXNAME = 'MATERIAL'                                           574
         AG_PARLIST = 'NONE'                                              574
         CALL AXMATERIAL                                                  574
C    *                                                                    574
C    Geant ATTRIBUTE : SOUM ===> SEEN=1 COLO=4 <===                       574
         CALL AGSRESET                                                    575
         AG_SEEN = 1                                                      575
         AG_COLO = 4                                                      575
         AG_TITLE = 'SOUM'                                                575
         AG_EXNAME = 'ATTRIBUTE'                                          575
         AG_PARLIST = 'SEEN_COLO'                                         575
         CALL AXATTRIBUTE                                                 575
C    *                                                                    575
C    Geant SHAPE : TUBE ===> RMIN=SOUP_DIAMIN/2.0 RMAX=SOUP_DIAMOUT/2.0,  575
C     DZ=SOUP_LENGTH/2.0 <===                                             575
         CALL AGSRESET                                                    577
         AG_RMIN = SOUP_DIAMIN/2.0                                        577
         AG_RMAX = SOUP_DIAMOUT/2.0                                       577
         AG_DZ = SOUP_LENGTH/2.0                                          577
         AG_TITLE = 'TUBE'                                                577
         AG_EXNAME = 'SHAPE'                                              577
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      577
         CALL AXSHAPE                                                     577
C    *                                                                    577
5521  CALL AGSPOPB                                                        578
      GO TO BLOCK_SOUR, (5520 ,5530)                                      578
C End of Local Procedure                                                  578
C ----------------------------------------------------------------------  579
C ------------ SLYD                                                       579
C Begin Local Procedure SLYD                                              580
C SLYD IS A SINGLE SVT LAYER                                              580
5280  CONTINUE                                                            580
         AG_TITLE='SLYD'                                                  580
         AG_MODULE=MODULE                                                 580
         AG_MARK='V00'                                                    580
         CALL AGSPUSH                                                     580
         AG_MODULE=MODULE                                                 580
C    *                                                                    580
         LADTHK = SWCA_WAFERTHK/2 + SWCA_ROHATHK + SWCA_WAFCARTH          581
         ELETHK=2.0*SELC_BETHK + SELC_WATTHK + SELC_BEOTHK +              582
     *   SELC_AGPDTHK                                                     582
         ELETHK=ELETHK + SELC_GLASSTHK + SELC_DYETHK                      583
         DEG=180.0/SVTL_NLADDER                                           584
         RAD=PI/SVTL_NLADDER                                              585
C    Geant MATERIAL : AIR ===> inherited arguments used <===              586
         CALL AGSRESET                                                    586
         AG_TITLE = 'AIR'                                                 586
         AG_EXNAME = 'MATERIAL'                                           586
         AG_PARLIST = 'NONE'                                              586
         CALL AXMATERIAL                                                  586
C    *                                                                    586
C    Geant ATTRIBUTE : SLYD ===> SEEN=0 COLO=1 <===                       586
         CALL AGSRESET                                                    587
         AG_SEEN = 0                                                      587
         AG_COLO = 1                                                      587
         AG_TITLE = 'SLYD'                                                587
         AG_EXNAME = 'ATTRIBUTE'                                          587
         AG_PARLIST = 'SEEN_COLO'                                         587
         CALL AXATTRIBUTE                                                 587
C    *                                                                    587
         RMIN=SVTL_RADIUS - LADTHK                                        587
         RMAX = SVTL_RADIUS*SVTL_RADIUS                                   589
         RMAX=RMAX+SWCA_WAFERWID*SWCA_WAFERWID/4.0                        590
         RMAX = SQRT(RMAX) + ELETHK                                       591
C    Check (RMAX>RADMAX-LADTHK)                                           592
         IF ((RMAX.GT.RADMAX-LADTHK)) THEN                                592
         ENDIF                                                            593
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   594
C    Check (RMAX<RMIN)                                                    595
         IF ((RMAX.LT.RMIN)) THEN                                         595
         ENDIF                                                            596
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    597
         CALL AGSRESET                                                    597
         AG_RMIN = RMIN                                                   597
         AG_RMAX = RMAX                                                   597
         AG_DZ = SWCA_LENGTH/2                                            597
         AG_TITLE = 'TUBE'                                                597
         AG_EXNAME = 'SHAPE'                                              597
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      597
         CALL AXSHAPE                                                     597
C    *                                                                    597
C    Create SLSD :                                                        598
C    Call To Local Procedure SLSD                                         598
         ASSIGN 5560 TO BLOCK_SLSD                                        598
         GO TO  5550                                                      598
5560     CONTINUE                                                         598
C    *                                                                    598
5281  CALL AGSPOPB                                                        599
      GO TO BLOCK_SLYD, (5280 ,5290,5300)                                 599
C End of Local Procedure                                                  599
C ----------------------------------------------------------------------  600
C ------------ SLSD                                                       600
C Begin Local Procedure SLSD                                              601
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         601
5550  CONTINUE                                                            601
         AG_TITLE='SLSD'                                                  601
         AG_MODULE=MODULE                                                 601
         AG_MARK='V00'                                                    601
         CALL AGSPUSH                                                     601
         AG_MODULE=MODULE                                                 601
C    *                                                                    601
C    Geant ATTRIBUTE : SLSD ===> SEEN=0 COLO=1 <===                       602
         CALL AGSRESET                                                    602
         AG_SEEN = 0                                                      602
         AG_COLO = 1                                                      602
         AG_TITLE = 'SLSD'                                                602
         AG_EXNAME = 'ATTRIBUTE'                                          602
         AG_PARLIST = 'SEEN_COLO'                                         602
         CALL AXATTRIBUTE                                                 602
C    *                                                                    602
C    Check SVTG_NLAYER<0                                                  602
         IF (SVTG_NLAYER.LT.0) THEN                                       603
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               604
         CALL AGSRESET                                                    604
         AG_NDIV = 1                                                      604
         AG_IAXIS = 2                                                     604
         AG_C0 = -90                                                      604
         AG_TITLE = 'DIVISION'                                            604
         AG_EXNAME = 'SHAPE'                                              604
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     604
         CALL AXSHAPE                                                     604
C    *                                                                    604
         ELSE                                                             604
C    Check SVTG_VERSION==1                                                606
         IF (SVTG_VERSION.EQ.1) THEN                                      606
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              607
         ELSE                                                             608
         C0=90-DEG*MOD(ILAYER,2)                                          609
         ENDIF                                                            610
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NLADDER IAXIS=2, C0=C0 <===    612
         CALL AGSRESET                                                    612
         AG_NDIV = SVTL_NLADDER                                           612
         AG_IAXIS = 2                                                     612
         AG_C0 = C0                                                       612
         AG_TITLE = 'DIVISION'                                            612
         AG_EXNAME = 'SHAPE'                                              612
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     612
         CALL AXSHAPE                                                     612
C    *                                                                    612
         ENDIF                                                            612
C    Create SLDI :                                                        614
C    Call To Local Procedure SLDI                                         614
         ASSIGN 5580 TO BLOCK_SLDI                                        614
         GO TO  5570                                                      614
5580     CONTINUE                                                         614
C    *                                                                    614
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ORT=YZX <===                615
         CALL AGSRESET                                                    615
         AG_X = SVTL_RADIUS                                               615
         AG_ALPHAN=AG_ALPHAN+1                                            615
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 615
         AG_TITLE = 'SLDI'                                                615
         AG_EXNAME = 'POSITION'                                           615
         AG_PARLIST = 'X_ORT'                                             615
         CALL AXPOSITION                                                  615
C    *                                                                    615
C    Create SELE :                                                        616
C    Call To Local Procedure SELE                                         616
         ASSIGN 5600 TO BLOCK_SELE                                        616
         GO TO  5590                                                      616
5600     CONTINUE                                                         616
C    *                                                                    616
         DEG=180.0/SVTL_NLADDER                                           617
         RAD=(TWOPI/2)/SVTL_NLADDER                                       618
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 619
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 620
C    *                                                                    621
         S=-1                                                             621
            GOTO 5613                                                     621
5611        S=S+(2)                                                       621
5613        IF((2)*(S-(1)).GT.0) GO TO 5612                               621
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  623
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=S*DEG <===         623
            CALL AGSRESET                                                 623
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                623
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               623
            AG_ALPHAN=AG_ALPHAN+1                                         623
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              623
            AG_ALPHAN=AG_ALPHAN+1                                         623
            AG_ALPHAK(AG_ALPHAN)=1                                        623
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 623
            AG_ALPHAN=AG_ALPHAN+1                                         623
            AG_ALPHAK(AG_ALPHAN)=3                                        623
            AG_ALPHAL(AG_ALPHAN)=S*DEG                                    623
            AG_TITLE = 'SELE'                                             623
            AG_EXNAME = 'POSITION'                                        623
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          623
            CALL AXPOSITION                                               623
C       *                                                                 623
         GO TO 5611                                                       624
5612     CONTINUE                                                         624
5551  CALL AGSPOPB                                                        625
      GO TO BLOCK_SLSD, (5550 ,5560)                                      625
C End of Local Procedure                                                  625
C ----------------------------------------------------------------------  626
C ------------ SLDI                                                       626
C Begin Local Procedure SLDI                                              627
C SLDI IS A LADDER VOLUME                                                 627
5570  CONTINUE                                                            627
         AG_TITLE='SLDI'                                                  627
         AG_MODULE=MODULE                                                 627
         AG_MARK='V00'                                                    627
         CALL AGSPUSH                                                     627
         AG_MODULE=MODULE                                                 627
C    *                                                                    627
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       628
C    Geant MATERIAL : AIR ===> inherited arguments used <===              629
         CALL AGSRESET                                                    629
         AG_TITLE = 'AIR'                                                 629
         AG_EXNAME = 'MATERIAL'                                           629
         AG_PARLIST = 'NONE'                                              629
         CALL AXMATERIAL                                                  629
C    *                                                                    629
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 SERIAL=SVTL_NWAFER <===    629
         CALL AGSRESET                                                    630
         AG_SEEN = 0                                                      630
         AG_COLO = 1                                                      630
         AG_SERIAL = SVTL_NWAFER                                          630
         AG_TITLE = 'SLDI'                                                630
         AG_EXNAME = 'ATTRIBUTE'                                          630
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  630
         CALL AXATTRIBUTE                                                 630
C    *                                                                    630
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  630
C    HK <===                                                              630
         CALL AGSRESET                                                    632
         AG_DX = SWCA_WAFERWID/2                                          632
         AG_DY = SWCA_LENGTH/2                                            632
         AG_DZ = LADTHK                                                   632
         AG_TITLE = 'BOX'                                                 632
         AG_EXNAME = 'SHAPE'                                              632
         AG_PARLIST = 'DX_DY_DZ'                                          632
         CALL AXSHAPE                                                     632
C    *                                                                    632
C    Create STLI :                                                        633
C    Call To Local Procedure STLI                                         633
         ASSIGN 5630 TO BLOCK_STLI                                        633
         GO TO  5620                                                      633
5630     CONTINUE                                                         633
C    *                                                                    633
C    Geant POSITION : STLI ===> inherited arguments used <===             634
         CALL AGSRESET                                                    634
         AG_TITLE = 'STLI'                                                634
         AG_EXNAME = 'POSITION'                                           634
         AG_PARLIST = 'NONE'                                              634
         CALL AXPOSITION                                                  634
C    *                                                                    634
C    Create SBER :                                                        635
C    Call To Local Procedure SBER                                         635
         ASSIGN 5650 TO BLOCK_SBER                                        635
         GO TO  5640                                                      635
5650     CONTINUE                                                         635
C    *                                                                    635
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  636
C    DTHK+SWCA_WAFCARTH/2 <===                                            636
         CALL AGSRESET                                                    636
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          636
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   636
         AG_TITLE = 'SBER'                                                636
         AG_EXNAME = 'POSITION'                                           636
         AG_PARLIST = 'X_Z'                                               636
         CALL AXPOSITION                                                  636
C    *                                                                    636
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  636
C    DTHK+SWCA_WAFCARTH/2 <===                                            636
         CALL AGSRESET                                                    637
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          637
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   637
         AG_TITLE = 'SBER'                                                637
         AG_EXNAME = 'POSITION'                                           637
         AG_PARLIST = 'X_Z'                                               637
         CALL AXPOSITION                                                  637
C    *                                                                    637
C    Create STAB :                                                        638
C    Call To Local Procedure STAB                                         638
         ASSIGN 5670 TO BLOCK_STAB                                        638
         GO TO  5660                                                      638
5670     CONTINUE                                                         638
C    *                                                                    638
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  639
C    WAFCARTH/2 <===                                                      639
         CALL AGSRESET                                                    639
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    639
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   639
         AG_TITLE = 'STAB'                                                639
         AG_EXNAME = 'POSITION'                                           639
         AG_PARLIST = 'Y_Z'                                               639
         CALL AXPOSITION                                                  639
C    *                                                                    639
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  639
C    _WAFCARTH/2 <===                                                     639
         CALL AGSRESET                                                    640
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   640
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   640
         AG_TITLE = 'STAB'                                                640
         AG_EXNAME = 'POSITION'                                           640
         AG_PARLIST = 'Y_Z'                                               640
         CALL AXPOSITION                                                  640
C    *                                                                    640
C    Create STRU :                                                        641
C    Call To Local Procedure STRU                                         641
         ASSIGN 5690 TO BLOCK_STRU                                        641
         GO TO  5680                                                      641
5690     CONTINUE                                                         641
C    *                                                                    641
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  644
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  644
C    =                                                                    644
         CALL AGSRESET                                                    644
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              644
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  644
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   644
         AG_TITLE = 'STRU'                                                644
         AG_EXNAME = 'POSITION'                                           644
         AG_PARLIST = 'Y_Z'                                               644
         CALL AXPOSITION                                                  644
C    *                                                                    644
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  644
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  644
C    ==                                                                   644
         CALL AGSRESET                                                    647
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             647
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  647
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   647
         AG_TITLE = 'STRU'                                                647
         AG_EXNAME = 'POSITION'                                           647
         AG_PARLIST = 'Y_Z'                                               647
         CALL AXPOSITION                                                  647
C    *                                                                    647
C    Create SRHC :                                                        648
C    Call To Local Procedure SRHC                                         648
         ASSIGN 5710 TO BLOCK_SRHC                                        648
         GO TO  5700                                                      648
5710     CONTINUE                                                         648
C    *                                                                    648
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  650
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         650
         CALL AGSRESET                                                    650
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          650
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                650
         AG_TITLE = 'SRHC'                                                650
         AG_EXNAME = 'POSITION'                                           650
         AG_PARLIST = 'X_Z'                                               650
         CALL AXPOSITION                                                  650
C    *                                                                    650
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  650
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         650
         CALL AGSRESET                                                    652
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          652
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                652
         AG_TITLE = 'SRHC'                                                652
         AG_EXNAME = 'POSITION'                                           652
         AG_PARLIST = 'X_Z'                                               652
         CALL AXPOSITION                                                  652
C    *                                                                    652
C    Create SPCB :                                                        653
C    Call To Local Procedure SPCB                                         653
         ASSIGN 5730 TO BLOCK_SPCB                                        653
         GO TO  5720                                                      653
5730     CONTINUE                                                         653
C    *                                                                    653
         YPCB = SVTL_NWAFER*(SWCA_WAFERLEN + SWCA_WAFERGAP)/2.0 +         654
     *   SVTL_PCBGAP + SVTL_PCBLEN/2.0                                    654
C    Geant POSITION : SPCB ===> X=0.0 Y=YPCB Z=SVTL_PCBTHK/2.0 <===       655
         CALL AGSRESET                                                    655
         AG_X = 0.0                                                       655
         AG_Y = YPCB                                                      655
         AG_Z = SVTL_PCBTHK/2.0                                           655
         AG_TITLE = 'SPCB'                                                655
         AG_EXNAME = 'POSITION'                                           655
         AG_PARLIST = 'X_Y_Z'                                             655
         CALL AXPOSITION                                                  655
C    *                                                                    655
C    Geant POSITION : SPCB ===> X=0.0 Y=-YPCB Z=SVTL_PCBTHK/2.0 ALPHAZ=1  655
C    80.0 <===                                                            655
         CALL AGSRESET                                                    656
         AG_X = 0.0                                                       656
         AG_Y = -YPCB                                                     656
         AG_Z = SVTL_PCBTHK/2.0                                           656
         AG_ALPHAN=AG_ALPHAN+1                                            656
         AG_ALPHAK(AG_ALPHAN)=3                                           656
         AG_ALPHAL(AG_ALPHAN)=180.0                                       656
         AG_TITLE = 'SPCB'                                                656
         AG_EXNAME = 'POSITION'                                           656
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      656
         CALL AXPOSITION                                                  656
C    *                                                                    656
5571  CALL AGSPOPB                                                        657
      GO TO BLOCK_SLDI, (5570 ,5580)                                      657
C End of Local Procedure                                                  657
C ----------------------------------------------------------------------  658
C ------------ SRHC                                                       658
C Begin Local Procedure SRHC                                              659
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     659
5700  CONTINUE                                                            659
         AG_TITLE='SRHC'                                                  659
         AG_MODULE=MODULE                                                 659
         AG_MARK='V00'                                                    659
         CALL AGSPUSH                                                     659
         AG_MODULE=MODULE                                                 659
C    *                                                                    659
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         660
         CALL AGSRESET                                                    660
         AG_A = 28                                                        660
         AG_Z = 14                                                        660
         AG_W = 1                                                         660
         AG_TITLE = 'SI'                                                  660
         AG_EXNAME = 'COMPONENT'                                          660
         AG_PARLIST = 'A_Z_W'                                             660
         CALL AXCOMPONENT                                                 660
C    *                                                                    660
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           660
         CALL AGSRESET                                                    661
         AG_A = 16                                                        661
         AG_Z = 8                                                         661
         AG_W = 2                                                         661
         AG_TITLE = 'O'                                                   661
         AG_EXNAME = 'COMPONENT'                                          661
         AG_PARLIST = 'A_Z_W'                                             661
         CALL AXCOMPONENT                                                 661
C    *                                                                    661
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             661
         CALL AGSRESET                                                    662
         AG_DENS = 2.2                                                    662
         AG_TITLE = 'GLASS'                                               662
         AG_EXNAME = 'MIXTURE'                                            662
         AG_PARLIST = 'DENS'                                              662
         CALL AXMIXTURE                                                   662
C    *                                                                    662
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       662
         CALL AGSRESET                                                    663
         AG_SEEN = 1                                                      663
         AG_COLO = 3                                                      663
         AG_TITLE = 'SRHC'                                                663
         AG_EXNAME = 'ATTRIBUTE'                                          663
         AG_PARLIST = 'SEEN_COLO'                                         663
         CALL AXATTRIBUTE                                                 663
C    *                                                                    663
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  663
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        663
         CALL AGSRESET                                                    665
         AG_DX = SWCA_WAFCARWD/2                                          665
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              665
         AG_DZ = SWCA_ROHATHK/2                                           665
         AG_TITLE = 'BOX'                                                 665
         AG_EXNAME = 'SHAPE'                                              665
         AG_PARLIST = 'DX_DY_DZ'                                          665
         CALL AXSHAPE                                                     665
C    *                                                                    665
5701  CALL AGSPOPB                                                        666
      GO TO BLOCK_SRHC, (5700 ,5710)                                      666
C End of Local Procedure                                                  666
C ----------------------------------------------------------------------  667
C ------------ STLI                                                       667
C Begin Local Procedure STLI                                              668
C STLI IS THE WAFER PACK CONTAINER                                        668
5620  CONTINUE                                                            668
         AG_TITLE='STLI'                                                  668
         AG_MODULE=MODULE                                                 668
         AG_MARK='V00'                                                    668
         CALL AGSPUSH                                                     668
         AG_MODULE=MODULE                                                 668
C    *                                                                    668
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              669
         CALL AGSRESET                                                    669
         AG_SEEN = 0                                                      669
         AG_COLO = 1                                                      669
         AG_SERIAL = 0                                                    669
         AG_TITLE = 'STLI'                                                669
         AG_EXNAME = 'ATTRIBUTE'                                          669
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  669
         CALL AXATTRIBUTE                                                 669
C    *                                                                    669
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  669
C    P), DZ=SWCA_WAFERTHK/2 <===                                          669
         CALL AGSRESET                                                    671
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              671
         AG_DZ = SWCA_WAFERTHK/2                                          671
         AG_TITLE = 'BOX'                                                 671
         AG_EXNAME = 'SHAPE'                                              671
         AG_PARLIST = 'DY_DZ'                                             671
         CALL AXSHAPE                                                     671
C    *                                                                    671
C    Create STSI :                                                        672
C    Call To Local Procedure STSI                                         672
         ASSIGN 5750 TO BLOCK_STSI                                        672
         GO TO  5740                                                      672
5750     CONTINUE                                                         672
C    *                                                                    672
5621  CALL AGSPOPB                                                        673
      GO TO BLOCK_STLI, (5620 ,5630)                                      673
C End of Local Procedure                                                  673
C ----------------------------------------------------------------------  674
C ------------ STSI                                                       674
C Begin Local Procedure STSI                                              675
C STSI IS A SINGLE WAVER CONTAINER                                        675
5740  CONTINUE                                                            675
         AG_TITLE='STSI'                                                  675
         AG_MODULE=MODULE                                                 675
         AG_MARK='V00'                                                    675
         CALL AGSPUSH                                                     675
         AG_MODULE=MODULE                                                 675
C    *                                                                    675
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            676
         CALL AGSRESET                                                    676
         AG_NDIV = SVTL_NWAFER                                            676
         AG_IAXIS = 2                                                     676
         AG_TITLE = 'DIVISION'                                            676
         AG_EXNAME = 'SHAPE'                                              676
         AG_PARLIST = 'NDIV_IAXIS'                                        676
         CALL AXSHAPE                                                     676
C    *                                                                    676
C    Create SVTD :                                                        677
C    Call To Local Procedure SVTD                                         677
         ASSIGN 5770 TO BLOCK_SVTD                                        677
         GO TO  5760                                                      677
5770     CONTINUE                                                         677
C    *                                                                    677
C    Geant POSITION : SVTD ===> inherited arguments used <===             678
         CALL AGSRESET                                                    678
         AG_TITLE = 'SVTD'                                                678
         AG_EXNAME = 'POSITION'                                           678
         AG_PARLIST = 'NONE'                                              678
         CALL AXPOSITION                                                  678
C    *                                                                    678
5741  CALL AGSPOPB                                                        679
      GO TO BLOCK_STSI, (5740 ,5750)                                      679
C End of Local Procedure                                                  679
C ----------------------------------------------------------------------  680
C ------------ SVTD                                                       680
C Begin Local Procedure SVTD                                              681
C SVTD IS AN ACTIVE WAFER VOLUME                                          681
5760  CONTINUE                                                            681
         AG_TITLE='SVTD'                                                  681
         AG_MODULE=MODULE                                                 681
         AG_MARK='V00'                                                    681
         CALL AGSPUSH                                                     681
         AG_MODULE=MODULE                                                 681
C    *                                                                    681
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          682
         CALL AGSRESET                                                    682
         AG_TITLE = 'SILICON'                                             682
         AG_EXNAME = 'MATERIAL'                                           682
         AG_PARLIST = 'NONE'                                              682
         CALL AXMATERIAL                                                  682
C    *                                                                    682
C    Geant MATERIAL : SENSSI ===> ISVOL=1 <===                            682
         CALL AGSRESET                                                    683
         AG_ISVOL = 1                                                     683
         AG_TITLE = 'SENSSI'                                              683
         AG_EXNAME = 'MATERIAL'                                           683
         AG_PARLIST = 'ISVOL'                                             683
         CALL AXMATERIAL                                                  683
C    *                                                                    683
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       683
         CALL AGSRESET                                                    684
         AG_SEEN = 1                                                      684
         AG_COLO = 4                                                      684
         AG_TITLE = 'SVTD'                                                684
         AG_EXNAME = 'ATTRIBUTE'                                          684
         AG_PARLIST = 'SEEN_COLO'                                         684
         CALL AXATTRIBUTE                                                 684
C    *                                                                    684
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       684
         CALL AGSRESET                                                    685
         AG_DY = SWCA_WAFERLEN/2                                          685
         AG_TITLE = 'BOX'                                                 685
         AG_EXNAME = 'SHAPE'                                              685
         AG_PARLIST = 'DY'                                                685
         CALL AXSHAPE                                                     685
C    *                                                                    685
         TRAPY=SWCA_SENSWID/2.0 - (SWCA_SENSWID - SWCA_SENSGIR)/2.0       685
C    Create STRA :                                                        687
C    Call To Local Procedure STRA                                         687
         ASSIGN 5790 TO BLOCK_STRA                                        687
         GO TO  5780                                                      687
5790     CONTINUE                                                         687
C    *                                                                    687
C    Geant POSITION : STRA ===> Y=+TRAPY ALPHAZ=+90 ORT=YZX <===          688
         CALL AGSRESET                                                    688
         AG_Y = +TRAPY                                                    688
         AG_ALPHAN=AG_ALPHAN+1                                            688
         AG_ALPHAK(AG_ALPHAN)=3                                           688
         AG_ALPHAL(AG_ALPHAN)=+90                                         688
         AG_ALPHAN=AG_ALPHAN+1                                            688
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 688
         AG_TITLE = 'STRA'                                                688
         AG_EXNAME = 'POSITION'                                           688
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      688
         CALL AXPOSITION                                                  688
C    *                                                                    688
C    Geant POSITION : STRA ===> Y=-TRAPY ALPHAZ=-90 ORT=YZX <===          688
         CALL AGSRESET                                                    689
         AG_Y = -TRAPY                                                    689
         AG_ALPHAN=AG_ALPHAN+1                                            689
         AG_ALPHAK(AG_ALPHAN)=3                                           689
         AG_ALPHAL(AG_ALPHAN)=-90                                         689
         AG_ALPHAN=AG_ALPHAN+1                                            689
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 689
         AG_TITLE = 'STRA'                                                689
         AG_EXNAME = 'POSITION'                                           689
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      689
         CALL AXPOSITION                                                  689
C    *                                                                    689
         SSIDX=SWCA_WAFERLEN/2.0 - (SWCA_WAFERLEN - SWCA_SENSLEN)/4.0     689
         SSIRY=SWCA_WAFERWID/2.0 - (SWCA_WAFERWID - SWCA_SENSWID)/4.0     691
C    Create SSID :                                                        692
C    Call To Local Procedure SSID                                         692
         ASSIGN 5810 TO BLOCK_SSID                                        692
         GO TO  5800                                                      692
5810     CONTINUE                                                         692
C    *                                                                    692
C    Geant POSITION : SSID ===> X=+SSIDX <===                             693
         CALL AGSRESET                                                    693
         AG_X = +SSIDX                                                    693
         AG_TITLE = 'SSID'                                                693
         AG_EXNAME = 'POSITION'                                           693
         AG_PARLIST = 'X'                                                 693
         CALL AXPOSITION                                                  693
C    *                                                                    693
C    Geant POSITION : SSID ===> X=-SSIDX <===                             693
         CALL AGSRESET                                                    694
         AG_X = -SSIDX                                                    694
         AG_TITLE = 'SSID'                                                694
         AG_EXNAME = 'POSITION'                                           694
         AG_PARLIST = 'X'                                                 694
         CALL AXPOSITION                                                  694
C    *                                                                    694
C    Create SSIR :                                                        695
C    Call To Local Procedure SSIR                                         695
         ASSIGN 5830 TO BLOCK_SSIR                                        695
         GO TO  5820                                                      695
5830     CONTINUE                                                         695
C    *                                                                    695
C    Geant POSITION : SSIR ===> Y=+SSIRY <===                             696
         CALL AGSRESET                                                    696
         AG_Y = +SSIRY                                                    696
         AG_TITLE = 'SSIR'                                                696
         AG_EXNAME = 'POSITION'                                           696
         AG_PARLIST = 'Y'                                                 696
         CALL AXPOSITION                                                  696
C    *                                                                    696
C    Geant POSITION : SSIR ===> Y=-SSIRY <===                             696
         CALL AGSRESET                                                    697
         AG_Y = -SSIRY                                                    697
         AG_TITLE = 'SSIR'                                                697
         AG_EXNAME = 'POSITION'                                           697
         AG_PARLIST = 'Y'                                                 697
         CALL AXPOSITION                                                  697
C    *                                                                    697
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 697
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  700
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  700
C    .,0.01)                                                              700
         AG_TITLE = 'SVTD'                                                700
         AG_EXNAME = 'HITS'                                               700
         AG_TYPE=AG_MAGIC                                                 700
         CALL AGSDETV('SVTD')                                             700
C    *                                                                    700
         AG_PARLIST='Z'                                                   700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = AG_MAGIC                                              700
         AG_BIN=.001                                                      700
         AG_OPTION='S'                                                    700
         AG_FHMIN = AG_MAGIC                                              700
         AG_FHMAX = AG_MAGIC                                              700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='Y'                                                   700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = AG_MAGIC                                              700
         AG_BIN=.001                                                      700
         AG_FHMIN=AG_MAGIC                                                700
         AG_FHMAX=AG_MAGIC                                                700
         AG_OPTION='*'                                                    700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='X'                                                   700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = AG_MAGIC                                              700
         AG_BIN=.001                                                      700
         AG_FHMIN=AG_MAGIC                                                700
         AG_FHMAX=AG_MAGIC                                                700
         AG_OPTION='*'                                                    700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='PTOT'                                                700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = 18                                                    700
         AG_BIN=AG_MAGIC                                                  700
         AG_OPTION='*'                                                    700
         AG_FHMIN = 0                                                     700
         AG_FHMAX = 100                                                   700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='CX'                                                  700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = 10                                                    700
         AG_BIN=AG_MAGIC                                                  700
         AG_FHMIN=AG_MAGIC                                                700
         AG_FHMAX=AG_MAGIC                                                700
         AG_OPTION='*'                                                    700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='CY'                                                  700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = 10                                                    700
         AG_BIN=AG_MAGIC                                                  700
         AG_FHMIN=AG_MAGIC                                                700
         AG_FHMAX=AG_MAGIC                                                700
         AG_OPTION='*'                                                    700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='CZ'                                                  700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = 10                                                    700
         AG_BIN=AG_MAGIC                                                  700
         AG_FHMIN=AG_MAGIC                                                700
         AG_FHMAX=AG_MAGIC                                                700
         AG_OPTION='*'                                                    700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='SLENG'                                               700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = 16                                                    700
         AG_BIN=AG_MAGIC                                                  700
         AG_OPTION='*'                                                    700
         AG_FHMIN = 0                                                     700
         AG_FHMAX = 500                                                   700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='TOF'                                                 700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = 16                                                    700
         AG_BIN=AG_MAGIC                                                  700
         AG_OPTION='*'                                                    700
         AG_FHMIN = 0                                                     700
         AG_FHMAX = 1.0E-6                                                700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='STEP'                                                700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = AG_MAGIC                                              700
         AG_BIN=.01                                                       700
         AG_FHMIN=AG_MAGIC                                                700
         AG_FHMAX=AG_MAGIC                                                700
         AG_OPTION='*'                                                    700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         AG_PARLIST='ELOSS'                                               700
         AG_TITLE = 'DETH'                                                700
         AG_NBITS = 22                                                    700
         AG_BIN=AG_MAGIC                                                  700
         AG_OPTION='*'                                                    700
         AG_FHMIN = 0.                                                    700
         AG_FHMAX = 0.01                                                  700
         CALL AGSDETH (0)                                                 700
C    *                                                                    700
         CALL AGSDETU('SVTD')                                             700
C    *                                                                    700
5761  CALL AGSPOPB                                                        701
      GO TO BLOCK_SVTD, (5760 ,5770)                                      701
C End of Local Procedure                                                  701
C ----------------------------------------------------------------------  702
C ------------ SSID                                                       702
C Begin Local Procedure SSID                                              703
C SSID IS A NON-SENSITIVE LEFT-RIGHT BORDER OF THE WAFER                  703
5800  CONTINUE                                                            703
         AG_TITLE='SSID'                                                  703
         AG_MODULE=MODULE                                                 703
         AG_MARK='V00'                                                    703
         CALL AGSPUSH                                                     703
         AG_MODULE=MODULE                                                 703
C    *                                                                    703
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          704
         CALL AGSRESET                                                    704
         AG_TITLE = 'SILICON'                                             704
         AG_EXNAME = 'MATERIAL'                                           704
         AG_PARLIST = 'NONE'                                              704
         CALL AXMATERIAL                                                  704
C    *                                                                    704
C    Geant MATERIAL : NONSENSSI ===> ISVOL=0 <===                         704
         CALL AGSRESET                                                    705
         AG_ISVOL = 0                                                     705
         AG_TITLE = 'NONSENSSI'                                           705
         AG_EXNAME = 'MATERIAL'                                           705
         AG_PARLIST = 'ISVOL'                                             705
         CALL AXMATERIAL                                                  705
C    *                                                                    705
C    Geant ATTRIBUTE : SSID ===> SEEN=1 COLO=1 <===                       705
         CALL AGSRESET                                                    706
         AG_SEEN = 1                                                      706
         AG_COLO = 1                                                      706
         AG_TITLE = 'SSID'                                                706
         AG_EXNAME = 'ATTRIBUTE'                                          706
         AG_PARLIST = 'SEEN_COLO'                                         706
         CALL AXATTRIBUTE                                                 706
C    *                                                                    706
C    Geant SHAPE : BOX ===> DX=(SWCA_WAFERLEN-SWCA_SENSLEN)/4.0, DY=SWCA  706
C    _WAFERWID/2.0 <===                                                   706
         CALL AGSRESET                                                    708
         AG_DX = (SWCA_WAFERLEN-SWCA_SENSLEN)/4.0                         708
         AG_DY = SWCA_WAFERWID/2.0                                        708
         AG_TITLE = 'BOX'                                                 708
         AG_EXNAME = 'SHAPE'                                              708
         AG_PARLIST = 'DX_DY'                                             708
         CALL AXSHAPE                                                     708
C    *                                                                    708
5801  CALL AGSPOPB                                                        709
      GO TO BLOCK_SSID, (5800 ,5810)                                      709
C End of Local Procedure                                                  709
C ----------------------------------------------------------------------  710
C ------------ SSIR                                                       710
C Begin Local Procedure SSIR                                              711
C SSIR IS A NON-SENSITIVE UP-DOWN BORDER OF THE WAFER                     711
5820  CONTINUE                                                            711
         AG_TITLE='SSIR'                                                  711
         AG_MODULE=MODULE                                                 711
         AG_MARK='V00'                                                    711
         CALL AGSPUSH                                                     711
         AG_MODULE=MODULE                                                 711
C    *                                                                    711
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          712
         CALL AGSRESET                                                    712
         AG_TITLE = 'SILICON'                                             712
         AG_EXNAME = 'MATERIAL'                                           712
         AG_PARLIST = 'NONE'                                              712
         CALL AXMATERIAL                                                  712
C    *                                                                    712
C    Geant MATERIAL : NONSENSSIL ===> ISVOL=0 <===                        712
         CALL AGSRESET                                                    713
         AG_ISVOL = 0                                                     713
         AG_TITLE = 'NONSENSSIL'                                          713
         AG_EXNAME = 'MATERIAL'                                           713
         AG_PARLIST = 'ISVOL'                                             713
         CALL AXMATERIAL                                                  713
C    *                                                                    713
C    Geant ATTRIBUTE : SSIR ===> SEEN=1 COLO=1 <===                       713
         CALL AGSRESET                                                    714
         AG_SEEN = 1                                                      714
         AG_COLO = 1                                                      714
         AG_TITLE = 'SSIR'                                                714
         AG_EXNAME = 'ATTRIBUTE'                                          714
         AG_PARLIST = 'SEEN_COLO'                                         714
         CALL AXATTRIBUTE                                                 714
C    *                                                                    714
C    Geant SHAPE : BOX ===> DX=SWCA_SENSLEN/2.0 DY=(SWCA_WAFERWID-SWCA_S  714
C    ENSWID)/4.0 <===                                                     714
         CALL AGSRESET                                                    715
         AG_DX = SWCA_SENSLEN/2.0                                         715
         AG_DY = (SWCA_WAFERWID-SWCA_SENSWID)/4.0                         715
         AG_TITLE = 'BOX'                                                 715
         AG_EXNAME = 'SHAPE'                                              715
         AG_PARLIST = 'DX_DY'                                             715
         CALL AXSHAPE                                                     715
C    *                                                                    715
5821  CALL AGSPOPB                                                        716
      GO TO BLOCK_SSIR, (5820 ,5830)                                      716
C End of Local Procedure                                                  716
C ----------------------------------------------------------------------  717
C ------------ STRA                                                       717
C Begin Local Procedure STRA                                              718
C STRA IS A TRAPEZOID OF TRIANGULAR SHAPE                                 718
5780  CONTINUE                                                            718
         AG_TITLE='STRA'                                                  718
         AG_MODULE=MODULE                                                 718
         AG_MARK='V00'                                                    718
         CALL AGSPUSH                                                     718
         AG_MODULE=MODULE                                                 718
C    *                                                                    718
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          719
         CALL AGSRESET                                                    719
         AG_TITLE = 'SILICON'                                             719
         AG_EXNAME = 'MATERIAL'                                           719
         AG_PARLIST = 'NONE'                                              719
         CALL AXMATERIAL                                                  719
C    *                                                                    719
C    Geant MATERIAL : NONSENSSILI ===> ISVOL=0 <===                       719
         CALL AGSRESET                                                    720
         AG_ISVOL = 0                                                     720
         AG_TITLE = 'NONSENSSILI'                                         720
         AG_EXNAME = 'MATERIAL'                                           720
         AG_PARLIST = 'ISVOL'                                             720
         CALL AXMATERIAL                                                  720
C    *                                                                    720
C    Geant ATTRIBUTE : STRA ===> SEEN=1 COLO=2 <===                       720
         CALL AGSRESET                                                    721
         AG_SEEN = 1                                                      721
         AG_COLO = 2                                                      721
         AG_TITLE = 'STRA'                                                721
         AG_EXNAME = 'ATTRIBUTE'                                          721
         AG_PARLIST = 'SEEN_COLO'                                         721
         CALL AXATTRIBUTE                                                 721
C    *                                                                    721
C    Geant SHAPE : TRD1 ===> DX1=0.0 DX2=SWCA_SENSWID/2.0, DY=SWCA_WAFER  721
C    THK/2.0 DZ=(SWCA_SENSLEN-SWCA_SENSGIR)/2.0 <===                      721
         CALL AGSRESET                                                    723
         AG_DX1 = 0.0                                                     723
         AG_DX2 = SWCA_SENSWID/2.0                                        723
         AG_DY = SWCA_WAFERTHK/2.0                                        723
         AG_DZ = (SWCA_SENSLEN-SWCA_SENSGIR)/2.0                          723
         AG_TITLE = 'TRD1'                                                723
         AG_EXNAME = 'SHAPE'                                              723
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     723
         CALL AXSHAPE                                                     723
C    *                                                                    723
5781  CALL AGSPOPB                                                        724
      GO TO BLOCK_STRA, (5780 ,5790)                                      724
C End of Local Procedure                                                  724
C ----------------------------------------------------------------------  725
C ------------ SBER                                                       725
C Begin Local Procedure SBER                                              726
C SBER ARE THE CARBON WAFER CARRIER RAILS                                 726
5640  CONTINUE                                                            726
         AG_TITLE='SBER'                                                  726
         AG_MODULE=MODULE                                                 726
         AG_MARK='V00'                                                    726
         CALL AGSPUSH                                                     726
         AG_MODULE=MODULE                                                 726
C    *                                                                    726
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           727
         CALL AGSRESET                                                    727
         AG_TITLE = 'CARBON'                                              727
         AG_EXNAME = 'MATERIAL'                                           727
         AG_PARLIST = 'NONE'                                              727
         CALL AXMATERIAL                                                  727
C    *                                                                    727
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       727
         CALL AGSRESET                                                    728
         AG_SEEN = 1                                                      728
         AG_COLO = 2                                                      728
         AG_TITLE = 'SBER'                                                728
         AG_EXNAME = 'ATTRIBUTE'                                          728
         AG_PARLIST = 'SEEN_COLO'                                         728
         CALL AXATTRIBUTE                                                 728
C    *                                                                    728
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  728
C    _WAFCARTH/2 <===                                                     728
         CALL AGSRESET                                                    730
         AG_DX = SWCA_WAFCARWD/2                                          730
         AG_DY = SWCA_LENGTH/2                                            730
         AG_DZ = SWCA_WAFCARTH/2                                          730
         AG_TITLE = 'BOX'                                                 730
         AG_EXNAME = 'SHAPE'                                              730
         AG_PARLIST = 'DX_DY_DZ'                                          730
         CALL AXSHAPE                                                     730
C    *                                                                    730
5641  CALL AGSPOPB                                                        731
      GO TO BLOCK_SBER, (5640 ,5650)                                      731
C End of Local Procedure                                                  731
C ----------------------------------------------------------------------  732
C ------------ STAB                                                       732
C Begin Local Procedure STAB                                              733
C STAB ARE THE CARBON WAFER CARRIER END TABS                              733
5660  CONTINUE                                                            733
         AG_TITLE='STAB'                                                  733
         AG_MODULE=MODULE                                                 733
         AG_MARK='V00'                                                    733
         CALL AGSPUSH                                                     733
         AG_MODULE=MODULE                                                 733
C    *                                                                    733
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           734
         CALL AGSRESET                                                    734
         AG_TITLE = 'CARBON'                                              734
         AG_EXNAME = 'MATERIAL'                                           734
         AG_PARLIST = 'NONE'                                              734
         CALL AXMATERIAL                                                  734
C    *                                                                    734
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       734
         CALL AGSRESET                                                    735
         AG_SEEN = 1                                                      735
         AG_COLO = 2                                                      735
         AG_TITLE = 'STAB'                                                735
         AG_EXNAME = 'ATTRIBUTE'                                          735
         AG_PARLIST = 'SEEN_COLO'                                         735
         CALL AXATTRIBUTE                                                 735
C    *                                                                    735
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  735
C    2 DZ=SWCA_WAFCARTH/2 <===                                            735
         CALL AGSRESET                                                    737
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            737
         AG_DY = TABLEN/2                                                 737
         AG_DZ = SWCA_WAFCARTH/2                                          737
         AG_TITLE = 'BOX'                                                 737
         AG_EXNAME = 'SHAPE'                                              737
         AG_PARLIST = 'DX_DY_DZ'                                          737
         CALL AXSHAPE                                                     737
C    *                                                                    737
5661  CALL AGSPOPB                                                        738
      GO TO BLOCK_STAB, (5660 ,5670)                                      738
C End of Local Procedure                                                  738
C ----------------------------------------------------------------------  739
C ------------ STRU                                                       739
C Begin Local Procedure STRU                                              740
C STRU ARE THE CARBON STRUTS BETWEEN THE WAFER CARRIER RAILS              740
5680  CONTINUE                                                            740
         AG_TITLE='STRU'                                                  740
         AG_MODULE=MODULE                                                 740
         AG_MARK='V00'                                                    740
         CALL AGSPUSH                                                     740
         AG_MODULE=MODULE                                                 740
C    *                                                                    740
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           741
         CALL AGSRESET                                                    741
         AG_TITLE = 'CARBON'                                              741
         AG_EXNAME = 'MATERIAL'                                           741
         AG_PARLIST = 'NONE'                                              741
         CALL AXMATERIAL                                                  741
C    *                                                                    741
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       741
         CALL AGSRESET                                                    742
         AG_SEEN = 1                                                      742
         AG_COLO = 2                                                      742
         AG_TITLE = 'STRU'                                                742
         AG_EXNAME = 'ATTRIBUTE'                                          742
         AG_PARLIST = 'SEEN_COLO'                                         742
         CALL AXATTRIBUTE                                                 742
C    *                                                                    742
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  742
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     742
         CALL AGSRESET                                                    744
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            744
         AG_DY = SWCA_STRUTLEN/2                                          744
         AG_DZ = SWCA_WAFCARTH/2                                          744
         AG_TITLE = 'BOX'                                                 744
         AG_EXNAME = 'SHAPE'                                              744
         AG_PARLIST = 'DX_DY_DZ'                                          744
         CALL AXSHAPE                                                     744
C    *                                                                    744
5681  CALL AGSPOPB                                                        745
      GO TO BLOCK_STRU, (5680 ,5690)                                      745
C End of Local Procedure                                                  745
C ----------------------------------------------------------------------  746
C ------------ SPCB                                                       746
C Begin Local Procedure SPCB                                              747
C SPCB IS THE G10 PCB                                                     747
5720  CONTINUE                                                            747
         AG_TITLE='SPCB'                                                  747
         AG_MODULE=MODULE                                                 747
         AG_MARK='V00'                                                    747
         CALL AGSPUSH                                                     747
         AG_MODULE=MODULE                                                 747
C    *                                                                    747
C    Geant MATERIAL : G10 ===> inherited arguments used <===              748
         CALL AGSRESET                                                    748
         AG_TITLE = 'G10'                                                 748
         AG_EXNAME = 'MATERIAL'                                           748
         AG_PARLIST = 'NONE'                                              748
         CALL AXMATERIAL                                                  748
C    *                                                                    748
C    Geant ATTRIBUTE : SPCB ===> SEEN=1 COLO=5 <===                       748
         CALL AGSRESET                                                    749
         AG_SEEN = 1                                                      749
         AG_COLO = 5                                                      749
         AG_TITLE = 'SPCB'                                                749
         AG_EXNAME = 'ATTRIBUTE'                                          749
         AG_PARLIST = 'SEEN_COLO'                                         749
         CALL AXATTRIBUTE                                                 749
C    *                                                                    749
C    Geant SHAPE : BOX ===> DX=SVTL_PCBWIDTH/2.0 DY=SVTL_PCBLEN/2.0, DZ=  749
C    SVTL_PCBTHK/2.0 <===                                                 749
         CALL AGSRESET                                                    751
         AG_DX = SVTL_PCBWIDTH/2.0                                        751
         AG_DY = SVTL_PCBLEN/2.0                                          751
         AG_DZ = SVTL_PCBTHK/2.0                                          751
         AG_TITLE = 'BOX'                                                 751
         AG_EXNAME = 'SHAPE'                                              751
         AG_PARLIST = 'DX_DY_DZ'                                          751
         CALL AXSHAPE                                                     751
C    *                                                                    751
5721  CALL AGSPOPB                                                        752
      GO TO BLOCK_SPCB, (5720 ,5730)                                      752
C End of Local Procedure                                                  752
C ----------------------------------------------------------------------  753
C ------------ SCBM                                                       753
C Begin Local Procedure SCBM                                              754
C SCBM IS THE MOTHER FOR THE BUNDLE OF CABLES GOING FROM PCBS             754
5390  CONTINUE                                                            754
         AG_TITLE='SCBM'                                                  754
         AG_MODULE=MODULE                                                 754
         AG_MARK='V00'                                                    754
         CALL AGSPUSH                                                     754
         AG_MODULE=MODULE                                                 754
C    *                                                                    754
C    Geant MATERIAL : AIR ===> inherited arguments used <===              755
         CALL AGSRESET                                                    755
         AG_TITLE = 'AIR'                                                 755
         AG_EXNAME = 'MATERIAL'                                           755
         AG_PARLIST = 'NONE'                                              755
         CALL AXMATERIAL                                                  755
C    *                                                                    755
C    Geant ATTRIBUTE : SCBM ===> SEEN=0 COLO=1 <===                       755
         CALL AGSRESET                                                    756
         AG_SEEN = 0                                                      756
         AG_COLO = 1                                                      756
         AG_TITLE = 'SCBM'                                                756
         AG_EXNAME = 'ATTRIBUTE'                                          756
         AG_PARLIST = 'SEEN_COLO'                                         756
         CALL AXATTRIBUTE                                                 756
C    *                                                                    756
C    Geant SHAPE : TUBE ===> RMIN=RADII(1) RMAX=SWAM_RMAX, DZ=SCBP_LEN <  756
C    ===                                                                  756
         CALL AGSRESET                                                    758
         AG_RMIN = RADII(1)                                               758
         AG_RMAX = SWAM_RMAX                                              758
         AG_DZ = SCBP_LEN                                                 758
         AG_TITLE = 'TUBE'                                                758
         AG_EXNAME = 'SHAPE'                                              758
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      758
         CALL AXSHAPE                                                     758
C    *                                                                    758
C    *                                                                    758
         ILAYER=1                                                         759
            GO TO 5843                                                    759
5841        ILAYER=ILAYER+1                                               759
5843        IF(ILAYER-(4).GT.0) GO TO 5842                                759
C       use SCBP bank                                                     760
            BPATH_SCBP='SCBP*'                                            760
            IDNUM_SCBP(1)=0                                               760
            SCBP_LAYER=ILAYER                                             760
            CALL AGDGETP('SVTTGEO11','*',-999,'SCBP',NSCBP_LAYER,         760
     *      RE_SCBP_LAYER,BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,    760
     *      BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)             760
C       Create SCBL :                                                     761
C       Call To Local Procedure SCBL                                      761
            ASSIGN 5860 TO BLOCK_SCBL                                     761
            GO TO  5850                                                   761
5860        CONTINUE                                                      761
C       *                                                                 761
C       Geant POSITION : SCBL ===> X=0.0 Y=0.0 Z=0.0 <===                 762
            CALL AGSRESET                                                 762
            AG_X = 0.0                                                    762
            AG_Y = 0.0                                                    762
            AG_Z = 0.0                                                    762
            AG_TITLE = 'SCBL'                                             762
            AG_EXNAME = 'POSITION'                                        762
            AG_PARLIST = 'X_Y_Z'                                          762
            CALL AXPOSITION                                               762
C       *                                                                 762
         GO TO 5841                                                       763
5842     CONTINUE                                                         763
C    *                                                                    764
         ILAYER=1                                                         764
            GO TO 5873                                                    764
5871        ILAYER=ILAYER+1                                               764
5873        IF(ILAYER-(3).GT.0) GO TO 5872                                764
C       use SFEP bank                                                     765
            BPATH_SFEP='SFEP*'                                            765
            IDNUM_SFEP(1)=0                                               765
            SFEP_LAYER=ILAYER                                             765
            CALL AGDGETP('SVTTGEO11','*',-999,'SFEP',NSFEP_LAYER,         765
     *      RE_SFEP_LAYER,BPATH_SFEP,IDNUM_SFEP,1,MAP_SFEP,NAMES_SFEP,    765
     *      BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)             765
C       Create SFED :                                                     766
C       Call To Local Procedure SFED                                      766
            ASSIGN 5890 TO BLOCK_SFED                                     766
            GO TO  5880                                                   766
5890        CONTINUE                                                      766
C       *                                                                 766
C       Geant POSITION : SFED ===> X=0.0 Y=0.0 Z=0.0 <===                 767
            CALL AGSRESET                                                 767
            AG_X = 0.0                                                    767
            AG_Y = 0.0                                                    767
            AG_Z = 0.0                                                    767
            AG_TITLE = 'SFED'                                             767
            AG_EXNAME = 'POSITION'                                        767
            AG_PARLIST = 'X_Y_Z'                                          767
            CALL AXPOSITION                                               767
C       *                                                                 767
C       Create SPLS :                                                     768
C       Call To Local Procedure SPLS                                      768
            ASSIGN 5910 TO BLOCK_SPLS                                     768
            GO TO  5900                                                   768
5910        CONTINUE                                                      768
C       *                                                                 768
C       Geant POSITION : SPLS ===> X=0.0 Y=0.0 Z=0.0 <===                 769
            CALL AGSRESET                                                 769
            AG_X = 0.0                                                    769
            AG_Y = 0.0                                                    769
            AG_Z = 0.0                                                    769
            AG_TITLE = 'SPLS'                                             769
            AG_EXNAME = 'POSITION'                                        769
            AG_PARLIST = 'X_Y_Z'                                          769
            CALL AXPOSITION                                               769
C       *                                                                 769
         GO TO 5871                                                       770
5872     CONTINUE                                                         770
5391  CALL AGSPOPB                                                        771
      GO TO BLOCK_SCBM, (5390 ,5400)                                      771
C End of Local Procedure                                                  771
C ----------------------------------------------------------------------  772
C ------------ SCBL                                                       772
C Begin Local Procedure SCBL                                              773
C SCBL IS THE BUNDLE OF CABLES GOING FROM PCBS TO MANIFOLDS               773
5850  CONTINUE                                                            773
         AG_TITLE='SCBL'                                                  773
         AG_MODULE=MODULE                                                 773
         AG_MARK='V00'                                                    773
         CALL AGSPUSH                                                     773
         AG_MODULE=MODULE                                                 773
C    *                                                                    773
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           774
         CALL AGSRESET                                                    774
         AG_TITLE = 'COPPER'                                              774
         AG_EXNAME = 'MATERIAL'                                           774
         AG_PARLIST = 'NONE'                                              774
         CALL AXMATERIAL                                                  774
C    *                                                                    774
C    Geant ATTRIBUTE : SCBL ===> SEEN=1 COLO=3 <===                       774
         CALL AGSRESET                                                    775
         AG_SEEN = 1                                                      775
         AG_COLO = 3                                                      775
         AG_TITLE = 'SCBL'                                                775
         AG_EXNAME = 'ATTRIBUTE'                                          775
         AG_PARLIST = 'SEEN_COLO'                                         775
         CALL AXATTRIBUTE                                                 775
C    *                                                                    775
         SQ =(SCBP_LEN*SCBP_LEN)                                          775
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  777
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  778
         A = PI                                                           779
         A=A*(SCBP_RMIN1*SCBP_RMIN1 + SCBP_RMIN2*SCBP_RMIN2)              780
         A=A*SQRT(1 + SQ)                                                 781
         CUTHK=(SCBP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            782
C    Geant SHAPE : CONE ===> DZ=SCBP_LEN RMN1=SCBP_RMIN1 RMX1=SCBP_RMIN1  784
C    +CUTHK, RMN2=SCBP_RMIN2 RMX2=SCBP_RMIN2+CUTHK <===                   784
         CALL AGSRESET                                                    784
         AG_DZ = SCBP_LEN                                                 784
         AG_RMN1 = SCBP_RMIN1                                             784
         AG_RMX1 = SCBP_RMIN1+CUTHK                                       784
         AG_RMN2 = SCBP_RMIN2                                             784
         AG_RMX2 = SCBP_RMIN2+CUTHK                                       784
         AG_TITLE = 'CONE'                                                784
         AG_EXNAME = 'SHAPE'                                              784
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            784
         CALL AXSHAPE                                                     784
C    *                                                                    784
5851  CALL AGSPOPB                                                        785
      GO TO BLOCK_SCBL, (5850 ,5860)                                      785
C End of Local Procedure                                                  785
C ----------------------------------------------------------------------  786
C ------------ SFED                                                       786
C Begin Local Procedure SFED                                              787
C SFED IS THE WATRER IN THE BUNDLE OF PIPES GOING TO MANIFOLDS            787
5880  CONTINUE                                                            787
         AG_TITLE='SFED'                                                  787
         AG_MODULE=MODULE                                                 787
         AG_MARK='V00'                                                    787
         CALL AGSPUSH                                                     787
         AG_MODULE=MODULE                                                 787
C    *                                                                    787
C    Geant MATERIAL : WATER ===> inherited arguments used <===            788
         CALL AGSRESET                                                    788
         AG_TITLE = 'WATER'                                               788
         AG_EXNAME = 'MATERIAL'                                           788
         AG_PARLIST = 'NONE'                                              788
         CALL AXMATERIAL                                                  788
C    *                                                                    788
C    Geant ATTRIBUTE : SFED ===> SEEN=1 COLO=1 <===                       788
         CALL AGSRESET                                                    789
         AG_SEEN = 1                                                      789
         AG_COLO = 1                                                      789
         AG_TITLE = 'SFED'                                                789
         AG_EXNAME = 'ATTRIBUTE'                                          789
         AG_PARLIST = 'SEEN_COLO'                                         789
         CALL AXATTRIBUTE                                                 789
C    *                                                                    789
         SQ = SFEP_LEN*SFEP_LEN                                           789
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  791
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  792
         A = PI                                                           793
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            794
         A=A*SQRT(1 + SQ)                                                 795
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            796
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1 RMX1=SFEP_RMIN1  798
C    +CUTHK, RMN2=SFEP_RMIN2 RMX2=SFEP_RMIN2+CUTHK <===                   798
         CALL AGSRESET                                                    798
         AG_DZ = SFEP_LEN                                                 798
         AG_RMN1 = SFEP_RMIN1                                             798
         AG_RMX1 = SFEP_RMIN1+CUTHK                                       798
         AG_RMN2 = SFEP_RMIN2                                             798
         AG_RMX2 = SFEP_RMIN2+CUTHK                                       798
         AG_TITLE = 'CONE'                                                798
         AG_EXNAME = 'SHAPE'                                              798
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            798
         CALL AXSHAPE                                                     798
C    *                                                                    798
5881  CALL AGSPOPB                                                        799
      GO TO BLOCK_SFED, (5880 ,5890)                                      799
C End of Local Procedure                                                  799
C ----------------------------------------------------------------------  800
C ------------ SPLS                                                       800
C Begin Local Procedure SPLS                                              801
C SPLS IS THE PLASTIC WALLS OF THE BUNDLE OF PIPES GOING TO MANIFOLDS     801
5900  CONTINUE                                                            801
         AG_TITLE='SPLS'                                                  801
         AG_MODULE=MODULE                                                 801
         AG_MARK='V00'                                                    801
         CALL AGSPUSH                                                     801
         AG_MODULE=MODULE                                                 801
C    *                                                                    801
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           802
         CALL AGSRESET                                                    802
         AG_A = 12                                                        802
         AG_Z = 6                                                         802
         AG_W = 1                                                         802
         AG_TITLE = 'C'                                                   802
         AG_EXNAME = 'COMPONENT'                                          802
         AG_PARLIST = 'A_Z_W'                                             802
         CALL AXCOMPONENT                                                 802
C    *                                                                    802
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           802
         CALL AGSRESET                                                    803
         AG_A = 1                                                         803
         AG_Z = 1                                                         803
         AG_W = 2                                                         803
         AG_TITLE = 'H2'                                                  803
         AG_EXNAME = 'COMPONENT'                                          803
         AG_PARLIST = 'A_Z_W'                                             803
         CALL AXCOMPONENT                                                 803
C    *                                                                    803
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             803
         CALL AGSRESET                                                    804
         AG_DENS = 0.935                                                  804
         AG_TITLE = 'CH2'                                                 804
         AG_EXNAME = 'MIXTURE'                                            804
         AG_PARLIST = 'DENS'                                              804
         CALL AXMIXTURE                                                   804
C    *                                                                    804
C    Geant ATTRIBUTE : SPLS ===> SEEN=1 COLO=6 <===                       804
         CALL AGSRESET                                                    805
         AG_SEEN = 1                                                      805
         AG_COLO = 6                                                      805
         AG_TITLE = 'SPLS'                                                805
         AG_EXNAME = 'ATTRIBUTE'                                          805
         AG_PARLIST = 'SEEN_COLO'                                         805
         CALL AXATTRIBUTE                                                 805
C    *                                                                    805
         SQ = SFEP_LEN*SFEP_LEN                                           805
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  807
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  808
         A = PI                                                           809
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            810
         A=A*SQRT(1 + SQ)                                                 811
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            812
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1+0.2, RMX1=SFEP_  815
C    RMIN1+0.2+CUTHK RMN2=SFEP_RMIN2+0.2, RMX2=SFEP_RMIN2+0.2+CUTHK <===  815
         CALL AGSRESET                                                    815
         AG_DZ = SFEP_LEN                                                 815
         AG_RMN1 = SFEP_RMIN1+0.2                                         815
         AG_RMX1 = SFEP_RMIN1+0.2+CUTHK                                   815
         AG_RMN2 = SFEP_RMIN2+0.2                                         815
         AG_RMX2 = SFEP_RMIN2+0.2+CUTHK                                   815
         AG_TITLE = 'CONE'                                                815
         AG_EXNAME = 'SHAPE'                                              815
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            815
         CALL AXSHAPE                                                     815
C    *                                                                    815
5901  CALL AGSPOPB                                                        816
      GO TO BLOCK_SPLS, (5900 ,5910)                                      816
C End of Local Procedure                                                  816
C ----------------------------------------------------------------------  817
C ------------ SELE                                                       817
C Begin Local Procedure SELE                                              818
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   818
5590  CONTINUE                                                            818
         AG_TITLE='SELE'                                                  818
         AG_MODULE=MODULE                                                 818
         AG_MARK='V00'                                                    818
         CALL AGSPUSH                                                     818
         AG_MODULE=MODULE                                                 818
C    *                                                                    818
C    Geant MATERIAL : AIR ===> inherited arguments used <===              819
         CALL AGSRESET                                                    819
         AG_TITLE = 'AIR'                                                 819
         AG_EXNAME = 'MATERIAL'                                           819
         AG_PARLIST = 'NONE'                                              819
         CALL AXMATERIAL                                                  819
C    *                                                                    819
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 SERIAL=ILAYER <===         819
         CALL AGSRESET                                                    820
         AG_SEEN = 0                                                      820
         AG_COLO = 1                                                      820
         AG_SERIAL = ILAYER                                               820
         AG_TITLE = 'SELE'                                                820
         AG_EXNAME = 'ATTRIBUTE'                                          820
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  820
         CALL AXATTRIBUTE                                                 820
C    *                                                                    820
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  820
C    K/2 <===                                                             820
         CALL AGSRESET                                                    822
         AG_DX = SELC_ELCAWID/2                                           822
         AG_DY = SWCA_LENGTH/2                                            822
         AG_DZ = ELETHK/2                                                 822
         AG_TITLE = 'BOX'                                                 822
         AG_EXNAME = 'SHAPE'                                              822
         AG_PARLIST = 'DX_DY_DZ'                                          822
         CALL AXSHAPE                                                     822
C    *                                                                    822
C    Create SWCH :                                                        823
C    Call To Local Procedure SWCH                                         823
         ASSIGN 5930 TO BLOCK_SWCH                                        823
         GO TO  5920                                                      823
5930     CONTINUE                                                         823
C    *                                                                    823
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             824
         CALL AGSRESET                                                    824
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    824
         AG_TITLE = 'SWCH'                                                824
         AG_EXNAME = 'POSITION'                                           824
         AG_PARLIST = 'Z'                                                 824
         CALL AXPOSITION                                                  824
C    *                                                                    824
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  824
C    BETHK/2 <===                                                         824
         CALL AGSRESET                                                    825
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             825
         AG_TITLE = 'SWCH'                                                825
         AG_EXNAME = 'POSITION'                                           825
         AG_PARLIST = 'Z'                                                 825
         CALL AXPOSITION                                                  825
C    *                                                                    825
C    Create SWCS :                                                        826
C    Call To Local Procedure SWCS                                         826
         ASSIGN 5950 TO BLOCK_SWCS                                        826
         GO TO  5940                                                      826
5950     CONTINUE                                                         826
C    *                                                                    826
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  828
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     828
         CALL AGSRESET                                                    828
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              828
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        828
         AG_TITLE = 'SWCS'                                                828
         AG_EXNAME = 'POSITION'                                           828
         AG_PARLIST = 'X_Z'                                               828
         CALL AXPOSITION                                                  828
C    *                                                                    828
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  828
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     828
         CALL AGSRESET                                                    830
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              830
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        830
         AG_TITLE = 'SWCS'                                                830
         AG_EXNAME = 'POSITION'                                           830
         AG_PARLIST = 'X_Z'                                               830
         CALL AXPOSITION                                                  830
C    *                                                                    830
C    Create SWCW :                                                        831
C    Call To Local Procedure SWCW                                         831
         ASSIGN 5970 TO BLOCK_SWCW                                        831
         GO TO  5960                                                      831
5970     CONTINUE                                                         831
C    *                                                                    831
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  832
C    =                                                                    832
         CALL AGSRESET                                                    832
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        832
         AG_TITLE = 'SWCW'                                                832
         AG_EXNAME = 'POSITION'                                           832
         AG_PARLIST = 'Z'                                                 832
         CALL AXPOSITION                                                  832
C    *                                                                    832
C    Create SBOI :                                                        833
C    Call To Local Procedure SBOI                                         833
         ASSIGN 5990 TO BLOCK_SBOI                                        833
         GO TO  5980                                                      833
5990     CONTINUE                                                         833
C    *                                                                    833
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  834
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       834
         CALL AGSRESET                                                    834
         AG_Z = (ELETHK/                                                  834
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          834
         AG_TITLE = 'SBOI'                                                834
         AG_EXNAME = 'POSITION'                                           834
         AG_PARLIST = 'Z'                                                 834
         CALL AXPOSITION                                                  834
C    *                                                                    834
C    Create SGLA :                                                        835
C    Call To Local Procedure SGLA                                         835
         ASSIGN 6010 TO BLOCK_SGLA                                        835
         GO TO  6000                                                      835
6010     CONTINUE                                                         835
C    *                                                                    835
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  836
C    _GLASSTHK/2 <===                                                     836
         CALL AGSRESET                                                    836
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         836
         AG_TITLE = 'SGLA'                                                836
         AG_EXNAME = 'POSITION'                                           836
         AG_PARLIST = 'Z'                                                 836
         CALL AXPOSITION                                                  836
C    *                                                                    836
C    Create SAGP :                                                        837
C    Call To Local Procedure SAGP                                         837
         ASSIGN 6030 TO BLOCK_SAGP                                        837
         GO TO  6020                                                      837
6030     CONTINUE                                                         837
C    *                                                                    837
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  838
C    ==                                                                   838
         CALL AGSRESET                                                    838
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       838
         AG_TITLE = 'SAGP'                                                838
         AG_EXNAME = 'POSITION'                                           838
         AG_PARLIST = 'Z'                                                 838
         CALL AXPOSITION                                                  838
C    *                                                                    838
C    Create SDYE :                                                        839
C    Call To Local Procedure SDYE                                         839
         ASSIGN 6050 TO BLOCK_SDYE                                        839
         GO TO  6040                                                      839
6050     CONTINUE                                                         839
C    *                                                                    839
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  841
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     841
         CALL AGSRESET                                                    841
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  841
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    841
         AG_TITLE = 'SDYE'                                                841
         AG_EXNAME = 'POSITION'                                           841
         AG_PARLIST = 'X_Z'                                               841
         CALL AXPOSITION                                                  841
C    *                                                                    841
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  841
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             841
         CALL AGSRESET                                                    843
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          843
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    843
         AG_TITLE = 'SDYE'                                                843
         AG_EXNAME = 'POSITION'                                           843
         AG_PARLIST = 'X_Z'                                               843
         CALL AXPOSITION                                                  843
C    *                                                                    843
C    Create SECA :                                                        844
C    Call To Local Procedure SECA                                         844
         ASSIGN 6070 TO BLOCK_SECA                                        844
         GO TO  6060                                                      844
6070     CONTINUE                                                         844
C    *                                                                    844
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  846
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    846
         CALL AGSRESET                                                    846
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             846
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        846
         AG_TITLE = 'SECA'                                                846
         AG_EXNAME = 'POSITION'                                           846
         AG_PARLIST = 'X_Z'                                               846
         CALL AXPOSITION                                                  846
C    *                                                                    846
5591  CALL AGSPOPB                                                        847
      GO TO BLOCK_SELE, (5590 ,5600)                                      847
C End of Local Procedure                                                  847
C ----------------------------------------------------------------------  848
C ------------ SDYE                                                       848
C Begin Local Procedure SDYE                                              849
C SDYE IS THE IC CHIP ON THE HYBRID                                       849
6040  CONTINUE                                                            849
         AG_TITLE='SDYE'                                                  849
         AG_MODULE=MODULE                                                 849
         AG_MARK='V00'                                                    849
         CALL AGSPUSH                                                     849
         AG_MODULE=MODULE                                                 849
C    *                                                                    849
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          850
         CALL AGSRESET                                                    850
         AG_TITLE = 'SILICON'                                             850
         AG_EXNAME = 'MATERIAL'                                           850
         AG_PARLIST = 'NONE'                                              850
         CALL AXMATERIAL                                                  850
C    *                                                                    850
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 SERIAL=0 <===              850
         CALL AGSRESET                                                    851
         AG_SEEN = 1                                                      851
         AG_COLO = 6                                                      851
         AG_SERIAL = 0                                                    851
         AG_TITLE = 'SDYE'                                                851
         AG_EXNAME = 'ATTRIBUTE'                                          851
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  851
         CALL AXATTRIBUTE                                                 851
C    *                                                                    851
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  851
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           851
         CALL AGSRESET                                                    853
         AG_DX = SELC_DYEWID/2                                            853
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              853
         AG_DZ = SELC_DYETHK/2                                            853
         AG_TITLE = 'BOX'                                                 853
         AG_EXNAME = 'SHAPE'                                              853
         AG_PARLIST = 'DX_DY_DZ'                                          853
         CALL AXSHAPE                                                     853
C    *                                                                    853
6041  CALL AGSPOPB                                                        854
      GO TO BLOCK_SDYE, (6040 ,6050)                                      854
C End of Local Procedure                                                  854
C ----------------------------------------------------------------------  855
C ------------ SECA                                                       855
C Begin Local Procedure SECA                                              856
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            856
6060  CONTINUE                                                            856
         AG_TITLE='SECA'                                                  856
         AG_MODULE=MODULE                                                 856
         AG_MARK='V00'                                                    856
         CALL AGSPUSH                                                     856
         AG_MODULE=MODULE                                                 856
C    *                                                                    856
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           857
         CALL AGSRESET                                                    857
         AG_TITLE = 'COPPER'                                              857
         AG_EXNAME = 'MATERIAL'                                           857
         AG_PARLIST = 'NONE'                                              857
         CALL AXMATERIAL                                                  857
C    *                                                                    857
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 SERIAL=0 <===              857
         CALL AGSRESET                                                    858
         AG_SEEN = 1                                                      858
         AG_COLO = 2                                                      858
         AG_SERIAL = 0                                                    858
         AG_TITLE = 'SECA'                                                858
         AG_EXNAME = 'ATTRIBUTE'                                          858
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  858
         CALL AXATTRIBUTE                                                 858
C    *                                                                    858
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  858
C    ABTHK/2 <===                                                         858
         CALL AGSRESET                                                    860
         AG_DX = SELC_CABWID/2                                            860
         AG_DY = SWCA_LENGTH/2                                            860
         AG_DZ = SELC_CABTHK/2                                            860
         AG_TITLE = 'BOX'                                                 860
         AG_EXNAME = 'SHAPE'                                              860
         AG_PARLIST = 'DX_DY_DZ'                                          860
         CALL AXSHAPE                                                     860
C    *                                                                    860
6061  CALL AGSPOPB                                                        861
      GO TO BLOCK_SECA, (6060 ,6070)                                      861
C End of Local Procedure                                                  861
C ----------------------------------------------------------------------  862
C ------------ SBOI                                                       862
C Begin Local Procedure SBOI                                              863
C SBOI IS THE BERILLIA LAYER                                              863
5980  CONTINUE                                                            863
         AG_TITLE='SBOI'                                                  863
         AG_MODULE=MODULE                                                 863
         AG_MARK='V00'                                                    863
         CALL AGSPUSH                                                     863
         AG_MODULE=MODULE                                                 863
C    *                                                                    863
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1 <===                           864
         CALL AGSRESET                                                    864
         AG_A = 9                                                         864
         AG_Z = 4                                                         864
         AG_W = 1                                                         864
         AG_TITLE = 'BE'                                                  864
         AG_EXNAME = 'COMPONENT'                                          864
         AG_PARLIST = 'A_Z_W'                                             864
         CALL AXCOMPONENT                                                 864
C    *                                                                    864
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           864
         CALL AGSRESET                                                    865
         AG_A = 16                                                        865
         AG_Z = 8                                                         865
         AG_W = 1                                                         865
         AG_TITLE = 'O'                                                   865
         AG_EXNAME = 'COMPONENT'                                          865
         AG_PARLIST = 'A_Z_W'                                             865
         CALL AXCOMPONENT                                                 865
C    *                                                                    865
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              865
         CALL AGSRESET                                                    866
         AG_DENS = 2.85                                                   866
         AG_TITLE = 'BEO'                                                 866
         AG_EXNAME = 'MIXTURE'                                            866
         AG_PARLIST = 'DENS'                                              866
         CALL AXMIXTURE                                                   866
C    *                                                                    866
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 SERIAL=0 <===              866
         CALL AGSRESET                                                    867
         AG_SEEN = 1                                                      867
         AG_COLO = 6                                                      867
         AG_SERIAL = 0                                                    867
         AG_TITLE = 'SBOI'                                                867
         AG_EXNAME = 'ATTRIBUTE'                                          867
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  867
         CALL AXATTRIBUTE                                                 867
C    *                                                                    867
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  867
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          867
         CALL AGSRESET                                                    869
         AG_DX = SELC_ELCAWID/2                                           869
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              869
         AG_DZ = SELC_BEOTHK/2                                            869
         AG_TITLE = 'BOX'                                                 869
         AG_EXNAME = 'SHAPE'                                              869
         AG_PARLIST = 'DX_DY_DZ'                                          869
         CALL AXSHAPE                                                     869
C    *                                                                    869
5981  CALL AGSPOPB                                                        870
      GO TO BLOCK_SBOI, (5980 ,5990)                                      870
C End of Local Procedure                                                  870
C ----------------------------------------------------------------------  871
C ------------ SAGP                                                       871
C Begin Local Procedure SAGP                                              872
C SAGP IS THE SILVER-PALLADIUM LAYER                                      872
6020  CONTINUE                                                            872
         AG_TITLE='SAGP'                                                  872
         AG_MODULE=MODULE                                                 872
         AG_MARK='V00'                                                    872
         CALL AGSPUSH                                                     872
         AG_MODULE=MODULE                                                 872
C    *                                                                    872
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1 <===                        873
         CALL AGSRESET                                                    873
         AG_A = 108                                                       873
         AG_Z = 47                                                        873
         AG_W = 1                                                         873
         AG_TITLE = 'AG'                                                  873
         AG_EXNAME = 'COMPONENT'                                          873
         AG_PARLIST = 'A_Z_W'                                             873
         CALL AXCOMPONENT                                                 873
C    *                                                                    873
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1 <===                        873
         CALL AGSRESET                                                    874
         AG_A = 106                                                       874
         AG_Z = 46                                                        874
         AG_W = 1                                                         874
         AG_TITLE = 'PD'                                                  874
         AG_EXNAME = 'COMPONENT'                                          874
         AG_PARLIST = 'A_Z_W'                                             874
         CALL AXCOMPONENT                                                 874
C    *                                                                    874
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            874
         CALL AGSRESET                                                    875
         AG_DENS = 11.25                                                  875
         AG_TITLE = 'AGPD'                                                875
         AG_EXNAME = 'MIXTURE'                                            875
         AG_PARLIST = 'DENS'                                              875
         CALL AXMIXTURE                                                   875
C    *                                                                    875
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 SERIAL=0 <===              875
         CALL AGSRESET                                                    876
         AG_SEEN = 1                                                      876
         AG_COLO = 2                                                      876
         AG_SERIAL = 0                                                    876
         AG_TITLE = 'SAGP'                                                876
         AG_EXNAME = 'ATTRIBUTE'                                          876
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  876
         CALL AXATTRIBUTE                                                 876
C    *                                                                    876
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  876
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         876
         CALL AGSRESET                                                    878
         AG_DX = SELC_ELCAWID/2                                           878
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              878
         AG_DZ = SELC_AGPDTHK/2                                           878
         AG_TITLE = 'BOX'                                                 878
         AG_EXNAME = 'SHAPE'                                              878
         AG_PARLIST = 'DX_DY_DZ'                                          878
         CALL AXSHAPE                                                     878
C    *                                                                    878
6021  CALL AGSPOPB                                                        879
      GO TO BLOCK_SAGP, (6020 ,6030)                                      879
C End of Local Procedure                                                  879
C ----------------------------------------------------------------------  880
C ------------ SGLA                                                       880
C Begin Local Procedure SGLA                                              881
C SGLA IS THE INSULATING GLASS LAYER                                      881
6000  CONTINUE                                                            881
         AG_TITLE='SGLA'                                                  881
         AG_MODULE=MODULE                                                 881
         AG_MARK='V00'                                                    881
         CALL AGSPUSH                                                     881
         AG_MODULE=MODULE                                                 881
C    *                                                                    881
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         882
         CALL AGSRESET                                                    882
         AG_A = 28                                                        882
         AG_Z = 14                                                        882
         AG_W = 1                                                         882
         AG_TITLE = 'SI'                                                  882
         AG_EXNAME = 'COMPONENT'                                          882
         AG_PARLIST = 'A_Z_W'                                             882
         CALL AXCOMPONENT                                                 882
C    *                                                                    882
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           882
         CALL AGSRESET                                                    883
         AG_A = 16                                                        883
         AG_Z = 8                                                         883
         AG_W = 2                                                         883
         AG_TITLE = 'O'                                                   883
         AG_EXNAME = 'COMPONENT'                                          883
         AG_PARLIST = 'A_Z_W'                                             883
         CALL AXCOMPONENT                                                 883
C    *                                                                    883
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             883
         CALL AGSRESET                                                    884
         AG_DENS = 2.2                                                    884
         AG_TITLE = 'GLASS'                                               884
         AG_EXNAME = 'MIXTURE'                                            884
         AG_PARLIST = 'DENS'                                              884
         CALL AXMIXTURE                                                   884
C    *                                                                    884
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 SERIAL=0 <===              884
         CALL AGSRESET                                                    885
         AG_SEEN = 1                                                      885
         AG_COLO = 6                                                      885
         AG_SERIAL = 0                                                    885
         AG_TITLE = 'SGLA'                                                885
         AG_EXNAME = 'ATTRIBUTE'                                          885
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  885
         CALL AXATTRIBUTE                                                 885
C    *                                                                    885
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  885
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        885
         CALL AGSRESET                                                    887
         AG_DX = SELC_ELCAWID/2                                           887
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              887
         AG_DZ = SELC_GLASSTHK/2                                          887
         AG_TITLE = 'BOX'                                                 887
         AG_EXNAME = 'SHAPE'                                              887
         AG_PARLIST = 'DX_DY_DZ'                                          887
         CALL AXSHAPE                                                     887
C    *                                                                    887
6001  CALL AGSPOPB                                                        888
      GO TO BLOCK_SGLA, (6000 ,6010)                                      888
C End of Local Procedure                                                  888
C ----------------------------------------------------------------------  889
C ------------ SWCH                                                       889
C Begin Local Procedure SWCH                                              890
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      890
5920  CONTINUE                                                            890
         AG_TITLE='SWCH'                                                  890
         AG_MODULE=MODULE                                                 890
         AG_MARK='V00'                                                    890
         CALL AGSPUSH                                                     890
         AG_MODULE=MODULE                                                 890
C    *                                                                    890
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           891
         CALL AGSRESET                                                    891
         AG_TITLE = 'CARBON'                                              891
         AG_EXNAME = 'MATERIAL'                                           891
         AG_PARLIST = 'NONE'                                              891
         CALL AXMATERIAL                                                  891
C    *                                                                    891
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 SERIAL=0 <===              891
         CALL AGSRESET                                                    892
         AG_SEEN = 1                                                      892
         AG_COLO = 2                                                      892
         AG_SERIAL = 0                                                    892
         AG_TITLE = 'SWCH'                                                892
         AG_EXNAME = 'ATTRIBUTE'                                          892
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  892
         CALL AXATTRIBUTE                                                 892
C    *                                                                    892
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  892
C    BETHK/2 <===                                                         892
         CALL AGSRESET                                                    894
         AG_DX = SELC_ELCAWID/2                                           894
         AG_DY = SWCA_LENGTH/2                                            894
         AG_DZ = SELC_BETHK/2                                             894
         AG_TITLE = 'BOX'                                                 894
         AG_EXNAME = 'SHAPE'                                              894
         AG_PARLIST = 'DX_DY_DZ'                                          894
         CALL AXSHAPE                                                     894
C    *                                                                    894
5921  CALL AGSPOPB                                                        895
      GO TO BLOCK_SWCH, (5920 ,5930)                                      895
C End of Local Procedure                                                  895
C ----------------------------------------------------------------------  896
C ------------ SWCS                                                       896
C Begin Local Procedure SWCS                                              897
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                897
5940  CONTINUE                                                            897
         AG_TITLE='SWCS'                                                  897
         AG_MODULE=MODULE                                                 897
         AG_MARK='V00'                                                    897
         CALL AGSPUSH                                                     897
         AG_MODULE=MODULE                                                 897
C    *                                                                    897
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           898
         CALL AGSRESET                                                    898
         AG_TITLE = 'CARBON'                                              898
         AG_EXNAME = 'MATERIAL'                                           898
         AG_PARLIST = 'NONE'                                              898
         CALL AXMATERIAL                                                  898
C    *                                                                    898
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 SERIAL=0 <===              898
         CALL AGSRESET                                                    899
         AG_SEEN = 1                                                      899
         AG_COLO = 2                                                      899
         AG_SERIAL = 0                                                    899
         AG_TITLE = 'SWCS'                                                899
         AG_EXNAME = 'ATTRIBUTE'                                          899
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  899
         CALL AXATTRIBUTE                                                 899
C    *                                                                    899
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  899
C    TTHK/2 <===                                                          899
         CALL AGSRESET                                                    901
         AG_DX = SELC_BETHK/2                                             901
         AG_DY = SWCA_LENGTH/2                                            901
         AG_DZ = SELC_WATTHK/2                                            901
         AG_TITLE = 'BOX'                                                 901
         AG_EXNAME = 'SHAPE'                                              901
         AG_PARLIST = 'DX_DY_DZ'                                          901
         CALL AXSHAPE                                                     901
C    *                                                                    901
5941  CALL AGSPOPB                                                        902
      GO TO BLOCK_SWCS, (5940 ,5950)                                      902
C End of Local Procedure                                                  902
C ----------------------------------------------------------------------  903
C ------------ SWCW                                                       903
C Begin Local Procedure SWCW                                              904
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       904
5960  CONTINUE                                                            904
         AG_TITLE='SWCW'                                                  904
         AG_MODULE=MODULE                                                 904
         AG_MARK='V00'                                                    904
         CALL AGSPUSH                                                     904
         AG_MODULE=MODULE                                                 904
C    *                                                                    904
C    Geant MATERIAL : WATER ===> inherited arguments used <===            905
         CALL AGSRESET                                                    905
         AG_TITLE = 'WATER'                                               905
         AG_EXNAME = 'MATERIAL'                                           905
         AG_PARLIST = 'NONE'                                              905
         CALL AXMATERIAL                                                  905
C    *                                                                    905
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 SERIAL=0 <===              905
         CALL AGSRESET                                                    906
         AG_SEEN = 1                                                      906
         AG_COLO = 6                                                      906
         AG_SERIAL = 0                                                    906
         AG_TITLE = 'SWCW'                                                906
         AG_EXNAME = 'ATTRIBUTE'                                          906
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  906
         CALL AXATTRIBUTE                                                 906
C    *                                                                    906
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  906
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     906
         CALL AGSRESET                                                    908
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          908
         AG_DY = SWCA_LENGTH/2                                            908
         AG_DZ = (SELC_WATTHK/2)                                          908
         AG_TITLE = 'BOX'                                                 908
         AG_EXNAME = 'SHAPE'                                              908
         AG_PARLIST = 'DX_DY_DZ'                                          908
         CALL AXSHAPE                                                     908
C    *                                                                    908
5961  CALL AGSPOPB                                                        909
      GO TO BLOCK_SWCW, (5960 ,5970)                                      909
C End of Local Procedure                                                  909
C ----------------------------------------------------------------------  910
C ------------ SIRT                                                       910
C Begin Local Procedure SIRT                                              911
C SIRT IS THE SVT INNER END RING TUBE                                     911
5070  CONTINUE                                                            911
         AG_TITLE='SIRT'                                                  911
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
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       912
         CALL AGSRESET                                                    913
         AG_SEEN = 1                                                      913
         AG_COLO = 2                                                      913
         AG_TITLE = 'SIRT'                                                913
         AG_EXNAME = 'ATTRIBUTE'                                          913
         AG_PARLIST = 'SEEN_COLO'                                         913
         CALL AXATTRIBUTE                                                 913
C    *                                                                    913
         IR_RMIN = COS(PI/8.0)                                            913
         IR_RMIN=IR_RMIN+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/           915
     *   8.0)*SIN(PI/8.0))                                                915
         IR_RMIN=IR_RMIN*SERG_IRNGPRMN                                    916
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  918
C    DRNGTH/2 <===                                                        918
         CALL AGSRESET                                                    918
         AG_RMIN = IR_RMIN                                                918
         AG_RMAX = SERG_IRNGTRMX                                          918
         AG_DZ = SERG_ENDRNGTH/2                                          918
         AG_TITLE = 'TUBE'                                                918
         AG_EXNAME = 'SHAPE'                                              918
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      918
         CALL AXSHAPE                                                     918
C    *                                                                    918
5071  CALL AGSPOPB                                                        919
      GO TO BLOCK_SIRT, (5070 ,5080)                                      919
C End of Local Procedure                                                  919
C ----------------------------------------------------------------------  920
C ------------ SIRP                                                       920
C Begin Local Procedure SIRP                                              921
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 921
5050  CONTINUE                                                            921
         AG_TITLE='SIRP'                                                  921
         AG_MODULE=MODULE                                                 921
         AG_MARK='V00'                                                    921
         CALL AGSPUSH                                                     921
         AG_MODULE=MODULE                                                 921
C    *                                                                    921
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        922
         CALL AGSRESET                                                    922
         AG_TITLE = 'BERILLIUM'                                           922
         AG_EXNAME = 'MATERIAL'                                           922
         AG_PARLIST = 'NONE'                                              922
         CALL AXMATERIAL                                                  922
C    *                                                                    922
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       922
         CALL AGSRESET                                                    923
         AG_SEEN = 1                                                      923
         AG_COLO = 2                                                      923
         AG_TITLE = 'SIRP'                                                923
         AG_EXNAME = 'ATTRIBUTE'                                          923
         AG_PARLIST = 'SEEN_COLO'                                         923
         CALL AXATTRIBUTE                                                 923
C    *                                                                    923
         ROU = COS(PI/8.0)                                                923
         ROU=ROU+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/8.0)*SIN(PI/       925
     *   8.0))                                                            925
         ROU = SERG_IRNGTRMX/ROU                                          926
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  929
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  929
C    OU,ROU} <===                                                         929
         CALL AGSRESET                                                    929
         AG_PHI1 = 0                                                      929
         AG_DPHI = 360                                                    929
         AG_NPDIV = 8                                                     929
         AG_NZ = 2                                                        929
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        929
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        929
         AG_RMN(1)=SERG_IRNGPRMN                                          929
         AG_RMN(2)=SERG_IRNGPRMN                                          929
         AG_RMX(1)=ROU                                                    929
         AG_RMX(2)=ROU                                                    929
         AG_TITLE = 'PGON'                                                929
         AG_EXNAME = 'SHAPE'                                              929
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     929
         CALL AXSHAPE                                                     929
C    *                                                                    929
5051  CALL AGSPOPB                                                        930
      GO TO BLOCK_SIRP, (5050 ,5060)                                      930
C End of Local Procedure                                                  930
C ----------------------------------------------------------------------  931
C ------------ SOER                                                       931
C Begin Local Procedure SOER                                              932
C SOER IS THE SVT OUTER END RING                                          932
5090  CONTINUE                                                            932
         AG_TITLE='SOER'                                                  932
         AG_MODULE=MODULE                                                 932
         AG_MARK='V00'                                                    932
         CALL AGSPUSH                                                     932
         AG_MODULE=MODULE                                                 932
C    *                                                                    932
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        933
         CALL AGSRESET                                                    933
         AG_TITLE = 'BERILLIUM'                                           933
         AG_EXNAME = 'MATERIAL'                                           933
         AG_PARLIST = 'NONE'                                              933
         CALL AXMATERIAL                                                  933
C    *                                                                    933
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       933
         CALL AGSRESET                                                    934
         AG_SEEN = 1                                                      934
         AG_COLO = 2                                                      934
         AG_TITLE = 'SOER'                                                934
         AG_EXNAME = 'ATTRIBUTE'                                          934
         AG_PARLIST = 'SEEN_COLO'                                         934
         CALL AXATTRIBUTE                                                 934
C    *                                                                    934
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  934
C    ERG_ENDRNGTH/2 <===                                                  934
         CALL AGSRESET                                                    936
         AG_RMIN = SERG_ORNGRMIN                                          936
         AG_RMAX = SERG_ORNGRMAX                                          936
         AG_DZ = SERG_ENDRNGTH/2                                          936
         AG_TITLE = 'TUBE'                                                936
         AG_EXNAME = 'SHAPE'                                              936
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      936
         CALL AXSHAPE                                                     936
C    *                                                                    936
5091  CALL AGSPOPB                                                        937
      GO TO BLOCK_SOER, (5090 ,5100)                                      937
C End of Local Procedure                                                  937
C ----------------------------------------------------------------------  938
C ------------ SIES                                                       938
C Begin Local Procedure SIES                                              939
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         939
5150  CONTINUE                                                            939
         AG_TITLE='SIES'                                                  939
         AG_MODULE=MODULE                                                 939
         AG_MARK='V00'                                                    939
         CALL AGSPUSH                                                     939
         AG_MODULE=MODULE                                                 939
C    *                                                                    939
C    Geant MATERIAL : AIR ===> inherited arguments used <===              940
         CALL AGSRESET                                                    940
         AG_TITLE = 'AIR'                                                 940
         AG_EXNAME = 'MATERIAL'                                           940
         AG_PARLIST = 'NONE'                                              940
         CALL AXMATERIAL                                                  940
C    *                                                                    940
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       940
         CALL AGSRESET                                                    941
         AG_SEEN = 0                                                      941
         AG_COLO = 1                                                      941
         AG_TITLE = 'SIES'                                                941
         AG_EXNAME = 'ATTRIBUTE'                                          941
         AG_PARLIST = 'SEEN_COLO'                                         941
         CALL AXATTRIBUTE                                                 941
C    *                                                                    941
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  941
C    ACK_Z-ENDRNG_Z) <===                                                 941
         CALL AGSRESET                                                    943
         AG_RMIN = IR_RMIN                                                943
         AG_RMAX = SERG_IRNGTRMX                                          943
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   943
         AG_TITLE = 'TUBE'                                                943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      943
         CALL AXSHAPE                                                     943
C    *                                                                    943
C    Create SISM :                                                        944
C    Call To Local Procedure SISM                                         944
         ASSIGN 6090 TO BLOCK_SISM                                        944
         GO TO  6080                                                      944
6090     CONTINUE                                                         944
C    *                                                                    944
5151  CALL AGSPOPB                                                        945
      GO TO BLOCK_SIES, (5150 ,5160)                                      945
C End of Local Procedure                                                  945
C ----------------------------------------------------------------------  946
C ------------ SISM                                                       946
C Begin Local Procedure SISM                                              947
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        947
6080  CONTINUE                                                            947
         AG_TITLE='SISM'                                                  947
         AG_MODULE=MODULE                                                 947
         AG_MARK='V00'                                                    947
         CALL AGSPUSH                                                     947
         AG_MODULE=MODULE                                                 947
C    *                                                                    947
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       948
         CALL AGSRESET                                                    948
         AG_SEEN = 0                                                      948
         AG_COLO = 1                                                      948
         AG_TITLE = 'SISM'                                                948
         AG_EXNAME = 'ATTRIBUTE'                                          948
         AG_PARLIST = 'SEEN_COLO'                                         948
         CALL AXATTRIBUTE                                                 948
C    *                                                                    948
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                948
         CALL AGSRESET                                                    949
         AG_NDIV = 4                                                      949
         AG_IAXIS = 2                                                     949
         AG_C0 = 45                                                       949
         AG_TITLE = 'DIVISION'                                            949
         AG_EXNAME = 'SHAPE'                                              949
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     949
         CALL AXSHAPE                                                     949
C    *                                                                    949
C    Create SCRW :                                                        950
C    Call To Local Procedure SCRW                                         950
         ASSIGN 6110 TO BLOCK_SCRW                                        950
         GO TO  6100                                                      950
6110     CONTINUE                                                         950
C    *                                                                    950
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  951
C    ===                                                                  951
         CALL AGSRESET                                                    951
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      951
         AG_TITLE = 'SCRW'                                                951
         AG_EXNAME = 'POSITION'                                           951
         AG_PARLIST = 'X'                                                 951
         CALL AXPOSITION                                                  951
C    *                                                                    951
6081  CALL AGSPOPB                                                        952
      GO TO BLOCK_SISM, (6080 ,6090)                                      952
C End of Local Procedure                                                  952
C ----------------------------------------------------------------------  953
C ------------ SOES                                                       953
C Begin Local Procedure SOES                                              954
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         954
5130  CONTINUE                                                            954
         AG_TITLE='SOES'                                                  954
         AG_MODULE=MODULE                                                 954
         AG_MARK='V00'                                                    954
         CALL AGSPUSH                                                     954
         AG_MODULE=MODULE                                                 954
C    *                                                                    954
C    Geant MATERIAL : AIR ===> inherited arguments used <===              955
         CALL AGSRESET                                                    955
         AG_TITLE = 'AIR'                                                 955
         AG_EXNAME = 'MATERIAL'                                           955
         AG_PARLIST = 'NONE'                                              955
         CALL AXMATERIAL                                                  955
C    *                                                                    955
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       955
         CALL AGSRESET                                                    956
         AG_SEEN = 0                                                      956
         AG_COLO = 1                                                      956
         AG_TITLE = 'SOES'                                                956
         AG_EXNAME = 'ATTRIBUTE'                                          956
         AG_PARLIST = 'SEEN_COLO'                                         956
         CALL AXATTRIBUTE                                                 956
C    *                                                                    956
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  956
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           956
         CALL AGSRESET                                                    958
         AG_RMIN = SERG_ORNGRMIN                                          958
         AG_RMAX = SERG_ORNGRMAX                                          958
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   958
         AG_TITLE = 'TUBE'                                                958
         AG_EXNAME = 'SHAPE'                                              958
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      958
         CALL AXSHAPE                                                     958
C    *                                                                    958
C    Create SOSM :                                                        959
C    Call To Local Procedure SOSM                                         959
         ASSIGN 6130 TO BLOCK_SOSM                                        959
         GO TO  6120                                                      959
6130     CONTINUE                                                         959
C    *                                                                    959
5131  CALL AGSPOPB                                                        960
      GO TO BLOCK_SOES, (5130 ,5140)                                      960
C End of Local Procedure                                                  960
C ----------------------------------------------------------------------  961
C ------------ SOSM                                                       961
C Begin Local Procedure SOSM                                              962
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        962
6120  CONTINUE                                                            962
         AG_TITLE='SOSM'                                                  962
         AG_MODULE=MODULE                                                 962
         AG_MARK='V00'                                                    962
         CALL AGSPUSH                                                     962
         AG_MODULE=MODULE                                                 962
C    *                                                                    962
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       963
         CALL AGSRESET                                                    963
         AG_SEEN = 0                                                      963
         AG_COLO = 1                                                      963
         AG_TITLE = 'SOSM'                                                963
         AG_EXNAME = 'ATTRIBUTE'                                          963
         AG_PARLIST = 'SEEN_COLO'                                         963
         CALL AXATTRIBUTE                                                 963
C    *                                                                    963
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                963
         CALL AGSRESET                                                    964
         AG_NDIV = 4                                                      964
         AG_IAXIS = 2                                                     964
         AG_C0 = 45                                                       964
         AG_TITLE = 'DIVISION'                                            964
         AG_EXNAME = 'SHAPE'                                              964
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     964
         CALL AXSHAPE                                                     964
C    *                                                                    964
C    Create SCRW :                                                        965
C    Call To Local Procedure SCRW                                         965
         ASSIGN 6140 TO BLOCK_SCRW                                        965
         GO TO  6100                                                      965
6140     CONTINUE                                                         965
C    *                                                                    965
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  966
C    ===                                                                  966
         CALL AGSRESET                                                    966
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      966
         AG_TITLE = 'SCRW'                                                966
         AG_EXNAME = 'POSITION'                                           966
         AG_PARLIST = 'X'                                                 966
         CALL AXPOSITION                                                  966
C    *                                                                    966
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  966
C    ===                                                                  966
         CALL AGSRESET                                                    967
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      967
         AG_TITLE = 'SCRW'                                                967
         AG_EXNAME = 'POSITION'                                           967
         AG_PARLIST = 'X'                                                 967
         CALL AXPOSITION                                                  967
C    *                                                                    967
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  967
C    =SSUP_ERJ2Y <===                                                     967
         CALL AGSRESET                                                    968
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      968
         AG_Y = SSUP_ERJ2Y                                                968
         AG_TITLE = 'SCRW'                                                968
         AG_EXNAME = 'POSITION'                                           968
         AG_PARLIST = 'X_Y'                                               968
         CALL AXPOSITION                                                  968
C    *                                                                    968
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  968
C    =-SSUP_ERJ2Y <===                                                    968
         CALL AGSRESET                                                    969
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      969
         AG_Y = -SSUP_ERJ2Y                                               969
         AG_TITLE = 'SCRW'                                                969
         AG_EXNAME = 'POSITION'                                           969
         AG_PARLIST = 'X_Y'                                               969
         CALL AXPOSITION                                                  969
C    *                                                                    969
6121  CALL AGSPOPB                                                        970
      GO TO BLOCK_SOSM, (6120 ,6130)                                      970
C End of Local Procedure                                                  970
C ----------------------------------------------------------------------  971
C ------------ SBRG                                                       971
C Begin Local Procedure SBRG                                              972
C SBRG IS THE BRACKET JOINING THE END RINGS                               972
5110  CONTINUE                                                            972
         AG_TITLE='SBRG'                                                  972
         AG_MODULE=MODULE                                                 972
         AG_MARK='V00'                                                    972
         CALL AGSPUSH                                                     972
         AG_MODULE=MODULE                                                 972
C    *                                                                    972
         RIN=SERG_IRNGPRMN                                                973
         ROU=SERG_ORNGRMAX                                                974
C    Geant MATERIAL : AIR ===> inherited arguments used <===              975
         CALL AGSRESET                                                    975
         AG_TITLE = 'AIR'                                                 975
         AG_EXNAME = 'MATERIAL'                                           975
         AG_PARLIST = 'NONE'                                              975
         CALL AXMATERIAL                                                  975
C    *                                                                    975
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       975
         CALL AGSRESET                                                    976
         AG_SEEN = 0                                                      976
         AG_COLO = 1                                                      976
         AG_TITLE = 'SBRG'                                                976
         AG_EXNAME = 'ATTRIBUTE'                                          976
         AG_PARLIST = 'SEEN_COLO'                                         976
         CALL AXATTRIBUTE                                                 976
C    *                                                                    976
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      976
         CALL AGSRESET                                                    977
         AG_RMIN = RIN                                                    977
         AG_RMAX = ROU                                                    977
         AG_DZ = SSUP_ERJTHK/2                                            977
         AG_TITLE = 'TUBE'                                                977
         AG_EXNAME = 'SHAPE'                                              977
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      977
         CALL AXSHAPE                                                     977
C    *                                                                    977
C    Create SBRM :                                                        978
C    Call To Local Procedure SBRM                                         978
         ASSIGN 6160 TO BLOCK_SBRM                                        978
         GO TO  6150                                                      978
6160     CONTINUE                                                         978
C    *                                                                    978
5111  CALL AGSPOPB                                                        979
      GO TO BLOCK_SBRG, (5110 ,5120)                                      979
C End of Local Procedure                                                  979
C ----------------------------------------------------------------------  980
C ------------ SBRM                                                       980
C Begin Local Procedure SBRM                                              981
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          981
6150  CONTINUE                                                            981
         AG_TITLE='SBRM'                                                  981
         AG_MODULE=MODULE                                                 981
         AG_MARK='V00'                                                    981
         CALL AGSPUSH                                                     981
         AG_MODULE=MODULE                                                 981
C    *                                                                    981
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       982
         CALL AGSRESET                                                    982
         AG_SEEN = 0                                                      982
         AG_COLO = 1                                                      982
         AG_TITLE = 'SBRM'                                                982
         AG_EXNAME = 'ATTRIBUTE'                                          982
         AG_PARLIST = 'SEEN_COLO'                                         982
         CALL AXATTRIBUTE                                                 982
C    *                                                                    982
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                982
         CALL AGSRESET                                                    983
         AG_NDIV = 4                                                      983
         AG_IAXIS = 2                                                     983
         AG_C0 = 45                                                       983
         AG_TITLE = 'DIVISION'                                            983
         AG_EXNAME = 'SHAPE'                                              983
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     983
         CALL AXSHAPE                                                     983
C    *                                                                    983
C    Create SBRI :                                                        984
C    Call To Local Procedure SBRI                                         984
         ASSIGN 6180 TO BLOCK_SBRI                                        984
         GO TO  6170                                                      984
6180     CONTINUE                                                         984
C    *                                                                    984
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        985
         CALL AGSRESET                                                    985
         AG_X = SSUP_ERJRAD                                               985
         AG_TITLE = 'SBRI'                                                985
         AG_EXNAME = 'POSITION'                                           985
         AG_PARLIST = 'X'                                                 985
         CALL AXPOSITION                                                  985
C    *                                                                    985
6151  CALL AGSPOPB                                                        986
      GO TO BLOCK_SBRM, (6150 ,6160)                                      986
C End of Local Procedure                                                  986
C ----------------------------------------------------------------------  987
C ------------ SBRI                                                       987
C Begin Local Procedure SBRI                                              988
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               988
6170  CONTINUE                                                            988
         AG_TITLE='SBRI'                                                  988
         AG_MODULE=MODULE                                                 988
         AG_MARK='V00'                                                    988
         CALL AGSPUSH                                                     988
         AG_MODULE=MODULE                                                 988
C    *                                                                    988
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       989
         CALL AGSRESET                                                    989
         AG_SEEN = 1                                                      989
         AG_COLO = 2                                                      989
         AG_TITLE = 'SBRI'                                                989
         AG_EXNAME = 'ATTRIBUTE'                                          989
         AG_PARLIST = 'SEEN_COLO'                                         989
         CALL AXATTRIBUTE                                                 989
C    *                                                                    989
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        989
         CALL AGSRESET                                                    990
         AG_TITLE = 'BERILLIUM'                                           990
         AG_EXNAME = 'MATERIAL'                                           990
         AG_PARLIST = 'NONE'                                              990
         CALL AXMATERIAL                                                  990
C    *                                                                    990
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  990
C    RJTHK/2 <===                                                         990
         CALL AGSRESET                                                    992
         AG_DX = SSUP_ERJLEN/2                                            992
         AG_DY = SSUP_ERJWID/2                                            992
         AG_DZ = SSUP_ERJTHK/2                                            992
         AG_TITLE = 'BOX'                                                 992
         AG_EXNAME = 'SHAPE'                                              992
         AG_PARLIST = 'DX_DY_DZ'                                          992
         CALL AXSHAPE                                                     992
C    *                                                                    992
6171  CALL AGSPOPB                                                        993
      GO TO BLOCK_SBRI, (6170 ,6180)                                      993
C End of Local Procedure                                                  993
C ----------------------------------------------------------------------  994
C ------------ SROD                                                       994
C Begin Local Procedure SROD                                              995
C SROD IS THE SVT CARBON COMPOSITE SUPPORT ROD                            995
5250  CONTINUE                                                            995
         AG_TITLE='SROD'                                                  995
         AG_MODULE=MODULE                                                 995
         AG_MARK='V00'                                                    995
         CALL AGSPUSH                                                     995
         AG_MODULE=MODULE                                                 995
C    *                                                                    995
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           996
         CALL AGSRESET                                                    996
         AG_TITLE = 'CARBON'                                              996
         AG_EXNAME = 'MATERIAL'                                           996
         AG_PARLIST = 'NONE'                                              996
         CALL AXMATERIAL                                                  996
C    *                                                                    996
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       996
         CALL AGSRESET                                                    997
         AG_SEEN = 1                                                      997
         AG_COLO = 2                                                      997
         AG_TITLE = 'SROD'                                                997
         AG_EXNAME = 'ATTRIBUTE'                                          997
         AG_PARLIST = 'SEEN_COLO'                                         997
         CALL AXATTRIBUTE                                                 997
C    *                                                                    997
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  997
C    P_RODLEN/2 <===                                                      997
         CALL AGSRESET                                                    999
         AG_RMIN = SSUP_RODID/2                                           999
         AG_RMAX = SSUP_RODOD/2                                           999
         AG_DZ = SSUP_RODLEN/2                                            999
         AG_TITLE = 'TUBE'                                                999
         AG_EXNAME = 'SHAPE'                                              999
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      999
         CALL AXSHAPE                                                     999
C    *                                                                    999
5251  CALL AGSPOPB                                                        1000
      GO TO BLOCK_SROD, (5250 ,5260)                                      1000
C End of Local Procedure                                                  1000
C ----------------------------------------------------------------------  1001
C ------------ SCON                                                       1001
C Begin Local Procedure SCON                                              1002
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               1002
5210  CONTINUE                                                            1002
         AG_TITLE='SCON'                                                  1002
         AG_MODULE=MODULE                                                 1002
         AG_MARK='V00'                                                    1002
         CALL AGSPUSH                                                     1002
         AG_MODULE=MODULE                                                 1002
C    *                                                                    1002
         CONE_THK1=SSUP_GRPHTHK                                           1003
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                1004
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              1005
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1006
         CALL AGSRESET                                                    1006
         AG_TITLE = 'AIR'                                                 1006
         AG_EXNAME = 'MATERIAL'                                           1006
         AG_PARLIST = 'NONE'                                              1006
         CALL AXMATERIAL                                                  1006
C    *                                                                    1006
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       1006
         CALL AGSRESET                                                    1007
         AG_SEEN = 0                                                      1007
         AG_COLO = 1                                                      1007
         AG_TITLE = 'SCON'                                                1007
         AG_EXNAME = 'ATTRIBUTE'                                          1007
         AG_PARLIST = 'SEEN_COLO'                                         1007
         CALL AXATTRIBUTE                                                 1007
C    *                                                                    1007
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  1007
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  1007
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  1007
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  1007
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  1007
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  1007
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      1007
         CALL AGSRESET                                                    1017
         AG_PHI1 = 0                                                      1017
         AG_DPHI = 360                                                    1017
         AG_NZ = 7                                                        1017
         AG_ZI(1)=SSUP_CONE1ZMN                                           1017
         AG_ZI(2)=SSUP_RODLEN/2                                           1017
         AG_ZI(3)=SSUP_RODLEN/2                                           1017
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1017
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1017
         AG_ZI(6)=SSUP_CONE3ZMX                                           1017
         AG_ZI(7)=SSUP_CONE4ZMX                                           1017
         AG_RMN(1)=SSUP_CON1IDMN                                          1017
         AG_RMN(2)=SSUP_CON1IDMN                                          1017
         AG_RMN(3)=SSUP_CON1IDMN                                          1017
         AG_RMN(4)=SSUP_CON1IDMN                                          1017
         AG_RMN(5)=SSUP_CON3IDMN                                          1017
         AG_RMN(6)=SSUP_CON4IDMN                                          1017
         AG_RMN(7)=SSUP_CON4IDMX                                          1017
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                1017
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                1017
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                1017
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                1017
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                1017
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                1017
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                1017
         AG_TITLE = 'PCON'                                                1017
         AG_EXNAME = 'SHAPE'                                              1017
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1017
         CALL AXSHAPE                                                     1017
C    *                                                                    1017
C    Create SGRA :                                                        1018
C    Call To Local Procedure SGRA                                         1018
         ASSIGN 6200 TO BLOCK_SGRA                                        1018
         GO TO  6190                                                      1018
6200     CONTINUE                                                         1018
C    *                                                                    1018
C    Geant POSITION : SGRA ===> inherited arguments used <===             1019
         CALL AGSRESET                                                    1019
         AG_TITLE = 'SGRA'                                                1019
         AG_EXNAME = 'POSITION'                                           1019
         AG_PARLIST = 'NONE'                                              1019
         CALL AXPOSITION                                                  1019
C    *                                                                    1019
C    Create STAP :                                                        1020
C    Call To Local Procedure STAP                                         1020
         ASSIGN 6220 TO BLOCK_STAP                                        1020
         GO TO  6210                                                      1020
6220     CONTINUE                                                         1020
C    *                                                                    1020
C    Geant POSITION : STAP ===> inherited arguments used <===             1021
         CALL AGSRESET                                                    1021
         AG_TITLE = 'STAP'                                                1021
         AG_EXNAME = 'POSITION'                                           1021
         AG_PARLIST = 'NONE'                                              1021
         CALL AXPOSITION                                                  1021
C    *                                                                    1021
C    Create STAC :                                                        1022
C    Call To Local Procedure STAC                                         1022
         ASSIGN 6240 TO BLOCK_STAC                                        1022
         GO TO  6230                                                      1022
6240     CONTINUE                                                         1022
C    *                                                                    1022
C    Geant POSITION : STAC ===> inherited arguments used <===             1023
         CALL AGSRESET                                                    1023
         AG_TITLE = 'STAC'                                                1023
         AG_EXNAME = 'POSITION'                                           1023
         AG_PARLIST = 'NONE'                                              1023
         CALL AXPOSITION                                                  1023
C    *                                                                    1023
C    Create SHLA :                                                        1024
C    Call To Local Procedure SHLA                                         1024
         ASSIGN 6260 TO BLOCK_SHLA                                        1024
         GO TO  6250                                                      1024
6260     CONTINUE                                                         1024
C    *                                                                    1024
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  1026
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             1026
         CALL AGSRESET                                                    1026
         AG_Z = SSUP_RODLEN/                                              1026
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    1026
         AG_TITLE = 'SHLA'                                                1026
         AG_EXNAME = 'POSITION'                                           1026
         AG_PARLIST = 'Z'                                                 1026
         CALL AXPOSITION                                                  1026
C    *                                                                    1026
C    Create SHLB :                                                        1027
C    Call To Local Procedure SHLB                                         1027
         ASSIGN 6280 TO BLOCK_SHLB                                        1027
         GO TO  6270                                                      1027
6280     CONTINUE                                                         1027
C    *                                                                    1027
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  1028
C    CONE3ZMX) <===                                                       1028
         CALL AGSRESET                                                    1028
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           1028
         AG_TITLE = 'SHLB'                                                1028
         AG_EXNAME = 'POSITION'                                           1028
         AG_PARLIST = 'Z'                                                 1028
         CALL AXPOSITION                                                  1028
C    *                                                                    1028
C    Create SCMY :                                                        1029
C    Call To Local Procedure SCMY                                         1029
         ASSIGN 6300 TO BLOCK_SCMY                                        1029
         GO TO  6290                                                      1029
6300     CONTINUE                                                         1029
C    *                                                                    1029
C    Geant POSITION : SCMY ===> inherited arguments used <===             1030
         CALL AGSRESET                                                    1030
         AG_TITLE = 'SCMY'                                                1030
         AG_EXNAME = 'POSITION'                                           1030
         AG_PARLIST = 'NONE'                                              1030
         CALL AXPOSITION                                                  1030
C    *                                                                    1030
C    Create SCAL :                                                        1031
C    Call To Local Procedure SCAL                                         1031
         ASSIGN 6320 TO BLOCK_SCAL                                        1031
         GO TO  6310                                                      1031
6320     CONTINUE                                                         1031
C    *                                                                    1031
C    Geant POSITION : SCAL ===> inherited arguments used <===             1032
         CALL AGSRESET                                                    1032
         AG_TITLE = 'SCAL'                                                1032
         AG_EXNAME = 'POSITION'                                           1032
         AG_PARLIST = 'NONE'                                              1032
         CALL AXPOSITION                                                  1032
C    *                                                                    1032
5211  CALL AGSPOPB                                                        1033
      GO TO BLOCK_SCON, (5210 ,5220)                                      1033
C End of Local Procedure                                                  1033
C ----------------------------------------------------------------------  1034
C ------------ SGRA                                                       1034
C Begin Local Procedure SGRA                                              1035
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 1035
6190  CONTINUE                                                            1035
         AG_TITLE='SGRA'                                                  1035
         AG_MODULE=MODULE                                                 1035
         AG_MARK='V00'                                                    1035
         CALL AGSPUSH                                                     1035
         AG_MODULE=MODULE                                                 1035
C    *                                                                    1035
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1036
         CALL AGSRESET                                                    1036
         AG_TITLE = 'CARBON'                                              1036
         AG_EXNAME = 'MATERIAL'                                           1036
         AG_PARLIST = 'NONE'                                              1036
         CALL AXMATERIAL                                                  1036
C    *                                                                    1036
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       1036
         CALL AGSRESET                                                    1037
         AG_SEEN = 1                                                      1037
         AG_COLO = 6                                                      1037
         AG_TITLE = 'SGRA'                                                1037
         AG_EXNAME = 'ATTRIBUTE'                                          1037
         AG_PARLIST = 'SEEN_COLO'                                         1037
         CALL AXATTRIBUTE                                                 1037
C    *                                                                    1037
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  1037
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  1037
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  1037
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  1037
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  1037
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              1037
         CALL AGSRESET                                                    1045
         AG_PHI1 = 0                                                      1045
         AG_DPHI = 360                                                    1045
         AG_NZ = 5                                                        1045
         AG_ZI(1)=SSUP_RODLEN/2                                           1045
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1045
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1045
         AG_ZI(4)=SSUP_CONE3ZMX                                           1045
         AG_ZI(5)=SSUP_CONE4ZMX                                           1045
         AG_RMN(1)=SSUP_CON1IDMN                                          1045
         AG_RMN(2)=SSUP_CON1IDMN                                          1045
         AG_RMN(3)=SSUP_CON3IDMN                                          1045
         AG_RMN(4)=SSUP_CON4IDMN                                          1045
         AG_RMN(5)=SSUP_CON4IDMX                                          1045
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1045
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1045
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1045
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             1045
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             1045
         AG_TITLE = 'PCON'                                                1045
         AG_EXNAME = 'SHAPE'                                              1045
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1045
         CALL AXSHAPE                                                     1045
C    *                                                                    1045
6191  CALL AGSPOPB                                                        1046
      GO TO BLOCK_SGRA, (6190 ,6200)                                      1046
C End of Local Procedure                                                  1046
C ----------------------------------------------------------------------  1047
C ------------ STAP                                                       1047
C Begin Local Procedure STAP                                              1048
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  1048
C E)                                                                      1048
6210  CONTINUE                                                            1048
         AG_TITLE='STAP'                                                  1048
         AG_MODULE=MODULE                                                 1048
         AG_MARK='V00'                                                    1048
         CALL AGSPUSH                                                     1048
         AG_MODULE=MODULE                                                 1048
C    *                                                                    1048
         ROFFSET=SSUP_GRPHTHK                                             1049
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1050
         CALL AGSRESET                                                    1050
         AG_A = 12                                                        1050
         AG_Z = 6                                                         1050
         AG_W = 1                                                         1050
         AG_TITLE = 'C'                                                   1050
         AG_EXNAME = 'COMPONENT'                                          1050
         AG_PARLIST = 'A_Z_W'                                             1050
         CALL AXCOMPONENT                                                 1050
C    *                                                                    1050
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1050
         CALL AGSRESET                                                    1051
         AG_A = 1                                                         1051
         AG_Z = 1                                                         1051
         AG_W = 2                                                         1051
         AG_TITLE = 'H2'                                                  1051
         AG_EXNAME = 'COMPONENT'                                          1051
         AG_PARLIST = 'A_Z_W'                                             1051
         CALL AXCOMPONENT                                                 1051
C    *                                                                    1051
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1051
         CALL AGSRESET                                                    1052
         AG_DENS = 0.935                                                  1052
         AG_TITLE = 'CH2'                                                 1052
         AG_EXNAME = 'MIXTURE'                                            1052
         AG_PARLIST = 'DENS'                                              1052
         CALL AXMIXTURE                                                   1052
C    *                                                                    1052
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       1052
         CALL AGSRESET                                                    1053
         AG_SEEN = 1                                                      1053
         AG_COLO = 3                                                      1053
         AG_TITLE = 'STAP'                                                1053
         AG_EXNAME = 'ATTRIBUTE'                                          1053
         AG_PARLIST = 'SEEN_COLO'                                         1053
         CALL AXATTRIBUTE                                                 1053
C    *                                                                    1053
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1053
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1053
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1053
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1053
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1053
         CALL AGSRESET                                                    1059
         AG_PHI1 = 0                                                      1059
         AG_DPHI = 360                                                    1059
         AG_NZ = 3                                                        1059
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1059
         AG_ZI(2)=SSUP_CONE3ZMX                                           1059
         AG_ZI(3)=SSUP_CONE4ZMX                                           1059
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1059
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1059
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1059
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1059
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1059
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1059
         AG_TITLE = 'PCON'                                                1059
         AG_EXNAME = 'SHAPE'                                              1059
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1059
         CALL AXSHAPE                                                     1059
C    *                                                                    1059
6211  CALL AGSPOPB                                                        1060
      GO TO BLOCK_STAP, (6210 ,6220)                                      1060
C End of Local Procedure                                                  1060
C ----------------------------------------------------------------------  1061
C ------------ STAC                                                       1061
C Begin Local Procedure STAC                                              1062
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      1062
6230  CONTINUE                                                            1062
         AG_TITLE='STAC'                                                  1062
         AG_MODULE=MODULE                                                 1062
         AG_MARK='V00'                                                    1062
         CALL AGSPUSH                                                     1062
         AG_MODULE=MODULE                                                 1062
C    *                                                                    1062
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             1063
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           1064
         CALL AGSRESET                                                    1064
         AG_TITLE = 'COPPER'                                              1064
         AG_EXNAME = 'MATERIAL'                                           1064
         AG_PARLIST = 'NONE'                                              1064
         CALL AXMATERIAL                                                  1064
C    *                                                                    1064
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       1064
         CALL AGSRESET                                                    1065
         AG_SEEN = 1                                                      1065
         AG_COLO = 2                                                      1065
         AG_TITLE = 'STAC'                                                1065
         AG_EXNAME = 'ATTRIBUTE'                                          1065
         AG_PARLIST = 'SEEN_COLO'                                         1065
         CALL AXATTRIBUTE                                                 1065
C    *                                                                    1065
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1065
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1065
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1065
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1065
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1065
         CALL AGSRESET                                                    1071
         AG_PHI1 = 0                                                      1071
         AG_DPHI = 360                                                    1071
         AG_NZ = 3                                                        1071
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1071
         AG_ZI(2)=SSUP_CONE3ZMX                                           1071
         AG_ZI(3)=SSUP_CONE4ZMX                                           1071
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1071
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1071
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1071
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1071
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1071
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1071
         AG_TITLE = 'PCON'                                                1071
         AG_EXNAME = 'SHAPE'                                              1071
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1071
         CALL AXSHAPE                                                     1071
C    *                                                                    1071
6231  CALL AGSPOPB                                                        1072
      GO TO BLOCK_STAC, (6230 ,6240)                                      1072
C End of Local Procedure                                                  1072
C ----------------------------------------------------------------------  1073
C ------------ SHLA                                                       1073
C Begin Local Procedure SHLA                                              1074
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              1074
6250  CONTINUE                                                            1074
         AG_TITLE='SHLA'                                                  1074
         AG_MODULE=MODULE                                                 1074
         AG_MARK='V00'                                                    1074
         CALL AGSPUSH                                                     1074
         AG_MODULE=MODULE                                                 1074
C    *                                                                    1074
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1075
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1076
         CALL AGSRESET                                                    1076
         AG_TITLE = 'AIR'                                                 1076
         AG_EXNAME = 'MATERIAL'                                           1076
         AG_PARLIST = 'NONE'                                              1076
         CALL AXMATERIAL                                                  1076
C    *                                                                    1076
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       1076
         CALL AGSRESET                                                    1077
         AG_SEEN = 0                                                      1077
         AG_COLO = 1                                                      1077
         AG_TITLE = 'SHLA'                                                1077
         AG_EXNAME = 'ATTRIBUTE'                                          1077
         AG_PARLIST = 'SEEN_COLO'                                         1077
         CALL AXATTRIBUTE                                                 1077
C    *                                                                    1077
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  1077
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  1077
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  1077
C    .0*SSUP_HOSRMX <===                                                  1077
         CALL AGSRESET                                                    1081
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            1081
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  1081
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1081
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  1081
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1081
         AG_TITLE = 'CONE'                                                1081
         AG_EXNAME = 'SHAPE'                                              1081
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1081
         CALL AXSHAPE                                                     1081
C    *                                                                    1081
C    Create SHMA :                                                        1082
C    Call To Local Procedure SHMA                                         1082
         ASSIGN 6340 TO BLOCK_SHMA                                        1082
         GO TO  6330                                                      1082
6340     CONTINUE                                                         1082
C    *                                                                    1082
6251  CALL AGSPOPB                                                        1083
      GO TO BLOCK_SHLA, (6250 ,6260)                                      1083
C End of Local Procedure                                                  1083
C ----------------------------------------------------------------------  1084
C ------------ SHMA                                                       1084
C Begin Local Procedure SHMA                                              1085
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           1085
6330  CONTINUE                                                            1085
         AG_TITLE='SHMA'                                                  1085
         AG_MODULE=MODULE                                                 1085
         AG_MARK='V00'                                                    1085
         CALL AGSPUSH                                                     1085
         AG_MODULE=MODULE                                                 1085
C    *                                                                    1085
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1086
         RMIN=SSUP_CON3IDMN + ROFFSET                                     1087
         RMAX=SSUP_CON4IDMN + ROFFSET                                     1088
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                1089
         ZMAX=SSUP_CONE3ZMX                                               1090
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1091
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1092
         CONE_LEN = SQRT(CONE_LEN)                                        1093
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1094
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1095
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1096
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1097
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1098
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       1099
         CALL AGSRESET                                                    1099
         AG_SEEN = 0                                                      1099
         AG_COLO = 1                                                      1099
         AG_TITLE = 'SHMA'                                                1099
         AG_EXNAME = 'ATTRIBUTE'                                          1099
         AG_PARLIST = 'SEEN_COLO'                                         1099
         CALL AXATTRIBUTE                                                 1099
C    *                                                                    1099
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1099
         CALL AGSRESET                                                    1101
         AG_NDIV = SSUP_NHOSES                                            1101
         AG_IAXIS = 2                                                     1101
         AG_C0 = 0.0                                                      1101
         AG_TITLE = 'DIVISION'                                            1101
         AG_EXNAME = 'SHAPE'                                              1101
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1101
         CALL AXSHAPE                                                     1101
C    *                                                                    1101
C    Create SWHO :                                                        1102
C    Call To Local Procedure SWHO                                         1102
         ASSIGN 6360 TO BLOCK_SWHO                                        1102
         GO TO  6350                                                      1102
6360     CONTINUE                                                         1102
C    *                                                                    1102
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1103
         CALL AGSRESET                                                    1103
         AG_X = XPOS                                                      1103
         AG_Y = 0                                                         1103
         AG_Z = ZPOS                                                      1103
         AG_ALPHAN=AG_ALPHAN+1                                            1103
         AG_ALPHAK(AG_ALPHAN)=2                                           1103
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1103
         AG_TITLE = 'SWHO'                                                1103
         AG_EXNAME = 'POSITION'                                           1103
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1103
         CALL AXPOSITION                                                  1103
C    *                                                                    1103
6331  CALL AGSPOPB                                                        1104
      GO TO BLOCK_SHMA, (6330 ,6340)                                      1104
C End of Local Procedure                                                  1104
C ----------------------------------------------------------------------  1105
C ------------ SHLB                                                       1105
C Begin Local Procedure SHLB                                              1106
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               1106
6270  CONTINUE                                                            1106
         AG_TITLE='SHLB'                                                  1106
         AG_MODULE=MODULE                                                 1106
         AG_MARK='V00'                                                    1106
         CALL AGSPUSH                                                     1106
         AG_MODULE=MODULE                                                 1106
C    *                                                                    1106
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1107
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1108
         CALL AGSRESET                                                    1108
         AG_TITLE = 'AIR'                                                 1108
         AG_EXNAME = 'MATERIAL'                                           1108
         AG_PARLIST = 'NONE'                                              1108
         CALL AXMATERIAL                                                  1108
C    *                                                                    1108
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       1108
         CALL AGSRESET                                                    1109
         AG_SEEN = 0                                                      1109
         AG_COLO = 1                                                      1109
         AG_TITLE = 'SHLB'                                                1109
         AG_EXNAME = 'ATTRIBUTE'                                          1109
         AG_PARLIST = 'SEEN_COLO'                                         1109
         CALL AXATTRIBUTE                                                 1109
C    *                                                                    1109
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  1109
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  1109
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  1109
C    X <===                                                               1109
         CALL AGSRESET                                                    1113
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         1113
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  1113
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1113
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  1113
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  1113
         AG_TITLE = 'CONE'                                                1113
         AG_EXNAME = 'SHAPE'                                              1113
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1113
         CALL AXSHAPE                                                     1113
C    *                                                                    1113
C    Create SHMB :                                                        1114
C    Call To Local Procedure SHMB                                         1114
         ASSIGN 6380 TO BLOCK_SHMB                                        1114
         GO TO  6370                                                      1114
6380     CONTINUE                                                         1114
C    *                                                                    1114
6271  CALL AGSPOPB                                                        1115
      GO TO BLOCK_SHLB, (6270 ,6280)                                      1115
C End of Local Procedure                                                  1115
C ----------------------------------------------------------------------  1116
C ------------ SHMB                                                       1116
C Begin Local Procedure SHMB                                              1117
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           1117
6370  CONTINUE                                                            1117
         AG_TITLE='SHMB'                                                  1117
         AG_MODULE=MODULE                                                 1117
         AG_MARK='V00'                                                    1117
         CALL AGSPUSH                                                     1117
         AG_MODULE=MODULE                                                 1117
C    *                                                                    1117
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1118
         RMIN=SSUP_CON4IDMN + ROFFSET                                     1119
         RMAX=SSUP_CON4IDMX + ROFFSET                                     1120
         ZMIN=SSUP_CONE3ZMX                                               1121
         ZMAX=SSUP_CONE4ZMX                                               1122
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1123
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1124
         CONE_LEN = SQRT(CONE_LEN)                                        1125
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1126
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1127
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1128
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1129
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1130
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       1131
         CALL AGSRESET                                                    1131
         AG_SEEN = 0                                                      1131
         AG_COLO = 1                                                      1131
         AG_TITLE = 'SHMB'                                                1131
         AG_EXNAME = 'ATTRIBUTE'                                          1131
         AG_PARLIST = 'SEEN_COLO'                                         1131
         CALL AXATTRIBUTE                                                 1131
C    *                                                                    1131
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1131
         CALL AGSRESET                                                    1133
         AG_NDIV = SSUP_NHOSES                                            1133
         AG_IAXIS = 2                                                     1133
         AG_C0 = 0.0                                                      1133
         AG_TITLE = 'DIVISION'                                            1133
         AG_EXNAME = 'SHAPE'                                              1133
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1133
         CALL AXSHAPE                                                     1133
C    *                                                                    1133
C    Create SWHO :                                                        1134
C    Call To Local Procedure SWHO                                         1134
         ASSIGN 6390 TO BLOCK_SWHO                                        1134
         GO TO  6350                                                      1134
6390     CONTINUE                                                         1134
C    *                                                                    1134
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1135
         CALL AGSRESET                                                    1135
         AG_X = XPOS                                                      1135
         AG_Y = 0                                                         1135
         AG_Z = ZPOS                                                      1135
         AG_ALPHAN=AG_ALPHAN+1                                            1135
         AG_ALPHAK(AG_ALPHAN)=2                                           1135
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1135
         AG_TITLE = 'SWHO'                                                1135
         AG_EXNAME = 'POSITION'                                           1135
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1135
         CALL AXPOSITION                                                  1135
C    *                                                                    1135
6371  CALL AGSPOPB                                                        1136
      GO TO BLOCK_SHMB, (6370 ,6380)                                      1136
C End of Local Procedure                                                  1136
C ----------------------------------------------------------------------  1137
C ------------ SWHO                                                       1137
C Begin Local Procedure SWHO                                              1138
C SWHO IS A WATER HOSE                                                    1138
6350  CONTINUE                                                            1138
         AG_TITLE='SWHO'                                                  1138
         AG_MODULE=MODULE                                                 1138
         AG_MARK='V00'                                                    1138
         CALL AGSPUSH                                                     1138
         AG_MODULE=MODULE                                                 1138
C    *                                                                    1138
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1139
         CALL AGSRESET                                                    1139
         AG_SEEN = 1                                                      1139
         AG_COLO = 3                                                      1139
         AG_TITLE = 'SWHO'                                                1139
         AG_EXNAME = 'ATTRIBUTE'                                          1139
         AG_PARLIST = 'SEEN_COLO'                                         1139
         CALL AXATTRIBUTE                                                 1139
C    *                                                                    1139
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1139
         CALL AGSRESET                                                    1140
         AG_A = 12                                                        1140
         AG_Z = 6                                                         1140
         AG_W = 1                                                         1140
         AG_TITLE = 'C'                                                   1140
         AG_EXNAME = 'COMPONENT'                                          1140
         AG_PARLIST = 'A_Z_W'                                             1140
         CALL AXCOMPONENT                                                 1140
C    *                                                                    1140
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1140
         CALL AGSRESET                                                    1141
         AG_A = 1                                                         1141
         AG_Z = 1                                                         1141
         AG_W = 2                                                         1141
         AG_TITLE = 'H2'                                                  1141
         AG_EXNAME = 'COMPONENT'                                          1141
         AG_PARLIST = 'A_Z_W'                                             1141
         CALL AXCOMPONENT                                                 1141
C    *                                                                    1141
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1141
         CALL AGSRESET                                                    1142
         AG_DENS = 0.935                                                  1142
         AG_TITLE = 'CH2'                                                 1142
         AG_EXNAME = 'MIXTURE'                                            1142
         AG_PARLIST = 'DENS'                                              1142
         CALL AXMIXTURE                                                   1142
C    *                                                                    1142
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1142
         CALL AGSRESET                                                    1143
         AG_SEEN = 1                                                      1143
         AG_COLO = 3                                                      1143
         AG_TITLE = 'SWHO'                                                1143
         AG_EXNAME = 'ATTRIBUTE'                                          1143
         AG_PARLIST = 'SEEN_COLO'                                         1143
         CALL AXATTRIBUTE                                                 1143
C    *                                                                    1143
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  1143
C    =                                                                    1143
         CALL AGSRESET                                                    1144
         AG_RMIN = 0                                                      1144
         AG_RMAX = SSUP_HOSRMX                                            1144
         AG_DZ = 0.5*CONE_LEN                                             1144
         AG_TITLE = 'TUBE'                                                1144
         AG_EXNAME = 'SHAPE'                                              1144
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1144
         CALL AXSHAPE                                                     1144
C    *                                                                    1144
C    Create SHWA :                                                        1145
C    Call To Local Procedure SHWA                                         1145
         ASSIGN 6410 TO BLOCK_SHWA                                        1145
         GO TO  6400                                                      1145
6410     CONTINUE                                                         1145
C    *                                                                    1145
C    Geant POSITION : SHWA ===> inherited arguments used <===             1146
         CALL AGSRESET                                                    1146
         AG_TITLE = 'SHWA'                                                1146
         AG_EXNAME = 'POSITION'                                           1146
         AG_PARLIST = 'NONE'                                              1146
         CALL AXPOSITION                                                  1146
C    *                                                                    1146
6351  CALL AGSPOPB                                                        1147
      GO TO BLOCK_SWHO, (6350 ,6360,6390)                                 1147
C End of Local Procedure                                                  1147
C ----------------------------------------------------------------------  1148
C ------------ SHWA                                                       1148
C Begin Local Procedure SHWA                                              1149
C SHWA IS THE WATER IN THE HOSE                                           1149
6400  CONTINUE                                                            1149
         AG_TITLE='SHWA'                                                  1149
         AG_MODULE=MODULE                                                 1149
         AG_MARK='V00'                                                    1149
         CALL AGSPUSH                                                     1149
         AG_MODULE=MODULE                                                 1149
C    *                                                                    1149
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       1150
         CALL AGSRESET                                                    1150
         AG_SEEN = 1                                                      1150
         AG_COLO = 6                                                      1150
         AG_TITLE = 'SHWA'                                                1150
         AG_EXNAME = 'ATTRIBUTE'                                          1150
         AG_PARLIST = 'SEEN_COLO'                                         1150
         CALL AXATTRIBUTE                                                 1150
C    *                                                                    1150
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1150
         CALL AGSRESET                                                    1151
         AG_TITLE = 'WATER'                                               1151
         AG_EXNAME = 'MATERIAL'                                           1151
         AG_PARLIST = 'NONE'                                              1151
         CALL AXMATERIAL                                                  1151
C    *                                                                    1151
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        1151
         CALL AGSRESET                                                    1152
         AG_RMAX = SSUP_HOSRMN                                            1152
         AG_TITLE = 'TUBE'                                                1152
         AG_EXNAME = 'SHAPE'                                              1152
         AG_PARLIST = 'RMAX'                                              1152
         CALL AXSHAPE                                                     1152
C    *                                                                    1152
6401  CALL AGSPOPB                                                        1153
      GO TO BLOCK_SHWA, (6400 ,6410)                                      1153
C End of Local Procedure                                                  1153
C ----------------------------------------------------------------------  1154
C ------------ SCMY                                                       1154
C Begin Local Procedure SCMY                                              1155
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            1155
6290  CONTINUE                                                            1155
         AG_TITLE='SCMY'                                                  1155
         AG_MODULE=MODULE                                                 1155
         AG_MARK='V00'                                                    1155
         CALL AGSPUSH                                                     1155
         AG_MODULE=MODULE                                                 1155
C    *                                                                    1155
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             1156
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          1157
         CALL AGSRESET                                                    1157
         AG_A = 12                                                        1157
         AG_Z = 6                                                         1157
         AG_W = 5                                                         1157
         AG_TITLE = 'C5'                                                  1157
         AG_EXNAME = 'COMPONENT'                                          1157
         AG_PARLIST = 'A_Z_W'                                             1157
         CALL AXCOMPONENT                                                 1157
C    *                                                                    1157
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           1157
         CALL AGSRESET                                                    1158
         AG_A = 1                                                         1158
         AG_Z = 1                                                         1158
         AG_W = 4                                                         1158
         AG_TITLE = 'H4'                                                  1158
         AG_EXNAME = 'COMPONENT'                                          1158
         AG_PARLIST = 'A_Z_W'                                             1158
         CALL AXCOMPONENT                                                 1158
C    *                                                                    1158
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          1158
         CALL AGSRESET                                                    1159
         AG_A = 16                                                        1159
         AG_Z = 8                                                         1159
         AG_W = 2                                                         1159
         AG_TITLE = 'O2'                                                  1159
         AG_EXNAME = 'COMPONENT'                                          1159
         AG_PARLIST = 'A_Z_W'                                             1159
         CALL AXCOMPONENT                                                 1159
C    *                                                                    1159
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            1159
         CALL AGSRESET                                                    1160
         AG_DENS = 1.39                                                   1160
         AG_TITLE = 'MYLAR'                                               1160
         AG_EXNAME = 'MIXTURE'                                            1160
         AG_PARLIST = 'DENS'                                              1160
         CALL AXMIXTURE                                                   1160
C    *                                                                    1160
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       1160
         CALL AGSRESET                                                    1161
         AG_SEEN = 1                                                      1161
         AG_COLO = 3                                                      1161
         AG_TITLE = 'SCMY'                                                1161
         AG_EXNAME = 'ATTRIBUTE'                                          1161
         AG_PARLIST = 'SEEN_COLO'                                         1161
         CALL AXATTRIBUTE                                                 1161
C    *                                                                    1161
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1161
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1161
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1161
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  1161
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     1161
         CALL AGSRESET                                                    1167
         AG_PHI1 = 0                                                      1167
         AG_DPHI = 360                                                    1167
         AG_NZ = 3                                                        1167
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1167
         AG_ZI(2)=SSUP_CONE3ZMX                                           1167
         AG_ZI(3)=SSUP_CONE4ZMX                                           1167
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1167
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1167
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1167
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    1167
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    1167
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    1167
         AG_TITLE = 'PCON'                                                1167
         AG_EXNAME = 'SHAPE'                                              1167
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1167
         CALL AXSHAPE                                                     1167
C    *                                                                    1167
6291  CALL AGSPOPB                                                        1168
      GO TO BLOCK_SCMY, (6290 ,6300)                                      1168
C End of Local Procedure                                                  1168
C ----------------------------------------------------------------------  1169
C ------------ SCAL                                                       1169
C Begin Local Procedure SCAL                                              1170
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     1170
6310  CONTINUE                                                            1170
         AG_TITLE='SCAL'                                                  1170
         AG_MODULE=MODULE                                                 1170
         AG_MARK='V00'                                                    1170
         CALL AGSPUSH                                                     1170
         AG_MODULE=MODULE                                                 1170
C    *                                                                    1170
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           1171
     *   SSUP_WRPMYTHK                                                    1171
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1172
         CALL AGSRESET                                                    1172
         AG_TITLE = 'ALUMINIUM'                                           1172
         AG_EXNAME = 'MATERIAL'                                           1172
         AG_PARLIST = 'NONE'                                              1172
         CALL AXMATERIAL                                                  1172
C    *                                                                    1172
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       1172
         CALL AGSRESET                                                    1173
         AG_SEEN = 1                                                      1173
         AG_COLO = 2                                                      1173
         AG_TITLE = 'SCAL'                                                1173
         AG_EXNAME = 'ATTRIBUTE'                                          1173
         AG_PARLIST = 'SEEN_COLO'                                         1173
         CALL AXATTRIBUTE                                                 1173
C    *                                                                    1173
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1173
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1173
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1173
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  1173
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     1173
         CALL AGSRESET                                                    1179
         AG_PHI1 = 0                                                      1179
         AG_DPHI = 360                                                    1179
         AG_NZ = 3                                                        1179
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1179
         AG_ZI(2)=SSUP_CONE3ZMX                                           1179
         AG_ZI(3)=SSUP_CONE4ZMX                                           1179
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1179
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1179
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1179
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    1179
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    1179
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    1179
         AG_TITLE = 'PCON'                                                1179
         AG_EXNAME = 'SHAPE'                                              1179
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1179
         CALL AXSHAPE                                                     1179
C    *                                                                    1179
6311  CALL AGSPOPB                                                        1180
      GO TO BLOCK_SCAL, (6310 ,6320)                                      1180
C End of Local Procedure                                                  1180
C ----------------------------------------------------------------------  1181
C ------------ SWMM                                                       1181
C Begin Local Procedure SWMM                                              1182
C SWMM IS THE WATER MANIFOLD MOTHER                                       1182
5170  CONTINUE                                                            1182
         AG_TITLE='SWMM'                                                  1182
         AG_MODULE=MODULE                                                 1182
         AG_MARK='V00'                                                    1182
         CALL AGSPUSH                                                     1182
         AG_MODULE=MODULE                                                 1182
C    *                                                                    1182
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     1183
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     1184
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1185
         CALL AGSRESET                                                    1185
         AG_TITLE = 'AIR'                                                 1185
         AG_EXNAME = 'MATERIAL'                                           1185
         AG_PARLIST = 'NONE'                                              1185
         CALL AXMATERIAL                                                  1185
C    *                                                                    1185
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       1185
         CALL AGSRESET                                                    1186
         AG_SEEN = 0                                                      1186
         AG_COLO = 1                                                      1186
         AG_TITLE = 'SWMM'                                                1186
         AG_EXNAME = 'ATTRIBUTE'                                          1186
         AG_PARLIST = 'SEEN_COLO'                                         1186
         CALL AXATTRIBUTE                                                 1186
C    *                                                                    1186
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1186
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   1186
         CALL AGSRESET                                                    1189
         AG_PHI1 = 0                                                      1189
         AG_DPHI = 360                                                    1189
         AG_NPDIV = 18                                                    1189
         AG_NZ = 2                                                        1189
         AG_ZI(1)=-SWAM_LEN/2                                             1189
         AG_ZI(2)=+SWAM_LEN/2                                             1189
         AG_RMN(1)=RIN                                                    1189
         AG_RMN(2)=RIN                                                    1189
         AG_RMX(1)=ROU                                                    1189
         AG_RMX(2)=ROU                                                    1189
         AG_TITLE = 'PGON'                                                1189
         AG_EXNAME = 'SHAPE'                                              1189
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1189
         CALL AXSHAPE                                                     1189
C    *                                                                    1189
C    Create SWMB :                                                        1190
C    Call To Local Procedure SWMB                                         1190
         ASSIGN 6430 TO BLOCK_SWMB                                        1190
         GO TO  6420                                                      1190
6430     CONTINUE                                                         1190
C    *                                                                    1190
C    Geant POSITION : SWMB ===> inherited arguments used <===             1191
         CALL AGSRESET                                                    1191
         AG_TITLE = 'SWMB'                                                1191
         AG_EXNAME = 'POSITION'                                           1191
         AG_PARLIST = 'NONE'                                              1191
         CALL AXPOSITION                                                  1191
C    *                                                                    1191
C    Create SWMT :                                                        1192
C    Call To Local Procedure SWMT                                         1192
         ASSIGN 6450 TO BLOCK_SWMT                                        1192
         GO TO  6440                                                      1192
6450     CONTINUE                                                         1192
C    *                                                                    1192
C    Geant POSITION : SWMT ===> inherited arguments used <===             1193
         CALL AGSRESET                                                    1193
         AG_TITLE = 'SWMT'                                                1193
         AG_EXNAME = 'POSITION'                                           1193
         AG_PARLIST = 'NONE'                                              1193
         CALL AXPOSITION                                                  1193
C    *                                                                    1193
C    Create SWMS :                                                        1194
C    Call To Local Procedure SWMS                                         1194
         ASSIGN 6470 TO BLOCK_SWMS                                        1194
         GO TO  6460                                                      1194
6470     CONTINUE                                                         1194
C    *                                                                    1194
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         1195
         CALL AGSRESET                                                    1195
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                1195
         AG_TITLE = 'SWMS'                                                1195
         AG_EXNAME = 'POSITION'                                           1195
         AG_PARLIST = 'Z'                                                 1195
         CALL AXPOSITION                                                  1195
C    *                                                                    1195
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          1195
         CALL AGSRESET                                                    1196
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 1196
         AG_TITLE = 'SWMS'                                                1196
         AG_EXNAME = 'POSITION'                                           1196
         AG_PARLIST = 'Z'                                                 1196
         CALL AXPOSITION                                                  1196
C    *                                                                    1196
C    Create SWMW :                                                        1197
C    Call To Local Procedure SWMW                                         1197
         ASSIGN 6490 TO BLOCK_SWMW                                        1197
         GO TO  6480                                                      1197
6490     CONTINUE                                                         1197
C    *                                                                    1197
C    Geant POSITION : SWMW ===> inherited arguments used <===             1198
         CALL AGSRESET                                                    1198
         AG_TITLE = 'SWMW'                                                1198
         AG_EXNAME = 'POSITION'                                           1198
         AG_PARLIST = 'NONE'                                              1198
         CALL AXPOSITION                                                  1198
C    *                                                                    1198
C    Create SOTB :                                                        1199
C    Call To Local Procedure SOTB                                         1199
         ASSIGN 6510 TO BLOCK_SOTB                                        1199
         GO TO  6500                                                      1199
6510     CONTINUE                                                         1199
C    *                                                                    1199
C    Geant POSITION : SOTB ===> inherited arguments used <===             1200
         CALL AGSRESET                                                    1200
         AG_TITLE = 'SOTB'                                                1200
         AG_EXNAME = 'POSITION'                                           1200
         AG_PARLIST = 'NONE'                                              1200
         CALL AXPOSITION                                                  1200
C    *                                                                    1200
C    Create SITB :                                                        1201
C    Call To Local Procedure SITB                                         1201
         ASSIGN 6530 TO BLOCK_SITB                                        1201
         GO TO  6520                                                      1201
6530     CONTINUE                                                         1201
C    *                                                                    1201
C    Geant POSITION : SITB ===> inherited arguments used <===             1202
         CALL AGSRESET                                                    1202
         AG_TITLE = 'SITB'                                                1202
         AG_EXNAME = 'POSITION'                                           1202
         AG_PARLIST = 'NONE'                                              1202
         CALL AXPOSITION                                                  1202
C    *                                                                    1202
5171  CALL AGSPOPB                                                        1203
      GO TO BLOCK_SWMM, (5170 ,5180)                                      1203
C End of Local Procedure                                                  1203
C ----------------------------------------------------------------------  1204
C ------------ SWMB                                                       1204
C Begin Local Procedure SWMB                                              1205
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       1205
6420  CONTINUE                                                            1205
         AG_TITLE='SWMB'                                                  1205
         AG_MODULE=MODULE                                                 1205
         AG_MARK='V00'                                                    1205
         CALL AGSPUSH                                                     1205
         AG_MODULE=MODULE                                                 1205
C    *                                                                    1205
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1206
         CALL AGSRESET                                                    1206
         AG_TITLE = 'ALUMINIUM'                                           1206
         AG_EXNAME = 'MATERIAL'                                           1206
         AG_PARLIST = 'NONE'                                              1206
         CALL AXMATERIAL                                                  1206
C    *                                                                    1206
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       1206
         CALL AGSRESET                                                    1207
         AG_SEEN = 1                                                      1207
         AG_COLO = 2                                                      1207
         AG_TITLE = 'SWMB'                                                1207
         AG_EXNAME = 'ATTRIBUTE'                                          1207
         AG_PARLIST = 'SEEN_COLO'                                         1207
         CALL AXATTRIBUTE                                                 1207
C    *                                                                    1207
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1207
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  1207
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          1207
         CALL AGSRESET                                                    1211
         AG_PHI1 = 0                                                      1211
         AG_DPHI = 360                                                    1211
         AG_NPDIV = 18                                                    1211
         AG_NZ = 2                                                        1211
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1211
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1211
         AG_RMN(1)=SWAM_RMIN                                              1211
         AG_RMN(2)=SWAM_RMIN                                              1211
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 1211
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 1211
         AG_TITLE = 'PGON'                                                1211
         AG_EXNAME = 'SHAPE'                                              1211
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1211
         CALL AXSHAPE                                                     1211
C    *                                                                    1211
6421  CALL AGSPOPB                                                        1212
      GO TO BLOCK_SWMB, (6420 ,6430)                                      1212
C End of Local Procedure                                                  1212
C ----------------------------------------------------------------------  1213
C ------------ SWMT                                                       1213
C Begin Local Procedure SWMT                                              1214
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            1214
6440  CONTINUE                                                            1214
         AG_TITLE='SWMT'                                                  1214
         AG_MODULE=MODULE                                                 1214
         AG_MARK='V00'                                                    1214
         CALL AGSPUSH                                                     1214
         AG_MODULE=MODULE                                                 1214
C    *                                                                    1214
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1215
         CALL AGSRESET                                                    1215
         AG_TITLE = 'ALUMINIUM'                                           1215
         AG_EXNAME = 'MATERIAL'                                           1215
         AG_PARLIST = 'NONE'                                              1215
         CALL AXMATERIAL                                                  1215
C    *                                                                    1215
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       1215
         CALL AGSRESET                                                    1216
         AG_SEEN = 1                                                      1216
         AG_COLO = 2                                                      1216
         AG_TITLE = 'SWMT'                                                1216
         AG_EXNAME = 'ATTRIBUTE'                                          1216
         AG_PARLIST = 'SEEN_COLO'                                         1216
         CALL AXATTRIBUTE                                                 1216
C    *                                                                    1216
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1216
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  1216
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          1216
         CALL AGSRESET                                                    1220
         AG_PHI1 = 0                                                      1220
         AG_DPHI = 360                                                    1220
         AG_NPDIV = 18                                                    1220
         AG_NZ = 2                                                        1220
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1220
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1220
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 1220
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 1220
         AG_RMX(1)=SWAM_RMAX                                              1220
         AG_RMX(2)=SWAM_RMAX                                              1220
         AG_TITLE = 'PGON'                                                1220
         AG_EXNAME = 'SHAPE'                                              1220
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1220
         CALL AXSHAPE                                                     1220
C    *                                                                    1220
6441  CALL AGSPOPB                                                        1221
      GO TO BLOCK_SWMT, (6440 ,6450)                                      1221
C End of Local Procedure                                                  1221
C ----------------------------------------------------------------------  1222
C ------------ SWMS                                                       1222
C Begin Local Procedure SWMS                                              1223
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  1223
6460  CONTINUE                                                            1223
         AG_TITLE='SWMS'                                                  1223
         AG_MODULE=MODULE                                                 1223
         AG_MARK='V00'                                                    1223
         CALL AGSPUSH                                                     1223
         AG_MODULE=MODULE                                                 1223
C    *                                                                    1223
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1224
         CALL AGSRESET                                                    1224
         AG_TITLE = 'ALUMINIUM'                                           1224
         AG_EXNAME = 'MATERIAL'                                           1224
         AG_PARLIST = 'NONE'                                              1224
         CALL AXMATERIAL                                                  1224
C    *                                                                    1224
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       1224
         CALL AGSRESET                                                    1225
         AG_SEEN = 1                                                      1225
         AG_COLO = 2                                                      1225
         AG_TITLE = 'SWMS'                                                1225
         AG_EXNAME = 'ATTRIBUTE'                                          1225
         AG_PARLIST = 'SEEN_COLO'                                         1225
         CALL AXATTRIBUTE                                                 1225
C    *                                                                    1225
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  1225
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  1225
C    WAM_RMAX} <===                                                       1225
         CALL AGSRESET                                                    1228
         AG_PHI1 = 0                                                      1228
         AG_DPHI = 360                                                    1228
         AG_NPDIV = 18                                                    1228
         AG_NZ = 2                                                        1228
         AG_ZI(1)=-SWAM_WALLTHK/2                                         1228
         AG_ZI(2)=SWAM_WALLTHK/2                                          1228
         AG_RMN(1)=SWAM_RMIN                                              1228
         AG_RMN(2)=SWAM_RMIN                                              1228
         AG_RMX(1)=SWAM_RMAX                                              1228
         AG_RMX(2)=SWAM_RMAX                                              1228
         AG_TITLE = 'PGON'                                                1228
         AG_EXNAME = 'SHAPE'                                              1228
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1228
         CALL AXSHAPE                                                     1228
C    *                                                                    1228
6461  CALL AGSPOPB                                                        1229
      GO TO BLOCK_SWMS, (6460 ,6470)                                      1229
C End of Local Procedure                                                  1229
C ----------------------------------------------------------------------  1230
C ------------ SWMW                                                       1230
C Begin Local Procedure SWMW                                              1231
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 1231
6480  CONTINUE                                                            1231
         AG_TITLE='SWMW'                                                  1231
         AG_MODULE=MODULE                                                 1231
         AG_MARK='V00'                                                    1231
         CALL AGSPUSH                                                     1231
         AG_MODULE=MODULE                                                 1231
C    *                                                                    1231
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       1232
         CALL AGSRESET                                                    1232
         AG_SEEN = 1                                                      1232
         AG_COLO = 6                                                      1232
         AG_TITLE = 'SWMW'                                                1232
         AG_EXNAME = 'ATTRIBUTE'                                          1232
         AG_PARLIST = 'SEEN_COLO'                                         1232
         CALL AXATTRIBUTE                                                 1232
C    *                                                                    1232
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1232
         CALL AGSRESET                                                    1233
         AG_TITLE = 'WATER'                                               1233
         AG_EXNAME = 'MATERIAL'                                           1233
         AG_PARLIST = 'NONE'                                              1233
         CALL AXMATERIAL                                                  1233
C    *                                                                    1233
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1233
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  1233
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  1233
C    -SWAM_WALLTHK} <===                                                  1233
         CALL AGSRESET                                                    1238
         AG_PHI1 = 0                                                      1238
         AG_DPHI = 360                                                    1238
         AG_NPDIV = 18                                                    1238
         AG_NZ = 2                                                        1238
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1238
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1238
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 1238
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 1238
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 1238
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 1238
         AG_TITLE = 'PGON'                                                1238
         AG_EXNAME = 'SHAPE'                                              1238
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1238
         CALL AXSHAPE                                                     1238
C    *                                                                    1238
6481  CALL AGSPOPB                                                        1239
      GO TO BLOCK_SWMW, (6480 ,6490)                                      1239
C End of Local Procedure                                                  1239
C ----------------------------------------------------------------------  1240
C ------------ SOTB                                                       1240
C Begin Local Procedure SOTB                                              1241
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            1241
6500  CONTINUE                                                            1241
         AG_TITLE='SOTB'                                                  1241
         AG_MODULE=MODULE                                                 1241
         AG_MARK='V00'                                                    1241
         CALL AGSPUSH                                                     1241
         AG_MODULE=MODULE                                                 1241
C    *                                                                    1241
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1242
         CALL AGSRESET                                                    1242
         AG_TITLE = 'G10'                                                 1242
         AG_EXNAME = 'MATERIAL'                                           1242
         AG_PARLIST = 'NONE'                                              1242
         CALL AXMATERIAL                                                  1242
C    *                                                                    1242
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       1242
         CALL AGSRESET                                                    1243
         AG_SEEN = 1                                                      1243
         AG_COLO = 3                                                      1243
         AG_TITLE = 'SOTB'                                                1243
         AG_EXNAME = 'ATTRIBUTE'                                          1243
         AG_PARLIST = 'SEEN_COLO'                                         1243
         CALL AXATTRIBUTE                                                 1243
C    *                                                                    1243
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1243
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  1243
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    1243
         CALL AGSRESET                                                    1246
         AG_PHI1 = 0                                                      1246
         AG_DPHI = 360                                                    1246
         AG_NPDIV = 18                                                    1246
         AG_NZ = 2                                                        1246
         AG_ZI(1)=-SWAM_LEN/2                                             1246
         AG_ZI(2)=+SWAM_LEN/2                                             1246
         AG_RMN(1)=SWAM_RMAX                                              1246
         AG_RMN(2)=SWAM_RMAX                                              1246
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 1246
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 1246
         AG_TITLE = 'PGON'                                                1246
         AG_EXNAME = 'SHAPE'                                              1246
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1246
         CALL AXSHAPE                                                     1246
C    *                                                                    1246
6501  CALL AGSPOPB                                                        1247
      GO TO BLOCK_SOTB, (6500 ,6510)                                      1247
C End of Local Procedure                                                  1247
C ----------------------------------------------------------------------  1248
C ------------ SITB                                                       1248
C Begin Local Procedure SITB                                              1249
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            1249
6520  CONTINUE                                                            1249
         AG_TITLE='SITB'                                                  1249
         AG_MODULE=MODULE                                                 1249
         AG_MARK='V00'                                                    1249
         CALL AGSPUSH                                                     1249
         AG_MODULE=MODULE                                                 1249
C    *                                                                    1249
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1250
         CALL AGSRESET                                                    1250
         AG_A = 12                                                        1250
         AG_Z = 6                                                         1250
         AG_W = 1                                                         1250
         AG_TITLE = 'C'                                                   1250
         AG_EXNAME = 'COMPONENT'                                          1250
         AG_PARLIST = 'A_Z_W'                                             1250
         CALL AXCOMPONENT                                                 1250
C    *                                                                    1250
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1250
         CALL AGSRESET                                                    1251
         AG_A = 1                                                         1251
         AG_Z = 1                                                         1251
         AG_W = 2                                                         1251
         AG_TITLE = 'H2'                                                  1251
         AG_EXNAME = 'COMPONENT'                                          1251
         AG_PARLIST = 'A_Z_W'                                             1251
         CALL AXCOMPONENT                                                 1251
C    *                                                                    1251
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1251
         CALL AGSRESET                                                    1252
         AG_DENS = 0.935                                                  1252
         AG_TITLE = 'CH2'                                                 1252
         AG_EXNAME = 'MIXTURE'                                            1252
         AG_PARLIST = 'DENS'                                              1252
         CALL AXMIXTURE                                                   1252
C    *                                                                    1252
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       1252
         CALL AGSRESET                                                    1253
         AG_SEEN = 1                                                      1253
         AG_COLO = 3                                                      1253
         AG_TITLE = 'SITB'                                                1253
         AG_EXNAME = 'ATTRIBUTE'                                          1253
         AG_PARLIST = 'SEEN_COLO'                                         1253
         CALL AXATTRIBUTE                                                 1253
C    *                                                                    1253
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1253
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  1253
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   1253
         CALL AGSRESET                                                    1257
         AG_PHI1 = 0                                                      1257
         AG_DPHI = 360                                                    1257
         AG_NPDIV = 18                                                    1257
         AG_NZ = 2                                                        1257
         AG_ZI(1)=-SWAM_LEN/2                                             1257
         AG_ZI(2)=+SWAM_LEN/2                                             1257
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 1257
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 1257
         AG_RMX(1)=SWAM_RMIN                                              1257
         AG_RMX(2)=SWAM_RMIN                                              1257
         AG_TITLE = 'PGON'                                                1257
         AG_EXNAME = 'SHAPE'                                              1257
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1257
         CALL AXSHAPE                                                     1257
C    *                                                                    1257
6521  CALL AGSPOPB                                                        1258
      GO TO BLOCK_SITB, (6520 ,6530)                                      1258
C End of Local Procedure                                                  1258
C ----------------------------------------------------------------------  1259
C ------------ SBWC                                                       1259
C Begin Local Procedure SBWC                                              1260
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           1260
5190  CONTINUE                                                            1260
         AG_TITLE='SBWC'                                                  1260
         AG_MODULE=MODULE                                                 1260
         AG_MARK='V00'                                                    1260
         CALL AGSPUSH                                                     1260
         AG_MODULE=MODULE                                                 1260
C    *                                                                    1260
         RIN=SWAM_RMIN                                                    1261
         ROU=SSUP_CON1IDMN                                                1262
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1263
         CALL AGSRESET                                                    1263
         AG_TITLE = 'AIR'                                                 1263
         AG_EXNAME = 'MATERIAL'                                           1263
         AG_PARLIST = 'NONE'                                              1263
         CALL AXMATERIAL                                                  1263
C    *                                                                    1263
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       1263
         CALL AGSRESET                                                    1264
         AG_SEEN = 0                                                      1264
         AG_COLO = 1                                                      1264
         AG_TITLE = 'SBWC'                                                1264
         AG_EXNAME = 'ATTRIBUTE'                                          1264
         AG_PARLIST = 'SEEN_COLO'                                         1264
         CALL AXATTRIBUTE                                                 1264
C    *                                                                    1264
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  1264
C    MIN+SWAM_LEN))/2 <===                                                1264
         CALL AGSRESET                                                    1265
         AG_RMIN = RIN                                                    1265
         AG_RMAX = ROU                                                    1265
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   1265
         AG_TITLE = 'TUBE'                                                1265
         AG_EXNAME = 'SHAPE'                                              1265
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1265
         CALL AXSHAPE                                                     1265
C    *                                                                    1265
C    Create SWCM :                                                        1266
C    Call To Local Procedure SWCM                                         1266
         ASSIGN 6550 TO BLOCK_SWCM                                        1266
         GO TO  6540                                                      1266
6550     CONTINUE                                                         1266
C    *                                                                    1266
5191  CALL AGSPOPB                                                        1267
      GO TO BLOCK_SBWC, (5190 ,5200)                                      1267
C End of Local Procedure                                                  1267
C ----------------------------------------------------------------------  1268
C ------------ SWCM                                                       1268
C Begin Local Procedure SWCM                                              1269
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   1269
6540  CONTINUE                                                            1269
         AG_TITLE='SWCM'                                                  1269
         AG_MODULE=MODULE                                                 1269
         AG_MARK='V00'                                                    1269
         CALL AGSPUSH                                                     1269
         AG_MODULE=MODULE                                                 1269
C    *                                                                    1269
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       1270
         CALL AGSRESET                                                    1270
         AG_SEEN = 0                                                      1270
         AG_COLO = 1                                                      1270
         AG_TITLE = 'SWCM'                                                1270
         AG_EXNAME = 'ATTRIBUTE'                                          1270
         AG_PARLIST = 'SEEN_COLO'                                         1270
         CALL AXATTRIBUTE                                                 1270
C    *                                                                    1270
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 1270
         CALL AGSRESET                                                    1271
         AG_NDIV = 3                                                      1271
         AG_IAXIS = 2                                                     1271
         AG_C0 = 0                                                        1271
         AG_TITLE = 'DIVISION'                                            1271
         AG_EXNAME = 'SHAPE'                                              1271
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1271
         CALL AXSHAPE                                                     1271
C    *                                                                    1271
C    Create SXAI :                                                        1272
C    Call To Local Procedure SXAI                                         1272
         ASSIGN 6570 TO BLOCK_SXAI                                        1272
         GO TO  6560                                                      1272
6570     CONTINUE                                                         1272
C    *                                                                    1272
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  1273
C    /2+SSUP_BRATHK/2) <===                                               1273
         CALL AGSRESET                                                    1273
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   1273
         AG_TITLE = 'SXAI'                                                1273
         AG_EXNAME = 'POSITION'                                           1273
         AG_PARLIST = 'Z'                                                 1273
         CALL AXPOSITION                                                  1273
C    *                                                                    1273
C    Create SXBI :                                                        1274
C    Call To Local Procedure SXBI                                         1274
         ASSIGN 6590 TO BLOCK_SXBI                                        1274
         GO TO  6580                                                      1274
6590     CONTINUE                                                         1274
C    *                                                                    1274
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    1275
         CALL AGSRESET                                                    1275
         AG_Z = (SSUP_BRATHK/2)                                           1275
         AG_TITLE = 'SXBI'                                                1275
         AG_EXNAME = 'POSITION'                                           1275
         AG_PARLIST = 'Z'                                                 1275
         CALL AXPOSITION                                                  1275
C    *                                                                    1275
6541  CALL AGSPOPB                                                        1276
      GO TO BLOCK_SWCM, (6540 ,6550)                                      1276
C End of Local Procedure                                                  1276
C ----------------------------------------------------------------------  1277
C ------------ SXAI                                                       1277
C Begin Local Procedure SXAI                                              1278
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      1278
6560  CONTINUE                                                            1278
         AG_TITLE='SXAI'                                                  1278
         AG_MODULE=MODULE                                                 1278
         AG_MARK='V00'                                                    1278
         CALL AGSPUSH                                                     1278
         AG_MODULE=MODULE                                                 1278
C    *                                                                    1278
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       1279
         CALL AGSRESET                                                    1279
         AG_SEEN = 1                                                      1279
         AG_COLO = 2                                                      1279
         AG_TITLE = 'SXAI'                                                1279
         AG_EXNAME = 'ATTRIBUTE'                                          1279
         AG_PARLIST = 'SEEN_COLO'                                         1279
         CALL AXATTRIBUTE                                                 1279
C    *                                                                    1279
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1279
         CALL AGSRESET                                                    1280
         AG_TITLE = 'ALUMINIUM'                                           1280
         AG_EXNAME = 'MATERIAL'                                           1280
         AG_PARLIST = 'NONE'                                              1280
         CALL AXMATERIAL                                                  1280
C    *                                                                    1280
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   1280
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         1280
         CALL AGSRESET                                                    1282
         AG_RMIN = SWAM_RMIN                                              1282
         AG_RMAX = SSUP_CON1IDMN                                          1282
         AG_PHI1 = -5                                                     1282
         AG_PHI2 = 5                                                      1282
         AG_DZ = SSUP_BRATHK/2                                            1282
         AG_TITLE = 'TUBS'                                                1282
         AG_EXNAME = 'SHAPE'                                              1282
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1282
         CALL AXSHAPE                                                     1282
C    *                                                                    1282
6561  CALL AGSPOPB                                                        1283
      GO TO BLOCK_SXAI, (6560 ,6570)                                      1283
C End of Local Procedure                                                  1283
C ----------------------------------------------------------------------  1284
C ------------ SXBI                                                       1284
C Begin Local Procedure SXBI                                              1285
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     1285
6580  CONTINUE                                                            1285
         AG_TITLE='SXBI'                                                  1285
         AG_MODULE=MODULE                                                 1285
         AG_MARK='V00'                                                    1285
         CALL AGSPUSH                                                     1285
         AG_MODULE=MODULE                                                 1285
C    *                                                                    1285
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       1286
         CALL AGSRESET                                                    1286
         AG_SEEN = 1                                                      1286
         AG_COLO = 2                                                      1286
         AG_TITLE = 'SXBI'                                                1286
         AG_EXNAME = 'ATTRIBUTE'                                          1286
         AG_PARLIST = 'SEEN_COLO'                                         1286
         CALL AXATTRIBUTE                                                 1286
C    *                                                                    1286
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1286
         CALL AGSRESET                                                    1287
         AG_TITLE = 'ALUMINIUM'                                           1287
         AG_EXNAME = 'MATERIAL'                                           1287
         AG_PARLIST = 'NONE'                                              1287
         CALL AXMATERIAL                                                  1287
C    *                                                                    1287
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  1287
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  1287
C    _BRATHK)/2) <===                                                     1287
         CALL AGSRESET                                                    1289
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              1289
         AG_RMAX = SSUP_CON1IDMN                                          1289
         AG_PHI1 = -5                                                     1289
         AG_PHI2 = 5                                                      1289
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     1289
         AG_TITLE = 'TUBS'                                                1289
         AG_EXNAME = 'SHAPE'                                              1289
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1289
         CALL AXSHAPE                                                     1289
C    *                                                                    1289
6581  CALL AGSPOPB                                                        1290
      GO TO BLOCK_SXBI, (6580 ,6590)                                      1290
C End of Local Procedure                                                  1290
C ----------------------------------------------------------------------  1291
C ------------ SCRW                                                       1291
C Begin Local Procedure SCRW                                              1292
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   1292
6100  CONTINUE                                                            1292
         AG_TITLE='SCRW'                                                  1292
         AG_MODULE=MODULE                                                 1292
         AG_MARK='V00'                                                    1292
         CALL AGSPUSH                                                     1292
         AG_MODULE=MODULE                                                 1292
C    *                                                                    1292
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        1293
         CALL AGSRESET                                                    1293
         AG_TITLE = 'BERILLIUM'                                           1293
         AG_EXNAME = 'MATERIAL'                                           1293
         AG_PARLIST = 'NONE'                                              1293
         CALL AXMATERIAL                                                  1293
C    *                                                                    1293
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       1293
         CALL AGSRESET                                                    1294
         AG_SEEN = 1                                                      1294
         AG_COLO = 2                                                      1294
         AG_TITLE = 'SCRW'                                                1294
         AG_EXNAME = 'ATTRIBUTE'                                          1294
         AG_PARLIST = 'SEEN_COLO'                                         1294
         CALL AXATTRIBUTE                                                 1294
C    *                                                                    1294
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  1294
C    NDRNG_Z) <===                                                        1294
         CALL AGSRESET                                                    1295
         AG_RMIN = 0                                                      1295
         AG_RMAX = SSUP_ERJDIA/2                                          1295
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   1295
         AG_TITLE = 'TUBE'                                                1295
         AG_EXNAME = 'SHAPE'                                              1295
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1295
         CALL AXSHAPE                                                     1295
C    *                                                                    1295
6101  CALL AGSPOPB                                                        1296
      GO TO BLOCK_SCRW, (6100 ,6110,6140)                                 1296
C End of Local Procedure                                                  1296
C ----------------------------------------------------------------------  1297
C ------------ SBSP                                                       1297
C Begin Local Procedure SBSP                                              1298
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1298
5230  CONTINUE                                                            1298
         AG_TITLE='SBSP'                                                  1298
         AG_MODULE=MODULE                                                 1298
         AG_MARK='V00'                                                    1298
         CALL AGSPUSH                                                     1298
         AG_MODULE=MODULE                                                 1298
C    *                                                                    1298
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1299
         CALL AGSRESET                                                    1299
         AG_TITLE = 'AIR'                                                 1299
         AG_EXNAME = 'MATERIAL'                                           1299
         AG_PARLIST = 'NONE'                                              1299
         CALL AXMATERIAL                                                  1299
C    *                                                                    1299
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1299
         CALL AGSRESET                                                    1300
         AG_SEEN = 0                                                      1300
         AG_COLO = 1                                                      1300
         AG_TITLE = 'SBSP'                                                1300
         AG_EXNAME = 'ATTRIBUTE'                                          1300
         AG_PARLIST = 'SEEN_COLO'                                         1300
         CALL AXATTRIBUTE                                                 1300
C    *                                                                    1300
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1300
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1300
         CALL AGSRESET                                                    1302
         AG_RMIN = SVTG_RSIZEMIN                                          1302
         AG_RMAX = SSUB_KMOUNTOD/2                                        1302
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1302
         AG_TITLE = 'TUBE'                                                1302
         AG_EXNAME = 'SHAPE'                                              1302
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1302
         CALL AXSHAPE                                                     1302
C    *                                                                    1302
C    Create SAKM :                                                        1303
C    Call To Local Procedure SAKM                                         1303
         ASSIGN 6610 TO BLOCK_SAKM                                        1303
         GO TO  6600                                                      1303
6610     CONTINUE                                                         1303
C    *                                                                    1303
C    Geant POSITION : SAKM ===> inherited arguments used <===             1304
         CALL AGSRESET                                                    1304
         AG_TITLE = 'SAKM'                                                1304
         AG_EXNAME = 'POSITION'                                           1304
         AG_PARLIST = 'NONE'                                              1304
         CALL AXPOSITION                                                  1304
C    *                                                                    1304
C    Create SBMM :                                                        1305
C    Call To Local Procedure SBMM                                         1305
         ASSIGN 6630 TO BLOCK_SBMM                                        1305
         GO TO  6620                                                      1305
6630     CONTINUE                                                         1305
C    *                                                                    1305
C    Create SBMM :                                                        1306
C    Call To Local Procedure SBMM                                         1306
         ASSIGN 6640 TO BLOCK_SBMM                                        1306
         GO TO  6620                                                      1306
6640     CONTINUE                                                         1306
C    *                                                                    1306
C    Create SBRL :                                                        1307
C    Call To Local Procedure SBRL                                         1307
         ASSIGN 6660 TO BLOCK_SBRL                                        1307
         GO TO  6650                                                      1307
6660     CONTINUE                                                         1307
C    *                                                                    1307
C    Create SBRX :                                                        1308
C    Call To Local Procedure SBRX                                         1308
         ASSIGN 6680 TO BLOCK_SBRX                                        1308
         GO TO  6670                                                      1308
6680     CONTINUE                                                         1308
C    *                                                                    1308
C    *                                                                    1309
         I=-1                                                             1309
            GOTO 6693                                                     1309
6691        I=I+(2)                                                       1309
6693        IF((2)*(I-(1)).GT.0) GO TO 6692                               1309
C       *                                                                 1310
            J=0                                                           1310
               GO TO 6703                                                 1310
6701           J=J+1                                                      1310
6703           IF(J-(1).GT.0) GO TO 6702                                  1310
               PHI=I*SSUB_SRCUTPHI+180*J                                  1311
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1312
     *         2.0)                                                       1312
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1314
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1314
C          ==                                                             1314
               CALL AGSRESET                                              1314
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1314
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1314
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1314
               AG_ALPHAN=AG_ALPHAN+1                                      1314
               AG_ALPHAK(AG_ALPHAN)=3                                     1314
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1314
               AG_TITLE = 'SBMM'                                          1314
               AG_EXNAME = 'POSITION'                                     1314
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1314
               CALL AXPOSITION                                            1314
C          *                                                              1314
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1314
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1317
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1317
C           ORT=ZXY <===                                                  1317
               CALL AGSRESET                                              1317
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1317
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1317
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1317
               AG_ALPHAN=AG_ALPHAN+1                                      1317
               AG_ALPHAK(AG_ALPHAN)=3                                     1317
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1317
               AG_ALPHAN=AG_ALPHAN+1                                      1317
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1317
               AG_TITLE = 'SBRL'                                          1317
               AG_EXNAME = 'POSITION'                                     1317
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1317
               CALL AXPOSITION                                            1317
C          *                                                              1317
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1317
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1317
C           ORT=ZXY <===                                                  1317
               CALL AGSRESET                                              1319
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1319
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1319
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1319
               AG_ALPHAN=AG_ALPHAN+1                                      1319
               AG_ALPHAK(AG_ALPHAN)=3                                     1319
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1319
               AG_ALPHAN=AG_ALPHAN+1                                      1319
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1319
               AG_TITLE = 'SBRX'                                          1319
               AG_EXNAME = 'POSITION'                                     1319
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1319
               CALL AXPOSITION                                            1319
C          *                                                              1319
            GO TO 6701                                                    1320
6702        CONTINUE                                                      1320
         GO TO 6691                                                       1321
6692     CONTINUE                                                         1321
C    Create SBSR :                                                        1322
C    Call To Local Procedure SBSR                                         1322
         ASSIGN 6720 TO BLOCK_SBSR                                        1322
         GO TO  6710                                                      1322
6720     CONTINUE                                                         1322
C    *                                                                    1322
C    Geant POSITION : SBSR ===> inherited arguments used <===             1323
         CALL AGSRESET                                                    1323
         AG_TITLE = 'SBSR'                                                1323
         AG_EXNAME = 'POSITION'                                           1323
         AG_PARLIST = 'NONE'                                              1323
         CALL AXPOSITION                                                  1323
C    *                                                                    1323
5231  CALL AGSPOPB                                                        1324
      GO TO BLOCK_SBSP, (5230 ,5240)                                      1324
C End of Local Procedure                                                  1324
C ----------------------------------------------------------------------  1325
C ------------ SAKM                                                       1325
C Begin Local Procedure SAKM                                              1326
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1326
6600  CONTINUE                                                            1326
         AG_TITLE='SAKM'                                                  1326
         AG_MODULE=MODULE                                                 1326
         AG_MARK='V00'                                                    1326
         CALL AGSPUSH                                                     1326
         AG_MODULE=MODULE                                                 1326
C    *                                                                    1326
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1327
         CALL AGSRESET                                                    1327
         AG_TITLE = 'ALUMINIUM'                                           1327
         AG_EXNAME = 'MATERIAL'                                           1327
         AG_PARLIST = 'NONE'                                              1327
         CALL AXMATERIAL                                                  1327
C    *                                                                    1327
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1327
         CALL AGSRESET                                                    1328
         AG_SEEN = 1                                                      1328
         AG_COLO = 2                                                      1328
         AG_TITLE = 'SAKM'                                                1328
         AG_EXNAME = 'ATTRIBUTE'                                          1328
         AG_PARLIST = 'SEEN_COLO'                                         1328
         CALL AXATTRIBUTE                                                 1328
C    *                                                                    1328
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1328
C    DZ=SSUB_KMNTTHK/2 <===                                               1328
         CALL AGSRESET                                                    1330
         AG_RMIN = SSUB_KMOUNTID/2                                        1330
         AG_RMAX = SSUB_KMOUNTOD/2                                        1330
         AG_DZ = SSUB_KMNTTHK/2                                           1330
         AG_TITLE = 'TUBE'                                                1330
         AG_EXNAME = 'SHAPE'                                              1330
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1330
         CALL AXSHAPE                                                     1330
C    *                                                                    1330
C    Create SCKM :                                                        1331
C    Call To Local Procedure SCKM                                         1331
         ASSIGN 6740 TO BLOCK_SCKM                                        1331
         GO TO  6730                                                      1331
6740     CONTINUE                                                         1331
C    *                                                                    1331
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1332
         CALL AGSRESET                                                    1332
         AG_Y = SSUB_KMCUTOFF                                             1332
         AG_KONLY = 'MANY'                                                1332
         AG_TITLE = 'SCKM'                                                1332
         AG_EXNAME = 'POSITION'                                           1332
         AG_PARLIST = 'Y_KONLY'                                           1332
         CALL AXPOSITION                                                  1332
C    *                                                                    1332
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1332
C     <===                                                                1332
         CALL AGSRESET                                                    1333
         AG_Y = -SSUB_KMCUTOFF                                            1333
         AG_ALPHAN=AG_ALPHAN+1                                            1333
         AG_ALPHAK(AG_ALPHAN)=3                                           1333
         AG_ALPHAL(AG_ALPHAN)=180                                         1333
         AG_KONLY = 'MANY'                                                1333
         AG_TITLE = 'SCKM'                                                1333
         AG_EXNAME = 'POSITION'                                           1333
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1333
         CALL AXPOSITION                                                  1333
C    *                                                                    1333
6601  CALL AGSPOPB                                                        1334
      GO TO BLOCK_SAKM, (6600 ,6610)                                      1334
C End of Local Procedure                                                  1334
C ----------------------------------------------------------------------  1335
C ------------ SCKM                                                       1335
C Begin Local Procedure SCKM                                              1336
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1336
6730  CONTINUE                                                            1336
         AG_TITLE='SCKM'                                                  1336
         AG_MODULE=MODULE                                                 1336
         AG_MARK='V00'                                                    1336
         CALL AGSPUSH                                                     1336
         AG_MODULE=MODULE                                                 1336
C    *                                                                    1336
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1337
         CALL AGSRESET                                                    1337
         AG_TITLE = 'AIR'                                                 1337
         AG_EXNAME = 'MATERIAL'                                           1337
         AG_PARLIST = 'NONE'                                              1337
         CALL AXMATERIAL                                                  1337
C    *                                                                    1337
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1337
         CALL AGSRESET                                                    1338
         AG_SEEN = 1                                                      1338
         AG_COLO = 1                                                      1338
         AG_TITLE = 'SCKM'                                                1338
         AG_EXNAME = 'ATTRIBUTE'                                          1338
         AG_PARLIST = 'SEEN_COLO'                                         1338
         CALL AXATTRIBUTE                                                 1338
C    *                                                                    1338
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1338
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1338
         CALL AGSRESET                                                    1341
         AG_RMIN = SSUB_KMCUTID/2                                         1341
         AG_RMAX = SSUB_KMCUTOD/2                                         1341
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1341
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1341
         AG_DZ = SSUB_KMNTTHK/2                                           1341
         AG_TITLE = 'TUBS'                                                1341
         AG_EXNAME = 'SHAPE'                                              1341
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1341
         CALL AXSHAPE                                                     1341
C    *                                                                    1341
6731  CALL AGSPOPB                                                        1342
      GO TO BLOCK_SCKM, (6730 ,6740)                                      1342
C End of Local Procedure                                                  1342
C ----------------------------------------------------------------------  1343
C ------------ SBSR                                                       1343
C Begin Local Procedure SBSR                                              1344
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1344
6710  CONTINUE                                                            1344
         AG_TITLE='SBSR'                                                  1344
         AG_MODULE=MODULE                                                 1344
         AG_MARK='V00'                                                    1344
         CALL AGSPUSH                                                     1344
         AG_MODULE=MODULE                                                 1344
C    *                                                                    1344
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1345
         CALL AGSRESET                                                    1345
         AG_TITLE = 'G10'                                                 1345
         AG_EXNAME = 'MATERIAL'                                           1345
         AG_PARLIST = 'NONE'                                              1345
         CALL AXMATERIAL                                                  1345
C    *                                                                    1345
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1345
         CALL AGSRESET                                                    1346
         AG_SEEN = 1                                                      1346
         AG_COLO = 3                                                      1346
         AG_TITLE = 'SBSR'                                                1346
         AG_EXNAME = 'ATTRIBUTE'                                          1346
         AG_PARLIST = 'SEEN_COLO'                                         1346
         CALL AXATTRIBUTE                                                 1346
C    *                                                                    1346
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1346
C    =SSUB_SRINGTHK/2 <===                                                1346
         CALL AGSRESET                                                    1348
         AG_RMIN = SSUB_SRINGID/2                                         1348
         AG_RMAX = SSUB_SRINGOD/2                                         1348
         AG_DZ = SSUB_SRINGTHK/2                                          1348
         AG_TITLE = 'TUBE'                                                1348
         AG_EXNAME = 'SHAPE'                                              1348
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1348
         CALL AXSHAPE                                                     1348
C    *                                                                    1348
C    Create SBCR :                                                        1349
C    Call To Local Procedure SBCR                                         1349
         ASSIGN 6760 TO BLOCK_SBCR                                        1349
         GO TO  6750                                                      1349
6760     CONTINUE                                                         1349
C    *                                                                    1349
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1350
C    *                                                                    1351
         I=-1                                                             1351
            GOTO 6773                                                     1351
6771        I=I+(2)                                                       1351
6773        IF((2)*(I-(1)).GT.0) GO TO 6772                               1351
C       *                                                                 1352
            J=0                                                           1352
               GO TO 6783                                                 1352
6781           J=J+1                                                      1352
6783           IF(J-(1).GT.0) GO TO 6782                                  1352
               PHI=I*SSUB_SRCUTPHI+180*J                                  1353
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1355
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1355
               CALL AGSRESET                                              1355
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1355
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1355
               AG_ALPHAN=AG_ALPHAN+1                                      1355
               AG_ALPHAK(AG_ALPHAN)=3                                     1355
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1355
               AG_KONLY = 'MANY'                                          1355
               AG_TITLE = 'SBCR'                                          1355
               AG_EXNAME = 'POSITION'                                     1355
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1355
               CALL AXPOSITION                                            1355
C          *                                                              1355
            GO TO 6781                                                    1356
6782        CONTINUE                                                      1356
         GO TO 6771                                                       1357
6772     CONTINUE                                                         1357
6711  CALL AGSPOPB                                                        1358
      GO TO BLOCK_SBSR, (6710 ,6720)                                      1358
C End of Local Procedure                                                  1358
C ----------------------------------------------------------------------  1359
C ------------ SBCR                                                       1359
C Begin Local Procedure SBCR                                              1360
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1360
6750  CONTINUE                                                            1360
         AG_TITLE='SBCR'                                                  1360
         AG_MODULE=MODULE                                                 1360
         AG_MARK='V00'                                                    1360
         CALL AGSPUSH                                                     1360
         AG_MODULE=MODULE                                                 1360
C    *                                                                    1360
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1361
         CALL AGSRESET                                                    1361
         AG_TITLE = 'AIR'                                                 1361
         AG_EXNAME = 'MATERIAL'                                           1361
         AG_PARLIST = 'NONE'                                              1361
         CALL AXMATERIAL                                                  1361
C    *                                                                    1361
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1361
         CALL AGSRESET                                                    1362
         AG_SEEN = 1                                                      1362
         AG_COLO = 1                                                      1362
         AG_TITLE = 'SBCR'                                                1362
         AG_EXNAME = 'ATTRIBUTE'                                          1362
         AG_PARLIST = 'SEEN_COLO'                                         1362
         CALL AXATTRIBUTE                                                 1362
C    *                                                                    1362
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1362
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1362
         CALL AGSRESET                                                    1364
         AG_DX = SSUB_SRCUTWID/2                                          1364
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1364
         AG_DZ = SSUB_SRINGTHK/2                                          1364
         AG_TITLE = 'BOX'                                                 1364
         AG_EXNAME = 'SHAPE'                                              1364
         AG_PARLIST = 'DX_DY_DZ'                                          1364
         CALL AXSHAPE                                                     1364
C    *                                                                    1364
6751  CALL AGSPOPB                                                        1365
      GO TO BLOCK_SBCR, (6750 ,6760)                                      1365
C End of Local Procedure                                                  1365
C ----------------------------------------------------------------------  1366
C ------------ SBRL                                                       1366
C Begin Local Procedure SBRL                                              1367
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1367
6650  CONTINUE                                                            1367
         AG_TITLE='SBRL'                                                  1367
         AG_MODULE=MODULE                                                 1367
         AG_MARK='V00'                                                    1367
         CALL AGSPUSH                                                     1367
         AG_MODULE=MODULE                                                 1367
C    *                                                                    1367
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1368
C    ADL=12.6 <===                                                        1368
         CALL AGSRESET                                                    1368
         AG_A = 20.719                                                    1368
         AG_Z = 10.307                                                    1368
         AG_DENS = 2.23                                                   1368
         AG_ABSL = 50.7                                                   1368
         AG_RADL = 12.6                                                   1368
         AG_TITLE = 'PYREX'                                               1368
         AG_EXNAME = 'MATERIAL'                                           1368
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1368
         CALL AXMATERIAL                                                  1368
C    *                                                                    1368
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1368
         CALL AGSRESET                                                    1369
         AG_SEEN = 1                                                      1369
         AG_COLO = 6                                                      1369
         AG_TITLE = 'SBRL'                                                1369
         AG_EXNAME = 'ATTRIBUTE'                                          1369
         AG_PARLIST = 'SEEN_COLO'                                         1369
         CALL AXATTRIBUTE                                                 1369
C    *                                                                    1369
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1369
C    =                                                                    1369
         CALL AGSRESET                                                    1370
         AG_RMIN = SSUB_SROLLID/2                                         1370
         AG_RMAX = SSUB_SROLLOD/2                                         1370
         AG_TITLE = 'TUBE'                                                1370
         AG_EXNAME = 'SHAPE'                                              1370
         AG_PARLIST = 'RMIN_RMAX'                                         1370
         CALL AXSHAPE                                                     1370
C    *                                                                    1370
6651  CALL AGSPOPB                                                        1371
      GO TO BLOCK_SBRL, (6650 ,6660)                                      1371
C End of Local Procedure                                                  1371
C ----------------------------------------------------------------------  1372
C ------------ SBRX                                                       1372
C Begin Local Procedure SBRX                                              1373
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1373
6670  CONTINUE                                                            1373
         AG_TITLE='SBRX'                                                  1373
         AG_MODULE=MODULE                                                 1373
         AG_MARK='V00'                                                    1373
         CALL AGSPUSH                                                     1373
         AG_MODULE=MODULE                                                 1373
C    *                                                                    1373
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1374
         CALL AGSRESET                                                    1374
         AG_TITLE = 'IRON'                                                1374
         AG_EXNAME = 'MATERIAL'                                           1374
         AG_PARLIST = 'NONE'                                              1374
         CALL AXMATERIAL                                                  1374
C    *                                                                    1374
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1374
         CALL AGSRESET                                                    1375
         AG_SEEN = 1                                                      1375
         AG_COLO = 2                                                      1375
         AG_TITLE = 'SBRX'                                                1375
         AG_EXNAME = 'ATTRIBUTE'                                          1375
         AG_PARLIST = 'SEEN_COLO'                                         1375
         CALL AXATTRIBUTE                                                 1375
C    *                                                                    1375
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1375
C    LEN/2 <===                                                           1375
         CALL AGSRESET                                                    1377
         AG_RMIN = 0.0                                                    1377
         AG_RMAX = SSUB_SROLLID/2                                         1377
         AG_DZ = SSUB_SWIRELEN/2                                          1377
         AG_TITLE = 'TUBE'                                                1377
         AG_EXNAME = 'SHAPE'                                              1377
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1377
         CALL AXSHAPE                                                     1377
C    *                                                                    1377
6671  CALL AGSPOPB                                                        1378
      GO TO BLOCK_SBRX, (6670 ,6680)                                      1378
C End of Local Procedure                                                  1378
C ----------------------------------------------------------------------  1379
C ------------ SBMM                                                       1379
C Begin Local Procedure SBMM                                              1380
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1380
6620  CONTINUE                                                            1380
         AG_TITLE='SBMM'                                                  1380
         AG_MODULE=MODULE                                                 1380
         AG_MARK='V00'                                                    1380
         CALL AGSPUSH                                                     1380
         AG_MODULE=MODULE                                                 1380
C    *                                                                    1380
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1381
         CALL AGSRESET                                                    1381
         AG_TITLE = 'AIR'                                                 1381
         AG_EXNAME = 'MATERIAL'                                           1381
         AG_PARLIST = 'NONE'                                              1381
         CALL AXMATERIAL                                                  1381
C    *                                                                    1381
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1381
         CALL AGSRESET                                                    1382
         AG_SEEN = 0                                                      1382
         AG_COLO = 1                                                      1382
         AG_TITLE = 'SBMM'                                                1382
         AG_EXNAME = 'ATTRIBUTE'                                          1382
         AG_PARLIST = 'SEEN_COLO'                                         1382
         CALL AXATTRIBUTE                                                 1382
C    *                                                                    1382
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1382
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1382
         CALL AGSRESET                                                    1384
         AG_DX = SSUB_MBLKIWID/2                                          1384
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1384
         AG_DZ = SSUB_MBLKHGH/2                                           1384
         AG_TITLE = 'BOX'                                                 1384
         AG_EXNAME = 'SHAPE'                                              1384
         AG_PARLIST = 'DX_DY_DZ'                                          1384
         CALL AXSHAPE                                                     1384
C    *                                                                    1384
C    Create SMRD :                                                        1385
C    Call To Local Procedure SMRD                                         1385
         ASSIGN 6800 TO BLOCK_SMRD                                        1385
         GO TO  6790                                                      1385
6800     CONTINUE                                                         1385
C    *                                                                    1385
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1386
     *   SSUB_SROLLOD                                                     1386
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1387
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1387
         CALL AGSRESET                                                    1387
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1387
         AG_ALPHAN=AG_ALPHAN+1                                            1387
         AG_ALPHAK(AG_ALPHAN)=1                                           1387
         AG_ALPHAL(AG_ALPHAN)=90                                          1387
         AG_TITLE = 'SMRD'                                                1387
         AG_EXNAME = 'POSITION'                                           1387
         AG_PARLIST = 'Y_ALPHAX'                                          1387
         CALL AXPOSITION                                                  1387
C    *                                                                    1387
C    Create SBMO :                                                        1388
C    Call To Local Procedure SBMO                                         1388
         ASSIGN 6820 TO BLOCK_SBMO                                        1388
         GO TO  6810                                                      1388
6820     CONTINUE                                                         1388
C    *                                                                    1388
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1389
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1390
C    UB_SROLLOD)/2 <===                                                   1390
         CALL AGSRESET                                                    1390
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1390
         AG_TITLE = 'SBMO'                                                1390
         AG_EXNAME = 'POSITION'                                           1390
         AG_PARLIST = 'Y'                                                 1390
         CALL AXPOSITION                                                  1390
C    *                                                                    1390
C    Create SBMI :                                                        1391
C    Call To Local Procedure SBMI                                         1391
         ASSIGN 6840 TO BLOCK_SBMI                                        1391
         GO TO  6830                                                      1391
6840     CONTINUE                                                         1391
C    *                                                                    1391
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1392
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1393
C    UB_SROLLOD)/2 <===                                                   1393
         CALL AGSRESET                                                    1393
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1393
         AG_TITLE = 'SBMI'                                                1393
         AG_EXNAME = 'POSITION'                                           1393
         AG_PARLIST = 'Y'                                                 1393
         CALL AXPOSITION                                                  1393
C    *                                                                    1393
6621  CALL AGSPOPB                                                        1394
      GO TO BLOCK_SBMM, (6620 ,6630,6640)                                 1394
C End of Local Procedure                                                  1394
C ----------------------------------------------------------------------  1395
C ------------ SMRD                                                       1395
C Begin Local Procedure SMRD                                              1396
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1396
6790  CONTINUE                                                            1396
         AG_TITLE='SMRD'                                                  1396
         AG_MODULE=MODULE                                                 1396
         AG_MARK='V00'                                                    1396
         CALL AGSPUSH                                                     1396
         AG_MODULE=MODULE                                                 1396
C    *                                                                    1396
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1397
         CALL AGSRESET                                                    1397
         AG_TITLE = 'ALUMINIUM'                                           1397
         AG_EXNAME = 'MATERIAL'                                           1397
         AG_PARLIST = 'NONE'                                              1397
         CALL AXMATERIAL                                                  1397
C    *                                                                    1397
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1397
         CALL AGSRESET                                                    1398
         AG_SEEN = 1                                                      1398
         AG_COLO = 2                                                      1398
         AG_TITLE = 'SMRD'                                                1398
         AG_EXNAME = 'ATTRIBUTE'                                          1398
         AG_PARLIST = 'SEEN_COLO'                                         1398
         CALL AXATTRIBUTE                                                 1398
C    *                                                                    1398
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1398
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1398
         CALL AGSRESET                                                    1400
         AG_RMIN = 0.0                                                    1400
         AG_RMAX = SSUB_MRODDIA/2                                         1400
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1400
         AG_TITLE = 'TUBE'                                                1400
         AG_EXNAME = 'SHAPE'                                              1400
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1400
         CALL AXSHAPE                                                     1400
C    *                                                                    1400
6791  CALL AGSPOPB                                                        1401
      GO TO BLOCK_SMRD, (6790 ,6800)                                      1401
C End of Local Procedure                                                  1401
C ----------------------------------------------------------------------  1402
C ------------ SBMO                                                       1402
C Begin Local Procedure SBMO                                              1403
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1403
6810  CONTINUE                                                            1403
         AG_TITLE='SBMO'                                                  1403
         AG_MODULE=MODULE                                                 1403
         AG_MARK='V00'                                                    1403
         CALL AGSPUSH                                                     1403
         AG_MODULE=MODULE                                                 1403
C    *                                                                    1403
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1404
         CALL AGSRESET                                                    1404
         AG_TITLE = 'G10'                                                 1404
         AG_EXNAME = 'MATERIAL'                                           1404
         AG_PARLIST = 'NONE'                                              1404
         CALL AXMATERIAL                                                  1404
C    *                                                                    1404
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1404
         CALL AGSRESET                                                    1405
         AG_SEEN = 1                                                      1405
         AG_COLO = 3                                                      1405
         AG_TITLE = 'SBMO'                                                1405
         AG_EXNAME = 'ATTRIBUTE'                                          1405
         AG_PARLIST = 'SEEN_COLO'                                         1405
         CALL AXATTRIBUTE                                                 1405
C    *                                                                    1405
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1405
C    UB_MBLKHGH/2 <===                                                    1405
         CALL AGSRESET                                                    1407
         AG_DX = SSUB_MBLKOWID/2                                          1407
         AG_DY = SSUB_MBLKOLEN/2                                          1407
         AG_DZ = SSUB_MBLKHGH/2                                           1407
         AG_TITLE = 'BOX'                                                 1407
         AG_EXNAME = 'SHAPE'                                              1407
         AG_PARLIST = 'DX_DY_DZ'                                          1407
         CALL AXSHAPE                                                     1407
C    *                                                                    1407
6811  CALL AGSPOPB                                                        1408
      GO TO BLOCK_SBMO, (6810 ,6820)                                      1408
C End of Local Procedure                                                  1408
C ----------------------------------------------------------------------  1409
C ------------ SBMI                                                       1409
C Begin Local Procedure SBMI                                              1410
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1410
6830  CONTINUE                                                            1410
         AG_TITLE='SBMI'                                                  1410
         AG_MODULE=MODULE                                                 1410
         AG_MARK='V00'                                                    1410
         CALL AGSPUSH                                                     1410
         AG_MODULE=MODULE                                                 1410
C    *                                                                    1410
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1411
         CALL AGSRESET                                                    1411
         AG_TITLE = 'G10'                                                 1411
         AG_EXNAME = 'MATERIAL'                                           1411
         AG_PARLIST = 'NONE'                                              1411
         CALL AXMATERIAL                                                  1411
C    *                                                                    1411
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1411
         CALL AGSRESET                                                    1412
         AG_SEEN = 1                                                      1412
         AG_COLO = 3                                                      1412
         AG_TITLE = 'SBMO'                                                1412
         AG_EXNAME = 'ATTRIBUTE'                                          1412
         AG_PARLIST = 'SEEN_COLO'                                         1412
         CALL AXATTRIBUTE                                                 1412
C    *                                                                    1412
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1412
C    UB_MBLKHGH/2 <===                                                    1412
         CALL AGSRESET                                                    1414
         AG_DX = SSUB_MBLKIWID/2                                          1414
         AG_DY = SSUB_MBLKILEN/2                                          1414
         AG_DZ = SSUB_MBLKHGH/2                                           1414
         AG_TITLE = 'BOX'                                                 1414
         AG_EXNAME = 'SHAPE'                                              1414
         AG_PARLIST = 'DX_DY_DZ'                                          1414
         CALL AXSHAPE                                                     1414
C    *                                                                    1414
6831  CALL AGSPOPB                                                        1415
      GO TO BLOCK_SBMI, (6830 ,6840)                                      1415
C End of Local Procedure                                                  1415
C ----------------------------------------------------------------------  1416
C ------------ SALM                                                       1416
C Begin Local Procedure SALM                                              1417
C SALM IS THE ALUMINUM SHIELD MESH                                        1417
5370  CONTINUE                                                            1417
         AG_TITLE='SALM'                                                  1417
         AG_MODULE=MODULE                                                 1417
         AG_MARK='V00'                                                    1417
         CALL AGSPUSH                                                     1417
         AG_MODULE=MODULE                                                 1417
C    *                                                                    1417
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1418
         CALL AGSRESET                                                    1418
         AG_TITLE = 'ALUMINIUM'                                           1418
         AG_EXNAME = 'MATERIAL'                                           1418
         AG_PARLIST = 'NONE'                                              1418
         CALL AXMATERIAL                                                  1418
C    *                                                                    1418
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1418
         CALL AGSRESET                                                    1419
         AG_SEEN = 1                                                      1419
         AG_COLO = 2                                                      1419
         AG_TITLE = 'SALM'                                                1419
         AG_EXNAME = 'ATTRIBUTE'                                          1419
         AG_PARLIST = 'SEEN_COLO'                                         1419
         CALL AXATTRIBUTE                                                 1419
C    *                                                                    1419
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1419
C    DZ=SSLD_ALMSHTHK/2 <===                                              1419
         CALL AGSRESET                                                    1421
         AG_RMIN = SSLD_ALMESHID/2                                        1421
         AG_RMAX = SSLD_ALMESHOD/2                                        1421
         AG_DZ = SSLD_ALMSHTHK/2                                          1421
         AG_TITLE = 'TUBE'                                                1421
         AG_EXNAME = 'SHAPE'                                              1421
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1421
         CALL AXSHAPE                                                     1421
C    *                                                                    1421
5371  CALL AGSPOPB                                                        1422
      GO TO BLOCK_SALM, (5370 ,5380)                                      1422
C End of Local Procedure                                                  1422
C ----------------------------------------------------------------------  1423
C ------------ SISH                                                       1423
C Begin Local Procedure SISH                                              1424
C SISH IS THE INNER SHIELD CYLINDER                                       1424
5310  CONTINUE                                                            1424
         AG_TITLE='SISH'                                                  1424
         AG_MODULE=MODULE                                                 1424
         AG_MARK='V00'                                                    1424
         CALL AGSPUSH                                                     1424
         AG_MODULE=MODULE                                                 1424
C    *                                                                    1424
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1425
         CALL AGSRESET                                                    1425
         AG_TITLE = 'ALKAP'                                               1425
         AG_EXNAME = 'MATERIAL'                                           1425
         AG_PARLIST = 'NONE'                                              1425
         CALL AXMATERIAL                                                  1425
C    *                                                                    1425
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1425
         CALL AGSRESET                                                    1426
         AG_SEEN = 1                                                      1426
         AG_COLO = 3                                                      1426
         AG_TITLE = 'SISH'                                                1426
         AG_EXNAME = 'ATTRIBUTE'                                          1426
         AG_PARLIST = 'SEEN_COLO'                                         1426
         CALL AXATTRIBUTE                                                 1426
C    *                                                                    1426
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1426
C    D_SINLEN <===                                                        1426
         CALL AGSRESET                                                    1428
         AG_RMIN = SSLD_SINRINN                                           1428
         AG_RMAX = SSLD_SINROUT                                           1428
         AG_DZ = SSLD_SINLEN                                              1428
         AG_TITLE = 'TUBE'                                                1428
         AG_EXNAME = 'SHAPE'                                              1428
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1428
         CALL AXSHAPE                                                     1428
C    *                                                                    1428
5311  CALL AGSPOPB                                                        1429
      GO TO BLOCK_SISH, (5310 ,5320)                                      1429
C End of Local Procedure                                                  1429
C ----------------------------------------------------------------------  1430
C ------------ SSSH                                                       1430
C Begin Local Procedure SSSH                                              1431
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1431
5350  CONTINUE                                                            1431
         AG_TITLE='SSSH'                                                  1431
         AG_MODULE=MODULE                                                 1431
         AG_MARK='V00'                                                    1431
         CALL AGSPUSH                                                     1431
         AG_MODULE=MODULE                                                 1431
C    *                                                                    1431
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1432
         CALL AGSRESET                                                    1432
         AG_TITLE = 'SSDALMY'                                             1432
         AG_EXNAME = 'MATERIAL'                                           1432
         AG_PARLIST = 'NONE'                                              1432
         CALL AXMATERIAL                                                  1432
C    *                                                                    1432
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1432
         CALL AGSRESET                                                    1433
         AG_SEEN = 1                                                      1433
         AG_COLO = 3                                                      1433
         AG_TITLE = 'SSSH'                                                1433
         AG_EXNAME = 'ATTRIBUTE'                                          1433
         AG_PARLIST = 'SEEN_COLO'                                         1433
         CALL AXATTRIBUTE                                                 1433
C    *                                                                    1433
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1433
C    SLD_SSEPLEN <===                                                     1433
         CALL AGSRESET                                                    1435
         AG_RMIN = SSLD_SSEPRINN                                          1435
         AG_RMAX = SSLD_SSEPROUT                                          1435
         AG_DZ = SSLD_SSEPLEN                                             1435
         AG_TITLE = 'TUBE'                                                1435
         AG_EXNAME = 'SHAPE'                                              1435
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1435
         CALL AXSHAPE                                                     1435
C    *                                                                    1435
5351  CALL AGSPOPB                                                        1436
      GO TO BLOCK_SSSH, (5350 ,5360)                                      1436
C End of Local Procedure                                                  1436
C ----------------------------------------------------------------------  1437
C ------------ SOSH                                                       1437
C Begin Local Procedure SOSH                                              1438
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1438
5330  CONTINUE                                                            1438
         AG_TITLE='SOSH'                                                  1438
         AG_MODULE=MODULE                                                 1438
         AG_MARK='V00'                                                    1438
         CALL AGSPUSH                                                     1438
         AG_MODULE=MODULE                                                 1438
C    *                                                                    1438
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1439
         CALL AGSRESET                                                    1439
         AG_TITLE = 'SSDALMY'                                             1439
         AG_EXNAME = 'MATERIAL'                                           1439
         AG_PARLIST = 'NONE'                                              1439
         CALL AXMATERIAL                                                  1439
C    *                                                                    1439
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1439
         CALL AGSRESET                                                    1440
         AG_SEEN = 1                                                      1440
         AG_COLO = 3                                                      1440
         AG_TITLE = 'SOSH'                                                1440
         AG_EXNAME = 'ATTRIBUTE'                                          1440
         AG_PARLIST = 'SEEN_COLO'                                         1440
         CALL AXATTRIBUTE                                                 1440
C    *                                                                    1440
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1440
C    SLD_SOUTLEN <===                                                     1440
         CALL AGSRESET                                                    1442
         AG_RMIN = SSLD_SOUTRINN                                          1442
         AG_RMAX = SSLD_SOUTROUT                                          1442
         AG_DZ = SSLD_SOUTLEN                                             1442
         AG_TITLE = 'TUBE'                                                1442
         AG_EXNAME = 'SHAPE'                                              1442
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1442
         CALL AXSHAPE                                                     1442
C    *                                                                    1442
5331  CALL AGSPOPB                                                        1443
      GO TO BLOCK_SOSH, (5330 ,5340)                                      1443
C End of Local Procedure                                                  1443
      WRITE(LOUT,6850)'===>>  [End AgML Module SVTTGEO11]  <<==='         1443
6850  FORMAT(A32,/,/)                                                     1443
C MODULE SVTTGEO11                                                        1445
      END                                                                 1445
