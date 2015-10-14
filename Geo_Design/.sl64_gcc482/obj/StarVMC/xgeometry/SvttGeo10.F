C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO10                                                2
C SVTTGEO10 IS EVOLUTION OF SVTTGEO6 WITH CARBON REPLACING BE AS PER, DA  2
C VE LYNN:                                                                2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'SVTTGEO10'/, BANK_TITLE/'*'/                           2
      DATA MCOMMENT/'IS EVOLUTION OF SVTTGEO6 WITH CARBON REPLACING BE A  2
     *S PER, DAVE LYNN:'/                                                 2
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
      INTEGER BLOCK_SLYD                                                  13
      INTEGER BLOCK_SLSD                                                  13
      INTEGER BLOCK_SLDI                                                  13
      INTEGER BLOCK_STLI                                                  13
      INTEGER BLOCK_STSI                                                  13
      INTEGER BLOCK_SVTD                                                  13
      INTEGER BLOCK_SBER                                                  13
      INTEGER BLOCK_STAB                                                  13
      INTEGER BLOCK_STRU                                                  13
      INTEGER BLOCK_SPCB                                                  13
      INTEGER BLOCK_SRHC                                                  13
      INTEGER BLOCK_SBWC                                                  13
      INTEGER BLOCK_SWCM                                                  13
      INTEGER BLOCK_SXAI                                                  13
      INTEGER BLOCK_SXBI                                                  13
      INTEGER BLOCK_STRA                                                  13
      INTEGER BLOCK_SSID                                                  13
      INTEGER BLOCK_SSIR                                                  13
      INTEGER BLOCK_SELE                                                  13
      INTEGER BLOCK_SWCH                                                  13
      INTEGER BLOCK_SWCW                                                  13
      INTEGER BLOCK_SWCS                                                  13
      INTEGER BLOCK_SBOI                                                  13
      INTEGER BLOCK_SAGP                                                  13
      INTEGER BLOCK_SDYE                                                  13
      INTEGER BLOCK_SECA                                                  13
      INTEGER BLOCK_SIRP                                                  13
      INTEGER BLOCK_SIRT                                                  13
      INTEGER BLOCK_SOER                                                  13
      INTEGER BLOCK_SCON                                                  13
      INTEGER BLOCK_SROD                                                  13
      INTEGER BLOCK_SGRA                                                  13
      INTEGER BLOCK_STAP                                                  13
      INTEGER BLOCK_STAC                                                  13
      INTEGER BLOCK_SHLA                                                  13
      INTEGER BLOCK_SHLB                                                  13
      INTEGER BLOCK_SHMA                                                  13
      INTEGER BLOCK_SHMB                                                  13
      INTEGER BLOCK_SWHO                                                  13
      INTEGER BLOCK_SHWA                                                  13
      INTEGER BLOCK_SCMY                                                  13
      INTEGER BLOCK_SCAL                                                  13
      INTEGER BLOCK_SWMM                                                  13
      INTEGER BLOCK_SWMB                                                  13
      INTEGER BLOCK_SWRP                                                  13
      INTEGER BLOCK_SXRL                                                  13
      INTEGER BLOCK_SYRU                                                  13
      INTEGER BLOCK_SWMT                                                  13
      INTEGER BLOCK_SWMS                                                  13
      INTEGER BLOCK_SWMW                                                  13
      INTEGER BLOCK_SOTB                                                  13
      INTEGER BLOCK_SITB                                                  13
      INTEGER BLOCK_SBRG                                                  13
      INTEGER BLOCK_SBRM                                                  13
      INTEGER BLOCK_SBRI                                                  13
      INTEGER BLOCK_SOES                                                  13
      INTEGER BLOCK_SIES                                                  13
      INTEGER BLOCK_SOSM                                                  13
      INTEGER BLOCK_SISM                                                  13
      INTEGER BLOCK_SCRW                                                  13
      INTEGER BLOCK_SGLA                                                  13
      INTEGER BLOCK_SBSP                                                  13
      INTEGER BLOCK_SAKM                                                  13
      INTEGER BLOCK_SCKM                                                  13
      INTEGER BLOCK_SBSR                                                  13
      INTEGER BLOCK_SBCR                                                  13
      INTEGER BLOCK_SBRX                                                  13
      INTEGER BLOCK_SBRL                                                  13
      INTEGER BLOCK_SBMM                                                  13
      INTEGER BLOCK_SBMO                                                  13
      INTEGER BLOCK_SBMI                                                  13
      INTEGER BLOCK_SMRD                                                  13
      INTEGER BLOCK_SALM                                                  13
      INTEGER BLOCK_SISH                                                  13
      INTEGER BLOCK_SSSH                                                  13
      INTEGER BLOCK_SOSH                                                  13
      INTEGER BLOCK_SCBM                                                  13
      INTEGER BLOCK_SCBL                                                  13
      INTEGER BLOCK_SFED                                                  13
      INTEGER BLOCK_SPLS                                                  13
      INTEGER BLOCK_SOUM                                                  13
      INTEGER BLOCK_SOUR                                                  13
C -                                                                       15
      REAL SVTG_VERSION                                                   15
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            15
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       15
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          15
     *MSVTG_VERSION)                                                      15
      CHARACTER CSVTG_VERSION*40/' '/                                     15
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             15
      INTEGER MSVTG_VERSION(2)/1,1/                                       15
      REAL RE_SVTG_VERSION                                                15
C -                                                                       15
      INTEGER SVTG_NLAYER                                                 15
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              15
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        15
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           15
     *MSVTG_NLAYER)                                                       15
      CHARACTER CSVTG_NLAYER*40/' '/                                      15
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               15
      INTEGER MSVTG_NLAYER(2)/1,1/                                        15
      REAL RE_SVTG_NLAYER                                                 15
C -                                                                       15
      REAL SVTG_RSIZEMIN                                                  15
      EQUIVALENCE (BANK_SVTG(5),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          15
     *(NAMES_SVTG(3),NSVTG_RSIZEMIN)                                      15
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_RSIZEMIN),(MAP_SVTG(1,3),         15
     *MSVTG_RSIZEMIN)                                                     15
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    15
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           15
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      15
      REAL RE_SVTG_RSIZEMIN                                               15
C -                                                                       15
      REAL SVTG_RSIZEMAX                                                  15
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          15
     *(NAMES_SVTG(4),NSVTG_RSIZEMAX)                                      15
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMAX),(MAP_SVTG(1,4),         15
     *MSVTG_RSIZEMAX)                                                     15
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    15
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           15
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      15
      REAL RE_SVTG_RSIZEMAX                                               15
C -                                                                       15
      REAL SVTG_ZSIZEMAX                                                  15
      EQUIVALENCE (BANK_SVTG(7),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          15
     *(NAMES_SVTG(5),NSVTG_ZSIZEMAX)                                      15
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_ZSIZEMAX),(MAP_SVTG(1,5),         15
     *MSVTG_ZSIZEMAX)                                                     15
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    15
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           15
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      15
      REAL RE_SVTG_ZSIZEMAX                                               15
C -                                                                       15
      REAL SVTG_ANGOFF                                                    15
      EQUIVALENCE (BANK_SVTG(8),SVTG_ANGOFF,RE_SVTG_ANGOFF),              15
     *(NAMES_SVTG(6),NSVTG_ANGOFF)                                        15
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ANGOFF),(MAP_SVTG(1,6),           15
     *MSVTG_ANGOFF)                                                       15
      CHARACTER CSVTG_ANGOFF*40/' '/                                      15
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               15
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        15
      REAL RE_SVTG_ANGOFF                                                 15
C -                                                                       15
      REAL SVTG_SUPPORTVER                                                15
      EQUIVALENCE (BANK_SVTG(9),SVTG_SUPPORTVER,RE_SVTG_SUPPORTVER),      15
     *(NAMES_SVTG(7),NSVTG_SUPPORTVER)                                    15
      EQUIVALENCE (COMMEN_SVTG(7),CSVTG_SUPPORTVER),(MAP_SVTG(1,7),       15
     *MSVTG_SUPPORTVER)                                                   15
      CHARACTER CSVTG_SUPPORTVER*40/' '/                                  15
      CHARACTER NSVTG_SUPPORTVER*12/'R.SUPPORTVER'/                       15
      INTEGER MSVTG_SUPPORTVER(2)/1,1/                                    15
      REAL RE_SVTG_SUPPORTVER                                             15
C -                                                                       15
      REAL SVTG_CONEVER                                                   15
      EQUIVALENCE (BANK_SVTG(10),SVTG_CONEVER,RE_SVTG_CONEVER),           15
     *(NAMES_SVTG(8),NSVTG_CONEVER)                                       15
      EQUIVALENCE (COMMEN_SVTG(8),CSVTG_CONEVER),(MAP_SVTG(1,8),          15
     *MSVTG_CONEVER)                                                      15
      CHARACTER CSVTG_CONEVER*40/' '/                                     15
      CHARACTER NSVTG_CONEVER*12/'R.CONEVER'/                             15
      INTEGER MSVTG_CONEVER(2)/1,1/                                       15
      REAL RE_SVTG_CONEVER                                                15
C -                                                                       15
      REAL SVTG_IFMANY                                                    15
      EQUIVALENCE (BANK_SVTG(11),SVTG_IFMANY,RE_SVTG_IFMANY),             15
     *(NAMES_SVTG(9),NSVTG_IFMANY)                                        15
      EQUIVALENCE (COMMEN_SVTG(9),CSVTG_IFMANY),(MAP_SVTG(1,9),           15
     *MSVTG_IFMANY)                                                       15
      CHARACTER CSVTG_IFMANY*40/' '/                                      15
      CHARACTER NSVTG_IFMANY*12/'R.IFMANY'/                               15
      INTEGER MSVTG_IFMANY(2)/1,1/                                        15
      REAL RE_SVTG_IFMANY                                                 15
C -                                                                       15
      REAL SVTG_NMIN                                                      15
      EQUIVALENCE (BANK_SVTG(12),SVTG_NMIN,RE_SVTG_NMIN),                 15
     *(NAMES_SVTG(10),NSVTG_NMIN)                                         15
      EQUIVALENCE (COMMEN_SVTG(10),CSVTG_NMIN),(MAP_SVTG(1,10),           15
     *MSVTG_NMIN)                                                         15
      CHARACTER CSVTG_NMIN*40/' '/                                        15
      CHARACTER NSVTG_NMIN*12/'R.NMIN'/                                   15
      INTEGER MSVTG_NMIN(2)/1,1/                                          15
      REAL RE_SVTG_NMIN                                                   15
C -                                                                       15
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                15
      PARAMETER (LEN_SVTG=12)                                             15
      PARAMETER (LLEN_SVTG=10)                                            15
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           15
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        15
      DATA IDNUM_SVTG/10*0/                                               15
      REAL BANK_SVTG(LEN_SVTG)                                            15
      CHARACTER*60 BPATH_SVTG/' '/                                        15
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          15
     *BPATH_SVTG                                                          15
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             15
C *                                                                       15
C -                                                                       17
      REAL SWCA_VERSION                                                   17
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            17
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       17
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          17
     *MSWCA_VERSION)                                                      17
      CHARACTER CSWCA_VERSION*40/' '/                                     17
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             17
      INTEGER MSWCA_VERSION(2)/1,1/                                       17
      REAL RE_SWCA_VERSION                                                17
C -                                                                       17
      REAL SWCA_LENGTH                                                    17
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              17
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        17
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           17
     *MSWCA_LENGTH)                                                       17
      CHARACTER CSWCA_LENGTH*40/' '/                                      17
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               17
      INTEGER MSWCA_LENGTH(2)/1,1/                                        17
      REAL RE_SWCA_LENGTH                                                 17
C -                                                                       17
      REAL SWCA_WAFERWID                                                  17
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          17
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      17
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         17
     *MSWCA_WAFERWID)                                                     17
      CHARACTER CSWCA_WAFERWID*40/' '/                                    17
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           17
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      17
      REAL RE_SWCA_WAFERWID                                               17
C -                                                                       17
      REAL SWCA_WAFERLEN                                                  17
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          17
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      17
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         17
     *MSWCA_WAFERLEN)                                                     17
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    17
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           17
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      17
      REAL RE_SWCA_WAFERLEN                                               17
C -                                                                       17
      REAL SWCA_WAFERTHK                                                  17
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          17
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      17
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         17
     *MSWCA_WAFERTHK)                                                     17
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    17
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           17
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      17
      REAL RE_SWCA_WAFERTHK                                               17
C -                                                                       17
      REAL SWCA_ROHATHK                                                   17
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            17
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       17
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          17
     *MSWCA_ROHATHK)                                                      17
      CHARACTER CSWCA_ROHATHK*40/' '/                                     17
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             17
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       17
      REAL RE_SWCA_ROHATHK                                                17
C -                                                                       17
      REAL SWCA_WAFCARWD                                                  17
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          17
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      17
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         17
     *MSWCA_WAFCARWD)                                                     17
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    17
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           17
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      17
      REAL RE_SWCA_WAFCARWD                                               17
C -                                                                       17
      REAL SWCA_WAFCARTH                                                  17
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         17
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      17
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         17
     *MSWCA_WAFCARTH)                                                     17
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    17
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           17
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      17
      REAL RE_SWCA_WAFCARTH                                               17
C -                                                                       17
      REAL SWCA_WAFERGAP                                                  17
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         17
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      17
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         17
     *MSWCA_WAFERGAP)                                                     17
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    17
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           17
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      17
      REAL RE_SWCA_WAFERGAP                                               17
C -                                                                       17
      REAL SWCA_DRIFT                                                     17
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               17
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        17
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          17
     *MSWCA_DRIFT)                                                        17
      CHARACTER CSWCA_DRIFT*40/' '/                                       17
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 17
      INTEGER MSWCA_DRIFT(2)/1,1/                                         17
      REAL RE_SWCA_DRIFT                                                  17
C -                                                                       17
      REAL SWCA_STRUTLEN                                                  17
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         17
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     17
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       17
     *MSWCA_STRUTLEN)                                                     17
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    17
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           17
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      17
      REAL RE_SWCA_STRUTLEN                                               17
C -                                                                       17
      REAL SWCA_SENSWID                                                   17
      EQUIVALENCE (BANK_SWCA(14),SWCA_SENSWID,RE_SWCA_SENSWID),           17
     *(NAMES_SWCA(12),NSWCA_SENSWID)                                      17
      EQUIVALENCE (COMMEN_SWCA(12),CSWCA_SENSWID),(MAP_SWCA(1,12),        17
     *MSWCA_SENSWID)                                                      17
      CHARACTER CSWCA_SENSWID*40/' '/                                     17
      CHARACTER NSWCA_SENSWID*12/'R.SENSWID'/                             17
      INTEGER MSWCA_SENSWID(2)/1,1/                                       17
      REAL RE_SWCA_SENSWID                                                17
C -                                                                       17
      REAL SWCA_SENSLEN                                                   17
      EQUIVALENCE (BANK_SWCA(15),SWCA_SENSLEN,RE_SWCA_SENSLEN),           17
     *(NAMES_SWCA(13),NSWCA_SENSLEN)                                      17
      EQUIVALENCE (COMMEN_SWCA(13),CSWCA_SENSLEN),(MAP_SWCA(1,13),        17
     *MSWCA_SENSLEN)                                                      17
      CHARACTER CSWCA_SENSLEN*40/' '/                                     17
      CHARACTER NSWCA_SENSLEN*12/'R.SENSLEN'/                             17
      INTEGER MSWCA_SENSLEN(2)/1,1/                                       17
      REAL RE_SWCA_SENSLEN                                                17
C -                                                                       17
      REAL SWCA_SENSGIR                                                   17
      EQUIVALENCE (BANK_SWCA(16),SWCA_SENSGIR,RE_SWCA_SENSGIR),           17
     *(NAMES_SWCA(14),NSWCA_SENSGIR)                                      17
      EQUIVALENCE (COMMEN_SWCA(14),CSWCA_SENSGIR),(MAP_SWCA(1,14),        17
     *MSWCA_SENSGIR)                                                      17
      CHARACTER CSWCA_SENSGIR*40/' '/                                     17
      CHARACTER NSWCA_SENSGIR*12/'R.SENSGIR'/                             17
      INTEGER MSWCA_SENSGIR(2)/1,1/                                       17
      REAL RE_SWCA_SENSGIR                                                17
C -                                                                       17
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                17
      PARAMETER (LEN_SWCA=16)                                             17
      PARAMETER (LLEN_SWCA=14)                                            17
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           17
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        17
      DATA IDNUM_SWCA/10*0/                                               17
      REAL BANK_SWCA(LEN_SWCA)                                            17
      CHARACTER*60 BPATH_SWCA/' '/                                        17
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          17
     *BPATH_SWCA                                                          17
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             17
C *                                                                       17
C -                                                                       20
      REAL SSUP_VERSION                                                   20
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            20
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       20
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          20
     *MSSUP_VERSION)                                                      20
      CHARACTER CSSUP_VERSION*40/' '/                                     20
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             20
      INTEGER MSSUP_VERSION(2)/1,1/                                       20
      REAL RE_SSUP_VERSION                                                20
C -                                                                       20
      REAL SSUP_CABTHK                                                    20
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              20
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        20
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           20
     *MSSUP_CABTHK)                                                       20
      CHARACTER CSSUP_CABTHK*40/' '/                                      20
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               20
      INTEGER MSSUP_CABTHK(2)/1,1/                                        20
      REAL RE_SSUP_CABTHK                                                 20
C -                                                                       20
      REAL SSUP_HOSRMN                                                    20
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              20
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        20
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           20
     *MSSUP_HOSRMN)                                                       20
      CHARACTER CSSUP_HOSRMN*40/' '/                                      20
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               20
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        20
      REAL RE_SSUP_HOSRMN                                                 20
C -                                                                       20
      REAL SSUP_HOSRMX                                                    20
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              20
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        20
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           20
     *MSSUP_HOSRMX)                                                       20
      CHARACTER CSSUP_HOSRMX*40/' '/                                      20
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               20
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        20
      REAL RE_SSUP_HOSRMX                                                 20
C -                                                                       20
      REAL SSUP_NHOSES                                                    20
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              20
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        20
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           20
     *MSSUP_NHOSES)                                                       20
      CHARACTER CSSUP_NHOSES*40/' '/                                      20
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               20
      INTEGER MSSUP_NHOSES(2)/1,1/                                        20
      REAL RE_SSUP_NHOSES                                                 20
C -                                                                       20
      REAL SSUP_WRPMYTHK                                                  20
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          20
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      20
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         20
     *MSSUP_WRPMYTHK)                                                     20
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    20
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           20
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      20
      REAL RE_SSUP_WRPMYTHK                                               20
C -                                                                       20
      REAL SSUP_WRPALTHK                                                  20
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          20
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      20
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         20
     *MSSUP_WRPALTHK)                                                     20
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    20
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           20
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      20
      REAL RE_SSUP_WRPALTHK                                               20
C -                                                                       20
      REAL SSUP_GRPHTHK                                                   20
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           20
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       20
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          20
     *MSSUP_GRPHTHK)                                                      20
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     20
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             20
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       20
      REAL RE_SSUP_GRPHTHK                                                20
C -                                                                       20
      REAL SSUP_CONE1ZMN                                                  20
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         20
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      20
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         20
     *MSSUP_CONE1ZMN)                                                     20
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    20
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           20
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      20
      REAL RE_SSUP_CONE1ZMN                                               20
C -                                                                       20
      REAL SSUP_RODLEN                                                    20
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             20
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       20
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         20
     *MSSUP_RODLEN)                                                       20
      CHARACTER CSSUP_RODLEN*40/' '/                                      20
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               20
      INTEGER MSSUP_RODLEN(2)/1,1/                                        20
      REAL RE_SSUP_RODLEN                                                 20
C -                                                                       20
      REAL SSUP_RODDIST                                                   20
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           20
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      20
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        20
     *MSSUP_RODDIST)                                                      20
      CHARACTER CSSUP_RODDIST*40/' '/                                     20
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             20
      INTEGER MSSUP_RODDIST(2)/1,1/                                       20
      REAL RE_SSUP_RODDIST                                                20
C -                                                                       20
      REAL SSUP_RODID                                                     20
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               20
     *(NAMES_SSUP(12),NSSUP_RODID)                                        20
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          20
     *MSSUP_RODID)                                                        20
      CHARACTER CSSUP_RODID*40/' '/                                       20
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 20
      INTEGER MSSUP_RODID(2)/1,1/                                         20
      REAL RE_SSUP_RODID                                                  20
C -                                                                       20
      REAL SSUP_RODOD                                                     20
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               20
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        20
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          20
     *MSSUP_RODOD)                                                        20
      CHARACTER CSSUP_RODOD*40/' '/                                       20
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 20
      INTEGER MSSUP_RODOD(2)/1,1/                                         20
      REAL RE_SSUP_RODOD                                                  20
C -                                                                       20
      REAL SSUP_CON1IDMN                                                  20
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         20
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     20
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       20
     *MSSUP_CON1IDMN)                                                     20
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    20
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           20
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      20
      REAL RE_SSUP_CON1IDMN                                               20
C -                                                                       20
      REAL SSUP_CON3IDMN                                                  20
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         20
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     20
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       20
     *MSSUP_CON3IDMN)                                                     20
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    20
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           20
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      20
      REAL RE_SSUP_CON3IDMN                                               20
C -                                                                       20
      REAL SSUP_CON4IDMN                                                  20
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         20
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     20
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       20
     *MSSUP_CON4IDMN)                                                     20
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    20
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           20
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      20
      REAL RE_SSUP_CON4IDMN                                               20
C -                                                                       20
      REAL SSUP_CON4IDMX                                                  20
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         20
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     20
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       20
     *MSSUP_CON4IDMX)                                                     20
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    20
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           20
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      20
      REAL RE_SSUP_CON4IDMX                                               20
C -                                                                       20
      REAL SSUP_CONE3ZMX                                                  20
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         20
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     20
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       20
     *MSSUP_CONE3ZMX)                                                     20
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    20
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           20
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      20
      REAL RE_SSUP_CONE3ZMX                                               20
C -                                                                       20
      REAL SSUP_CONE4ZMX                                                  20
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         20
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     20
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       20
     *MSSUP_CONE4ZMX)                                                     20
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    20
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           20
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      20
      REAL RE_SSUP_CONE4ZMX                                               20
C -                                                                       20
      REAL SSUP_BRATHK                                                    20
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             20
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       20
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         20
     *MSSUP_BRATHK)                                                       20
      CHARACTER CSSUP_BRATHK*40/' '/                                      20
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               20
      INTEGER MSSUP_BRATHK(2)/1,1/                                        20
      REAL RE_SSUP_BRATHK                                                 20
C -                                                                       20
      REAL SSUP_ERJTHK                                                    20
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             20
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       20
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         20
     *MSSUP_ERJTHK)                                                       20
      CHARACTER CSSUP_ERJTHK*40/' '/                                      20
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               20
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        20
      REAL RE_SSUP_ERJTHK                                                 20
C -                                                                       20
      REAL SSUP_ERJWID                                                    20
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             20
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       20
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         20
     *MSSUP_ERJWID)                                                       20
      CHARACTER CSSUP_ERJWID*40/' '/                                      20
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               20
      INTEGER MSSUP_ERJWID(2)/1,1/                                        20
      REAL RE_SSUP_ERJWID                                                 20
C -                                                                       20
      REAL SSUP_ERJLEN                                                    20
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             20
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       20
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         20
     *MSSUP_ERJLEN)                                                       20
      CHARACTER CSSUP_ERJLEN*40/' '/                                      20
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               20
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        20
      REAL RE_SSUP_ERJLEN                                                 20
C -                                                                       20
      REAL SSUP_ERJZDIS                                                   20
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           20
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      20
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        20
     *MSSUP_ERJZDIS)                                                      20
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     20
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             20
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       20
      REAL RE_SSUP_ERJZDIS                                                20
C -                                                                       20
      REAL SSUP_ERJ1X                                                     20
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               20
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        20
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          20
     *MSSUP_ERJ1X)                                                        20
      CHARACTER CSSUP_ERJ1X*40/' '/                                       20
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 20
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         20
      REAL RE_SSUP_ERJ1X                                                  20
C -                                                                       20
      REAL SSUP_ERJ2X                                                     20
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               20
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        20
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          20
     *MSSUP_ERJ2X)                                                        20
      CHARACTER CSSUP_ERJ2X*40/' '/                                       20
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 20
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         20
      REAL RE_SSUP_ERJ2X                                                  20
C -                                                                       20
      REAL SSUP_ERJ2Y                                                     20
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               20
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        20
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          20
     *MSSUP_ERJ2Y)                                                        20
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       20
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 20
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         20
      REAL RE_SSUP_ERJ2Y                                                  20
C -                                                                       20
      REAL SSUP_ERJRAD                                                    20
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             20
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       20
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         20
     *MSSUP_ERJRAD)                                                       20
      CHARACTER CSSUP_ERJRAD*40/' '/                                      20
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               20
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        20
      REAL RE_SSUP_ERJRAD                                                 20
C -                                                                       20
      REAL SSUP_ERJDIA                                                    20
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             20
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       20
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         20
     *MSSUP_ERJDIA)                                                       20
      CHARACTER CSSUP_ERJDIA*40/' '/                                      20
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               20
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        20
      REAL RE_SSUP_ERJDIA                                                 20
C -                                                                       20
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                20
      PARAMETER (LEN_SSUP=31)                                             20
      PARAMETER (LLEN_SSUP=29)                                            20
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           20
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        20
      DATA IDNUM_SSUP/10*0/                                               20
      REAL BANK_SSUP(LEN_SSUP)                                            20
      CHARACTER*60 BPATH_SSUP/' '/                                        20
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          20
     *BPATH_SSUP                                                          20
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             20
C *                                                                       20
C -                                                                       24
      REAL SSUB_VERSION                                                   24
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            24
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       24
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          24
     *MSSUB_VERSION)                                                      24
      CHARACTER CSSUB_VERSION*40/' '/                                     24
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             24
      INTEGER MSSUB_VERSION(2)/1,1/                                       24
      REAL RE_SSUB_VERSION                                                24
C -                                                                       24
      REAL SSUB_KMOUNTID                                                  24
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          24
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      24
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         24
     *MSSUB_KMOUNTID)                                                     24
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    24
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           24
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      24
      REAL RE_SSUB_KMOUNTID                                               24
C -                                                                       24
      REAL SSUB_KMOUNTOD                                                  24
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          24
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      24
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         24
     *MSSUB_KMOUNTOD)                                                     24
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    24
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           24
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      24
      REAL RE_SSUB_KMOUNTOD                                               24
C -                                                                       24
      REAL SSUB_KMNTTHK                                                   24
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            24
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       24
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          24
     *MSSUB_KMNTTHK)                                                      24
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     24
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             24
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       24
      REAL RE_SSUB_KMNTTHK                                                24
C -                                                                       24
      REAL SSUB_KMCUTOD                                                   24
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            24
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       24
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          24
     *MSSUB_KMCUTOD)                                                      24
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     24
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             24
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       24
      REAL RE_SSUB_KMCUTOD                                                24
C -                                                                       24
      REAL SSUB_KMCUTID                                                   24
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            24
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       24
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          24
     *MSSUB_KMCUTID)                                                      24
      CHARACTER CSSUB_KMCUTID*40/' '/                                     24
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             24
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       24
      REAL RE_SSUB_KMCUTID                                                24
C -                                                                       24
      REAL SSUB_KMCUTOA                                                   24
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            24
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       24
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          24
     *MSSUB_KMCUTOA)                                                      24
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     24
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             24
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       24
      REAL RE_SSUB_KMCUTOA                                                24
C -                                                                       24
      REAL SSUB_KMCUTOFF                                                  24
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         24
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      24
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         24
     *MSSUB_KMCUTOFF)                                                     24
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    24
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           24
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      24
      REAL RE_SSUB_KMCUTOFF                                               24
C -                                                                       24
      REAL SSUB_SRINGID                                                   24
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           24
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       24
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          24
     *MSSUB_SRINGID)                                                      24
      CHARACTER CSSUB_SRINGID*40/' '/                                     24
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             24
      INTEGER MSSUB_SRINGID(2)/1,1/                                       24
      REAL RE_SSUB_SRINGID                                                24
C -                                                                       24
      REAL SSUB_SRINGOD                                                   24
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           24
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      24
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        24
     *MSSUB_SRINGOD)                                                      24
      CHARACTER CSSUB_SRINGOD*40/' '/                                     24
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             24
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       24
      REAL RE_SSUB_SRINGOD                                                24
C -                                                                       24
      REAL SSUB_SRINGTHK                                                  24
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         24
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     24
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       24
     *MSSUB_SRINGTHK)                                                     24
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    24
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           24
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      24
      REAL RE_SSUB_SRINGTHK                                               24
C -                                                                       24
      REAL SSUB_SRCUTPHI                                                  24
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         24
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     24
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       24
     *MSSUB_SRCUTPHI)                                                     24
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    24
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           24
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      24
      REAL RE_SSUB_SRCUTPHI                                               24
C -                                                                       24
      REAL SSUB_SRCUTWID                                                  24
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         24
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     24
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       24
     *MSSUB_SRCUTWID)                                                     24
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    24
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           24
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      24
      REAL RE_SSUB_SRCUTWID                                               24
C -                                                                       24
      REAL SSUB_SRCUTOUT                                                  24
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         24
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     24
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       24
     *MSSUB_SRCUTOUT)                                                     24
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    24
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           24
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      24
      REAL RE_SSUB_SRCUTOUT                                               24
C -                                                                       24
      REAL SSUB_SRCUTIN                                                   24
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           24
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      24
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        24
     *MSSUB_SRCUTIN)                                                      24
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     24
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             24
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       24
      REAL RE_SSUB_SRCUTIN                                                24
C -                                                                       24
      REAL SSUB_SROLLID                                                   24
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           24
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      24
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        24
     *MSSUB_SROLLID)                                                      24
      CHARACTER CSSUB_SROLLID*40/' '/                                     24
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             24
      INTEGER MSSUB_SROLLID(2)/1,1/                                       24
      REAL RE_SSUB_SROLLID                                                24
C -                                                                       24
      REAL SSUB_SROLLOD                                                   24
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           24
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      24
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        24
     *MSSUB_SROLLOD)                                                      24
      CHARACTER CSSUB_SROLLOD*40/' '/                                     24
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             24
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       24
      REAL RE_SSUB_SROLLOD                                                24
C -                                                                       24
      REAL SSUB_SROLLLEN                                                  24
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         24
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     24
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       24
     *MSSUB_SROLLLEN)                                                     24
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    24
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           24
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      24
      REAL RE_SSUB_SROLLLEN                                               24
C -                                                                       24
      REAL SSUB_SWIRELEN                                                  24
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         24
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     24
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       24
     *MSSUB_SWIRELEN)                                                     24
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    24
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           24
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      24
      REAL RE_SSUB_SWIRELEN                                               24
C -                                                                       24
      REAL SSUB_MBLKHGH                                                   24
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           24
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      24
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        24
     *MSSUB_MBLKHGH)                                                      24
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     24
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             24
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       24
      REAL RE_SSUB_MBLKHGH                                                24
C -                                                                       24
      REAL SSUB_MBLKOWID                                                  24
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         24
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     24
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       24
     *MSSUB_MBLKOWID)                                                     24
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    24
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           24
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      24
      REAL RE_SSUB_MBLKOWID                                               24
C -                                                                       24
      REAL SSUB_MBLKOLEN                                                  24
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         24
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     24
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       24
     *MSSUB_MBLKOLEN)                                                     24
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    24
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           24
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      24
      REAL RE_SSUB_MBLKOLEN                                               24
C -                                                                       24
      REAL SSUB_MBLKIWID                                                  24
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         24
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     24
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       24
     *MSSUB_MBLKIWID)                                                     24
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    24
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           24
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      24
      REAL RE_SSUB_MBLKIWID                                               24
C -                                                                       24
      REAL SSUB_MBLKILEN                                                  24
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         24
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     24
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       24
     *MSSUB_MBLKILEN)                                                     24
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    24
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           24
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      24
      REAL RE_SSUB_MBLKILEN                                               24
C -                                                                       24
      REAL SSUB_MBLKORAD                                                  24
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         24
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     24
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       24
     *MSSUB_MBLKORAD)                                                     24
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    24
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           24
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      24
      REAL RE_SSUB_MBLKORAD                                               24
C -                                                                       24
      REAL SSUB_MBLKIRAD                                                  24
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         24
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     24
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       24
     *MSSUB_MBLKIRAD)                                                     24
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    24
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           24
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      24
      REAL RE_SSUB_MBLKIRAD                                               24
C -                                                                       24
      REAL SSUB_MRODDIA                                                   24
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           24
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      24
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        24
     *MSSUB_MRODDIA)                                                      24
      CHARACTER CSSUB_MRODDIA*40/' '/                                     24
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             24
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       24
      REAL RE_SSUB_MRODDIA                                                24
C -                                                                       24
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                24
      PARAMETER (LEN_SSUB=29)                                             24
      PARAMETER (LLEN_SSUB=27)                                            24
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           24
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        24
      DATA IDNUM_SSUB/10*0/                                               24
      REAL BANK_SSUB(LEN_SSUB)                                            24
      CHARACTER*60 BPATH_SSUB/' '/                                        24
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          24
     *BPATH_SSUB                                                          24
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             24
C *                                                                       24
C -                                                                       25
      REAL SWAM_VERSION                                                   25
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            25
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       25
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          25
     *MSWAM_VERSION)                                                      25
      CHARACTER CSWAM_VERSION*40/' '/                                     25
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             25
      INTEGER MSWAM_VERSION(2)/1,1/                                       25
      REAL RE_SWAM_VERSION                                                25
C -                                                                       25
      REAL SWAM_ZMIN                                                      25
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   25
     *NSWAM_ZMIN)                                                         25
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             25
     *MSWAM_ZMIN)                                                         25
      CHARACTER CSWAM_ZMIN*40/' '/                                        25
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   25
      INTEGER MSWAM_ZMIN(2)/1,1/                                          25
      REAL RE_SWAM_ZMIN                                                   25
C -                                                                       25
      REAL SWAM_LEN                                                       25
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     25
     *NSWAM_LEN)                                                          25
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    25
      CHARACTER CSWAM_LEN*40/' '/                                         25
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     25
      INTEGER MSWAM_LEN(2)/1,1/                                           25
      REAL RE_SWAM_LEN                                                    25
C -                                                                       25
      REAL SWAM_RMIN                                                      25
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   25
     *NSWAM_RMIN)                                                         25
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             25
     *MSWAM_RMIN)                                                         25
      CHARACTER CSWAM_RMIN*40/' '/                                        25
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   25
      INTEGER MSWAM_RMIN(2)/1,1/                                          25
      REAL RE_SWAM_RMIN                                                   25
C -                                                                       25
      REAL SWAM_RMAX                                                      25
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   25
     *NSWAM_RMAX)                                                         25
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             25
     *MSWAM_RMAX)                                                         25
      CHARACTER CSWAM_RMAX*40/' '/                                        25
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   25
      INTEGER MSWAM_RMAX(2)/1,1/                                          25
      REAL RE_SWAM_RMAX                                                   25
C -                                                                       25
      REAL SWAM_TBRDTHK                                                   25
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            25
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       25
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          25
     *MSWAM_TBRDTHK)                                                      25
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     25
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             25
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       25
      REAL RE_SWAM_TBRDTHK                                                25
C -                                                                       25
      REAL SWAM_WALLTHK                                                   25
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            25
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       25
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          25
     *MSWAM_WALLTHK)                                                      25
      CHARACTER CSWAM_WALLTHK*40/' '/                                     25
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             25
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       25
      REAL RE_SWAM_WALLTHK                                                25
C -                                                                       25
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                25
      PARAMETER (LEN_SWAM=9)                                              25
      PARAMETER (LLEN_SWAM=7)                                             25
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           25
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        25
      DATA IDNUM_SWAM/10*0/                                               25
      REAL BANK_SWAM(LEN_SWAM)                                            25
      CHARACTER*60 BPATH_SWAM/' '/                                        25
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          25
     *BPATH_SWAM                                                          25
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             25
C *                                                                       25
C -                                                                       26
      REAL SERG_VERSION                                                   26
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            26
     *(NAMES_SERG(1),NSERG_VERSION)                                       26
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          26
     *MSERG_VERSION)                                                      26
      CHARACTER CSERG_VERSION*40/' '/                                     26
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             26
      INTEGER MSERG_VERSION(2)/1,1/                                       26
      REAL RE_SERG_VERSION                                                26
C -                                                                       26
      REAL SERG_IRNGTRMX                                                  26
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          26
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      26
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         26
     *MSERG_IRNGTRMX)                                                     26
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    26
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           26
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      26
      REAL RE_SERG_IRNGTRMX                                               26
C -                                                                       26
      REAL SERG_IRNGPRMN                                                  26
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          26
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      26
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         26
     *MSERG_IRNGPRMN)                                                     26
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    26
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           26
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      26
      REAL RE_SERG_IRNGPRMN                                               26
C -                                                                       26
      REAL SERG_ORNGRMIN                                                  26
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          26
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      26
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         26
     *MSERG_ORNGRMIN)                                                     26
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    26
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           26
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      26
      REAL RE_SERG_ORNGRMIN                                               26
C -                                                                       26
      REAL SERG_ORNGRMAX                                                  26
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          26
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      26
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         26
     *MSERG_ORNGRMAX)                                                     26
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    26
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           26
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      26
      REAL RE_SERG_ORNGRMAX                                               26
C -                                                                       26
      REAL SERG_ENDRNGTH                                                  26
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          26
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      26
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         26
     *MSERG_ENDRNGTH)                                                     26
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    26
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           26
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      26
      REAL RE_SERG_ENDRNGTH                                               26
C -                                                                       26
      REAL SERG_ENDRNGZM                                                  26
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          26
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      26
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         26
     *MSERG_ENDRNGZM)                                                     26
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    26
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           26
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      26
      REAL RE_SERG_ENDRNGZM                                               26
C -                                                                       26
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                26
      PARAMETER (LEN_SERG=9)                                              26
      PARAMETER (LLEN_SERG=7)                                             26
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           26
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        26
      DATA IDNUM_SERG/10*0/                                               26
      REAL BANK_SERG(LEN_SERG)                                            26
      CHARACTER*60 BPATH_SERG/' '/                                        26
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          26
     *BPATH_SERG                                                          26
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             26
C *                                                                       26
C -                                                                       28
      REAL SELC_VERSION                                                   28
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            28
     *(NAMES_SELC(1),NSELC_VERSION)                                       28
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          28
     *MSELC_VERSION)                                                      28
      CHARACTER CSELC_VERSION*40/' '/                                     28
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             28
      INTEGER MSELC_VERSION(2)/1,1/                                       28
      REAL RE_SELC_VERSION                                                28
C -                                                                       28
      REAL SELC_BETHK                                                     28
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                28
     *(NAMES_SELC(2),NSELC_BETHK)                                         28
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            28
     *MSELC_BETHK)                                                        28
      CHARACTER CSELC_BETHK*40/' '/                                       28
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 28
      INTEGER MSELC_BETHK(2)/1,1/                                         28
      REAL RE_SELC_BETHK                                                  28
C -                                                                       28
      REAL SELC_WATTHK                                                    28
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              28
     *(NAMES_SELC(3),NSELC_WATTHK)                                        28
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           28
     *MSELC_WATTHK)                                                       28
      CHARACTER CSELC_WATTHK*40/' '/                                      28
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               28
      INTEGER MSELC_WATTHK(2)/1,1/                                        28
      REAL RE_SELC_WATTHK                                                 28
C -                                                                       28
      REAL SELC_BEOTHK                                                    28
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              28
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        28
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           28
     *MSELC_BEOTHK)                                                       28
      CHARACTER CSELC_BEOTHK*40/' '/                                      28
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               28
      INTEGER MSELC_BEOTHK(2)/1,1/                                        28
      REAL RE_SELC_BEOTHK                                                 28
C -                                                                       28
      REAL SELC_DYETHK                                                    28
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              28
     *(NAMES_SELC(5),NSELC_DYETHK)                                        28
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           28
     *MSELC_DYETHK)                                                       28
      CHARACTER CSELC_DYETHK*40/' '/                                      28
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               28
      INTEGER MSELC_DYETHK(2)/1,1/                                        28
      REAL RE_SELC_DYETHK                                                 28
C -                                                                       28
      REAL SELC_DYEWID                                                    28
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              28
     *(NAMES_SELC(6),NSELC_DYEWID)                                        28
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           28
     *MSELC_DYEWID)                                                       28
      CHARACTER CSELC_DYEWID*40/' '/                                      28
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               28
      INTEGER MSELC_DYEWID(2)/1,1/                                        28
      REAL RE_SELC_DYEWID                                                 28
C -                                                                       28
      REAL SELC_DYESPC                                                    28
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              28
     *(NAMES_SELC(7),NSELC_DYESPC)                                        28
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           28
     *MSELC_DYESPC)                                                       28
      CHARACTER CSELC_DYESPC*40/' '/                                      28
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               28
      INTEGER MSELC_DYESPC(2)/1,1/                                        28
      REAL RE_SELC_DYESPC                                                 28
C -                                                                       28
      REAL SELC_ELCAWID                                                   28
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           28
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       28
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          28
     *MSELC_ELCAWID)                                                      28
      CHARACTER CSELC_ELCAWID*40/' '/                                     28
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             28
      INTEGER MSELC_ELCAWID(2)/1,1/                                       28
      REAL RE_SELC_ELCAWID                                                28
C -                                                                       28
      REAL SELC_AGPDTHK                                                   28
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           28
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       28
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          28
     *MSELC_AGPDTHK)                                                      28
      CHARACTER CSELC_AGPDTHK*40/' '/                                     28
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             28
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       28
      REAL RE_SELC_AGPDTHK                                                28
C -                                                                       28
      REAL SELC_GLASSTHK                                                  28
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         28
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     28
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       28
     *MSELC_GLASSTHK)                                                     28
      CHARACTER CSELC_GLASSTHK*40/' '/                                    28
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           28
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      28
      REAL RE_SELC_GLASSTHK                                               28
C -                                                                       28
      REAL SELC_CABTHK                                                    28
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             28
     *(NAMES_SELC(11),NSELC_CABTHK)                                       28
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         28
     *MSELC_CABTHK)                                                       28
      CHARACTER CSELC_CABTHK*40/' '/                                      28
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               28
      INTEGER MSELC_CABTHK(2)/1,1/                                        28
      REAL RE_SELC_CABTHK                                                 28
C -                                                                       28
      REAL SELC_CABWID                                                    28
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             28
     *(NAMES_SELC(12),NSELC_CABWID)                                       28
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         28
     *MSELC_CABWID)                                                       28
      CHARACTER CSELC_CABWID*40/' '/                                      28
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               28
      INTEGER MSELC_CABWID(2)/1,1/                                        28
      REAL RE_SELC_CABWID                                                 28
C -                                                                       28
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                28
      PARAMETER (LEN_SELC=14)                                             28
      PARAMETER (LLEN_SELC=12)                                            28
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           28
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        28
      DATA IDNUM_SELC/10*0/                                               28
      REAL BANK_SELC(LEN_SELC)                                            28
      CHARACTER*60 BPATH_SELC/' '/                                        28
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          28
     *BPATH_SELC                                                          28
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             28
C *                                                                       28
C -                                                                       30
      INTEGER SVTL_LAYER                                                  30
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                30
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         30
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            30
     *MSVTL_LAYER)                                                        30
      CHARACTER CSVTL_LAYER*40/' '/                                       30
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 30
      INTEGER MSVTL_LAYER(2)/1,1/                                         30
      REAL RE_SVTL_LAYER                                                  30
C -                                                                       30
      REAL SVTL_NLADDER                                                   30
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            30
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       30
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          30
     *MSVTL_NLADDER)                                                      30
      CHARACTER CSVTL_NLADDER*40/' '/                                     30
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             30
      INTEGER MSVTL_NLADDER(2)/1,1/                                       30
      REAL RE_SVTL_NLADDER                                                30
C -                                                                       30
      REAL SVTL_NWAFER                                                    30
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              30
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        30
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           30
     *MSVTL_NWAFER)                                                       30
      CHARACTER CSVTL_NWAFER*40/' '/                                      30
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               30
      INTEGER MSVTL_NWAFER(2)/1,1/                                        30
      REAL RE_SVTL_NWAFER                                                 30
C -                                                                       30
      REAL SVTL_RADIUS                                                    30
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              30
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        30
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           30
     *MSVTL_RADIUS)                                                       30
      CHARACTER CSVTL_RADIUS*40/' '/                                      30
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               30
      INTEGER MSVTL_RADIUS(2)/1,1/                                        30
      REAL RE_SVTL_RADIUS                                                 30
C -                                                                       30
      REAL SVTL_BAREEDGE                                                  30
      EQUIVALENCE (BANK_SVTL(7),SVTL_BAREEDGE,RE_SVTL_BAREEDGE),          30
     *(NAMES_SVTL(5),NSVTL_BAREEDGE)                                      30
      EQUIVALENCE (COMMEN_SVTL(5),CSVTL_BAREEDGE),(MAP_SVTL(1,5),         30
     *MSVTL_BAREEDGE)                                                     30
      CHARACTER CSVTL_BAREEDGE*40/' '/                                    30
      CHARACTER NSVTL_BAREEDGE*12/'R.BAREEDGE'/                           30
      INTEGER MSVTL_BAREEDGE(2)/1,1/                                      30
      REAL RE_SVTL_BAREEDGE                                               30
C -                                                                       30
      REAL SVTL_PCBLEN                                                    30
      EQUIVALENCE (BANK_SVTL(8),SVTL_PCBLEN,RE_SVTL_PCBLEN),              30
     *(NAMES_SVTL(6),NSVTL_PCBLEN)                                        30
      EQUIVALENCE (COMMEN_SVTL(6),CSVTL_PCBLEN),(MAP_SVTL(1,6),           30
     *MSVTL_PCBLEN)                                                       30
      CHARACTER CSVTL_PCBLEN*40/' '/                                      30
      CHARACTER NSVTL_PCBLEN*12/'R.PCBLEN'/                               30
      INTEGER MSVTL_PCBLEN(2)/1,1/                                        30
      REAL RE_SVTL_PCBLEN                                                 30
C -                                                                       30
      REAL SVTL_PCBWIDTH                                                  30
      EQUIVALENCE (BANK_SVTL(9),SVTL_PCBWIDTH,RE_SVTL_PCBWIDTH),          30
     *(NAMES_SVTL(7),NSVTL_PCBWIDTH)                                      30
      EQUIVALENCE (COMMEN_SVTL(7),CSVTL_PCBWIDTH),(MAP_SVTL(1,7),         30
     *MSVTL_PCBWIDTH)                                                     30
      CHARACTER CSVTL_PCBWIDTH*40/' '/                                    30
      CHARACTER NSVTL_PCBWIDTH*12/'R.PCBWIDTH'/                           30
      INTEGER MSVTL_PCBWIDTH(2)/1,1/                                      30
      REAL RE_SVTL_PCBWIDTH                                               30
C -                                                                       30
      REAL SVTL_PCBTHK                                                    30
      EQUIVALENCE (BANK_SVTL(10),SVTL_PCBTHK,RE_SVTL_PCBTHK),             30
     *(NAMES_SVTL(8),NSVTL_PCBTHK)                                        30
      EQUIVALENCE (COMMEN_SVTL(8),CSVTL_PCBTHK),(MAP_SVTL(1,8),           30
     *MSVTL_PCBTHK)                                                       30
      CHARACTER CSVTL_PCBTHK*40/' '/                                      30
      CHARACTER NSVTL_PCBTHK*12/'R.PCBTHK'/                               30
      INTEGER MSVTL_PCBTHK(2)/1,1/                                        30
      REAL RE_SVTL_PCBTHK                                                 30
C -                                                                       30
      REAL SVTL_PCBGAP                                                    30
      EQUIVALENCE (BANK_SVTL(11),SVTL_PCBGAP,RE_SVTL_PCBGAP),             30
     *(NAMES_SVTL(9),NSVTL_PCBGAP)                                        30
      EQUIVALENCE (COMMEN_SVTL(9),CSVTL_PCBGAP),(MAP_SVTL(1,9),           30
     *MSVTL_PCBGAP)                                                       30
      CHARACTER CSVTL_PCBGAP*40/' '/                                      30
      CHARACTER NSVTL_PCBGAP*12/'R.PCBGAP'/                               30
      INTEGER MSVTL_PCBGAP(2)/1,1/                                        30
      REAL RE_SVTL_PCBGAP                                                 30
C -                                                                       30
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                30
      PARAMETER (LEN_SVTL=11)                                             30
      PARAMETER (LLEN_SVTL=9)                                             30
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           30
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        30
      DATA IDNUM_SVTL/10*0/                                               30
      REAL BANK_SVTL(LEN_SVTL)                                            30
      CHARACTER*60 BPATH_SVTL/' '/                                        30
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          30
     *BPATH_SVTL                                                          30
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             30
C *                                                                       30
C -                                                                       32
      REAL SSLD_VERSION                                                   32
      EQUIVALENCE (BANK_SSLD(3),SSLD_VERSION,RE_SSLD_VERSION),            32
     *(NAMES_SSLD(1),NSSLD_VERSION)                                       32
      EQUIVALENCE (COMMEN_SSLD(1),CSSLD_VERSION),(MAP_SSLD(1,1),          32
     *MSSLD_VERSION)                                                      32
      CHARACTER CSSLD_VERSION*40/' '/                                     32
      CHARACTER NSSLD_VERSION*12/'R.VERSION'/                             32
      INTEGER MSSLD_VERSION(2)/1,1/                                       32
      REAL RE_SSLD_VERSION                                                32
C -                                                                       32
      REAL SSLD_SINRINN                                                   32
      EQUIVALENCE (BANK_SSLD(4),SSLD_SINRINN,RE_SSLD_SINRINN),            32
     *(NAMES_SSLD(2),NSSLD_SINRINN)                                       32
      EQUIVALENCE (COMMEN_SSLD(2),CSSLD_SINRINN),(MAP_SSLD(1,2),          32
     *MSSLD_SINRINN)                                                      32
      CHARACTER CSSLD_SINRINN*40/' '/                                     32
      CHARACTER NSSLD_SINRINN*12/'R.SINRINN'/                             32
      INTEGER MSSLD_SINRINN(2)/1,1/                                       32
      REAL RE_SSLD_SINRINN                                                32
C -                                                                       32
      REAL SSLD_SINROUT                                                   32
      EQUIVALENCE (BANK_SSLD(5),SSLD_SINROUT,RE_SSLD_SINROUT),            32
     *(NAMES_SSLD(3),NSSLD_SINROUT)                                       32
      EQUIVALENCE (COMMEN_SSLD(3),CSSLD_SINROUT),(MAP_SSLD(1,3),          32
     *MSSLD_SINROUT)                                                      32
      CHARACTER CSSLD_SINROUT*40/' '/                                     32
      CHARACTER NSSLD_SINROUT*12/'R.SINROUT'/                             32
      INTEGER MSSLD_SINROUT(2)/1,1/                                       32
      REAL RE_SSLD_SINROUT                                                32
C -                                                                       32
      REAL SSLD_SINLEN                                                    32
      EQUIVALENCE (BANK_SSLD(6),SSLD_SINLEN,RE_SSLD_SINLEN),              32
     *(NAMES_SSLD(4),NSSLD_SINLEN)                                        32
      EQUIVALENCE (COMMEN_SSLD(4),CSSLD_SINLEN),(MAP_SSLD(1,4),           32
     *MSSLD_SINLEN)                                                       32
      CHARACTER CSSLD_SINLEN*40/' '/                                      32
      CHARACTER NSSLD_SINLEN*12/'R.SINLEN'/                               32
      INTEGER MSSLD_SINLEN(2)/1,1/                                        32
      REAL RE_SSLD_SINLEN                                                 32
C -                                                                       32
      REAL SSLD_SSEPRINN                                                  32
      EQUIVALENCE (BANK_SSLD(7),SSLD_SSEPRINN,RE_SSLD_SSEPRINN),          32
     *(NAMES_SSLD(5),NSSLD_SSEPRINN)                                      32
      EQUIVALENCE (COMMEN_SSLD(5),CSSLD_SSEPRINN),(MAP_SSLD(1,5),         32
     *MSSLD_SSEPRINN)                                                     32
      CHARACTER CSSLD_SSEPRINN*40/' '/                                    32
      CHARACTER NSSLD_SSEPRINN*12/'R.SSEPRINN'/                           32
      INTEGER MSSLD_SSEPRINN(2)/1,1/                                      32
      REAL RE_SSLD_SSEPRINN                                               32
C -                                                                       32
      REAL SSLD_SSEPROUT                                                  32
      EQUIVALENCE (BANK_SSLD(8),SSLD_SSEPROUT,RE_SSLD_SSEPROUT),          32
     *(NAMES_SSLD(6),NSSLD_SSEPROUT)                                      32
      EQUIVALENCE (COMMEN_SSLD(6),CSSLD_SSEPROUT),(MAP_SSLD(1,6),         32
     *MSSLD_SSEPROUT)                                                     32
      CHARACTER CSSLD_SSEPROUT*40/' '/                                    32
      CHARACTER NSSLD_SSEPROUT*12/'R.SSEPROUT'/                           32
      INTEGER MSSLD_SSEPROUT(2)/1,1/                                      32
      REAL RE_SSLD_SSEPROUT                                               32
C -                                                                       32
      REAL SSLD_SSEPLEN                                                   32
      EQUIVALENCE (BANK_SSLD(9),SSLD_SSEPLEN,RE_SSLD_SSEPLEN),            32
     *(NAMES_SSLD(7),NSSLD_SSEPLEN)                                       32
      EQUIVALENCE (COMMEN_SSLD(7),CSSLD_SSEPLEN),(MAP_SSLD(1,7),          32
     *MSSLD_SSEPLEN)                                                      32
      CHARACTER CSSLD_SSEPLEN*40/' '/                                     32
      CHARACTER NSSLD_SSEPLEN*12/'R.SSEPLEN'/                             32
      INTEGER MSSLD_SSEPLEN(2)/1,1/                                       32
      REAL RE_SSLD_SSEPLEN                                                32
C -                                                                       32
      REAL SSLD_SOUTRINN                                                  32
      EQUIVALENCE (BANK_SSLD(10),SSLD_SOUTRINN,RE_SSLD_SOUTRINN),         32
     *(NAMES_SSLD(8),NSSLD_SOUTRINN)                                      32
      EQUIVALENCE (COMMEN_SSLD(8),CSSLD_SOUTRINN),(MAP_SSLD(1,8),         32
     *MSSLD_SOUTRINN)                                                     32
      CHARACTER CSSLD_SOUTRINN*40/' '/                                    32
      CHARACTER NSSLD_SOUTRINN*12/'R.SOUTRINN'/                           32
      INTEGER MSSLD_SOUTRINN(2)/1,1/                                      32
      REAL RE_SSLD_SOUTRINN                                               32
C -                                                                       32
      REAL SSLD_SOUTROUT                                                  32
      EQUIVALENCE (BANK_SSLD(11),SSLD_SOUTROUT,RE_SSLD_SOUTROUT),         32
     *(NAMES_SSLD(9),NSSLD_SOUTROUT)                                      32
      EQUIVALENCE (COMMEN_SSLD(9),CSSLD_SOUTROUT),(MAP_SSLD(1,9),         32
     *MSSLD_SOUTROUT)                                                     32
      CHARACTER CSSLD_SOUTROUT*40/' '/                                    32
      CHARACTER NSSLD_SOUTROUT*12/'R.SOUTROUT'/                           32
      INTEGER MSSLD_SOUTROUT(2)/1,1/                                      32
      REAL RE_SSLD_SOUTROUT                                               32
C -                                                                       32
      REAL SSLD_SOUTLEN                                                   32
      EQUIVALENCE (BANK_SSLD(12),SSLD_SOUTLEN,RE_SSLD_SOUTLEN),           32
     *(NAMES_SSLD(10),NSSLD_SOUTLEN)                                      32
      EQUIVALENCE (COMMEN_SSLD(10),CSSLD_SOUTLEN),(MAP_SSLD(1,10),        32
     *MSSLD_SOUTLEN)                                                      32
      CHARACTER CSSLD_SOUTLEN*40/' '/                                     32
      CHARACTER NSSLD_SOUTLEN*12/'R.SOUTLEN'/                             32
      INTEGER MSSLD_SOUTLEN(2)/1,1/                                       32
      REAL RE_SSLD_SOUTLEN                                                32
C -                                                                       32
      REAL SSLD_ALMESHID                                                  32
      EQUIVALENCE (BANK_SSLD(13),SSLD_ALMESHID,RE_SSLD_ALMESHID),         32
     *(NAMES_SSLD(11),NSSLD_ALMESHID)                                     32
      EQUIVALENCE (COMMEN_SSLD(11),CSSLD_ALMESHID),(MAP_SSLD(1,11),       32
     *MSSLD_ALMESHID)                                                     32
      CHARACTER CSSLD_ALMESHID*40/' '/                                    32
      CHARACTER NSSLD_ALMESHID*12/'R.ALMESHID'/                           32
      INTEGER MSSLD_ALMESHID(2)/1,1/                                      32
      REAL RE_SSLD_ALMESHID                                               32
C -                                                                       32
      REAL SSLD_ALMESHOD                                                  32
      EQUIVALENCE (BANK_SSLD(14),SSLD_ALMESHOD,RE_SSLD_ALMESHOD),         32
     *(NAMES_SSLD(12),NSSLD_ALMESHOD)                                     32
      EQUIVALENCE (COMMEN_SSLD(12),CSSLD_ALMESHOD),(MAP_SSLD(1,12),       32
     *MSSLD_ALMESHOD)                                                     32
      CHARACTER CSSLD_ALMESHOD*40/' '/                                    32
      CHARACTER NSSLD_ALMESHOD*12/'R.ALMESHOD'/                           32
      INTEGER MSSLD_ALMESHOD(2)/1,1/                                      32
      REAL RE_SSLD_ALMESHOD                                               32
C -                                                                       32
      REAL SSLD_ALMSHTHK                                                  32
      EQUIVALENCE (BANK_SSLD(15),SSLD_ALMSHTHK,RE_SSLD_ALMSHTHK),         32
     *(NAMES_SSLD(13),NSSLD_ALMSHTHK)                                     32
      EQUIVALENCE (COMMEN_SSLD(13),CSSLD_ALMSHTHK),(MAP_SSLD(1,13),       32
     *MSSLD_ALMSHTHK)                                                     32
      CHARACTER CSSLD_ALMSHTHK*40/' '/                                    32
      CHARACTER NSSLD_ALMSHTHK*12/'R.ALMSHTHK'/                           32
      INTEGER MSSLD_ALMSHTHK(2)/1,1/                                      32
      REAL RE_SSLD_ALMSHTHK                                               32
C -                                                                       32
      REAL SSLD_ALMSHPOS                                                  32
      EQUIVALENCE (BANK_SSLD(16),SSLD_ALMSHPOS,RE_SSLD_ALMSHPOS),         32
     *(NAMES_SSLD(14),NSSLD_ALMSHPOS)                                     32
      EQUIVALENCE (COMMEN_SSLD(14),CSSLD_ALMSHPOS),(MAP_SSLD(1,14),       32
     *MSSLD_ALMSHPOS)                                                     32
      CHARACTER CSSLD_ALMSHPOS*40/' '/                                    32
      CHARACTER NSSLD_ALMSHPOS*12/'R.ALMSHPOS'/                           32
      INTEGER MSSLD_ALMSHPOS(2)/1,1/                                      32
      REAL RE_SSLD_ALMSHPOS                                               32
C -                                                                       32
      INTEGER LEN_SSLD,LLEN_SSLD,LINK_SSLD                                32
      PARAMETER (LEN_SSLD=16)                                             32
      PARAMETER (LLEN_SSLD=14)                                            32
      INTEGER FLAG_SSLD/0/,MAP_SSLD(2,LLEN_SSLD),IDNUM_SSLD(10)           32
      CHARACTER NAMES_SSLD(LLEN_SSLD)*12,COMMEN_SSLD(LLEN_SSLD)*40        32
      DATA IDNUM_SSLD/10*0/                                               32
      REAL BANK_SSLD(LEN_SSLD)                                            32
      CHARACTER*60 BPATH_SSLD/' '/                                        32
      SAVE FLAG_SSLD,MAP_SSLD,IDNUM_SSLD,NAMES_SSLD,COMMEN_SSLD,          32
     *BPATH_SSLD                                                          32
      DATA LINK_SSLD/0/,BANK_SSLD/LEN_SSLD*0/                             32
C *                                                                       32
C -                                                                       33
      INTEGER SCBP_LAYER                                                  33
      EQUIVALENCE (BANK_SCBP(3),SCBP_LAYER,RE_SCBP_LAYER),                33
     *(NAMES_SCBP(1),NSCBP_LAYER)                                         33
      EQUIVALENCE (COMMEN_SCBP(1),CSCBP_LAYER),(MAP_SCBP(1,1),            33
     *MSCBP_LAYER)                                                        33
      CHARACTER CSCBP_LAYER*40/' '/                                       33
      CHARACTER NSCBP_LAYER*12/'I.LAYER'/                                 33
      INTEGER MSCBP_LAYER(2)/1,1/                                         33
      REAL RE_SCBP_LAYER                                                  33
C -                                                                       33
      REAL SCBP_LEN                                                       33
      EQUIVALENCE (BANK_SCBP(4),SCBP_LEN,RE_SCBP_LEN),(NAMES_SCBP(2),     33
     *NSCBP_LEN)                                                          33
      EQUIVALENCE (COMMEN_SCBP(2),CSCBP_LEN),(MAP_SCBP(1,2),MSCBP_LEN)    33
      CHARACTER CSCBP_LEN*40/' '/                                         33
      CHARACTER NSCBP_LEN*12/'R.LEN'/                                     33
      INTEGER MSCBP_LEN(2)/1,1/                                           33
      REAL RE_SCBP_LEN                                                    33
C -                                                                       33
      REAL SCBP_RMIN1                                                     33
      EQUIVALENCE (BANK_SCBP(5),SCBP_RMIN1,RE_SCBP_RMIN1),                33
     *(NAMES_SCBP(3),NSCBP_RMIN1)                                         33
      EQUIVALENCE (COMMEN_SCBP(3),CSCBP_RMIN1),(MAP_SCBP(1,3),            33
     *MSCBP_RMIN1)                                                        33
      CHARACTER CSCBP_RMIN1*40/' '/                                       33
      CHARACTER NSCBP_RMIN1*12/'R.RMIN1'/                                 33
      INTEGER MSCBP_RMIN1(2)/1,1/                                         33
      REAL RE_SCBP_RMIN1                                                  33
C -                                                                       33
      REAL SCBP_RMAX1                                                     33
      EQUIVALENCE (BANK_SCBP(6),SCBP_RMAX1,RE_SCBP_RMAX1),                33
     *(NAMES_SCBP(4),NSCBP_RMAX1)                                         33
      EQUIVALENCE (COMMEN_SCBP(4),CSCBP_RMAX1),(MAP_SCBP(1,4),            33
     *MSCBP_RMAX1)                                                        33
      CHARACTER CSCBP_RMAX1*40/' '/                                       33
      CHARACTER NSCBP_RMAX1*12/'R.RMAX1'/                                 33
      INTEGER MSCBP_RMAX1(2)/1,1/                                         33
      REAL RE_SCBP_RMAX1                                                  33
C -                                                                       33
      REAL SCBP_RMIN2                                                     33
      EQUIVALENCE (BANK_SCBP(7),SCBP_RMIN2,RE_SCBP_RMIN2),                33
     *(NAMES_SCBP(5),NSCBP_RMIN2)                                         33
      EQUIVALENCE (COMMEN_SCBP(5),CSCBP_RMIN2),(MAP_SCBP(1,5),            33
     *MSCBP_RMIN2)                                                        33
      CHARACTER CSCBP_RMIN2*40/' '/                                       33
      CHARACTER NSCBP_RMIN2*12/'R.RMIN2'/                                 33
      INTEGER MSCBP_RMIN2(2)/1,1/                                         33
      REAL RE_SCBP_RMIN2                                                  33
C -                                                                       33
      REAL SCBP_RMAX2                                                     33
      EQUIVALENCE (BANK_SCBP(8),SCBP_RMAX2,RE_SCBP_RMAX2),                33
     *(NAMES_SCBP(6),NSCBP_RMAX2)                                         33
      EQUIVALENCE (COMMEN_SCBP(6),CSCBP_RMAX2),(MAP_SCBP(1,6),            33
     *MSCBP_RMAX2)                                                        33
      CHARACTER CSCBP_RMAX2*40/' '/                                       33
      CHARACTER NSCBP_RMAX2*12/'R.RMAX2'/                                 33
      INTEGER MSCBP_RMAX2(2)/1,1/                                         33
      REAL RE_SCBP_RMAX2                                                  33
C -                                                                       33
      REAL SCBP_VOL                                                       33
      EQUIVALENCE (BANK_SCBP(9),SCBP_VOL,RE_SCBP_VOL),(NAMES_SCBP(7),     33
     *NSCBP_VOL)                                                          33
      EQUIVALENCE (COMMEN_SCBP(7),CSCBP_VOL),(MAP_SCBP(1,7),MSCBP_VOL)    33
      CHARACTER CSCBP_VOL*40/' '/                                         33
      CHARACTER NSCBP_VOL*12/'R.VOL'/                                     33
      INTEGER MSCBP_VOL(2)/1,1/                                           33
      REAL RE_SCBP_VOL                                                    33
C -                                                                       33
      INTEGER LEN_SCBP,LLEN_SCBP,LINK_SCBP                                33
      PARAMETER (LEN_SCBP=9)                                              33
      PARAMETER (LLEN_SCBP=7)                                             33
      INTEGER FLAG_SCBP/0/,MAP_SCBP(2,LLEN_SCBP),IDNUM_SCBP(10)           33
      CHARACTER NAMES_SCBP(LLEN_SCBP)*12,COMMEN_SCBP(LLEN_SCBP)*40        33
      DATA IDNUM_SCBP/10*0/                                               33
      REAL BANK_SCBP(LEN_SCBP)                                            33
      CHARACTER*60 BPATH_SCBP/' '/                                        33
      SAVE FLAG_SCBP,MAP_SCBP,IDNUM_SCBP,NAMES_SCBP,COMMEN_SCBP,          33
     *BPATH_SCBP                                                          33
      DATA LINK_SCBP/0/,BANK_SCBP/LEN_SCBP*0/                             33
C *                                                                       33
C -                                                                       34
      INTEGER SFEP_LAYER                                                  34
      EQUIVALENCE (BANK_SFEP(3),SFEP_LAYER,RE_SFEP_LAYER),                34
     *(NAMES_SFEP(1),NSFEP_LAYER)                                         34
      EQUIVALENCE (COMMEN_SFEP(1),CSFEP_LAYER),(MAP_SFEP(1,1),            34
     *MSFEP_LAYER)                                                        34
      CHARACTER CSFEP_LAYER*40/' '/                                       34
      CHARACTER NSFEP_LAYER*12/'I.LAYER'/                                 34
      INTEGER MSFEP_LAYER(2)/1,1/                                         34
      REAL RE_SFEP_LAYER                                                  34
C -                                                                       34
      REAL SFEP_LEN                                                       34
      EQUIVALENCE (BANK_SFEP(4),SFEP_LEN,RE_SFEP_LEN),(NAMES_SFEP(2),     34
     *NSFEP_LEN)                                                          34
      EQUIVALENCE (COMMEN_SFEP(2),CSFEP_LEN),(MAP_SFEP(1,2),MSFEP_LEN)    34
      CHARACTER CSFEP_LEN*40/' '/                                         34
      CHARACTER NSFEP_LEN*12/'R.LEN'/                                     34
      INTEGER MSFEP_LEN(2)/1,1/                                           34
      REAL RE_SFEP_LEN                                                    34
C -                                                                       34
      REAL SFEP_RMIN1                                                     34
      EQUIVALENCE (BANK_SFEP(5),SFEP_RMIN1,RE_SFEP_RMIN1),                34
     *(NAMES_SFEP(3),NSFEP_RMIN1)                                         34
      EQUIVALENCE (COMMEN_SFEP(3),CSFEP_RMIN1),(MAP_SFEP(1,3),            34
     *MSFEP_RMIN1)                                                        34
      CHARACTER CSFEP_RMIN1*40/' '/                                       34
      CHARACTER NSFEP_RMIN1*12/'R.RMIN1'/                                 34
      INTEGER MSFEP_RMIN1(2)/1,1/                                         34
      REAL RE_SFEP_RMIN1                                                  34
C -                                                                       34
      REAL SFEP_RMAX1                                                     34
      EQUIVALENCE (BANK_SFEP(6),SFEP_RMAX1,RE_SFEP_RMAX1),                34
     *(NAMES_SFEP(4),NSFEP_RMAX1)                                         34
      EQUIVALENCE (COMMEN_SFEP(4),CSFEP_RMAX1),(MAP_SFEP(1,4),            34
     *MSFEP_RMAX1)                                                        34
      CHARACTER CSFEP_RMAX1*40/' '/                                       34
      CHARACTER NSFEP_RMAX1*12/'R.RMAX1'/                                 34
      INTEGER MSFEP_RMAX1(2)/1,1/                                         34
      REAL RE_SFEP_RMAX1                                                  34
C -                                                                       34
      REAL SFEP_RMIN2                                                     34
      EQUIVALENCE (BANK_SFEP(7),SFEP_RMIN2,RE_SFEP_RMIN2),                34
     *(NAMES_SFEP(5),NSFEP_RMIN2)                                         34
      EQUIVALENCE (COMMEN_SFEP(5),CSFEP_RMIN2),(MAP_SFEP(1,5),            34
     *MSFEP_RMIN2)                                                        34
      CHARACTER CSFEP_RMIN2*40/' '/                                       34
      CHARACTER NSFEP_RMIN2*12/'R.RMIN2'/                                 34
      INTEGER MSFEP_RMIN2(2)/1,1/                                         34
      REAL RE_SFEP_RMIN2                                                  34
C -                                                                       34
      REAL SFEP_RMAX2                                                     34
      EQUIVALENCE (BANK_SFEP(8),SFEP_RMAX2,RE_SFEP_RMAX2),                34
     *(NAMES_SFEP(6),NSFEP_RMAX2)                                         34
      EQUIVALENCE (COMMEN_SFEP(6),CSFEP_RMAX2),(MAP_SFEP(1,6),            34
     *MSFEP_RMAX2)                                                        34
      CHARACTER CSFEP_RMAX2*40/' '/                                       34
      CHARACTER NSFEP_RMAX2*12/'R.RMAX2'/                                 34
      INTEGER MSFEP_RMAX2(2)/1,1/                                         34
      REAL RE_SFEP_RMAX2                                                  34
C -                                                                       34
      REAL SFEP_VOL                                                       34
      EQUIVALENCE (BANK_SFEP(9),SFEP_VOL,RE_SFEP_VOL),(NAMES_SFEP(7),     34
     *NSFEP_VOL)                                                          34
      EQUIVALENCE (COMMEN_SFEP(7),CSFEP_VOL),(MAP_SFEP(1,7),MSFEP_VOL)    34
      CHARACTER CSFEP_VOL*40/' '/                                         34
      CHARACTER NSFEP_VOL*12/'R.VOL'/                                     34
      INTEGER MSFEP_VOL(2)/1,1/                                           34
      REAL RE_SFEP_VOL                                                    34
C -                                                                       34
      REAL SFEP_VOLPLAST                                                  34
      EQUIVALENCE (BANK_SFEP(10),SFEP_VOLPLAST,RE_SFEP_VOLPLAST),         34
     *(NAMES_SFEP(8),NSFEP_VOLPLAST)                                      34
      EQUIVALENCE (COMMEN_SFEP(8),CSFEP_VOLPLAST),(MAP_SFEP(1,8),         34
     *MSFEP_VOLPLAST)                                                     34
      CHARACTER CSFEP_VOLPLAST*40/' '/                                    34
      CHARACTER NSFEP_VOLPLAST*12/'R.VOLPLAST'/                           34
      INTEGER MSFEP_VOLPLAST(2)/1,1/                                      34
      REAL RE_SFEP_VOLPLAST                                               34
C -                                                                       34
      INTEGER LEN_SFEP,LLEN_SFEP,LINK_SFEP                                34
      PARAMETER (LEN_SFEP=10)                                             34
      PARAMETER (LLEN_SFEP=8)                                             34
      INTEGER FLAG_SFEP/0/,MAP_SFEP(2,LLEN_SFEP),IDNUM_SFEP(10)           34
      CHARACTER NAMES_SFEP(LLEN_SFEP)*12,COMMEN_SFEP(LLEN_SFEP)*40        34
      DATA IDNUM_SFEP/10*0/                                               34
      REAL BANK_SFEP(LEN_SFEP)                                            34
      CHARACTER*60 BPATH_SFEP/' '/                                        34
      SAVE FLAG_SFEP,MAP_SFEP,IDNUM_SFEP,NAMES_SFEP,COMMEN_SFEP,          34
     *BPATH_SFEP                                                          34
      DATA LINK_SFEP/0/,BANK_SFEP/LEN_SFEP*0/                             34
C *                                                                       34
C -                                                                       35
      INTEGER SWCX_LAYER                                                  35
      EQUIVALENCE (BANK_SWCX(3),SWCX_LAYER,RE_SWCX_LAYER),                35
     *(NAMES_SWCX(1),NSWCX_LAYER)                                         35
      EQUIVALENCE (COMMEN_SWCX(1),CSWCX_LAYER),(MAP_SWCX(1,1),            35
     *MSWCX_LAYER)                                                        35
      CHARACTER CSWCX_LAYER*40/' '/                                       35
      CHARACTER NSWCX_LAYER*12/'I.LAYER'/                                 35
      INTEGER MSWCX_LAYER(2)/1,1/                                         35
      REAL RE_SWCX_LAYER                                                  35
C -                                                                       35
      REAL SWCX_LENGTH                                                    35
      EQUIVALENCE (BANK_SWCX(4),SWCX_LENGTH,RE_SWCX_LENGTH),              35
     *(NAMES_SWCX(2),NSWCX_LENGTH)                                        35
      EQUIVALENCE (COMMEN_SWCX(2),CSWCX_LENGTH),(MAP_SWCX(1,2),           35
     *MSWCX_LENGTH)                                                       35
      CHARACTER CSWCX_LENGTH*40/' '/                                      35
      CHARACTER NSWCX_LENGTH*12/'R.LENGTH'/                               35
      INTEGER MSWCX_LENGTH(2)/1,1/                                        35
      REAL RE_SWCX_LENGTH                                                 35
C -                                                                       35
      REAL SWCX_DR                                                        35
      EQUIVALENCE (BANK_SWCX(5),SWCX_DR,RE_SWCX_DR),(NAMES_SWCX(3),       35
     *NSWCX_DR)                                                           35
      EQUIVALENCE (COMMEN_SWCX(3),CSWCX_DR),(MAP_SWCX(1,3),MSWCX_DR)      35
      CHARACTER CSWCX_DR*40/' '/                                          35
      CHARACTER NSWCX_DR*12/'R.DR'/                                       35
      INTEGER MSWCX_DR(2)/1,1/                                            35
      REAL RE_SWCX_DR                                                     35
C -                                                                       35
      REAL SWCX_OFFSET                                                    35
      EQUIVALENCE (BANK_SWCX(6),SWCX_OFFSET,RE_SWCX_OFFSET),              35
     *(NAMES_SWCX(4),NSWCX_OFFSET)                                        35
      EQUIVALENCE (COMMEN_SWCX(4),CSWCX_OFFSET),(MAP_SWCX(1,4),           35
     *MSWCX_OFFSET)                                                       35
      CHARACTER CSWCX_OFFSET*40/' '/                                      35
      CHARACTER NSWCX_OFFSET*12/'R.OFFSET'/                               35
      INTEGER MSWCX_OFFSET(2)/1,1/                                        35
      REAL RE_SWCX_OFFSET                                                 35
C -                                                                       35
      REAL SWCX_RAD                                                       35
      EQUIVALENCE (BANK_SWCX(7),SWCX_RAD,RE_SWCX_RAD),(NAMES_SWCX(5),     35
     *NSWCX_RAD)                                                          35
      EQUIVALENCE (COMMEN_SWCX(5),CSWCX_RAD),(MAP_SWCX(1,5),MSWCX_RAD)    35
      CHARACTER CSWCX_RAD*40/' '/                                         35
      CHARACTER NSWCX_RAD*12/'R.RAD'/                                     35
      INTEGER MSWCX_RAD(2)/1,1/                                           35
      REAL RE_SWCX_RAD                                                    35
C -                                                                       35
      REAL SWCX_WALL                                                      35
      EQUIVALENCE (BANK_SWCX(8),SWCX_WALL,RE_SWCX_WALL),(NAMES_SWCX(6),   35
     *NSWCX_WALL)                                                         35
      EQUIVALENCE (COMMEN_SWCX(6),CSWCX_WALL),(MAP_SWCX(1,6),             35
     *MSWCX_WALL)                                                         35
      CHARACTER CSWCX_WALL*40/' '/                                        35
      CHARACTER NSWCX_WALL*12/'R.WALL'/                                   35
      INTEGER MSWCX_WALL(2)/1,1/                                          35
      REAL RE_SWCX_WALL                                                   35
C -                                                                       35
      REAL SWCX_ROFFSET                                                   35
      EQUIVALENCE (BANK_SWCX(9),SWCX_ROFFSET,RE_SWCX_ROFFSET),            35
     *(NAMES_SWCX(7),NSWCX_ROFFSET)                                       35
      EQUIVALENCE (COMMEN_SWCX(7),CSWCX_ROFFSET),(MAP_SWCX(1,7),          35
     *MSWCX_ROFFSET)                                                      35
      CHARACTER CSWCX_ROFFSET*40/' '/                                     35
      CHARACTER NSWCX_ROFFSET*12/'R.ROFFSET'/                             35
      INTEGER MSWCX_ROFFSET(2)/1,1/                                       35
      REAL RE_SWCX_ROFFSET                                                35
C -                                                                       35
      INTEGER LEN_SWCX,LLEN_SWCX,LINK_SWCX                                35
      PARAMETER (LEN_SWCX=9)                                              35
      PARAMETER (LLEN_SWCX=7)                                             35
      INTEGER FLAG_SWCX/0/,MAP_SWCX(2,LLEN_SWCX),IDNUM_SWCX(10)           35
      CHARACTER NAMES_SWCX(LLEN_SWCX)*12,COMMEN_SWCX(LLEN_SWCX)*40        35
      DATA IDNUM_SWCX/10*0/                                               35
      REAL BANK_SWCX(LEN_SWCX)                                            35
      CHARACTER*60 BPATH_SWCX/' '/                                        35
      SAVE FLAG_SWCX,MAP_SWCX,IDNUM_SWCX,NAMES_SWCX,COMMEN_SWCX,          35
     *BPATH_SWCX                                                          35
      DATA LINK_SWCX/0/,BANK_SWCX/LEN_SWCX*0/                             35
C *                                                                       35
C -                                                                       36
      REAL SOUP_VERSION                                                   36
      EQUIVALENCE (BANK_SOUP(3),SOUP_VERSION,RE_SOUP_VERSION),            36
     *(NAMES_SOUP(1),NSOUP_VERSION)                                       36
      EQUIVALENCE (COMMEN_SOUP(1),CSOUP_VERSION),(MAP_SOUP(1,1),          36
     *MSOUP_VERSION)                                                      36
      CHARACTER CSOUP_VERSION*40/' '/                                     36
      CHARACTER NSOUP_VERSION*12/'R.VERSION'/                             36
      INTEGER MSOUP_VERSION(2)/1,1/                                       36
      REAL RE_SOUP_VERSION                                                36
C -                                                                       36
      REAL SOUP_LENGTH                                                    36
      EQUIVALENCE (BANK_SOUP(4),SOUP_LENGTH,RE_SOUP_LENGTH),              36
     *(NAMES_SOUP(2),NSOUP_LENGTH)                                        36
      EQUIVALENCE (COMMEN_SOUP(2),CSOUP_LENGTH),(MAP_SOUP(1,2),           36
     *MSOUP_LENGTH)                                                       36
      CHARACTER CSOUP_LENGTH*40/' '/                                      36
      CHARACTER NSOUP_LENGTH*12/'R.LENGTH'/                               36
      INTEGER MSOUP_LENGTH(2)/1,1/                                        36
      REAL RE_SOUP_LENGTH                                                 36
C -                                                                       36
      REAL SOUP_ROUT                                                      36
      EQUIVALENCE (BANK_SOUP(5),SOUP_ROUT,RE_SOUP_ROUT),(NAMES_SOUP(3),   36
     *NSOUP_ROUT)                                                         36
      EQUIVALENCE (COMMEN_SOUP(3),CSOUP_ROUT),(MAP_SOUP(1,3),             36
     *MSOUP_ROUT)                                                         36
      CHARACTER CSOUP_ROUT*40/' '/                                        36
      CHARACTER NSOUP_ROUT*12/'R.ROUT'/                                   36
      INTEGER MSOUP_ROUT(2)/1,1/                                          36
      REAL RE_SOUP_ROUT                                                   36
C -                                                                       36
      REAL SOUP_DR                                                        36
      EQUIVALENCE (BANK_SOUP(6),SOUP_DR,RE_SOUP_DR),(NAMES_SOUP(4),       36
     *NSOUP_DR)                                                           36
      EQUIVALENCE (COMMEN_SOUP(4),CSOUP_DR),(MAP_SOUP(1,4),MSOUP_DR)      36
      CHARACTER CSOUP_DR*40/' '/                                          36
      CHARACTER NSOUP_DR*12/'R.DR'/                                       36
      INTEGER MSOUP_DR(2)/1,1/                                            36
      REAL RE_SOUP_DR                                                     36
C -                                                                       36
      REAL SOUP_PHI1                                                      36
      EQUIVALENCE (BANK_SOUP(7),SOUP_PHI1,RE_SOUP_PHI1),(NAMES_SOUP(5),   36
     *NSOUP_PHI1)                                                         36
      EQUIVALENCE (COMMEN_SOUP(5),CSOUP_PHI1),(MAP_SOUP(1,5),             36
     *MSOUP_PHI1)                                                         36
      CHARACTER CSOUP_PHI1*40/' '/                                        36
      CHARACTER NSOUP_PHI1*12/'R.PHI1'/                                   36
      INTEGER MSOUP_PHI1(2)/1,1/                                          36
      REAL RE_SOUP_PHI1                                                   36
C -                                                                       36
      REAL SOUP_PHI2                                                      36
      EQUIVALENCE (BANK_SOUP(8),SOUP_PHI2,RE_SOUP_PHI2),(NAMES_SOUP(6),   36
     *NSOUP_PHI2)                                                         36
      EQUIVALENCE (COMMEN_SOUP(6),CSOUP_PHI2),(MAP_SOUP(1,6),             36
     *MSOUP_PHI2)                                                         36
      CHARACTER CSOUP_PHI2*40/' '/                                        36
      CHARACTER NSOUP_PHI2*12/'R.PHI2'/                                   36
      INTEGER MSOUP_PHI2(2)/1,1/                                          36
      REAL RE_SOUP_PHI2                                                   36
C -                                                                       36
      REAL SOUP_DIAMOUT                                                   36
      EQUIVALENCE (BANK_SOUP(9),SOUP_DIAMOUT,RE_SOUP_DIAMOUT),            36
     *(NAMES_SOUP(7),NSOUP_DIAMOUT)                                       36
      EQUIVALENCE (COMMEN_SOUP(7),CSOUP_DIAMOUT),(MAP_SOUP(1,7),          36
     *MSOUP_DIAMOUT)                                                      36
      CHARACTER CSOUP_DIAMOUT*40/' '/                                     36
      CHARACTER NSOUP_DIAMOUT*12/'R.DIAMOUT'/                             36
      INTEGER MSOUP_DIAMOUT(2)/1,1/                                       36
      REAL RE_SOUP_DIAMOUT                                                36
C -                                                                       36
      REAL SOUP_DIAMIN                                                    36
      EQUIVALENCE (BANK_SOUP(10),SOUP_DIAMIN,RE_SOUP_DIAMIN),             36
     *(NAMES_SOUP(8),NSOUP_DIAMIN)                                        36
      EQUIVALENCE (COMMEN_SOUP(8),CSOUP_DIAMIN),(MAP_SOUP(1,8),           36
     *MSOUP_DIAMIN)                                                       36
      CHARACTER CSOUP_DIAMIN*40/' '/                                      36
      CHARACTER NSOUP_DIAMIN*12/'R.DIAMIN'/                               36
      INTEGER MSOUP_DIAMIN(2)/1,1/                                        36
      REAL RE_SOUP_DIAMIN                                                 36
C -                                                                       36
      INTEGER LEN_SOUP,LLEN_SOUP,LINK_SOUP                                36
      PARAMETER (LEN_SOUP=10)                                             36
      PARAMETER (LLEN_SOUP=8)                                             36
      INTEGER FLAG_SOUP/0/,MAP_SOUP(2,LLEN_SOUP),IDNUM_SOUP(10)           36
      CHARACTER NAMES_SOUP(LLEN_SOUP)*12,COMMEN_SOUP(LLEN_SOUP)*40        36
      DATA IDNUM_SOUP/10*0/                                               36
      REAL BANK_SOUP(LEN_SOUP)                                            36
      CHARACTER*60 BPATH_SOUP/' '/                                        36
      SAVE FLAG_SOUP,MAP_SOUP,IDNUM_SOUP,NAMES_SOUP,COMMEN_SOUP,          36
     *BPATH_SOUP                                                          36
      DATA LINK_SOUP/0/,BANK_SOUP/LEN_SOUP*0/                             36
C *                                                                       36
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 37
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         39
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           41
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          43
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    45
      REAL XBUF,PHI,XBUF1,XBUF2                                           47
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     49
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          51
     *SHIELD_PHI(4)                                                       51
      REAL TRAPY,SSIDX,SSIRY                                              53
      INTEGER I_PHI                                                       55
      RADII(1)= 6.37                                                      57
      RADII(2)= 7.38                                                      58
      RADII(3)=10.38                                                      59
      RADII(4)=11.27                                                      60
      RADII(5)=14.19                                                      61
      RADII(6)=15.13                                                      62
      RAD_OFFSET=0.015                                                    63
      SHIELD_PHI(1)=9.0                                                   64
      SHIELD_PHI(2)=27.0                                                  65
      SHIELD_PHI(3)=45.0                                                  66
      SHIELD_PHI(4)=63.0                                                  67
C *                                                                       68
      CALL AGSBEGM(MODULE,IPRIN)                                          68
      IF (IPRIN.LT.0) RETURN                                              68
C Filling bank SVTG - BASIC SVT DIMENSIONS                                68
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   68
      BPATH_SVTG='SVTG*'                                                  68
      IDNUM_SVTG(1)=0                                                     69
C GEOMETRY VERSION                                                        69
      SVTG_VERSION=2                                                      69
      CSVTG_VERSION='GEOMETRY VERSION'                                    69
C NUMBER OF SVT LAYERS (WAS 7)                                            70
      SVTG_NLAYER=7                                                       70
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         70
C STV INNERMOST RADIUS                                                    71
      SVTG_RSIZEMIN=4.100                                                 71
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               71
C STV OUTERMOST RADIUS                                                    72
      SVTG_RSIZEMAX=46.107                                                72
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               72
C SVT+FTPC LENGTH                                                         73
      SVTG_ZSIZEMAX=270                                                   73
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    73
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          74
      SVTG_ANGOFF=0                                                       74
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       74
C VERSIONING OF THE SHIELD                                                75
      SVTG_SUPPORTVER=1                                                   75
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         75
C VERSIONING OF THE SUPPORT CONE                                          76
      SVTG_CONEVER=1                                                      76
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      76
C WHETHER WE USE THE GEANT MANY OPTION                                    77
      SVTG_IFMANY=0                                                       77
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 77
C THE INDEX OF THE INNERMOST LAYER                                        78
      SVTG_NMIN=1                                                         78
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       78
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    79
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           79
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   79
C *                                                                       79
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    79
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           79
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   79
C *                                                                       80
      CALL AGSBEGM(MODULE,IPRIN)                                          80
      IF (IPRIN.LT.0) RETURN                                              80
C Filling bank SWCA - WAFER CARRIER                                       80
      BANK_TITLE='WAFER CARRIER'                                          80
      BPATH_SWCA='SWCA*'                                                  80
      IDNUM_SWCA(1)=0                                                     81
C GEOMETRY VERSION                                                        81
      SWCA_VERSION=1                                                      81
      CSWCA_VERSION='GEOMETRY VERSION'                                    81
C LADDER LENGTH                                                           82
      SWCA_LENGTH=56.04                                                   82
      CSWCA_LENGTH='LADDER LENGTH'                                        82
C WAFER WIDTH                                                             83
      SWCA_WAFERWID=6.305                                                 83
      CSWCA_WAFERWID='WAFER WIDTH'                                        83
C WAFER LENGTH                                                            84
      SWCA_WAFERLEN=6.305                                                 84
      CSWCA_WAFERLEN='WAFER LENGTH'                                       84
C WAFER THICKNESS                                                         85
      SWCA_WAFERTHK=0.0300                                                85
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    85
C ROHA CELL PLUS GLUE THICKNESS                                           86
      SWCA_ROHATHK=0.0381                                                 86
      CSWCA_ROHATHK='ROHA CELL PLUS GLUE THICKNESS'                       86
C WAFER CARRIER RAILS WIDTH                                               87
      SWCA_WAFCARWD=1.5                                                   87
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          87
C WAFER CARRIER THICKNESS                                                 88
      SWCA_WAFCARTH=0.0300                                                88
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            88
C INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY                      89
      SWCA_WAFERGAP=0.0                                                   89
      CSWCA_WAFERGAP='INTER-WAFER GAP - THERE IS A 50 MICRON UNCERTAINTY  89
     *'                                                                   89
C DRIFT DIRECTION                                                         90
      SWCA_DRIFT=1                                                        90
      CSWCA_DRIFT='DRIFT DIRECTION'                                       90
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        91
      SWCA_STRUTLEN=1.0                                                   91
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   91
C DEFINE THE SENSITIVE SQUARE WIDTH (NEW)                                 92
      SWCA_SENSWID=6.000                                                  92
      CSWCA_SENSWID='DEFINE THE SENSITIVE SQUARE WIDTH (NEW)'             92
C DEFINE THE SENSITIVE SQUARE LENGTH (NEW)                                93
      SWCA_SENSLEN=6.000                                                  93
      CSWCA_SENSLEN='DEFINE THE SENSITIVE SQUARE LENGTH (NEW)'            93
C GIRTH (FOR LACK OF A BETTER WORD)                                       94
      SWCA_SENSGIR=5.775                                                  94
      CSWCA_SENSGIR='GIRTH (FOR LACK OF A BETTER WORD)'                   94
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    95
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           95
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   95
C *                                                                       95
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    95
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           95
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   95
C *                                                                       96
      CALL AGSBEGM(MODULE,IPRIN)                                          96
      IF (IPRIN.LT.0) RETURN                                              96
C Filling bank SSUP - SUPPORT STRUCTURES                                  96
      BANK_TITLE='SUPPORT STRUCTURES'                                     96
      BPATH_SSUP='SSUP*'                                                  96
      IDNUM_SSUP(1)=0                                                     97
C GEOMETRY VERSION                                                        97
      SSUP_VERSION=1                                                      97
      CSSUP_VERSION='GEOMETRY VERSION'                                    97
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            98
      SSUP_CABTHK=0.05                                                    98
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         98
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             99
      SSUP_HOSRMN=0.75                                                    99
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          99
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             100
      SSUP_HOSRMX=0.95                                                    100
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          100
C NUMBER OF WATER HOSES                                                   101
      SSUP_NHOSES=10                                                      101
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                101
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             102
      SSUP_WRPMYTHK=0.10                                                  102
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        102
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   103
      SSUP_WRPALTHK=0.01                                                  103
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              103
C SUPPORT CONE THICKNESS                                                  104
      SSUP_GRPHTHK=0.16                                                   104
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              104
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              105
      SSUP_CONE1ZMN=52.23                                                 105
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         105
C LENGTH OF SUPPORT RODS                                                  106
      SSUP_RODLEN=110.8                                                   106
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               106
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               107
      SSUP_RODDIST=17.5                                                   107
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           107
C ID OF CARBON SUPPORT RODS (APPROX)                                      108
      SSUP_RODID=2.5                                                      108
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    108
C OD OF CARBON SUPPORT RODS (APPROX)                                      109
      SSUP_RODOD=3.05                                                     109
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    109
C MINIMUM ID OF CONE 1                                                    110
      SSUP_CON1IDMN=15.67                                                 110
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               110
C MINIMUM ID OF CONE 3 (TBD)                                              111
      SSUP_CON3IDMN=21.67                                                 111
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         111
C MINIMUM ID OF CONE 4 (TBD)                                              112
      SSUP_CON4IDMN=37.4                                                  112
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         112
C MAXIMUM ID OF CONE 4 (TBD)                                              113
      SSUP_CON4IDMX=37.4                                                  113
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         113
C MAXIMUM Z OF CONE 3 (TBD)                                               114
      SSUP_CONE3ZMX=150.0                                                 114
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          114
C MAXIMUM Z OF CONE 4 (TBD)                                               115
      SSUP_CONE4ZMX=229.36                                                115
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          115
C THICKNESS OF AL BRACKETS                                                116
      SSUP_BRATHK=.2                                                      116
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             116
C (Z) THICKNESS OF END RING JOINING BRACKETS                              117
      SSUP_ERJTHK=.1                                                      117
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           117
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          118
      SSUP_ERJWID=2.07                                                    118
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       118
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            119
      SSUP_ERJLEN=5.19                                                    119
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         119
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    120
      SSUP_ERJZDIS=2.0                                                    120
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                120
C ERJ SCREW 1 X POSITION (RADIAL)                                         121
      SSUP_ERJ1X=0.31                                                     121
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       121
C ERJ SCREW 2 X POSITION                                                  122
      SSUP_ERJ2X=1.15                                                     122
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                122
C ERJ SCREW 2 Y POSITION                                                  123
      SSUP_ERJ2Y=0.72                                                     123
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                123
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   124
      SSUP_ERJRAD=10.80                                                   124
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                124
C ERJ SCREW DIAMETER                                                      125
      SSUP_ERJDIA=0.17                                                    125
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   125
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    126
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           126
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   126
C *                                                                       126
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    126
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           126
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   126
C *                                                                       127
      CALL AGSBEGM(MODULE,IPRIN)                                          127
      IF (IPRIN.LT.0) RETURN                                              127
C Filling bank SSUP - SUPPORT STRUCTURES                                  127
      BANK_TITLE='SUPPORT STRUCTURES'                                     127
      BPATH_SSUP='SSUP*'                                                  127
      IDNUM_SSUP(1)=0                                                     128
C GEOMETRY VERSION                                                        128
      SSUP_VERSION=2                                                      128
      CSSUP_VERSION='GEOMETRY VERSION'                                    128
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            129
      SSUP_CABTHK=0.21                                                    129
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         129
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             130
      SSUP_HOSRMN=0.75                                                    130
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          130
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             131
      SSUP_HOSRMX=0.95                                                    131
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          131
C NUMBER OF WATER HOSES                                                   132
      SSUP_NHOSES=10                                                      132
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                132
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             133
      SSUP_WRPMYTHK=0.10                                                  133
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        133
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   134
      SSUP_WRPALTHK=0.01                                                  134
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              134
C SUPPORT CONE THICKNESS                                                  135
      SSUP_GRPHTHK=0.16                                                   135
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              135
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              136
      SSUP_CONE1ZMN=52.23                                                 136
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         136
C LENGTH OF SUPPORT RODS                                                  137
      SSUP_RODLEN=110.8                                                   137
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               137
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               138
      SSUP_RODDIST=17.5                                                   138
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           138
C ID OF CARBON SUPPORT RODS (APPROX)                                      139
      SSUP_RODID=2.5                                                      139
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    139
C OD OF CARBON SUPPORT RODS (APPROX)                                      140
      SSUP_RODOD=3.05                                                     140
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    140
C MINIMUM ID OF CONE 1                                                    141
      SSUP_CON1IDMN=15.67                                                 141
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               141
C MINIMUM ID OF CONE 3 (TBD)                                              142
      SSUP_CON3IDMN=21.67                                                 142
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         142
C MINIMUM ID OF CONE 4 (TBD)                                              143
      SSUP_CON4IDMN=37.4                                                  143
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         143
C MAXIMUM ID OF CONE 4 (TBD)                                              144
      SSUP_CON4IDMX=37.4                                                  144
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         144
C MAXIMUM Z OF CONE 3 (TBD)                                               145
      SSUP_CONE3ZMX=150.0                                                 145
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          145
C MAXIMUM Z OF CONE 4 (TBD)                                               146
      SSUP_CONE4ZMX=229.36                                                146
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          146
C THICKNESS OF AL BRACKETS                                                147
      SSUP_BRATHK=.2                                                      147
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             147
C (Z) THICKNESS OF END RING JOINING BRACKETS                              148
      SSUP_ERJTHK=.1                                                      148
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           148
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          149
      SSUP_ERJWID=2.07                                                    149
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       149
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            150
      SSUP_ERJLEN=5.19                                                    150
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         150
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    151
      SSUP_ERJZDIS=2.0                                                    151
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                151
C ERJ SCREW 1 X POSITION (RADIAL)                                         152
      SSUP_ERJ1X=0.31                                                     152
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       152
C ERJ SCREW 2 X POSITION                                                  153
      SSUP_ERJ2X=1.15                                                     153
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                153
C ERJ SCREW 2 Y POSITION                                                  154
      SSUP_ERJ2Y=0.72                                                     154
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                154
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   155
      SSUP_ERJRAD=10.80                                                   155
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                155
C ERJ SCREW DIAMETER                                                      156
      SSUP_ERJDIA=0.17                                                    156
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   156
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    157
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           157
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   157
C *                                                                       157
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    157
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           157
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   157
C *                                                                       158
      CALL AGSBEGM(MODULE,IPRIN)                                          158
      IF (IPRIN.LT.0) RETURN                                              158
C Filling bank SSUB - BEAMPIPE SUPPORT                                    158
      BANK_TITLE='BEAMPIPE SUPPORT'                                       158
      BPATH_SSUB='SSUB*'                                                  158
      IDNUM_SSUB(1)=0                                                     159
C GEOMETRY VERSION                                                        159
      SSUB_VERSION=1                                                      159
      CSSUB_VERSION='GEOMETRY VERSION'                                    159
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  160
      SSUB_KMOUNTID=31.34                                                 160
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             160
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  161
      SSUB_KMOUNTOD=38.96                                                 161
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             161
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    162
      SSUB_KMNTTHK=0.64                                                   162
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                162
C OD OF CUTOUT IN KINEMATIC MOUNT                                         163
      SSUB_KMCUTOD=18.31                                                  163
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     163
C ID OF CUTOUT IN KINEMATIC MOUNT                                         164
      SSUB_KMCUTID=14                                                     164
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     164
C OPENING ANGLE OF CUTOUT                                                 165
      SSUB_KMCUTOA=38                                                     165
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             165
C OFFSET OF CUTOUT CENTER FROM AXIS                                       166
      SSUB_KMCUTOFF=26.58                                                 166
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  166
C ID OF BEAMPIPE SUPPORT RING                                             167
      SSUB_SRINGID=8.47                                                   167
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         167
C OD OF BEAMPIPE SUPPORT RING                                             168
      SSUB_SRINGOD=12.78                                                  168
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         168
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     169
      SSUB_SRINGTHK=0.64                                                  169
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                169
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     170
      SSUB_SRCUTPHI=38                                                    170
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                170
C SUPPORT RING CUTOUT WIDTH                                               171
      SSUB_SRCUTWID=3.63                                                  171
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          171
C SUPPORT RING CUTOUT DEPTH                                               172
      SSUB_SRCUTOUT=5.08                                                  172
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          172
C SUPPORT RING CUTOUT START                                               173
      SSUB_SRCUTIN=3.5                                                    173
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           173
C SUPPORT ROLLER ID                                                       174
      SSUB_SROLLID=0.2                                                    174
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   174
C SUPPORT ROLLER OD                                                       175
      SSUB_SROLLOD=0.62                                                   175
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   175
C SUPPORT ROLLER LENGTH                                                   176
      SSUB_SROLLLEN=2.54                                                  176
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              176
C SUPPORT ROLLER AXIS LENGTH                                              177
      SSUB_SWIRELEN=5.08                                                  177
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         177
C MOUNTING BLOCK HEIGHT                                                   178
      SSUB_MBLKHGH=0.97                                                   178
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               178
C OUTER MOUNTING BLOCK WIDTH                                              179
      SSUB_MBLKOWID=2.54                                                  179
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         179
C OUTER MOUNTING BLOCK LENGTH                                             180
      SSUB_MBLKOLEN=1.27                                                  180
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        180
C INNER MOUNTING BLOCK WIDTH                                              181
      SSUB_MBLKIWID=3.175                                                 181
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         181
C INNER MOUNTING BLOCK LENGTH                                             182
      SSUB_MBLKILEN=1.27                                                  182
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        182
C OUTER MOUNTING BLOCK AT RADIUS                                          183
      SSUB_MBLKORAD=17.4                                                  183
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     183
C INNER MOUNTING BLOCK AT RADIUS                                          184
      SSUB_MBLKIRAD=5.42                                                  184
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     184
C MOUNTING SUPPORT ROD DIAMETER                                           185
      SSUB_MRODDIA=0.32                                                   185
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       185
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    186
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           186
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   186
C *                                                                       186
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    186
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           186
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   186
C *                                                                       187
      CALL AGSBEGM(MODULE,IPRIN)                                          187
      IF (IPRIN.LT.0) RETURN                                              187
C Filling bank SERG - END RINGS                                           187
      BANK_TITLE='END RINGS'                                              187
      BPATH_SERG='SERG*'                                                  187
      IDNUM_SERG(1)=0                                                     188
C GEOMETRY VERSION                                                        188
      SERG_VERSION=1                                                      188
      CSERG_VERSION='GEOMETRY VERSION'                                    188
C INNER END RING TUBE MAXIMUM RADIUS                                      189
      SERG_IRNGTRMX=9.703                                                 189
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 189
C INNER END RING POLYGON MINIMUM RADIUS                                   190
      SERG_IRNGPRMN=7.671                                                 190
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              190
C OUTER END RING MINIMUM RADIUS                                           191
      SERG_ORNGRMIN=11.900                                                191
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      191
C OUTER END RING MAXIMUM RADIUS                                           192
      SERG_ORNGRMAX=13.805                                                192
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      192
C END RING THICKNESS                                                      193
      SERG_ENDRNGTH=0.2                                                   193
      CSERG_ENDRNGTH='END RING THICKNESS'                                 193
C MINIMUM Z FOR END RINGS                                                 194
      SERG_ENDRNGZM=23.01                                                 194
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            194
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    195
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           195
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   195
C *                                                                       195
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    195
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           195
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   195
C *                                                                       196
      CALL AGSBEGM(MODULE,IPRIN)                                          196
      IF (IPRIN.LT.0) RETURN                                              196
C Filling bank SWAM - WATER MANIFOLD                                      196
      BANK_TITLE='WATER MANIFOLD'                                         196
      BPATH_SWAM='SWAM*'                                                  196
      IDNUM_SWAM(1)=0                                                     197
C GEOMETRY VERSION                                                        197
      SWAM_VERSION=1                                                      197
      CSWAM_VERSION='GEOMETRY VERSION'                                    197
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                198
      SWAM_ZMIN=33.9                                                      198
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               198
C WATER MANIFOLD FULL LENGTH                                              199
      SWAM_LEN=16.0                                                       199
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              199
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         200
      SWAM_RMIN=15.24                                                     200
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        200
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         201
      SWAM_RMAX=16.83                                                     201
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        201
C TRANSITION BOARD THICKNESS                                              202
      SWAM_TBRDTHK=0.1                                                    202
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          202
C WATER MANIFOLD WALL THICKNESS                                           203
      SWAM_WALLTHK=0.1                                                    203
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       203
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    204
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           204
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   204
C *                                                                       204
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    204
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           204
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   204
C *                                                                       205
      CALL AGSBEGM(MODULE,IPRIN)                                          205
      IF (IPRIN.LT.0) RETURN                                              205
C Filling bank SELC - ELECTRONICS CARRIER DATA                            205
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               205
      BPATH_SELC='SELC*'                                                  205
      IDNUM_SELC(1)=0                                                     206
C GEOMETRY VERSION                                                        206
      SELC_VERSION=1                                                      206
      CSELC_VERSION='GEOMETRY VERSION'                                    206
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              207
      SELC_BETHK=0.0500                                                   207
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            207
C THICKNESS OF WATER CHANNEL                                              208
      SELC_WATTHK=0.0750                                                  208
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           208
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    209
      SELC_BEOTHK=0.0500                                                  209
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 209
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                210
      SELC_DYETHK=0.0340                                                  210
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             210
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                211
      SELC_DYEWID=0.3                                                     211
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             211
C SPACE IC CHIPS                                                          212
      SELC_DYESPC=0.1                                                     212
      CSELC_DYESPC='SPACE IC CHIPS'                                       212
C ELECTRONICS CARRIER WIDTH                                               213
      SELC_ELCAWID=2.0                                                    213
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           213
C THICKNESS OF AG-PD CONDUCTOR                                            214
      SELC_AGPDTHK=0.0030                                                 214
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        214
C THICKNESS OF GLASS INSULATOR                                            215
      SELC_GLASSTHK=0.0150                                                215
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       215
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        216
      SELC_CABTHK=0.0033                                                  216
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     216
C CABLE WIDTH                                                             217
      SELC_CABWID=0.6                                                     217
      CSELC_CABWID='CABLE WIDTH'                                          217
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    218
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           218
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   218
C *                                                                       218
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    218
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           218
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   218
C *                                                                       219
      CALL AGSBEGM(MODULE,IPRIN)                                          219
      IF (IPRIN.LT.0) RETURN                                              219
C Filling bank SVTL - SINGLE LAYER DATA                                   219
      BANK_TITLE='SINGLE LAYER DATA'                                      219
      BPATH_SVTL='SVTL*'                                                  219
      IDNUM_SVTL(1)=0                                                     220
C LAYER NUMBER                                                            220
      SVTL_LAYER=1                                                        220
      CSVTL_LAYER='LAYER NUMBER'                                          220
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 221
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     221
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              221
C NUMBER OF LADDER                                                        222
      SVTL_NLADDER=4                                                      222
      CSVTL_NLADDER='NUMBER OF LADDER'                                    222
C NUMBER OF WAFERS                                                        223
      SVTL_NWAFER=4                                                       223
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     223
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        224
      SVTL_BAREEDGE=1.0                                                   224
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   224
C PCB LENGTH                                                              225
      SVTL_PCBLEN=14.9                                                    225
      CSVTL_PCBLEN='PCB LENGTH'                                           225
C PCB WIDTH                                                               226
      SVTL_PCBWIDTH=6.3                                                   226
      CSVTL_PCBWIDTH='PCB WIDTH'                                          226
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            227
      SVTL_PCBTHK=0.1                                                     227
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  227
     *R COPPER'                                                           227
C GAP BETWEEN THE PCB AND THE SI WAFER                                    228
      SVTL_PCBGAP=0.2                                                     228
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 228
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    229
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           229
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   229
C *                                                                       229
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    229
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           229
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   229
C *                                                                       230
      CALL AGSBEGM(MODULE,IPRIN)                                          230
      IF (IPRIN.LT.0) RETURN                                              230
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             230
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                230
      BPATH_SVTL='SVTL*'                                                  230
      IDNUM_SVTL(1)=0                                                     231
C LAYER NUMBER                                                            231
      SVTL_LAYER=2                                                        231
      CSVTL_LAYER='LAYER NUMBER'                                          231
C LAYER RADIUS                                                            232
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     232
      CSVTL_RADIUS='LAYER RADIUS'                                         232
C PCB LENGTH                                                              233
      SVTL_PCBLEN=14.9                                                    233
      CSVTL_PCBLEN='PCB LENGTH'                                           233
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    234
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           234
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   234
C *                                                                       234
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    234
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           234
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   234
C *                                                                       235
      CALL AGSBEGM(MODULE,IPRIN)                                          235
      IF (IPRIN.LT.0) RETURN                                              235
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             235
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                235
      BPATH_SVTL='SVTL*'                                                  235
      IDNUM_SVTL(1)=0                                                     236
C LAYER NUMBER                                                            236
      SVTL_LAYER=3                                                        236
      CSVTL_LAYER='LAYER NUMBER'                                          236
C LAYER RADIUS                                                            237
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     237
      CSVTL_RADIUS='LAYER RADIUS'                                         237
C NUMBER OF LADDER                                                        238
      SVTL_NLADDER=6                                                      238
      CSVTL_NLADDER='NUMBER OF LADDER'                                    238
C NUMBER OF WAFERS                                                        239
      SVTL_NWAFER=6                                                       239
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     239
C PCB LENGTH                                                              240
      SVTL_PCBLEN=7.5                                                     240
      CSVTL_PCBLEN='PCB LENGTH'                                           240
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    241
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           241
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   241
C *                                                                       241
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    241
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           241
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   241
C *                                                                       242
      CALL AGSBEGM(MODULE,IPRIN)                                          242
      IF (IPRIN.LT.0) RETURN                                              242
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             242
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                242
      BPATH_SVTL='SVTL*'                                                  242
      IDNUM_SVTL(1)=0                                                     243
C LAYER NUMBER                                                            243
      SVTL_LAYER=4                                                        243
      CSVTL_LAYER='LAYER NUMBER'                                          243
C LAYER RADIUS                                                            244
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     244
      CSVTL_RADIUS='LAYER RADIUS'                                         244
C PCB LENGTH                                                              245
      SVTL_PCBLEN=7.5                                                     245
      CSVTL_PCBLEN='PCB LENGTH'                                           245
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    246
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           246
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   246
C *                                                                       246
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    246
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           246
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   246
C *                                                                       247
      CALL AGSBEGM(MODULE,IPRIN)                                          247
      IF (IPRIN.LT.0) RETURN                                              247
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             247
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                247
      BPATH_SVTL='SVTL*'                                                  247
      IDNUM_SVTL(1)=0                                                     248
C LAYER NUMBER                                                            248
      SVTL_LAYER=5                                                        248
      CSVTL_LAYER='LAYER NUMBER'                                          248
C LAYER RADIUS                                                            249
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     249
      CSVTL_RADIUS='LAYER RADIUS'                                         249
C NUMBER OF LADDER                                                        250
      SVTL_NLADDER=8                                                      250
      CSVTL_NLADDER='NUMBER OF LADDER'                                    250
C NUMBER OF WAFERS                                                        251
      SVTL_NWAFER=7                                                       251
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     251
C PCB LENGTH                                                              252
      SVTL_PCBLEN=4.4                                                     252
      CSVTL_PCBLEN='PCB LENGTH'                                           252
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    253
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           253
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   253
C *                                                                       253
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    253
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           253
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   253
C *                                                                       254
      CALL AGSBEGM(MODULE,IPRIN)                                          254
      IF (IPRIN.LT.0) RETURN                                              254
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             254
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                254
      BPATH_SVTL='SVTL*'                                                  254
      IDNUM_SVTL(1)=0                                                     255
C LAYER NUMBER                                                            255
      SVTL_LAYER=6                                                        255
      CSVTL_LAYER='LAYER NUMBER'                                          255
C LAYER RADIUS                                                            256
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     256
      CSVTL_RADIUS='LAYER RADIUS'                                         256
C PCB LENGTH                                                              257
      SVTL_PCBLEN=4.4                                                     257
      CSVTL_PCBLEN='PCB LENGTH'                                           257
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    258
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           258
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   258
C *                                                                       258
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    258
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           258
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   258
C *                                                                       259
      CALL AGSBEGM(MODULE,IPRIN)                                          259
      IF (IPRIN.LT.0) RETURN                                              259
C Filling bank SSLD - SHIELDING PARAMETERS                                259
      BANK_TITLE='SHIELDING PARAMETERS'                                   259
      BPATH_SSLD='SSLD*'                                                  259
      IDNUM_SSLD(1)=0                                                     260
C GEOMETRY VERSION                                                        260
      SSLD_VERSION=1                                                      260
      CSSLD_VERSION='GEOMETRY VERSION'                                    260
C INNER SHIELD CYLINDER, INNER RADIUS                                     261
      SSLD_SINRINN=5                                                      261
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 261
C INNER SHIELD CYLINDER, OUTER RADIUS                                     262
      SSLD_SINROUT=5.008                                                  262
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 262
C INNER SHIELD CYLINDER, HALF LENGTH                                      263
      SSLD_SINLEN=53.5                                                    263
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   263
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                264
      SSLD_SSEPRINN=22                                                    264
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           264
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                265
      SSLD_SSEPROUT=22.018                                                265
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           265
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 266
      SSLD_SSEPLEN=55.4                                                   266
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             266
C OUTER SHIELD CYLINDER, INNER RADIUS                                     267
      SSLD_SOUTRINN=29.5                                                  267
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                267
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     268
      SSLD_SOUTROUT=29.52                                                 268
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                268
C OUTER SHIELD CYLINDER, HALF LENGTH                                      269
      SSLD_SOUTLEN=65.4                                                   269
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  269
C ALUMINUM SHIELD MESH INNER DIAMETER                                     270
      SSLD_ALMESHID=9.7                                                   270
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                270
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     271
      SSLD_ALMESHOD=44                                                    271
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                271
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                272
      SSLD_ALMSHTHK=0.03                                                  272
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           272
C ALUMINUM SHIELD MESH Z POSITION                                         273
      SSLD_ALMSHPOS=53.5                                                  273
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    273
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    274
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           274
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   274
C *                                                                       274
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    274
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           274
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   274
C *                                                                       275
      CALL AGSBEGM(MODULE,IPRIN)                                          275
      IF (IPRIN.LT.0) RETURN                                              275
C Filling bank SSLD - SHIELDING PARAMETERS                                275
      BANK_TITLE='SHIELDING PARAMETERS'                                   275
      BPATH_SSLD='SSLD*'                                                  275
      IDNUM_SSLD(1)=0                                                     276
C GEOMETRY VERSION                                                        276
      SSLD_VERSION=2                                                      276
      CSSLD_VERSION='GEOMETRY VERSION'                                    276
C INNER SHIELD CYLINDER, INNER RADIUS                                     277
      SSLD_SINRINN=5.9                                                    277
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 277
C INNER SHIELD CYLINDER, OUTER RADIUS                                     278
      SSLD_SINROUT=5.908                                                  278
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 278
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    279
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           279
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   279
C *                                                                       279
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    279
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           279
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   279
C *                                                                       280
      CALL AGSBEGM(MODULE,IPRIN)                                          280
      IF (IPRIN.LT.0) RETURN                                              280
C Filling bank SSLD - SHIELDING PARAMETERS                                280
      BANK_TITLE='SHIELDING PARAMETERS'                                   280
      BPATH_SSLD='SSLD*'                                                  280
      IDNUM_SSLD(1)=0                                                     281
C GEOMETRY VERSION                                                        281
      SSLD_VERSION=3                                                      281
      CSSLD_VERSION='GEOMETRY VERSION'                                    281
C INNER SHIELD CYLINDER, INNER RADIUS                                     282
      SSLD_SINRINN=5                                                      282
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 282
C INNER SHIELD CYLINDER, OUTER RADIUS                                     283
      SSLD_SINROUT=5.008                                                  283
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 283
C INNER SHIELD CYLINDER, HALF LENGTH                                      284
      SSLD_SINLEN=53.5                                                    284
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   284
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                285
      SSLD_SSEPRINN=22                                                    285
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           285
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                286
      SSLD_SSEPROUT=22.018                                                286
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           286
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 287
      SSLD_SSEPLEN=55.4                                                   287
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             287
C OUTER SHIELD CYLINDER, INNER RADIUS                                     288
      SSLD_SOUTRINN=31.80                                                 288
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                288
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     289
      SSLD_SOUTROUT=31.82                                                 289
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                289
C OUTER SHIELD CYLINDER, HALF LENGTH                                      290
      SSLD_SOUTLEN=65.4                                                   290
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  290
C ALUMINUM SHIELD MESH INNER DIAMETER                                     291
      SSLD_ALMESHID=9.7                                                   291
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                291
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     292
      SSLD_ALMESHOD=44                                                    292
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                292
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                293
      SSLD_ALMSHTHK=0.03                                                  293
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           293
C ALUMINUM SHIELD MESH Z POSITION                                         294
      SSLD_ALMSHPOS=53.5                                                  294
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    294
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    295
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           295
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   295
C *                                                                       295
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    295
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           295
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   295
C *                                                                       296
      DO 5011 I=1,4                                                       296
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 297
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 298
5011  CONTINUE                                                            299
5012  CONTINUE                                                            299
C *                                                                       300
      CALL AGSBEGM(MODULE,IPRIN)                                          300
      IF (IPRIN.LT.0) RETURN                                              300
C Filling bank SCBP - CABLING                                             300
      BANK_TITLE='CABLING'                                                300
      BPATH_SCBP='SCBP*'                                                  300
      IDNUM_SCBP(1)=0                                                     301
C LAYER                                                                   301
      SCBP_LAYER=1                                                        301
      CSCBP_LAYER='LAYER'                                                 301
C LENGTH                                                                  302
      SCBP_LEN=1.85                                                       302
      CSCBP_LEN='LENGTH'                                                  302
C MIN RADIUS CLOSER TO WAFERS                                             303
      SCBP_RMIN1=RAD_CONES_IN(1)                                          303
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           303
C MIN RADIUS FURTHER FROM WAFERS                                          304
      SCBP_RMIN2=RAD_CONES_OUT(1)                                         304
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        304
C VOLUME OF COPPER, LV+HV CABLES                                          305
      SCBP_VOL=7.24+3.21                                                  305
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          305
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    306
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           306
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   306
C *                                                                       306
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    306
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           306
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   306
C *                                                                       307
      CALL AGSBEGM(MODULE,IPRIN)                                          307
      IF (IPRIN.LT.0) RETURN                                              307
C Filling bank SCBP - CABLING                                             307
      BANK_TITLE='CABLING'                                                307
      BPATH_SCBP='SCBP*'                                                  307
      IDNUM_SCBP(1)=0                                                     308
C LAYER                                                                   308
      SCBP_LAYER=2                                                        308
      CSCBP_LAYER='LAYER'                                                 308
C MIN RADIUS CLOSER TO WAFERS                                             309
      SCBP_RMIN1=RAD_CONES_IN(2)                                          309
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           309
C MIN RADIUS FURTHER FROM WAFERS                                          310
      SCBP_RMIN2=RAD_CONES_OUT(2)                                         310
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        310
C VOLUME OF COPPER, LV+HV CABLES                                          311
      SCBP_VOL=15.54+5.7                                                  311
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          311
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    312
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           312
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   312
C *                                                                       312
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    312
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           312
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   312
C *                                                                       313
      CALL AGSBEGM(MODULE,IPRIN)                                          313
      IF (IPRIN.LT.0) RETURN                                              313
C Filling bank SCBP - CABLING                                             313
      BANK_TITLE='CABLING'                                                313
      BPATH_SCBP='SCBP*'                                                  313
      IDNUM_SCBP(1)=0                                                     314
C LAYER                                                                   314
      SCBP_LAYER=3                                                        314
      CSCBP_LAYER='LAYER'                                                 314
C MIN RADIUS CLOSER TO WAFERS                                             315
      SCBP_RMIN1=RAD_CONES_IN(3)                                          315
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           315
C MIN RADIUS FURTHER FROM WAFERS                                          316
      SCBP_RMIN2=RAD_CONES_OUT(3)                                         316
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        316
C VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE                   317
      SCBP_VOL=4.05+2.02+3.67+1.69                                        317
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE'   317
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    318
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           318
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   318
C *                                                                       318
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    318
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           318
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   318
C *                                                                       319
      CALL AGSBEGM(MODULE,IPRIN)                                          319
      IF (IPRIN.LT.0) RETURN                                              319
C Filling bank SCBP - CABLING                                             319
      BANK_TITLE='CABLING'                                                319
      BPATH_SCBP='SCBP*'                                                  319
      IDNUM_SCBP(1)=0                                                     320
C LAYER (FORMER 5TH)                                                      320
      SCBP_LAYER=4                                                        320
      CSCBP_LAYER='LAYER (FORMER 5TH)'                                    320
C MIN RADIUS CLOSER TO WAFERS                                             321
      SCBP_RMIN1=RAD_CONES_IN(4)                                          321
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           321
C MIN RADIUS FURTHER FROM WAFERS                                          322
      SCBP_RMIN2=RAD_CONES_OUT(4)                                         322
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        322
C VOLUME OF COPPER, LV+HV CABLES                                          323
      SCBP_VOL=6.95+2.43                                                  323
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          323
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    324
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           324
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   324
C *                                                                       324
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    324
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           324
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   324
C *                                                                       325
      CALL AGSBEGM(MODULE,IPRIN)                                          325
      IF (IPRIN.LT.0) RETURN                                              325
C Filling bank SFEP - WATER FEED                                          325
      BANK_TITLE='WATER FEED'                                             325
      BPATH_SFEP='SFEP*'                                                  325
      IDNUM_SFEP(1)=0                                                     326
C LAYER                                                                   326
      SFEP_LAYER=1                                                        326
      CSFEP_LAYER='LAYER'                                                 326
C LENGTH                                                                  327
      SFEP_LEN=1.85                                                       327
      CSFEP_LEN='LENGTH'                                                  327
C MIN RADIUS CLOSER TO WAFERS                                             328
      SFEP_RMIN1=0.5*(RAD_CONES_IN(1) +RAD_CONES_IN(2))                   328
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           328
C MIN RADIUS FURTHER FROM WAFERS                                          329
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(1)+RAD_CONES_OUT(2))                  329
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        329
C VOLUME OF WATER                                                         330
      SFEP_VOL=16.0                                                       330
      CSFEP_VOL='VOLUME OF WATER'                                         330
C VOLUME OF PLASTIC                                                       331
      SFEP_VOLPLAST=38.4                                                  331
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  331
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    332
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           332
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   332
C *                                                                       332
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    332
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           332
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   332
C *                                                                       333
      CALL AGSBEGM(MODULE,IPRIN)                                          333
      IF (IPRIN.LT.0) RETURN                                              333
C Filling bank SFEP - WATER FEED                                          333
      BANK_TITLE='WATER FEED'                                             333
      BPATH_SFEP='SFEP*'                                                  333
      IDNUM_SFEP(1)=0                                                     334
C LAYER                                                                   334
      SFEP_LAYER=2                                                        334
      CSFEP_LAYER='LAYER'                                                 334
C MIN RADIUS CLOSER TO WAFERS                                             335
      SFEP_RMIN1=0.5*(RAD_CONES_IN(2) +RAD_CONES_IN(3))                   335
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           335
C MIN RADIUS FURTHER FROM WAFERS                                          336
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(2)+RAD_CONES_OUT(3))                  336
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        336
C VOLUME OF WATER                                                         337
      SFEP_VOL=24.0                                                       337
      CSFEP_VOL='VOLUME OF WATER'                                         337
C VOLUME OF PLASTIC                                                       338
      SFEP_VOLPLAST=57.6                                                  338
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  338
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    339
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           339
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   339
C *                                                                       339
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    339
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           339
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   339
C *                                                                       340
      CALL AGSBEGM(MODULE,IPRIN)                                          340
      IF (IPRIN.LT.0) RETURN                                              340
C Filling bank SFEP - WATER FEED                                          340
      BANK_TITLE='WATER FEED'                                             340
      BPATH_SFEP='SFEP*'                                                  340
      IDNUM_SFEP(1)=0                                                     341
C LAYER                                                                   341
      SFEP_LAYER=3                                                        341
      CSFEP_LAYER='LAYER'                                                 341
C MIN RADIUS CLOSER TO WAFERS                                             342
      SFEP_RMIN1=0.5*(RAD_CONES_IN(3) +RAD_CONES_IN(4))                   342
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           342
C MIN RADIUS FURTHER FROM WAFERS                                          343
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(3)+RAD_CONES_OUT(4))                  343
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        343
C VOLUME OF WATER                                                         344
      SFEP_VOL=32                                                         344
      CSFEP_VOL='VOLUME OF WATER'                                         344
C VOLUME OF PLASTIC                                                       345
      SFEP_VOLPLAST=76.8                                                  345
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  345
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    346
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           346
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   346
C *                                                                       346
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    346
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           346
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   346
C *                                                                       347
      CALL AGSBEGM(MODULE,IPRIN)                                          347
      IF (IPRIN.LT.0) RETURN                                              347
C Filling bank SWCX - SEGMENTS OF THE WATER DISTRIBUTION PIPES            347
      BANK_TITLE='SEGMENTS OF THE WATER DISTRIBUTION PIPES'               347
      BPATH_SWCX='SWCX*'                                                  347
      IDNUM_SWCX(1)=0                                                     348
C VERSION                                                                 348
      SWCX_LAYER=1                                                        348
      CSWCX_LAYER='VERSION'                                               348
C OF THE RING IN THE Z DIRECTION                                          349
      SWCX_LENGTH=2.8                                                     349
      CSWCX_LENGTH='OF THE RING IN THE Z DIRECTION'                       349
C THICKNESS OF THE MOTHER LAYER                                           350
      SWCX_DR=0.72                                                        350
      CSWCX_DR='THICKNESS OF THE MOTHER LAYER'                            350
C INNER PLASTIC TUBE RADIUS                                               351
      SWCX_RAD=0.2                                                        351
      CSWCX_RAD='INNER PLASTIC TUBE RADIUS'                               351
C FROM THE EDGE OF THE LADDER SUPPORT, INWARD                             352
      SWCX_OFFSET=-2.0                                                    352
      CSWCX_OFFSET='FROM THE EDGE OF THE LADDER SUPPORT, INWARD'          352
C RADIAL OFFSET                                                           353
      SWCX_ROFFSET=1.0                                                    353
      CSWCX_ROFFSET='RADIAL OFFSET'                                       353
C THICKNESS OF THE PLASTIC PIPE WALL                                      354
      SWCX_WALL=0.16                                                      354
      CSWCX_WALL='THICKNESS OF THE PLASTIC PIPE WALL'                     354
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    355
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           355
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   355
C *                                                                       355
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    355
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           355
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   355
C *                                                                       356
      CALL AGSBEGM(MODULE,IPRIN)                                          356
      IF (IPRIN.LT.0) RETURN                                              356
C Filling bank SOUP - MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS      356
      BANK_TITLE='MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS'         356
      BPATH_SOUP='SOUP*'                                                  356
      IDNUM_SOUP(1)=0                                                     357
C VERSION                                                                 357
      SOUP_VERSION=1                                                      357
      CSOUP_VERSION='VERSION'                                             357
C LENGTH                                                                  358
      SOUP_LENGTH=82.5                                                    358
      CSOUP_LENGTH='LENGTH'                                               358
C OUTER RADIUS OF THE SHIELD                                              359
      SOUP_ROUT=19.41                                                     359
      CSOUP_ROUT='OUTER RADIUS OF THE SHIELD'                             359
C DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF THE MOTHER)     360
      SOUP_DR=0.711                                                       360
      CSOUP_DR='DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF   360
     *THE MOTHER)'                                                        360
C STARTING ANGLE OF THE MOTHER                                            361
      SOUP_PHI1=0.0                                                       361
      CSOUP_PHI1='STARTING ANGLE OF THE MOTHER'                           361
C FINISHING ANGLE OF THE MOTHER                                           362
      SOUP_PHI2=70.0                                                      362
      CSOUP_PHI2='FINISHING ANGLE OF THE MOTHER'                          362
C OUTER DIAM OF THE CARBON TUBE                                           363
      SOUP_DIAMOUT=0.711                                                  363
      CSOUP_DIAMOUT='OUTER DIAM OF THE CARBON TUBE'                       363
C INNER DIAM OF THE CARBON TUBE                                           364
      SOUP_DIAMIN=0.620                                                   364
      CSOUP_DIAMIN='INNER DIAM OF THE CARBON TUBE'                        364
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    365
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           365
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   365
C *                                                                       365
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    365
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           365
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   365
C use default SVTG bank                                                   366
      BPATH_SVTG='SVTG*'                                                  366
      IDNUM_SVTG(1)=0                                                     366
      CALL AGDGETP('SVTTGEO10','*',-999,'SVTG',' ',BANK_SVTG,             366
     *BPATH_SVTG,IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,     366
     *LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                                      366
C use default SWCA bank                                                   367
      BPATH_SWCA='SWCA*'                                                  367
      IDNUM_SWCA(1)=0                                                     367
      CALL AGDGETP('SVTTGEO10','*',-999,'SWCA',' ',BANK_SWCA,             367
     *BPATH_SWCA,IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,     367
     *LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                                      367
C use default SELC bank                                                   368
      BPATH_SELC='SELC*'                                                  368
      IDNUM_SELC(1)=0                                                     368
      CALL AGDGETP('SVTTGEO10','*',-999,'SELC',' ',BANK_SELC,             368
     *BPATH_SELC,IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,     368
     *LLEN_SELC,LINK_SELC,FLAG_SELC)                                      368
C use SSUP bank                                                           369
      BPATH_SSUP='SSUP*'                                                  369
      IDNUM_SSUP(1)=0                                                     369
      SSUP_VERSION=SVTG_CONEVER                                           369
      CALL AGDGETP('SVTTGEO10','*',-999,'SSUP',NSSUP_VERSION,             369
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        369
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   369
C use default SSUB bank                                                   370
      BPATH_SSUB='SSUB*'                                                  370
      IDNUM_SSUB(1)=0                                                     370
      CALL AGDGETP('SVTTGEO10','*',-999,'SSUB',' ',BANK_SSUB,             370
     *BPATH_SSUB,IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,     370
     *LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                                      370
C use default SERG bank                                                   371
      BPATH_SERG='SERG*'                                                  371
      IDNUM_SERG(1)=0                                                     371
      CALL AGDGETP('SVTTGEO10','*',-999,'SERG',' ',BANK_SERG,             371
     *BPATH_SERG,IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,     371
     *LLEN_SERG,LINK_SERG,FLAG_SERG)                                      371
C use default SWAM bank                                                   372
      BPATH_SWAM='SWAM*'                                                  372
      IDNUM_SWAM(1)=0                                                     372
      CALL AGDGETP('SVTTGEO10','*',-999,'SWAM',' ',BANK_SWAM,             372
     *BPATH_SWAM,IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,     372
     *LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                                      372
C use default SELC bank                                                   373
      BPATH_SELC='SELC*'                                                  373
      IDNUM_SELC(1)=0                                                     373
      CALL AGDGETP('SVTTGEO10','*',-999,'SELC',' ',BANK_SELC,             373
     *BPATH_SELC,IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,     373
     *LLEN_SELC,LINK_SELC,FLAG_SELC)                                      373
C use SSLD bank                                                           374
      BPATH_SSLD='SSLD*'                                                  374
      IDNUM_SSLD(1)=0                                                     374
      SSLD_VERSION=SVTG_SUPPORTVER                                        374
      CALL AGDGETP('SVTTGEO10','*',-999,'SSLD',NSSLD_VERSION,             374
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        374
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   374
C use default SCBP bank                                                   375
      BPATH_SCBP='SCBP*'                                                  375
      IDNUM_SCBP(1)=0                                                     375
      CALL AGDGETP('SVTTGEO10','*',-999,'SCBP',' ',BANK_SCBP,             375
     *BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,     375
     *LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                                      375
C use default SWCX bank                                                   376
      BPATH_SWCX='SWCX*'                                                  376
      IDNUM_SWCX(1)=0                                                     376
      CALL AGDGETP('SVTTGEO10','*',-999,'SWCX',' ',BANK_SWCX,             376
     *BPATH_SWCX,IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,     376
     *LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                                      376
C use default SOUP bank                                                   377
      BPATH_SOUP='SOUP*'                                                  377
      IDNUM_SOUP(1)=0                                                     377
      CALL AGDGETP('SVTTGEO10','*',-999,'SOUP',' ',BANK_SOUP,             377
     *BPATH_SOUP,IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,     377
     *LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                                      377
C Check SVTG_SUPPORTVER==2                                                378
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      378
      SVTG_RSIZEMIN = 6.0                                                 379
      ENDIF                                                               380
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             381
      CALL AGSRESET                                                       381
      AG_A = 28.08                                                        381
      AG_Z = 14                                                           381
      AG_W = 0.6*1*28./60.                                                381
      AG_TITLE = 'SI'                                                     381
      AG_EXNAME = 'COMPONENT'                                             381
      AG_PARLIST = 'A_Z_W'                                                381
      CALL AXCOMPONENT                                                    381
C *                                                                       381
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  381
C =                                                                       381
      CALL AGSRESET                                                       382
      AG_A = 16                                                           382
      AG_Z = 8                                                            382
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               382
      AG_TITLE = 'O'                                                      382
      AG_EXNAME = 'COMPONENT'                                             382
      AG_PARLIST = 'A_Z_W'                                                382
      CALL AXCOMPONENT                                                    382
C *                                                                       382
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 382
      CALL AGSRESET                                                       383
      AG_A = 12                                                           383
      AG_Z = 6                                                            383
      AG_W = 0.4*8*12./174.                                               383
      AG_TITLE = 'C'                                                      383
      AG_EXNAME = 'COMPONENT'                                             383
      AG_PARLIST = 'A_Z_W'                                                383
      CALL AXCOMPONENT                                                    383
C *                                                                       383
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  383
      CALL AGSRESET                                                       384
      AG_A = 1                                                            384
      AG_Z = 1                                                            384
      AG_W = 0.4*14*1./174.                                               384
      AG_TITLE = 'H'                                                      384
      AG_EXNAME = 'COMPONENT'                                             384
      AG_PARLIST = 'A_Z_W'                                                384
      CALL AXCOMPONENT                                                    384
C *                                                                       384
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  384
      CALL AGSRESET                                                       385
      AG_DENS = 1.7                                                       385
      AG_TITLE = 'G10'                                                    385
      AG_EXNAME = 'MIXTURE'                                               385
      AG_PARLIST = 'DENS'                                                 385
      CALL AXMIXTURE                                                      385
C *                                                                       385
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       385
      CALL AGSRESET                                                       386
      AG_A = 12                                                           386
      AG_Z = 6                                                            386
      AG_W = 10000*5                                                      386
      AG_TITLE = 'C5'                                                     386
      AG_EXNAME = 'COMPONENT'                                             386
      AG_PARLIST = 'A_Z_W'                                                386
      CALL AXCOMPONENT                                                    386
C *                                                                       386
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        386
      CALL AGSRESET                                                       387
      AG_A = 1                                                            387
      AG_Z = 1                                                            387
      AG_W = 10000*4                                                      387
      AG_TITLE = 'H4'                                                     387
      AG_EXNAME = 'COMPONENT'                                             387
      AG_PARLIST = 'A_Z_W'                                                387
      CALL AXCOMPONENT                                                    387
C *                                                                       387
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       387
      CALL AGSRESET                                                       388
      AG_A = 16                                                           388
      AG_Z = 8                                                            388
      AG_W = 10000*2                                                      388
      AG_TITLE = 'O2'                                                     388
      AG_EXNAME = 'COMPONENT'                                             388
      AG_PARLIST = 'A_Z_W'                                                388
      CALL AXCOMPONENT                                                    388
C *                                                                       388
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 388
      CALL AGSRESET                                                       389
      AG_A = 27                                                           389
      AG_Z = 13                                                           389
      AG_W = 10000*0.2302                                                 389
      AG_TITLE = 'AL'                                                     389
      AG_EXNAME = 'COMPONENT'                                             389
      AG_PARLIST = 'A_Z_W'                                                389
      CALL AXCOMPONENT                                                    389
C *                                                                       389
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              389
      CALL AGSRESET                                                       390
      AG_DENS = 1.432                                                     390
      AG_TITLE = 'ALKAP'                                                  390
      AG_EXNAME = 'MIXTURE'                                               390
      AG_PARLIST = 'DENS'                                                 390
      CALL AXMIXTURE                                                      390
C *                                                                       390
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       390
      CALL AGSRESET                                                       391
      AG_A = 12                                                           391
      AG_Z = 6                                                            391
      AG_W = 10000*5                                                      391
      AG_TITLE = 'C5'                                                     391
      AG_EXNAME = 'COMPONENT'                                             391
      AG_PARLIST = 'A_Z_W'                                                391
      CALL AXCOMPONENT                                                    391
C *                                                                       391
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        391
      CALL AGSRESET                                                       392
      AG_A = 1                                                            392
      AG_Z = 1                                                            392
      AG_W = 10000*4                                                      392
      AG_TITLE = 'H4'                                                     392
      AG_EXNAME = 'COMPONENT'                                             392
      AG_PARLIST = 'A_Z_W'                                                392
      CALL AXCOMPONENT                                                    392
C *                                                                       392
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       392
      CALL AGSRESET                                                       393
      AG_A = 16                                                           393
      AG_Z = 8                                                            393
      AG_W = 10000*2                                                      393
      AG_TITLE = 'O2'                                                     393
      AG_EXNAME = 'COMPONENT'                                             393
      AG_PARLIST = 'A_Z_W'                                                393
      CALL AXCOMPONENT                                                    393
C *                                                                       393
C Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                          393
      CALL AGSRESET                                                       394
      AG_A = 27                                                           394
      AG_Z = 13                                                           394
      AG_W = 986                                                          394
      AG_TITLE = 'AL'                                                     394
      AG_EXNAME = 'COMPONENT'                                             394
      AG_PARLIST = 'A_Z_W'                                                394
      CALL AXCOMPONENT                                                    394
C *                                                                       394
C Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                          394
      CALL AGSRESET                                                       395
      AG_DENS = 1.40845                                                   395
      AG_TITLE = 'SSDALMY'                                                395
      AG_EXNAME = 'MIXTURE'                                               395
      AG_PARLIST = 'DENS'                                                 395
      CALL AXMIXTURE                                                      395
C *                                                                       395
C Check SWAM_LEN>0                                                        395
      IF (SWAM_LEN.GT.0) THEN                                             396
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              397
      CALL AGSRESET                                                       397
      AG_A = 1                                                            397
      AG_Z = 1                                                            397
      AG_W = 2                                                            397
      AG_TITLE = 'H2'                                                     397
      AG_EXNAME = 'COMPONENT'                                             397
      AG_PARLIST = 'A_Z_W'                                                397
      CALL AXCOMPONENT                                                    397
C *                                                                       397
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              397
      CALL AGSRESET                                                       398
      AG_A = 16                                                           398
      AG_Z = 8                                                            398
      AG_W = 1                                                            398
      AG_TITLE = 'O'                                                      398
      AG_EXNAME = 'COMPONENT'                                             398
      AG_PARLIST = 'A_Z_W'                                                398
      CALL AXCOMPONENT                                                    398
C *                                                                       398
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                398
      CALL AGSRESET                                                       399
      AG_DENS = 1.0                                                       399
      AG_TITLE = 'WATER'                                                  399
      AG_EXNAME = 'MIXTURE'                                               399
      AG_PARLIST = 'DENS'                                                 399
      CALL AXMIXTURE                                                      399
C *                                                                       399
      ELSE                                                                399
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             401
      CALL AGSRESET                                                       401
      AG_DENS = 0.0009                                                    401
      AG_TITLE = 'WATER'                                                  401
      AG_EXNAME = 'MIXTURE'                                               401
      AG_PARLIST = 'DENS'                                                 401
      CALL AXMIXTURE                                                      401
C *                                                                       401
      ENDIF                                                               401
C Check SVTG_IFMANY>0                                                     403
      IF (SVTG_IFMANY.GT.0) THEN                                          403
      CALL AGSBEGM(MODULE,IPRIN)                                          404
C Check IPRIN<0                                                           404
      IF (IPRIN.LT.0) RETURN                                              404
C Create SVTT :                                                           404
C Call To Local Procedure SVTT                                            404
      ASSIGN 5030 TO BLOCK_SVTT                                           404
      GO TO  5020                                                         404
5030  CONTINUE                                                            404
C *                                                                       404
C Geant POSITION : SVTT ===> MOTHER='CAVE' KONLY='MANY' <===              405
      CALL AGSRESET                                                       405
      AG_MOTHER = 'CAVE'                                                  405
      AG_KONLY = 'MANY'                                                   405
      AG_TITLE = 'SVTT'                                                   405
      AG_EXNAME = 'POSITION'                                              405
      AG_PARLIST = 'MOTHER_KONLY'                                         405
      CALL AXPOSITION                                                     405
C *                                                                       405
      ELSE                                                                405
      CALL AGSBEGM(MODULE,IPRIN)                                          407
C Check IPRIN<0                                                           407
      IF (IPRIN.LT.0) RETURN                                              407
C Create SVTT :                                                           407
C Call To Local Procedure SVTT                                            407
      ASSIGN 5040 TO BLOCK_SVTT                                           407
      GO TO  5020                                                         407
5040  CONTINUE                                                            407
C *                                                                       407
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           408
      CALL AGSRESET                                                       408
      AG_MOTHER = 'CAVE'                                                  408
      AG_TITLE = 'SVTT'                                                   408
      AG_EXNAME = 'POSITION'                                              408
      AG_PARLIST = 'MOTHER'                                               408
      CALL AXPOSITION                                                     408
C *                                                                       408
      ENDIF                                                               408
C ----------------------------------------------------------------------  410
C ------------ SVTT                                                       410
      CALL AGSENDM                                                        411
      RETURN                                                              411
C Begin Local Procedure SVTT                                              411
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   411
5020  CONTINUE                                                            411
         AG_TITLE='SVTT'                                                  411
         AG_MODULE=MODULE                                                 411
         AG_MARK='V00'                                                    411
         CALL AGSPUSH                                                     411
         AG_MODULE=MODULE                                                 411
C    *                                                                    411
         RSIZEMAX=SSUP_CON4IDMX                                           412
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 413
     *   2.0*SSUP_HOSRMX                                                  413
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                414
         CLEARANCE=SVTG_RSIZEMAX - RSIZEMAX                               415
C    Check (CLEARANCE<0)                                                  416
         IF ((CLEARANCE.LT.0)) THEN                                       416
         ENDIF                                                            417
C    Geant MATERIAL : AIR ===> inherited arguments used <===              418
         CALL AGSRESET                                                    418
         AG_TITLE = 'AIR'                                                 418
         AG_EXNAME = 'MATERIAL'                                           418
         AG_PARLIST = 'NONE'                                              418
         CALL AXMATERIAL                                                  418
C    *                                                                    418
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       418
         CALL AGSRESET                                                    419
         AG_SEEN = 0                                                      419
         AG_COLO = 1                                                      419
         AG_TITLE = 'SVTT'                                                419
         AG_EXNAME = 'ATTRIBUTE'                                          419
         AG_PARLIST = 'SEEN_COLO'                                         419
         CALL AXATTRIBUTE                                                 419
C    *                                                                    419
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  419
C    VTG_ZSIZEMAX <===                                                    419
         CALL AGSRESET                                                    421
         AG_RMIN = SVTG_RSIZEMIN                                          421
         AG_RMAX = SVTG_RSIZEMAX                                          421
         AG_DZ = SVTG_ZSIZEMAX                                            421
         AG_TITLE = 'TUBE'                                                421
         AG_EXNAME = 'SHAPE'                                              421
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      421
         CALL AXSHAPE                                                     421
C    *                                                                    421
C    Create SIRP :                                                        422
C    Call To Local Procedure SIRP                                         422
         ASSIGN 5060 TO BLOCK_SIRP                                        422
         GO TO  5050                                                      422
5060     CONTINUE                                                         422
C    *                                                                    422
C    Geant POSITION : SIRP ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  423
C    2.5 <===                                                             423
         CALL AGSRESET                                                    423
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             423
         AG_ALPHAN=AG_ALPHAN+1                                            423
         AG_ALPHAK(AG_ALPHAN)=3                                           423
         AG_ALPHAL(AG_ALPHAN)=22.5                                        423
         AG_TITLE = 'SIRP'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'Z_ALPHAZ'                                          423
         CALL AXPOSITION                                                  423
C    *                                                                    423
C    Geant POSITION : SIRP ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  423
C    22.5 <===                                                            423
         CALL AGSRESET                                                    424
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            424
         AG_ALPHAN=AG_ALPHAN+1                                            424
         AG_ALPHAK(AG_ALPHAN)=3                                           424
         AG_ALPHAL(AG_ALPHAN)=22.5                                        424
         AG_TITLE = 'SIRP'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'Z_ALPHAZ'                                          424
         CALL AXPOSITION                                                  424
C    *                                                                    424
C    Create SIRT :                                                        425
C    Call To Local Procedure SIRT                                         425
         ASSIGN 5080 TO BLOCK_SIRT                                        425
         GO TO  5070                                                      425
5080     CONTINUE                                                         425
C    *                                                                    425
C    Geant POSITION : SIRT ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  426
C    2.5 <===                                                             426
         CALL AGSRESET                                                    426
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             426
         AG_ALPHAN=AG_ALPHAN+1                                            426
         AG_ALPHAK(AG_ALPHAN)=3                                           426
         AG_ALPHAL(AG_ALPHAN)=22.5                                        426
         AG_TITLE = 'SIRT'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'Z_ALPHAZ'                                          426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Geant POSITION : SIRT ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  426
C    22.5 <===                                                            426
         CALL AGSRESET                                                    427
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            427
         AG_ALPHAN=AG_ALPHAN+1                                            427
         AG_ALPHAK(AG_ALPHAN)=3                                           427
         AG_ALPHAL(AG_ALPHAN)=22.5                                        427
         AG_TITLE = 'SIRT'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'Z_ALPHAZ'                                          427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Create SOER :                                                        428
C    Call To Local Procedure SOER                                         428
         ASSIGN 5100 TO BLOCK_SOER                                        428
         GO TO  5090                                                      428
5100     CONTINUE                                                         428
C    *                                                                    428
C    Geant POSITION : SOER ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 <===      429
         CALL AGSRESET                                                    429
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             429
         AG_TITLE = 'SOER'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'Z'                                                 429
         CALL AXPOSITION                                                  429
C    *                                                                    429
C    Geant POSITION : SOER ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 <===     429
         CALL AGSRESET                                                    430
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            430
         AG_TITLE = 'SOER'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'Z'                                                 430
         CALL AXPOSITION                                                  430
C    *                                                                    430
C    Create SBRG :                                                        431
C    Call To Local Procedure SBRG                                         431
         ASSIGN 5120 TO BLOCK_SBRG                                        431
         GO TO  5110                                                      431
5120     CONTINUE                                                         431
C    *                                                                    431
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  432
C    /2 <===                                                              432
         CALL AGSRESET                                                    432
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  432
         AG_TITLE = 'SBRG'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'Z'                                                 432
         CALL AXPOSITION                                                  432
C    *                                                                    432
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  432
C    K/2 <===                                                             432
         CALL AGSRESET                                                    433
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 433
         AG_TITLE = 'SBRG'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'Z'                                                 433
         CALL AXPOSITION                                                  433
C    *                                                                    433
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           433
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             435
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      436
C    Create SOES :                                                        437
C    Call To Local Procedure SOES                                         437
         ASSIGN 5140 TO BLOCK_SOES                                        437
         GO TO  5130                                                      437
5140     CONTINUE                                                         437
C    *                                                                    437
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               438
         CALL AGSRESET                                                    438
         AG_Z = SCREW_Z                                                   438
         AG_KONLY = 'MANY'                                                438
         AG_TITLE = 'SOES'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'Z_KONLY'                                           438
         CALL AXPOSITION                                                  438
C    *                                                                    438
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              438
         CALL AGSRESET                                                    439
         AG_Z = -SCREW_Z                                                  439
         AG_KONLY = 'MANY'                                                439
         AG_TITLE = 'SOES'                                                439
         AG_EXNAME = 'POSITION'                                           439
         AG_PARLIST = 'Z_KONLY'                                           439
         CALL AXPOSITION                                                  439
C    *                                                                    439
C    Create SIES :                                                        440
C    Call To Local Procedure SIES                                         440
         ASSIGN 5160 TO BLOCK_SIES                                        440
         GO TO  5150                                                      440
5160     CONTINUE                                                         440
C    *                                                                    440
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               441
         CALL AGSRESET                                                    441
         AG_Z = SCREW_Z                                                   441
         AG_KONLY = 'MANY'                                                441
         AG_TITLE = 'SIES'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'Z_KONLY'                                           441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              441
         CALL AGSRESET                                                    442
         AG_Z = -SCREW_Z                                                  442
         AG_KONLY = 'MANY'                                                442
         AG_TITLE = 'SIES'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'Z_KONLY'                                           442
         CALL AXPOSITION                                                  442
C    *                                                                    442
C    Check SWAM_LEN>0                                                     442
         IF (SWAM_LEN.GT.0) THEN                                          443
C    Create SWMM :                                                        444
C    Call To Local Procedure SWMM                                         444
         ASSIGN 5180 TO BLOCK_SWMM                                        444
         GO TO  5170                                                      444
5180     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : SWMM ===> Z=SWAM_ZMIN+SWAM_LEN/2 <===               445
         CALL AGSRESET                                                    445
         AG_Z = SWAM_ZMIN+SWAM_LEN/2                                      445
         AG_TITLE = 'SWMM'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'Z'                                                 445
         CALL AXPOSITION                                                  445
C    *                                                                    445
C    Geant POSITION : SWMM ===> Z=-SWAM_ZMIN-SWAM_LEN/2 <===              445
         CALL AGSRESET                                                    446
         AG_Z = -SWAM_ZMIN-SWAM_LEN/2                                     446
         AG_TITLE = 'SWMM'                                                446
         AG_EXNAME = 'POSITION'                                           446
         AG_PARLIST = 'Z'                                                 446
         CALL AXPOSITION                                                  446
C    *                                                                    446
         ENDIF                                                            446
C    Check SWAM_LEN>0                                                     448
         IF (SWAM_LEN.GT.0) THEN                                          448
C    Create SBWC :                                                        449
C    Call To Local Procedure SBWC                                         449
         ASSIGN 5200 TO BLOCK_SBWC                                        449
         GO TO  5190                                                      449
5200     CONTINUE                                                         449
C    *                                                                    449
C    Geant POSITION : SBWC ===> Z=(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SW  450
C    AM_ZMIN+SWAM_LEN))/2) <===                                           450
         CALL AGSRESET                                                    450
         AG_Z =                                                           450
     *   (SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)      450
         AG_TITLE = 'SBWC'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'Z'                                                 450
         CALL AXPOSITION                                                  450
C    *                                                                    450
C    Geant POSITION : SBWC ===> Z=-(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(S  450
C    WAM_ZMIN+SWAM_LEN))/2) THETAZ=180 <===                               450
         CALL AGSRESET                                                    452
         AG_Z =                                                           452
     *   -(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)     452
         AG_THETAZ = 180                                                  452
         AG_TITLE = 'SBWC'                                                452
         AG_EXNAME = 'POSITION'                                           452
         AG_PARLIST = 'Z_THETAZ'                                          452
         CALL AXPOSITION                                                  452
C    *                                                                    452
         ENDIF                                                            452
C    Create SCON :                                                        454
C    Call To Local Procedure SCON                                         454
         ASSIGN 5220 TO BLOCK_SCON                                        454
         GO TO  5210                                                      454
5220     CONTINUE                                                         454
C    *                                                                    454
C    Geant POSITION : SCON ===> inherited arguments used <===             455
         CALL AGSRESET                                                    455
         AG_TITLE = 'SCON'                                                455
         AG_EXNAME = 'POSITION'                                           455
         AG_PARLIST = 'NONE'                                              455
         CALL AXPOSITION                                                  455
C    *                                                                    455
C    Geant POSITION : SCON ===> THETAZ=180 <===                           455
         CALL AGSRESET                                                    456
         AG_THETAZ = 180                                                  456
         AG_TITLE = 'SCON'                                                456
         AG_EXNAME = 'POSITION'                                           456
         AG_PARLIST = 'THETAZ'                                            456
         CALL AXPOSITION                                                  456
C    *                                                                    456
C    Create SBSP :                                                        457
C    Call To Local Procedure SBSP                                         457
         ASSIGN 5240 TO BLOCK_SBSP                                        457
         GO TO  5230                                                      457
5240     CONTINUE                                                         457
C    *                                                                    457
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     458
         CALL AGSRESET                                                    458
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            458
         AG_TITLE = 'SBSP'                                                458
         AG_EXNAME = 'POSITION'                                           458
         AG_PARLIST = 'Z'                                                 458
         CALL AXPOSITION                                                  458
C    *                                                                    458
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  458
C    =180 <===                                                            458
         CALL AGSRESET                                                    459
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           459
         AG_THETAZ = 180                                                  459
         AG_TITLE = 'SBSP'                                                459
         AG_EXNAME = 'POSITION'                                           459
         AG_PARLIST = 'Z_THETAZ'                                          459
         CALL AXPOSITION                                                  459
C    *                                                                    459
C    Create SROD :                                                        460
C    Call To Local Procedure SROD                                         460
         ASSIGN 5260 TO BLOCK_SROD                                        460
         GO TO  5250                                                      460
5260     CONTINUE                                                         460
C    *                                                                    460
C    Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===          461
         CALL AGSRESET                                                    461
         AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                 461
         AG_TITLE = 'SROD'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'Y'                                                 461
         CALL AXPOSITION                                                  461
C    *                                                                    461
C    Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===         461
         CALL AGSRESET                                                    462
         AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                462
         AG_TITLE = 'SROD'                                                462
         AG_EXNAME = 'POSITION'                                           462
         AG_PARLIST = 'Y'                                                 462
         CALL AXPOSITION                                                  462
C    *                                                                    462
         RADMAX=SVTG_RSIZEMAX                                             462
C    *                                                                    464
         ILAYER=SVTG_NMIN                                                 464
            GO TO 5273                                                    464
5271        ILAYER=ILAYER+1                                               464
5273        IF(ILAYER-(MIN(6,SVTG_NLAYER)).GT.0) GO TO 5272               464
C       Check ILAYER<6                                                    465
            IF (ILAYER.LT.6) THEN                                         465
C       use SVTL bank                                                     466
            BPATH_SVTL='SVTL*'                                            466
            IDNUM_SVTL(1)=0                                               466
            SVTL_LAYER=ILAYER+1                                           466
            CALL AGDGETP('SVTTGEO10','*',-999,'SVTL',NSVTL_LAYER,         466
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    466
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             466
            RADMAX=SVTL_RADIUS                                            467
            ELSE                                                          468
            RADMAX=SVTG_RSIZEMAX                                          469
            ENDIF                                                         470
C       use SVTL bank                                                     471
            BPATH_SVTL='SVTL*'                                            471
            IDNUM_SVTL(1)=0                                               471
            SVTL_LAYER=ILAYER                                             471
            CALL AGDGETP('SVTTGEO10','*',-999,'SVTL',NSVTL_LAYER,         471
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    471
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             471
C       Create SLYD :                                                     472
C       Call To Local Procedure SLYD                                      472
            ASSIGN 5290 TO BLOCK_SLYD                                     472
            GO TO  5280                                                   472
5290        CONTINUE                                                      472
C       *                                                                 472
C       Check ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6                   473
            IF (ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6) THEN           473
C       Geant POSITION : SLYD ===> KONLY='MANY' <===                      474
            CALL AGSRESET                                                 474
            AG_KONLY = 'MANY'                                             474
            AG_TITLE = 'SLYD'                                             474
            AG_EXNAME = 'POSITION'                                        474
            AG_PARLIST = 'KONLY'                                          474
            CALL AXPOSITION                                               474
C       *                                                                 474
            ELSE                                                          474
C       Geant POSITION : SLYD ===> inherited arguments used <===          476
            CALL AGSRESET                                                 476
            AG_TITLE = 'SLYD'                                             476
            AG_EXNAME = 'POSITION'                                        476
            AG_PARLIST = 'NONE'                                           476
            CALL AXPOSITION                                               476
C       *                                                                 476
            ENDIF                                                         476
         GO TO 5271                                                       478
5272     CONTINUE                                                         478
C    Check SVTG_NLAYER<0                                                  479
         IF (SVTG_NLAYER.LT.0) THEN                                       479
C    use SVTL bank                                                        480
         BPATH_SVTL='SVTL*'                                               480
         IDNUM_SVTL(1)=0                                                  480
         SVTL_LAYER=-SVTG_NLAYER                                          480
         CALL AGDGETP('SVTTGEO10','*',-999,'SVTL',NSVTL_LAYER,            480
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       480
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                480
C    Create SLYD :                                                        481
C    Call To Local Procedure SLYD                                         481
         ASSIGN 5300 TO BLOCK_SLYD                                        481
         GO TO  5280                                                      481
5300     CONTINUE                                                         481
C    *                                                                    481
C    Geant POSITION : SLYD ===> inherited arguments used <===             482
         CALL AGSRESET                                                    482
         AG_TITLE = 'SLYD'                                                482
         AG_EXNAME = 'POSITION'                                           482
         AG_PARLIST = 'NONE'                                              482
         CALL AXPOSITION                                                  482
C    *                                                                    482
         ENDIF                                                            482
C    Create SISH :                                                        484
C    Call To Local Procedure SISH                                         484
         ASSIGN 5320 TO BLOCK_SISH                                        484
         GO TO  5310                                                      484
5320     CONTINUE                                                         484
C    *                                                                    484
C    Geant POSITION : SISH ===> inherited arguments used <===             485
         CALL AGSRESET                                                    485
         AG_TITLE = 'SISH'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'NONE'                                              485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    Create SOSH :                                                        486
C    Call To Local Procedure SOSH                                         486
         ASSIGN 5340 TO BLOCK_SOSH                                        486
         GO TO  5330                                                      486
5340     CONTINUE                                                         486
C    *                                                                    486
C    Geant POSITION : SOSH ===> inherited arguments used <===             487
         CALL AGSRESET                                                    487
         AG_TITLE = 'SOSH'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'NONE'                                              487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Check SVTG_NLAYER>6                                                  487
         IF (SVTG_NLAYER.GT.6) THEN                                       488
C    Create SSSH :                                                        489
C    Call To Local Procedure SSSH                                         489
         ASSIGN 5360 TO BLOCK_SSSH                                        489
         GO TO  5350                                                      489
5360     CONTINUE                                                         489
C    *                                                                    489
C    Geant POSITION : SSSH ===> inherited arguments used <===             490
         CALL AGSRESET                                                    490
         AG_TITLE = 'SSSH'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'NONE'                                              490
         CALL AXPOSITION                                                  490
C    *                                                                    490
         ENDIF                                                            490
C    Create SALM :                                                        492
C    Call To Local Procedure SALM                                         492
         ASSIGN 5380 TO BLOCK_SALM                                        492
         GO TO  5370                                                      492
5380     CONTINUE                                                         492
C    *                                                                    492
C    Geant POSITION : SALM ===> Z=SSLD_ALMSHPOS-SSLD_ALMSHTHK/2 <===      493
         CALL AGSRESET                                                    493
         AG_Z = SSLD_ALMSHPOS-SSLD_ALMSHTHK/2                             493
         AG_TITLE = 'SALM'                                                493
         AG_EXNAME = 'POSITION'                                           493
         AG_PARLIST = 'Z'                                                 493
         CALL AXPOSITION                                                  493
C    *                                                                    493
C    Geant POSITION : SALM ===> Z=-(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2) <===   493
         CALL AGSRESET                                                    494
         AG_Z = -(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2)                          494
         AG_TITLE = 'SALM'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'Z'                                                 494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Create SCBM :                                                        495
C    Call To Local Procedure SCBM                                         495
         ASSIGN 5400 TO BLOCK_SCBM                                        495
         GO TO  5390                                                      495
5400     CONTINUE                                                         495
C    *                                                                    495
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=SWAM_ZMIN-SCBP_LEN <===     496
         CALL AGSRESET                                                    496
         AG_X = 0.0                                                       496
         AG_Y = 0.0                                                       496
         AG_Z = SWAM_ZMIN-SCBP_LEN                                        496
         AG_TITLE = 'SCBM'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'X_Y_Z'                                             496
         CALL AXPOSITION                                                  496
C    *                                                                    496
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=-(SWAM_ZMIN-SCBP_LEN) THET  496
C    AZ=180.0 <===                                                        496
         CALL AGSRESET                                                    497
         AG_X = 0.0                                                       497
         AG_Y = 0.0                                                       497
         AG_Z = -(SWAM_ZMIN-SCBP_LEN)                                     497
         AG_THETAZ = 180.0                                                497
         AG_TITLE = 'SCBM'                                                497
         AG_EXNAME = 'POSITION'                                           497
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      497
         CALL AXPOSITION                                                  497
C    *                                                                    497
C    use SVTL bank                                                        497
         BPATH_SVTL='SVTL*'                                               498
         IDNUM_SVTL(1)=0                                                  498
         SVTL_LAYER=6                                                     498
         CALL AGDGETP('SVTTGEO10','*',-999,'SVTL',NSVTL_LAYER,            498
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       498
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                498
C    Create SXRL :                                                        499
C    Call To Local Procedure SXRL                                         499
         ASSIGN 5420 TO BLOCK_SXRL                                        499
         GO TO  5410                                                      499
5420     CONTINUE                                                         499
C    *                                                                    499
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  501
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  501
         CALL AGSRESET                                                    501
         AG_X = 0.0                                                       501
         AG_Y = 0.0                                                       501
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               501
         AG_KONLY = 'MANY'                                                501
         AG_TITLE = 'SXRL'                                                501
         AG_EXNAME = 'POSITION'                                           501
         AG_PARLIST = 'X_Y_Z_KONLY'                                       501
         CALL AXPOSITION                                                  501
C    *                                                                    501
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  501
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 501
         CALL AGSRESET                                                    503
         AG_X = 0.0                                                       503
         AG_Y = 0.0                                                       503
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              503
         AG_KONLY = 'MANY'                                                503
         AG_TITLE = 'SXRL'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_Y_Z_KONLY'                                       503
         CALL AXPOSITION                                                  503
C    *                                                                    503
C    use SVTL bank                                                        503
         BPATH_SVTL='SVTL*'                                               504
         IDNUM_SVTL(1)=0                                                  504
         SVTL_LAYER=4                                                     504
         CALL AGDGETP('SVTTGEO10','*',-999,'SVTL',NSVTL_LAYER,            504
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       504
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                504
C    Create SXRL :                                                        505
C    Call To Local Procedure SXRL                                         505
         ASSIGN 5430 TO BLOCK_SXRL                                        505
         GO TO  5410                                                      505
5430     CONTINUE                                                         505
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
         SVTL_LAYER=2                                                     510
         CALL AGDGETP('SVTTGEO10','*',-999,'SVTL',NSVTL_LAYER,            510
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       510
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                510
C    Create SXRL :                                                        511
C    Call To Local Procedure SXRL                                         511
         ASSIGN 5440 TO BLOCK_SXRL                                        511
         GO TO  5410                                                      511
5440     CONTINUE                                                         511
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
C    Create SOUM :                                                        516
C    Call To Local Procedure SOUM                                         516
         ASSIGN 5460 TO BLOCK_SOUM                                        516
         GO TO  5450                                                      516
5460     CONTINUE                                                         516
C    *                                                                    516
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 <===                    517
         CALL AGSRESET                                                    517
         AG_X = 0.0                                                       517
         AG_Y = 0.0                                                       517
         AG_Z = 0.0                                                       517
         AG_TITLE = 'SOUM'                                                517
         AG_EXNAME = 'POSITION'                                           517
         AG_PARLIST = 'X_Y_Z'                                             517
         CALL AXPOSITION                                                  517
C    *                                                                    517
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 <===       517
         CALL AGSRESET                                                    518
         AG_X = 0.0                                                       518
         AG_Y = 0.0                                                       518
         AG_Z = 0.0                                                       518
         AG_ALPHAN=AG_ALPHAN+1                                            518
         AG_ALPHAK(AG_ALPHAN)=2                                           518
         AG_ALPHAL(AG_ALPHAN)=180.0                                       518
         AG_TITLE = 'SOUM'                                                518
         AG_EXNAME = 'POSITION'                                           518
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      518
         CALL AXPOSITION                                                  518
C    *                                                                    518
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 ALPHAZ=18  518
C    0 <===                                                               518
         CALL AGSRESET                                                    519
         AG_X = 0.0                                                       519
         AG_Y = 0.0                                                       519
         AG_Z = 0.0                                                       519
         AG_ALPHAN=AG_ALPHAN+1                                            519
         AG_ALPHAK(AG_ALPHAN)=2                                           519
         AG_ALPHAL(AG_ALPHAN)=180.0                                       519
         AG_ALPHAN=AG_ALPHAN+1                                            519
         AG_ALPHAK(AG_ALPHAN)=3                                           519
         AG_ALPHAL(AG_ALPHAN)=180                                         519
         AG_TITLE = 'SOUM'                                                519
         AG_EXNAME = 'POSITION'                                           519
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               519
         CALL AXPOSITION                                                  519
C    *                                                                    519
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAZ=180 <===         519
         CALL AGSRESET                                                    520
         AG_X = 0.0                                                       520
         AG_Y = 0.0                                                       520
         AG_Z = 0.0                                                       520
         AG_ALPHAN=AG_ALPHAN+1                                            520
         AG_ALPHAK(AG_ALPHAN)=3                                           520
         AG_ALPHAL(AG_ALPHAN)=180                                         520
         AG_TITLE = 'SOUM'                                                520
         AG_EXNAME = 'POSITION'                                           520
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      520
         CALL AXPOSITION                                                  520
C    *                                                                    520
5021  CALL AGSPOPB                                                        521
      GO TO BLOCK_SVTT, (5020 ,5030,5040)                                 521
C End of Local Procedure                                                  521
C ----------------------------------------------------------------------  522
C ------------ SXRL                                                       522
C Begin Local Procedure SXRL                                              523
C SXRL IS THE MOTHER OF THE CIRCULAR WATER PIPES                          523
5410  CONTINUE                                                            523
         AG_TITLE='SXRL'                                                  523
         AG_MODULE=MODULE                                                 523
         AG_MARK='V00'                                                    523
         CALL AGSPUSH                                                     523
         AG_MODULE=MODULE                                                 523
C    *                                                                    523
C    Geant MATERIAL : AIR ===> inherited arguments used <===              524
         CALL AGSRESET                                                    524
         AG_TITLE = 'AIR'                                                 524
         AG_EXNAME = 'MATERIAL'                                           524
         AG_PARLIST = 'NONE'                                              524
         CALL AXMATERIAL                                                  524
C    *                                                                    524
C    Geant ATTRIBUTE : SXRL ===> SEEN=0 COLO=1 <===                       524
         CALL AGSRESET                                                    525
         AG_SEEN = 0                                                      525
         AG_COLO = 1                                                      525
         AG_TITLE = 'SXRL'                                                525
         AG_EXNAME = 'ATTRIBUTE'                                          525
         AG_PARLIST = 'SEEN_COLO'                                         525
         CALL AXATTRIBUTE                                                 525
C    *                                                                    525
C    Geant SHAPE : TUBE ===> RMIN=SWCX_ROFFSET+SVTL_RADIUS, RMAX=SWCX_RO  525
C    FFSET+SVTL_RADIUS+SWCX_DR DZ=SWCX_LENGTH/2.0 <===                    525
         CALL AGSRESET                                                    527
         AG_RMIN = SWCX_ROFFSET+SVTL_RADIUS                               527
         AG_RMAX = SWCX_ROFFSET+SVTL_RADIUS+SWCX_DR                       527
         AG_DZ = SWCX_LENGTH/2.0                                          527
         AG_TITLE = 'TUBE'                                                527
         AG_EXNAME = 'SHAPE'                                              527
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      527
         CALL AXSHAPE                                                     527
C    *                                                                    527
C    Create SWRP :                                                        528
C    Call To Local Procedure SWRP                                         528
         ASSIGN 5480 TO BLOCK_SWRP                                        528
         GO TO  5470                                                      528
5480     CONTINUE                                                         528
C    *                                                                    528
C    Create SYRU :                                                        529
C    Call To Local Procedure SYRU                                         529
         ASSIGN 5500 TO BLOCK_SYRU                                        529
         GO TO  5490                                                      529
5500     CONTINUE                                                         529
C    *                                                                    529
C    *                                                                    530
         I_PHI=1                                                          530
            GO TO 5513                                                    530
5511        I_PHI=I_PHI+1                                                 530
5513        IF(I_PHI-(4*SVTL_NLADDER).GT.0) GO TO 5512                    530
            TUBE_ANGLE=(PI/(2.0*SVTL_NLADDER))*(I_PHI-0.5)                531
C       Geant POSITION : SWRP ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  533
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  533
C       SWCX_DR/2.0) Z=0.0 <===                                           533
            CALL AGSRESET                                                 533
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     533
     *      2.0)                                                          533
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     533
     *      2.0)                                                          533
            AG_Z = 0.0                                                    533
            AG_TITLE = 'SWRP'                                             533
            AG_EXNAME = 'POSITION'                                        533
            AG_PARLIST = 'X_Y_Z'                                          533
            CALL AXPOSITION                                               533
C       *                                                                 533
C       Geant POSITION : SYRU ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  533
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  533
C       SWCX_DR/2.0) Z=0.0 <===                                           533
            CALL AGSRESET                                                 535
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     535
     *      2.0)                                                          535
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     535
     *      2.0)                                                          535
            AG_Z = 0.0                                                    535
            AG_TITLE = 'SYRU'                                             535
            AG_EXNAME = 'POSITION'                                        535
            AG_PARLIST = 'X_Y_Z'                                          535
            CALL AXPOSITION                                               535
C       *                                                                 535
         GO TO 5511                                                       536
5512     CONTINUE                                                         536
5411  CALL AGSPOPB                                                        537
      GO TO BLOCK_SXRL, (5410 ,5420,5430,5440)                            537
C End of Local Procedure                                                  537
C ----------------------------------------------------------------------  538
C ------------ SWRP                                                       538
C Begin Local Procedure SWRP                                              540
C SWRP IS AN APPROXIMATION OF WATER IN THE CIRCULAR PIPE, A RECTANGULAR,  540
C  ONE                                                                    540
5470  CONTINUE                                                            540
         AG_TITLE='SWRP'                                                  540
         AG_MODULE=MODULE                                                 540
         AG_MARK='V00'                                                    540
         CALL AGSPUSH                                                     540
         AG_MODULE=MODULE                                                 540
C    *                                                                    540
C    Geant MATERIAL : WATER ===> inherited arguments used <===            541
         CALL AGSRESET                                                    541
         AG_TITLE = 'WATER'                                               541
         AG_EXNAME = 'MATERIAL'                                           541
         AG_PARLIST = 'NONE'                                              541
         CALL AXMATERIAL                                                  541
C    *                                                                    541
C    Geant ATTRIBUTE : SWRP ===> SEEN=1 COLO=4 <===                       541
         CALL AGSRESET                                                    542
         AG_SEEN = 1                                                      542
         AG_COLO = 4                                                      542
         AG_TITLE = 'SWRP'                                                542
         AG_EXNAME = 'ATTRIBUTE'                                          542
         AG_PARLIST = 'SEEN_COLO'                                         542
         CALL AXATTRIBUTE                                                 542
C    *                                                                    542
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SWCX_RAD DZ=SWCX_LENGTH/2.0 <  542
C    ===                                                                  542
         CALL AGSRESET                                                    543
         AG_RMIN = 0.0                                                    543
         AG_RMAX = SWCX_RAD                                               543
         AG_DZ = SWCX_LENGTH/2.0                                          543
         AG_TITLE = 'TUBE'                                                543
         AG_EXNAME = 'SHAPE'                                              543
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      543
         CALL AXSHAPE                                                     543
C    *                                                                    543
5471  CALL AGSPOPB                                                        544
      GO TO BLOCK_SWRP, (5470 ,5480)                                      544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ SYRU                                                       545
C Begin Local Procedure SYRU                                              546
C SYRU IS THE WALL OF THE WATER PIPE                                      546
5490  CONTINUE                                                            546
         AG_TITLE='SYRU'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           547
         CALL AGSRESET                                                    547
         AG_A = 12                                                        547
         AG_Z = 6                                                         547
         AG_W = 1                                                         547
         AG_TITLE = 'C'                                                   547
         AG_EXNAME = 'COMPONENT'                                          547
         AG_PARLIST = 'A_Z_W'                                             547
         CALL AXCOMPONENT                                                 547
C    *                                                                    547
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           547
         CALL AGSRESET                                                    548
         AG_A = 1                                                         548
         AG_Z = 1                                                         548
         AG_W = 2                                                         548
         AG_TITLE = 'H2'                                                  548
         AG_EXNAME = 'COMPONENT'                                          548
         AG_PARLIST = 'A_Z_W'                                             548
         CALL AXCOMPONENT                                                 548
C    *                                                                    548
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             548
         CALL AGSRESET                                                    549
         AG_DENS = 0.935                                                  549
         AG_TITLE = 'CH2'                                                 549
         AG_EXNAME = 'MIXTURE'                                            549
         AG_PARLIST = 'DENS'                                              549
         CALL AXMIXTURE                                                   549
C    *                                                                    549
C    Geant ATTRIBUTE : SYRU ===> SEEN=1 COLO=4 <===                       549
         CALL AGSRESET                                                    550
         AG_SEEN = 1                                                      550
         AG_COLO = 4                                                      550
         AG_TITLE = 'SYRU'                                                550
         AG_EXNAME = 'ATTRIBUTE'                                          550
         AG_PARLIST = 'SEEN_COLO'                                         550
         CALL AXATTRIBUTE                                                 550
C    *                                                                    550
C    Geant SHAPE : TUBE ===> RMIN=SWCX_RAD RMAX=SWCX_RAD+SWCX_WALL, DZ=S  550
C    WCX_LENGTH/2.0 <===                                                  550
         CALL AGSRESET                                                    552
         AG_RMIN = SWCX_RAD                                               552
         AG_RMAX = SWCX_RAD+SWCX_WALL                                     552
         AG_DZ = SWCX_LENGTH/2.0                                          552
         AG_TITLE = 'TUBE'                                                552
         AG_EXNAME = 'SHAPE'                                              552
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      552
         CALL AXSHAPE                                                     552
C    *                                                                    552
5491  CALL AGSPOPB                                                        553
      GO TO BLOCK_SYRU, (5490 ,5500)                                      553
C End of Local Procedure                                                  553
C ----------------------------------------------------------------------  554
C ------------ SOUM                                                       554
C Begin Local Procedure SOUM                                              555
C SOUM IS THE MOTHER OF THE ARRAY OF THE OUTER SHILEDING SUPPORT TUBES    555
5450  CONTINUE                                                            555
         AG_TITLE='SOUM'                                                  555
         AG_MODULE=MODULE                                                 555
         AG_MARK='V00'                                                    555
         CALL AGSPUSH                                                     555
         AG_MODULE=MODULE                                                 555
C    *                                                                    555
C    Geant MATERIAL : AIR ===> inherited arguments used <===              556
         CALL AGSRESET                                                    556
         AG_TITLE = 'AIR'                                                 556
         AG_EXNAME = 'MATERIAL'                                           556
         AG_PARLIST = 'NONE'                                              556
         CALL AXMATERIAL                                                  556
C    *                                                                    556
C    Geant ATTRIBUTE : SOUM ===> SEEN=0 COLO=1 <===                       556
         CALL AGSRESET                                                    557
         AG_SEEN = 0                                                      557
         AG_COLO = 1                                                      557
         AG_TITLE = 'SOUM'                                                557
         AG_EXNAME = 'ATTRIBUTE'                                          557
         AG_PARLIST = 'SEEN_COLO'                                         557
         CALL AXATTRIBUTE                                                 557
C    *                                                                    557
C    Geant SHAPE : TUBS ===> RMIN=SOUP_ROUT-SOUP_DR RMAX=SOUP_ROUT, PHI1  557
C    =SOUP_PHI1 PHI2=SOUP_PHI2 DZ=SOUP_LENGTH/2.0 <===                    557
         CALL AGSRESET                                                    559
         AG_RMIN = SOUP_ROUT-SOUP_DR                                      559
         AG_RMAX = SOUP_ROUT                                              559
         AG_PHI1 = SOUP_PHI1                                              559
         AG_PHI2 = SOUP_PHI2                                              559
         AG_DZ = SOUP_LENGTH/2.0                                          559
         AG_TITLE = 'TUBS'                                                559
         AG_EXNAME = 'SHAPE'                                              559
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            559
         CALL AXSHAPE                                                     559
C    *                                                                    559
C    Create SOUR :                                                        560
C    Call To Local Procedure SOUR                                         560
         ASSIGN 5530 TO BLOCK_SOUR                                        560
         GO TO  5520                                                      560
5530     CONTINUE                                                         560
C    *                                                                    560
C    *                                                                    561
         I_PHI=1                                                          561
            GO TO 5543                                                    561
5541        I_PHI=I_PHI+1                                                 561
5543        IF(I_PHI-(4).GT.0) GO TO 5542                                 561
C       Geant POSITION : SOUR ===> X=COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SO  563
C       UP_ROUT-SOUP_DR/2.0) Y=SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROU  563
C       T-SOUP_DR/2.0) Z=0.0 <===                                         563
            CALL AGSRESET                                                 563
            AG_X = COS(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    563
     *      2.0)                                                          563
            AG_Y = SIN(SHIELD_PHI(I_PHI)*PI/180.0)*(SOUP_ROUT-SOUP_DR/    563
     *      2.0)                                                          563
            AG_Z = 0.0                                                    563
            AG_TITLE = 'SOUR'                                             563
            AG_EXNAME = 'POSITION'                                        563
            AG_PARLIST = 'X_Y_Z'                                          563
            CALL AXPOSITION                                               563
C       *                                                                 563
         GO TO 5541                                                       564
5542     CONTINUE                                                         564
5451  CALL AGSPOPB                                                        565
      GO TO BLOCK_SOUM, (5450 ,5460)                                      565
C End of Local Procedure                                                  565
C ----------------------------------------------------------------------  566
C ------------ SOUR                                                       566
C Begin Local Procedure SOUR                                              567
C SOUR IS THE OUTER SHILEDING SUPPORT TUBES (RODS)                        567
5520  CONTINUE                                                            567
         AG_TITLE='SOUR'                                                  567
         AG_MODULE=MODULE                                                 567
         AG_MARK='V00'                                                    567
         CALL AGSPUSH                                                     567
         AG_MODULE=MODULE                                                 567
C    *                                                                    567
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           568
         CALL AGSRESET                                                    568
         AG_TITLE = 'CARBON'                                              568
         AG_EXNAME = 'MATERIAL'                                           568
         AG_PARLIST = 'NONE'                                              568
         CALL AXMATERIAL                                                  568
C    *                                                                    568
C    Geant ATTRIBUTE : SOUM ===> SEEN=1 COLO=4 <===                       568
         CALL AGSRESET                                                    569
         AG_SEEN = 1                                                      569
         AG_COLO = 4                                                      569
         AG_TITLE = 'SOUM'                                                569
         AG_EXNAME = 'ATTRIBUTE'                                          569
         AG_PARLIST = 'SEEN_COLO'                                         569
         CALL AXATTRIBUTE                                                 569
C    *                                                                    569
C    Geant SHAPE : TUBE ===> RMIN=SOUP_DIAMIN/2.0 RMAX=SOUP_DIAMOUT/2.0,  569
C     DZ=SOUP_LENGTH/2.0 <===                                             569
         CALL AGSRESET                                                    571
         AG_RMIN = SOUP_DIAMIN/2.0                                        571
         AG_RMAX = SOUP_DIAMOUT/2.0                                       571
         AG_DZ = SOUP_LENGTH/2.0                                          571
         AG_TITLE = 'TUBE'                                                571
         AG_EXNAME = 'SHAPE'                                              571
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      571
         CALL AXSHAPE                                                     571
C    *                                                                    571
5521  CALL AGSPOPB                                                        572
      GO TO BLOCK_SOUR, (5520 ,5530)                                      572
C End of Local Procedure                                                  572
C ----------------------------------------------------------------------  573
C ------------ SLYD                                                       573
C Begin Local Procedure SLYD                                              574
C SLYD IS A SINGLE SVT LAYER                                              574
5280  CONTINUE                                                            574
         AG_TITLE='SLYD'                                                  574
         AG_MODULE=MODULE                                                 574
         AG_MARK='V00'                                                    574
         CALL AGSPUSH                                                     574
         AG_MODULE=MODULE                                                 574
C    *                                                                    574
         LADTHK = SWCA_WAFERTHK/2 + SWCA_ROHATHK + SWCA_WAFCARTH          575
         ELETHK=2.0*SELC_BETHK + SELC_WATTHK + SELC_BEOTHK +              576
     *   SELC_AGPDTHK                                                     576
         ELETHK=ELETHK + SELC_GLASSTHK + SELC_DYETHK                      577
         DEG=180.0/SVTL_NLADDER                                           578
         RAD=PI/SVTL_NLADDER                                              579
C    Geant MATERIAL : AIR ===> inherited arguments used <===              580
         CALL AGSRESET                                                    580
         AG_TITLE = 'AIR'                                                 580
         AG_EXNAME = 'MATERIAL'                                           580
         AG_PARLIST = 'NONE'                                              580
         CALL AXMATERIAL                                                  580
C    *                                                                    580
C    Geant ATTRIBUTE : SLYD ===> SEEN=0 COLO=1 <===                       580
         CALL AGSRESET                                                    581
         AG_SEEN = 0                                                      581
         AG_COLO = 1                                                      581
         AG_TITLE = 'SLYD'                                                581
         AG_EXNAME = 'ATTRIBUTE'                                          581
         AG_PARLIST = 'SEEN_COLO'                                         581
         CALL AXATTRIBUTE                                                 581
C    *                                                                    581
         RMIN=SVTL_RADIUS - LADTHK                                        581
         RMAX=SQRT((SVTL_RADIUS)**2 + (SWCA_WAFERWID/2)**2) + ELETHK      583
         RMAX = SVTL_RADIUS*SVTL_RADIUS                                   584
         RMAX=RMAX+SWCA_WAFERWID*SWCA_WAFERWID/4.0                        585
         RMAX = SQRT(RMAX) + ELETHK                                       586
C    Check (RMAX>RADMAX-LADTHK)                                           587
         IF ((RMAX.GT.RADMAX-LADTHK)) THEN                                587
         ENDIF                                                            588
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   589
C    Check (RMAX<RMIN)                                                    590
         IF ((RMAX.LT.RMIN)) THEN                                         590
         ENDIF                                                            591
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    592
         CALL AGSRESET                                                    592
         AG_RMIN = RMIN                                                   592
         AG_RMAX = RMAX                                                   592
         AG_DZ = SWCA_LENGTH/2                                            592
         AG_TITLE = 'TUBE'                                                592
         AG_EXNAME = 'SHAPE'                                              592
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      592
         CALL AXSHAPE                                                     592
C    *                                                                    592
C    Create SLSD :                                                        593
C    Call To Local Procedure SLSD                                         593
         ASSIGN 5560 TO BLOCK_SLSD                                        593
         GO TO  5550                                                      593
5560     CONTINUE                                                         593
C    *                                                                    593
5281  CALL AGSPOPB                                                        594
      GO TO BLOCK_SLYD, (5280 ,5290,5300)                                 594
C End of Local Procedure                                                  594
C ----------------------------------------------------------------------  595
C ------------ SLSD                                                       595
C Begin Local Procedure SLSD                                              596
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         596
5550  CONTINUE                                                            596
         AG_TITLE='SLSD'                                                  596
         AG_MODULE=MODULE                                                 596
         AG_MARK='V00'                                                    596
         CALL AGSPUSH                                                     596
         AG_MODULE=MODULE                                                 596
C    *                                                                    596
C    Geant ATTRIBUTE : SLSD ===> SEEN=0 COLO=1 <===                       597
         CALL AGSRESET                                                    597
         AG_SEEN = 0                                                      597
         AG_COLO = 1                                                      597
         AG_TITLE = 'SLSD'                                                597
         AG_EXNAME = 'ATTRIBUTE'                                          597
         AG_PARLIST = 'SEEN_COLO'                                         597
         CALL AXATTRIBUTE                                                 597
C    *                                                                    597
C    Check SVTG_NLAYER<0                                                  597
         IF (SVTG_NLAYER.LT.0) THEN                                       598
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               599
         CALL AGSRESET                                                    599
         AG_NDIV = 1                                                      599
         AG_IAXIS = 2                                                     599
         AG_C0 = -90                                                      599
         AG_TITLE = 'DIVISION'                                            599
         AG_EXNAME = 'SHAPE'                                              599
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     599
         CALL AXSHAPE                                                     599
C    *                                                                    599
         ELSE                                                             599
C    Check SVTG_VERSION==1                                                601
         IF (SVTG_VERSION.EQ.1) THEN                                      601
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              602
         ELSE                                                             603
         C0=90-DEG*MOD(ILAYER,2)                                          604
         ENDIF                                                            605
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NLADDER IAXIS=2, C0=C0 <===    607
         CALL AGSRESET                                                    607
         AG_NDIV = SVTL_NLADDER                                           607
         AG_IAXIS = 2                                                     607
         AG_C0 = C0                                                       607
         AG_TITLE = 'DIVISION'                                            607
         AG_EXNAME = 'SHAPE'                                              607
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     607
         CALL AXSHAPE                                                     607
C    *                                                                    607
         ENDIF                                                            607
C    Create SLDI :                                                        609
C    Call To Local Procedure SLDI                                         609
         ASSIGN 5580 TO BLOCK_SLDI                                        609
         GO TO  5570                                                      609
5580     CONTINUE                                                         609
C    *                                                                    609
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ORT=YZX <===                610
         CALL AGSRESET                                                    610
         AG_X = SVTL_RADIUS                                               610
         AG_ALPHAN=AG_ALPHAN+1                                            610
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 610
         AG_TITLE = 'SLDI'                                                610
         AG_EXNAME = 'POSITION'                                           610
         AG_PARLIST = 'X_ORT'                                             610
         CALL AXPOSITION                                                  610
C    *                                                                    610
C    Create SELE :                                                        611
C    Call To Local Procedure SELE                                         611
         ASSIGN 5600 TO BLOCK_SELE                                        611
         GO TO  5590                                                      611
5600     CONTINUE                                                         611
C    *                                                                    611
         DEG=180.0/SVTL_NLADDER                                           612
         RAD=(TWOPI/2)/SVTL_NLADDER                                       613
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 614
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 615
C    *                                                                    616
         S=-1                                                             616
            GOTO 5613                                                     616
5611        S=S+(2)                                                       616
5613        IF((2)*(S-(1)).GT.0) GO TO 5612                               616
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  618
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=S*DEG <===         618
            CALL AGSRESET                                                 618
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                618
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               618
            AG_ALPHAN=AG_ALPHAN+1                                         618
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              618
            AG_ALPHAN=AG_ALPHAN+1                                         618
            AG_ALPHAK(AG_ALPHAN)=1                                        618
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 618
            AG_ALPHAN=AG_ALPHAN+1                                         618
            AG_ALPHAK(AG_ALPHAN)=3                                        618
            AG_ALPHAL(AG_ALPHAN)=S*DEG                                    618
            AG_TITLE = 'SELE'                                             618
            AG_EXNAME = 'POSITION'                                        618
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          618
            CALL AXPOSITION                                               618
C       *                                                                 618
         GO TO 5611                                                       619
5612     CONTINUE                                                         619
5551  CALL AGSPOPB                                                        620
      GO TO BLOCK_SLSD, (5550 ,5560)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ SLDI                                                       621
C Begin Local Procedure SLDI                                              622
C SLDI IS A LADDER VOLUME                                                 622
5570  CONTINUE                                                            622
         AG_TITLE='SLDI'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       623
C    Geant MATERIAL : AIR ===> inherited arguments used <===              624
         CALL AGSRESET                                                    624
         AG_TITLE = 'AIR'                                                 624
         AG_EXNAME = 'MATERIAL'                                           624
         AG_PARLIST = 'NONE'                                              624
         CALL AXMATERIAL                                                  624
C    *                                                                    624
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 SERIAL=SVTL_NWAFER <===    624
         CALL AGSRESET                                                    625
         AG_SEEN = 0                                                      625
         AG_COLO = 1                                                      625
         AG_SERIAL = SVTL_NWAFER                                          625
         AG_TITLE = 'SLDI'                                                625
         AG_EXNAME = 'ATTRIBUTE'                                          625
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  625
         CALL AXATTRIBUTE                                                 625
C    *                                                                    625
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  625
C    HK <===                                                              625
         CALL AGSRESET                                                    627
         AG_DX = SWCA_WAFERWID/2                                          627
         AG_DY = SWCA_LENGTH/2                                            627
         AG_DZ = LADTHK                                                   627
         AG_TITLE = 'BOX'                                                 627
         AG_EXNAME = 'SHAPE'                                              627
         AG_PARLIST = 'DX_DY_DZ'                                          627
         CALL AXSHAPE                                                     627
C    *                                                                    627
C    Create STLI :                                                        628
C    Call To Local Procedure STLI                                         628
         ASSIGN 5630 TO BLOCK_STLI                                        628
         GO TO  5620                                                      628
5630     CONTINUE                                                         628
C    *                                                                    628
C    Geant POSITION : STLI ===> inherited arguments used <===             629
         CALL AGSRESET                                                    629
         AG_TITLE = 'STLI'                                                629
         AG_EXNAME = 'POSITION'                                           629
         AG_PARLIST = 'NONE'                                              629
         CALL AXPOSITION                                                  629
C    *                                                                    629
C    Create SBER :                                                        630
C    Call To Local Procedure SBER                                         630
         ASSIGN 5650 TO BLOCK_SBER                                        630
         GO TO  5640                                                      630
5650     CONTINUE                                                         630
C    *                                                                    630
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  631
C    DTHK+SWCA_WAFCARTH/2 <===                                            631
         CALL AGSRESET                                                    631
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          631
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   631
         AG_TITLE = 'SBER'                                                631
         AG_EXNAME = 'POSITION'                                           631
         AG_PARLIST = 'X_Z'                                               631
         CALL AXPOSITION                                                  631
C    *                                                                    631
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  631
C    DTHK+SWCA_WAFCARTH/2 <===                                            631
         CALL AGSRESET                                                    632
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          632
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   632
         AG_TITLE = 'SBER'                                                632
         AG_EXNAME = 'POSITION'                                           632
         AG_PARLIST = 'X_Z'                                               632
         CALL AXPOSITION                                                  632
C    *                                                                    632
C    Create STAB :                                                        633
C    Call To Local Procedure STAB                                         633
         ASSIGN 5670 TO BLOCK_STAB                                        633
         GO TO  5660                                                      633
5670     CONTINUE                                                         633
C    *                                                                    633
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  634
C    WAFCARTH/2 <===                                                      634
         CALL AGSRESET                                                    634
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    634
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   634
         AG_TITLE = 'STAB'                                                634
         AG_EXNAME = 'POSITION'                                           634
         AG_PARLIST = 'Y_Z'                                               634
         CALL AXPOSITION                                                  634
C    *                                                                    634
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  634
C    _WAFCARTH/2 <===                                                     634
         CALL AGSRESET                                                    635
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   635
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   635
         AG_TITLE = 'STAB'                                                635
         AG_EXNAME = 'POSITION'                                           635
         AG_PARLIST = 'Y_Z'                                               635
         CALL AXPOSITION                                                  635
C    *                                                                    635
C    Create STRU :                                                        636
C    Call To Local Procedure STRU                                         636
         ASSIGN 5690 TO BLOCK_STRU                                        636
         GO TO  5680                                                      636
5690     CONTINUE                                                         636
C    *                                                                    636
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  639
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  639
C    =                                                                    639
         CALL AGSRESET                                                    639
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              639
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  639
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   639
         AG_TITLE = 'STRU'                                                639
         AG_EXNAME = 'POSITION'                                           639
         AG_PARLIST = 'Y_Z'                                               639
         CALL AXPOSITION                                                  639
C    *                                                                    639
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  639
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  639
C    ==                                                                   639
         CALL AGSRESET                                                    642
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             642
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  642
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   642
         AG_TITLE = 'STRU'                                                642
         AG_EXNAME = 'POSITION'                                           642
         AG_PARLIST = 'Y_Z'                                               642
         CALL AXPOSITION                                                  642
C    *                                                                    642
C    Create SRHC :                                                        643
C    Call To Local Procedure SRHC                                         643
         ASSIGN 5710 TO BLOCK_SRHC                                        643
         GO TO  5700                                                      643
5710     CONTINUE                                                         643
C    *                                                                    643
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  645
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         645
         CALL AGSRESET                                                    645
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          645
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                645
         AG_TITLE = 'SRHC'                                                645
         AG_EXNAME = 'POSITION'                                           645
         AG_PARLIST = 'X_Z'                                               645
         CALL AXPOSITION                                                  645
C    *                                                                    645
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  645
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         645
         CALL AGSRESET                                                    647
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          647
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                647
         AG_TITLE = 'SRHC'                                                647
         AG_EXNAME = 'POSITION'                                           647
         AG_PARLIST = 'X_Z'                                               647
         CALL AXPOSITION                                                  647
C    *                                                                    647
C    Create SPCB :                                                        648
C    Call To Local Procedure SPCB                                         648
         ASSIGN 5730 TO BLOCK_SPCB                                        648
         GO TO  5720                                                      648
5730     CONTINUE                                                         648
C    *                                                                    648
         YPCB = SVTL_NWAFER*(SWCA_WAFERLEN + SWCA_WAFERGAP)/2.0 +         649
     *   SVTL_PCBGAP + SVTL_PCBLEN/2.0                                    649
C    Geant POSITION : SPCB ===> X=0.0 Y=YPCB Z=SVTL_PCBTHK/2.0 <===       650
         CALL AGSRESET                                                    650
         AG_X = 0.0                                                       650
         AG_Y = YPCB                                                      650
         AG_Z = SVTL_PCBTHK/2.0                                           650
         AG_TITLE = 'SPCB'                                                650
         AG_EXNAME = 'POSITION'                                           650
         AG_PARLIST = 'X_Y_Z'                                             650
         CALL AXPOSITION                                                  650
C    *                                                                    650
C    Geant POSITION : SPCB ===> X=0.0 Y=-YPCB Z=SVTL_PCBTHK/2.0 ALPHAZ=1  650
C    80.0 <===                                                            650
         CALL AGSRESET                                                    651
         AG_X = 0.0                                                       651
         AG_Y = -YPCB                                                     651
         AG_Z = SVTL_PCBTHK/2.0                                           651
         AG_ALPHAN=AG_ALPHAN+1                                            651
         AG_ALPHAK(AG_ALPHAN)=3                                           651
         AG_ALPHAL(AG_ALPHAN)=180.0                                       651
         AG_TITLE = 'SPCB'                                                651
         AG_EXNAME = 'POSITION'                                           651
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      651
         CALL AXPOSITION                                                  651
C    *                                                                    651
5571  CALL AGSPOPB                                                        652
      GO TO BLOCK_SLDI, (5570 ,5580)                                      652
C End of Local Procedure                                                  652
C ----------------------------------------------------------------------  653
C ------------ SRHC                                                       653
C Begin Local Procedure SRHC                                              654
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     654
5700  CONTINUE                                                            654
         AG_TITLE='SRHC'                                                  654
         AG_MODULE=MODULE                                                 654
         AG_MARK='V00'                                                    654
         CALL AGSPUSH                                                     654
         AG_MODULE=MODULE                                                 654
C    *                                                                    654
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           655
         CALL AGSRESET                                                    655
         AG_A = 12                                                        655
         AG_Z = 6                                                         655
         AG_W = 1                                                         655
         AG_TITLE = 'C'                                                   655
         AG_EXNAME = 'COMPONENT'                                          655
         AG_PARLIST = 'A_Z_W'                                             655
         CALL AXCOMPONENT                                                 655
C    *                                                                    655
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           655
         CALL AGSRESET                                                    656
         AG_A = 1                                                         656
         AG_Z = 1                                                         656
         AG_W = 2                                                         656
         AG_TITLE = 'H2'                                                  656
         AG_EXNAME = 'COMPONENT'                                          656
         AG_PARLIST = 'A_Z_W'                                             656
         CALL AXCOMPONENT                                                 656
C    *                                                                    656
C    Geant MIXTURE : ROHA ===> DENS=0.0304 <===                           656
         CALL AGSRESET                                                    657
         AG_DENS = 0.0304                                                 657
         AG_TITLE = 'ROHA'                                                657
         AG_EXNAME = 'MIXTURE'                                            657
         AG_PARLIST = 'DENS'                                              657
         CALL AXMIXTURE                                                   657
C    *                                                                    657
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       657
         CALL AGSRESET                                                    658
         AG_SEEN = 1                                                      658
         AG_COLO = 3                                                      658
         AG_TITLE = 'SRHC'                                                658
         AG_EXNAME = 'ATTRIBUTE'                                          658
         AG_PARLIST = 'SEEN_COLO'                                         658
         CALL AXATTRIBUTE                                                 658
C    *                                                                    658
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  658
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        658
         CALL AGSRESET                                                    660
         AG_DX = SWCA_WAFCARWD/2                                          660
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              660
         AG_DZ = SWCA_ROHATHK/2                                           660
         AG_TITLE = 'BOX'                                                 660
         AG_EXNAME = 'SHAPE'                                              660
         AG_PARLIST = 'DX_DY_DZ'                                          660
         CALL AXSHAPE                                                     660
C    *                                                                    660
5701  CALL AGSPOPB                                                        661
      GO TO BLOCK_SRHC, (5700 ,5710)                                      661
C End of Local Procedure                                                  661
C ----------------------------------------------------------------------  662
C ------------ STLI                                                       662
C Begin Local Procedure STLI                                              663
C STLI IS THE WAFER PACK CONTAINER                                        663
5620  CONTINUE                                                            663
         AG_TITLE='STLI'                                                  663
         AG_MODULE=MODULE                                                 663
         AG_MARK='V00'                                                    663
         CALL AGSPUSH                                                     663
         AG_MODULE=MODULE                                                 663
C    *                                                                    663
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              664
         CALL AGSRESET                                                    664
         AG_SEEN = 0                                                      664
         AG_COLO = 1                                                      664
         AG_SERIAL = 0                                                    664
         AG_TITLE = 'STLI'                                                664
         AG_EXNAME = 'ATTRIBUTE'                                          664
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  664
         CALL AXATTRIBUTE                                                 664
C    *                                                                    664
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  664
C    P), DZ=SWCA_WAFERTHK/2 <===                                          664
         CALL AGSRESET                                                    666
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              666
         AG_DZ = SWCA_WAFERTHK/2                                          666
         AG_TITLE = 'BOX'                                                 666
         AG_EXNAME = 'SHAPE'                                              666
         AG_PARLIST = 'DY_DZ'                                             666
         CALL AXSHAPE                                                     666
C    *                                                                    666
C    Create STSI :                                                        667
C    Call To Local Procedure STSI                                         667
         ASSIGN 5750 TO BLOCK_STSI                                        667
         GO TO  5740                                                      667
5750     CONTINUE                                                         667
C    *                                                                    667
5621  CALL AGSPOPB                                                        668
      GO TO BLOCK_STLI, (5620 ,5630)                                      668
C End of Local Procedure                                                  668
C ----------------------------------------------------------------------  669
C ------------ STSI                                                       669
C Begin Local Procedure STSI                                              670
C STSI IS A SINGLE WAVER CONTAINER                                        670
5740  CONTINUE                                                            670
         AG_TITLE='STSI'                                                  670
         AG_MODULE=MODULE                                                 670
         AG_MARK='V00'                                                    670
         CALL AGSPUSH                                                     670
         AG_MODULE=MODULE                                                 670
C    *                                                                    670
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            671
         CALL AGSRESET                                                    671
         AG_NDIV = SVTL_NWAFER                                            671
         AG_IAXIS = 2                                                     671
         AG_TITLE = 'DIVISION'                                            671
         AG_EXNAME = 'SHAPE'                                              671
         AG_PARLIST = 'NDIV_IAXIS'                                        671
         CALL AXSHAPE                                                     671
C    *                                                                    671
C    Create SVTD :                                                        672
C    Call To Local Procedure SVTD                                         672
         ASSIGN 5770 TO BLOCK_SVTD                                        672
         GO TO  5760                                                      672
5770     CONTINUE                                                         672
C    *                                                                    672
C    Geant POSITION : SVTD ===> inherited arguments used <===             673
         CALL AGSRESET                                                    673
         AG_TITLE = 'SVTD'                                                673
         AG_EXNAME = 'POSITION'                                           673
         AG_PARLIST = 'NONE'                                              673
         CALL AXPOSITION                                                  673
C    *                                                                    673
5741  CALL AGSPOPB                                                        674
      GO TO BLOCK_STSI, (5740 ,5750)                                      674
C End of Local Procedure                                                  674
C ----------------------------------------------------------------------  675
C ------------ SVTD                                                       675
C Begin Local Procedure SVTD                                              676
C SVTD IS AN ACTIVE WAFER VOLUME                                          676
5760  CONTINUE                                                            676
         AG_TITLE='SVTD'                                                  676
         AG_MODULE=MODULE                                                 676
         AG_MARK='V00'                                                    676
         CALL AGSPUSH                                                     676
         AG_MODULE=MODULE                                                 676
C    *                                                                    676
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          677
         CALL AGSRESET                                                    677
         AG_TITLE = 'SILICON'                                             677
         AG_EXNAME = 'MATERIAL'                                           677
         AG_PARLIST = 'NONE'                                              677
         CALL AXMATERIAL                                                  677
C    *                                                                    677
C    Geant MATERIAL : SENSSI ===> ISVOL=1 <===                            677
         CALL AGSRESET                                                    678
         AG_ISVOL = 1                                                     678
         AG_TITLE = 'SENSSI'                                              678
         AG_EXNAME = 'MATERIAL'                                           678
         AG_PARLIST = 'ISVOL'                                             678
         CALL AXMATERIAL                                                  678
C    *                                                                    678
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       678
         CALL AGSRESET                                                    679
         AG_SEEN = 1                                                      679
         AG_COLO = 4                                                      679
         AG_TITLE = 'SVTD'                                                679
         AG_EXNAME = 'ATTRIBUTE'                                          679
         AG_PARLIST = 'SEEN_COLO'                                         679
         CALL AXATTRIBUTE                                                 679
C    *                                                                    679
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       679
         CALL AGSRESET                                                    680
         AG_DY = SWCA_WAFERLEN/2                                          680
         AG_TITLE = 'BOX'                                                 680
         AG_EXNAME = 'SHAPE'                                              680
         AG_PARLIST = 'DY'                                                680
         CALL AXSHAPE                                                     680
C    *                                                                    680
         TRAPY=SWCA_SENSWID/2.0 - (SWCA_SENSWID - SWCA_SENSGIR)/2.0       680
C    Create STRA :                                                        682
C    Call To Local Procedure STRA                                         682
         ASSIGN 5790 TO BLOCK_STRA                                        682
         GO TO  5780                                                      682
5790     CONTINUE                                                         682
C    *                                                                    682
C    Geant POSITION : STRA ===> Y=+TRAPY ALPHAZ=+90 ORT=YZX <===          683
         CALL AGSRESET                                                    683
         AG_Y = +TRAPY                                                    683
         AG_ALPHAN=AG_ALPHAN+1                                            683
         AG_ALPHAK(AG_ALPHAN)=3                                           683
         AG_ALPHAL(AG_ALPHAN)=+90                                         683
         AG_ALPHAN=AG_ALPHAN+1                                            683
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 683
         AG_TITLE = 'STRA'                                                683
         AG_EXNAME = 'POSITION'                                           683
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      683
         CALL AXPOSITION                                                  683
C    *                                                                    683
C    Geant POSITION : STRA ===> Y=-TRAPY ALPHAZ=-90 ORT=YZX <===          683
         CALL AGSRESET                                                    684
         AG_Y = -TRAPY                                                    684
         AG_ALPHAN=AG_ALPHAN+1                                            684
         AG_ALPHAK(AG_ALPHAN)=3                                           684
         AG_ALPHAL(AG_ALPHAN)=-90                                         684
         AG_ALPHAN=AG_ALPHAN+1                                            684
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 684
         AG_TITLE = 'STRA'                                                684
         AG_EXNAME = 'POSITION'                                           684
         AG_PARLIST = 'Y_ALPHAZ_ORT'                                      684
         CALL AXPOSITION                                                  684
C    *                                                                    684
         SSIDX=SWCA_WAFERLEN/2.0 - (SWCA_WAFERLEN - SWCA_SENSLEN)/4.0     684
         SSIRY=SWCA_WAFERWID/2.0 - (SWCA_WAFERWID - SWCA_SENSWID)/4.0     686
C    Create SSID :                                                        687
C    Call To Local Procedure SSID                                         687
         ASSIGN 5810 TO BLOCK_SSID                                        687
         GO TO  5800                                                      687
5810     CONTINUE                                                         687
C    *                                                                    687
C    Geant POSITION : SSID ===> X=+SSIDX <===                             688
         CALL AGSRESET                                                    688
         AG_X = +SSIDX                                                    688
         AG_TITLE = 'SSID'                                                688
         AG_EXNAME = 'POSITION'                                           688
         AG_PARLIST = 'X'                                                 688
         CALL AXPOSITION                                                  688
C    *                                                                    688
C    Geant POSITION : SSID ===> X=-SSIDX <===                             688
         CALL AGSRESET                                                    689
         AG_X = -SSIDX                                                    689
         AG_TITLE = 'SSID'                                                689
         AG_EXNAME = 'POSITION'                                           689
         AG_PARLIST = 'X'                                                 689
         CALL AXPOSITION                                                  689
C    *                                                                    689
C    Create SSIR :                                                        690
C    Call To Local Procedure SSIR                                         690
         ASSIGN 5830 TO BLOCK_SSIR                                        690
         GO TO  5820                                                      690
5830     CONTINUE                                                         690
C    *                                                                    690
C    Geant POSITION : SSIR ===> Y=+SSIRY <===                             691
         CALL AGSRESET                                                    691
         AG_Y = +SSIRY                                                    691
         AG_TITLE = 'SSIR'                                                691
         AG_EXNAME = 'POSITION'                                           691
         AG_PARLIST = 'Y'                                                 691
         CALL AXPOSITION                                                  691
C    *                                                                    691
C    Geant POSITION : SSIR ===> Y=-SSIRY <===                             691
         CALL AGSRESET                                                    692
         AG_Y = -SSIRY                                                    692
         AG_TITLE = 'SSIR'                                                692
         AG_EXNAME = 'POSITION'                                           692
         AG_PARLIST = 'Y'                                                 692
         CALL AXPOSITION                                                  692
C    *                                                                    692
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 692
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  695
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  695
C    .,0.01)                                                              695
         AG_TITLE = 'SVTD'                                                695
         AG_EXNAME = 'HITS'                                               695
         AG_TYPE=AG_MAGIC                                                 695
         CALL AGSDETV('SVTD')                                             695
C    *                                                                    695
         AG_PARLIST='Z'                                                   695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = AG_MAGIC                                              695
         AG_BIN=.001                                                      695
         AG_OPTION='S'                                                    695
         AG_FHMIN = AG_MAGIC                                              695
         AG_FHMAX = AG_MAGIC                                              695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='Y'                                                   695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = AG_MAGIC                                              695
         AG_BIN=.001                                                      695
         AG_FHMIN=AG_MAGIC                                                695
         AG_FHMAX=AG_MAGIC                                                695
         AG_OPTION='*'                                                    695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='X'                                                   695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = AG_MAGIC                                              695
         AG_BIN=.001                                                      695
         AG_FHMIN=AG_MAGIC                                                695
         AG_FHMAX=AG_MAGIC                                                695
         AG_OPTION='*'                                                    695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='PTOT'                                                695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 18                                                    695
         AG_BIN=AG_MAGIC                                                  695
         AG_OPTION='*'                                                    695
         AG_FHMIN = 0                                                     695
         AG_FHMAX = 100                                                   695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='CX'                                                  695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 10                                                    695
         AG_BIN=AG_MAGIC                                                  695
         AG_FHMIN=AG_MAGIC                                                695
         AG_FHMAX=AG_MAGIC                                                695
         AG_OPTION='*'                                                    695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='CY'                                                  695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 10                                                    695
         AG_BIN=AG_MAGIC                                                  695
         AG_FHMIN=AG_MAGIC                                                695
         AG_FHMAX=AG_MAGIC                                                695
         AG_OPTION='*'                                                    695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='CZ'                                                  695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 10                                                    695
         AG_BIN=AG_MAGIC                                                  695
         AG_FHMIN=AG_MAGIC                                                695
         AG_FHMAX=AG_MAGIC                                                695
         AG_OPTION='*'                                                    695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='SLENG'                                               695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 16                                                    695
         AG_BIN=AG_MAGIC                                                  695
         AG_OPTION='*'                                                    695
         AG_FHMIN = 0                                                     695
         AG_FHMAX = 500                                                   695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='TOF'                                                 695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 16                                                    695
         AG_BIN=AG_MAGIC                                                  695
         AG_OPTION='*'                                                    695
         AG_FHMIN = 0                                                     695
         AG_FHMAX = 1.0E-6                                                695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='STEP'                                                695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = AG_MAGIC                                              695
         AG_BIN=.01                                                       695
         AG_FHMIN=AG_MAGIC                                                695
         AG_FHMAX=AG_MAGIC                                                695
         AG_OPTION='*'                                                    695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         AG_PARLIST='ELOSS'                                               695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 22                                                    695
         AG_BIN=AG_MAGIC                                                  695
         AG_OPTION='*'                                                    695
         AG_FHMIN = 0.                                                    695
         AG_FHMAX = 0.01                                                  695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         CALL AGSDETU('SVTD')                                             695
C    *                                                                    695
5761  CALL AGSPOPB                                                        696
      GO TO BLOCK_SVTD, (5760 ,5770)                                      696
C End of Local Procedure                                                  696
C ----------------------------------------------------------------------  697
C ------------ SSID                                                       697
C Begin Local Procedure SSID                                              698
C SSID IS A NON-SENSITIVE LEFT-RIGHT BORDER OF THE WAFER                  698
5800  CONTINUE                                                            698
         AG_TITLE='SSID'                                                  698
         AG_MODULE=MODULE                                                 698
         AG_MARK='V00'                                                    698
         CALL AGSPUSH                                                     698
         AG_MODULE=MODULE                                                 698
C    *                                                                    698
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          699
         CALL AGSRESET                                                    699
         AG_TITLE = 'SILICON'                                             699
         AG_EXNAME = 'MATERIAL'                                           699
         AG_PARLIST = 'NONE'                                              699
         CALL AXMATERIAL                                                  699
C    *                                                                    699
C    Geant MATERIAL : NONSENSSI ===> ISVOL=0 <===                         699
         CALL AGSRESET                                                    700
         AG_ISVOL = 0                                                     700
         AG_TITLE = 'NONSENSSI'                                           700
         AG_EXNAME = 'MATERIAL'                                           700
         AG_PARLIST = 'ISVOL'                                             700
         CALL AXMATERIAL                                                  700
C    *                                                                    700
C    Geant ATTRIBUTE : SSID ===> SEEN=1 COLO=1 <===                       700
         CALL AGSRESET                                                    701
         AG_SEEN = 1                                                      701
         AG_COLO = 1                                                      701
         AG_TITLE = 'SSID'                                                701
         AG_EXNAME = 'ATTRIBUTE'                                          701
         AG_PARLIST = 'SEEN_COLO'                                         701
         CALL AXATTRIBUTE                                                 701
C    *                                                                    701
C    Geant SHAPE : BOX ===> DX=(SWCA_WAFERLEN-SWCA_SENSLEN)/4.0, DY=SWCA  701
C    _WAFERWID/2.0 <===                                                   701
         CALL AGSRESET                                                    703
         AG_DX = (SWCA_WAFERLEN-SWCA_SENSLEN)/4.0                         703
         AG_DY = SWCA_WAFERWID/2.0                                        703
         AG_TITLE = 'BOX'                                                 703
         AG_EXNAME = 'SHAPE'                                              703
         AG_PARLIST = 'DX_DY'                                             703
         CALL AXSHAPE                                                     703
C    *                                                                    703
5801  CALL AGSPOPB                                                        704
      GO TO BLOCK_SSID, (5800 ,5810)                                      704
C End of Local Procedure                                                  704
C ----------------------------------------------------------------------  705
C ------------ SSIR                                                       705
C Begin Local Procedure SSIR                                              706
C SSIR IS A NON-SENSITIVE UP-DOWN BORDER OF THE WAFER                     706
5820  CONTINUE                                                            706
         AG_TITLE='SSIR'                                                  706
         AG_MODULE=MODULE                                                 706
         AG_MARK='V00'                                                    706
         CALL AGSPUSH                                                     706
         AG_MODULE=MODULE                                                 706
C    *                                                                    706
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          707
         CALL AGSRESET                                                    707
         AG_TITLE = 'SILICON'                                             707
         AG_EXNAME = 'MATERIAL'                                           707
         AG_PARLIST = 'NONE'                                              707
         CALL AXMATERIAL                                                  707
C    *                                                                    707
C    Geant MATERIAL : NONSENSSIL ===> ISVOL=0 <===                        707
         CALL AGSRESET                                                    708
         AG_ISVOL = 0                                                     708
         AG_TITLE = 'NONSENSSIL'                                          708
         AG_EXNAME = 'MATERIAL'                                           708
         AG_PARLIST = 'ISVOL'                                             708
         CALL AXMATERIAL                                                  708
C    *                                                                    708
C    Geant ATTRIBUTE : SSIR ===> SEEN=1 COLO=1 <===                       708
         CALL AGSRESET                                                    709
         AG_SEEN = 1                                                      709
         AG_COLO = 1                                                      709
         AG_TITLE = 'SSIR'                                                709
         AG_EXNAME = 'ATTRIBUTE'                                          709
         AG_PARLIST = 'SEEN_COLO'                                         709
         CALL AXATTRIBUTE                                                 709
C    *                                                                    709
C    Geant SHAPE : BOX ===> DX=SWCA_SENSLEN/2.0 DY=(SWCA_WAFERWID-SWCA_S  709
C    ENSWID)/4.0 <===                                                     709
         CALL AGSRESET                                                    710
         AG_DX = SWCA_SENSLEN/2.0                                         710
         AG_DY = (SWCA_WAFERWID-SWCA_SENSWID)/4.0                         710
         AG_TITLE = 'BOX'                                                 710
         AG_EXNAME = 'SHAPE'                                              710
         AG_PARLIST = 'DX_DY'                                             710
         CALL AXSHAPE                                                     710
C    *                                                                    710
5821  CALL AGSPOPB                                                        711
      GO TO BLOCK_SSIR, (5820 ,5830)                                      711
C End of Local Procedure                                                  711
C ----------------------------------------------------------------------  712
C ------------ STRA                                                       712
C Begin Local Procedure STRA                                              713
C STRA IS A TRAPEZOID OF TRIANGULAR SHAPE                                 713
5780  CONTINUE                                                            713
         AG_TITLE='STRA'                                                  713
         AG_MODULE=MODULE                                                 713
         AG_MARK='V00'                                                    713
         CALL AGSPUSH                                                     713
         AG_MODULE=MODULE                                                 713
C    *                                                                    713
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          714
         CALL AGSRESET                                                    714
         AG_TITLE = 'SILICON'                                             714
         AG_EXNAME = 'MATERIAL'                                           714
         AG_PARLIST = 'NONE'                                              714
         CALL AXMATERIAL                                                  714
C    *                                                                    714
C    Geant MATERIAL : NONSENSSILI ===> ISVOL=0 <===                       714
         CALL AGSRESET                                                    715
         AG_ISVOL = 0                                                     715
         AG_TITLE = 'NONSENSSILI'                                         715
         AG_EXNAME = 'MATERIAL'                                           715
         AG_PARLIST = 'ISVOL'                                             715
         CALL AXMATERIAL                                                  715
C    *                                                                    715
C    Geant ATTRIBUTE : STRA ===> SEEN=1 COLO=2 <===                       715
         CALL AGSRESET                                                    716
         AG_SEEN = 1                                                      716
         AG_COLO = 2                                                      716
         AG_TITLE = 'STRA'                                                716
         AG_EXNAME = 'ATTRIBUTE'                                          716
         AG_PARLIST = 'SEEN_COLO'                                         716
         CALL AXATTRIBUTE                                                 716
C    *                                                                    716
C    Geant SHAPE : TRD1 ===> DX1=0.0 DX2=SWCA_SENSWID/2.0, DY=SWCA_WAFER  716
C    THK/2.0 DZ=(SWCA_SENSLEN-SWCA_SENSGIR)/2.0 <===                      716
         CALL AGSRESET                                                    718
         AG_DX1 = 0.0                                                     718
         AG_DX2 = SWCA_SENSWID/2.0                                        718
         AG_DY = SWCA_WAFERTHK/2.0                                        718
         AG_DZ = (SWCA_SENSLEN-SWCA_SENSGIR)/2.0                          718
         AG_TITLE = 'TRD1'                                                718
         AG_EXNAME = 'SHAPE'                                              718
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     718
         CALL AXSHAPE                                                     718
C    *                                                                    718
5781  CALL AGSPOPB                                                        719
      GO TO BLOCK_STRA, (5780 ,5790)                                      719
C End of Local Procedure                                                  719
C ----------------------------------------------------------------------  720
C ------------ SBER                                                       720
C Begin Local Procedure SBER                                              721
C SBER ARE THE CARBON WAFER CARRIER RAILS                                 721
5640  CONTINUE                                                            721
         AG_TITLE='SBER'                                                  721
         AG_MODULE=MODULE                                                 721
         AG_MARK='V00'                                                    721
         CALL AGSPUSH                                                     721
         AG_MODULE=MODULE                                                 721
C    *                                                                    721
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           722
         CALL AGSRESET                                                    722
         AG_TITLE = 'CARBON'                                              722
         AG_EXNAME = 'MATERIAL'                                           722
         AG_PARLIST = 'NONE'                                              722
         CALL AXMATERIAL                                                  722
C    *                                                                    722
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       722
         CALL AGSRESET                                                    723
         AG_SEEN = 1                                                      723
         AG_COLO = 2                                                      723
         AG_TITLE = 'SBER'                                                723
         AG_EXNAME = 'ATTRIBUTE'                                          723
         AG_PARLIST = 'SEEN_COLO'                                         723
         CALL AXATTRIBUTE                                                 723
C    *                                                                    723
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  723
C    _WAFCARTH/2 <===                                                     723
         CALL AGSRESET                                                    725
         AG_DX = SWCA_WAFCARWD/2                                          725
         AG_DY = SWCA_LENGTH/2                                            725
         AG_DZ = SWCA_WAFCARTH/2                                          725
         AG_TITLE = 'BOX'                                                 725
         AG_EXNAME = 'SHAPE'                                              725
         AG_PARLIST = 'DX_DY_DZ'                                          725
         CALL AXSHAPE                                                     725
C    *                                                                    725
5641  CALL AGSPOPB                                                        726
      GO TO BLOCK_SBER, (5640 ,5650)                                      726
C End of Local Procedure                                                  726
C ----------------------------------------------------------------------  727
C ------------ STAB                                                       727
C Begin Local Procedure STAB                                              728
C STAB ARE THE CARBON WAFER CARRIER END TABS                              728
5660  CONTINUE                                                            728
         AG_TITLE='STAB'                                                  728
         AG_MODULE=MODULE                                                 728
         AG_MARK='V00'                                                    728
         CALL AGSPUSH                                                     728
         AG_MODULE=MODULE                                                 728
C    *                                                                    728
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           729
         CALL AGSRESET                                                    729
         AG_TITLE = 'CARBON'                                              729
         AG_EXNAME = 'MATERIAL'                                           729
         AG_PARLIST = 'NONE'                                              729
         CALL AXMATERIAL                                                  729
C    *                                                                    729
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       729
         CALL AGSRESET                                                    730
         AG_SEEN = 1                                                      730
         AG_COLO = 2                                                      730
         AG_TITLE = 'STAB'                                                730
         AG_EXNAME = 'ATTRIBUTE'                                          730
         AG_PARLIST = 'SEEN_COLO'                                         730
         CALL AXATTRIBUTE                                                 730
C    *                                                                    730
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  730
C    2 DZ=SWCA_WAFCARTH/2 <===                                            730
         CALL AGSRESET                                                    732
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            732
         AG_DY = TABLEN/2                                                 732
         AG_DZ = SWCA_WAFCARTH/2                                          732
         AG_TITLE = 'BOX'                                                 732
         AG_EXNAME = 'SHAPE'                                              732
         AG_PARLIST = 'DX_DY_DZ'                                          732
         CALL AXSHAPE                                                     732
C    *                                                                    732
5661  CALL AGSPOPB                                                        733
      GO TO BLOCK_STAB, (5660 ,5670)                                      733
C End of Local Procedure                                                  733
C ----------------------------------------------------------------------  734
C ------------ STRU                                                       734
C Begin Local Procedure STRU                                              735
C STRU ARE THE CARBON STRUTS BETWEEN THE WAFER CARRIER RAILS              735
5680  CONTINUE                                                            735
         AG_TITLE='STRU'                                                  735
         AG_MODULE=MODULE                                                 735
         AG_MARK='V00'                                                    735
         CALL AGSPUSH                                                     735
         AG_MODULE=MODULE                                                 735
C    *                                                                    735
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           736
         CALL AGSRESET                                                    736
         AG_TITLE = 'CARBON'                                              736
         AG_EXNAME = 'MATERIAL'                                           736
         AG_PARLIST = 'NONE'                                              736
         CALL AXMATERIAL                                                  736
C    *                                                                    736
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       736
         CALL AGSRESET                                                    737
         AG_SEEN = 1                                                      737
         AG_COLO = 2                                                      737
         AG_TITLE = 'STRU'                                                737
         AG_EXNAME = 'ATTRIBUTE'                                          737
         AG_PARLIST = 'SEEN_COLO'                                         737
         CALL AXATTRIBUTE                                                 737
C    *                                                                    737
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  737
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     737
         CALL AGSRESET                                                    739
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            739
         AG_DY = SWCA_STRUTLEN/2                                          739
         AG_DZ = SWCA_WAFCARTH/2                                          739
         AG_TITLE = 'BOX'                                                 739
         AG_EXNAME = 'SHAPE'                                              739
         AG_PARLIST = 'DX_DY_DZ'                                          739
         CALL AXSHAPE                                                     739
C    *                                                                    739
5681  CALL AGSPOPB                                                        740
      GO TO BLOCK_STRU, (5680 ,5690)                                      740
C End of Local Procedure                                                  740
C ----------------------------------------------------------------------  741
C ------------ SPCB                                                       741
C Begin Local Procedure SPCB                                              742
C SPCB IS THE G10 PCB                                                     742
5720  CONTINUE                                                            742
         AG_TITLE='SPCB'                                                  742
         AG_MODULE=MODULE                                                 742
         AG_MARK='V00'                                                    742
         CALL AGSPUSH                                                     742
         AG_MODULE=MODULE                                                 742
C    *                                                                    742
C    Geant MATERIAL : G10 ===> inherited arguments used <===              743
         CALL AGSRESET                                                    743
         AG_TITLE = 'G10'                                                 743
         AG_EXNAME = 'MATERIAL'                                           743
         AG_PARLIST = 'NONE'                                              743
         CALL AXMATERIAL                                                  743
C    *                                                                    743
C    Geant ATTRIBUTE : SPCB ===> SEEN=1 COLO=5 <===                       743
         CALL AGSRESET                                                    744
         AG_SEEN = 1                                                      744
         AG_COLO = 5                                                      744
         AG_TITLE = 'SPCB'                                                744
         AG_EXNAME = 'ATTRIBUTE'                                          744
         AG_PARLIST = 'SEEN_COLO'                                         744
         CALL AXATTRIBUTE                                                 744
C    *                                                                    744
C    Geant SHAPE : BOX ===> DX=SVTL_PCBWIDTH/2.0 DY=SVTL_PCBLEN/2.0, DZ=  744
C    SVTL_PCBTHK/2.0 <===                                                 744
         CALL AGSRESET                                                    746
         AG_DX = SVTL_PCBWIDTH/2.0                                        746
         AG_DY = SVTL_PCBLEN/2.0                                          746
         AG_DZ = SVTL_PCBTHK/2.0                                          746
         AG_TITLE = 'BOX'                                                 746
         AG_EXNAME = 'SHAPE'                                              746
         AG_PARLIST = 'DX_DY_DZ'                                          746
         CALL AXSHAPE                                                     746
C    *                                                                    746
5721  CALL AGSPOPB                                                        747
      GO TO BLOCK_SPCB, (5720 ,5730)                                      747
C End of Local Procedure                                                  747
C ----------------------------------------------------------------------  748
C ------------ SCBM                                                       748
C Begin Local Procedure SCBM                                              749
C SCBM IS THE MOTHER FOR THE BUNDLE OF CABLES GOING FROM PCBS             749
5390  CONTINUE                                                            749
         AG_TITLE='SCBM'                                                  749
         AG_MODULE=MODULE                                                 749
         AG_MARK='V00'                                                    749
         CALL AGSPUSH                                                     749
         AG_MODULE=MODULE                                                 749
C    *                                                                    749
C    Geant MATERIAL : AIR ===> inherited arguments used <===              750
         CALL AGSRESET                                                    750
         AG_TITLE = 'AIR'                                                 750
         AG_EXNAME = 'MATERIAL'                                           750
         AG_PARLIST = 'NONE'                                              750
         CALL AXMATERIAL                                                  750
C    *                                                                    750
C    Geant ATTRIBUTE : SCBM ===> SEEN=0 COLO=1 <===                       750
         CALL AGSRESET                                                    751
         AG_SEEN = 0                                                      751
         AG_COLO = 1                                                      751
         AG_TITLE = 'SCBM'                                                751
         AG_EXNAME = 'ATTRIBUTE'                                          751
         AG_PARLIST = 'SEEN_COLO'                                         751
         CALL AXATTRIBUTE                                                 751
C    *                                                                    751
C    Geant SHAPE : TUBE ===> RMIN=RADII(1) RMAX=SWAM_RMAX, DZ=SCBP_LEN <  751
C    ===                                                                  751
         CALL AGSRESET                                                    753
         AG_RMIN = RADII(1)                                               753
         AG_RMAX = SWAM_RMAX                                              753
         AG_DZ = SCBP_LEN                                                 753
         AG_TITLE = 'TUBE'                                                753
         AG_EXNAME = 'SHAPE'                                              753
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      753
         CALL AXSHAPE                                                     753
C    *                                                                    753
C    *                                                                    753
         ILAYER=1                                                         754
            GO TO 5843                                                    754
5841        ILAYER=ILAYER+1                                               754
5843        IF(ILAYER-(4).GT.0) GO TO 5842                                754
C       use SCBP bank                                                     755
            BPATH_SCBP='SCBP*'                                            755
            IDNUM_SCBP(1)=0                                               755
            SCBP_LAYER=ILAYER                                             755
            CALL AGDGETP('SVTTGEO10','*',-999,'SCBP',NSCBP_LAYER,         755
     *      RE_SCBP_LAYER,BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,    755
     *      BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)             755
C       Create SCBL :                                                     756
C       Call To Local Procedure SCBL                                      756
            ASSIGN 5860 TO BLOCK_SCBL                                     756
            GO TO  5850                                                   756
5860        CONTINUE                                                      756
C       *                                                                 756
C       Geant POSITION : SCBL ===> X=0.0 Y=0.0 Z=0.0 <===                 757
            CALL AGSRESET                                                 757
            AG_X = 0.0                                                    757
            AG_Y = 0.0                                                    757
            AG_Z = 0.0                                                    757
            AG_TITLE = 'SCBL'                                             757
            AG_EXNAME = 'POSITION'                                        757
            AG_PARLIST = 'X_Y_Z'                                          757
            CALL AXPOSITION                                               757
C       *                                                                 757
         GO TO 5841                                                       758
5842     CONTINUE                                                         758
C    *                                                                    759
         ILAYER=1                                                         759
            GO TO 5873                                                    759
5871        ILAYER=ILAYER+1                                               759
5873        IF(ILAYER-(3).GT.0) GO TO 5872                                759
C       use SFEP bank                                                     760
            BPATH_SFEP='SFEP*'                                            760
            IDNUM_SFEP(1)=0                                               760
            SFEP_LAYER=ILAYER                                             760
            CALL AGDGETP('SVTTGEO10','*',-999,'SFEP',NSFEP_LAYER,         760
     *      RE_SFEP_LAYER,BPATH_SFEP,IDNUM_SFEP,1,MAP_SFEP,NAMES_SFEP,    760
     *      BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)             760
C       Create SFED :                                                     761
C       Call To Local Procedure SFED                                      761
            ASSIGN 5890 TO BLOCK_SFED                                     761
            GO TO  5880                                                   761
5890        CONTINUE                                                      761
C       *                                                                 761
C       Geant POSITION : SFED ===> X=0.0 Y=0.0 Z=0.0 <===                 762
            CALL AGSRESET                                                 762
            AG_X = 0.0                                                    762
            AG_Y = 0.0                                                    762
            AG_Z = 0.0                                                    762
            AG_TITLE = 'SFED'                                             762
            AG_EXNAME = 'POSITION'                                        762
            AG_PARLIST = 'X_Y_Z'                                          762
            CALL AXPOSITION                                               762
C       *                                                                 762
C       Create SPLS :                                                     763
C       Call To Local Procedure SPLS                                      763
            ASSIGN 5910 TO BLOCK_SPLS                                     763
            GO TO  5900                                                   763
5910        CONTINUE                                                      763
C       *                                                                 763
C       Geant POSITION : SPLS ===> X=0.0 Y=0.0 Z=0.0 <===                 764
            CALL AGSRESET                                                 764
            AG_X = 0.0                                                    764
            AG_Y = 0.0                                                    764
            AG_Z = 0.0                                                    764
            AG_TITLE = 'SPLS'                                             764
            AG_EXNAME = 'POSITION'                                        764
            AG_PARLIST = 'X_Y_Z'                                          764
            CALL AXPOSITION                                               764
C       *                                                                 764
         GO TO 5871                                                       765
5872     CONTINUE                                                         765
5391  CALL AGSPOPB                                                        766
      GO TO BLOCK_SCBM, (5390 ,5400)                                      766
C End of Local Procedure                                                  766
C ----------------------------------------------------------------------  767
C ------------ SCBL                                                       767
C Begin Local Procedure SCBL                                              768
C SCBL IS THE BUNDLE OF CABLES GOING FROM PCBS TO MANIFOLDS               768
5850  CONTINUE                                                            768
         AG_TITLE='SCBL'                                                  768
         AG_MODULE=MODULE                                                 768
         AG_MARK='V00'                                                    768
         CALL AGSPUSH                                                     768
         AG_MODULE=MODULE                                                 768
C    *                                                                    768
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           769
         CALL AGSRESET                                                    769
         AG_TITLE = 'COPPER'                                              769
         AG_EXNAME = 'MATERIAL'                                           769
         AG_PARLIST = 'NONE'                                              769
         CALL AXMATERIAL                                                  769
C    *                                                                    769
C    Geant ATTRIBUTE : SCBL ===> SEEN=1 COLO=3 <===                       769
         CALL AGSRESET                                                    770
         AG_SEEN = 1                                                      770
         AG_COLO = 3                                                      770
         AG_TITLE = 'SCBL'                                                770
         AG_EXNAME = 'ATTRIBUTE'                                          770
         AG_PARLIST = 'SEEN_COLO'                                         770
         CALL AXATTRIBUTE                                                 770
C    *                                                                    770
         SQ =(SCBP_LEN*SCBP_LEN)                                          770
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  772
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  773
         A = PI                                                           774
         A=A*(SCBP_RMIN1*SCBP_RMIN1 + SCBP_RMIN2*SCBP_RMIN2)              775
         A=A*SQRT(1 + SQ)                                                 776
         CUTHK=(SCBP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            777
C    Geant SHAPE : CONE ===> DZ=SCBP_LEN RMN1=SCBP_RMIN1 RMX1=SCBP_RMIN1  779
C    +CUTHK, RMN2=SCBP_RMIN2 RMX2=SCBP_RMIN2+CUTHK <===                   779
         CALL AGSRESET                                                    779
         AG_DZ = SCBP_LEN                                                 779
         AG_RMN1 = SCBP_RMIN1                                             779
         AG_RMX1 = SCBP_RMIN1+CUTHK                                       779
         AG_RMN2 = SCBP_RMIN2                                             779
         AG_RMX2 = SCBP_RMIN2+CUTHK                                       779
         AG_TITLE = 'CONE'                                                779
         AG_EXNAME = 'SHAPE'                                              779
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            779
         CALL AXSHAPE                                                     779
C    *                                                                    779
5851  CALL AGSPOPB                                                        780
      GO TO BLOCK_SCBL, (5850 ,5860)                                      780
C End of Local Procedure                                                  780
C ----------------------------------------------------------------------  781
C ------------ SFED                                                       781
C Begin Local Procedure SFED                                              782
C SFED IS THE WATRER IN THE BUNDLE OF PIPES GOING TO MANIFOLDS            782
5880  CONTINUE                                                            782
         AG_TITLE='SFED'                                                  782
         AG_MODULE=MODULE                                                 782
         AG_MARK='V00'                                                    782
         CALL AGSPUSH                                                     782
         AG_MODULE=MODULE                                                 782
C    *                                                                    782
C    Geant MATERIAL : WATER ===> inherited arguments used <===            783
         CALL AGSRESET                                                    783
         AG_TITLE = 'WATER'                                               783
         AG_EXNAME = 'MATERIAL'                                           783
         AG_PARLIST = 'NONE'                                              783
         CALL AXMATERIAL                                                  783
C    *                                                                    783
C    Geant ATTRIBUTE : SFED ===> SEEN=1 COLO=1 <===                       783
         CALL AGSRESET                                                    784
         AG_SEEN = 1                                                      784
         AG_COLO = 1                                                      784
         AG_TITLE = 'SFED'                                                784
         AG_EXNAME = 'ATTRIBUTE'                                          784
         AG_PARLIST = 'SEEN_COLO'                                         784
         CALL AXATTRIBUTE                                                 784
C    *                                                                    784
         SQ = SFEP_LEN*SFEP_LEN                                           784
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  786
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  787
         A = PI                                                           788
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            789
         A=A*SQRT(1 + SQ)                                                 790
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            791
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1 RMX1=SFEP_RMIN1  793
C    +CUTHK, RMN2=SFEP_RMIN2 RMX2=SFEP_RMIN2+CUTHK <===                   793
         CALL AGSRESET                                                    793
         AG_DZ = SFEP_LEN                                                 793
         AG_RMN1 = SFEP_RMIN1                                             793
         AG_RMX1 = SFEP_RMIN1+CUTHK                                       793
         AG_RMN2 = SFEP_RMIN2                                             793
         AG_RMX2 = SFEP_RMIN2+CUTHK                                       793
         AG_TITLE = 'CONE'                                                793
         AG_EXNAME = 'SHAPE'                                              793
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            793
         CALL AXSHAPE                                                     793
C    *                                                                    793
5881  CALL AGSPOPB                                                        794
      GO TO BLOCK_SFED, (5880 ,5890)                                      794
C End of Local Procedure                                                  794
C ----------------------------------------------------------------------  795
C ------------ SPLS                                                       795
C Begin Local Procedure SPLS                                              796
C SPLS IS THE PLASTIC WALLS OF THE BUNDLE OF PIPES GOING TO MANIFOLDS     796
5900  CONTINUE                                                            796
         AG_TITLE='SPLS'                                                  796
         AG_MODULE=MODULE                                                 796
         AG_MARK='V00'                                                    796
         CALL AGSPUSH                                                     796
         AG_MODULE=MODULE                                                 796
C    *                                                                    796
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           797
         CALL AGSRESET                                                    797
         AG_A = 12                                                        797
         AG_Z = 6                                                         797
         AG_W = 1                                                         797
         AG_TITLE = 'C'                                                   797
         AG_EXNAME = 'COMPONENT'                                          797
         AG_PARLIST = 'A_Z_W'                                             797
         CALL AXCOMPONENT                                                 797
C    *                                                                    797
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           797
         CALL AGSRESET                                                    798
         AG_A = 1                                                         798
         AG_Z = 1                                                         798
         AG_W = 2                                                         798
         AG_TITLE = 'H2'                                                  798
         AG_EXNAME = 'COMPONENT'                                          798
         AG_PARLIST = 'A_Z_W'                                             798
         CALL AXCOMPONENT                                                 798
C    *                                                                    798
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             798
         CALL AGSRESET                                                    799
         AG_DENS = 0.935                                                  799
         AG_TITLE = 'CH2'                                                 799
         AG_EXNAME = 'MIXTURE'                                            799
         AG_PARLIST = 'DENS'                                              799
         CALL AXMIXTURE                                                   799
C    *                                                                    799
C    Geant ATTRIBUTE : SPLS ===> SEEN=1 COLO=6 <===                       799
         CALL AGSRESET                                                    800
         AG_SEEN = 1                                                      800
         AG_COLO = 6                                                      800
         AG_TITLE = 'SPLS'                                                800
         AG_EXNAME = 'ATTRIBUTE'                                          800
         AG_PARLIST = 'SEEN_COLO'                                         800
         CALL AXATTRIBUTE                                                 800
C    *                                                                    800
         SQ = SFEP_LEN*SFEP_LEN                                           800
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  802
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  803
         A = PI                                                           804
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            805
         A=A*SQRT(1 + SQ)                                                 806
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            807
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1+0.2, RMX1=SFEP_  810
C    RMIN1+0.2+CUTHK RMN2=SFEP_RMIN2+0.2, RMX2=SFEP_RMIN2+0.2+CUTHK <===  810
         CALL AGSRESET                                                    810
         AG_DZ = SFEP_LEN                                                 810
         AG_RMN1 = SFEP_RMIN1+0.2                                         810
         AG_RMX1 = SFEP_RMIN1+0.2+CUTHK                                   810
         AG_RMN2 = SFEP_RMIN2+0.2                                         810
         AG_RMX2 = SFEP_RMIN2+0.2+CUTHK                                   810
         AG_TITLE = 'CONE'                                                810
         AG_EXNAME = 'SHAPE'                                              810
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            810
         CALL AXSHAPE                                                     810
C    *                                                                    810
5901  CALL AGSPOPB                                                        811
      GO TO BLOCK_SPLS, (5900 ,5910)                                      811
C End of Local Procedure                                                  811
C ----------------------------------------------------------------------  812
C ------------ SELE                                                       812
C Begin Local Procedure SELE                                              813
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   813
5590  CONTINUE                                                            813
         AG_TITLE='SELE'                                                  813
         AG_MODULE=MODULE                                                 813
         AG_MARK='V00'                                                    813
         CALL AGSPUSH                                                     813
         AG_MODULE=MODULE                                                 813
C    *                                                                    813
C    Geant MATERIAL : AIR ===> inherited arguments used <===              814
         CALL AGSRESET                                                    814
         AG_TITLE = 'AIR'                                                 814
         AG_EXNAME = 'MATERIAL'                                           814
         AG_PARLIST = 'NONE'                                              814
         CALL AXMATERIAL                                                  814
C    *                                                                    814
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 SERIAL=ILAYER <===         814
         CALL AGSRESET                                                    815
         AG_SEEN = 0                                                      815
         AG_COLO = 1                                                      815
         AG_SERIAL = ILAYER                                               815
         AG_TITLE = 'SELE'                                                815
         AG_EXNAME = 'ATTRIBUTE'                                          815
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  815
         CALL AXATTRIBUTE                                                 815
C    *                                                                    815
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  815
C    K/2 <===                                                             815
         CALL AGSRESET                                                    817
         AG_DX = SELC_ELCAWID/2                                           817
         AG_DY = SWCA_LENGTH/2                                            817
         AG_DZ = ELETHK/2                                                 817
         AG_TITLE = 'BOX'                                                 817
         AG_EXNAME = 'SHAPE'                                              817
         AG_PARLIST = 'DX_DY_DZ'                                          817
         CALL AXSHAPE                                                     817
C    *                                                                    817
C    Create SWCH :                                                        818
C    Call To Local Procedure SWCH                                         818
         ASSIGN 5930 TO BLOCK_SWCH                                        818
         GO TO  5920                                                      818
5930     CONTINUE                                                         818
C    *                                                                    818
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             819
         CALL AGSRESET                                                    819
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    819
         AG_TITLE = 'SWCH'                                                819
         AG_EXNAME = 'POSITION'                                           819
         AG_PARLIST = 'Z'                                                 819
         CALL AXPOSITION                                                  819
C    *                                                                    819
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  819
C    BETHK/2 <===                                                         819
         CALL AGSRESET                                                    820
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             820
         AG_TITLE = 'SWCH'                                                820
         AG_EXNAME = 'POSITION'                                           820
         AG_PARLIST = 'Z'                                                 820
         CALL AXPOSITION                                                  820
C    *                                                                    820
C    Create SWCS :                                                        821
C    Call To Local Procedure SWCS                                         821
         ASSIGN 5950 TO BLOCK_SWCS                                        821
         GO TO  5940                                                      821
5950     CONTINUE                                                         821
C    *                                                                    821
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  823
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     823
         CALL AGSRESET                                                    823
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              823
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        823
         AG_TITLE = 'SWCS'                                                823
         AG_EXNAME = 'POSITION'                                           823
         AG_PARLIST = 'X_Z'                                               823
         CALL AXPOSITION                                                  823
C    *                                                                    823
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  823
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     823
         CALL AGSRESET                                                    825
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              825
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        825
         AG_TITLE = 'SWCS'                                                825
         AG_EXNAME = 'POSITION'                                           825
         AG_PARLIST = 'X_Z'                                               825
         CALL AXPOSITION                                                  825
C    *                                                                    825
C    Create SWCW :                                                        826
C    Call To Local Procedure SWCW                                         826
         ASSIGN 5970 TO BLOCK_SWCW                                        826
         GO TO  5960                                                      826
5970     CONTINUE                                                         826
C    *                                                                    826
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  827
C    =                                                                    827
         CALL AGSRESET                                                    827
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        827
         AG_TITLE = 'SWCW'                                                827
         AG_EXNAME = 'POSITION'                                           827
         AG_PARLIST = 'Z'                                                 827
         CALL AXPOSITION                                                  827
C    *                                                                    827
C    Create SBOI :                                                        828
C    Call To Local Procedure SBOI                                         828
         ASSIGN 5990 TO BLOCK_SBOI                                        828
         GO TO  5980                                                      828
5990     CONTINUE                                                         828
C    *                                                                    828
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  829
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       829
         CALL AGSRESET                                                    829
         AG_Z = (ELETHK/                                                  829
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          829
         AG_TITLE = 'SBOI'                                                829
         AG_EXNAME = 'POSITION'                                           829
         AG_PARLIST = 'Z'                                                 829
         CALL AXPOSITION                                                  829
C    *                                                                    829
C    Create SGLA :                                                        830
C    Call To Local Procedure SGLA                                         830
         ASSIGN 6010 TO BLOCK_SGLA                                        830
         GO TO  6000                                                      830
6010     CONTINUE                                                         830
C    *                                                                    830
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  831
C    _GLASSTHK/2 <===                                                     831
         CALL AGSRESET                                                    831
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         831
         AG_TITLE = 'SGLA'                                                831
         AG_EXNAME = 'POSITION'                                           831
         AG_PARLIST = 'Z'                                                 831
         CALL AXPOSITION                                                  831
C    *                                                                    831
C    Create SAGP :                                                        832
C    Call To Local Procedure SAGP                                         832
         ASSIGN 6030 TO BLOCK_SAGP                                        832
         GO TO  6020                                                      832
6030     CONTINUE                                                         832
C    *                                                                    832
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  833
C    ==                                                                   833
         CALL AGSRESET                                                    833
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       833
         AG_TITLE = 'SAGP'                                                833
         AG_EXNAME = 'POSITION'                                           833
         AG_PARLIST = 'Z'                                                 833
         CALL AXPOSITION                                                  833
C    *                                                                    833
C    Create SDYE :                                                        834
C    Call To Local Procedure SDYE                                         834
         ASSIGN 6050 TO BLOCK_SDYE                                        834
         GO TO  6040                                                      834
6050     CONTINUE                                                         834
C    *                                                                    834
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  836
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     836
         CALL AGSRESET                                                    836
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  836
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    836
         AG_TITLE = 'SDYE'                                                836
         AG_EXNAME = 'POSITION'                                           836
         AG_PARLIST = 'X_Z'                                               836
         CALL AXPOSITION                                                  836
C    *                                                                    836
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  836
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             836
         CALL AGSRESET                                                    838
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          838
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    838
         AG_TITLE = 'SDYE'                                                838
         AG_EXNAME = 'POSITION'                                           838
         AG_PARLIST = 'X_Z'                                               838
         CALL AXPOSITION                                                  838
C    *                                                                    838
C    Create SECA :                                                        839
C    Call To Local Procedure SECA                                         839
         ASSIGN 6070 TO BLOCK_SECA                                        839
         GO TO  6060                                                      839
6070     CONTINUE                                                         839
C    *                                                                    839
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  841
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    841
         CALL AGSRESET                                                    841
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             841
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        841
         AG_TITLE = 'SECA'                                                841
         AG_EXNAME = 'POSITION'                                           841
         AG_PARLIST = 'X_Z'                                               841
         CALL AXPOSITION                                                  841
C    *                                                                    841
5591  CALL AGSPOPB                                                        842
      GO TO BLOCK_SELE, (5590 ,5600)                                      842
C End of Local Procedure                                                  842
C ----------------------------------------------------------------------  843
C ------------ SDYE                                                       843
C Begin Local Procedure SDYE                                              844
C SDYE IS THE IC CHIP ON THE HYBRID                                       844
6040  CONTINUE                                                            844
         AG_TITLE='SDYE'                                                  844
         AG_MODULE=MODULE                                                 844
         AG_MARK='V00'                                                    844
         CALL AGSPUSH                                                     844
         AG_MODULE=MODULE                                                 844
C    *                                                                    844
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          845
         CALL AGSRESET                                                    845
         AG_TITLE = 'SILICON'                                             845
         AG_EXNAME = 'MATERIAL'                                           845
         AG_PARLIST = 'NONE'                                              845
         CALL AXMATERIAL                                                  845
C    *                                                                    845
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 SERIAL=0 <===              845
         CALL AGSRESET                                                    846
         AG_SEEN = 1                                                      846
         AG_COLO = 6                                                      846
         AG_SERIAL = 0                                                    846
         AG_TITLE = 'SDYE'                                                846
         AG_EXNAME = 'ATTRIBUTE'                                          846
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  846
         CALL AXATTRIBUTE                                                 846
C    *                                                                    846
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  846
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           846
         CALL AGSRESET                                                    848
         AG_DX = SELC_DYEWID/2                                            848
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              848
         AG_DZ = SELC_DYETHK/2                                            848
         AG_TITLE = 'BOX'                                                 848
         AG_EXNAME = 'SHAPE'                                              848
         AG_PARLIST = 'DX_DY_DZ'                                          848
         CALL AXSHAPE                                                     848
C    *                                                                    848
6041  CALL AGSPOPB                                                        849
      GO TO BLOCK_SDYE, (6040 ,6050)                                      849
C End of Local Procedure                                                  849
C ----------------------------------------------------------------------  850
C ------------ SECA                                                       850
C Begin Local Procedure SECA                                              851
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            851
6060  CONTINUE                                                            851
         AG_TITLE='SECA'                                                  851
         AG_MODULE=MODULE                                                 851
         AG_MARK='V00'                                                    851
         CALL AGSPUSH                                                     851
         AG_MODULE=MODULE                                                 851
C    *                                                                    851
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           852
         CALL AGSRESET                                                    852
         AG_TITLE = 'COPPER'                                              852
         AG_EXNAME = 'MATERIAL'                                           852
         AG_PARLIST = 'NONE'                                              852
         CALL AXMATERIAL                                                  852
C    *                                                                    852
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 SERIAL=0 <===              852
         CALL AGSRESET                                                    853
         AG_SEEN = 1                                                      853
         AG_COLO = 2                                                      853
         AG_SERIAL = 0                                                    853
         AG_TITLE = 'SECA'                                                853
         AG_EXNAME = 'ATTRIBUTE'                                          853
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  853
         CALL AXATTRIBUTE                                                 853
C    *                                                                    853
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  853
C    ABTHK/2 <===                                                         853
         CALL AGSRESET                                                    855
         AG_DX = SELC_CABWID/2                                            855
         AG_DY = SWCA_LENGTH/2                                            855
         AG_DZ = SELC_CABTHK/2                                            855
         AG_TITLE = 'BOX'                                                 855
         AG_EXNAME = 'SHAPE'                                              855
         AG_PARLIST = 'DX_DY_DZ'                                          855
         CALL AXSHAPE                                                     855
C    *                                                                    855
6061  CALL AGSPOPB                                                        856
      GO TO BLOCK_SECA, (6060 ,6070)                                      856
C End of Local Procedure                                                  856
C ----------------------------------------------------------------------  857
C ------------ SBOI                                                       857
C Begin Local Procedure SBOI                                              858
C SBOI IS THE BERILLIA LAYER                                              858
5980  CONTINUE                                                            858
         AG_TITLE='SBOI'                                                  858
         AG_MODULE=MODULE                                                 858
         AG_MARK='V00'                                                    858
         CALL AGSPUSH                                                     858
         AG_MODULE=MODULE                                                 858
C    *                                                                    858
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1 <===                           859
         CALL AGSRESET                                                    859
         AG_A = 9                                                         859
         AG_Z = 4                                                         859
         AG_W = 1                                                         859
         AG_TITLE = 'BE'                                                  859
         AG_EXNAME = 'COMPONENT'                                          859
         AG_PARLIST = 'A_Z_W'                                             859
         CALL AXCOMPONENT                                                 859
C    *                                                                    859
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           859
         CALL AGSRESET                                                    860
         AG_A = 16                                                        860
         AG_Z = 8                                                         860
         AG_W = 1                                                         860
         AG_TITLE = 'O'                                                   860
         AG_EXNAME = 'COMPONENT'                                          860
         AG_PARLIST = 'A_Z_W'                                             860
         CALL AXCOMPONENT                                                 860
C    *                                                                    860
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              860
         CALL AGSRESET                                                    861
         AG_DENS = 2.85                                                   861
         AG_TITLE = 'BEO'                                                 861
         AG_EXNAME = 'MIXTURE'                                            861
         AG_PARLIST = 'DENS'                                              861
         CALL AXMIXTURE                                                   861
C    *                                                                    861
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 SERIAL=0 <===              861
         CALL AGSRESET                                                    862
         AG_SEEN = 1                                                      862
         AG_COLO = 6                                                      862
         AG_SERIAL = 0                                                    862
         AG_TITLE = 'SBOI'                                                862
         AG_EXNAME = 'ATTRIBUTE'                                          862
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  862
         CALL AXATTRIBUTE                                                 862
C    *                                                                    862
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  862
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          862
         CALL AGSRESET                                                    864
         AG_DX = SELC_ELCAWID/2                                           864
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              864
         AG_DZ = SELC_BEOTHK/2                                            864
         AG_TITLE = 'BOX'                                                 864
         AG_EXNAME = 'SHAPE'                                              864
         AG_PARLIST = 'DX_DY_DZ'                                          864
         CALL AXSHAPE                                                     864
C    *                                                                    864
5981  CALL AGSPOPB                                                        865
      GO TO BLOCK_SBOI, (5980 ,5990)                                      865
C End of Local Procedure                                                  865
C ----------------------------------------------------------------------  866
C ------------ SAGP                                                       866
C Begin Local Procedure SAGP                                              867
C SAGP IS THE SILVER-PALLADIUM LAYER                                      867
6020  CONTINUE                                                            867
         AG_TITLE='SAGP'                                                  867
         AG_MODULE=MODULE                                                 867
         AG_MARK='V00'                                                    867
         CALL AGSPUSH                                                     867
         AG_MODULE=MODULE                                                 867
C    *                                                                    867
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1 <===                        868
         CALL AGSRESET                                                    868
         AG_A = 108                                                       868
         AG_Z = 47                                                        868
         AG_W = 1                                                         868
         AG_TITLE = 'AG'                                                  868
         AG_EXNAME = 'COMPONENT'                                          868
         AG_PARLIST = 'A_Z_W'                                             868
         CALL AXCOMPONENT                                                 868
C    *                                                                    868
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1 <===                        868
         CALL AGSRESET                                                    869
         AG_A = 106                                                       869
         AG_Z = 46                                                        869
         AG_W = 1                                                         869
         AG_TITLE = 'PD'                                                  869
         AG_EXNAME = 'COMPONENT'                                          869
         AG_PARLIST = 'A_Z_W'                                             869
         CALL AXCOMPONENT                                                 869
C    *                                                                    869
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            869
         CALL AGSRESET                                                    870
         AG_DENS = 11.25                                                  870
         AG_TITLE = 'AGPD'                                                870
         AG_EXNAME = 'MIXTURE'                                            870
         AG_PARLIST = 'DENS'                                              870
         CALL AXMIXTURE                                                   870
C    *                                                                    870
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 SERIAL=0 <===              870
         CALL AGSRESET                                                    871
         AG_SEEN = 1                                                      871
         AG_COLO = 2                                                      871
         AG_SERIAL = 0                                                    871
         AG_TITLE = 'SAGP'                                                871
         AG_EXNAME = 'ATTRIBUTE'                                          871
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  871
         CALL AXATTRIBUTE                                                 871
C    *                                                                    871
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  871
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         871
         CALL AGSRESET                                                    873
         AG_DX = SELC_ELCAWID/2                                           873
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              873
         AG_DZ = SELC_AGPDTHK/2                                           873
         AG_TITLE = 'BOX'                                                 873
         AG_EXNAME = 'SHAPE'                                              873
         AG_PARLIST = 'DX_DY_DZ'                                          873
         CALL AXSHAPE                                                     873
C    *                                                                    873
6021  CALL AGSPOPB                                                        874
      GO TO BLOCK_SAGP, (6020 ,6030)                                      874
C End of Local Procedure                                                  874
C ----------------------------------------------------------------------  875
C ------------ SGLA                                                       875
C Begin Local Procedure SGLA                                              876
C SGLA IS THE INSULATING GLASS LAYER                                      876
6000  CONTINUE                                                            876
         AG_TITLE='SGLA'                                                  876
         AG_MODULE=MODULE                                                 876
         AG_MARK='V00'                                                    876
         CALL AGSPUSH                                                     876
         AG_MODULE=MODULE                                                 876
C    *                                                                    876
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         877
         CALL AGSRESET                                                    877
         AG_A = 28                                                        877
         AG_Z = 14                                                        877
         AG_W = 1                                                         877
         AG_TITLE = 'SI'                                                  877
         AG_EXNAME = 'COMPONENT'                                          877
         AG_PARLIST = 'A_Z_W'                                             877
         CALL AXCOMPONENT                                                 877
C    *                                                                    877
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           877
         CALL AGSRESET                                                    878
         AG_A = 16                                                        878
         AG_Z = 8                                                         878
         AG_W = 2                                                         878
         AG_TITLE = 'O'                                                   878
         AG_EXNAME = 'COMPONENT'                                          878
         AG_PARLIST = 'A_Z_W'                                             878
         CALL AXCOMPONENT                                                 878
C    *                                                                    878
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             878
         CALL AGSRESET                                                    879
         AG_DENS = 2.2                                                    879
         AG_TITLE = 'GLASS'                                               879
         AG_EXNAME = 'MIXTURE'                                            879
         AG_PARLIST = 'DENS'                                              879
         CALL AXMIXTURE                                                   879
C    *                                                                    879
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 SERIAL=0 <===              879
         CALL AGSRESET                                                    880
         AG_SEEN = 1                                                      880
         AG_COLO = 6                                                      880
         AG_SERIAL = 0                                                    880
         AG_TITLE = 'SGLA'                                                880
         AG_EXNAME = 'ATTRIBUTE'                                          880
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  880
         CALL AXATTRIBUTE                                                 880
C    *                                                                    880
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  880
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        880
         CALL AGSRESET                                                    882
         AG_DX = SELC_ELCAWID/2                                           882
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              882
         AG_DZ = SELC_GLASSTHK/2                                          882
         AG_TITLE = 'BOX'                                                 882
         AG_EXNAME = 'SHAPE'                                              882
         AG_PARLIST = 'DX_DY_DZ'                                          882
         CALL AXSHAPE                                                     882
C    *                                                                    882
6001  CALL AGSPOPB                                                        883
      GO TO BLOCK_SGLA, (6000 ,6010)                                      883
C End of Local Procedure                                                  883
C ----------------------------------------------------------------------  884
C ------------ SWCH                                                       884
C Begin Local Procedure SWCH                                              885
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      885
5920  CONTINUE                                                            885
         AG_TITLE='SWCH'                                                  885
         AG_MODULE=MODULE                                                 885
         AG_MARK='V00'                                                    885
         CALL AGSPUSH                                                     885
         AG_MODULE=MODULE                                                 885
C    *                                                                    885
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           886
         CALL AGSRESET                                                    886
         AG_TITLE = 'CARBON'                                              886
         AG_EXNAME = 'MATERIAL'                                           886
         AG_PARLIST = 'NONE'                                              886
         CALL AXMATERIAL                                                  886
C    *                                                                    886
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 SERIAL=0 <===              886
         CALL AGSRESET                                                    887
         AG_SEEN = 1                                                      887
         AG_COLO = 2                                                      887
         AG_SERIAL = 0                                                    887
         AG_TITLE = 'SWCH'                                                887
         AG_EXNAME = 'ATTRIBUTE'                                          887
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  887
         CALL AXATTRIBUTE                                                 887
C    *                                                                    887
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  887
C    BETHK/2 <===                                                         887
         CALL AGSRESET                                                    889
         AG_DX = SELC_ELCAWID/2                                           889
         AG_DY = SWCA_LENGTH/2                                            889
         AG_DZ = SELC_BETHK/2                                             889
         AG_TITLE = 'BOX'                                                 889
         AG_EXNAME = 'SHAPE'                                              889
         AG_PARLIST = 'DX_DY_DZ'                                          889
         CALL AXSHAPE                                                     889
C    *                                                                    889
5921  CALL AGSPOPB                                                        890
      GO TO BLOCK_SWCH, (5920 ,5930)                                      890
C End of Local Procedure                                                  890
C ----------------------------------------------------------------------  891
C ------------ SWCS                                                       891
C Begin Local Procedure SWCS                                              892
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                892
5940  CONTINUE                                                            892
         AG_TITLE='SWCS'                                                  892
         AG_MODULE=MODULE                                                 892
         AG_MARK='V00'                                                    892
         CALL AGSPUSH                                                     892
         AG_MODULE=MODULE                                                 892
C    *                                                                    892
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           893
         CALL AGSRESET                                                    893
         AG_TITLE = 'CARBON'                                              893
         AG_EXNAME = 'MATERIAL'                                           893
         AG_PARLIST = 'NONE'                                              893
         CALL AXMATERIAL                                                  893
C    *                                                                    893
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 SERIAL=0 <===              893
         CALL AGSRESET                                                    894
         AG_SEEN = 1                                                      894
         AG_COLO = 2                                                      894
         AG_SERIAL = 0                                                    894
         AG_TITLE = 'SWCS'                                                894
         AG_EXNAME = 'ATTRIBUTE'                                          894
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  894
         CALL AXATTRIBUTE                                                 894
C    *                                                                    894
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  894
C    TTHK/2 <===                                                          894
         CALL AGSRESET                                                    896
         AG_DX = SELC_BETHK/2                                             896
         AG_DY = SWCA_LENGTH/2                                            896
         AG_DZ = SELC_WATTHK/2                                            896
         AG_TITLE = 'BOX'                                                 896
         AG_EXNAME = 'SHAPE'                                              896
         AG_PARLIST = 'DX_DY_DZ'                                          896
         CALL AXSHAPE                                                     896
C    *                                                                    896
5941  CALL AGSPOPB                                                        897
      GO TO BLOCK_SWCS, (5940 ,5950)                                      897
C End of Local Procedure                                                  897
C ----------------------------------------------------------------------  898
C ------------ SWCW                                                       898
C Begin Local Procedure SWCW                                              899
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       899
5960  CONTINUE                                                            899
         AG_TITLE='SWCW'                                                  899
         AG_MODULE=MODULE                                                 899
         AG_MARK='V00'                                                    899
         CALL AGSPUSH                                                     899
         AG_MODULE=MODULE                                                 899
C    *                                                                    899
C    Geant MATERIAL : WATER ===> inherited arguments used <===            900
         CALL AGSRESET                                                    900
         AG_TITLE = 'WATER'                                               900
         AG_EXNAME = 'MATERIAL'                                           900
         AG_PARLIST = 'NONE'                                              900
         CALL AXMATERIAL                                                  900
C    *                                                                    900
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 SERIAL=0 <===              900
         CALL AGSRESET                                                    901
         AG_SEEN = 1                                                      901
         AG_COLO = 6                                                      901
         AG_SERIAL = 0                                                    901
         AG_TITLE = 'SWCW'                                                901
         AG_EXNAME = 'ATTRIBUTE'                                          901
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  901
         CALL AXATTRIBUTE                                                 901
C    *                                                                    901
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  901
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     901
         CALL AGSRESET                                                    903
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          903
         AG_DY = SWCA_LENGTH/2                                            903
         AG_DZ = (SELC_WATTHK/2)                                          903
         AG_TITLE = 'BOX'                                                 903
         AG_EXNAME = 'SHAPE'                                              903
         AG_PARLIST = 'DX_DY_DZ'                                          903
         CALL AXSHAPE                                                     903
C    *                                                                    903
5961  CALL AGSPOPB                                                        904
      GO TO BLOCK_SWCW, (5960 ,5970)                                      904
C End of Local Procedure                                                  904
C ----------------------------------------------------------------------  905
C ------------ SIRT                                                       905
C Begin Local Procedure SIRT                                              906
C SIRT IS THE SVT INNER END RING TUBE                                     906
5070  CONTINUE                                                            906
         AG_TITLE='SIRT'                                                  906
         AG_MODULE=MODULE                                                 906
         AG_MARK='V00'                                                    906
         CALL AGSPUSH                                                     906
         AG_MODULE=MODULE                                                 906
C    *                                                                    906
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        907
         CALL AGSRESET                                                    907
         AG_TITLE = 'BERILLIUM'                                           907
         AG_EXNAME = 'MATERIAL'                                           907
         AG_PARLIST = 'NONE'                                              907
         CALL AXMATERIAL                                                  907
C    *                                                                    907
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       907
         CALL AGSRESET                                                    908
         AG_SEEN = 1                                                      908
         AG_COLO = 2                                                      908
         AG_TITLE = 'SIRT'                                                908
         AG_EXNAME = 'ATTRIBUTE'                                          908
         AG_PARLIST = 'SEEN_COLO'                                         908
         CALL AXATTRIBUTE                                                 908
C    *                                                                    908
         IR_RMIN = COS(PI/8.0)                                            908
         IR_RMIN=IR_RMIN+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/           910
     *   8.0)*SIN(PI/8.0))                                                910
         IR_RMIN=IR_RMIN*SERG_IRNGPRMN                                    911
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  913
C    DRNGTH/2 <===                                                        913
         CALL AGSRESET                                                    913
         AG_RMIN = IR_RMIN                                                913
         AG_RMAX = SERG_IRNGTRMX                                          913
         AG_DZ = SERG_ENDRNGTH/2                                          913
         AG_TITLE = 'TUBE'                                                913
         AG_EXNAME = 'SHAPE'                                              913
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      913
         CALL AXSHAPE                                                     913
C    *                                                                    913
5071  CALL AGSPOPB                                                        914
      GO TO BLOCK_SIRT, (5070 ,5080)                                      914
C End of Local Procedure                                                  914
C ----------------------------------------------------------------------  915
C ------------ SIRP                                                       915
C Begin Local Procedure SIRP                                              916
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 916
5050  CONTINUE                                                            916
         AG_TITLE='SIRP'                                                  916
         AG_MODULE=MODULE                                                 916
         AG_MARK='V00'                                                    916
         CALL AGSPUSH                                                     916
         AG_MODULE=MODULE                                                 916
C    *                                                                    916
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        917
         CALL AGSRESET                                                    917
         AG_TITLE = 'BERILLIUM'                                           917
         AG_EXNAME = 'MATERIAL'                                           917
         AG_PARLIST = 'NONE'                                              917
         CALL AXMATERIAL                                                  917
C    *                                                                    917
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       917
         CALL AGSRESET                                                    918
         AG_SEEN = 1                                                      918
         AG_COLO = 2                                                      918
         AG_TITLE = 'SIRP'                                                918
         AG_EXNAME = 'ATTRIBUTE'                                          918
         AG_PARLIST = 'SEEN_COLO'                                         918
         CALL AXATTRIBUTE                                                 918
C    *                                                                    918
         ROU = COS(PI/8.0)                                                918
         ROU=ROU+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/8.0)*SIN(PI/       920
     *   8.0))                                                            920
         ROU = SERG_IRNGTRMX/ROU                                          921
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  924
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  924
C    OU,ROU} <===                                                         924
         CALL AGSRESET                                                    924
         AG_PHI1 = 0                                                      924
         AG_DPHI = 360                                                    924
         AG_NPDIV = 8                                                     924
         AG_NZ = 2                                                        924
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        924
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        924
         AG_RMN(1)=SERG_IRNGPRMN                                          924
         AG_RMN(2)=SERG_IRNGPRMN                                          924
         AG_RMX(1)=ROU                                                    924
         AG_RMX(2)=ROU                                                    924
         AG_TITLE = 'PGON'                                                924
         AG_EXNAME = 'SHAPE'                                              924
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     924
         CALL AXSHAPE                                                     924
C    *                                                                    924
5051  CALL AGSPOPB                                                        925
      GO TO BLOCK_SIRP, (5050 ,5060)                                      925
C End of Local Procedure                                                  925
C ----------------------------------------------------------------------  926
C ------------ SOER                                                       926
C Begin Local Procedure SOER                                              927
C SOER IS THE SVT OUTER END RING                                          927
5090  CONTINUE                                                            927
         AG_TITLE='SOER'                                                  927
         AG_MODULE=MODULE                                                 927
         AG_MARK='V00'                                                    927
         CALL AGSPUSH                                                     927
         AG_MODULE=MODULE                                                 927
C    *                                                                    927
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        928
         CALL AGSRESET                                                    928
         AG_TITLE = 'BERILLIUM'                                           928
         AG_EXNAME = 'MATERIAL'                                           928
         AG_PARLIST = 'NONE'                                              928
         CALL AXMATERIAL                                                  928
C    *                                                                    928
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       928
         CALL AGSRESET                                                    929
         AG_SEEN = 1                                                      929
         AG_COLO = 2                                                      929
         AG_TITLE = 'SOER'                                                929
         AG_EXNAME = 'ATTRIBUTE'                                          929
         AG_PARLIST = 'SEEN_COLO'                                         929
         CALL AXATTRIBUTE                                                 929
C    *                                                                    929
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  929
C    ERG_ENDRNGTH/2 <===                                                  929
         CALL AGSRESET                                                    931
         AG_RMIN = SERG_ORNGRMIN                                          931
         AG_RMAX = SERG_ORNGRMAX                                          931
         AG_DZ = SERG_ENDRNGTH/2                                          931
         AG_TITLE = 'TUBE'                                                931
         AG_EXNAME = 'SHAPE'                                              931
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      931
         CALL AXSHAPE                                                     931
C    *                                                                    931
5091  CALL AGSPOPB                                                        932
      GO TO BLOCK_SOER, (5090 ,5100)                                      932
C End of Local Procedure                                                  932
C ----------------------------------------------------------------------  933
C ------------ SIES                                                       933
C Begin Local Procedure SIES                                              934
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         934
5150  CONTINUE                                                            934
         AG_TITLE='SIES'                                                  934
         AG_MODULE=MODULE                                                 934
         AG_MARK='V00'                                                    934
         CALL AGSPUSH                                                     934
         AG_MODULE=MODULE                                                 934
C    *                                                                    934
C    Geant MATERIAL : AIR ===> inherited arguments used <===              935
         CALL AGSRESET                                                    935
         AG_TITLE = 'AIR'                                                 935
         AG_EXNAME = 'MATERIAL'                                           935
         AG_PARLIST = 'NONE'                                              935
         CALL AXMATERIAL                                                  935
C    *                                                                    935
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       935
         CALL AGSRESET                                                    936
         AG_SEEN = 0                                                      936
         AG_COLO = 1                                                      936
         AG_TITLE = 'SIES'                                                936
         AG_EXNAME = 'ATTRIBUTE'                                          936
         AG_PARLIST = 'SEEN_COLO'                                         936
         CALL AXATTRIBUTE                                                 936
C    *                                                                    936
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  936
C    ACK_Z-ENDRNG_Z) <===                                                 936
         CALL AGSRESET                                                    938
         AG_RMIN = IR_RMIN                                                938
         AG_RMAX = SERG_IRNGTRMX                                          938
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   938
         AG_TITLE = 'TUBE'                                                938
         AG_EXNAME = 'SHAPE'                                              938
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      938
         CALL AXSHAPE                                                     938
C    *                                                                    938
C    Create SISM :                                                        939
C    Call To Local Procedure SISM                                         939
         ASSIGN 6090 TO BLOCK_SISM                                        939
         GO TO  6080                                                      939
6090     CONTINUE                                                         939
C    *                                                                    939
5151  CALL AGSPOPB                                                        940
      GO TO BLOCK_SIES, (5150 ,5160)                                      940
C End of Local Procedure                                                  940
C ----------------------------------------------------------------------  941
C ------------ SISM                                                       941
C Begin Local Procedure SISM                                              942
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        942
6080  CONTINUE                                                            942
         AG_TITLE='SISM'                                                  942
         AG_MODULE=MODULE                                                 942
         AG_MARK='V00'                                                    942
         CALL AGSPUSH                                                     942
         AG_MODULE=MODULE                                                 942
C    *                                                                    942
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       943
         CALL AGSRESET                                                    943
         AG_SEEN = 0                                                      943
         AG_COLO = 1                                                      943
         AG_TITLE = 'SISM'                                                943
         AG_EXNAME = 'ATTRIBUTE'                                          943
         AG_PARLIST = 'SEEN_COLO'                                         943
         CALL AXATTRIBUTE                                                 943
C    *                                                                    943
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                943
         CALL AGSRESET                                                    944
         AG_NDIV = 4                                                      944
         AG_IAXIS = 2                                                     944
         AG_C0 = 45                                                       944
         AG_TITLE = 'DIVISION'                                            944
         AG_EXNAME = 'SHAPE'                                              944
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     944
         CALL AXSHAPE                                                     944
C    *                                                                    944
C    Create SCRW :                                                        945
C    Call To Local Procedure SCRW                                         945
         ASSIGN 6110 TO BLOCK_SCRW                                        945
         GO TO  6100                                                      945
6110     CONTINUE                                                         945
C    *                                                                    945
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  946
C    ===                                                                  946
         CALL AGSRESET                                                    946
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      946
         AG_TITLE = 'SCRW'                                                946
         AG_EXNAME = 'POSITION'                                           946
         AG_PARLIST = 'X'                                                 946
         CALL AXPOSITION                                                  946
C    *                                                                    946
6081  CALL AGSPOPB                                                        947
      GO TO BLOCK_SISM, (6080 ,6090)                                      947
C End of Local Procedure                                                  947
C ----------------------------------------------------------------------  948
C ------------ SOES                                                       948
C Begin Local Procedure SOES                                              949
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         949
5130  CONTINUE                                                            949
         AG_TITLE='SOES'                                                  949
         AG_MODULE=MODULE                                                 949
         AG_MARK='V00'                                                    949
         CALL AGSPUSH                                                     949
         AG_MODULE=MODULE                                                 949
C    *                                                                    949
C    Geant MATERIAL : AIR ===> inherited arguments used <===              950
         CALL AGSRESET                                                    950
         AG_TITLE = 'AIR'                                                 950
         AG_EXNAME = 'MATERIAL'                                           950
         AG_PARLIST = 'NONE'                                              950
         CALL AXMATERIAL                                                  950
C    *                                                                    950
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       950
         CALL AGSRESET                                                    951
         AG_SEEN = 0                                                      951
         AG_COLO = 1                                                      951
         AG_TITLE = 'SOES'                                                951
         AG_EXNAME = 'ATTRIBUTE'                                          951
         AG_PARLIST = 'SEEN_COLO'                                         951
         CALL AXATTRIBUTE                                                 951
C    *                                                                    951
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  951
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           951
         CALL AGSRESET                                                    953
         AG_RMIN = SERG_ORNGRMIN                                          953
         AG_RMAX = SERG_ORNGRMAX                                          953
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   953
         AG_TITLE = 'TUBE'                                                953
         AG_EXNAME = 'SHAPE'                                              953
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      953
         CALL AXSHAPE                                                     953
C    *                                                                    953
C    Create SOSM :                                                        954
C    Call To Local Procedure SOSM                                         954
         ASSIGN 6130 TO BLOCK_SOSM                                        954
         GO TO  6120                                                      954
6130     CONTINUE                                                         954
C    *                                                                    954
5131  CALL AGSPOPB                                                        955
      GO TO BLOCK_SOES, (5130 ,5140)                                      955
C End of Local Procedure                                                  955
C ----------------------------------------------------------------------  956
C ------------ SOSM                                                       956
C Begin Local Procedure SOSM                                              957
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        957
6120  CONTINUE                                                            957
         AG_TITLE='SOSM'                                                  957
         AG_MODULE=MODULE                                                 957
         AG_MARK='V00'                                                    957
         CALL AGSPUSH                                                     957
         AG_MODULE=MODULE                                                 957
C    *                                                                    957
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       958
         CALL AGSRESET                                                    958
         AG_SEEN = 0                                                      958
         AG_COLO = 1                                                      958
         AG_TITLE = 'SOSM'                                                958
         AG_EXNAME = 'ATTRIBUTE'                                          958
         AG_PARLIST = 'SEEN_COLO'                                         958
         CALL AXATTRIBUTE                                                 958
C    *                                                                    958
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                958
         CALL AGSRESET                                                    959
         AG_NDIV = 4                                                      959
         AG_IAXIS = 2                                                     959
         AG_C0 = 45                                                       959
         AG_TITLE = 'DIVISION'                                            959
         AG_EXNAME = 'SHAPE'                                              959
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     959
         CALL AXSHAPE                                                     959
C    *                                                                    959
C    Create SCRW :                                                        960
C    Call To Local Procedure SCRW                                         960
         ASSIGN 6140 TO BLOCK_SCRW                                        960
         GO TO  6100                                                      960
6140     CONTINUE                                                         960
C    *                                                                    960
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  961
C    ===                                                                  961
         CALL AGSRESET                                                    961
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      961
         AG_TITLE = 'SCRW'                                                961
         AG_EXNAME = 'POSITION'                                           961
         AG_PARLIST = 'X'                                                 961
         CALL AXPOSITION                                                  961
C    *                                                                    961
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  961
C    ===                                                                  961
         CALL AGSRESET                                                    962
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      962
         AG_TITLE = 'SCRW'                                                962
         AG_EXNAME = 'POSITION'                                           962
         AG_PARLIST = 'X'                                                 962
         CALL AXPOSITION                                                  962
C    *                                                                    962
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  962
C    =SSUP_ERJ2Y <===                                                     962
         CALL AGSRESET                                                    963
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      963
         AG_Y = SSUP_ERJ2Y                                                963
         AG_TITLE = 'SCRW'                                                963
         AG_EXNAME = 'POSITION'                                           963
         AG_PARLIST = 'X_Y'                                               963
         CALL AXPOSITION                                                  963
C    *                                                                    963
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  963
C    =-SSUP_ERJ2Y <===                                                    963
         CALL AGSRESET                                                    964
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      964
         AG_Y = -SSUP_ERJ2Y                                               964
         AG_TITLE = 'SCRW'                                                964
         AG_EXNAME = 'POSITION'                                           964
         AG_PARLIST = 'X_Y'                                               964
         CALL AXPOSITION                                                  964
C    *                                                                    964
6121  CALL AGSPOPB                                                        965
      GO TO BLOCK_SOSM, (6120 ,6130)                                      965
C End of Local Procedure                                                  965
C ----------------------------------------------------------------------  966
C ------------ SBRG                                                       966
C Begin Local Procedure SBRG                                              967
C SBRG IS THE BRACKET JOINING THE END RINGS                               967
5110  CONTINUE                                                            967
         AG_TITLE='SBRG'                                                  967
         AG_MODULE=MODULE                                                 967
         AG_MARK='V00'                                                    967
         CALL AGSPUSH                                                     967
         AG_MODULE=MODULE                                                 967
C    *                                                                    967
         RIN=SERG_IRNGPRMN                                                968
         ROU=SERG_ORNGRMAX                                                969
C    Geant MATERIAL : AIR ===> inherited arguments used <===              970
         CALL AGSRESET                                                    970
         AG_TITLE = 'AIR'                                                 970
         AG_EXNAME = 'MATERIAL'                                           970
         AG_PARLIST = 'NONE'                                              970
         CALL AXMATERIAL                                                  970
C    *                                                                    970
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       970
         CALL AGSRESET                                                    971
         AG_SEEN = 0                                                      971
         AG_COLO = 1                                                      971
         AG_TITLE = 'SBRG'                                                971
         AG_EXNAME = 'ATTRIBUTE'                                          971
         AG_PARLIST = 'SEEN_COLO'                                         971
         CALL AXATTRIBUTE                                                 971
C    *                                                                    971
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      971
         CALL AGSRESET                                                    972
         AG_RMIN = RIN                                                    972
         AG_RMAX = ROU                                                    972
         AG_DZ = SSUP_ERJTHK/2                                            972
         AG_TITLE = 'TUBE'                                                972
         AG_EXNAME = 'SHAPE'                                              972
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      972
         CALL AXSHAPE                                                     972
C    *                                                                    972
C    Create SBRM :                                                        973
C    Call To Local Procedure SBRM                                         973
         ASSIGN 6160 TO BLOCK_SBRM                                        973
         GO TO  6150                                                      973
6160     CONTINUE                                                         973
C    *                                                                    973
5111  CALL AGSPOPB                                                        974
      GO TO BLOCK_SBRG, (5110 ,5120)                                      974
C End of Local Procedure                                                  974
C ----------------------------------------------------------------------  975
C ------------ SBRM                                                       975
C Begin Local Procedure SBRM                                              976
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          976
6150  CONTINUE                                                            976
         AG_TITLE='SBRM'                                                  976
         AG_MODULE=MODULE                                                 976
         AG_MARK='V00'                                                    976
         CALL AGSPUSH                                                     976
         AG_MODULE=MODULE                                                 976
C    *                                                                    976
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       977
         CALL AGSRESET                                                    977
         AG_SEEN = 0                                                      977
         AG_COLO = 1                                                      977
         AG_TITLE = 'SBRM'                                                977
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
C    Create SBRI :                                                        979
C    Call To Local Procedure SBRI                                         979
         ASSIGN 6180 TO BLOCK_SBRI                                        979
         GO TO  6170                                                      979
6180     CONTINUE                                                         979
C    *                                                                    979
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        980
         CALL AGSRESET                                                    980
         AG_X = SSUP_ERJRAD                                               980
         AG_TITLE = 'SBRI'                                                980
         AG_EXNAME = 'POSITION'                                           980
         AG_PARLIST = 'X'                                                 980
         CALL AXPOSITION                                                  980
C    *                                                                    980
6151  CALL AGSPOPB                                                        981
      GO TO BLOCK_SBRM, (6150 ,6160)                                      981
C End of Local Procedure                                                  981
C ----------------------------------------------------------------------  982
C ------------ SBRI                                                       982
C Begin Local Procedure SBRI                                              983
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               983
6170  CONTINUE                                                            983
         AG_TITLE='SBRI'                                                  983
         AG_MODULE=MODULE                                                 983
         AG_MARK='V00'                                                    983
         CALL AGSPUSH                                                     983
         AG_MODULE=MODULE                                                 983
C    *                                                                    983
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       984
         CALL AGSRESET                                                    984
         AG_SEEN = 1                                                      984
         AG_COLO = 2                                                      984
         AG_TITLE = 'SBRI'                                                984
         AG_EXNAME = 'ATTRIBUTE'                                          984
         AG_PARLIST = 'SEEN_COLO'                                         984
         CALL AXATTRIBUTE                                                 984
C    *                                                                    984
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        984
         CALL AGSRESET                                                    985
         AG_TITLE = 'BERILLIUM'                                           985
         AG_EXNAME = 'MATERIAL'                                           985
         AG_PARLIST = 'NONE'                                              985
         CALL AXMATERIAL                                                  985
C    *                                                                    985
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  985
C    RJTHK/2 <===                                                         985
         CALL AGSRESET                                                    987
         AG_DX = SSUP_ERJLEN/2                                            987
         AG_DY = SSUP_ERJWID/2                                            987
         AG_DZ = SSUP_ERJTHK/2                                            987
         AG_TITLE = 'BOX'                                                 987
         AG_EXNAME = 'SHAPE'                                              987
         AG_PARLIST = 'DX_DY_DZ'                                          987
         CALL AXSHAPE                                                     987
C    *                                                                    987
6171  CALL AGSPOPB                                                        988
      GO TO BLOCK_SBRI, (6170 ,6180)                                      988
C End of Local Procedure                                                  988
C ----------------------------------------------------------------------  989
C ------------ SROD                                                       989
C Begin Local Procedure SROD                                              990
C SROD IS THE SVT CARBON COMPOSITE SUPPORT ROD                            990
5250  CONTINUE                                                            990
         AG_TITLE='SROD'                                                  990
         AG_MODULE=MODULE                                                 990
         AG_MARK='V00'                                                    990
         CALL AGSPUSH                                                     990
         AG_MODULE=MODULE                                                 990
C    *                                                                    990
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           991
         CALL AGSRESET                                                    991
         AG_TITLE = 'CARBON'                                              991
         AG_EXNAME = 'MATERIAL'                                           991
         AG_PARLIST = 'NONE'                                              991
         CALL AXMATERIAL                                                  991
C    *                                                                    991
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       991
         CALL AGSRESET                                                    992
         AG_SEEN = 1                                                      992
         AG_COLO = 2                                                      992
         AG_TITLE = 'SROD'                                                992
         AG_EXNAME = 'ATTRIBUTE'                                          992
         AG_PARLIST = 'SEEN_COLO'                                         992
         CALL AXATTRIBUTE                                                 992
C    *                                                                    992
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  992
C    P_RODLEN/2 <===                                                      992
         CALL AGSRESET                                                    994
         AG_RMIN = SSUP_RODID/2                                           994
         AG_RMAX = SSUP_RODOD/2                                           994
         AG_DZ = SSUP_RODLEN/2                                            994
         AG_TITLE = 'TUBE'                                                994
         AG_EXNAME = 'SHAPE'                                              994
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      994
         CALL AXSHAPE                                                     994
C    *                                                                    994
5251  CALL AGSPOPB                                                        995
      GO TO BLOCK_SROD, (5250 ,5260)                                      995
C End of Local Procedure                                                  995
C ----------------------------------------------------------------------  996
C ------------ SCON                                                       996
C Begin Local Procedure SCON                                              997
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               997
5210  CONTINUE                                                            997
         AG_TITLE='SCON'                                                  997
         AG_MODULE=MODULE                                                 997
         AG_MARK='V00'                                                    997
         CALL AGSPUSH                                                     997
         AG_MODULE=MODULE                                                 997
C    *                                                                    997
         CONE_THK1=SSUP_GRPHTHK                                           998
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                999
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              1000
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1001
         CALL AGSRESET                                                    1001
         AG_TITLE = 'AIR'                                                 1001
         AG_EXNAME = 'MATERIAL'                                           1001
         AG_PARLIST = 'NONE'                                              1001
         CALL AXMATERIAL                                                  1001
C    *                                                                    1001
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       1001
         CALL AGSRESET                                                    1002
         AG_SEEN = 0                                                      1002
         AG_COLO = 1                                                      1002
         AG_TITLE = 'SCON'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  1002
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  1002
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  1002
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  1002
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  1002
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  1002
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      1002
         CALL AGSRESET                                                    1012
         AG_PHI1 = 0                                                      1012
         AG_DPHI = 360                                                    1012
         AG_NZ = 7                                                        1012
         AG_ZI(1)=SSUP_CONE1ZMN                                           1012
         AG_ZI(2)=SSUP_RODLEN/2                                           1012
         AG_ZI(3)=SSUP_RODLEN/2                                           1012
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1012
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1012
         AG_ZI(6)=SSUP_CONE3ZMX                                           1012
         AG_ZI(7)=SSUP_CONE4ZMX                                           1012
         AG_RMN(1)=SSUP_CON1IDMN                                          1012
         AG_RMN(2)=SSUP_CON1IDMN                                          1012
         AG_RMN(3)=SSUP_CON1IDMN                                          1012
         AG_RMN(4)=SSUP_CON1IDMN                                          1012
         AG_RMN(5)=SSUP_CON3IDMN                                          1012
         AG_RMN(6)=SSUP_CON4IDMN                                          1012
         AG_RMN(7)=SSUP_CON4IDMX                                          1012
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                1012
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                1012
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                1012
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                1012
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                1012
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                1012
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                1012
         AG_TITLE = 'PCON'                                                1012
         AG_EXNAME = 'SHAPE'                                              1012
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1012
         CALL AXSHAPE                                                     1012
C    *                                                                    1012
C    Create SGRA :                                                        1013
C    Call To Local Procedure SGRA                                         1013
         ASSIGN 6200 TO BLOCK_SGRA                                        1013
         GO TO  6190                                                      1013
6200     CONTINUE                                                         1013
C    *                                                                    1013
C    Geant POSITION : SGRA ===> inherited arguments used <===             1014
         CALL AGSRESET                                                    1014
         AG_TITLE = 'SGRA'                                                1014
         AG_EXNAME = 'POSITION'                                           1014
         AG_PARLIST = 'NONE'                                              1014
         CALL AXPOSITION                                                  1014
C    *                                                                    1014
C    Create STAP :                                                        1015
C    Call To Local Procedure STAP                                         1015
         ASSIGN 6220 TO BLOCK_STAP                                        1015
         GO TO  6210                                                      1015
6220     CONTINUE                                                         1015
C    *                                                                    1015
C    Geant POSITION : STAP ===> inherited arguments used <===             1016
         CALL AGSRESET                                                    1016
         AG_TITLE = 'STAP'                                                1016
         AG_EXNAME = 'POSITION'                                           1016
         AG_PARLIST = 'NONE'                                              1016
         CALL AXPOSITION                                                  1016
C    *                                                                    1016
C    Create STAC :                                                        1017
C    Call To Local Procedure STAC                                         1017
         ASSIGN 6240 TO BLOCK_STAC                                        1017
         GO TO  6230                                                      1017
6240     CONTINUE                                                         1017
C    *                                                                    1017
C    Geant POSITION : STAC ===> inherited arguments used <===             1018
         CALL AGSRESET                                                    1018
         AG_TITLE = 'STAC'                                                1018
         AG_EXNAME = 'POSITION'                                           1018
         AG_PARLIST = 'NONE'                                              1018
         CALL AXPOSITION                                                  1018
C    *                                                                    1018
C    Create SHLA :                                                        1019
C    Call To Local Procedure SHLA                                         1019
         ASSIGN 6260 TO BLOCK_SHLA                                        1019
         GO TO  6250                                                      1019
6260     CONTINUE                                                         1019
C    *                                                                    1019
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  1021
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             1021
         CALL AGSRESET                                                    1021
         AG_Z = SSUP_RODLEN/                                              1021
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    1021
         AG_TITLE = 'SHLA'                                                1021
         AG_EXNAME = 'POSITION'                                           1021
         AG_PARLIST = 'Z'                                                 1021
         CALL AXPOSITION                                                  1021
C    *                                                                    1021
C    Create SHLB :                                                        1022
C    Call To Local Procedure SHLB                                         1022
         ASSIGN 6280 TO BLOCK_SHLB                                        1022
         GO TO  6270                                                      1022
6280     CONTINUE                                                         1022
C    *                                                                    1022
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  1023
C    CONE3ZMX) <===                                                       1023
         CALL AGSRESET                                                    1023
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           1023
         AG_TITLE = 'SHLB'                                                1023
         AG_EXNAME = 'POSITION'                                           1023
         AG_PARLIST = 'Z'                                                 1023
         CALL AXPOSITION                                                  1023
C    *                                                                    1023
C    Create SCMY :                                                        1024
C    Call To Local Procedure SCMY                                         1024
         ASSIGN 6300 TO BLOCK_SCMY                                        1024
         GO TO  6290                                                      1024
6300     CONTINUE                                                         1024
C    *                                                                    1024
C    Geant POSITION : SCMY ===> inherited arguments used <===             1025
         CALL AGSRESET                                                    1025
         AG_TITLE = 'SCMY'                                                1025
         AG_EXNAME = 'POSITION'                                           1025
         AG_PARLIST = 'NONE'                                              1025
         CALL AXPOSITION                                                  1025
C    *                                                                    1025
C    Create SCAL :                                                        1026
C    Call To Local Procedure SCAL                                         1026
         ASSIGN 6320 TO BLOCK_SCAL                                        1026
         GO TO  6310                                                      1026
6320     CONTINUE                                                         1026
C    *                                                                    1026
C    Geant POSITION : SCAL ===> inherited arguments used <===             1027
         CALL AGSRESET                                                    1027
         AG_TITLE = 'SCAL'                                                1027
         AG_EXNAME = 'POSITION'                                           1027
         AG_PARLIST = 'NONE'                                              1027
         CALL AXPOSITION                                                  1027
C    *                                                                    1027
5211  CALL AGSPOPB                                                        1028
      GO TO BLOCK_SCON, (5210 ,5220)                                      1028
C End of Local Procedure                                                  1028
C ----------------------------------------------------------------------  1029
C ------------ SGRA                                                       1029
C Begin Local Procedure SGRA                                              1030
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 1030
6190  CONTINUE                                                            1030
         AG_TITLE='SGRA'                                                  1030
         AG_MODULE=MODULE                                                 1030
         AG_MARK='V00'                                                    1030
         CALL AGSPUSH                                                     1030
         AG_MODULE=MODULE                                                 1030
C    *                                                                    1030
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1031
         CALL AGSRESET                                                    1031
         AG_TITLE = 'CARBON'                                              1031
         AG_EXNAME = 'MATERIAL'                                           1031
         AG_PARLIST = 'NONE'                                              1031
         CALL AXMATERIAL                                                  1031
C    *                                                                    1031
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       1031
         CALL AGSRESET                                                    1032
         AG_SEEN = 1                                                      1032
         AG_COLO = 6                                                      1032
         AG_TITLE = 'SGRA'                                                1032
         AG_EXNAME = 'ATTRIBUTE'                                          1032
         AG_PARLIST = 'SEEN_COLO'                                         1032
         CALL AXATTRIBUTE                                                 1032
C    *                                                                    1032
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  1032
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  1032
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  1032
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  1032
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  1032
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              1032
         CALL AGSRESET                                                    1040
         AG_PHI1 = 0                                                      1040
         AG_DPHI = 360                                                    1040
         AG_NZ = 5                                                        1040
         AG_ZI(1)=SSUP_RODLEN/2                                           1040
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1040
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1040
         AG_ZI(4)=SSUP_CONE3ZMX                                           1040
         AG_ZI(5)=SSUP_CONE4ZMX                                           1040
         AG_RMN(1)=SSUP_CON1IDMN                                          1040
         AG_RMN(2)=SSUP_CON1IDMN                                          1040
         AG_RMN(3)=SSUP_CON3IDMN                                          1040
         AG_RMN(4)=SSUP_CON4IDMN                                          1040
         AG_RMN(5)=SSUP_CON4IDMX                                          1040
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1040
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1040
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             1040
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             1040
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             1040
         AG_TITLE = 'PCON'                                                1040
         AG_EXNAME = 'SHAPE'                                              1040
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1040
         CALL AXSHAPE                                                     1040
C    *                                                                    1040
6191  CALL AGSPOPB                                                        1041
      GO TO BLOCK_SGRA, (6190 ,6200)                                      1041
C End of Local Procedure                                                  1041
C ----------------------------------------------------------------------  1042
C ------------ STAP                                                       1042
C Begin Local Procedure STAP                                              1043
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  1043
C E)                                                                      1043
6210  CONTINUE                                                            1043
         AG_TITLE='STAP'                                                  1043
         AG_MODULE=MODULE                                                 1043
         AG_MARK='V00'                                                    1043
         CALL AGSPUSH                                                     1043
         AG_MODULE=MODULE                                                 1043
C    *                                                                    1043
         ROFFSET=SSUP_GRPHTHK                                             1044
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1045
         CALL AGSRESET                                                    1045
         AG_A = 12                                                        1045
         AG_Z = 6                                                         1045
         AG_W = 1                                                         1045
         AG_TITLE = 'C'                                                   1045
         AG_EXNAME = 'COMPONENT'                                          1045
         AG_PARLIST = 'A_Z_W'                                             1045
         CALL AXCOMPONENT                                                 1045
C    *                                                                    1045
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1045
         CALL AGSRESET                                                    1046
         AG_A = 1                                                         1046
         AG_Z = 1                                                         1046
         AG_W = 2                                                         1046
         AG_TITLE = 'H2'                                                  1046
         AG_EXNAME = 'COMPONENT'                                          1046
         AG_PARLIST = 'A_Z_W'                                             1046
         CALL AXCOMPONENT                                                 1046
C    *                                                                    1046
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1046
         CALL AGSRESET                                                    1047
         AG_DENS = 0.935                                                  1047
         AG_TITLE = 'CH2'                                                 1047
         AG_EXNAME = 'MIXTURE'                                            1047
         AG_PARLIST = 'DENS'                                              1047
         CALL AXMIXTURE                                                   1047
C    *                                                                    1047
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       1047
         CALL AGSRESET                                                    1048
         AG_SEEN = 1                                                      1048
         AG_COLO = 3                                                      1048
         AG_TITLE = 'STAP'                                                1048
         AG_EXNAME = 'ATTRIBUTE'                                          1048
         AG_PARLIST = 'SEEN_COLO'                                         1048
         CALL AXATTRIBUTE                                                 1048
C    *                                                                    1048
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1048
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1048
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1048
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1048
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1048
         CALL AGSRESET                                                    1054
         AG_PHI1 = 0                                                      1054
         AG_DPHI = 360                                                    1054
         AG_NZ = 3                                                        1054
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1054
         AG_ZI(2)=SSUP_CONE3ZMX                                           1054
         AG_ZI(3)=SSUP_CONE4ZMX                                           1054
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1054
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1054
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1054
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1054
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1054
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1054
         AG_TITLE = 'PCON'                                                1054
         AG_EXNAME = 'SHAPE'                                              1054
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1054
         CALL AXSHAPE                                                     1054
C    *                                                                    1054
6211  CALL AGSPOPB                                                        1055
      GO TO BLOCK_STAP, (6210 ,6220)                                      1055
C End of Local Procedure                                                  1055
C ----------------------------------------------------------------------  1056
C ------------ STAC                                                       1056
C Begin Local Procedure STAC                                              1057
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      1057
6230  CONTINUE                                                            1057
         AG_TITLE='STAC'                                                  1057
         AG_MODULE=MODULE                                                 1057
         AG_MARK='V00'                                                    1057
         CALL AGSPUSH                                                     1057
         AG_MODULE=MODULE                                                 1057
C    *                                                                    1057
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             1058
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           1059
         CALL AGSRESET                                                    1059
         AG_TITLE = 'COPPER'                                              1059
         AG_EXNAME = 'MATERIAL'                                           1059
         AG_PARLIST = 'NONE'                                              1059
         CALL AXMATERIAL                                                  1059
C    *                                                                    1059
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       1059
         CALL AGSRESET                                                    1060
         AG_SEEN = 1                                                      1060
         AG_COLO = 2                                                      1060
         AG_TITLE = 'STAC'                                                1060
         AG_EXNAME = 'ATTRIBUTE'                                          1060
         AG_PARLIST = 'SEEN_COLO'                                         1060
         CALL AXATTRIBUTE                                                 1060
C    *                                                                    1060
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1060
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1060
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1060
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  1060
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     1060
         CALL AGSRESET                                                    1066
         AG_PHI1 = 0                                                      1066
         AG_DPHI = 360                                                    1066
         AG_NZ = 3                                                        1066
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1066
         AG_ZI(2)=SSUP_CONE3ZMX                                           1066
         AG_ZI(3)=SSUP_CONE4ZMX                                           1066
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1066
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1066
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1066
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1066
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1066
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1066
         AG_TITLE = 'PCON'                                                1066
         AG_EXNAME = 'SHAPE'                                              1066
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1066
         CALL AXSHAPE                                                     1066
C    *                                                                    1066
6231  CALL AGSPOPB                                                        1067
      GO TO BLOCK_STAC, (6230 ,6240)                                      1067
C End of Local Procedure                                                  1067
C ----------------------------------------------------------------------  1068
C ------------ SHLA                                                       1068
C Begin Local Procedure SHLA                                              1069
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              1069
6250  CONTINUE                                                            1069
         AG_TITLE='SHLA'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1070
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1071
         CALL AGSRESET                                                    1071
         AG_TITLE = 'AIR'                                                 1071
         AG_EXNAME = 'MATERIAL'                                           1071
         AG_PARLIST = 'NONE'                                              1071
         CALL AXMATERIAL                                                  1071
C    *                                                                    1071
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       1071
         CALL AGSRESET                                                    1072
         AG_SEEN = 0                                                      1072
         AG_COLO = 1                                                      1072
         AG_TITLE = 'SHLA'                                                1072
         AG_EXNAME = 'ATTRIBUTE'                                          1072
         AG_PARLIST = 'SEEN_COLO'                                         1072
         CALL AXATTRIBUTE                                                 1072
C    *                                                                    1072
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  1072
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  1072
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  1072
C    .0*SSUP_HOSRMX <===                                                  1072
         CALL AGSRESET                                                    1076
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            1076
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  1076
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1076
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  1076
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1076
         AG_TITLE = 'CONE'                                                1076
         AG_EXNAME = 'SHAPE'                                              1076
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1076
         CALL AXSHAPE                                                     1076
C    *                                                                    1076
C    Create SHMA :                                                        1077
C    Call To Local Procedure SHMA                                         1077
         ASSIGN 6340 TO BLOCK_SHMA                                        1077
         GO TO  6330                                                      1077
6340     CONTINUE                                                         1077
C    *                                                                    1077
6251  CALL AGSPOPB                                                        1078
      GO TO BLOCK_SHLA, (6250 ,6260)                                      1078
C End of Local Procedure                                                  1078
C ----------------------------------------------------------------------  1079
C ------------ SHMA                                                       1079
C Begin Local Procedure SHMA                                              1080
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           1080
6330  CONTINUE                                                            1080
         AG_TITLE='SHMA'                                                  1080
         AG_MODULE=MODULE                                                 1080
         AG_MARK='V00'                                                    1080
         CALL AGSPUSH                                                     1080
         AG_MODULE=MODULE                                                 1080
C    *                                                                    1080
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1081
         RMIN=SSUP_CON3IDMN + ROFFSET                                     1082
         RMAX=SSUP_CON4IDMN + ROFFSET                                     1083
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                1084
         ZMAX=SSUP_CONE3ZMX                                               1085
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1086
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1087
         CONE_LEN = SQRT(CONE_LEN)                                        1088
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1089
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1090
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1091
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1092
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1093
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       1094
         CALL AGSRESET                                                    1094
         AG_SEEN = 0                                                      1094
         AG_COLO = 1                                                      1094
         AG_TITLE = 'SHMA'                                                1094
         AG_EXNAME = 'ATTRIBUTE'                                          1094
         AG_PARLIST = 'SEEN_COLO'                                         1094
         CALL AXATTRIBUTE                                                 1094
C    *                                                                    1094
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1094
         CALL AGSRESET                                                    1096
         AG_NDIV = SSUP_NHOSES                                            1096
         AG_IAXIS = 2                                                     1096
         AG_C0 = 0.0                                                      1096
         AG_TITLE = 'DIVISION'                                            1096
         AG_EXNAME = 'SHAPE'                                              1096
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1096
         CALL AXSHAPE                                                     1096
C    *                                                                    1096
C    Create SWHO :                                                        1097
C    Call To Local Procedure SWHO                                         1097
         ASSIGN 6360 TO BLOCK_SWHO                                        1097
         GO TO  6350                                                      1097
6360     CONTINUE                                                         1097
C    *                                                                    1097
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1098
         CALL AGSRESET                                                    1098
         AG_X = XPOS                                                      1098
         AG_Y = 0                                                         1098
         AG_Z = ZPOS                                                      1098
         AG_ALPHAN=AG_ALPHAN+1                                            1098
         AG_ALPHAK(AG_ALPHAN)=2                                           1098
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1098
         AG_TITLE = 'SWHO'                                                1098
         AG_EXNAME = 'POSITION'                                           1098
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1098
         CALL AXPOSITION                                                  1098
C    *                                                                    1098
6331  CALL AGSPOPB                                                        1099
      GO TO BLOCK_SHMA, (6330 ,6340)                                      1099
C End of Local Procedure                                                  1099
C ----------------------------------------------------------------------  1100
C ------------ SHLB                                                       1100
C Begin Local Procedure SHLB                                              1101
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               1101
6270  CONTINUE                                                            1101
         AG_TITLE='SHLB'                                                  1101
         AG_MODULE=MODULE                                                 1101
         AG_MARK='V00'                                                    1101
         CALL AGSPUSH                                                     1101
         AG_MODULE=MODULE                                                 1101
C    *                                                                    1101
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1102
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1103
         CALL AGSRESET                                                    1103
         AG_TITLE = 'AIR'                                                 1103
         AG_EXNAME = 'MATERIAL'                                           1103
         AG_PARLIST = 'NONE'                                              1103
         CALL AXMATERIAL                                                  1103
C    *                                                                    1103
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       1103
         CALL AGSRESET                                                    1104
         AG_SEEN = 0                                                      1104
         AG_COLO = 1                                                      1104
         AG_TITLE = 'SHLB'                                                1104
         AG_EXNAME = 'ATTRIBUTE'                                          1104
         AG_PARLIST = 'SEEN_COLO'                                         1104
         CALL AXATTRIBUTE                                                 1104
C    *                                                                    1104
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  1104
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  1104
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  1104
C    X <===                                                               1104
         CALL AGSRESET                                                    1108
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         1108
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  1108
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1108
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  1108
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  1108
         AG_TITLE = 'CONE'                                                1108
         AG_EXNAME = 'SHAPE'                                              1108
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1108
         CALL AXSHAPE                                                     1108
C    *                                                                    1108
C    Create SHMB :                                                        1109
C    Call To Local Procedure SHMB                                         1109
         ASSIGN 6380 TO BLOCK_SHMB                                        1109
         GO TO  6370                                                      1109
6380     CONTINUE                                                         1109
C    *                                                                    1109
6271  CALL AGSPOPB                                                        1110
      GO TO BLOCK_SHLB, (6270 ,6280)                                      1110
C End of Local Procedure                                                  1110
C ----------------------------------------------------------------------  1111
C ------------ SHMB                                                       1111
C Begin Local Procedure SHMB                                              1112
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           1112
6370  CONTINUE                                                            1112
         AG_TITLE='SHMB'                                                  1112
         AG_MODULE=MODULE                                                 1112
         AG_MARK='V00'                                                    1112
         CALL AGSPUSH                                                     1112
         AG_MODULE=MODULE                                                 1112
C    *                                                                    1112
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1113
         RMIN=SSUP_CON4IDMN + ROFFSET                                     1114
         RMAX=SSUP_CON4IDMX + ROFFSET                                     1115
         ZMIN=SSUP_CONE3ZMX                                               1116
         ZMAX=SSUP_CONE4ZMX                                               1117
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1118
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1119
         CONE_LEN = SQRT(CONE_LEN)                                        1120
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1121
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1122
         ANGLE=ASIN(CONE_SIN)*180/PI                                      1123
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1124
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1125
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       1126
         CALL AGSRESET                                                    1126
         AG_SEEN = 0                                                      1126
         AG_COLO = 1                                                      1126
         AG_TITLE = 'SHMB'                                                1126
         AG_EXNAME = 'ATTRIBUTE'                                          1126
         AG_PARLIST = 'SEEN_COLO'                                         1126
         CALL AXATTRIBUTE                                                 1126
C    *                                                                    1126
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1126
         CALL AGSRESET                                                    1128
         AG_NDIV = SSUP_NHOSES                                            1128
         AG_IAXIS = 2                                                     1128
         AG_C0 = 0.0                                                      1128
         AG_TITLE = 'DIVISION'                                            1128
         AG_EXNAME = 'SHAPE'                                              1128
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1128
         CALL AXSHAPE                                                     1128
C    *                                                                    1128
C    Create SWHO :                                                        1129
C    Call To Local Procedure SWHO                                         1129
         ASSIGN 6390 TO BLOCK_SWHO                                        1129
         GO TO  6350                                                      1129
6390     CONTINUE                                                         1129
C    *                                                                    1129
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1130
         CALL AGSRESET                                                    1130
         AG_X = XPOS                                                      1130
         AG_Y = 0                                                         1130
         AG_Z = ZPOS                                                      1130
         AG_ALPHAN=AG_ALPHAN+1                                            1130
         AG_ALPHAK(AG_ALPHAN)=2                                           1130
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1130
         AG_TITLE = 'SWHO'                                                1130
         AG_EXNAME = 'POSITION'                                           1130
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1130
         CALL AXPOSITION                                                  1130
C    *                                                                    1130
6371  CALL AGSPOPB                                                        1131
      GO TO BLOCK_SHMB, (6370 ,6380)                                      1131
C End of Local Procedure                                                  1131
C ----------------------------------------------------------------------  1132
C ------------ SWHO                                                       1132
C Begin Local Procedure SWHO                                              1133
C SWHO IS A WATER HOSE                                                    1133
6350  CONTINUE                                                            1133
         AG_TITLE='SWHO'                                                  1133
         AG_MODULE=MODULE                                                 1133
         AG_MARK='V00'                                                    1133
         CALL AGSPUSH                                                     1133
         AG_MODULE=MODULE                                                 1133
C    *                                                                    1133
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1134
         CALL AGSRESET                                                    1134
         AG_SEEN = 1                                                      1134
         AG_COLO = 3                                                      1134
         AG_TITLE = 'SWHO'                                                1134
         AG_EXNAME = 'ATTRIBUTE'                                          1134
         AG_PARLIST = 'SEEN_COLO'                                         1134
         CALL AXATTRIBUTE                                                 1134
C    *                                                                    1134
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1134
         CALL AGSRESET                                                    1135
         AG_A = 12                                                        1135
         AG_Z = 6                                                         1135
         AG_W = 1                                                         1135
         AG_TITLE = 'C'                                                   1135
         AG_EXNAME = 'COMPONENT'                                          1135
         AG_PARLIST = 'A_Z_W'                                             1135
         CALL AXCOMPONENT                                                 1135
C    *                                                                    1135
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1135
         CALL AGSRESET                                                    1136
         AG_A = 1                                                         1136
         AG_Z = 1                                                         1136
         AG_W = 2                                                         1136
         AG_TITLE = 'H2'                                                  1136
         AG_EXNAME = 'COMPONENT'                                          1136
         AG_PARLIST = 'A_Z_W'                                             1136
         CALL AXCOMPONENT                                                 1136
C    *                                                                    1136
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1136
         CALL AGSRESET                                                    1137
         AG_DENS = 0.935                                                  1137
         AG_TITLE = 'CH2'                                                 1137
         AG_EXNAME = 'MIXTURE'                                            1137
         AG_PARLIST = 'DENS'                                              1137
         CALL AXMIXTURE                                                   1137
C    *                                                                    1137
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1137
         CALL AGSRESET                                                    1138
         AG_SEEN = 1                                                      1138
         AG_COLO = 3                                                      1138
         AG_TITLE = 'SWHO'                                                1138
         AG_EXNAME = 'ATTRIBUTE'                                          1138
         AG_PARLIST = 'SEEN_COLO'                                         1138
         CALL AXATTRIBUTE                                                 1138
C    *                                                                    1138
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  1138
C    =                                                                    1138
         CALL AGSRESET                                                    1139
         AG_RMIN = 0                                                      1139
         AG_RMAX = SSUP_HOSRMX                                            1139
         AG_DZ = 0.5*CONE_LEN                                             1139
         AG_TITLE = 'TUBE'                                                1139
         AG_EXNAME = 'SHAPE'                                              1139
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1139
         CALL AXSHAPE                                                     1139
C    *                                                                    1139
C    Create SHWA :                                                        1140
C    Call To Local Procedure SHWA                                         1140
         ASSIGN 6410 TO BLOCK_SHWA                                        1140
         GO TO  6400                                                      1140
6410     CONTINUE                                                         1140
C    *                                                                    1140
C    Geant POSITION : SHWA ===> inherited arguments used <===             1141
         CALL AGSRESET                                                    1141
         AG_TITLE = 'SHWA'                                                1141
         AG_EXNAME = 'POSITION'                                           1141
         AG_PARLIST = 'NONE'                                              1141
         CALL AXPOSITION                                                  1141
C    *                                                                    1141
6351  CALL AGSPOPB                                                        1142
      GO TO BLOCK_SWHO, (6350 ,6360,6390)                                 1142
C End of Local Procedure                                                  1142
C ----------------------------------------------------------------------  1143
C ------------ SHWA                                                       1143
C Begin Local Procedure SHWA                                              1144
C SHWA IS THE WATER IN THE HOSE                                           1144
6400  CONTINUE                                                            1144
         AG_TITLE='SHWA'                                                  1144
         AG_MODULE=MODULE                                                 1144
         AG_MARK='V00'                                                    1144
         CALL AGSPUSH                                                     1144
         AG_MODULE=MODULE                                                 1144
C    *                                                                    1144
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       1145
         CALL AGSRESET                                                    1145
         AG_SEEN = 1                                                      1145
         AG_COLO = 6                                                      1145
         AG_TITLE = 'SHWA'                                                1145
         AG_EXNAME = 'ATTRIBUTE'                                          1145
         AG_PARLIST = 'SEEN_COLO'                                         1145
         CALL AXATTRIBUTE                                                 1145
C    *                                                                    1145
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1145
         CALL AGSRESET                                                    1146
         AG_TITLE = 'WATER'                                               1146
         AG_EXNAME = 'MATERIAL'                                           1146
         AG_PARLIST = 'NONE'                                              1146
         CALL AXMATERIAL                                                  1146
C    *                                                                    1146
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        1146
         CALL AGSRESET                                                    1147
         AG_RMAX = SSUP_HOSRMN                                            1147
         AG_TITLE = 'TUBE'                                                1147
         AG_EXNAME = 'SHAPE'                                              1147
         AG_PARLIST = 'RMAX'                                              1147
         CALL AXSHAPE                                                     1147
C    *                                                                    1147
6401  CALL AGSPOPB                                                        1148
      GO TO BLOCK_SHWA, (6400 ,6410)                                      1148
C End of Local Procedure                                                  1148
C ----------------------------------------------------------------------  1149
C ------------ SCMY                                                       1149
C Begin Local Procedure SCMY                                              1150
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            1150
6290  CONTINUE                                                            1150
         AG_TITLE='SCMY'                                                  1150
         AG_MODULE=MODULE                                                 1150
         AG_MARK='V00'                                                    1150
         CALL AGSPUSH                                                     1150
         AG_MODULE=MODULE                                                 1150
C    *                                                                    1150
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             1151
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          1152
         CALL AGSRESET                                                    1152
         AG_A = 12                                                        1152
         AG_Z = 6                                                         1152
         AG_W = 5                                                         1152
         AG_TITLE = 'C5'                                                  1152
         AG_EXNAME = 'COMPONENT'                                          1152
         AG_PARLIST = 'A_Z_W'                                             1152
         CALL AXCOMPONENT                                                 1152
C    *                                                                    1152
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           1152
         CALL AGSRESET                                                    1153
         AG_A = 1                                                         1153
         AG_Z = 1                                                         1153
         AG_W = 4                                                         1153
         AG_TITLE = 'H4'                                                  1153
         AG_EXNAME = 'COMPONENT'                                          1153
         AG_PARLIST = 'A_Z_W'                                             1153
         CALL AXCOMPONENT                                                 1153
C    *                                                                    1153
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          1153
         CALL AGSRESET                                                    1154
         AG_A = 16                                                        1154
         AG_Z = 8                                                         1154
         AG_W = 2                                                         1154
         AG_TITLE = 'O2'                                                  1154
         AG_EXNAME = 'COMPONENT'                                          1154
         AG_PARLIST = 'A_Z_W'                                             1154
         CALL AXCOMPONENT                                                 1154
C    *                                                                    1154
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            1154
         CALL AGSRESET                                                    1155
         AG_DENS = 1.39                                                   1155
         AG_TITLE = 'MYLAR'                                               1155
         AG_EXNAME = 'MIXTURE'                                            1155
         AG_PARLIST = 'DENS'                                              1155
         CALL AXMIXTURE                                                   1155
C    *                                                                    1155
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       1155
         CALL AGSRESET                                                    1156
         AG_SEEN = 1                                                      1156
         AG_COLO = 3                                                      1156
         AG_TITLE = 'SCMY'                                                1156
         AG_EXNAME = 'ATTRIBUTE'                                          1156
         AG_PARLIST = 'SEEN_COLO'                                         1156
         CALL AXATTRIBUTE                                                 1156
C    *                                                                    1156
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1156
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1156
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1156
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  1156
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     1156
         CALL AGSRESET                                                    1162
         AG_PHI1 = 0                                                      1162
         AG_DPHI = 360                                                    1162
         AG_NZ = 3                                                        1162
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1162
         AG_ZI(2)=SSUP_CONE3ZMX                                           1162
         AG_ZI(3)=SSUP_CONE4ZMX                                           1162
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1162
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1162
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1162
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    1162
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    1162
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    1162
         AG_TITLE = 'PCON'                                                1162
         AG_EXNAME = 'SHAPE'                                              1162
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1162
         CALL AXSHAPE                                                     1162
C    *                                                                    1162
6291  CALL AGSPOPB                                                        1163
      GO TO BLOCK_SCMY, (6290 ,6300)                                      1163
C End of Local Procedure                                                  1163
C ----------------------------------------------------------------------  1164
C ------------ SCAL                                                       1164
C Begin Local Procedure SCAL                                              1165
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     1165
6310  CONTINUE                                                            1165
         AG_TITLE='SCAL'                                                  1165
         AG_MODULE=MODULE                                                 1165
         AG_MARK='V00'                                                    1165
         CALL AGSPUSH                                                     1165
         AG_MODULE=MODULE                                                 1165
C    *                                                                    1165
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           1166
     *   SSUP_WRPMYTHK                                                    1166
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1167
         CALL AGSRESET                                                    1167
         AG_TITLE = 'ALUMINIUM'                                           1167
         AG_EXNAME = 'MATERIAL'                                           1167
         AG_PARLIST = 'NONE'                                              1167
         CALL AXMATERIAL                                                  1167
C    *                                                                    1167
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       1167
         CALL AGSRESET                                                    1168
         AG_SEEN = 1                                                      1168
         AG_COLO = 2                                                      1168
         AG_TITLE = 'SCAL'                                                1168
         AG_EXNAME = 'ATTRIBUTE'                                          1168
         AG_PARLIST = 'SEEN_COLO'                                         1168
         CALL AXATTRIBUTE                                                 1168
C    *                                                                    1168
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1168
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1168
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1168
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  1168
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     1168
         CALL AGSRESET                                                    1174
         AG_PHI1 = 0                                                      1174
         AG_DPHI = 360                                                    1174
         AG_NZ = 3                                                        1174
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1174
         AG_ZI(2)=SSUP_CONE3ZMX                                           1174
         AG_ZI(3)=SSUP_CONE4ZMX                                           1174
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1174
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1174
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1174
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    1174
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    1174
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    1174
         AG_TITLE = 'PCON'                                                1174
         AG_EXNAME = 'SHAPE'                                              1174
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1174
         CALL AXSHAPE                                                     1174
C    *                                                                    1174
6311  CALL AGSPOPB                                                        1175
      GO TO BLOCK_SCAL, (6310 ,6320)                                      1175
C End of Local Procedure                                                  1175
C ----------------------------------------------------------------------  1176
C ------------ SWMM                                                       1176
C Begin Local Procedure SWMM                                              1177
C SWMM IS THE WATER MANIFOLD MOTHER                                       1177
5170  CONTINUE                                                            1177
         AG_TITLE='SWMM'                                                  1177
         AG_MODULE=MODULE                                                 1177
         AG_MARK='V00'                                                    1177
         CALL AGSPUSH                                                     1177
         AG_MODULE=MODULE                                                 1177
C    *                                                                    1177
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     1178
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     1179
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1180
         CALL AGSRESET                                                    1180
         AG_TITLE = 'AIR'                                                 1180
         AG_EXNAME = 'MATERIAL'                                           1180
         AG_PARLIST = 'NONE'                                              1180
         CALL AXMATERIAL                                                  1180
C    *                                                                    1180
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       1180
         CALL AGSRESET                                                    1181
         AG_SEEN = 0                                                      1181
         AG_COLO = 1                                                      1181
         AG_TITLE = 'SWMM'                                                1181
         AG_EXNAME = 'ATTRIBUTE'                                          1181
         AG_PARLIST = 'SEEN_COLO'                                         1181
         CALL AXATTRIBUTE                                                 1181
C    *                                                                    1181
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1181
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   1181
         CALL AGSRESET                                                    1184
         AG_PHI1 = 0                                                      1184
         AG_DPHI = 360                                                    1184
         AG_NPDIV = 18                                                    1184
         AG_NZ = 2                                                        1184
         AG_ZI(1)=-SWAM_LEN/2                                             1184
         AG_ZI(2)=+SWAM_LEN/2                                             1184
         AG_RMN(1)=RIN                                                    1184
         AG_RMN(2)=RIN                                                    1184
         AG_RMX(1)=ROU                                                    1184
         AG_RMX(2)=ROU                                                    1184
         AG_TITLE = 'PGON'                                                1184
         AG_EXNAME = 'SHAPE'                                              1184
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1184
         CALL AXSHAPE                                                     1184
C    *                                                                    1184
C    Create SWMB :                                                        1185
C    Call To Local Procedure SWMB                                         1185
         ASSIGN 6430 TO BLOCK_SWMB                                        1185
         GO TO  6420                                                      1185
6430     CONTINUE                                                         1185
C    *                                                                    1185
C    Geant POSITION : SWMB ===> inherited arguments used <===             1186
         CALL AGSRESET                                                    1186
         AG_TITLE = 'SWMB'                                                1186
         AG_EXNAME = 'POSITION'                                           1186
         AG_PARLIST = 'NONE'                                              1186
         CALL AXPOSITION                                                  1186
C    *                                                                    1186
C    Create SWMT :                                                        1187
C    Call To Local Procedure SWMT                                         1187
         ASSIGN 6450 TO BLOCK_SWMT                                        1187
         GO TO  6440                                                      1187
6450     CONTINUE                                                         1187
C    *                                                                    1187
C    Geant POSITION : SWMT ===> inherited arguments used <===             1188
         CALL AGSRESET                                                    1188
         AG_TITLE = 'SWMT'                                                1188
         AG_EXNAME = 'POSITION'                                           1188
         AG_PARLIST = 'NONE'                                              1188
         CALL AXPOSITION                                                  1188
C    *                                                                    1188
C    Create SWMS :                                                        1189
C    Call To Local Procedure SWMS                                         1189
         ASSIGN 6470 TO BLOCK_SWMS                                        1189
         GO TO  6460                                                      1189
6470     CONTINUE                                                         1189
C    *                                                                    1189
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         1190
         CALL AGSRESET                                                    1190
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                1190
         AG_TITLE = 'SWMS'                                                1190
         AG_EXNAME = 'POSITION'                                           1190
         AG_PARLIST = 'Z'                                                 1190
         CALL AXPOSITION                                                  1190
C    *                                                                    1190
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          1190
         CALL AGSRESET                                                    1191
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 1191
         AG_TITLE = 'SWMS'                                                1191
         AG_EXNAME = 'POSITION'                                           1191
         AG_PARLIST = 'Z'                                                 1191
         CALL AXPOSITION                                                  1191
C    *                                                                    1191
C    Create SWMW :                                                        1192
C    Call To Local Procedure SWMW                                         1192
         ASSIGN 6490 TO BLOCK_SWMW                                        1192
         GO TO  6480                                                      1192
6490     CONTINUE                                                         1192
C    *                                                                    1192
C    Geant POSITION : SWMW ===> inherited arguments used <===             1193
         CALL AGSRESET                                                    1193
         AG_TITLE = 'SWMW'                                                1193
         AG_EXNAME = 'POSITION'                                           1193
         AG_PARLIST = 'NONE'                                              1193
         CALL AXPOSITION                                                  1193
C    *                                                                    1193
C    Create SOTB :                                                        1194
C    Call To Local Procedure SOTB                                         1194
         ASSIGN 6510 TO BLOCK_SOTB                                        1194
         GO TO  6500                                                      1194
6510     CONTINUE                                                         1194
C    *                                                                    1194
C    Geant POSITION : SOTB ===> inherited arguments used <===             1195
         CALL AGSRESET                                                    1195
         AG_TITLE = 'SOTB'                                                1195
         AG_EXNAME = 'POSITION'                                           1195
         AG_PARLIST = 'NONE'                                              1195
         CALL AXPOSITION                                                  1195
C    *                                                                    1195
C    Create SITB :                                                        1196
C    Call To Local Procedure SITB                                         1196
         ASSIGN 6530 TO BLOCK_SITB                                        1196
         GO TO  6520                                                      1196
6530     CONTINUE                                                         1196
C    *                                                                    1196
C    Geant POSITION : SITB ===> inherited arguments used <===             1197
         CALL AGSRESET                                                    1197
         AG_TITLE = 'SITB'                                                1197
         AG_EXNAME = 'POSITION'                                           1197
         AG_PARLIST = 'NONE'                                              1197
         CALL AXPOSITION                                                  1197
C    *                                                                    1197
5171  CALL AGSPOPB                                                        1198
      GO TO BLOCK_SWMM, (5170 ,5180)                                      1198
C End of Local Procedure                                                  1198
C ----------------------------------------------------------------------  1199
C ------------ SWMB                                                       1199
C Begin Local Procedure SWMB                                              1200
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       1200
6420  CONTINUE                                                            1200
         AG_TITLE='SWMB'                                                  1200
         AG_MODULE=MODULE                                                 1200
         AG_MARK='V00'                                                    1200
         CALL AGSPUSH                                                     1200
         AG_MODULE=MODULE                                                 1200
C    *                                                                    1200
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1201
         CALL AGSRESET                                                    1201
         AG_TITLE = 'ALUMINIUM'                                           1201
         AG_EXNAME = 'MATERIAL'                                           1201
         AG_PARLIST = 'NONE'                                              1201
         CALL AXMATERIAL                                                  1201
C    *                                                                    1201
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       1201
         CALL AGSRESET                                                    1202
         AG_SEEN = 1                                                      1202
         AG_COLO = 2                                                      1202
         AG_TITLE = 'SWMB'                                                1202
         AG_EXNAME = 'ATTRIBUTE'                                          1202
         AG_PARLIST = 'SEEN_COLO'                                         1202
         CALL AXATTRIBUTE                                                 1202
C    *                                                                    1202
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1202
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  1202
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          1202
         CALL AGSRESET                                                    1206
         AG_PHI1 = 0                                                      1206
         AG_DPHI = 360                                                    1206
         AG_NPDIV = 18                                                    1206
         AG_NZ = 2                                                        1206
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1206
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1206
         AG_RMN(1)=SWAM_RMIN                                              1206
         AG_RMN(2)=SWAM_RMIN                                              1206
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 1206
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 1206
         AG_TITLE = 'PGON'                                                1206
         AG_EXNAME = 'SHAPE'                                              1206
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1206
         CALL AXSHAPE                                                     1206
C    *                                                                    1206
6421  CALL AGSPOPB                                                        1207
      GO TO BLOCK_SWMB, (6420 ,6430)                                      1207
C End of Local Procedure                                                  1207
C ----------------------------------------------------------------------  1208
C ------------ SWMT                                                       1208
C Begin Local Procedure SWMT                                              1209
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            1209
6440  CONTINUE                                                            1209
         AG_TITLE='SWMT'                                                  1209
         AG_MODULE=MODULE                                                 1209
         AG_MARK='V00'                                                    1209
         CALL AGSPUSH                                                     1209
         AG_MODULE=MODULE                                                 1209
C    *                                                                    1209
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1210
         CALL AGSRESET                                                    1210
         AG_TITLE = 'ALUMINIUM'                                           1210
         AG_EXNAME = 'MATERIAL'                                           1210
         AG_PARLIST = 'NONE'                                              1210
         CALL AXMATERIAL                                                  1210
C    *                                                                    1210
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       1210
         CALL AGSRESET                                                    1211
         AG_SEEN = 1                                                      1211
         AG_COLO = 2                                                      1211
         AG_TITLE = 'SWMT'                                                1211
         AG_EXNAME = 'ATTRIBUTE'                                          1211
         AG_PARLIST = 'SEEN_COLO'                                         1211
         CALL AXATTRIBUTE                                                 1211
C    *                                                                    1211
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1211
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  1211
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          1211
         CALL AGSRESET                                                    1215
         AG_PHI1 = 0                                                      1215
         AG_DPHI = 360                                                    1215
         AG_NPDIV = 18                                                    1215
         AG_NZ = 2                                                        1215
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1215
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1215
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 1215
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 1215
         AG_RMX(1)=SWAM_RMAX                                              1215
         AG_RMX(2)=SWAM_RMAX                                              1215
         AG_TITLE = 'PGON'                                                1215
         AG_EXNAME = 'SHAPE'                                              1215
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1215
         CALL AXSHAPE                                                     1215
C    *                                                                    1215
6441  CALL AGSPOPB                                                        1216
      GO TO BLOCK_SWMT, (6440 ,6450)                                      1216
C End of Local Procedure                                                  1216
C ----------------------------------------------------------------------  1217
C ------------ SWMS                                                       1217
C Begin Local Procedure SWMS                                              1218
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  1218
6460  CONTINUE                                                            1218
         AG_TITLE='SWMS'                                                  1218
         AG_MODULE=MODULE                                                 1218
         AG_MARK='V00'                                                    1218
         CALL AGSPUSH                                                     1218
         AG_MODULE=MODULE                                                 1218
C    *                                                                    1218
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1219
         CALL AGSRESET                                                    1219
         AG_TITLE = 'ALUMINIUM'                                           1219
         AG_EXNAME = 'MATERIAL'                                           1219
         AG_PARLIST = 'NONE'                                              1219
         CALL AXMATERIAL                                                  1219
C    *                                                                    1219
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       1219
         CALL AGSRESET                                                    1220
         AG_SEEN = 1                                                      1220
         AG_COLO = 2                                                      1220
         AG_TITLE = 'SWMS'                                                1220
         AG_EXNAME = 'ATTRIBUTE'                                          1220
         AG_PARLIST = 'SEEN_COLO'                                         1220
         CALL AXATTRIBUTE                                                 1220
C    *                                                                    1220
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  1220
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  1220
C    WAM_RMAX} <===                                                       1220
         CALL AGSRESET                                                    1223
         AG_PHI1 = 0                                                      1223
         AG_DPHI = 360                                                    1223
         AG_NPDIV = 18                                                    1223
         AG_NZ = 2                                                        1223
         AG_ZI(1)=-SWAM_WALLTHK/2                                         1223
         AG_ZI(2)=SWAM_WALLTHK/2                                          1223
         AG_RMN(1)=SWAM_RMIN                                              1223
         AG_RMN(2)=SWAM_RMIN                                              1223
         AG_RMX(1)=SWAM_RMAX                                              1223
         AG_RMX(2)=SWAM_RMAX                                              1223
         AG_TITLE = 'PGON'                                                1223
         AG_EXNAME = 'SHAPE'                                              1223
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1223
         CALL AXSHAPE                                                     1223
C    *                                                                    1223
6461  CALL AGSPOPB                                                        1224
      GO TO BLOCK_SWMS, (6460 ,6470)                                      1224
C End of Local Procedure                                                  1224
C ----------------------------------------------------------------------  1225
C ------------ SWMW                                                       1225
C Begin Local Procedure SWMW                                              1226
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 1226
6480  CONTINUE                                                            1226
         AG_TITLE='SWMW'                                                  1226
         AG_MODULE=MODULE                                                 1226
         AG_MARK='V00'                                                    1226
         CALL AGSPUSH                                                     1226
         AG_MODULE=MODULE                                                 1226
C    *                                                                    1226
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       1227
         CALL AGSRESET                                                    1227
         AG_SEEN = 1                                                      1227
         AG_COLO = 6                                                      1227
         AG_TITLE = 'SWMW'                                                1227
         AG_EXNAME = 'ATTRIBUTE'                                          1227
         AG_PARLIST = 'SEEN_COLO'                                         1227
         CALL AXATTRIBUTE                                                 1227
C    *                                                                    1227
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1227
         CALL AGSRESET                                                    1228
         AG_TITLE = 'WATER'                                               1228
         AG_EXNAME = 'MATERIAL'                                           1228
         AG_PARLIST = 'NONE'                                              1228
         CALL AXMATERIAL                                                  1228
C    *                                                                    1228
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1228
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  1228
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  1228
C    -SWAM_WALLTHK} <===                                                  1228
         CALL AGSRESET                                                    1233
         AG_PHI1 = 0                                                      1233
         AG_DPHI = 360                                                    1233
         AG_NPDIV = 18                                                    1233
         AG_NZ = 2                                                        1233
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1233
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1233
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 1233
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 1233
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 1233
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 1233
         AG_TITLE = 'PGON'                                                1233
         AG_EXNAME = 'SHAPE'                                              1233
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1233
         CALL AXSHAPE                                                     1233
C    *                                                                    1233
6481  CALL AGSPOPB                                                        1234
      GO TO BLOCK_SWMW, (6480 ,6490)                                      1234
C End of Local Procedure                                                  1234
C ----------------------------------------------------------------------  1235
C ------------ SOTB                                                       1235
C Begin Local Procedure SOTB                                              1236
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            1236
6500  CONTINUE                                                            1236
         AG_TITLE='SOTB'                                                  1236
         AG_MODULE=MODULE                                                 1236
         AG_MARK='V00'                                                    1236
         CALL AGSPUSH                                                     1236
         AG_MODULE=MODULE                                                 1236
C    *                                                                    1236
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1237
         CALL AGSRESET                                                    1237
         AG_TITLE = 'G10'                                                 1237
         AG_EXNAME = 'MATERIAL'                                           1237
         AG_PARLIST = 'NONE'                                              1237
         CALL AXMATERIAL                                                  1237
C    *                                                                    1237
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       1237
         CALL AGSRESET                                                    1238
         AG_SEEN = 1                                                      1238
         AG_COLO = 3                                                      1238
         AG_TITLE = 'SOTB'                                                1238
         AG_EXNAME = 'ATTRIBUTE'                                          1238
         AG_PARLIST = 'SEEN_COLO'                                         1238
         CALL AXATTRIBUTE                                                 1238
C    *                                                                    1238
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1238
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  1238
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    1238
         CALL AGSRESET                                                    1241
         AG_PHI1 = 0                                                      1241
         AG_DPHI = 360                                                    1241
         AG_NPDIV = 18                                                    1241
         AG_NZ = 2                                                        1241
         AG_ZI(1)=-SWAM_LEN/2                                             1241
         AG_ZI(2)=+SWAM_LEN/2                                             1241
         AG_RMN(1)=SWAM_RMAX                                              1241
         AG_RMN(2)=SWAM_RMAX                                              1241
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 1241
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 1241
         AG_TITLE = 'PGON'                                                1241
         AG_EXNAME = 'SHAPE'                                              1241
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1241
         CALL AXSHAPE                                                     1241
C    *                                                                    1241
6501  CALL AGSPOPB                                                        1242
      GO TO BLOCK_SOTB, (6500 ,6510)                                      1242
C End of Local Procedure                                                  1242
C ----------------------------------------------------------------------  1243
C ------------ SITB                                                       1243
C Begin Local Procedure SITB                                              1244
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            1244
6520  CONTINUE                                                            1244
         AG_TITLE='SITB'                                                  1244
         AG_MODULE=MODULE                                                 1244
         AG_MARK='V00'                                                    1244
         CALL AGSPUSH                                                     1244
         AG_MODULE=MODULE                                                 1244
C    *                                                                    1244
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1245
         CALL AGSRESET                                                    1245
         AG_A = 12                                                        1245
         AG_Z = 6                                                         1245
         AG_W = 1                                                         1245
         AG_TITLE = 'C'                                                   1245
         AG_EXNAME = 'COMPONENT'                                          1245
         AG_PARLIST = 'A_Z_W'                                             1245
         CALL AXCOMPONENT                                                 1245
C    *                                                                    1245
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1245
         CALL AGSRESET                                                    1246
         AG_A = 1                                                         1246
         AG_Z = 1                                                         1246
         AG_W = 2                                                         1246
         AG_TITLE = 'H2'                                                  1246
         AG_EXNAME = 'COMPONENT'                                          1246
         AG_PARLIST = 'A_Z_W'                                             1246
         CALL AXCOMPONENT                                                 1246
C    *                                                                    1246
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1246
         CALL AGSRESET                                                    1247
         AG_DENS = 0.935                                                  1247
         AG_TITLE = 'CH2'                                                 1247
         AG_EXNAME = 'MIXTURE'                                            1247
         AG_PARLIST = 'DENS'                                              1247
         CALL AXMIXTURE                                                   1247
C    *                                                                    1247
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       1247
         CALL AGSRESET                                                    1248
         AG_SEEN = 1                                                      1248
         AG_COLO = 3                                                      1248
         AG_TITLE = 'SITB'                                                1248
         AG_EXNAME = 'ATTRIBUTE'                                          1248
         AG_PARLIST = 'SEEN_COLO'                                         1248
         CALL AXATTRIBUTE                                                 1248
C    *                                                                    1248
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1248
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  1248
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   1248
         CALL AGSRESET                                                    1252
         AG_PHI1 = 0                                                      1252
         AG_DPHI = 360                                                    1252
         AG_NPDIV = 18                                                    1252
         AG_NZ = 2                                                        1252
         AG_ZI(1)=-SWAM_LEN/2                                             1252
         AG_ZI(2)=+SWAM_LEN/2                                             1252
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 1252
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 1252
         AG_RMX(1)=SWAM_RMIN                                              1252
         AG_RMX(2)=SWAM_RMIN                                              1252
         AG_TITLE = 'PGON'                                                1252
         AG_EXNAME = 'SHAPE'                                              1252
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1252
         CALL AXSHAPE                                                     1252
C    *                                                                    1252
6521  CALL AGSPOPB                                                        1253
      GO TO BLOCK_SITB, (6520 ,6530)                                      1253
C End of Local Procedure                                                  1253
C ----------------------------------------------------------------------  1254
C ------------ SBWC                                                       1254
C Begin Local Procedure SBWC                                              1255
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           1255
5190  CONTINUE                                                            1255
         AG_TITLE='SBWC'                                                  1255
         AG_MODULE=MODULE                                                 1255
         AG_MARK='V00'                                                    1255
         CALL AGSPUSH                                                     1255
         AG_MODULE=MODULE                                                 1255
C    *                                                                    1255
         RIN=SWAM_RMIN                                                    1256
         ROU=SSUP_CON1IDMN                                                1257
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1258
         CALL AGSRESET                                                    1258
         AG_TITLE = 'AIR'                                                 1258
         AG_EXNAME = 'MATERIAL'                                           1258
         AG_PARLIST = 'NONE'                                              1258
         CALL AXMATERIAL                                                  1258
C    *                                                                    1258
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       1258
         CALL AGSRESET                                                    1259
         AG_SEEN = 0                                                      1259
         AG_COLO = 1                                                      1259
         AG_TITLE = 'SBWC'                                                1259
         AG_EXNAME = 'ATTRIBUTE'                                          1259
         AG_PARLIST = 'SEEN_COLO'                                         1259
         CALL AXATTRIBUTE                                                 1259
C    *                                                                    1259
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  1259
C    MIN+SWAM_LEN))/2 <===                                                1259
         CALL AGSRESET                                                    1260
         AG_RMIN = RIN                                                    1260
         AG_RMAX = ROU                                                    1260
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   1260
         AG_TITLE = 'TUBE'                                                1260
         AG_EXNAME = 'SHAPE'                                              1260
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1260
         CALL AXSHAPE                                                     1260
C    *                                                                    1260
C    Create SWCM :                                                        1261
C    Call To Local Procedure SWCM                                         1261
         ASSIGN 6550 TO BLOCK_SWCM                                        1261
         GO TO  6540                                                      1261
6550     CONTINUE                                                         1261
C    *                                                                    1261
5191  CALL AGSPOPB                                                        1262
      GO TO BLOCK_SBWC, (5190 ,5200)                                      1262
C End of Local Procedure                                                  1262
C ----------------------------------------------------------------------  1263
C ------------ SWCM                                                       1263
C Begin Local Procedure SWCM                                              1264
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   1264
6540  CONTINUE                                                            1264
         AG_TITLE='SWCM'                                                  1264
         AG_MODULE=MODULE                                                 1264
         AG_MARK='V00'                                                    1264
         CALL AGSPUSH                                                     1264
         AG_MODULE=MODULE                                                 1264
C    *                                                                    1264
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       1265
         CALL AGSRESET                                                    1265
         AG_SEEN = 0                                                      1265
         AG_COLO = 1                                                      1265
         AG_TITLE = 'SWCM'                                                1265
         AG_EXNAME = 'ATTRIBUTE'                                          1265
         AG_PARLIST = 'SEEN_COLO'                                         1265
         CALL AXATTRIBUTE                                                 1265
C    *                                                                    1265
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 1265
         CALL AGSRESET                                                    1266
         AG_NDIV = 3                                                      1266
         AG_IAXIS = 2                                                     1266
         AG_C0 = 0                                                        1266
         AG_TITLE = 'DIVISION'                                            1266
         AG_EXNAME = 'SHAPE'                                              1266
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1266
         CALL AXSHAPE                                                     1266
C    *                                                                    1266
C    Create SXAI :                                                        1267
C    Call To Local Procedure SXAI                                         1267
         ASSIGN 6570 TO BLOCK_SXAI                                        1267
         GO TO  6560                                                      1267
6570     CONTINUE                                                         1267
C    *                                                                    1267
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  1268
C    /2+SSUP_BRATHK/2) <===                                               1268
         CALL AGSRESET                                                    1268
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   1268
         AG_TITLE = 'SXAI'                                                1268
         AG_EXNAME = 'POSITION'                                           1268
         AG_PARLIST = 'Z'                                                 1268
         CALL AXPOSITION                                                  1268
C    *                                                                    1268
C    Create SXBI :                                                        1269
C    Call To Local Procedure SXBI                                         1269
         ASSIGN 6590 TO BLOCK_SXBI                                        1269
         GO TO  6580                                                      1269
6590     CONTINUE                                                         1269
C    *                                                                    1269
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    1270
         CALL AGSRESET                                                    1270
         AG_Z = (SSUP_BRATHK/2)                                           1270
         AG_TITLE = 'SXBI'                                                1270
         AG_EXNAME = 'POSITION'                                           1270
         AG_PARLIST = 'Z'                                                 1270
         CALL AXPOSITION                                                  1270
C    *                                                                    1270
6541  CALL AGSPOPB                                                        1271
      GO TO BLOCK_SWCM, (6540 ,6550)                                      1271
C End of Local Procedure                                                  1271
C ----------------------------------------------------------------------  1272
C ------------ SXAI                                                       1272
C Begin Local Procedure SXAI                                              1273
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      1273
6560  CONTINUE                                                            1273
         AG_TITLE='SXAI'                                                  1273
         AG_MODULE=MODULE                                                 1273
         AG_MARK='V00'                                                    1273
         CALL AGSPUSH                                                     1273
         AG_MODULE=MODULE                                                 1273
C    *                                                                    1273
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       1274
         CALL AGSRESET                                                    1274
         AG_SEEN = 1                                                      1274
         AG_COLO = 2                                                      1274
         AG_TITLE = 'SXAI'                                                1274
         AG_EXNAME = 'ATTRIBUTE'                                          1274
         AG_PARLIST = 'SEEN_COLO'                                         1274
         CALL AXATTRIBUTE                                                 1274
C    *                                                                    1274
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1274
         CALL AGSRESET                                                    1275
         AG_TITLE = 'ALUMINIUM'                                           1275
         AG_EXNAME = 'MATERIAL'                                           1275
         AG_PARLIST = 'NONE'                                              1275
         CALL AXMATERIAL                                                  1275
C    *                                                                    1275
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   1275
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         1275
         CALL AGSRESET                                                    1277
         AG_RMIN = SWAM_RMIN                                              1277
         AG_RMAX = SSUP_CON1IDMN                                          1277
         AG_PHI1 = -5                                                     1277
         AG_PHI2 = 5                                                      1277
         AG_DZ = SSUP_BRATHK/2                                            1277
         AG_TITLE = 'TUBS'                                                1277
         AG_EXNAME = 'SHAPE'                                              1277
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1277
         CALL AXSHAPE                                                     1277
C    *                                                                    1277
6561  CALL AGSPOPB                                                        1278
      GO TO BLOCK_SXAI, (6560 ,6570)                                      1278
C End of Local Procedure                                                  1278
C ----------------------------------------------------------------------  1279
C ------------ SXBI                                                       1279
C Begin Local Procedure SXBI                                              1280
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     1280
6580  CONTINUE                                                            1280
         AG_TITLE='SXBI'                                                  1280
         AG_MODULE=MODULE                                                 1280
         AG_MARK='V00'                                                    1280
         CALL AGSPUSH                                                     1280
         AG_MODULE=MODULE                                                 1280
C    *                                                                    1280
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       1281
         CALL AGSRESET                                                    1281
         AG_SEEN = 1                                                      1281
         AG_COLO = 2                                                      1281
         AG_TITLE = 'SXBI'                                                1281
         AG_EXNAME = 'ATTRIBUTE'                                          1281
         AG_PARLIST = 'SEEN_COLO'                                         1281
         CALL AXATTRIBUTE                                                 1281
C    *                                                                    1281
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1281
         CALL AGSRESET                                                    1282
         AG_TITLE = 'ALUMINIUM'                                           1282
         AG_EXNAME = 'MATERIAL'                                           1282
         AG_PARLIST = 'NONE'                                              1282
         CALL AXMATERIAL                                                  1282
C    *                                                                    1282
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  1282
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  1282
C    _BRATHK)/2) <===                                                     1282
         CALL AGSRESET                                                    1284
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              1284
         AG_RMAX = SSUP_CON1IDMN                                          1284
         AG_PHI1 = -5                                                     1284
         AG_PHI2 = 5                                                      1284
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     1284
         AG_TITLE = 'TUBS'                                                1284
         AG_EXNAME = 'SHAPE'                                              1284
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1284
         CALL AXSHAPE                                                     1284
C    *                                                                    1284
6581  CALL AGSPOPB                                                        1285
      GO TO BLOCK_SXBI, (6580 ,6590)                                      1285
C End of Local Procedure                                                  1285
C ----------------------------------------------------------------------  1286
C ------------ SCRW                                                       1286
C Begin Local Procedure SCRW                                              1287
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   1287
6100  CONTINUE                                                            1287
         AG_TITLE='SCRW'                                                  1287
         AG_MODULE=MODULE                                                 1287
         AG_MARK='V00'                                                    1287
         CALL AGSPUSH                                                     1287
         AG_MODULE=MODULE                                                 1287
C    *                                                                    1287
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        1288
         CALL AGSRESET                                                    1288
         AG_TITLE = 'BERILLIUM'                                           1288
         AG_EXNAME = 'MATERIAL'                                           1288
         AG_PARLIST = 'NONE'                                              1288
         CALL AXMATERIAL                                                  1288
C    *                                                                    1288
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       1288
         CALL AGSRESET                                                    1289
         AG_SEEN = 1                                                      1289
         AG_COLO = 2                                                      1289
         AG_TITLE = 'SCRW'                                                1289
         AG_EXNAME = 'ATTRIBUTE'                                          1289
         AG_PARLIST = 'SEEN_COLO'                                         1289
         CALL AXATTRIBUTE                                                 1289
C    *                                                                    1289
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  1289
C    NDRNG_Z) <===                                                        1289
         CALL AGSRESET                                                    1290
         AG_RMIN = 0                                                      1290
         AG_RMAX = SSUP_ERJDIA/2                                          1290
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   1290
         AG_TITLE = 'TUBE'                                                1290
         AG_EXNAME = 'SHAPE'                                              1290
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1290
         CALL AXSHAPE                                                     1290
C    *                                                                    1290
6101  CALL AGSPOPB                                                        1291
      GO TO BLOCK_SCRW, (6100 ,6110,6140)                                 1291
C End of Local Procedure                                                  1291
C ----------------------------------------------------------------------  1292
C ------------ SBSP                                                       1292
C Begin Local Procedure SBSP                                              1293
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1293
5230  CONTINUE                                                            1293
         AG_TITLE='SBSP'                                                  1293
         AG_MODULE=MODULE                                                 1293
         AG_MARK='V00'                                                    1293
         CALL AGSPUSH                                                     1293
         AG_MODULE=MODULE                                                 1293
C    *                                                                    1293
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1294
         CALL AGSRESET                                                    1294
         AG_TITLE = 'AIR'                                                 1294
         AG_EXNAME = 'MATERIAL'                                           1294
         AG_PARLIST = 'NONE'                                              1294
         CALL AXMATERIAL                                                  1294
C    *                                                                    1294
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1294
         CALL AGSRESET                                                    1295
         AG_SEEN = 0                                                      1295
         AG_COLO = 1                                                      1295
         AG_TITLE = 'SBSP'                                                1295
         AG_EXNAME = 'ATTRIBUTE'                                          1295
         AG_PARLIST = 'SEEN_COLO'                                         1295
         CALL AXATTRIBUTE                                                 1295
C    *                                                                    1295
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1295
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1295
         CALL AGSRESET                                                    1297
         AG_RMIN = SVTG_RSIZEMIN                                          1297
         AG_RMAX = SSUB_KMOUNTOD/2                                        1297
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1297
         AG_TITLE = 'TUBE'                                                1297
         AG_EXNAME = 'SHAPE'                                              1297
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1297
         CALL AXSHAPE                                                     1297
C    *                                                                    1297
C    Create SAKM :                                                        1298
C    Call To Local Procedure SAKM                                         1298
         ASSIGN 6610 TO BLOCK_SAKM                                        1298
         GO TO  6600                                                      1298
6610     CONTINUE                                                         1298
C    *                                                                    1298
C    Geant POSITION : SAKM ===> inherited arguments used <===             1299
         CALL AGSRESET                                                    1299
         AG_TITLE = 'SAKM'                                                1299
         AG_EXNAME = 'POSITION'                                           1299
         AG_PARLIST = 'NONE'                                              1299
         CALL AXPOSITION                                                  1299
C    *                                                                    1299
C    Create SBMM :                                                        1300
C    Call To Local Procedure SBMM                                         1300
         ASSIGN 6630 TO BLOCK_SBMM                                        1300
         GO TO  6620                                                      1300
6630     CONTINUE                                                         1300
C    *                                                                    1300
C    Create SBMM :                                                        1301
C    Call To Local Procedure SBMM                                         1301
         ASSIGN 6640 TO BLOCK_SBMM                                        1301
         GO TO  6620                                                      1301
6640     CONTINUE                                                         1301
C    *                                                                    1301
C    Create SBRL :                                                        1302
C    Call To Local Procedure SBRL                                         1302
         ASSIGN 6660 TO BLOCK_SBRL                                        1302
         GO TO  6650                                                      1302
6660     CONTINUE                                                         1302
C    *                                                                    1302
C    Create SBRX :                                                        1303
C    Call To Local Procedure SBRX                                         1303
         ASSIGN 6680 TO BLOCK_SBRX                                        1303
         GO TO  6670                                                      1303
6680     CONTINUE                                                         1303
C    *                                                                    1303
C    *                                                                    1304
         I=-1                                                             1304
            GOTO 6693                                                     1304
6691        I=I+(2)                                                       1304
6693        IF((2)*(I-(1)).GT.0) GO TO 6692                               1304
C       *                                                                 1305
            J=0                                                           1305
               GO TO 6703                                                 1305
6701           J=J+1                                                      1305
6703           IF(J-(1).GT.0) GO TO 6702                                  1305
               PHI=I*SSUB_SRCUTPHI+180*J                                  1306
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1307
     *         2.0)                                                       1307
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1309
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1309
C          ==                                                             1309
               CALL AGSRESET                                              1309
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1309
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1309
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1309
               AG_ALPHAN=AG_ALPHAN+1                                      1309
               AG_ALPHAK(AG_ALPHAN)=3                                     1309
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1309
               AG_TITLE = 'SBMM'                                          1309
               AG_EXNAME = 'POSITION'                                     1309
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1309
               CALL AXPOSITION                                            1309
C          *                                                              1309
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1309
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1312
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1312
C           ORT=ZXY <===                                                  1312
               CALL AGSRESET                                              1312
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1312
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1312
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1312
               AG_ALPHAN=AG_ALPHAN+1                                      1312
               AG_ALPHAK(AG_ALPHAN)=3                                     1312
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1312
               AG_ALPHAN=AG_ALPHAN+1                                      1312
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1312
               AG_TITLE = 'SBRL'                                          1312
               AG_EXNAME = 'POSITION'                                     1312
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1312
               CALL AXPOSITION                                            1312
C          *                                                              1312
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1312
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1312
C           ORT=ZXY <===                                                  1312
               CALL AGSRESET                                              1314
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1314
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1314
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1314
               AG_ALPHAN=AG_ALPHAN+1                                      1314
               AG_ALPHAK(AG_ALPHAN)=3                                     1314
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1314
               AG_ALPHAN=AG_ALPHAN+1                                      1314
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1314
               AG_TITLE = 'SBRX'                                          1314
               AG_EXNAME = 'POSITION'                                     1314
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1314
               CALL AXPOSITION                                            1314
C          *                                                              1314
            GO TO 6701                                                    1315
6702        CONTINUE                                                      1315
         GO TO 6691                                                       1316
6692     CONTINUE                                                         1316
C    Create SBSR :                                                        1317
C    Call To Local Procedure SBSR                                         1317
         ASSIGN 6720 TO BLOCK_SBSR                                        1317
         GO TO  6710                                                      1317
6720     CONTINUE                                                         1317
C    *                                                                    1317
C    Geant POSITION : SBSR ===> inherited arguments used <===             1318
         CALL AGSRESET                                                    1318
         AG_TITLE = 'SBSR'                                                1318
         AG_EXNAME = 'POSITION'                                           1318
         AG_PARLIST = 'NONE'                                              1318
         CALL AXPOSITION                                                  1318
C    *                                                                    1318
5231  CALL AGSPOPB                                                        1319
      GO TO BLOCK_SBSP, (5230 ,5240)                                      1319
C End of Local Procedure                                                  1319
C ----------------------------------------------------------------------  1320
C ------------ SAKM                                                       1320
C Begin Local Procedure SAKM                                              1321
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1321
6600  CONTINUE                                                            1321
         AG_TITLE='SAKM'                                                  1321
         AG_MODULE=MODULE                                                 1321
         AG_MARK='V00'                                                    1321
         CALL AGSPUSH                                                     1321
         AG_MODULE=MODULE                                                 1321
C    *                                                                    1321
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1322
         CALL AGSRESET                                                    1322
         AG_TITLE = 'ALUMINIUM'                                           1322
         AG_EXNAME = 'MATERIAL'                                           1322
         AG_PARLIST = 'NONE'                                              1322
         CALL AXMATERIAL                                                  1322
C    *                                                                    1322
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1322
         CALL AGSRESET                                                    1323
         AG_SEEN = 1                                                      1323
         AG_COLO = 2                                                      1323
         AG_TITLE = 'SAKM'                                                1323
         AG_EXNAME = 'ATTRIBUTE'                                          1323
         AG_PARLIST = 'SEEN_COLO'                                         1323
         CALL AXATTRIBUTE                                                 1323
C    *                                                                    1323
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1323
C    DZ=SSUB_KMNTTHK/2 <===                                               1323
         CALL AGSRESET                                                    1325
         AG_RMIN = SSUB_KMOUNTID/2                                        1325
         AG_RMAX = SSUB_KMOUNTOD/2                                        1325
         AG_DZ = SSUB_KMNTTHK/2                                           1325
         AG_TITLE = 'TUBE'                                                1325
         AG_EXNAME = 'SHAPE'                                              1325
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1325
         CALL AXSHAPE                                                     1325
C    *                                                                    1325
C    Create SCKM :                                                        1326
C    Call To Local Procedure SCKM                                         1326
         ASSIGN 6740 TO BLOCK_SCKM                                        1326
         GO TO  6730                                                      1326
6740     CONTINUE                                                         1326
C    *                                                                    1326
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1327
         CALL AGSRESET                                                    1327
         AG_Y = SSUB_KMCUTOFF                                             1327
         AG_KONLY = 'MANY'                                                1327
         AG_TITLE = 'SCKM'                                                1327
         AG_EXNAME = 'POSITION'                                           1327
         AG_PARLIST = 'Y_KONLY'                                           1327
         CALL AXPOSITION                                                  1327
C    *                                                                    1327
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1327
C     <===                                                                1327
         CALL AGSRESET                                                    1328
         AG_Y = -SSUB_KMCUTOFF                                            1328
         AG_ALPHAN=AG_ALPHAN+1                                            1328
         AG_ALPHAK(AG_ALPHAN)=3                                           1328
         AG_ALPHAL(AG_ALPHAN)=180                                         1328
         AG_KONLY = 'MANY'                                                1328
         AG_TITLE = 'SCKM'                                                1328
         AG_EXNAME = 'POSITION'                                           1328
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1328
         CALL AXPOSITION                                                  1328
C    *                                                                    1328
6601  CALL AGSPOPB                                                        1329
      GO TO BLOCK_SAKM, (6600 ,6610)                                      1329
C End of Local Procedure                                                  1329
C ----------------------------------------------------------------------  1330
C ------------ SCKM                                                       1330
C Begin Local Procedure SCKM                                              1331
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1331
6730  CONTINUE                                                            1331
         AG_TITLE='SCKM'                                                  1331
         AG_MODULE=MODULE                                                 1331
         AG_MARK='V00'                                                    1331
         CALL AGSPUSH                                                     1331
         AG_MODULE=MODULE                                                 1331
C    *                                                                    1331
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1332
         CALL AGSRESET                                                    1332
         AG_TITLE = 'AIR'                                                 1332
         AG_EXNAME = 'MATERIAL'                                           1332
         AG_PARLIST = 'NONE'                                              1332
         CALL AXMATERIAL                                                  1332
C    *                                                                    1332
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1332
         CALL AGSRESET                                                    1333
         AG_SEEN = 1                                                      1333
         AG_COLO = 1                                                      1333
         AG_TITLE = 'SCKM'                                                1333
         AG_EXNAME = 'ATTRIBUTE'                                          1333
         AG_PARLIST = 'SEEN_COLO'                                         1333
         CALL AXATTRIBUTE                                                 1333
C    *                                                                    1333
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1333
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1333
         CALL AGSRESET                                                    1336
         AG_RMIN = SSUB_KMCUTID/2                                         1336
         AG_RMAX = SSUB_KMCUTOD/2                                         1336
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1336
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1336
         AG_DZ = SSUB_KMNTTHK/2                                           1336
         AG_TITLE = 'TUBS'                                                1336
         AG_EXNAME = 'SHAPE'                                              1336
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1336
         CALL AXSHAPE                                                     1336
C    *                                                                    1336
6731  CALL AGSPOPB                                                        1337
      GO TO BLOCK_SCKM, (6730 ,6740)                                      1337
C End of Local Procedure                                                  1337
C ----------------------------------------------------------------------  1338
C ------------ SBSR                                                       1338
C Begin Local Procedure SBSR                                              1339
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1339
6710  CONTINUE                                                            1339
         AG_TITLE='SBSR'                                                  1339
         AG_MODULE=MODULE                                                 1339
         AG_MARK='V00'                                                    1339
         CALL AGSPUSH                                                     1339
         AG_MODULE=MODULE                                                 1339
C    *                                                                    1339
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1340
         CALL AGSRESET                                                    1340
         AG_TITLE = 'G10'                                                 1340
         AG_EXNAME = 'MATERIAL'                                           1340
         AG_PARLIST = 'NONE'                                              1340
         CALL AXMATERIAL                                                  1340
C    *                                                                    1340
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1340
         CALL AGSRESET                                                    1341
         AG_SEEN = 1                                                      1341
         AG_COLO = 3                                                      1341
         AG_TITLE = 'SBSR'                                                1341
         AG_EXNAME = 'ATTRIBUTE'                                          1341
         AG_PARLIST = 'SEEN_COLO'                                         1341
         CALL AXATTRIBUTE                                                 1341
C    *                                                                    1341
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1341
C    =SSUB_SRINGTHK/2 <===                                                1341
         CALL AGSRESET                                                    1343
         AG_RMIN = SSUB_SRINGID/2                                         1343
         AG_RMAX = SSUB_SRINGOD/2                                         1343
         AG_DZ = SSUB_SRINGTHK/2                                          1343
         AG_TITLE = 'TUBE'                                                1343
         AG_EXNAME = 'SHAPE'                                              1343
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1343
         CALL AXSHAPE                                                     1343
C    *                                                                    1343
C    Create SBCR :                                                        1344
C    Call To Local Procedure SBCR                                         1344
         ASSIGN 6760 TO BLOCK_SBCR                                        1344
         GO TO  6750                                                      1344
6760     CONTINUE                                                         1344
C    *                                                                    1344
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1345
C    *                                                                    1346
         I=-1                                                             1346
            GOTO 6773                                                     1346
6771        I=I+(2)                                                       1346
6773        IF((2)*(I-(1)).GT.0) GO TO 6772                               1346
C       *                                                                 1347
            J=0                                                           1347
               GO TO 6783                                                 1347
6781           J=J+1                                                      1347
6783           IF(J-(1).GT.0) GO TO 6782                                  1347
               PHI=I*SSUB_SRCUTPHI+180*J                                  1348
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1350
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1350
               CALL AGSRESET                                              1350
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1350
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1350
               AG_ALPHAN=AG_ALPHAN+1                                      1350
               AG_ALPHAK(AG_ALPHAN)=3                                     1350
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1350
               AG_KONLY = 'MANY'                                          1350
               AG_TITLE = 'SBCR'                                          1350
               AG_EXNAME = 'POSITION'                                     1350
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1350
               CALL AXPOSITION                                            1350
C          *                                                              1350
            GO TO 6781                                                    1351
6782        CONTINUE                                                      1351
         GO TO 6771                                                       1352
6772     CONTINUE                                                         1352
6711  CALL AGSPOPB                                                        1353
      GO TO BLOCK_SBSR, (6710 ,6720)                                      1353
C End of Local Procedure                                                  1353
C ----------------------------------------------------------------------  1354
C ------------ SBCR                                                       1354
C Begin Local Procedure SBCR                                              1355
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1355
6750  CONTINUE                                                            1355
         AG_TITLE='SBCR'                                                  1355
         AG_MODULE=MODULE                                                 1355
         AG_MARK='V00'                                                    1355
         CALL AGSPUSH                                                     1355
         AG_MODULE=MODULE                                                 1355
C    *                                                                    1355
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1356
         CALL AGSRESET                                                    1356
         AG_TITLE = 'AIR'                                                 1356
         AG_EXNAME = 'MATERIAL'                                           1356
         AG_PARLIST = 'NONE'                                              1356
         CALL AXMATERIAL                                                  1356
C    *                                                                    1356
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1356
         CALL AGSRESET                                                    1357
         AG_SEEN = 1                                                      1357
         AG_COLO = 1                                                      1357
         AG_TITLE = 'SBCR'                                                1357
         AG_EXNAME = 'ATTRIBUTE'                                          1357
         AG_PARLIST = 'SEEN_COLO'                                         1357
         CALL AXATTRIBUTE                                                 1357
C    *                                                                    1357
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1357
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1357
         CALL AGSRESET                                                    1359
         AG_DX = SSUB_SRCUTWID/2                                          1359
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1359
         AG_DZ = SSUB_SRINGTHK/2                                          1359
         AG_TITLE = 'BOX'                                                 1359
         AG_EXNAME = 'SHAPE'                                              1359
         AG_PARLIST = 'DX_DY_DZ'                                          1359
         CALL AXSHAPE                                                     1359
C    *                                                                    1359
6751  CALL AGSPOPB                                                        1360
      GO TO BLOCK_SBCR, (6750 ,6760)                                      1360
C End of Local Procedure                                                  1360
C ----------------------------------------------------------------------  1361
C ------------ SBRL                                                       1361
C Begin Local Procedure SBRL                                              1362
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1362
6650  CONTINUE                                                            1362
         AG_TITLE='SBRL'                                                  1362
         AG_MODULE=MODULE                                                 1362
         AG_MARK='V00'                                                    1362
         CALL AGSPUSH                                                     1362
         AG_MODULE=MODULE                                                 1362
C    *                                                                    1362
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1363
C    ADL=12.6 <===                                                        1363
         CALL AGSRESET                                                    1363
         AG_A = 20.719                                                    1363
         AG_Z = 10.307                                                    1363
         AG_DENS = 2.23                                                   1363
         AG_ABSL = 50.7                                                   1363
         AG_RADL = 12.6                                                   1363
         AG_TITLE = 'PYREX'                                               1363
         AG_EXNAME = 'MATERIAL'                                           1363
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1363
         CALL AXMATERIAL                                                  1363
C    *                                                                    1363
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1363
         CALL AGSRESET                                                    1364
         AG_SEEN = 1                                                      1364
         AG_COLO = 6                                                      1364
         AG_TITLE = 'SBRL'                                                1364
         AG_EXNAME = 'ATTRIBUTE'                                          1364
         AG_PARLIST = 'SEEN_COLO'                                         1364
         CALL AXATTRIBUTE                                                 1364
C    *                                                                    1364
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1364
C    =                                                                    1364
         CALL AGSRESET                                                    1365
         AG_RMIN = SSUB_SROLLID/2                                         1365
         AG_RMAX = SSUB_SROLLOD/2                                         1365
         AG_TITLE = 'TUBE'                                                1365
         AG_EXNAME = 'SHAPE'                                              1365
         AG_PARLIST = 'RMIN_RMAX'                                         1365
         CALL AXSHAPE                                                     1365
C    *                                                                    1365
6651  CALL AGSPOPB                                                        1366
      GO TO BLOCK_SBRL, (6650 ,6660)                                      1366
C End of Local Procedure                                                  1366
C ----------------------------------------------------------------------  1367
C ------------ SBRX                                                       1367
C Begin Local Procedure SBRX                                              1368
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1368
6670  CONTINUE                                                            1368
         AG_TITLE='SBRX'                                                  1368
         AG_MODULE=MODULE                                                 1368
         AG_MARK='V00'                                                    1368
         CALL AGSPUSH                                                     1368
         AG_MODULE=MODULE                                                 1368
C    *                                                                    1368
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1369
         CALL AGSRESET                                                    1369
         AG_TITLE = 'IRON'                                                1369
         AG_EXNAME = 'MATERIAL'                                           1369
         AG_PARLIST = 'NONE'                                              1369
         CALL AXMATERIAL                                                  1369
C    *                                                                    1369
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1369
         CALL AGSRESET                                                    1370
         AG_SEEN = 1                                                      1370
         AG_COLO = 2                                                      1370
         AG_TITLE = 'SBRX'                                                1370
         AG_EXNAME = 'ATTRIBUTE'                                          1370
         AG_PARLIST = 'SEEN_COLO'                                         1370
         CALL AXATTRIBUTE                                                 1370
C    *                                                                    1370
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1370
C    LEN/2 <===                                                           1370
         CALL AGSRESET                                                    1372
         AG_RMIN = 0.0                                                    1372
         AG_RMAX = SSUB_SROLLID/2                                         1372
         AG_DZ = SSUB_SWIRELEN/2                                          1372
         AG_TITLE = 'TUBE'                                                1372
         AG_EXNAME = 'SHAPE'                                              1372
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1372
         CALL AXSHAPE                                                     1372
C    *                                                                    1372
6671  CALL AGSPOPB                                                        1373
      GO TO BLOCK_SBRX, (6670 ,6680)                                      1373
C End of Local Procedure                                                  1373
C ----------------------------------------------------------------------  1374
C ------------ SBMM                                                       1374
C Begin Local Procedure SBMM                                              1375
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1375
6620  CONTINUE                                                            1375
         AG_TITLE='SBMM'                                                  1375
         AG_MODULE=MODULE                                                 1375
         AG_MARK='V00'                                                    1375
         CALL AGSPUSH                                                     1375
         AG_MODULE=MODULE                                                 1375
C    *                                                                    1375
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1376
         CALL AGSRESET                                                    1376
         AG_TITLE = 'AIR'                                                 1376
         AG_EXNAME = 'MATERIAL'                                           1376
         AG_PARLIST = 'NONE'                                              1376
         CALL AXMATERIAL                                                  1376
C    *                                                                    1376
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1376
         CALL AGSRESET                                                    1377
         AG_SEEN = 0                                                      1377
         AG_COLO = 1                                                      1377
         AG_TITLE = 'SBMM'                                                1377
         AG_EXNAME = 'ATTRIBUTE'                                          1377
         AG_PARLIST = 'SEEN_COLO'                                         1377
         CALL AXATTRIBUTE                                                 1377
C    *                                                                    1377
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1377
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1377
         CALL AGSRESET                                                    1379
         AG_DX = SSUB_MBLKIWID/2                                          1379
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1379
         AG_DZ = SSUB_MBLKHGH/2                                           1379
         AG_TITLE = 'BOX'                                                 1379
         AG_EXNAME = 'SHAPE'                                              1379
         AG_PARLIST = 'DX_DY_DZ'                                          1379
         CALL AXSHAPE                                                     1379
C    *                                                                    1379
C    Create SMRD :                                                        1380
C    Call To Local Procedure SMRD                                         1380
         ASSIGN 6800 TO BLOCK_SMRD                                        1380
         GO TO  6790                                                      1380
6800     CONTINUE                                                         1380
C    *                                                                    1380
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1381
     *   SSUB_SROLLOD                                                     1381
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1382
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1382
         CALL AGSRESET                                                    1382
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1382
         AG_ALPHAN=AG_ALPHAN+1                                            1382
         AG_ALPHAK(AG_ALPHAN)=1                                           1382
         AG_ALPHAL(AG_ALPHAN)=90                                          1382
         AG_TITLE = 'SMRD'                                                1382
         AG_EXNAME = 'POSITION'                                           1382
         AG_PARLIST = 'Y_ALPHAX'                                          1382
         CALL AXPOSITION                                                  1382
C    *                                                                    1382
C    Create SBMO :                                                        1383
C    Call To Local Procedure SBMO                                         1383
         ASSIGN 6820 TO BLOCK_SBMO                                        1383
         GO TO  6810                                                      1383
6820     CONTINUE                                                         1383
C    *                                                                    1383
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1384
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1385
C    UB_SROLLOD)/2 <===                                                   1385
         CALL AGSRESET                                                    1385
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1385
         AG_TITLE = 'SBMO'                                                1385
         AG_EXNAME = 'POSITION'                                           1385
         AG_PARLIST = 'Y'                                                 1385
         CALL AXPOSITION                                                  1385
C    *                                                                    1385
C    Create SBMI :                                                        1386
C    Call To Local Procedure SBMI                                         1386
         ASSIGN 6840 TO BLOCK_SBMI                                        1386
         GO TO  6830                                                      1386
6840     CONTINUE                                                         1386
C    *                                                                    1386
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1387
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1388
C    UB_SROLLOD)/2 <===                                                   1388
         CALL AGSRESET                                                    1388
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1388
         AG_TITLE = 'SBMI'                                                1388
         AG_EXNAME = 'POSITION'                                           1388
         AG_PARLIST = 'Y'                                                 1388
         CALL AXPOSITION                                                  1388
C    *                                                                    1388
6621  CALL AGSPOPB                                                        1389
      GO TO BLOCK_SBMM, (6620 ,6630,6640)                                 1389
C End of Local Procedure                                                  1389
C ----------------------------------------------------------------------  1390
C ------------ SMRD                                                       1390
C Begin Local Procedure SMRD                                              1391
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1391
6790  CONTINUE                                                            1391
         AG_TITLE='SMRD'                                                  1391
         AG_MODULE=MODULE                                                 1391
         AG_MARK='V00'                                                    1391
         CALL AGSPUSH                                                     1391
         AG_MODULE=MODULE                                                 1391
C    *                                                                    1391
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1392
         CALL AGSRESET                                                    1392
         AG_TITLE = 'ALUMINIUM'                                           1392
         AG_EXNAME = 'MATERIAL'                                           1392
         AG_PARLIST = 'NONE'                                              1392
         CALL AXMATERIAL                                                  1392
C    *                                                                    1392
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1392
         CALL AGSRESET                                                    1393
         AG_SEEN = 1                                                      1393
         AG_COLO = 2                                                      1393
         AG_TITLE = 'SMRD'                                                1393
         AG_EXNAME = 'ATTRIBUTE'                                          1393
         AG_PARLIST = 'SEEN_COLO'                                         1393
         CALL AXATTRIBUTE                                                 1393
C    *                                                                    1393
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1393
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1393
         CALL AGSRESET                                                    1395
         AG_RMIN = 0.0                                                    1395
         AG_RMAX = SSUB_MRODDIA/2                                         1395
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1395
         AG_TITLE = 'TUBE'                                                1395
         AG_EXNAME = 'SHAPE'                                              1395
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1395
         CALL AXSHAPE                                                     1395
C    *                                                                    1395
6791  CALL AGSPOPB                                                        1396
      GO TO BLOCK_SMRD, (6790 ,6800)                                      1396
C End of Local Procedure                                                  1396
C ----------------------------------------------------------------------  1397
C ------------ SBMO                                                       1397
C Begin Local Procedure SBMO                                              1398
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1398
6810  CONTINUE                                                            1398
         AG_TITLE='SBMO'                                                  1398
         AG_MODULE=MODULE                                                 1398
         AG_MARK='V00'                                                    1398
         CALL AGSPUSH                                                     1398
         AG_MODULE=MODULE                                                 1398
C    *                                                                    1398
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1399
         CALL AGSRESET                                                    1399
         AG_TITLE = 'G10'                                                 1399
         AG_EXNAME = 'MATERIAL'                                           1399
         AG_PARLIST = 'NONE'                                              1399
         CALL AXMATERIAL                                                  1399
C    *                                                                    1399
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1399
         CALL AGSRESET                                                    1400
         AG_SEEN = 1                                                      1400
         AG_COLO = 3                                                      1400
         AG_TITLE = 'SBMO'                                                1400
         AG_EXNAME = 'ATTRIBUTE'                                          1400
         AG_PARLIST = 'SEEN_COLO'                                         1400
         CALL AXATTRIBUTE                                                 1400
C    *                                                                    1400
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1400
C    UB_MBLKHGH/2 <===                                                    1400
         CALL AGSRESET                                                    1402
         AG_DX = SSUB_MBLKOWID/2                                          1402
         AG_DY = SSUB_MBLKOLEN/2                                          1402
         AG_DZ = SSUB_MBLKHGH/2                                           1402
         AG_TITLE = 'BOX'                                                 1402
         AG_EXNAME = 'SHAPE'                                              1402
         AG_PARLIST = 'DX_DY_DZ'                                          1402
         CALL AXSHAPE                                                     1402
C    *                                                                    1402
6811  CALL AGSPOPB                                                        1403
      GO TO BLOCK_SBMO, (6810 ,6820)                                      1403
C End of Local Procedure                                                  1403
C ----------------------------------------------------------------------  1404
C ------------ SBMI                                                       1404
C Begin Local Procedure SBMI                                              1405
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1405
6830  CONTINUE                                                            1405
         AG_TITLE='SBMI'                                                  1405
         AG_MODULE=MODULE                                                 1405
         AG_MARK='V00'                                                    1405
         CALL AGSPUSH                                                     1405
         AG_MODULE=MODULE                                                 1405
C    *                                                                    1405
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1406
         CALL AGSRESET                                                    1406
         AG_TITLE = 'G10'                                                 1406
         AG_EXNAME = 'MATERIAL'                                           1406
         AG_PARLIST = 'NONE'                                              1406
         CALL AXMATERIAL                                                  1406
C    *                                                                    1406
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1406
         CALL AGSRESET                                                    1407
         AG_SEEN = 1                                                      1407
         AG_COLO = 3                                                      1407
         AG_TITLE = 'SBMO'                                                1407
         AG_EXNAME = 'ATTRIBUTE'                                          1407
         AG_PARLIST = 'SEEN_COLO'                                         1407
         CALL AXATTRIBUTE                                                 1407
C    *                                                                    1407
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1407
C    UB_MBLKHGH/2 <===                                                    1407
         CALL AGSRESET                                                    1409
         AG_DX = SSUB_MBLKIWID/2                                          1409
         AG_DY = SSUB_MBLKILEN/2                                          1409
         AG_DZ = SSUB_MBLKHGH/2                                           1409
         AG_TITLE = 'BOX'                                                 1409
         AG_EXNAME = 'SHAPE'                                              1409
         AG_PARLIST = 'DX_DY_DZ'                                          1409
         CALL AXSHAPE                                                     1409
C    *                                                                    1409
6831  CALL AGSPOPB                                                        1410
      GO TO BLOCK_SBMI, (6830 ,6840)                                      1410
C End of Local Procedure                                                  1410
C ----------------------------------------------------------------------  1411
C ------------ SALM                                                       1411
C Begin Local Procedure SALM                                              1412
C SALM IS THE ALUMINUM SHIELD MESH                                        1412
5370  CONTINUE                                                            1412
         AG_TITLE='SALM'                                                  1412
         AG_MODULE=MODULE                                                 1412
         AG_MARK='V00'                                                    1412
         CALL AGSPUSH                                                     1412
         AG_MODULE=MODULE                                                 1412
C    *                                                                    1412
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1413
         CALL AGSRESET                                                    1413
         AG_TITLE = 'ALUMINIUM'                                           1413
         AG_EXNAME = 'MATERIAL'                                           1413
         AG_PARLIST = 'NONE'                                              1413
         CALL AXMATERIAL                                                  1413
C    *                                                                    1413
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1413
         CALL AGSRESET                                                    1414
         AG_SEEN = 1                                                      1414
         AG_COLO = 2                                                      1414
         AG_TITLE = 'SALM'                                                1414
         AG_EXNAME = 'ATTRIBUTE'                                          1414
         AG_PARLIST = 'SEEN_COLO'                                         1414
         CALL AXATTRIBUTE                                                 1414
C    *                                                                    1414
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1414
C    DZ=SSLD_ALMSHTHK/2 <===                                              1414
         CALL AGSRESET                                                    1416
         AG_RMIN = SSLD_ALMESHID/2                                        1416
         AG_RMAX = SSLD_ALMESHOD/2                                        1416
         AG_DZ = SSLD_ALMSHTHK/2                                          1416
         AG_TITLE = 'TUBE'                                                1416
         AG_EXNAME = 'SHAPE'                                              1416
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1416
         CALL AXSHAPE                                                     1416
C    *                                                                    1416
5371  CALL AGSPOPB                                                        1417
      GO TO BLOCK_SALM, (5370 ,5380)                                      1417
C End of Local Procedure                                                  1417
C ----------------------------------------------------------------------  1418
C ------------ SISH                                                       1418
C Begin Local Procedure SISH                                              1419
C SISH IS THE INNER SHIELD CYLINDER                                       1419
5310  CONTINUE                                                            1419
         AG_TITLE='SISH'                                                  1419
         AG_MODULE=MODULE                                                 1419
         AG_MARK='V00'                                                    1419
         CALL AGSPUSH                                                     1419
         AG_MODULE=MODULE                                                 1419
C    *                                                                    1419
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1420
         CALL AGSRESET                                                    1420
         AG_TITLE = 'ALKAP'                                               1420
         AG_EXNAME = 'MATERIAL'                                           1420
         AG_PARLIST = 'NONE'                                              1420
         CALL AXMATERIAL                                                  1420
C    *                                                                    1420
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1420
         CALL AGSRESET                                                    1421
         AG_SEEN = 1                                                      1421
         AG_COLO = 3                                                      1421
         AG_TITLE = 'SISH'                                                1421
         AG_EXNAME = 'ATTRIBUTE'                                          1421
         AG_PARLIST = 'SEEN_COLO'                                         1421
         CALL AXATTRIBUTE                                                 1421
C    *                                                                    1421
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1421
C    D_SINLEN <===                                                        1421
         CALL AGSRESET                                                    1423
         AG_RMIN = SSLD_SINRINN                                           1423
         AG_RMAX = SSLD_SINROUT                                           1423
         AG_DZ = SSLD_SINLEN                                              1423
         AG_TITLE = 'TUBE'                                                1423
         AG_EXNAME = 'SHAPE'                                              1423
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1423
         CALL AXSHAPE                                                     1423
C    *                                                                    1423
5311  CALL AGSPOPB                                                        1424
      GO TO BLOCK_SISH, (5310 ,5320)                                      1424
C End of Local Procedure                                                  1424
C ----------------------------------------------------------------------  1425
C ------------ SSSH                                                       1425
C Begin Local Procedure SSSH                                              1426
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1426
5350  CONTINUE                                                            1426
         AG_TITLE='SSSH'                                                  1426
         AG_MODULE=MODULE                                                 1426
         AG_MARK='V00'                                                    1426
         CALL AGSPUSH                                                     1426
         AG_MODULE=MODULE                                                 1426
C    *                                                                    1426
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1427
         CALL AGSRESET                                                    1427
         AG_TITLE = 'SSDALMY'                                             1427
         AG_EXNAME = 'MATERIAL'                                           1427
         AG_PARLIST = 'NONE'                                              1427
         CALL AXMATERIAL                                                  1427
C    *                                                                    1427
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1427
         CALL AGSRESET                                                    1428
         AG_SEEN = 1                                                      1428
         AG_COLO = 3                                                      1428
         AG_TITLE = 'SSSH'                                                1428
         AG_EXNAME = 'ATTRIBUTE'                                          1428
         AG_PARLIST = 'SEEN_COLO'                                         1428
         CALL AXATTRIBUTE                                                 1428
C    *                                                                    1428
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1428
C    SLD_SSEPLEN <===                                                     1428
         CALL AGSRESET                                                    1430
         AG_RMIN = SSLD_SSEPRINN                                          1430
         AG_RMAX = SSLD_SSEPROUT                                          1430
         AG_DZ = SSLD_SSEPLEN                                             1430
         AG_TITLE = 'TUBE'                                                1430
         AG_EXNAME = 'SHAPE'                                              1430
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1430
         CALL AXSHAPE                                                     1430
C    *                                                                    1430
5351  CALL AGSPOPB                                                        1431
      GO TO BLOCK_SSSH, (5350 ,5360)                                      1431
C End of Local Procedure                                                  1431
C ----------------------------------------------------------------------  1432
C ------------ SOSH                                                       1432
C Begin Local Procedure SOSH                                              1433
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1433
5330  CONTINUE                                                            1433
         AG_TITLE='SOSH'                                                  1433
         AG_MODULE=MODULE                                                 1433
         AG_MARK='V00'                                                    1433
         CALL AGSPUSH                                                     1433
         AG_MODULE=MODULE                                                 1433
C    *                                                                    1433
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1434
         CALL AGSRESET                                                    1434
         AG_TITLE = 'SSDALMY'                                             1434
         AG_EXNAME = 'MATERIAL'                                           1434
         AG_PARLIST = 'NONE'                                              1434
         CALL AXMATERIAL                                                  1434
C    *                                                                    1434
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1434
         CALL AGSRESET                                                    1435
         AG_SEEN = 1                                                      1435
         AG_COLO = 3                                                      1435
         AG_TITLE = 'SOSH'                                                1435
         AG_EXNAME = 'ATTRIBUTE'                                          1435
         AG_PARLIST = 'SEEN_COLO'                                         1435
         CALL AXATTRIBUTE                                                 1435
C    *                                                                    1435
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1435
C    SLD_SOUTLEN <===                                                     1435
         CALL AGSRESET                                                    1437
         AG_RMIN = SSLD_SOUTRINN                                          1437
         AG_RMAX = SSLD_SOUTROUT                                          1437
         AG_DZ = SSLD_SOUTLEN                                             1437
         AG_TITLE = 'TUBE'                                                1437
         AG_EXNAME = 'SHAPE'                                              1437
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1437
         CALL AXSHAPE                                                     1437
C    *                                                                    1437
5331  CALL AGSPOPB                                                        1438
      GO TO BLOCK_SOSH, (5330 ,5340)                                      1438
C End of Local Procedure                                                  1438
      WRITE(LOUT,6850)'===>>  [End AgML Module SVTTGEO10]  <<==='         1438
6850  FORMAT(A32,/,/)                                                     1438
C MODULE SVTTGEO10                                                        1440
      END                                                                 1440
