C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO2                                                 1
C SVTTGEO2 IS THE SVT GEOMETRY FOR STAR: CORRECTED AND AUGMENTED          1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SVTTGEO2'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE SVT GEOMETRY FOR STAR: CORRECTED AND AUGMENT  1
     *ED'/                                                                1
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
C CREATED 21 AUG 03                                                       2
      CHARACTER*40 CREATED /'21 AUG 03'/                                  3
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
      INTEGER BLOCK_SLYD                                                  19
      INTEGER BLOCK_SLSD                                                  19
      INTEGER BLOCK_SLDI                                                  19
      INTEGER BLOCK_STLI                                                  19
      INTEGER BLOCK_STSI                                                  19
      INTEGER BLOCK_SVTD                                                  19
      INTEGER BLOCK_SBER                                                  19
      INTEGER BLOCK_STAB                                                  19
      INTEGER BLOCK_STRU                                                  19
      INTEGER BLOCK_SPCB                                                  19
      INTEGER BLOCK_SRHC                                                  19
      INTEGER BLOCK_SBWC                                                  19
      INTEGER BLOCK_SWCM                                                  19
      INTEGER BLOCK_SXAI                                                  19
      INTEGER BLOCK_SXBI                                                  19
      INTEGER BLOCK_SELE                                                  19
      INTEGER BLOCK_SWCH                                                  19
      INTEGER BLOCK_SWCW                                                  19
      INTEGER BLOCK_SWCS                                                  19
      INTEGER BLOCK_SBOI                                                  19
      INTEGER BLOCK_SAGP                                                  19
      INTEGER BLOCK_SDYE                                                  19
      INTEGER BLOCK_SECA                                                  19
      INTEGER BLOCK_SIRP                                                  19
      INTEGER BLOCK_SIRT                                                  19
      INTEGER BLOCK_SOER                                                  19
      INTEGER BLOCK_SCON                                                  19
      INTEGER BLOCK_SROD                                                  19
      INTEGER BLOCK_SGRA                                                  19
      INTEGER BLOCK_STAP                                                  19
      INTEGER BLOCK_STAC                                                  19
      INTEGER BLOCK_SHLA                                                  19
      INTEGER BLOCK_SHLB                                                  19
      INTEGER BLOCK_SHMA                                                  19
      INTEGER BLOCK_SHMB                                                  19
      INTEGER BLOCK_SWHO                                                  19
      INTEGER BLOCK_SHWA                                                  19
      INTEGER BLOCK_SCMY                                                  19
      INTEGER BLOCK_SCAL                                                  19
      INTEGER BLOCK_SWMM                                                  19
      INTEGER BLOCK_SWMB                                                  19
      INTEGER BLOCK_SWRP                                                  19
      INTEGER BLOCK_SXRL                                                  19
      INTEGER BLOCK_SYRU                                                  19
      INTEGER BLOCK_SWMT                                                  19
      INTEGER BLOCK_SWMS                                                  19
      INTEGER BLOCK_SWMW                                                  19
      INTEGER BLOCK_SOTB                                                  19
      INTEGER BLOCK_SITB                                                  19
      INTEGER BLOCK_SBRG                                                  19
      INTEGER BLOCK_SBRM                                                  19
      INTEGER BLOCK_SBRI                                                  19
      INTEGER BLOCK_SOES                                                  19
      INTEGER BLOCK_SIES                                                  19
      INTEGER BLOCK_SOSM                                                  19
      INTEGER BLOCK_SISM                                                  19
      INTEGER BLOCK_SCRW                                                  19
      INTEGER BLOCK_SGLA                                                  19
      INTEGER BLOCK_SFMO                                                  19
      INTEGER BLOCK_SFLM                                                  19
      INTEGER BLOCK_SFDM                                                  19
      INTEGER BLOCK_SFSW                                                  19
      INTEGER BLOCK_SFSD                                                  19
      INTEGER BLOCK_SFSM                                                  19
      INTEGER BLOCK_SFSS                                                  19
      INTEGER BLOCK_SFCP                                                  19
      INTEGER BLOCK_SFCF                                                  19
      INTEGER BLOCK_SFCT                                                  19
      INTEGER BLOCK_SFCX                                                  19
      INTEGER BLOCK_SFCW                                                  19
      INTEGER BLOCK_SBSP                                                  19
      INTEGER BLOCK_SAKM                                                  19
      INTEGER BLOCK_SCKM                                                  19
      INTEGER BLOCK_SBSR                                                  19
      INTEGER BLOCK_SBCR                                                  19
      INTEGER BLOCK_SBRX                                                  19
      INTEGER BLOCK_SBRL                                                  19
      INTEGER BLOCK_SBMM                                                  19
      INTEGER BLOCK_SBMO                                                  19
      INTEGER BLOCK_SBMI                                                  19
      INTEGER BLOCK_SMRD                                                  19
      INTEGER BLOCK_SALM                                                  19
      INTEGER BLOCK_SISH                                                  19
      INTEGER BLOCK_SSSH                                                  19
      INTEGER BLOCK_SOSH                                                  19
      INTEGER BLOCK_SCBM                                                  19
      INTEGER BLOCK_SCBL                                                  19
      INTEGER BLOCK_SFED                                                  19
      INTEGER BLOCK_SPLS                                                  19
      INTEGER BLOCK_SOUM                                                  19
      INTEGER BLOCK_SOUR                                                  19
C -                                                                       20
      REAL SVTG_VERSION                                                   20
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            20
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       20
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          20
     *MSVTG_VERSION)                                                      20
      CHARACTER CSVTG_VERSION*40/' '/                                     20
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             20
      INTEGER MSVTG_VERSION(2)/1,1/                                       20
      REAL RE_SVTG_VERSION                                                20
C -                                                                       20
      INTEGER SVTG_NLAYER                                                 20
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              20
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        20
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           20
     *MSVTG_NLAYER)                                                       20
      CHARACTER CSVTG_NLAYER*40/' '/                                      20
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               20
      INTEGER MSVTG_NLAYER(2)/1,1/                                        20
      REAL RE_SVTG_NLAYER                                                 20
C -                                                                       20
      REAL SVTG_RSIZEMIN                                                  20
      EQUIVALENCE (BANK_SVTG(5),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          20
     *(NAMES_SVTG(3),NSVTG_RSIZEMIN)                                      20
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_RSIZEMIN),(MAP_SVTG(1,3),         20
     *MSVTG_RSIZEMIN)                                                     20
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    20
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           20
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      20
      REAL RE_SVTG_RSIZEMIN                                               20
C -                                                                       20
      REAL SVTG_RSIZEMAX                                                  20
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          20
     *(NAMES_SVTG(4),NSVTG_RSIZEMAX)                                      20
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMAX),(MAP_SVTG(1,4),         20
     *MSVTG_RSIZEMAX)                                                     20
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    20
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           20
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      20
      REAL RE_SVTG_RSIZEMAX                                               20
C -                                                                       20
      REAL SVTG_ZSIZEMAX                                                  20
      EQUIVALENCE (BANK_SVTG(7),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          20
     *(NAMES_SVTG(5),NSVTG_ZSIZEMAX)                                      20
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_ZSIZEMAX),(MAP_SVTG(1,5),         20
     *MSVTG_ZSIZEMAX)                                                     20
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    20
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           20
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      20
      REAL RE_SVTG_ZSIZEMAX                                               20
C -                                                                       20
      REAL SVTG_ANGOFF                                                    20
      EQUIVALENCE (BANK_SVTG(8),SVTG_ANGOFF,RE_SVTG_ANGOFF),              20
     *(NAMES_SVTG(6),NSVTG_ANGOFF)                                        20
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ANGOFF),(MAP_SVTG(1,6),           20
     *MSVTG_ANGOFF)                                                       20
      CHARACTER CSVTG_ANGOFF*40/' '/                                      20
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               20
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        20
      REAL RE_SVTG_ANGOFF                                                 20
C -                                                                       20
      REAL SVTG_SUPPORTVER                                                20
      EQUIVALENCE (BANK_SVTG(9),SVTG_SUPPORTVER,RE_SVTG_SUPPORTVER),      20
     *(NAMES_SVTG(7),NSVTG_SUPPORTVER)                                    20
      EQUIVALENCE (COMMEN_SVTG(7),CSVTG_SUPPORTVER),(MAP_SVTG(1,7),       20
     *MSVTG_SUPPORTVER)                                                   20
      CHARACTER CSVTG_SUPPORTVER*40/' '/                                  20
      CHARACTER NSVTG_SUPPORTVER*12/'R.SUPPORTVER'/                       20
      INTEGER MSVTG_SUPPORTVER(2)/1,1/                                    20
      REAL RE_SVTG_SUPPORTVER                                             20
C -                                                                       20
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                20
      PARAMETER (LEN_SVTG=9)                                              20
      PARAMETER (LLEN_SVTG=7)                                             20
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           20
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        20
      DATA IDNUM_SVTG/10*0/                                               20
      REAL BANK_SVTG(LEN_SVTG)                                            20
      CHARACTER*60 BPATH_SVTG/' '/                                        20
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          20
     *BPATH_SVTG                                                          20
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             20
C *                                                                       20
C -                                                                       22
      REAL SWCA_VERSION                                                   22
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            22
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       22
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          22
     *MSWCA_VERSION)                                                      22
      CHARACTER CSWCA_VERSION*40/' '/                                     22
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             22
      INTEGER MSWCA_VERSION(2)/1,1/                                       22
      REAL RE_SWCA_VERSION                                                22
C -                                                                       22
      REAL SWCA_LENGTH                                                    22
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              22
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        22
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           22
     *MSWCA_LENGTH)                                                       22
      CHARACTER CSWCA_LENGTH*40/' '/                                      22
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               22
      INTEGER MSWCA_LENGTH(2)/1,1/                                        22
      REAL RE_SWCA_LENGTH                                                 22
C -                                                                       22
      REAL SWCA_WAFERWID                                                  22
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          22
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      22
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         22
     *MSWCA_WAFERWID)                                                     22
      CHARACTER CSWCA_WAFERWID*40/' '/                                    22
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           22
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      22
      REAL RE_SWCA_WAFERWID                                               22
C -                                                                       22
      REAL SWCA_WAFERLEN                                                  22
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          22
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      22
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         22
     *MSWCA_WAFERLEN)                                                     22
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    22
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           22
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      22
      REAL RE_SWCA_WAFERLEN                                               22
C -                                                                       22
      REAL SWCA_WAFERTHK                                                  22
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          22
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      22
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         22
     *MSWCA_WAFERTHK)                                                     22
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    22
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           22
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      22
      REAL RE_SWCA_WAFERTHK                                               22
C -                                                                       22
      REAL SWCA_ROHATHK                                                   22
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            22
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       22
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          22
     *MSWCA_ROHATHK)                                                      22
      CHARACTER CSWCA_ROHATHK*40/' '/                                     22
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             22
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       22
      REAL RE_SWCA_ROHATHK                                                22
C -                                                                       22
      REAL SWCA_WAFCARWD                                                  22
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          22
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      22
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         22
     *MSWCA_WAFCARWD)                                                     22
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    22
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           22
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      22
      REAL RE_SWCA_WAFCARWD                                               22
C -                                                                       22
      REAL SWCA_WAFCARTH                                                  22
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         22
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      22
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         22
     *MSWCA_WAFCARTH)                                                     22
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    22
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           22
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      22
      REAL RE_SWCA_WAFCARTH                                               22
C -                                                                       22
      REAL SWCA_WAFERGAP                                                  22
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         22
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      22
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         22
     *MSWCA_WAFERGAP)                                                     22
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    22
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           22
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      22
      REAL RE_SWCA_WAFERGAP                                               22
C -                                                                       22
      REAL SWCA_DRIFT                                                     22
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               22
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        22
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          22
     *MSWCA_DRIFT)                                                        22
      CHARACTER CSWCA_DRIFT*40/' '/                                       22
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 22
      INTEGER MSWCA_DRIFT(2)/1,1/                                         22
      REAL RE_SWCA_DRIFT                                                  22
C -                                                                       22
      REAL SWCA_STRUTLEN                                                  22
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         22
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     22
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       22
     *MSWCA_STRUTLEN)                                                     22
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    22
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           22
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      22
      REAL RE_SWCA_STRUTLEN                                               22
C -                                                                       22
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                22
      PARAMETER (LEN_SWCA=13)                                             22
      PARAMETER (LLEN_SWCA=11)                                            22
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           22
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        22
      DATA IDNUM_SWCA/10*0/                                               22
      REAL BANK_SWCA(LEN_SWCA)                                            22
      CHARACTER*60 BPATH_SWCA/' '/                                        22
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          22
     *BPATH_SWCA                                                          22
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             22
C *                                                                       22
C -                                                                       25
      REAL SSUP_VERSION                                                   25
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            25
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       25
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          25
     *MSSUP_VERSION)                                                      25
      CHARACTER CSSUP_VERSION*40/' '/                                     25
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             25
      INTEGER MSSUP_VERSION(2)/1,1/                                       25
      REAL RE_SSUP_VERSION                                                25
C -                                                                       25
      REAL SSUP_CABTHK                                                    25
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              25
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        25
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           25
     *MSSUP_CABTHK)                                                       25
      CHARACTER CSSUP_CABTHK*40/' '/                                      25
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               25
      INTEGER MSSUP_CABTHK(2)/1,1/                                        25
      REAL RE_SSUP_CABTHK                                                 25
C -                                                                       25
      REAL SSUP_HOSRMN                                                    25
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              25
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        25
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           25
     *MSSUP_HOSRMN)                                                       25
      CHARACTER CSSUP_HOSRMN*40/' '/                                      25
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               25
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        25
      REAL RE_SSUP_HOSRMN                                                 25
C -                                                                       25
      REAL SSUP_HOSRMX                                                    25
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              25
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        25
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           25
     *MSSUP_HOSRMX)                                                       25
      CHARACTER CSSUP_HOSRMX*40/' '/                                      25
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               25
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        25
      REAL RE_SSUP_HOSRMX                                                 25
C -                                                                       25
      REAL SSUP_NHOSES                                                    25
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              25
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        25
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           25
     *MSSUP_NHOSES)                                                       25
      CHARACTER CSSUP_NHOSES*40/' '/                                      25
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               25
      INTEGER MSSUP_NHOSES(2)/1,1/                                        25
      REAL RE_SSUP_NHOSES                                                 25
C -                                                                       25
      REAL SSUP_WRPMYTHK                                                  25
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          25
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      25
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         25
     *MSSUP_WRPMYTHK)                                                     25
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    25
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           25
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      25
      REAL RE_SSUP_WRPMYTHK                                               25
C -                                                                       25
      REAL SSUP_WRPALTHK                                                  25
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          25
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      25
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         25
     *MSSUP_WRPALTHK)                                                     25
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    25
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           25
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      25
      REAL RE_SSUP_WRPALTHK                                               25
C -                                                                       25
      REAL SSUP_GRPHTHK                                                   25
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           25
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       25
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          25
     *MSSUP_GRPHTHK)                                                      25
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     25
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             25
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       25
      REAL RE_SSUP_GRPHTHK                                                25
C -                                                                       25
      REAL SSUP_CONE1ZMN                                                  25
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         25
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      25
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         25
     *MSSUP_CONE1ZMN)                                                     25
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    25
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           25
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      25
      REAL RE_SSUP_CONE1ZMN                                               25
C -                                                                       25
      REAL SSUP_RODLEN                                                    25
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             25
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       25
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         25
     *MSSUP_RODLEN)                                                       25
      CHARACTER CSSUP_RODLEN*40/' '/                                      25
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               25
      INTEGER MSSUP_RODLEN(2)/1,1/                                        25
      REAL RE_SSUP_RODLEN                                                 25
C -                                                                       25
      REAL SSUP_RODDIST                                                   25
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           25
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      25
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        25
     *MSSUP_RODDIST)                                                      25
      CHARACTER CSSUP_RODDIST*40/' '/                                     25
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             25
      INTEGER MSSUP_RODDIST(2)/1,1/                                       25
      REAL RE_SSUP_RODDIST                                                25
C -                                                                       25
      REAL SSUP_RODID                                                     25
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               25
     *(NAMES_SSUP(12),NSSUP_RODID)                                        25
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          25
     *MSSUP_RODID)                                                        25
      CHARACTER CSSUP_RODID*40/' '/                                       25
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 25
      INTEGER MSSUP_RODID(2)/1,1/                                         25
      REAL RE_SSUP_RODID                                                  25
C -                                                                       25
      REAL SSUP_RODOD                                                     25
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               25
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        25
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          25
     *MSSUP_RODOD)                                                        25
      CHARACTER CSSUP_RODOD*40/' '/                                       25
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 25
      INTEGER MSSUP_RODOD(2)/1,1/                                         25
      REAL RE_SSUP_RODOD                                                  25
C -                                                                       25
      REAL SSUP_CON1IDMN                                                  25
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         25
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     25
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       25
     *MSSUP_CON1IDMN)                                                     25
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    25
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           25
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      25
      REAL RE_SSUP_CON1IDMN                                               25
C -                                                                       25
      REAL SSUP_CON3IDMN                                                  25
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         25
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     25
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       25
     *MSSUP_CON3IDMN)                                                     25
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    25
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           25
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      25
      REAL RE_SSUP_CON3IDMN                                               25
C -                                                                       25
      REAL SSUP_CON4IDMN                                                  25
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         25
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     25
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       25
     *MSSUP_CON4IDMN)                                                     25
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    25
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           25
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      25
      REAL RE_SSUP_CON4IDMN                                               25
C -                                                                       25
      REAL SSUP_CON4IDMX                                                  25
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         25
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     25
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       25
     *MSSUP_CON4IDMX)                                                     25
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    25
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           25
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      25
      REAL RE_SSUP_CON4IDMX                                               25
C -                                                                       25
      REAL SSUP_CONE3ZMX                                                  25
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         25
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     25
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       25
     *MSSUP_CONE3ZMX)                                                     25
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    25
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           25
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      25
      REAL RE_SSUP_CONE3ZMX                                               25
C -                                                                       25
      REAL SSUP_CONE4ZMX                                                  25
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         25
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     25
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       25
     *MSSUP_CONE4ZMX)                                                     25
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    25
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           25
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      25
      REAL RE_SSUP_CONE4ZMX                                               25
C -                                                                       25
      REAL SSUP_BRATHK                                                    25
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             25
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       25
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         25
     *MSSUP_BRATHK)                                                       25
      CHARACTER CSSUP_BRATHK*40/' '/                                      25
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               25
      INTEGER MSSUP_BRATHK(2)/1,1/                                        25
      REAL RE_SSUP_BRATHK                                                 25
C -                                                                       25
      REAL SSUP_ERJTHK                                                    25
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             25
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       25
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         25
     *MSSUP_ERJTHK)                                                       25
      CHARACTER CSSUP_ERJTHK*40/' '/                                      25
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               25
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        25
      REAL RE_SSUP_ERJTHK                                                 25
C -                                                                       25
      REAL SSUP_ERJWID                                                    25
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             25
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       25
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         25
     *MSSUP_ERJWID)                                                       25
      CHARACTER CSSUP_ERJWID*40/' '/                                      25
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               25
      INTEGER MSSUP_ERJWID(2)/1,1/                                        25
      REAL RE_SSUP_ERJWID                                                 25
C -                                                                       25
      REAL SSUP_ERJLEN                                                    25
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             25
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       25
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         25
     *MSSUP_ERJLEN)                                                       25
      CHARACTER CSSUP_ERJLEN*40/' '/                                      25
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               25
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        25
      REAL RE_SSUP_ERJLEN                                                 25
C -                                                                       25
      REAL SSUP_ERJZDIS                                                   25
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           25
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      25
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        25
     *MSSUP_ERJZDIS)                                                      25
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     25
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             25
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       25
      REAL RE_SSUP_ERJZDIS                                                25
C -                                                                       25
      REAL SSUP_ERJ1X                                                     25
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               25
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        25
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          25
     *MSSUP_ERJ1X)                                                        25
      CHARACTER CSSUP_ERJ1X*40/' '/                                       25
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 25
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         25
      REAL RE_SSUP_ERJ1X                                                  25
C -                                                                       25
      REAL SSUP_ERJ2X                                                     25
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               25
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        25
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          25
     *MSSUP_ERJ2X)                                                        25
      CHARACTER CSSUP_ERJ2X*40/' '/                                       25
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 25
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         25
      REAL RE_SSUP_ERJ2X                                                  25
C -                                                                       25
      REAL SSUP_ERJ2Y                                                     25
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               25
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        25
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          25
     *MSSUP_ERJ2Y)                                                        25
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       25
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 25
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         25
      REAL RE_SSUP_ERJ2Y                                                  25
C -                                                                       25
      REAL SSUP_ERJRAD                                                    25
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             25
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       25
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         25
     *MSSUP_ERJRAD)                                                       25
      CHARACTER CSSUP_ERJRAD*40/' '/                                      25
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               25
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        25
      REAL RE_SSUP_ERJRAD                                                 25
C -                                                                       25
      REAL SSUP_ERJDIA                                                    25
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             25
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       25
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         25
     *MSSUP_ERJDIA)                                                       25
      CHARACTER CSSUP_ERJDIA*40/' '/                                      25
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               25
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        25
      REAL RE_SSUP_ERJDIA                                                 25
C -                                                                       25
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                25
      PARAMETER (LEN_SSUP=31)                                             25
      PARAMETER (LLEN_SSUP=29)                                            25
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           25
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        25
      DATA IDNUM_SSUP/10*0/                                               25
      REAL BANK_SSUP(LEN_SSUP)                                            25
      CHARACTER*60 BPATH_SSUP/' '/                                        25
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          25
     *BPATH_SSUP                                                          25
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             25
C *                                                                       25
C -                                                                       29
      REAL SSUB_VERSION                                                   29
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            29
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       29
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          29
     *MSSUB_VERSION)                                                      29
      CHARACTER CSSUB_VERSION*40/' '/                                     29
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             29
      INTEGER MSSUB_VERSION(2)/1,1/                                       29
      REAL RE_SSUB_VERSION                                                29
C -                                                                       29
      REAL SSUB_KMOUNTID                                                  29
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          29
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      29
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         29
     *MSSUB_KMOUNTID)                                                     29
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    29
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           29
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      29
      REAL RE_SSUB_KMOUNTID                                               29
C -                                                                       29
      REAL SSUB_KMOUNTOD                                                  29
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          29
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      29
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         29
     *MSSUB_KMOUNTOD)                                                     29
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    29
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           29
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      29
      REAL RE_SSUB_KMOUNTOD                                               29
C -                                                                       29
      REAL SSUB_KMNTTHK                                                   29
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            29
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       29
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          29
     *MSSUB_KMNTTHK)                                                      29
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     29
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             29
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       29
      REAL RE_SSUB_KMNTTHK                                                29
C -                                                                       29
      REAL SSUB_KMCUTOD                                                   29
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            29
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       29
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          29
     *MSSUB_KMCUTOD)                                                      29
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     29
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             29
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       29
      REAL RE_SSUB_KMCUTOD                                                29
C -                                                                       29
      REAL SSUB_KMCUTID                                                   29
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            29
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       29
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          29
     *MSSUB_KMCUTID)                                                      29
      CHARACTER CSSUB_KMCUTID*40/' '/                                     29
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             29
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       29
      REAL RE_SSUB_KMCUTID                                                29
C -                                                                       29
      REAL SSUB_KMCUTOA                                                   29
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            29
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       29
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          29
     *MSSUB_KMCUTOA)                                                      29
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     29
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             29
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       29
      REAL RE_SSUB_KMCUTOA                                                29
C -                                                                       29
      REAL SSUB_KMCUTOFF                                                  29
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         29
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      29
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         29
     *MSSUB_KMCUTOFF)                                                     29
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    29
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           29
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      29
      REAL RE_SSUB_KMCUTOFF                                               29
C -                                                                       29
      REAL SSUB_SRINGID                                                   29
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           29
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       29
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          29
     *MSSUB_SRINGID)                                                      29
      CHARACTER CSSUB_SRINGID*40/' '/                                     29
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             29
      INTEGER MSSUB_SRINGID(2)/1,1/                                       29
      REAL RE_SSUB_SRINGID                                                29
C -                                                                       29
      REAL SSUB_SRINGOD                                                   29
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           29
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      29
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        29
     *MSSUB_SRINGOD)                                                      29
      CHARACTER CSSUB_SRINGOD*40/' '/                                     29
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             29
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       29
      REAL RE_SSUB_SRINGOD                                                29
C -                                                                       29
      REAL SSUB_SRINGTHK                                                  29
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         29
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     29
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       29
     *MSSUB_SRINGTHK)                                                     29
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    29
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           29
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      29
      REAL RE_SSUB_SRINGTHK                                               29
C -                                                                       29
      REAL SSUB_SRCUTPHI                                                  29
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         29
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     29
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       29
     *MSSUB_SRCUTPHI)                                                     29
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    29
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           29
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      29
      REAL RE_SSUB_SRCUTPHI                                               29
C -                                                                       29
      REAL SSUB_SRCUTWID                                                  29
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         29
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     29
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       29
     *MSSUB_SRCUTWID)                                                     29
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    29
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           29
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      29
      REAL RE_SSUB_SRCUTWID                                               29
C -                                                                       29
      REAL SSUB_SRCUTOUT                                                  29
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         29
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     29
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       29
     *MSSUB_SRCUTOUT)                                                     29
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    29
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           29
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      29
      REAL RE_SSUB_SRCUTOUT                                               29
C -                                                                       29
      REAL SSUB_SRCUTIN                                                   29
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           29
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      29
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        29
     *MSSUB_SRCUTIN)                                                      29
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     29
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             29
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       29
      REAL RE_SSUB_SRCUTIN                                                29
C -                                                                       29
      REAL SSUB_SROLLID                                                   29
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           29
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      29
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        29
     *MSSUB_SROLLID)                                                      29
      CHARACTER CSSUB_SROLLID*40/' '/                                     29
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             29
      INTEGER MSSUB_SROLLID(2)/1,1/                                       29
      REAL RE_SSUB_SROLLID                                                29
C -                                                                       29
      REAL SSUB_SROLLOD                                                   29
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           29
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      29
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        29
     *MSSUB_SROLLOD)                                                      29
      CHARACTER CSSUB_SROLLOD*40/' '/                                     29
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             29
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       29
      REAL RE_SSUB_SROLLOD                                                29
C -                                                                       29
      REAL SSUB_SROLLLEN                                                  29
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         29
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     29
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       29
     *MSSUB_SROLLLEN)                                                     29
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    29
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           29
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      29
      REAL RE_SSUB_SROLLLEN                                               29
C -                                                                       29
      REAL SSUB_SWIRELEN                                                  29
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         29
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     29
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       29
     *MSSUB_SWIRELEN)                                                     29
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    29
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           29
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      29
      REAL RE_SSUB_SWIRELEN                                               29
C -                                                                       29
      REAL SSUB_MBLKHGH                                                   29
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           29
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      29
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        29
     *MSSUB_MBLKHGH)                                                      29
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     29
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             29
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       29
      REAL RE_SSUB_MBLKHGH                                                29
C -                                                                       29
      REAL SSUB_MBLKOWID                                                  29
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         29
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     29
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       29
     *MSSUB_MBLKOWID)                                                     29
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    29
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           29
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      29
      REAL RE_SSUB_MBLKOWID                                               29
C -                                                                       29
      REAL SSUB_MBLKOLEN                                                  29
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         29
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     29
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       29
     *MSSUB_MBLKOLEN)                                                     29
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    29
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           29
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      29
      REAL RE_SSUB_MBLKOLEN                                               29
C -                                                                       29
      REAL SSUB_MBLKIWID                                                  29
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         29
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     29
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       29
     *MSSUB_MBLKIWID)                                                     29
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    29
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           29
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      29
      REAL RE_SSUB_MBLKIWID                                               29
C -                                                                       29
      REAL SSUB_MBLKILEN                                                  29
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         29
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     29
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       29
     *MSSUB_MBLKILEN)                                                     29
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    29
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           29
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      29
      REAL RE_SSUB_MBLKILEN                                               29
C -                                                                       29
      REAL SSUB_MBLKORAD                                                  29
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         29
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     29
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       29
     *MSSUB_MBLKORAD)                                                     29
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    29
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           29
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      29
      REAL RE_SSUB_MBLKORAD                                               29
C -                                                                       29
      REAL SSUB_MBLKIRAD                                                  29
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         29
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     29
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       29
     *MSSUB_MBLKIRAD)                                                     29
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    29
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           29
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      29
      REAL RE_SSUB_MBLKIRAD                                               29
C -                                                                       29
      REAL SSUB_MRODDIA                                                   29
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           29
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      29
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        29
     *MSSUB_MRODDIA)                                                      29
      CHARACTER CSSUB_MRODDIA*40/' '/                                     29
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             29
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       29
      REAL RE_SSUB_MRODDIA                                                29
C -                                                                       29
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                29
      PARAMETER (LEN_SSUB=29)                                             29
      PARAMETER (LLEN_SSUB=27)                                            29
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           29
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        29
      DATA IDNUM_SSUB/10*0/                                               29
      REAL BANK_SSUB(LEN_SSUB)                                            29
      CHARACTER*60 BPATH_SSUB/' '/                                        29
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          29
     *BPATH_SSUB                                                          29
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             29
C *                                                                       29
C -                                                                       30
      REAL SWAM_VERSION                                                   30
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            30
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       30
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          30
     *MSWAM_VERSION)                                                      30
      CHARACTER CSWAM_VERSION*40/' '/                                     30
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             30
      INTEGER MSWAM_VERSION(2)/1,1/                                       30
      REAL RE_SWAM_VERSION                                                30
C -                                                                       30
      REAL SWAM_ZMIN                                                      30
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   30
     *NSWAM_ZMIN)                                                         30
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             30
     *MSWAM_ZMIN)                                                         30
      CHARACTER CSWAM_ZMIN*40/' '/                                        30
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   30
      INTEGER MSWAM_ZMIN(2)/1,1/                                          30
      REAL RE_SWAM_ZMIN                                                   30
C -                                                                       30
      REAL SWAM_LEN                                                       30
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     30
     *NSWAM_LEN)                                                          30
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    30
      CHARACTER CSWAM_LEN*40/' '/                                         30
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     30
      INTEGER MSWAM_LEN(2)/1,1/                                           30
      REAL RE_SWAM_LEN                                                    30
C -                                                                       30
      REAL SWAM_RMIN                                                      30
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   30
     *NSWAM_RMIN)                                                         30
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             30
     *MSWAM_RMIN)                                                         30
      CHARACTER CSWAM_RMIN*40/' '/                                        30
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   30
      INTEGER MSWAM_RMIN(2)/1,1/                                          30
      REAL RE_SWAM_RMIN                                                   30
C -                                                                       30
      REAL SWAM_RMAX                                                      30
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   30
     *NSWAM_RMAX)                                                         30
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             30
     *MSWAM_RMAX)                                                         30
      CHARACTER CSWAM_RMAX*40/' '/                                        30
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   30
      INTEGER MSWAM_RMAX(2)/1,1/                                          30
      REAL RE_SWAM_RMAX                                                   30
C -                                                                       30
      REAL SWAM_TBRDTHK                                                   30
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            30
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       30
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          30
     *MSWAM_TBRDTHK)                                                      30
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     30
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             30
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       30
      REAL RE_SWAM_TBRDTHK                                                30
C -                                                                       30
      REAL SWAM_WALLTHK                                                   30
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            30
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       30
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          30
     *MSWAM_WALLTHK)                                                      30
      CHARACTER CSWAM_WALLTHK*40/' '/                                     30
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             30
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       30
      REAL RE_SWAM_WALLTHK                                                30
C -                                                                       30
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                30
      PARAMETER (LEN_SWAM=9)                                              30
      PARAMETER (LLEN_SWAM=7)                                             30
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           30
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        30
      DATA IDNUM_SWAM/10*0/                                               30
      REAL BANK_SWAM(LEN_SWAM)                                            30
      CHARACTER*60 BPATH_SWAM/' '/                                        30
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          30
     *BPATH_SWAM                                                          30
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             30
C *                                                                       30
C -                                                                       31
      REAL SERG_VERSION                                                   31
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            31
     *(NAMES_SERG(1),NSERG_VERSION)                                       31
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          31
     *MSERG_VERSION)                                                      31
      CHARACTER CSERG_VERSION*40/' '/                                     31
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             31
      INTEGER MSERG_VERSION(2)/1,1/                                       31
      REAL RE_SERG_VERSION                                                31
C -                                                                       31
      REAL SERG_IRNGTRMX                                                  31
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          31
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      31
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         31
     *MSERG_IRNGTRMX)                                                     31
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    31
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           31
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      31
      REAL RE_SERG_IRNGTRMX                                               31
C -                                                                       31
      REAL SERG_IRNGPRMN                                                  31
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          31
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      31
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         31
     *MSERG_IRNGPRMN)                                                     31
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    31
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           31
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      31
      REAL RE_SERG_IRNGPRMN                                               31
C -                                                                       31
      REAL SERG_ORNGRMIN                                                  31
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          31
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      31
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         31
     *MSERG_ORNGRMIN)                                                     31
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    31
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           31
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      31
      REAL RE_SERG_ORNGRMIN                                               31
C -                                                                       31
      REAL SERG_ORNGRMAX                                                  31
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          31
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      31
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         31
     *MSERG_ORNGRMAX)                                                     31
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    31
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           31
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      31
      REAL RE_SERG_ORNGRMAX                                               31
C -                                                                       31
      REAL SERG_ENDRNGTH                                                  31
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          31
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      31
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         31
     *MSERG_ENDRNGTH)                                                     31
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    31
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           31
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      31
      REAL RE_SERG_ENDRNGTH                                               31
C -                                                                       31
      REAL SERG_ENDRNGZM                                                  31
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          31
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      31
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         31
     *MSERG_ENDRNGZM)                                                     31
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    31
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           31
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      31
      REAL RE_SERG_ENDRNGZM                                               31
C -                                                                       31
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                31
      PARAMETER (LEN_SERG=9)                                              31
      PARAMETER (LLEN_SERG=7)                                             31
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           31
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        31
      DATA IDNUM_SERG/10*0/                                               31
      REAL BANK_SERG(LEN_SERG)                                            31
      CHARACTER*60 BPATH_SERG/' '/                                        31
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          31
     *BPATH_SERG                                                          31
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             31
C *                                                                       31
C -                                                                       33
      REAL SELC_VERSION                                                   33
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            33
     *(NAMES_SELC(1),NSELC_VERSION)                                       33
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          33
     *MSELC_VERSION)                                                      33
      CHARACTER CSELC_VERSION*40/' '/                                     33
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             33
      INTEGER MSELC_VERSION(2)/1,1/                                       33
      REAL RE_SELC_VERSION                                                33
C -                                                                       33
      REAL SELC_BETHK                                                     33
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                33
     *(NAMES_SELC(2),NSELC_BETHK)                                         33
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            33
     *MSELC_BETHK)                                                        33
      CHARACTER CSELC_BETHK*40/' '/                                       33
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 33
      INTEGER MSELC_BETHK(2)/1,1/                                         33
      REAL RE_SELC_BETHK                                                  33
C -                                                                       33
      REAL SELC_WATTHK                                                    33
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              33
     *(NAMES_SELC(3),NSELC_WATTHK)                                        33
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           33
     *MSELC_WATTHK)                                                       33
      CHARACTER CSELC_WATTHK*40/' '/                                      33
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               33
      INTEGER MSELC_WATTHK(2)/1,1/                                        33
      REAL RE_SELC_WATTHK                                                 33
C -                                                                       33
      REAL SELC_BEOTHK                                                    33
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              33
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        33
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           33
     *MSELC_BEOTHK)                                                       33
      CHARACTER CSELC_BEOTHK*40/' '/                                      33
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               33
      INTEGER MSELC_BEOTHK(2)/1,1/                                        33
      REAL RE_SELC_BEOTHK                                                 33
C -                                                                       33
      REAL SELC_DYETHK                                                    33
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              33
     *(NAMES_SELC(5),NSELC_DYETHK)                                        33
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           33
     *MSELC_DYETHK)                                                       33
      CHARACTER CSELC_DYETHK*40/' '/                                      33
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               33
      INTEGER MSELC_DYETHK(2)/1,1/                                        33
      REAL RE_SELC_DYETHK                                                 33
C -                                                                       33
      REAL SELC_DYEWID                                                    33
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              33
     *(NAMES_SELC(6),NSELC_DYEWID)                                        33
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           33
     *MSELC_DYEWID)                                                       33
      CHARACTER CSELC_DYEWID*40/' '/                                      33
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               33
      INTEGER MSELC_DYEWID(2)/1,1/                                        33
      REAL RE_SELC_DYEWID                                                 33
C -                                                                       33
      REAL SELC_DYESPC                                                    33
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              33
     *(NAMES_SELC(7),NSELC_DYESPC)                                        33
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           33
     *MSELC_DYESPC)                                                       33
      CHARACTER CSELC_DYESPC*40/' '/                                      33
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               33
      INTEGER MSELC_DYESPC(2)/1,1/                                        33
      REAL RE_SELC_DYESPC                                                 33
C -                                                                       33
      REAL SELC_ELCAWID                                                   33
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           33
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       33
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          33
     *MSELC_ELCAWID)                                                      33
      CHARACTER CSELC_ELCAWID*40/' '/                                     33
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             33
      INTEGER MSELC_ELCAWID(2)/1,1/                                       33
      REAL RE_SELC_ELCAWID                                                33
C -                                                                       33
      REAL SELC_AGPDTHK                                                   33
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           33
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       33
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          33
     *MSELC_AGPDTHK)                                                      33
      CHARACTER CSELC_AGPDTHK*40/' '/                                     33
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             33
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       33
      REAL RE_SELC_AGPDTHK                                                33
C -                                                                       33
      REAL SELC_GLASSTHK                                                  33
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         33
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     33
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       33
     *MSELC_GLASSTHK)                                                     33
      CHARACTER CSELC_GLASSTHK*40/' '/                                    33
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           33
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      33
      REAL RE_SELC_GLASSTHK                                               33
C -                                                                       33
      REAL SELC_CABTHK                                                    33
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             33
     *(NAMES_SELC(11),NSELC_CABTHK)                                       33
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         33
     *MSELC_CABTHK)                                                       33
      CHARACTER CSELC_CABTHK*40/' '/                                      33
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               33
      INTEGER MSELC_CABTHK(2)/1,1/                                        33
      REAL RE_SELC_CABTHK                                                 33
C -                                                                       33
      REAL SELC_CABWID                                                    33
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             33
     *(NAMES_SELC(12),NSELC_CABWID)                                       33
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         33
     *MSELC_CABWID)                                                       33
      CHARACTER CSELC_CABWID*40/' '/                                      33
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               33
      INTEGER MSELC_CABWID(2)/1,1/                                        33
      REAL RE_SELC_CABWID                                                 33
C -                                                                       33
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                33
      PARAMETER (LEN_SELC=14)                                             33
      PARAMETER (LLEN_SELC=12)                                            33
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           33
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        33
      DATA IDNUM_SELC/10*0/                                               33
      REAL BANK_SELC(LEN_SELC)                                            33
      CHARACTER*60 BPATH_SELC/' '/                                        33
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          33
     *BPATH_SELC                                                          33
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             33
C *                                                                       33
C -                                                                       35
      INTEGER SVTL_LAYER                                                  35
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                35
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         35
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            35
     *MSVTL_LAYER)                                                        35
      CHARACTER CSVTL_LAYER*40/' '/                                       35
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 35
      INTEGER MSVTL_LAYER(2)/1,1/                                         35
      REAL RE_SVTL_LAYER                                                  35
C -                                                                       35
      REAL SVTL_NLADDER                                                   35
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            35
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       35
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          35
     *MSVTL_NLADDER)                                                      35
      CHARACTER CSVTL_NLADDER*40/' '/                                     35
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             35
      INTEGER MSVTL_NLADDER(2)/1,1/                                       35
      REAL RE_SVTL_NLADDER                                                35
C -                                                                       35
      REAL SVTL_NWAFER                                                    35
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              35
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        35
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           35
     *MSVTL_NWAFER)                                                       35
      CHARACTER CSVTL_NWAFER*40/' '/                                      35
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               35
      INTEGER MSVTL_NWAFER(2)/1,1/                                        35
      REAL RE_SVTL_NWAFER                                                 35
C -                                                                       35
      REAL SVTL_RADIUS                                                    35
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              35
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        35
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           35
     *MSVTL_RADIUS)                                                       35
      CHARACTER CSVTL_RADIUS*40/' '/                                      35
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               35
      INTEGER MSVTL_RADIUS(2)/1,1/                                        35
      REAL RE_SVTL_RADIUS                                                 35
C -                                                                       35
      REAL SVTL_BAREEDGE                                                  35
      EQUIVALENCE (BANK_SVTL(7),SVTL_BAREEDGE,RE_SVTL_BAREEDGE),          35
     *(NAMES_SVTL(5),NSVTL_BAREEDGE)                                      35
      EQUIVALENCE (COMMEN_SVTL(5),CSVTL_BAREEDGE),(MAP_SVTL(1,5),         35
     *MSVTL_BAREEDGE)                                                     35
      CHARACTER CSVTL_BAREEDGE*40/' '/                                    35
      CHARACTER NSVTL_BAREEDGE*12/'R.BAREEDGE'/                           35
      INTEGER MSVTL_BAREEDGE(2)/1,1/                                      35
      REAL RE_SVTL_BAREEDGE                                               35
C -                                                                       35
      REAL SVTL_PCBLEN                                                    35
      EQUIVALENCE (BANK_SVTL(8),SVTL_PCBLEN,RE_SVTL_PCBLEN),              35
     *(NAMES_SVTL(6),NSVTL_PCBLEN)                                        35
      EQUIVALENCE (COMMEN_SVTL(6),CSVTL_PCBLEN),(MAP_SVTL(1,6),           35
     *MSVTL_PCBLEN)                                                       35
      CHARACTER CSVTL_PCBLEN*40/' '/                                      35
      CHARACTER NSVTL_PCBLEN*12/'R.PCBLEN'/                               35
      INTEGER MSVTL_PCBLEN(2)/1,1/                                        35
      REAL RE_SVTL_PCBLEN                                                 35
C -                                                                       35
      REAL SVTL_PCBWIDTH                                                  35
      EQUIVALENCE (BANK_SVTL(9),SVTL_PCBWIDTH,RE_SVTL_PCBWIDTH),          35
     *(NAMES_SVTL(7),NSVTL_PCBWIDTH)                                      35
      EQUIVALENCE (COMMEN_SVTL(7),CSVTL_PCBWIDTH),(MAP_SVTL(1,7),         35
     *MSVTL_PCBWIDTH)                                                     35
      CHARACTER CSVTL_PCBWIDTH*40/' '/                                    35
      CHARACTER NSVTL_PCBWIDTH*12/'R.PCBWIDTH'/                           35
      INTEGER MSVTL_PCBWIDTH(2)/1,1/                                      35
      REAL RE_SVTL_PCBWIDTH                                               35
C -                                                                       35
      REAL SVTL_PCBTHK                                                    35
      EQUIVALENCE (BANK_SVTL(10),SVTL_PCBTHK,RE_SVTL_PCBTHK),             35
     *(NAMES_SVTL(8),NSVTL_PCBTHK)                                        35
      EQUIVALENCE (COMMEN_SVTL(8),CSVTL_PCBTHK),(MAP_SVTL(1,8),           35
     *MSVTL_PCBTHK)                                                       35
      CHARACTER CSVTL_PCBTHK*40/' '/                                      35
      CHARACTER NSVTL_PCBTHK*12/'R.PCBTHK'/                               35
      INTEGER MSVTL_PCBTHK(2)/1,1/                                        35
      REAL RE_SVTL_PCBTHK                                                 35
C -                                                                       35
      REAL SVTL_PCBGAP                                                    35
      EQUIVALENCE (BANK_SVTL(11),SVTL_PCBGAP,RE_SVTL_PCBGAP),             35
     *(NAMES_SVTL(9),NSVTL_PCBGAP)                                        35
      EQUIVALENCE (COMMEN_SVTL(9),CSVTL_PCBGAP),(MAP_SVTL(1,9),           35
     *MSVTL_PCBGAP)                                                       35
      CHARACTER CSVTL_PCBGAP*40/' '/                                      35
      CHARACTER NSVTL_PCBGAP*12/'R.PCBGAP'/                               35
      INTEGER MSVTL_PCBGAP(2)/1,1/                                        35
      REAL RE_SVTL_PCBGAP                                                 35
C -                                                                       35
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                35
      PARAMETER (LEN_SVTL=11)                                             35
      PARAMETER (LLEN_SVTL=9)                                             35
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           35
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        35
      DATA IDNUM_SVTL/10*0/                                               35
      REAL BANK_SVTL(LEN_SVTL)                                            35
      CHARACTER*60 BPATH_SVTL/' '/                                        35
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          35
     *BPATH_SVTL                                                          35
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             35
C *                                                                       35
C -                                                                       37
      REAL SFPA_VERSION                                                   37
      EQUIVALENCE (BANK_SFPA(3),SFPA_VERSION,RE_SFPA_VERSION),            37
     *(NAMES_SFPA(1),NSFPA_VERSION)                                       37
      EQUIVALENCE (COMMEN_SFPA(1),CSFPA_VERSION),(MAP_SFPA(1,1),          37
     *MSFPA_VERSION)                                                      37
      CHARACTER CSFPA_VERSION*40/' '/                                     37
      CHARACTER NSFPA_VERSION*12/'R.VERSION'/                             37
      INTEGER MSFPA_VERSION(2)/1,1/                                       37
      REAL RE_SFPA_VERSION                                                37
C -                                                                       37
      REAL SFPA_RMIN                                                      37
      EQUIVALENCE (BANK_SFPA(4),SFPA_RMIN,RE_SFPA_RMIN),(NAMES_SFPA(2),   37
     *NSFPA_RMIN)                                                         37
      EQUIVALENCE (COMMEN_SFPA(2),CSFPA_RMIN),(MAP_SFPA(1,2),             37
     *MSFPA_RMIN)                                                         37
      CHARACTER CSFPA_RMIN*40/' '/                                        37
      CHARACTER NSFPA_RMIN*12/'R.RMIN'/                                   37
      INTEGER MSFPA_RMIN(2)/1,1/                                          37
      REAL RE_SFPA_RMIN                                                   37
C -                                                                       37
      REAL SFPA_RMAX                                                      37
      EQUIVALENCE (BANK_SFPA(5),SFPA_RMAX,RE_SFPA_RMAX),(NAMES_SFPA(3),   37
     *NSFPA_RMAX)                                                         37
      EQUIVALENCE (COMMEN_SFPA(3),CSFPA_RMAX),(MAP_SFPA(1,3),             37
     *MSFPA_RMAX)                                                         37
      CHARACTER CSFPA_RMAX*40/' '/                                        37
      CHARACTER NSFPA_RMAX*12/'R.RMAX'/                                   37
      INTEGER MSFPA_RMAX(2)/1,1/                                          37
      REAL RE_SFPA_RMAX                                                   37
C -                                                                       37
      REAL SFPA_LEN                                                       37
      EQUIVALENCE (BANK_SFPA(6),SFPA_LEN,RE_SFPA_LEN),(NAMES_SFPA(4),     37
     *NSFPA_LEN)                                                          37
      EQUIVALENCE (COMMEN_SFPA(4),CSFPA_LEN),(MAP_SFPA(1,4),MSFPA_LEN)    37
      CHARACTER CSFPA_LEN*40/' '/                                         37
      CHARACTER NSFPA_LEN*12/'R.LEN'/                                     37
      INTEGER MSFPA_LEN(2)/1,1/                                           37
      REAL RE_SFPA_LEN                                                    37
C -                                                                       37
      REAL SFPA_RAD                                                       37
      EQUIVALENCE (BANK_SFPA(7),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(5),     37
     *NSFPA_RAD)                                                          37
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_RAD),(MAP_SFPA(1,5),MSFPA_RAD)    37
      CHARACTER CSFPA_RAD*40/' '/                                         37
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     37
      INTEGER MSFPA_RAD(2)/1,1/                                           37
      REAL RE_SFPA_RAD                                                    37
C -                                                                       37
      REAL SFPA_NSSD                                                      37
      EQUIVALENCE (BANK_SFPA(8),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(6),   37
     *NSFPA_NSSD)                                                         37
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_NSSD),(MAP_SFPA(1,6),             37
     *MSFPA_NSSD)                                                         37
      CHARACTER CSFPA_NSSD*40/' '/                                        37
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   37
      INTEGER MSFPA_NSSD(2)/1,1/                                          37
      REAL RE_SFPA_NSSD                                                   37
C -                                                                       37
      REAL SFPA_DMWID                                                     37
      EQUIVALENCE (BANK_SFPA(9),SFPA_DMWID,RE_SFPA_DMWID),                37
     *(NAMES_SFPA(7),NSFPA_DMWID)                                         37
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_DMWID),(MAP_SFPA(1,7),            37
     *MSFPA_DMWID)                                                        37
      CHARACTER CSFPA_DMWID*40/' '/                                       37
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 37
      INTEGER MSFPA_DMWID(2)/1,1/                                         37
      REAL RE_SFPA_DMWID                                                  37
C -                                                                       37
      REAL SFPA_DMTHK                                                     37
      EQUIVALENCE (BANK_SFPA(10),SFPA_DMTHK,RE_SFPA_DMTHK),               37
     *(NAMES_SFPA(8),NSFPA_DMTHK)                                         37
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_DMTHK),(MAP_SFPA(1,8),            37
     *MSFPA_DMTHK)                                                        37
      CHARACTER CSFPA_DMTHK*40/' '/                                       37
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 37
      INTEGER MSFPA_DMTHK(2)/1,1/                                         37
      REAL RE_SFPA_DMTHK                                                  37
C -                                                                       37
      REAL SFPA_DMLEN                                                     37
      EQUIVALENCE (BANK_SFPA(11),SFPA_DMLEN,RE_SFPA_DMLEN),               37
     *(NAMES_SFPA(9),NSFPA_DMLEN)                                         37
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_DMLEN),(MAP_SFPA(1,9),            37
     *MSFPA_DMLEN)                                                        37
      CHARACTER CSFPA_DMLEN*40/' '/                                       37
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 37
      INTEGER MSFPA_DMLEN(2)/1,1/                                         37
      REAL RE_SFPA_DMLEN                                                  37
C -                                                                       37
      REAL SFPA_SMWID                                                     37
      EQUIVALENCE (BANK_SFPA(12),SFPA_SMWID,RE_SFPA_SMWID),               37
     *(NAMES_SFPA(10),NSFPA_SMWID)                                        37
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_SMWID),(MAP_SFPA(1,10),          37
     *MSFPA_SMWID)                                                        37
      CHARACTER CSFPA_SMWID*40/' '/                                       37
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 37
      INTEGER MSFPA_SMWID(2)/1,1/                                         37
      REAL RE_SFPA_SMWID                                                  37
C -                                                                       37
      REAL SFPA_SMTHK                                                     37
      EQUIVALENCE (BANK_SFPA(13),SFPA_SMTHK,RE_SFPA_SMTHK),               37
     *(NAMES_SFPA(11),NSFPA_SMTHK)                                        37
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_SMTHK),(MAP_SFPA(1,11),          37
     *MSFPA_SMTHK)                                                        37
      CHARACTER CSFPA_SMTHK*40/' '/                                       37
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 37
      INTEGER MSFPA_SMTHK(2)/1,1/                                         37
      REAL RE_SFPA_SMTHK                                                  37
C -                                                                       37
      REAL SFPA_SMLEN                                                     37
      EQUIVALENCE (BANK_SFPA(14),SFPA_SMLEN,RE_SFPA_SMLEN),               37
     *(NAMES_SFPA(12),NSFPA_SMLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_SMLEN),(MAP_SFPA(1,12),          37
     *MSFPA_SMLEN)                                                        37
      CHARACTER CSFPA_SMLEN*40/' '/                                       37
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 37
      INTEGER MSFPA_SMLEN(2)/1,1/                                         37
      REAL RE_SFPA_SMLEN                                                  37
C -                                                                       37
      REAL SFPA_SSLEN                                                     37
      EQUIVALENCE (BANK_SFPA(15),SFPA_SSLEN,RE_SFPA_SSLEN),               37
     *(NAMES_SFPA(13),NSFPA_SSLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SSLEN),(MAP_SFPA(1,13),          37
     *MSFPA_SSLEN)                                                        37
      CHARACTER CSFPA_SSLEN*40/' '/                                       37
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 37
      INTEGER MSFPA_SSLEN(2)/1,1/                                         37
      REAL RE_SFPA_SSLEN                                                  37
C -                                                                       37
      REAL SFPA_WPLEN                                                     37
      EQUIVALENCE (BANK_SFPA(16),SFPA_WPLEN,RE_SFPA_WPLEN),               37
     *(NAMES_SFPA(14),NSFPA_WPLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_WPLEN),(MAP_SFPA(1,14),          37
     *MSFPA_WPLEN)                                                        37
      CHARACTER CSFPA_WPLEN*40/' '/                                       37
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 37
      INTEGER MSFPA_WPLEN(2)/1,1/                                         37
      REAL RE_SFPA_WPLEN                                                  37
C -                                                                       37
      REAL SFPA_SDLEN                                                     37
      EQUIVALENCE (BANK_SFPA(17),SFPA_SDLEN,RE_SFPA_SDLEN),               37
     *(NAMES_SFPA(15),NSFPA_SDLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SDLEN),(MAP_SFPA(1,15),          37
     *MSFPA_SDLEN)                                                        37
      CHARACTER CSFPA_SDLEN*40/' '/                                       37
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 37
      INTEGER MSFPA_SDLEN(2)/1,1/                                         37
      REAL RE_SFPA_SDLEN                                                  37
C -                                                                       37
      REAL SFPA_TILT                                                      37
      EQUIVALENCE (BANK_SFPA(18),SFPA_TILT,RE_SFPA_TILT),                 37
     *(NAMES_SFPA(16),NSFPA_TILT)                                         37
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_TILT),(MAP_SFPA(1,16),           37
     *MSFPA_TILT)                                                         37
      CHARACTER CSFPA_TILT*40/' '/                                        37
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   37
      INTEGER MSFPA_TILT(2)/1,1/                                          37
      REAL RE_SFPA_TILT                                                   37
C -                                                                       37
      REAL SFPA_CPRAD                                                     37
      EQUIVALENCE (BANK_SFPA(19),SFPA_CPRAD,RE_SFPA_CPRAD),               37
     *(NAMES_SFPA(17),NSFPA_CPRAD)                                        37
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_CPRAD),(MAP_SFPA(1,17),          37
     *MSFPA_CPRAD)                                                        37
      CHARACTER CSFPA_CPRAD*40/' '/                                       37
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 37
      INTEGER MSFPA_CPRAD(2)/1,1/                                         37
      REAL RE_SFPA_CPRAD                                                  37
C -                                                                       37
      REAL SFPA_CPRAL                                                     37
      EQUIVALENCE (BANK_SFPA(20),SFPA_CPRAL,RE_SFPA_CPRAL),               37
     *(NAMES_SFPA(18),NSFPA_CPRAL)                                        37
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_CPRAL),(MAP_SFPA(1,18),          37
     *MSFPA_CPRAL)                                                        37
      CHARACTER CSFPA_CPRAL*40/' '/                                       37
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 37
      INTEGER MSFPA_CPRAL(2)/1,1/                                         37
      REAL RE_SFPA_CPRAL                                                  37
C -                                                                       37
      REAL SFPA_CFRAD                                                     37
      EQUIVALENCE (BANK_SFPA(21),SFPA_CFRAD,RE_SFPA_CFRAD),               37
     *(NAMES_SFPA(19),NSFPA_CFRAD)                                        37
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_CFRAD),(MAP_SFPA(1,19),          37
     *MSFPA_CFRAD)                                                        37
      CHARACTER CSFPA_CFRAD*40/' '/                                       37
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 37
      INTEGER MSFPA_CFRAD(2)/1,1/                                         37
      REAL RE_SFPA_CFRAD                                                  37
C -                                                                       37
      REAL SFPA_GPTHK                                                     37
      EQUIVALENCE (BANK_SFPA(22),SFPA_GPTHK,RE_SFPA_GPTHK),               37
     *(NAMES_SFPA(20),NSFPA_GPTHK)                                        37
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_GPTHK),(MAP_SFPA(1,20),          37
     *MSFPA_GPTHK)                                                        37
      CHARACTER CSFPA_GPTHK*40/' '/                                       37
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 37
      INTEGER MSFPA_GPTHK(2)/1,1/                                         37
      REAL RE_SFPA_GPTHK                                                  37
C -                                                                       37
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                37
      PARAMETER (LEN_SFPA=22)                                             37
      PARAMETER (LLEN_SFPA=20)                                            37
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           37
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        37
      DATA IDNUM_SFPA/10*0/                                               37
      REAL BANK_SFPA(LEN_SFPA)                                            37
      CHARACTER*60 BPATH_SFPA/' '/                                        37
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          37
     *BPATH_SFPA                                                          37
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             37
C *                                                                       37
C -                                                                       39
      REAL SSLD_VERSION                                                   39
      EQUIVALENCE (BANK_SSLD(3),SSLD_VERSION,RE_SSLD_VERSION),            39
     *(NAMES_SSLD(1),NSSLD_VERSION)                                       39
      EQUIVALENCE (COMMEN_SSLD(1),CSSLD_VERSION),(MAP_SSLD(1,1),          39
     *MSSLD_VERSION)                                                      39
      CHARACTER CSSLD_VERSION*40/' '/                                     39
      CHARACTER NSSLD_VERSION*12/'R.VERSION'/                             39
      INTEGER MSSLD_VERSION(2)/1,1/                                       39
      REAL RE_SSLD_VERSION                                                39
C -                                                                       39
      REAL SSLD_SINRINN                                                   39
      EQUIVALENCE (BANK_SSLD(4),SSLD_SINRINN,RE_SSLD_SINRINN),            39
     *(NAMES_SSLD(2),NSSLD_SINRINN)                                       39
      EQUIVALENCE (COMMEN_SSLD(2),CSSLD_SINRINN),(MAP_SSLD(1,2),          39
     *MSSLD_SINRINN)                                                      39
      CHARACTER CSSLD_SINRINN*40/' '/                                     39
      CHARACTER NSSLD_SINRINN*12/'R.SINRINN'/                             39
      INTEGER MSSLD_SINRINN(2)/1,1/                                       39
      REAL RE_SSLD_SINRINN                                                39
C -                                                                       39
      REAL SSLD_SINROUT                                                   39
      EQUIVALENCE (BANK_SSLD(5),SSLD_SINROUT,RE_SSLD_SINROUT),            39
     *(NAMES_SSLD(3),NSSLD_SINROUT)                                       39
      EQUIVALENCE (COMMEN_SSLD(3),CSSLD_SINROUT),(MAP_SSLD(1,3),          39
     *MSSLD_SINROUT)                                                      39
      CHARACTER CSSLD_SINROUT*40/' '/                                     39
      CHARACTER NSSLD_SINROUT*12/'R.SINROUT'/                             39
      INTEGER MSSLD_SINROUT(2)/1,1/                                       39
      REAL RE_SSLD_SINROUT                                                39
C -                                                                       39
      REAL SSLD_SINLEN                                                    39
      EQUIVALENCE (BANK_SSLD(6),SSLD_SINLEN,RE_SSLD_SINLEN),              39
     *(NAMES_SSLD(4),NSSLD_SINLEN)                                        39
      EQUIVALENCE (COMMEN_SSLD(4),CSSLD_SINLEN),(MAP_SSLD(1,4),           39
     *MSSLD_SINLEN)                                                       39
      CHARACTER CSSLD_SINLEN*40/' '/                                      39
      CHARACTER NSSLD_SINLEN*12/'R.SINLEN'/                               39
      INTEGER MSSLD_SINLEN(2)/1,1/                                        39
      REAL RE_SSLD_SINLEN                                                 39
C -                                                                       39
      REAL SSLD_SSEPRINN                                                  39
      EQUIVALENCE (BANK_SSLD(7),SSLD_SSEPRINN,RE_SSLD_SSEPRINN),          39
     *(NAMES_SSLD(5),NSSLD_SSEPRINN)                                      39
      EQUIVALENCE (COMMEN_SSLD(5),CSSLD_SSEPRINN),(MAP_SSLD(1,5),         39
     *MSSLD_SSEPRINN)                                                     39
      CHARACTER CSSLD_SSEPRINN*40/' '/                                    39
      CHARACTER NSSLD_SSEPRINN*12/'R.SSEPRINN'/                           39
      INTEGER MSSLD_SSEPRINN(2)/1,1/                                      39
      REAL RE_SSLD_SSEPRINN                                               39
C -                                                                       39
      REAL SSLD_SSEPROUT                                                  39
      EQUIVALENCE (BANK_SSLD(8),SSLD_SSEPROUT,RE_SSLD_SSEPROUT),          39
     *(NAMES_SSLD(6),NSSLD_SSEPROUT)                                      39
      EQUIVALENCE (COMMEN_SSLD(6),CSSLD_SSEPROUT),(MAP_SSLD(1,6),         39
     *MSSLD_SSEPROUT)                                                     39
      CHARACTER CSSLD_SSEPROUT*40/' '/                                    39
      CHARACTER NSSLD_SSEPROUT*12/'R.SSEPROUT'/                           39
      INTEGER MSSLD_SSEPROUT(2)/1,1/                                      39
      REAL RE_SSLD_SSEPROUT                                               39
C -                                                                       39
      REAL SSLD_SSEPLEN                                                   39
      EQUIVALENCE (BANK_SSLD(9),SSLD_SSEPLEN,RE_SSLD_SSEPLEN),            39
     *(NAMES_SSLD(7),NSSLD_SSEPLEN)                                       39
      EQUIVALENCE (COMMEN_SSLD(7),CSSLD_SSEPLEN),(MAP_SSLD(1,7),          39
     *MSSLD_SSEPLEN)                                                      39
      CHARACTER CSSLD_SSEPLEN*40/' '/                                     39
      CHARACTER NSSLD_SSEPLEN*12/'R.SSEPLEN'/                             39
      INTEGER MSSLD_SSEPLEN(2)/1,1/                                       39
      REAL RE_SSLD_SSEPLEN                                                39
C -                                                                       39
      REAL SSLD_SOUTRINN                                                  39
      EQUIVALENCE (BANK_SSLD(10),SSLD_SOUTRINN,RE_SSLD_SOUTRINN),         39
     *(NAMES_SSLD(8),NSSLD_SOUTRINN)                                      39
      EQUIVALENCE (COMMEN_SSLD(8),CSSLD_SOUTRINN),(MAP_SSLD(1,8),         39
     *MSSLD_SOUTRINN)                                                     39
      CHARACTER CSSLD_SOUTRINN*40/' '/                                    39
      CHARACTER NSSLD_SOUTRINN*12/'R.SOUTRINN'/                           39
      INTEGER MSSLD_SOUTRINN(2)/1,1/                                      39
      REAL RE_SSLD_SOUTRINN                                               39
C -                                                                       39
      REAL SSLD_SOUTROUT                                                  39
      EQUIVALENCE (BANK_SSLD(11),SSLD_SOUTROUT,RE_SSLD_SOUTROUT),         39
     *(NAMES_SSLD(9),NSSLD_SOUTROUT)                                      39
      EQUIVALENCE (COMMEN_SSLD(9),CSSLD_SOUTROUT),(MAP_SSLD(1,9),         39
     *MSSLD_SOUTROUT)                                                     39
      CHARACTER CSSLD_SOUTROUT*40/' '/                                    39
      CHARACTER NSSLD_SOUTROUT*12/'R.SOUTROUT'/                           39
      INTEGER MSSLD_SOUTROUT(2)/1,1/                                      39
      REAL RE_SSLD_SOUTROUT                                               39
C -                                                                       39
      REAL SSLD_SOUTLEN                                                   39
      EQUIVALENCE (BANK_SSLD(12),SSLD_SOUTLEN,RE_SSLD_SOUTLEN),           39
     *(NAMES_SSLD(10),NSSLD_SOUTLEN)                                      39
      EQUIVALENCE (COMMEN_SSLD(10),CSSLD_SOUTLEN),(MAP_SSLD(1,10),        39
     *MSSLD_SOUTLEN)                                                      39
      CHARACTER CSSLD_SOUTLEN*40/' '/                                     39
      CHARACTER NSSLD_SOUTLEN*12/'R.SOUTLEN'/                             39
      INTEGER MSSLD_SOUTLEN(2)/1,1/                                       39
      REAL RE_SSLD_SOUTLEN                                                39
C -                                                                       39
      REAL SSLD_ALMESHID                                                  39
      EQUIVALENCE (BANK_SSLD(13),SSLD_ALMESHID,RE_SSLD_ALMESHID),         39
     *(NAMES_SSLD(11),NSSLD_ALMESHID)                                     39
      EQUIVALENCE (COMMEN_SSLD(11),CSSLD_ALMESHID),(MAP_SSLD(1,11),       39
     *MSSLD_ALMESHID)                                                     39
      CHARACTER CSSLD_ALMESHID*40/' '/                                    39
      CHARACTER NSSLD_ALMESHID*12/'R.ALMESHID'/                           39
      INTEGER MSSLD_ALMESHID(2)/1,1/                                      39
      REAL RE_SSLD_ALMESHID                                               39
C -                                                                       39
      REAL SSLD_ALMESHOD                                                  39
      EQUIVALENCE (BANK_SSLD(14),SSLD_ALMESHOD,RE_SSLD_ALMESHOD),         39
     *(NAMES_SSLD(12),NSSLD_ALMESHOD)                                     39
      EQUIVALENCE (COMMEN_SSLD(12),CSSLD_ALMESHOD),(MAP_SSLD(1,12),       39
     *MSSLD_ALMESHOD)                                                     39
      CHARACTER CSSLD_ALMESHOD*40/' '/                                    39
      CHARACTER NSSLD_ALMESHOD*12/'R.ALMESHOD'/                           39
      INTEGER MSSLD_ALMESHOD(2)/1,1/                                      39
      REAL RE_SSLD_ALMESHOD                                               39
C -                                                                       39
      REAL SSLD_ALMSHTHK                                                  39
      EQUIVALENCE (BANK_SSLD(15),SSLD_ALMSHTHK,RE_SSLD_ALMSHTHK),         39
     *(NAMES_SSLD(13),NSSLD_ALMSHTHK)                                     39
      EQUIVALENCE (COMMEN_SSLD(13),CSSLD_ALMSHTHK),(MAP_SSLD(1,13),       39
     *MSSLD_ALMSHTHK)                                                     39
      CHARACTER CSSLD_ALMSHTHK*40/' '/                                    39
      CHARACTER NSSLD_ALMSHTHK*12/'R.ALMSHTHK'/                           39
      INTEGER MSSLD_ALMSHTHK(2)/1,1/                                      39
      REAL RE_SSLD_ALMSHTHK                                               39
C -                                                                       39
      REAL SSLD_ALMSHPOS                                                  39
      EQUIVALENCE (BANK_SSLD(16),SSLD_ALMSHPOS,RE_SSLD_ALMSHPOS),         39
     *(NAMES_SSLD(14),NSSLD_ALMSHPOS)                                     39
      EQUIVALENCE (COMMEN_SSLD(14),CSSLD_ALMSHPOS),(MAP_SSLD(1,14),       39
     *MSSLD_ALMSHPOS)                                                     39
      CHARACTER CSSLD_ALMSHPOS*40/' '/                                    39
      CHARACTER NSSLD_ALMSHPOS*12/'R.ALMSHPOS'/                           39
      INTEGER MSSLD_ALMSHPOS(2)/1,1/                                      39
      REAL RE_SSLD_ALMSHPOS                                               39
C -                                                                       39
      INTEGER LEN_SSLD,LLEN_SSLD,LINK_SSLD                                39
      PARAMETER (LEN_SSLD=16)                                             39
      PARAMETER (LLEN_SSLD=14)                                            39
      INTEGER FLAG_SSLD/0/,MAP_SSLD(2,LLEN_SSLD),IDNUM_SSLD(10)           39
      CHARACTER NAMES_SSLD(LLEN_SSLD)*12,COMMEN_SSLD(LLEN_SSLD)*40        39
      DATA IDNUM_SSLD/10*0/                                               39
      REAL BANK_SSLD(LEN_SSLD)                                            39
      CHARACTER*60 BPATH_SSLD/' '/                                        39
      SAVE FLAG_SSLD,MAP_SSLD,IDNUM_SSLD,NAMES_SSLD,COMMEN_SSLD,          39
     *BPATH_SSLD                                                          39
      DATA LINK_SSLD/0/,BANK_SSLD/LEN_SSLD*0/                             39
C *                                                                       39
C -                                                                       40
      INTEGER SCBP_LAYER                                                  40
      EQUIVALENCE (BANK_SCBP(3),SCBP_LAYER,RE_SCBP_LAYER),                40
     *(NAMES_SCBP(1),NSCBP_LAYER)                                         40
      EQUIVALENCE (COMMEN_SCBP(1),CSCBP_LAYER),(MAP_SCBP(1,1),            40
     *MSCBP_LAYER)                                                        40
      CHARACTER CSCBP_LAYER*40/' '/                                       40
      CHARACTER NSCBP_LAYER*12/'I.LAYER'/                                 40
      INTEGER MSCBP_LAYER(2)/1,1/                                         40
      REAL RE_SCBP_LAYER                                                  40
C -                                                                       40
      REAL SCBP_LEN                                                       40
      EQUIVALENCE (BANK_SCBP(4),SCBP_LEN,RE_SCBP_LEN),(NAMES_SCBP(2),     40
     *NSCBP_LEN)                                                          40
      EQUIVALENCE (COMMEN_SCBP(2),CSCBP_LEN),(MAP_SCBP(1,2),MSCBP_LEN)    40
      CHARACTER CSCBP_LEN*40/' '/                                         40
      CHARACTER NSCBP_LEN*12/'R.LEN'/                                     40
      INTEGER MSCBP_LEN(2)/1,1/                                           40
      REAL RE_SCBP_LEN                                                    40
C -                                                                       40
      REAL SCBP_RMIN1                                                     40
      EQUIVALENCE (BANK_SCBP(5),SCBP_RMIN1,RE_SCBP_RMIN1),                40
     *(NAMES_SCBP(3),NSCBP_RMIN1)                                         40
      EQUIVALENCE (COMMEN_SCBP(3),CSCBP_RMIN1),(MAP_SCBP(1,3),            40
     *MSCBP_RMIN1)                                                        40
      CHARACTER CSCBP_RMIN1*40/' '/                                       40
      CHARACTER NSCBP_RMIN1*12/'R.RMIN1'/                                 40
      INTEGER MSCBP_RMIN1(2)/1,1/                                         40
      REAL RE_SCBP_RMIN1                                                  40
C -                                                                       40
      REAL SCBP_RMAX1                                                     40
      EQUIVALENCE (BANK_SCBP(6),SCBP_RMAX1,RE_SCBP_RMAX1),                40
     *(NAMES_SCBP(4),NSCBP_RMAX1)                                         40
      EQUIVALENCE (COMMEN_SCBP(4),CSCBP_RMAX1),(MAP_SCBP(1,4),            40
     *MSCBP_RMAX1)                                                        40
      CHARACTER CSCBP_RMAX1*40/' '/                                       40
      CHARACTER NSCBP_RMAX1*12/'R.RMAX1'/                                 40
      INTEGER MSCBP_RMAX1(2)/1,1/                                         40
      REAL RE_SCBP_RMAX1                                                  40
C -                                                                       40
      REAL SCBP_RMIN2                                                     40
      EQUIVALENCE (BANK_SCBP(7),SCBP_RMIN2,RE_SCBP_RMIN2),                40
     *(NAMES_SCBP(5),NSCBP_RMIN2)                                         40
      EQUIVALENCE (COMMEN_SCBP(5),CSCBP_RMIN2),(MAP_SCBP(1,5),            40
     *MSCBP_RMIN2)                                                        40
      CHARACTER CSCBP_RMIN2*40/' '/                                       40
      CHARACTER NSCBP_RMIN2*12/'R.RMIN2'/                                 40
      INTEGER MSCBP_RMIN2(2)/1,1/                                         40
      REAL RE_SCBP_RMIN2                                                  40
C -                                                                       40
      REAL SCBP_RMAX2                                                     40
      EQUIVALENCE (BANK_SCBP(8),SCBP_RMAX2,RE_SCBP_RMAX2),                40
     *(NAMES_SCBP(6),NSCBP_RMAX2)                                         40
      EQUIVALENCE (COMMEN_SCBP(6),CSCBP_RMAX2),(MAP_SCBP(1,6),            40
     *MSCBP_RMAX2)                                                        40
      CHARACTER CSCBP_RMAX2*40/' '/                                       40
      CHARACTER NSCBP_RMAX2*12/'R.RMAX2'/                                 40
      INTEGER MSCBP_RMAX2(2)/1,1/                                         40
      REAL RE_SCBP_RMAX2                                                  40
C -                                                                       40
      REAL SCBP_VOL                                                       40
      EQUIVALENCE (BANK_SCBP(9),SCBP_VOL,RE_SCBP_VOL),(NAMES_SCBP(7),     40
     *NSCBP_VOL)                                                          40
      EQUIVALENCE (COMMEN_SCBP(7),CSCBP_VOL),(MAP_SCBP(1,7),MSCBP_VOL)    40
      CHARACTER CSCBP_VOL*40/' '/                                         40
      CHARACTER NSCBP_VOL*12/'R.VOL'/                                     40
      INTEGER MSCBP_VOL(2)/1,1/                                           40
      REAL RE_SCBP_VOL                                                    40
C -                                                                       40
      INTEGER LEN_SCBP,LLEN_SCBP,LINK_SCBP                                40
      PARAMETER (LEN_SCBP=9)                                              40
      PARAMETER (LLEN_SCBP=7)                                             40
      INTEGER FLAG_SCBP/0/,MAP_SCBP(2,LLEN_SCBP),IDNUM_SCBP(10)           40
      CHARACTER NAMES_SCBP(LLEN_SCBP)*12,COMMEN_SCBP(LLEN_SCBP)*40        40
      DATA IDNUM_SCBP/10*0/                                               40
      REAL BANK_SCBP(LEN_SCBP)                                            40
      CHARACTER*60 BPATH_SCBP/' '/                                        40
      SAVE FLAG_SCBP,MAP_SCBP,IDNUM_SCBP,NAMES_SCBP,COMMEN_SCBP,          40
     *BPATH_SCBP                                                          40
      DATA LINK_SCBP/0/,BANK_SCBP/LEN_SCBP*0/                             40
C *                                                                       40
C -                                                                       41
      INTEGER SFEP_LAYER                                                  41
      EQUIVALENCE (BANK_SFEP(3),SFEP_LAYER,RE_SFEP_LAYER),                41
     *(NAMES_SFEP(1),NSFEP_LAYER)                                         41
      EQUIVALENCE (COMMEN_SFEP(1),CSFEP_LAYER),(MAP_SFEP(1,1),            41
     *MSFEP_LAYER)                                                        41
      CHARACTER CSFEP_LAYER*40/' '/                                       41
      CHARACTER NSFEP_LAYER*12/'I.LAYER'/                                 41
      INTEGER MSFEP_LAYER(2)/1,1/                                         41
      REAL RE_SFEP_LAYER                                                  41
C -                                                                       41
      REAL SFEP_LEN                                                       41
      EQUIVALENCE (BANK_SFEP(4),SFEP_LEN,RE_SFEP_LEN),(NAMES_SFEP(2),     41
     *NSFEP_LEN)                                                          41
      EQUIVALENCE (COMMEN_SFEP(2),CSFEP_LEN),(MAP_SFEP(1,2),MSFEP_LEN)    41
      CHARACTER CSFEP_LEN*40/' '/                                         41
      CHARACTER NSFEP_LEN*12/'R.LEN'/                                     41
      INTEGER MSFEP_LEN(2)/1,1/                                           41
      REAL RE_SFEP_LEN                                                    41
C -                                                                       41
      REAL SFEP_RMIN1                                                     41
      EQUIVALENCE (BANK_SFEP(5),SFEP_RMIN1,RE_SFEP_RMIN1),                41
     *(NAMES_SFEP(3),NSFEP_RMIN1)                                         41
      EQUIVALENCE (COMMEN_SFEP(3),CSFEP_RMIN1),(MAP_SFEP(1,3),            41
     *MSFEP_RMIN1)                                                        41
      CHARACTER CSFEP_RMIN1*40/' '/                                       41
      CHARACTER NSFEP_RMIN1*12/'R.RMIN1'/                                 41
      INTEGER MSFEP_RMIN1(2)/1,1/                                         41
      REAL RE_SFEP_RMIN1                                                  41
C -                                                                       41
      REAL SFEP_RMAX1                                                     41
      EQUIVALENCE (BANK_SFEP(6),SFEP_RMAX1,RE_SFEP_RMAX1),                41
     *(NAMES_SFEP(4),NSFEP_RMAX1)                                         41
      EQUIVALENCE (COMMEN_SFEP(4),CSFEP_RMAX1),(MAP_SFEP(1,4),            41
     *MSFEP_RMAX1)                                                        41
      CHARACTER CSFEP_RMAX1*40/' '/                                       41
      CHARACTER NSFEP_RMAX1*12/'R.RMAX1'/                                 41
      INTEGER MSFEP_RMAX1(2)/1,1/                                         41
      REAL RE_SFEP_RMAX1                                                  41
C -                                                                       41
      REAL SFEP_RMIN2                                                     41
      EQUIVALENCE (BANK_SFEP(7),SFEP_RMIN2,RE_SFEP_RMIN2),                41
     *(NAMES_SFEP(5),NSFEP_RMIN2)                                         41
      EQUIVALENCE (COMMEN_SFEP(5),CSFEP_RMIN2),(MAP_SFEP(1,5),            41
     *MSFEP_RMIN2)                                                        41
      CHARACTER CSFEP_RMIN2*40/' '/                                       41
      CHARACTER NSFEP_RMIN2*12/'R.RMIN2'/                                 41
      INTEGER MSFEP_RMIN2(2)/1,1/                                         41
      REAL RE_SFEP_RMIN2                                                  41
C -                                                                       41
      REAL SFEP_RMAX2                                                     41
      EQUIVALENCE (BANK_SFEP(8),SFEP_RMAX2,RE_SFEP_RMAX2),                41
     *(NAMES_SFEP(6),NSFEP_RMAX2)                                         41
      EQUIVALENCE (COMMEN_SFEP(6),CSFEP_RMAX2),(MAP_SFEP(1,6),            41
     *MSFEP_RMAX2)                                                        41
      CHARACTER CSFEP_RMAX2*40/' '/                                       41
      CHARACTER NSFEP_RMAX2*12/'R.RMAX2'/                                 41
      INTEGER MSFEP_RMAX2(2)/1,1/                                         41
      REAL RE_SFEP_RMAX2                                                  41
C -                                                                       41
      REAL SFEP_VOL                                                       41
      EQUIVALENCE (BANK_SFEP(9),SFEP_VOL,RE_SFEP_VOL),(NAMES_SFEP(7),     41
     *NSFEP_VOL)                                                          41
      EQUIVALENCE (COMMEN_SFEP(7),CSFEP_VOL),(MAP_SFEP(1,7),MSFEP_VOL)    41
      CHARACTER CSFEP_VOL*40/' '/                                         41
      CHARACTER NSFEP_VOL*12/'R.VOL'/                                     41
      INTEGER MSFEP_VOL(2)/1,1/                                           41
      REAL RE_SFEP_VOL                                                    41
C -                                                                       41
      REAL SFEP_VOLPLAST                                                  41
      EQUIVALENCE (BANK_SFEP(10),SFEP_VOLPLAST,RE_SFEP_VOLPLAST),         41
     *(NAMES_SFEP(8),NSFEP_VOLPLAST)                                      41
      EQUIVALENCE (COMMEN_SFEP(8),CSFEP_VOLPLAST),(MAP_SFEP(1,8),         41
     *MSFEP_VOLPLAST)                                                     41
      CHARACTER CSFEP_VOLPLAST*40/' '/                                    41
      CHARACTER NSFEP_VOLPLAST*12/'R.VOLPLAST'/                           41
      INTEGER MSFEP_VOLPLAST(2)/1,1/                                      41
      REAL RE_SFEP_VOLPLAST                                               41
C -                                                                       41
      INTEGER LEN_SFEP,LLEN_SFEP,LINK_SFEP                                41
      PARAMETER (LEN_SFEP=10)                                             41
      PARAMETER (LLEN_SFEP=8)                                             41
      INTEGER FLAG_SFEP/0/,MAP_SFEP(2,LLEN_SFEP),IDNUM_SFEP(10)           41
      CHARACTER NAMES_SFEP(LLEN_SFEP)*12,COMMEN_SFEP(LLEN_SFEP)*40        41
      DATA IDNUM_SFEP/10*0/                                               41
      REAL BANK_SFEP(LEN_SFEP)                                            41
      CHARACTER*60 BPATH_SFEP/' '/                                        41
      SAVE FLAG_SFEP,MAP_SFEP,IDNUM_SFEP,NAMES_SFEP,COMMEN_SFEP,          41
     *BPATH_SFEP                                                          41
      DATA LINK_SFEP/0/,BANK_SFEP/LEN_SFEP*0/                             41
C *                                                                       41
C -                                                                       42
      INTEGER SWCX_LAYER                                                  42
      EQUIVALENCE (BANK_SWCX(3),SWCX_LAYER,RE_SWCX_LAYER),                42
     *(NAMES_SWCX(1),NSWCX_LAYER)                                         42
      EQUIVALENCE (COMMEN_SWCX(1),CSWCX_LAYER),(MAP_SWCX(1,1),            42
     *MSWCX_LAYER)                                                        42
      CHARACTER CSWCX_LAYER*40/' '/                                       42
      CHARACTER NSWCX_LAYER*12/'I.LAYER'/                                 42
      INTEGER MSWCX_LAYER(2)/1,1/                                         42
      REAL RE_SWCX_LAYER                                                  42
C -                                                                       42
      REAL SWCX_LENGTH                                                    42
      EQUIVALENCE (BANK_SWCX(4),SWCX_LENGTH,RE_SWCX_LENGTH),              42
     *(NAMES_SWCX(2),NSWCX_LENGTH)                                        42
      EQUIVALENCE (COMMEN_SWCX(2),CSWCX_LENGTH),(MAP_SWCX(1,2),           42
     *MSWCX_LENGTH)                                                       42
      CHARACTER CSWCX_LENGTH*40/' '/                                      42
      CHARACTER NSWCX_LENGTH*12/'R.LENGTH'/                               42
      INTEGER MSWCX_LENGTH(2)/1,1/                                        42
      REAL RE_SWCX_LENGTH                                                 42
C -                                                                       42
      REAL SWCX_DR                                                        42
      EQUIVALENCE (BANK_SWCX(5),SWCX_DR,RE_SWCX_DR),(NAMES_SWCX(3),       42
     *NSWCX_DR)                                                           42
      EQUIVALENCE (COMMEN_SWCX(3),CSWCX_DR),(MAP_SWCX(1,3),MSWCX_DR)      42
      CHARACTER CSWCX_DR*40/' '/                                          42
      CHARACTER NSWCX_DR*12/'R.DR'/                                       42
      INTEGER MSWCX_DR(2)/1,1/                                            42
      REAL RE_SWCX_DR                                                     42
C -                                                                       42
      REAL SWCX_OFFSET                                                    42
      EQUIVALENCE (BANK_SWCX(6),SWCX_OFFSET,RE_SWCX_OFFSET),              42
     *(NAMES_SWCX(4),NSWCX_OFFSET)                                        42
      EQUIVALENCE (COMMEN_SWCX(4),CSWCX_OFFSET),(MAP_SWCX(1,4),           42
     *MSWCX_OFFSET)                                                       42
      CHARACTER CSWCX_OFFSET*40/' '/                                      42
      CHARACTER NSWCX_OFFSET*12/'R.OFFSET'/                               42
      INTEGER MSWCX_OFFSET(2)/1,1/                                        42
      REAL RE_SWCX_OFFSET                                                 42
C -                                                                       42
      REAL SWCX_RAD                                                       42
      EQUIVALENCE (BANK_SWCX(7),SWCX_RAD,RE_SWCX_RAD),(NAMES_SWCX(5),     42
     *NSWCX_RAD)                                                          42
      EQUIVALENCE (COMMEN_SWCX(5),CSWCX_RAD),(MAP_SWCX(1,5),MSWCX_RAD)    42
      CHARACTER CSWCX_RAD*40/' '/                                         42
      CHARACTER NSWCX_RAD*12/'R.RAD'/                                     42
      INTEGER MSWCX_RAD(2)/1,1/                                           42
      REAL RE_SWCX_RAD                                                    42
C -                                                                       42
      REAL SWCX_WALL                                                      42
      EQUIVALENCE (BANK_SWCX(8),SWCX_WALL,RE_SWCX_WALL),(NAMES_SWCX(6),   42
     *NSWCX_WALL)                                                         42
      EQUIVALENCE (COMMEN_SWCX(6),CSWCX_WALL),(MAP_SWCX(1,6),             42
     *MSWCX_WALL)                                                         42
      CHARACTER CSWCX_WALL*40/' '/                                        42
      CHARACTER NSWCX_WALL*12/'R.WALL'/                                   42
      INTEGER MSWCX_WALL(2)/1,1/                                          42
      REAL RE_SWCX_WALL                                                   42
C -                                                                       42
      REAL SWCX_ROFFSET                                                   42
      EQUIVALENCE (BANK_SWCX(9),SWCX_ROFFSET,RE_SWCX_ROFFSET),            42
     *(NAMES_SWCX(7),NSWCX_ROFFSET)                                       42
      EQUIVALENCE (COMMEN_SWCX(7),CSWCX_ROFFSET),(MAP_SWCX(1,7),          42
     *MSWCX_ROFFSET)                                                      42
      CHARACTER CSWCX_ROFFSET*40/' '/                                     42
      CHARACTER NSWCX_ROFFSET*12/'R.ROFFSET'/                             42
      INTEGER MSWCX_ROFFSET(2)/1,1/                                       42
      REAL RE_SWCX_ROFFSET                                                42
C -                                                                       42
      INTEGER LEN_SWCX,LLEN_SWCX,LINK_SWCX                                42
      PARAMETER (LEN_SWCX=9)                                              42
      PARAMETER (LLEN_SWCX=7)                                             42
      INTEGER FLAG_SWCX/0/,MAP_SWCX(2,LLEN_SWCX),IDNUM_SWCX(10)           42
      CHARACTER NAMES_SWCX(LLEN_SWCX)*12,COMMEN_SWCX(LLEN_SWCX)*40        42
      DATA IDNUM_SWCX/10*0/                                               42
      REAL BANK_SWCX(LEN_SWCX)                                            42
      CHARACTER*60 BPATH_SWCX/' '/                                        42
      SAVE FLAG_SWCX,MAP_SWCX,IDNUM_SWCX,NAMES_SWCX,COMMEN_SWCX,          42
     *BPATH_SWCX                                                          42
      DATA LINK_SWCX/0/,BANK_SWCX/LEN_SWCX*0/                             42
C *                                                                       42
C -                                                                       43
      REAL SOUP_VERSION                                                   43
      EQUIVALENCE (BANK_SOUP(3),SOUP_VERSION,RE_SOUP_VERSION),            43
     *(NAMES_SOUP(1),NSOUP_VERSION)                                       43
      EQUIVALENCE (COMMEN_SOUP(1),CSOUP_VERSION),(MAP_SOUP(1,1),          43
     *MSOUP_VERSION)                                                      43
      CHARACTER CSOUP_VERSION*40/' '/                                     43
      CHARACTER NSOUP_VERSION*12/'R.VERSION'/                             43
      INTEGER MSOUP_VERSION(2)/1,1/                                       43
      REAL RE_SOUP_VERSION                                                43
C -                                                                       43
      REAL SOUP_LENGTH                                                    43
      EQUIVALENCE (BANK_SOUP(4),SOUP_LENGTH,RE_SOUP_LENGTH),              43
     *(NAMES_SOUP(2),NSOUP_LENGTH)                                        43
      EQUIVALENCE (COMMEN_SOUP(2),CSOUP_LENGTH),(MAP_SOUP(1,2),           43
     *MSOUP_LENGTH)                                                       43
      CHARACTER CSOUP_LENGTH*40/' '/                                      43
      CHARACTER NSOUP_LENGTH*12/'R.LENGTH'/                               43
      INTEGER MSOUP_LENGTH(2)/1,1/                                        43
      REAL RE_SOUP_LENGTH                                                 43
C -                                                                       43
      REAL SOUP_ROUT                                                      43
      EQUIVALENCE (BANK_SOUP(5),SOUP_ROUT,RE_SOUP_ROUT),(NAMES_SOUP(3),   43
     *NSOUP_ROUT)                                                         43
      EQUIVALENCE (COMMEN_SOUP(3),CSOUP_ROUT),(MAP_SOUP(1,3),             43
     *MSOUP_ROUT)                                                         43
      CHARACTER CSOUP_ROUT*40/' '/                                        43
      CHARACTER NSOUP_ROUT*12/'R.ROUT'/                                   43
      INTEGER MSOUP_ROUT(2)/1,1/                                          43
      REAL RE_SOUP_ROUT                                                   43
C -                                                                       43
      REAL SOUP_DR                                                        43
      EQUIVALENCE (BANK_SOUP(6),SOUP_DR,RE_SOUP_DR),(NAMES_SOUP(4),       43
     *NSOUP_DR)                                                           43
      EQUIVALENCE (COMMEN_SOUP(4),CSOUP_DR),(MAP_SOUP(1,4),MSOUP_DR)      43
      CHARACTER CSOUP_DR*40/' '/                                          43
      CHARACTER NSOUP_DR*12/'R.DR'/                                       43
      INTEGER MSOUP_DR(2)/1,1/                                            43
      REAL RE_SOUP_DR                                                     43
C -                                                                       43
      REAL SOUP_PHI1                                                      43
      EQUIVALENCE (BANK_SOUP(7),SOUP_PHI1,RE_SOUP_PHI1),(NAMES_SOUP(5),   43
     *NSOUP_PHI1)                                                         43
      EQUIVALENCE (COMMEN_SOUP(5),CSOUP_PHI1),(MAP_SOUP(1,5),             43
     *MSOUP_PHI1)                                                         43
      CHARACTER CSOUP_PHI1*40/' '/                                        43
      CHARACTER NSOUP_PHI1*12/'R.PHI1'/                                   43
      INTEGER MSOUP_PHI1(2)/1,1/                                          43
      REAL RE_SOUP_PHI1                                                   43
C -                                                                       43
      REAL SOUP_PHI2                                                      43
      EQUIVALENCE (BANK_SOUP(8),SOUP_PHI2,RE_SOUP_PHI2),(NAMES_SOUP(6),   43
     *NSOUP_PHI2)                                                         43
      EQUIVALENCE (COMMEN_SOUP(6),CSOUP_PHI2),(MAP_SOUP(1,6),             43
     *MSOUP_PHI2)                                                         43
      CHARACTER CSOUP_PHI2*40/' '/                                        43
      CHARACTER NSOUP_PHI2*12/'R.PHI2'/                                   43
      INTEGER MSOUP_PHI2(2)/1,1/                                          43
      REAL RE_SOUP_PHI2                                                   43
C -                                                                       43
      REAL SOUP_DIAMOUT                                                   43
      EQUIVALENCE (BANK_SOUP(9),SOUP_DIAMOUT,RE_SOUP_DIAMOUT),            43
     *(NAMES_SOUP(7),NSOUP_DIAMOUT)                                       43
      EQUIVALENCE (COMMEN_SOUP(7),CSOUP_DIAMOUT),(MAP_SOUP(1,7),          43
     *MSOUP_DIAMOUT)                                                      43
      CHARACTER CSOUP_DIAMOUT*40/' '/                                     43
      CHARACTER NSOUP_DIAMOUT*12/'R.DIAMOUT'/                             43
      INTEGER MSOUP_DIAMOUT(2)/1,1/                                       43
      REAL RE_SOUP_DIAMOUT                                                43
C -                                                                       43
      REAL SOUP_DIAMIN                                                    43
      EQUIVALENCE (BANK_SOUP(10),SOUP_DIAMIN,RE_SOUP_DIAMIN),             43
     *(NAMES_SOUP(8),NSOUP_DIAMIN)                                        43
      EQUIVALENCE (COMMEN_SOUP(8),CSOUP_DIAMIN),(MAP_SOUP(1,8),           43
     *MSOUP_DIAMIN)                                                       43
      CHARACTER CSOUP_DIAMIN*40/' '/                                      43
      CHARACTER NSOUP_DIAMIN*12/'R.DIAMIN'/                               43
      INTEGER MSOUP_DIAMIN(2)/1,1/                                        43
      REAL RE_SOUP_DIAMIN                                                 43
C -                                                                       43
      INTEGER LEN_SOUP,LLEN_SOUP,LINK_SOUP                                43
      PARAMETER (LEN_SOUP=10)                                             43
      PARAMETER (LLEN_SOUP=8)                                             43
      INTEGER FLAG_SOUP/0/,MAP_SOUP(2,LLEN_SOUP),IDNUM_SOUP(10)           43
      CHARACTER NAMES_SOUP(LLEN_SOUP)*12,COMMEN_SOUP(LLEN_SOUP)*40        43
      DATA IDNUM_SOUP/10*0/                                               43
      REAL BANK_SOUP(LEN_SOUP)                                            43
      CHARACTER*60 BPATH_SOUP/' '/                                        43
      SAVE FLAG_SOUP,MAP_SOUP,IDNUM_SOUP,NAMES_SOUP,COMMEN_SOUP,          43
     *BPATH_SOUP                                                          43
      DATA LINK_SOUP/0/,BANK_SOUP/LEN_SOUP*0/                             43
C *                                                                       43
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 44
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         46
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           48
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          50
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    52
      REAL XBUF,PHI,XBUF1,XBUF2                                           54
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     56
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          58
     *SHIELD_PHI(4)                                                       58
      INTEGER I_PHI                                                       60
      RADII(1)= 6.37                                                      62
      RADII(2)= 7.38                                                      63
      RADII(3)=10.38                                                      64
      RADII(4)=11.27                                                      65
      RADII(5)=14.19                                                      66
      RADII(6)=15.13                                                      67
      RAD_OFFSET=0.015                                                    68
      SHIELD_PHI(1)=9.0                                                   69
      SHIELD_PHI(2)=27.0                                                  70
      SHIELD_PHI(3)=45.0                                                  71
      SHIELD_PHI(4)=63.0                                                  72
C *                                                                       73
      CALL AGSBEGM(MODULE,IPRIN)                                          73
      IF (IPRIN.LT.0) RETURN                                              73
C Filling bank SVTG - BASIC SVT DIMENSIONS                                73
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   73
      BPATH_SVTG='SVTG*'                                                  73
      IDNUM_SVTG(1)=0                                                     74
C GEOMETRY VERSION                                                        74
      SVTG_VERSION=2                                                      74
      CSVTG_VERSION='GEOMETRY VERSION'                                    74
C NUMBER OF SVT LAYERS (WAS 7)                                            75
      SVTG_NLAYER=7                                                       75
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         75
C STV INNERMOST RADIUS                                                    76
      SVTG_RSIZEMIN=4.100                                                 76
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               76
C STV OUTERMOST RADIUS                                                    77
      SVTG_RSIZEMAX=46.107                                                77
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               77
C SVT+FTPC LENGTH                                                         78
      SVTG_ZSIZEMAX=270                                                   78
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    78
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          79
      SVTG_ANGOFF=0                                                       79
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       79
C VERSIONING OF THE SHIELD                                                80
      SVTG_SUPPORTVER=1                                                   80
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         80
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
C WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIONS)                   91
      SWCA_WAFERGAP=0.05                                                  91
      CSWCA_WAFERGAP='WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIO  91
     *NS)'                                                                91
C DRIFT DIRECTION                                                         92
      SWCA_DRIFT=1                                                        92
      CSWCA_DRIFT='DRIFT DIRECTION'                                       92
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        93
      SWCA_STRUTLEN=1.0                                                   93
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   93
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    94
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           94
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   94
C *                                                                       94
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    94
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           94
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   94
C *                                                                       95
      CALL AGSBEGM(MODULE,IPRIN)                                          95
      IF (IPRIN.LT.0) RETURN                                              95
C Filling bank SSUP - SUPPORT STRUCTURES                                  95
      BANK_TITLE='SUPPORT STRUCTURES'                                     95
      BPATH_SSUP='SSUP*'                                                  95
      IDNUM_SSUP(1)=0                                                     96
C GEOMETRY VERSION                                                        96
      SSUP_VERSION=1                                                      96
      CSSUP_VERSION='GEOMETRY VERSION'                                    96
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            97
      SSUP_CABTHK=0.05                                                    97
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         97
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             98
      SSUP_HOSRMN=0.75                                                    98
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          98
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             99
      SSUP_HOSRMX=0.95                                                    99
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          99
C NUMBER OF WATER HOSES                                                   100
      SSUP_NHOSES=10                                                      100
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                100
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             101
      SSUP_WRPMYTHK=0.10                                                  101
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        101
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   102
      SSUP_WRPALTHK=0.01                                                  102
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              102
C SUPPORT CONE THICKNESS                                                  103
      SSUP_GRPHTHK=0.16                                                   103
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              103
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              104
      SSUP_CONE1ZMN=52.23                                                 104
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         104
C LENGTH OF SUPPORT RODS                                                  105
      SSUP_RODLEN=110.8                                                   105
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               105
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               106
      SSUP_RODDIST=17.5                                                   106
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           106
C ID OF CARBON SUPPORT RODS (APPROX)                                      107
      SSUP_RODID=2.5                                                      107
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    107
C OD OF CARBON SUPPORT RODS (APPROX)                                      108
      SSUP_RODOD=3.05                                                     108
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    108
C MINIMUM ID OF CONE 1                                                    109
      SSUP_CON1IDMN=15.67                                                 109
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               109
C MINIMUM ID OF CONE 3 (TBD)                                              110
      SSUP_CON3IDMN=21.67                                                 110
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         110
C MINIMUM ID OF CONE 4 (TBD)                                              111
      SSUP_CON4IDMN=37.4                                                  111
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         111
C MAXIMUM ID OF CONE 4 (TBD)                                              112
      SSUP_CON4IDMX=37.4                                                  112
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         112
C MAXIMUM Z OF CONE 3 (TBD)                                               113
      SSUP_CONE3ZMX=150.0                                                 113
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          113
C MAXIMUM Z OF CONE 4 (TBD)                                               114
      SSUP_CONE4ZMX=229.36                                                114
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          114
C THICKNESS OF AL BRACKETS                                                115
      SSUP_BRATHK=.2                                                      115
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             115
C (Z) THICKNESS OF END RING JOINING BRACKETS                              116
      SSUP_ERJTHK=.1                                                      116
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           116
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          117
      SSUP_ERJWID=2.07                                                    117
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       117
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            118
      SSUP_ERJLEN=5.19                                                    118
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         118
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    119
      SSUP_ERJZDIS=2.0                                                    119
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                119
C ERJ SCREW 1 X POSITION (RADIAL)                                         120
      SSUP_ERJ1X=0.31                                                     120
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       120
C ERJ SCREW 2 X POSITION                                                  121
      SSUP_ERJ2X=1.15                                                     121
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                121
C ERJ SCREW 2 Y POSITION                                                  122
      SSUP_ERJ2Y=0.72                                                     122
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                122
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   123
      SSUP_ERJRAD=10.80                                                   123
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                123
C ERJ SCREW DIAMETER                                                      124
      SSUP_ERJDIA=0.17                                                    124
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   124
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    125
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           125
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   125
C *                                                                       125
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    125
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           125
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   125
C *                                                                       126
      CALL AGSBEGM(MODULE,IPRIN)                                          126
      IF (IPRIN.LT.0) RETURN                                              126
C Filling bank SSUB - BEAMPIPE SUPPORT                                    126
      BANK_TITLE='BEAMPIPE SUPPORT'                                       126
      BPATH_SSUB='SSUB*'                                                  126
      IDNUM_SSUB(1)=0                                                     127
C GEOMETRY VERSION                                                        127
      SSUB_VERSION=1                                                      127
      CSSUB_VERSION='GEOMETRY VERSION'                                    127
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  128
      SSUB_KMOUNTID=31.34                                                 128
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             128
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  129
      SSUB_KMOUNTOD=38.96                                                 129
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             129
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    130
      SSUB_KMNTTHK=0.64                                                   130
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                130
C OD OF CUTOUT IN KINEMATIC MOUNT                                         131
      SSUB_KMCUTOD=18.31                                                  131
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     131
C ID OF CUTOUT IN KINEMATIC MOUNT                                         132
      SSUB_KMCUTID=14                                                     132
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     132
C OPENING ANGLE OF CUTOUT                                                 133
      SSUB_KMCUTOA=38                                                     133
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             133
C OFFSET OF CUTOUT CENTER FROM AXIS                                       134
      SSUB_KMCUTOFF=26.58                                                 134
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  134
C ID OF BEAMPIPE SUPPORT RING                                             135
      SSUB_SRINGID=8.47                                                   135
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         135
C OD OF BEAMPIPE SUPPORT RING                                             136
      SSUB_SRINGOD=12.78                                                  136
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         136
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     137
      SSUB_SRINGTHK=0.64                                                  137
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                137
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     138
      SSUB_SRCUTPHI=38                                                    138
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                138
C SUPPORT RING CUTOUT WIDTH                                               139
      SSUB_SRCUTWID=3.63                                                  139
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          139
C SUPPORT RING CUTOUT DEPTH                                               140
      SSUB_SRCUTOUT=5.08                                                  140
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          140
C SUPPORT RING CUTOUT START                                               141
      SSUB_SRCUTIN=3.5                                                    141
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           141
C SUPPORT ROLLER ID                                                       142
      SSUB_SROLLID=0.2                                                    142
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   142
C SUPPORT ROLLER OD                                                       143
      SSUB_SROLLOD=0.62                                                   143
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   143
C SUPPORT ROLLER LENGTH                                                   144
      SSUB_SROLLLEN=2.54                                                  144
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              144
C SUPPORT ROLLER AXIS LENGTH                                              145
      SSUB_SWIRELEN=5.08                                                  145
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         145
C MOUNTING BLOCK HEIGHT                                                   146
      SSUB_MBLKHGH=0.97                                                   146
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               146
C OUTER MOUNTING BLOCK WIDTH                                              147
      SSUB_MBLKOWID=2.54                                                  147
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         147
C OUTER MOUNTING BLOCK LENGTH                                             148
      SSUB_MBLKOLEN=1.27                                                  148
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        148
C INNER MOUNTING BLOCK WIDTH                                              149
      SSUB_MBLKIWID=3.175                                                 149
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         149
C INNER MOUNTING BLOCK LENGTH                                             150
      SSUB_MBLKILEN=1.27                                                  150
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        150
C OUTER MOUNTING BLOCK AT RADIUS                                          151
      SSUB_MBLKORAD=17.4                                                  151
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     151
C INNER MOUNTING BLOCK AT RADIUS                                          152
      SSUB_MBLKIRAD=5.42                                                  152
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     152
C MOUNTING SUPPORT ROD DIAMETER                                           153
      SSUB_MRODDIA=0.32                                                   153
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       153
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    154
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           154
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   154
C *                                                                       154
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    154
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           154
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   154
C *                                                                       155
      CALL AGSBEGM(MODULE,IPRIN)                                          155
      IF (IPRIN.LT.0) RETURN                                              155
C Filling bank SERG - END RINGS                                           155
      BANK_TITLE='END RINGS'                                              155
      BPATH_SERG='SERG*'                                                  155
      IDNUM_SERG(1)=0                                                     156
C GEOMETRY VERSION                                                        156
      SERG_VERSION=1                                                      156
      CSERG_VERSION='GEOMETRY VERSION'                                    156
C INNER END RING TUBE MAXIMUM RADIUS                                      157
      SERG_IRNGTRMX=9.703                                                 157
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 157
C INNER END RING POLYGON MINIMUM RADIUS                                   158
      SERG_IRNGPRMN=7.671                                                 158
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              158
C OUTER END RING MINIMUM RADIUS                                           159
      SERG_ORNGRMIN=11.900                                                159
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      159
C OUTER END RING MAXIMUM RADIUS                                           160
      SERG_ORNGRMAX=13.805                                                160
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      160
C END RING THICKNESS                                                      161
      SERG_ENDRNGTH=0.2                                                   161
      CSERG_ENDRNGTH='END RING THICKNESS'                                 161
C MINIMUM Z FOR END RINGS                                                 162
      SERG_ENDRNGZM=23.01                                                 162
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            162
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    163
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           163
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   163
C *                                                                       163
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    163
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           163
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   163
C *                                                                       164
      CALL AGSBEGM(MODULE,IPRIN)                                          164
      IF (IPRIN.LT.0) RETURN                                              164
C Filling bank SWAM - WATER MANIFOLD                                      164
      BANK_TITLE='WATER MANIFOLD'                                         164
      BPATH_SWAM='SWAM*'                                                  164
      IDNUM_SWAM(1)=0                                                     165
C GEOMETRY VERSION                                                        165
      SWAM_VERSION=1                                                      165
      CSWAM_VERSION='GEOMETRY VERSION'                                    165
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                166
      SWAM_ZMIN=33.9                                                      166
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               166
C WATER MANIFOLD FULL LENGTH                                              167
      SWAM_LEN=16.0                                                       167
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              167
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         168
      SWAM_RMIN=15.24                                                     168
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        168
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         169
      SWAM_RMAX=16.83                                                     169
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        169
C TRANSITION BOARD THICKNESS                                              170
      SWAM_TBRDTHK=0.1                                                    170
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          170
C WATER MANIFOLD WALL THICKNESS                                           171
      SWAM_WALLTHK=0.1                                                    171
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       171
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    172
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           172
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   172
C *                                                                       172
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    172
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           172
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   172
C *                                                                       173
      CALL AGSBEGM(MODULE,IPRIN)                                          173
      IF (IPRIN.LT.0) RETURN                                              173
C Filling bank SELC - ELECTRONICS CARRIER DATA                            173
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               173
      BPATH_SELC='SELC*'                                                  173
      IDNUM_SELC(1)=0                                                     174
C GEOMETRY VERSION                                                        174
      SELC_VERSION=1                                                      174
      CSELC_VERSION='GEOMETRY VERSION'                                    174
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              175
      SELC_BETHK=0.0500                                                   175
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            175
C THICKNESS OF WATER CHANNEL                                              176
      SELC_WATTHK=0.0750                                                  176
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           176
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    177
      SELC_BEOTHK=0.0500                                                  177
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 177
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                178
      SELC_DYETHK=0.0340                                                  178
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             178
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                179
      SELC_DYEWID=0.3                                                     179
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             179
C SPACE IC CHIPS                                                          180
      SELC_DYESPC=0.1                                                     180
      CSELC_DYESPC='SPACE IC CHIPS'                                       180
C ELECTRONICS CARRIER WIDTH                                               181
      SELC_ELCAWID=2.0                                                    181
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           181
C THICKNESS OF AG-PD CONDUCTOR                                            182
      SELC_AGPDTHK=0.0030                                                 182
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        182
C THICKNESS OF GLASS INSULATOR                                            183
      SELC_GLASSTHK=0.0150                                                183
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       183
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        184
      SELC_CABTHK=0.0033                                                  184
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     184
C CABLE WIDTH                                                             185
      SELC_CABWID=0.6                                                     185
      CSELC_CABWID='CABLE WIDTH'                                          185
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    186
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           186
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   186
C *                                                                       186
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    186
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           186
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   186
C *                                                                       187
      CALL AGSBEGM(MODULE,IPRIN)                                          187
      IF (IPRIN.LT.0) RETURN                                              187
C Filling bank SVTL - SINGLE LAYER DATA                                   187
      BANK_TITLE='SINGLE LAYER DATA'                                      187
      BPATH_SVTL='SVTL*'                                                  187
      IDNUM_SVTL(1)=0                                                     188
C LAYER NUMBER                                                            188
      SVTL_LAYER=1                                                        188
      CSVTL_LAYER='LAYER NUMBER'                                          188
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 189
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     189
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              189
C NUMBER OF LADDER                                                        190
      SVTL_NLADDER=4                                                      190
      CSVTL_NLADDER='NUMBER OF LADDER'                                    190
C NUMBER OF WAFERS                                                        191
      SVTL_NWAFER=4                                                       191
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     191
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        192
      SVTL_BAREEDGE=1.0                                                   192
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   192
C PCB LENGTH                                                              193
      SVTL_PCBLEN=14.9                                                    193
      CSVTL_PCBLEN='PCB LENGTH'                                           193
C PCB WIDTH                                                               194
      SVTL_PCBWIDTH=6.3                                                   194
      CSVTL_PCBWIDTH='PCB WIDTH'                                          194
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            195
      SVTL_PCBTHK=0.1                                                     195
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  195
     *R COPPER'                                                           195
C GAP BETWEEN THE PCB AND THE SI WAFER                                    196
      SVTL_PCBGAP=0.2                                                     196
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 196
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    197
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           197
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   197
C *                                                                       197
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    197
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           197
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   197
C *                                                                       198
      CALL AGSBEGM(MODULE,IPRIN)                                          198
      IF (IPRIN.LT.0) RETURN                                              198
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             198
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                198
      BPATH_SVTL='SVTL*'                                                  198
      IDNUM_SVTL(1)=0                                                     199
C LAYER NUMBER                                                            199
      SVTL_LAYER=2                                                        199
      CSVTL_LAYER='LAYER NUMBER'                                          199
C LAYER RADIUS                                                            200
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     200
      CSVTL_RADIUS='LAYER RADIUS'                                         200
C PCB LENGTH                                                              201
      SVTL_PCBLEN=14.9                                                    201
      CSVTL_PCBLEN='PCB LENGTH'                                           201
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    202
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           202
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   202
C *                                                                       202
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    202
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           202
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   202
C *                                                                       203
      CALL AGSBEGM(MODULE,IPRIN)                                          203
      IF (IPRIN.LT.0) RETURN                                              203
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             203
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                203
      BPATH_SVTL='SVTL*'                                                  203
      IDNUM_SVTL(1)=0                                                     204
C LAYER NUMBER                                                            204
      SVTL_LAYER=3                                                        204
      CSVTL_LAYER='LAYER NUMBER'                                          204
C LAYER RADIUS                                                            205
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     205
      CSVTL_RADIUS='LAYER RADIUS'                                         205
C NUMBER OF LADDER                                                        206
      SVTL_NLADDER=6                                                      206
      CSVTL_NLADDER='NUMBER OF LADDER'                                    206
C NUMBER OF WAFERS                                                        207
      SVTL_NWAFER=6                                                       207
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     207
C PCB LENGTH                                                              208
      SVTL_PCBLEN=7.5                                                     208
      CSVTL_PCBLEN='PCB LENGTH'                                           208
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    209
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           209
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   209
C *                                                                       209
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    209
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           209
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   209
C *                                                                       210
      CALL AGSBEGM(MODULE,IPRIN)                                          210
      IF (IPRIN.LT.0) RETURN                                              210
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             210
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                210
      BPATH_SVTL='SVTL*'                                                  210
      IDNUM_SVTL(1)=0                                                     211
C LAYER NUMBER                                                            211
      SVTL_LAYER=4                                                        211
      CSVTL_LAYER='LAYER NUMBER'                                          211
C LAYER RADIUS                                                            212
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     212
      CSVTL_RADIUS='LAYER RADIUS'                                         212
C PCB LENGTH                                                              213
      SVTL_PCBLEN=7.5                                                     213
      CSVTL_PCBLEN='PCB LENGTH'                                           213
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    214
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           214
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   214
C *                                                                       214
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    214
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           214
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   214
C *                                                                       215
      CALL AGSBEGM(MODULE,IPRIN)                                          215
      IF (IPRIN.LT.0) RETURN                                              215
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             215
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                215
      BPATH_SVTL='SVTL*'                                                  215
      IDNUM_SVTL(1)=0                                                     216
C LAYER NUMBER                                                            216
      SVTL_LAYER=5                                                        216
      CSVTL_LAYER='LAYER NUMBER'                                          216
C LAYER RADIUS                                                            217
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     217
      CSVTL_RADIUS='LAYER RADIUS'                                         217
C NUMBER OF LADDER                                                        218
      SVTL_NLADDER=8                                                      218
      CSVTL_NLADDER='NUMBER OF LADDER'                                    218
C NUMBER OF WAFERS                                                        219
      SVTL_NWAFER=7                                                       219
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     219
C PCB LENGTH                                                              220
      SVTL_PCBLEN=4.4                                                     220
      CSVTL_PCBLEN='PCB LENGTH'                                           220
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    221
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           221
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   221
C *                                                                       221
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    221
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           221
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   221
C *                                                                       222
      CALL AGSBEGM(MODULE,IPRIN)                                          222
      IF (IPRIN.LT.0) RETURN                                              222
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             222
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                222
      BPATH_SVTL='SVTL*'                                                  222
      IDNUM_SVTL(1)=0                                                     223
C LAYER NUMBER                                                            223
      SVTL_LAYER=6                                                        223
      CSVTL_LAYER='LAYER NUMBER'                                          223
C LAYER RADIUS                                                            224
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     224
      CSVTL_RADIUS='LAYER RADIUS'                                         224
C PCB LENGTH                                                              225
      SVTL_PCBLEN=4.4                                                     225
      CSVTL_PCBLEN='PCB LENGTH'                                           225
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    226
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           226
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   226
C *                                                                       226
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    226
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           226
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   226
C *                                                                       227
      CALL AGSBEGM(MODULE,IPRIN)                                          227
      IF (IPRIN.LT.0) RETURN                                              227
C Filling bank SFPA - FOURTH LAYER PARAMETERS                             227
      BANK_TITLE='FOURTH LAYER PARAMETERS'                                227
      BPATH_SFPA='SFPA*'                                                  227
      IDNUM_SFPA(1)=0                                                     228
C GEOMETRY VERSION                                                        228
      SFPA_VERSION=1                                                      228
      CSFPA_VERSION='GEOMETRY VERSION'                                    228
C MOTHER RMIN                                                             229
      SFPA_RMIN=22.0                                                      229
      CSFPA_RMIN='MOTHER RMIN'                                            229
C MOTHER RMAX                                                             230
      SFPA_RMAX=28.0                                                      230
      CSFPA_RMAX='MOTHER RMAX'                                            230
C MOTHER LEN                                                              231
      SFPA_LEN=100.                                                       231
      CSFPA_LEN='MOTHER LEN'                                              231
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              232
      SFPA_RAD=23.                                                        232
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              232
C NUMBER OF SILICON STRIP DETECTORS                                       233
      SFPA_NSSD=16                                                        233
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      233
C DETECTOR MOTHER WIDTH                                                   234
      SFPA_DMWID=7.5                                                      234
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 234
C DETECTOR MOTHER THICKNESS                                               235
      SFPA_DMTHK=0.03                                                     235
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             235
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          236
      SFPA_DMLEN=90.                                                      236
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        236
C STRUCTURE MOTHER WIDTH                                                  237
      SFPA_SMWID=7.5                                                      237
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                237
C STRUCTURE MOTHER THICKNESS                                              238
      SFPA_SMTHK=3.5                                                      238
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            238
C STRUCTURE MOTHER LENGTH (COOL. PIPE+CARBON FIBER)                       239
      SFPA_SMLEN=95.                                                      239
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH (COOL. PIPE+CARBON FIBER)'     239
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  240
      SFPA_SSLEN=95./20.                                                  240
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                240
C LENGTH OF WAFER PACK                                                    241
      SFPA_WPLEN=68.8                                                     241
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  241
C LENGHT OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          242
      SFPA_SDLEN=4.2                                                      242
      CSFPA_SDLEN='LENGHT OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        242
C TILING ANGLE (DEGREES)                                                  243
      SFPA_TILT=5.0                                                       243
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 243
C COOLING PIPE OUTER RADIUS                                               244
      SFPA_CPRAD=0.1                                                      244
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             244
C COOLING PIPE INNER RADIUS                                               245
      SFPA_CPRAL=0.09                                                     245
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             245
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            246
      SFPA_CFRAD=0.1                                                      246
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          246
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               247
      SFPA_GPTHK=0.5                                                      247
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             247
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    248
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           248
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   248
C *                                                                       248
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    248
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           248
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   248
C *                                                                       249
      CALL AGSBEGM(MODULE,IPRIN)                                          249
      IF (IPRIN.LT.0) RETURN                                              249
C Filling bank SSLD - SHIELDING PARAMETERS                                249
      BANK_TITLE='SHIELDING PARAMETERS'                                   249
      BPATH_SSLD='SSLD*'                                                  249
      IDNUM_SSLD(1)=0                                                     250
C GEOMETRY VERSION                                                        250
      SSLD_VERSION=1                                                      250
      CSSLD_VERSION='GEOMETRY VERSION'                                    250
C INNER SHIELD CYLINDER, INNER RADIUS                                     251
      SSLD_SINRINN=5                                                      251
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 251
C INNER SHIELD CYLINDER, OUTER RADIUS                                     252
      SSLD_SINROUT=5.008                                                  252
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 252
C INNER SHIELD CYLINDER, HALF LENGTH                                      253
      SSLD_SINLEN=53.5                                                    253
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   253
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                254
      SSLD_SSEPRINN=22                                                    254
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           254
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                255
      SSLD_SSEPROUT=22.018                                                255
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           255
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 256
      SSLD_SSEPLEN=55.4                                                   256
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             256
C OUTER SHIELD CYLINDER, INNER RADIUS                                     257
      SSLD_SOUTRINN=28                                                    257
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                257
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     258
      SSLD_SOUTROUT=28.02                                                 258
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                258
C OUTER SHIELD CYLINDER, HALF LENGTH                                      259
      SSLD_SOUTLEN=65.4                                                   259
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  259
C ALUMINUM SHIELD MESH INNER DIAMETER                                     260
      SSLD_ALMESHID=9.7                                                   260
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                260
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     261
      SSLD_ALMESHOD=44                                                    261
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                261
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                262
      SSLD_ALMSHTHK=0.03                                                  262
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           262
C ALUMINUM SHIELD MESH Z POSITION                                         263
      SSLD_ALMSHPOS=53.5                                                  263
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    263
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    264
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           264
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   264
C *                                                                       264
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    264
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           264
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   264
C *                                                                       265
      CALL AGSBEGM(MODULE,IPRIN)                                          265
      IF (IPRIN.LT.0) RETURN                                              265
C Filling bank SSLD - SHIELDING PARAMETERS                                265
      BANK_TITLE='SHIELDING PARAMETERS'                                   265
      BPATH_SSLD='SSLD*'                                                  265
      IDNUM_SSLD(1)=0                                                     266
C GEOMETRY VERSION                                                        266
      SSLD_VERSION=2                                                      266
      CSSLD_VERSION='GEOMETRY VERSION'                                    266
C INNER SHIELD CYLINDER, INNER RADIUS                                     267
      SSLD_SINRINN=5.7                                                    267
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 267
C INNER SHIELD CYLINDER, OUTER RADIUS                                     268
      SSLD_SINROUT=5.708                                                  268
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 268
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    269
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           269
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   269
C *                                                                       269
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    269
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           269
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   269
C *                                                                       270
      DO 5011 I=1,4                                                       270
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 271
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 272
5011  CONTINUE                                                            273
5012  CONTINUE                                                            273
C *                                                                       274
      CALL AGSBEGM(MODULE,IPRIN)                                          274
      IF (IPRIN.LT.0) RETURN                                              274
C Filling bank SCBP - CABLING                                             274
      BANK_TITLE='CABLING'                                                274
      BPATH_SCBP='SCBP*'                                                  274
      IDNUM_SCBP(1)=0                                                     275
C LAYER                                                                   275
      SCBP_LAYER=1                                                        275
      CSCBP_LAYER='LAYER'                                                 275
C LENGTH                                                                  276
      SCBP_LEN=1.85                                                       276
      CSCBP_LEN='LENGTH'                                                  276
C MIN RADIUS CLOSER TO WAFERS                                             277
      SCBP_RMIN1=RAD_CONES_IN(1)                                          277
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           277
C MIN RADIUS FURTHER FROM WAFERS                                          278
      SCBP_RMIN2=RAD_CONES_OUT(1)                                         278
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        278
C VOLUME OF COPPER, LV+HV CABLES                                          279
      SCBP_VOL=7.24+3.21                                                  279
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          279
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    280
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           280
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   280
C *                                                                       280
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    280
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           280
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   280
C *                                                                       281
      CALL AGSBEGM(MODULE,IPRIN)                                          281
      IF (IPRIN.LT.0) RETURN                                              281
C Filling bank SCBP - CABLING                                             281
      BANK_TITLE='CABLING'                                                281
      BPATH_SCBP='SCBP*'                                                  281
      IDNUM_SCBP(1)=0                                                     282
C LAYER                                                                   282
      SCBP_LAYER=2                                                        282
      CSCBP_LAYER='LAYER'                                                 282
C MIN RADIUS CLOSER TO WAFERS                                             283
      SCBP_RMIN1=RAD_CONES_IN(2)                                          283
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           283
C MIN RADIUS FURTHER FROM WAFERS                                          284
      SCBP_RMIN2=RAD_CONES_OUT(2)                                         284
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        284
C VOLUME OF COPPER, LV+HV CABLES                                          285
      SCBP_VOL=15.54+5.7                                                  285
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          285
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    286
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           286
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   286
C *                                                                       286
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    286
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           286
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   286
C *                                                                       287
      CALL AGSBEGM(MODULE,IPRIN)                                          287
      IF (IPRIN.LT.0) RETURN                                              287
C Filling bank SCBP - CABLING                                             287
      BANK_TITLE='CABLING'                                                287
      BPATH_SCBP='SCBP*'                                                  287
      IDNUM_SCBP(1)=0                                                     288
C LAYER                                                                   288
      SCBP_LAYER=3                                                        288
      CSCBP_LAYER='LAYER'                                                 288
C MIN RADIUS CLOSER TO WAFERS                                             289
      SCBP_RMIN1=RAD_CONES_IN(3)                                          289
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           289
C MIN RADIUS FURTHER FROM WAFERS                                          290
      SCBP_RMIN2=RAD_CONES_OUT(3)                                         290
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        290
C VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE                   291
      SCBP_VOL=4.05+2.02+3.67+1.69                                        291
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE'   291
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    292
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           292
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   292
C *                                                                       292
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    292
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           292
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   292
C *                                                                       293
      CALL AGSBEGM(MODULE,IPRIN)                                          293
      IF (IPRIN.LT.0) RETURN                                              293
C Filling bank SCBP - CABLING                                             293
      BANK_TITLE='CABLING'                                                293
      BPATH_SCBP='SCBP*'                                                  293
      IDNUM_SCBP(1)=0                                                     294
C LAYER (FORMER 5TH)                                                      294
      SCBP_LAYER=4                                                        294
      CSCBP_LAYER='LAYER (FORMER 5TH)'                                    294
C MIN RADIUS CLOSER TO WAFERS                                             295
      SCBP_RMIN1=RAD_CONES_IN(4)                                          295
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           295
C MIN RADIUS FURTHER FROM WAFERS                                          296
      SCBP_RMIN2=RAD_CONES_OUT(4)                                         296
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        296
C VOLUME OF COPPER, LV+HV CABLES                                          297
      SCBP_VOL=6.95+2.43                                                  297
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          297
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    298
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           298
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   298
C *                                                                       298
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    298
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           298
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   298
C *                                                                       299
      CALL AGSBEGM(MODULE,IPRIN)                                          299
      IF (IPRIN.LT.0) RETURN                                              299
C Filling bank SFEP - WATER FEED                                          299
      BANK_TITLE='WATER FEED'                                             299
      BPATH_SFEP='SFEP*'                                                  299
      IDNUM_SFEP(1)=0                                                     300
C LAYER                                                                   300
      SFEP_LAYER=1                                                        300
      CSFEP_LAYER='LAYER'                                                 300
C LENGTH                                                                  301
      SFEP_LEN=1.85                                                       301
      CSFEP_LEN='LENGTH'                                                  301
C MIN RADIUS CLOSER TO WAFERS                                             302
      SFEP_RMIN1=0.5*(RAD_CONES_IN(1) +RAD_CONES_IN(2))                   302
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           302
C MIN RADIUS FURTHER FROM WAFERS                                          303
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(1)+RAD_CONES_OUT(2))                  303
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        303
C VOLUME OF WATER                                                         304
      SFEP_VOL=16.0                                                       304
      CSFEP_VOL='VOLUME OF WATER'                                         304
C VOLUME OF PLASTIC                                                       305
      SFEP_VOLPLAST=38.4                                                  305
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  305
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    306
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           306
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   306
C *                                                                       306
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    306
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           306
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   306
C *                                                                       307
      CALL AGSBEGM(MODULE,IPRIN)                                          307
      IF (IPRIN.LT.0) RETURN                                              307
C Filling bank SFEP - WATER FEED                                          307
      BANK_TITLE='WATER FEED'                                             307
      BPATH_SFEP='SFEP*'                                                  307
      IDNUM_SFEP(1)=0                                                     308
C LAYER                                                                   308
      SFEP_LAYER=2                                                        308
      CSFEP_LAYER='LAYER'                                                 308
C MIN RADIUS CLOSER TO WAFERS                                             309
      SFEP_RMIN1=0.5*(RAD_CONES_IN(2) +RAD_CONES_IN(3))                   309
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           309
C MIN RADIUS FURTHER FROM WAFERS                                          310
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(2)+RAD_CONES_OUT(3))                  310
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        310
C VOLUME OF WATER                                                         311
      SFEP_VOL=24.0                                                       311
      CSFEP_VOL='VOLUME OF WATER'                                         311
C VOLUME OF PLASTIC                                                       312
      SFEP_VOLPLAST=57.6                                                  312
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  312
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    313
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           313
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   313
C *                                                                       313
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    313
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           313
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   313
C *                                                                       314
      CALL AGSBEGM(MODULE,IPRIN)                                          314
      IF (IPRIN.LT.0) RETURN                                              314
C Filling bank SFEP - WATER FEED                                          314
      BANK_TITLE='WATER FEED'                                             314
      BPATH_SFEP='SFEP*'                                                  314
      IDNUM_SFEP(1)=0                                                     315
C LAYER                                                                   315
      SFEP_LAYER=3                                                        315
      CSFEP_LAYER='LAYER'                                                 315
C MIN RADIUS CLOSER TO WAFERS                                             316
      SFEP_RMIN1=0.5*(RAD_CONES_IN(3) +RAD_CONES_IN(4))                   316
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           316
C MIN RADIUS FURTHER FROM WAFERS                                          317
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(3)+RAD_CONES_OUT(4))                  317
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        317
C VOLUME OF WATER                                                         318
      SFEP_VOL=32                                                         318
      CSFEP_VOL='VOLUME OF WATER'                                         318
C VOLUME OF PLASTIC                                                       319
      SFEP_VOLPLAST=76.8                                                  319
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  319
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    320
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           320
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   320
C *                                                                       320
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    320
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           320
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   320
C *                                                                       321
      CALL AGSBEGM(MODULE,IPRIN)                                          321
      IF (IPRIN.LT.0) RETURN                                              321
C Filling bank SWCX - SEGMENTS OF THE WATER DISTRIBUTION PIPES            321
      BANK_TITLE='SEGMENTS OF THE WATER DISTRIBUTION PIPES'               321
      BPATH_SWCX='SWCX*'                                                  321
      IDNUM_SWCX(1)=0                                                     322
C VERSION                                                                 322
      SWCX_LAYER=1                                                        322
      CSWCX_LAYER='VERSION'                                               322
C OF THE RING IN THE Z DIRECTION                                          323
      SWCX_LENGTH=2.8                                                     323
      CSWCX_LENGTH='OF THE RING IN THE Z DIRECTION'                       323
C THICKNESS OF THE MOTHER LAYER                                           324
      SWCX_DR=0.72                                                        324
      CSWCX_DR='THICKNESS OF THE MOTHER LAYER'                            324
C INNER PLASTIC TUBE RADIUS                                               325
      SWCX_RAD=0.2                                                        325
      CSWCX_RAD='INNER PLASTIC TUBE RADIUS'                               325
C FROM THE EDGE OF THE LADDER SUPPORT, INWARD                             326
      SWCX_OFFSET=-2.0                                                    326
      CSWCX_OFFSET='FROM THE EDGE OF THE LADDER SUPPORT, INWARD'          326
C RADIAL OFFSET                                                           327
      SWCX_ROFFSET=1.0                                                    327
      CSWCX_ROFFSET='RADIAL OFFSET'                                       327
C THICKNESS OF THE PLASTIC PIPE WALL                                      328
      SWCX_WALL=0.16                                                      328
      CSWCX_WALL='THICKNESS OF THE PLASTIC PIPE WALL'                     328
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    329
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           329
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   329
C *                                                                       329
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    329
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           329
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   329
C *                                                                       330
      CALL AGSBEGM(MODULE,IPRIN)                                          330
      IF (IPRIN.LT.0) RETURN                                              330
C Filling bank SOUP - MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS      330
      BANK_TITLE='MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS'         330
      BPATH_SOUP='SOUP*'                                                  330
      IDNUM_SOUP(1)=0                                                     331
C VERSION                                                                 331
      SOUP_VERSION=1                                                      331
      CSOUP_VERSION='VERSION'                                             331
C LENGTH                                                                  332
      SOUP_LENGTH=82.5                                                    332
      CSOUP_LENGTH='LENGTH'                                               332
C OUTER RADIUS OF THE SHIELD                                              333
      SOUP_ROUT=19.41                                                     333
      CSOUP_ROUT='OUTER RADIUS OF THE SHIELD'                             333
C DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF THE MOTHER)     334
      SOUP_DR=0.711                                                       334
      CSOUP_DR='DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF   334
     *THE MOTHER)'                                                        334
C STARTING ANGLE OF THE MOTHER                                            335
      SOUP_PHI1=0.0                                                       335
      CSOUP_PHI1='STARTING ANGLE OF THE MOTHER'                           335
C FINISHING ANGLE OF THE MOTHER                                           336
      SOUP_PHI2=70.0                                                      336
      CSOUP_PHI2='FINISHING ANGLE OF THE MOTHER'                          336
C OUTER DIAM OF THE CARBON TUBE                                           337
      SOUP_DIAMOUT=0.711                                                  337
      CSOUP_DIAMOUT='OUTER DIAM OF THE CARBON TUBE'                       337
C INNER DIAM OF THE CARBON TUBE                                           338
      SOUP_DIAMIN=0.620                                                   338
      CSOUP_DIAMIN='INNER DIAM OF THE CARBON TUBE'                        338
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    339
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           339
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   339
C *                                                                       339
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    339
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           339
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   339
C use default SVTG bank                                                   340
      BPATH_SVTG='SVTG*'                                                  340
      IDNUM_SVTG(1)=0                                                     340
      CALL AGDGETP('SVTTGEO2','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,  340
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      340
     *LINK_SVTG,FLAG_SVTG)                                                340
C use default SWCA bank                                                   341
      BPATH_SWCA='SWCA*'                                                  341
      IDNUM_SWCA(1)=0                                                     341
      CALL AGDGETP('SVTTGEO2','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,  341
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      341
     *LINK_SWCA,FLAG_SWCA)                                                341
C use default SELC bank                                                   342
      BPATH_SELC='SELC*'                                                  342
      IDNUM_SELC(1)=0                                                     342
      CALL AGDGETP('SVTTGEO2','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  342
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      342
     *LINK_SELC,FLAG_SELC)                                                342
C use default SSUP bank                                                   343
      BPATH_SSUP='SSUP*'                                                  343
      IDNUM_SSUP(1)=0                                                     343
      CALL AGDGETP('SVTTGEO2','*',-999,'SSUP',' ',BANK_SSUP, BPATH_SSUP,  343
     *IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,BANK_SSUP,LEN_SSUP,LLEN_SSUP,      343
     *LINK_SSUP,FLAG_SSUP)                                                343
C use default SSUB bank                                                   344
      BPATH_SSUB='SSUB*'                                                  344
      IDNUM_SSUB(1)=0                                                     344
      CALL AGDGETP('SVTTGEO2','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,  344
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      344
     *LINK_SSUB,FLAG_SSUB)                                                344
C use default SERG bank                                                   345
      BPATH_SERG='SERG*'                                                  345
      IDNUM_SERG(1)=0                                                     345
      CALL AGDGETP('SVTTGEO2','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,  345
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      345
     *LINK_SERG,FLAG_SERG)                                                345
C use default SWAM bank                                                   346
      BPATH_SWAM='SWAM*'                                                  346
      IDNUM_SWAM(1)=0                                                     346
      CALL AGDGETP('SVTTGEO2','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,  346
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      346
     *LINK_SWAM,FLAG_SWAM)                                                346
C use default SELC bank                                                   347
      BPATH_SELC='SELC*'                                                  347
      IDNUM_SELC(1)=0                                                     347
      CALL AGDGETP('SVTTGEO2','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  347
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      347
     *LINK_SELC,FLAG_SELC)                                                347
C use default SFPA bank                                                   348
      BPATH_SFPA='SFPA*'                                                  348
      IDNUM_SFPA(1)=0                                                     348
      CALL AGDGETP('SVTTGEO2','*',-999,'SFPA',' ',BANK_SFPA, BPATH_SFPA,  348
     *IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,BANK_SFPA,LEN_SFPA,LLEN_SFPA,      348
     *LINK_SFPA,FLAG_SFPA)                                                348
C use SSLD bank                                                           349
      BPATH_SSLD='SSLD*'                                                  349
      IDNUM_SSLD(1)=0                                                     349
      SSLD_VERSION=SVTG_SUPPORTVER                                        349
      CALL AGDGETP('SVTTGEO2','*',-999,'SSLD',NSSLD_VERSION,              349
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        349
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   349
C use default SCBP bank                                                   350
      BPATH_SCBP='SCBP*'                                                  350
      IDNUM_SCBP(1)=0                                                     350
      CALL AGDGETP('SVTTGEO2','*',-999,'SCBP',' ',BANK_SCBP, BPATH_SCBP,  350
     *IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,LLEN_SCBP,      350
     *LINK_SCBP,FLAG_SCBP)                                                350
C use default SWCX bank                                                   351
      BPATH_SWCX='SWCX*'                                                  351
      IDNUM_SWCX(1)=0                                                     351
      CALL AGDGETP('SVTTGEO2','*',-999,'SWCX',' ',BANK_SWCX, BPATH_SWCX,  351
     *IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,LLEN_SWCX,      351
     *LINK_SWCX,FLAG_SWCX)                                                351
C use default SOUP bank                                                   352
      BPATH_SOUP='SOUP*'                                                  352
      IDNUM_SOUP(1)=0                                                     352
      CALL AGDGETP('SVTTGEO2','*',-999,'SOUP',' ',BANK_SOUP, BPATH_SOUP,  352
     *IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,LLEN_SOUP,      352
     *LINK_SOUP,FLAG_SOUP)                                                352
C Check SVTG_SUPPORTVER==2                                                353
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      353
      SVTG_RSIZEMIN = 1.4                                                 354
      ENDIF                                                               355
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             356
      CALL AGSRESET                                                       356
      AG_A = 28.08                                                        356
      AG_Z = 14                                                           356
      AG_W = 0.6*1*28./60.                                                356
      AG_TITLE = 'SI'                                                     356
      AG_EXNAME = 'COMPONENT'                                             356
      AG_PARLIST = 'A_Z_W'                                                356
      CALL AXCOMPONENT                                                    356
C *                                                                       356
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  356
C =                                                                       356
      CALL AGSRESET                                                       357
      AG_A = 16                                                           357
      AG_Z = 8                                                            357
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               357
      AG_TITLE = 'O'                                                      357
      AG_EXNAME = 'COMPONENT'                                             357
      AG_PARLIST = 'A_Z_W'                                                357
      CALL AXCOMPONENT                                                    357
C *                                                                       357
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 357
      CALL AGSRESET                                                       358
      AG_A = 12                                                           358
      AG_Z = 6                                                            358
      AG_W = 0.4*8*12./174.                                               358
      AG_TITLE = 'C'                                                      358
      AG_EXNAME = 'COMPONENT'                                             358
      AG_PARLIST = 'A_Z_W'                                                358
      CALL AXCOMPONENT                                                    358
C *                                                                       358
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  358
      CALL AGSRESET                                                       359
      AG_A = 1                                                            359
      AG_Z = 1                                                            359
      AG_W = 0.4*14*1./174.                                               359
      AG_TITLE = 'H'                                                      359
      AG_EXNAME = 'COMPONENT'                                             359
      AG_PARLIST = 'A_Z_W'                                                359
      CALL AXCOMPONENT                                                    359
C *                                                                       359
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  359
      CALL AGSRESET                                                       360
      AG_DENS = 1.7                                                       360
      AG_TITLE = 'G10'                                                    360
      AG_EXNAME = 'MIXTURE'                                               360
      AG_PARLIST = 'DENS'                                                 360
      CALL AXMIXTURE                                                      360
C *                                                                       360
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       360
      CALL AGSRESET                                                       361
      AG_A = 12                                                           361
      AG_Z = 6                                                            361
      AG_W = 10000*5                                                      361
      AG_TITLE = 'C5'                                                     361
      AG_EXNAME = 'COMPONENT'                                             361
      AG_PARLIST = 'A_Z_W'                                                361
      CALL AXCOMPONENT                                                    361
C *                                                                       361
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        361
      CALL AGSRESET                                                       362
      AG_A = 1                                                            362
      AG_Z = 1                                                            362
      AG_W = 10000*4                                                      362
      AG_TITLE = 'H4'                                                     362
      AG_EXNAME = 'COMPONENT'                                             362
      AG_PARLIST = 'A_Z_W'                                                362
      CALL AXCOMPONENT                                                    362
C *                                                                       362
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       362
      CALL AGSRESET                                                       363
      AG_A = 16                                                           363
      AG_Z = 8                                                            363
      AG_W = 10000*2                                                      363
      AG_TITLE = 'O2'                                                     363
      AG_EXNAME = 'COMPONENT'                                             363
      AG_PARLIST = 'A_Z_W'                                                363
      CALL AXCOMPONENT                                                    363
C *                                                                       363
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 363
      CALL AGSRESET                                                       364
      AG_A = 27                                                           364
      AG_Z = 13                                                           364
      AG_W = 10000*0.2302                                                 364
      AG_TITLE = 'AL'                                                     364
      AG_EXNAME = 'COMPONENT'                                             364
      AG_PARLIST = 'A_Z_W'                                                364
      CALL AXCOMPONENT                                                    364
C *                                                                       364
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              364
      CALL AGSRESET                                                       365
      AG_DENS = 1.432                                                     365
      AG_TITLE = 'ALKAP'                                                  365
      AG_EXNAME = 'MIXTURE'                                               365
      AG_PARLIST = 'DENS'                                                 365
      CALL AXMIXTURE                                                      365
C *                                                                       365
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       365
      CALL AGSRESET                                                       366
      AG_A = 12                                                           366
      AG_Z = 6                                                            366
      AG_W = 10000*5                                                      366
      AG_TITLE = 'C5'                                                     366
      AG_EXNAME = 'COMPONENT'                                             366
      AG_PARLIST = 'A_Z_W'                                                366
      CALL AXCOMPONENT                                                    366
C *                                                                       366
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        366
      CALL AGSRESET                                                       367
      AG_A = 1                                                            367
      AG_Z = 1                                                            367
      AG_W = 10000*4                                                      367
      AG_TITLE = 'H4'                                                     367
      AG_EXNAME = 'COMPONENT'                                             367
      AG_PARLIST = 'A_Z_W'                                                367
      CALL AXCOMPONENT                                                    367
C *                                                                       367
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       367
      CALL AGSRESET                                                       368
      AG_A = 16                                                           368
      AG_Z = 8                                                            368
      AG_W = 10000*2                                                      368
      AG_TITLE = 'O2'                                                     368
      AG_EXNAME = 'COMPONENT'                                             368
      AG_PARLIST = 'A_Z_W'                                                368
      CALL AXCOMPONENT                                                    368
C *                                                                       368
C Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                          368
      CALL AGSRESET                                                       369
      AG_A = 27                                                           369
      AG_Z = 13                                                           369
      AG_W = 986                                                          369
      AG_TITLE = 'AL'                                                     369
      AG_EXNAME = 'COMPONENT'                                             369
      AG_PARLIST = 'A_Z_W'                                                369
      CALL AXCOMPONENT                                                    369
C *                                                                       369
C Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                          369
      CALL AGSRESET                                                       370
      AG_DENS = 1.40845                                                   370
      AG_TITLE = 'SSDALMY'                                                370
      AG_EXNAME = 'MIXTURE'                                               370
      AG_PARLIST = 'DENS'                                                 370
      CALL AXMIXTURE                                                      370
C *                                                                       370
C Check SWAM_LEN>0                                                        370
      IF (SWAM_LEN.GT.0) THEN                                             371
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              372
      CALL AGSRESET                                                       372
      AG_A = 1                                                            372
      AG_Z = 1                                                            372
      AG_W = 2                                                            372
      AG_TITLE = 'H2'                                                     372
      AG_EXNAME = 'COMPONENT'                                             372
      AG_PARLIST = 'A_Z_W'                                                372
      CALL AXCOMPONENT                                                    372
C *                                                                       372
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              372
      CALL AGSRESET                                                       373
      AG_A = 16                                                           373
      AG_Z = 8                                                            373
      AG_W = 1                                                            373
      AG_TITLE = 'O'                                                      373
      AG_EXNAME = 'COMPONENT'                                             373
      AG_PARLIST = 'A_Z_W'                                                373
      CALL AXCOMPONENT                                                    373
C *                                                                       373
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                373
      CALL AGSRESET                                                       374
      AG_DENS = 1.0                                                       374
      AG_TITLE = 'WATER'                                                  374
      AG_EXNAME = 'MIXTURE'                                               374
      AG_PARLIST = 'DENS'                                                 374
      CALL AXMIXTURE                                                      374
C *                                                                       374
      ELSE                                                                374
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             376
      CALL AGSRESET                                                       376
      AG_DENS = 0.0009                                                    376
      AG_TITLE = 'WATER'                                                  376
      AG_EXNAME = 'MIXTURE'                                               376
      AG_PARLIST = 'DENS'                                                 376
      CALL AXMIXTURE                                                      376
C *                                                                       376
      ENDIF                                                               376
      CALL AGSBEGM(MODULE,IPRIN)                                          378
C Check IPRIN<0                                                           378
      IF (IPRIN.LT.0) RETURN                                              378
C Create SVTT :                                                           378
C Call To Local Procedure SVTT                                            378
      ASSIGN 5030 TO BLOCK_SVTT                                           378
      GO TO  5020                                                         378
5030  CONTINUE                                                            378
C *                                                                       378
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           379
      CALL AGSRESET                                                       379
      AG_MOTHER = 'CAVE'                                                  379
      AG_TITLE = 'SVTT'                                                   379
      AG_EXNAME = 'POSITION'                                              379
      AG_PARLIST = 'MOTHER'                                               379
      CALL AXPOSITION                                                     379
C *                                                                       379
C ----------------------------------------------------------------------  379
C ------------ SVTT                                                       379
      CALL AGSENDM                                                        381
      RETURN                                                              381
C Begin Local Procedure SVTT                                              381
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   381
5020  CONTINUE                                                            381
         AG_TITLE='SVTT'                                                  381
         AG_MODULE=MODULE                                                 381
         AG_MARK='V00'                                                    381
         CALL AGSPUSH                                                     381
         AG_MODULE=MODULE                                                 381
C    *                                                                    381
         RSIZEMAX=SSUP_CON4IDMX                                           382
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 383
     *   2.0*SSUP_HOSRMX                                                  383
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                384
         CLEARANCE=SVTG_RSIZEMAX - RSIZEMAX                               385
C    Geant MATERIAL : AIR ===> inherited arguments used <===              386
         CALL AGSRESET                                                    386
         AG_TITLE = 'AIR'                                                 386
         AG_EXNAME = 'MATERIAL'                                           386
         AG_PARLIST = 'NONE'                                              386
         CALL AXMATERIAL                                                  386
C    *                                                                    386
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       386
         CALL AGSRESET                                                    387
         AG_SEEN = 0                                                      387
         AG_COLO = 1                                                      387
         AG_TITLE = 'SVTT'                                                387
         AG_EXNAME = 'ATTRIBUTE'                                          387
         AG_PARLIST = 'SEEN_COLO'                                         387
         CALL AXATTRIBUTE                                                 387
C    *                                                                    387
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  387
C    VTG_ZSIZEMAX <===                                                    387
         CALL AGSRESET                                                    389
         AG_RMIN = SVTG_RSIZEMIN                                          389
         AG_RMAX = SVTG_RSIZEMAX                                          389
         AG_DZ = SVTG_ZSIZEMAX                                            389
         AG_TITLE = 'TUBE'                                                389
         AG_EXNAME = 'SHAPE'                                              389
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      389
         CALL AXSHAPE                                                     389
C    *                                                                    389
C    Create SIRP :                                                        390
C    Call To Local Procedure SIRP                                         390
         ASSIGN 5050 TO BLOCK_SIRP                                        390
         GO TO  5040                                                      390
5050     CONTINUE                                                         390
C    *                                                                    390
C    Geant POSITION : SIRP ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  391
C    2.5 <===                                                             391
         CALL AGSRESET                                                    391
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             391
         AG_ALPHAN=AG_ALPHAN+1                                            391
         AG_ALPHAK(AG_ALPHAN)=3                                           391
         AG_ALPHAL(AG_ALPHAN)=22.5                                        391
         AG_TITLE = 'SIRP'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'Z_ALPHAZ'                                          391
         CALL AXPOSITION                                                  391
C    *                                                                    391
C    Geant POSITION : SIRP ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  391
C    22.5 <===                                                            391
         CALL AGSRESET                                                    392
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            392
         AG_ALPHAN=AG_ALPHAN+1                                            392
         AG_ALPHAK(AG_ALPHAN)=3                                           392
         AG_ALPHAL(AG_ALPHAN)=22.5                                        392
         AG_TITLE = 'SIRP'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'Z_ALPHAZ'                                          392
         CALL AXPOSITION                                                  392
C    *                                                                    392
C    Create SIRT :                                                        393
C    Call To Local Procedure SIRT                                         393
         ASSIGN 5070 TO BLOCK_SIRT                                        393
         GO TO  5060                                                      393
5070     CONTINUE                                                         393
C    *                                                                    393
C    Geant POSITION : SIRT ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  394
C    2.5 <===                                                             394
         CALL AGSRESET                                                    394
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             394
         AG_ALPHAN=AG_ALPHAN+1                                            394
         AG_ALPHAK(AG_ALPHAN)=3                                           394
         AG_ALPHAL(AG_ALPHAN)=22.5                                        394
         AG_TITLE = 'SIRT'                                                394
         AG_EXNAME = 'POSITION'                                           394
         AG_PARLIST = 'Z_ALPHAZ'                                          394
         CALL AXPOSITION                                                  394
C    *                                                                    394
C    Geant POSITION : SIRT ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  394
C    22.5 <===                                                            394
         CALL AGSRESET                                                    395
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            395
         AG_ALPHAN=AG_ALPHAN+1                                            395
         AG_ALPHAK(AG_ALPHAN)=3                                           395
         AG_ALPHAL(AG_ALPHAN)=22.5                                        395
         AG_TITLE = 'SIRT'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'Z_ALPHAZ'                                          395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Create SOER :                                                        396
C    Call To Local Procedure SOER                                         396
         ASSIGN 5090 TO BLOCK_SOER                                        396
         GO TO  5080                                                      396
5090     CONTINUE                                                         396
C    *                                                                    396
C    Geant POSITION : SOER ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 <===      397
         CALL AGSRESET                                                    397
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             397
         AG_TITLE = 'SOER'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'Z'                                                 397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Geant POSITION : SOER ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 <===     397
         CALL AGSRESET                                                    398
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            398
         AG_TITLE = 'SOER'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'Z'                                                 398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Create SBRG :                                                        399
C    Call To Local Procedure SBRG                                         399
         ASSIGN 5110 TO BLOCK_SBRG                                        399
         GO TO  5100                                                      399
5110     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  400
C    /2 <===                                                              400
         CALL AGSRESET                                                    400
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  400
         AG_TITLE = 'SBRG'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'Z'                                                 400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  400
C    K/2 <===                                                             400
         CALL AGSRESET                                                    401
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 401
         AG_TITLE = 'SBRG'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'Z'                                                 401
         CALL AXPOSITION                                                  401
C    *                                                                    401
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           401
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             403
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      404
C    Create SOES :                                                        405
C    Call To Local Procedure SOES                                         405
         ASSIGN 5130 TO BLOCK_SOES                                        405
         GO TO  5120                                                      405
5130     CONTINUE                                                         405
C    *                                                                    405
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               406
         CALL AGSRESET                                                    406
         AG_Z = SCREW_Z                                                   406
         AG_KONLY = 'MANY'                                                406
         AG_TITLE = 'SOES'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'Z_KONLY'                                           406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              406
         CALL AGSRESET                                                    407
         AG_Z = -SCREW_Z                                                  407
         AG_KONLY = 'MANY'                                                407
         AG_TITLE = 'SOES'                                                407
         AG_EXNAME = 'POSITION'                                           407
         AG_PARLIST = 'Z_KONLY'                                           407
         CALL AXPOSITION                                                  407
C    *                                                                    407
C    Create SIES :                                                        408
C    Call To Local Procedure SIES                                         408
         ASSIGN 5150 TO BLOCK_SIES                                        408
         GO TO  5140                                                      408
5150     CONTINUE                                                         408
C    *                                                                    408
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               409
         CALL AGSRESET                                                    409
         AG_Z = SCREW_Z                                                   409
         AG_KONLY = 'MANY'                                                409
         AG_TITLE = 'SIES'                                                409
         AG_EXNAME = 'POSITION'                                           409
         AG_PARLIST = 'Z_KONLY'                                           409
         CALL AXPOSITION                                                  409
C    *                                                                    409
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              409
         CALL AGSRESET                                                    410
         AG_Z = -SCREW_Z                                                  410
         AG_KONLY = 'MANY'                                                410
         AG_TITLE = 'SIES'                                                410
         AG_EXNAME = 'POSITION'                                           410
         AG_PARLIST = 'Z_KONLY'                                           410
         CALL AXPOSITION                                                  410
C    *                                                                    410
C    Check SWAM_LEN>0                                                     410
         IF (SWAM_LEN.GT.0) THEN                                          411
C    Create SWMM :                                                        412
C    Call To Local Procedure SWMM                                         412
         ASSIGN 5170 TO BLOCK_SWMM                                        412
         GO TO  5160                                                      412
5170     CONTINUE                                                         412
C    *                                                                    412
C    Geant POSITION : SWMM ===> Z=SWAM_ZMIN+SWAM_LEN/2 <===               413
         CALL AGSRESET                                                    413
         AG_Z = SWAM_ZMIN+SWAM_LEN/2                                      413
         AG_TITLE = 'SWMM'                                                413
         AG_EXNAME = 'POSITION'                                           413
         AG_PARLIST = 'Z'                                                 413
         CALL AXPOSITION                                                  413
C    *                                                                    413
C    Geant POSITION : SWMM ===> Z=-SWAM_ZMIN-SWAM_LEN/2 <===              413
         CALL AGSRESET                                                    414
         AG_Z = -SWAM_ZMIN-SWAM_LEN/2                                     414
         AG_TITLE = 'SWMM'                                                414
         AG_EXNAME = 'POSITION'                                           414
         AG_PARLIST = 'Z'                                                 414
         CALL AXPOSITION                                                  414
C    *                                                                    414
         ENDIF                                                            414
C    Check SWAM_LEN>0                                                     416
         IF (SWAM_LEN.GT.0) THEN                                          416
C    Create SBWC :                                                        417
C    Call To Local Procedure SBWC                                         417
         ASSIGN 5190 TO BLOCK_SBWC                                        417
         GO TO  5180                                                      417
5190     CONTINUE                                                         417
C    *                                                                    417
C    Geant POSITION : SBWC ===> Z=(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SW  418
C    AM_ZMIN+SWAM_LEN))/2) <===                                           418
         CALL AGSRESET                                                    418
         AG_Z =                                                           418
     *   (SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)      418
         AG_TITLE = 'SBWC'                                                418
         AG_EXNAME = 'POSITION'                                           418
         AG_PARLIST = 'Z'                                                 418
         CALL AXPOSITION                                                  418
C    *                                                                    418
C    Geant POSITION : SBWC ===> Z=-(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(S  418
C    WAM_ZMIN+SWAM_LEN))/2) THETAZ=180 <===                               418
         CALL AGSRESET                                                    420
         AG_Z =                                                           420
     *   -(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)     420
         AG_THETAZ = 180                                                  420
         AG_TITLE = 'SBWC'                                                420
         AG_EXNAME = 'POSITION'                                           420
         AG_PARLIST = 'Z_THETAZ'                                          420
         CALL AXPOSITION                                                  420
C    *                                                                    420
         ENDIF                                                            420
C    Create SCON :                                                        422
C    Call To Local Procedure SCON                                         422
         ASSIGN 5210 TO BLOCK_SCON                                        422
         GO TO  5200                                                      422
5210     CONTINUE                                                         422
C    *                                                                    422
C    Geant POSITION : SCON ===> inherited arguments used <===             423
         CALL AGSRESET                                                    423
         AG_TITLE = 'SCON'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'NONE'                                              423
         CALL AXPOSITION                                                  423
C    *                                                                    423
C    Geant POSITION : SCON ===> THETAZ=180 <===                           423
         CALL AGSRESET                                                    424
         AG_THETAZ = 180                                                  424
         AG_TITLE = 'SCON'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'THETAZ'                                            424
         CALL AXPOSITION                                                  424
C    *                                                                    424
C    Create SBSP :                                                        425
C    Call To Local Procedure SBSP                                         425
         ASSIGN 5230 TO BLOCK_SBSP                                        425
         GO TO  5220                                                      425
5230     CONTINUE                                                         425
C    *                                                                    425
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     426
         CALL AGSRESET                                                    426
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            426
         AG_TITLE = 'SBSP'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'Z'                                                 426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  426
C    =180 <===                                                            426
         CALL AGSRESET                                                    427
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           427
         AG_THETAZ = 180                                                  427
         AG_TITLE = 'SBSP'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'Z_THETAZ'                                          427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Create SROD :                                                        428
C    Call To Local Procedure SROD                                         428
         ASSIGN 5250 TO BLOCK_SROD                                        428
         GO TO  5240                                                      428
5250     CONTINUE                                                         428
C    *                                                                    428
C    Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===          429
         CALL AGSRESET                                                    429
         AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                 429
         AG_TITLE = 'SROD'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'Y'                                                 429
         CALL AXPOSITION                                                  429
C    *                                                                    429
C    Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===         429
         CALL AGSRESET                                                    430
         AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                430
         AG_TITLE = 'SROD'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'Y'                                                 430
         CALL AXPOSITION                                                  430
C    *                                                                    430
         RADMAX=SVTG_RSIZEMAX                                             430
C    *                                                                    432
         ILAYER=1                                                         432
            GO TO 5263                                                    432
5261        ILAYER=ILAYER+1                                               432
5263        IF(ILAYER-(MIN(6,SVTG_NLAYER)).GT.0) GO TO 5262               432
C       Check ILAYER<6                                                    433
            IF (ILAYER.LT.6) THEN                                         433
C       use SVTL bank                                                     434
            BPATH_SVTL='SVTL*'                                            434
            IDNUM_SVTL(1)=0                                               434
            SVTL_LAYER=ILAYER+1                                           434
            CALL AGDGETP('SVTTGEO2','*',-999,'SVTL',NSVTL_LAYER,          434
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    434
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             434
            RADMAX=SVTL_RADIUS                                            435
            ELSE                                                          436
            RADMAX=SVTG_RSIZEMAX                                          437
            ENDIF                                                         438
C       use SVTL bank                                                     439
            BPATH_SVTL='SVTL*'                                            439
            IDNUM_SVTL(1)=0                                               439
            SVTL_LAYER=ILAYER                                             439
            CALL AGDGETP('SVTTGEO2','*',-999,'SVTL',NSVTL_LAYER,          439
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    439
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             439
C       Create SLYD :                                                     440
C       Call To Local Procedure SLYD                                      440
            ASSIGN 5280 TO BLOCK_SLYD                                     440
            GO TO  5270                                                   440
5280        CONTINUE                                                      440
C       *                                                                 440
C       Check ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6                   441
            IF (ILAYER.EQ.2.OR.ILAYER.EQ.4.OR.ILAYER.EQ.6) THEN           441
C       Geant POSITION : SLYD ===> KONLY='MANY' <===                      442
            CALL AGSRESET                                                 442
            AG_KONLY = 'MANY'                                             442
            AG_TITLE = 'SLYD'                                             442
            AG_EXNAME = 'POSITION'                                        442
            AG_PARLIST = 'KONLY'                                          442
            CALL AXPOSITION                                               442
C       *                                                                 442
            ELSE                                                          442
C       Geant POSITION : SLYD ===> inherited arguments used <===          444
            CALL AGSRESET                                                 444
            AG_TITLE = 'SLYD'                                             444
            AG_EXNAME = 'POSITION'                                        444
            AG_PARLIST = 'NONE'                                           444
            CALL AXPOSITION                                               444
C       *                                                                 444
            ENDIF                                                         444
         GO TO 5261                                                       446
5262     CONTINUE                                                         446
C    Check SVTG_NLAYER<0                                                  447
         IF (SVTG_NLAYER.LT.0) THEN                                       447
C    use SVTL bank                                                        448
         BPATH_SVTL='SVTL*'                                               448
         IDNUM_SVTL(1)=0                                                  448
         SVTL_LAYER=-SVTG_NLAYER                                          448
         CALL AGDGETP('SVTTGEO2','*',-999,'SVTL',NSVTL_LAYER,             448
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       448
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                448
C    Create SLYD :                                                        449
C    Call To Local Procedure SLYD                                         449
         ASSIGN 5290 TO BLOCK_SLYD                                        449
         GO TO  5270                                                      449
5290     CONTINUE                                                         449
C    *                                                                    449
C    Geant POSITION : SLYD ===> inherited arguments used <===             450
         CALL AGSRESET                                                    450
         AG_TITLE = 'SLYD'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'NONE'                                              450
         CALL AXPOSITION                                                  450
C    *                                                                    450
         ENDIF                                                            450
C    Check SVTG_NLAYER>6                                                  452
         IF (SVTG_NLAYER.GT.6) THEN                                       452
C    Create SFMO :                                                        453
C    Call To Local Procedure SFMO                                         453
         ASSIGN 5310 TO BLOCK_SFMO                                        453
         GO TO  5300                                                      453
5310     CONTINUE                                                         453
C    *                                                                    453
C    Geant POSITION : SFMO ===> inherited arguments used <===             454
         CALL AGSRESET                                                    454
         AG_TITLE = 'SFMO'                                                454
         AG_EXNAME = 'POSITION'                                           454
         AG_PARLIST = 'NONE'                                              454
         CALL AXPOSITION                                                  454
C    *                                                                    454
         ENDIF                                                            454
C    Create SISH :                                                        456
C    Call To Local Procedure SISH                                         456
         ASSIGN 5330 TO BLOCK_SISH                                        456
         GO TO  5320                                                      456
5330     CONTINUE                                                         456
C    *                                                                    456
C    Geant POSITION : SISH ===> inherited arguments used <===             457
         CALL AGSRESET                                                    457
         AG_TITLE = 'SISH'                                                457
         AG_EXNAME = 'POSITION'                                           457
         AG_PARLIST = 'NONE'                                              457
         CALL AXPOSITION                                                  457
C    *                                                                    457
C    Create SOSH :                                                        458
C    Call To Local Procedure SOSH                                         458
         ASSIGN 5350 TO BLOCK_SOSH                                        458
         GO TO  5340                                                      458
5350     CONTINUE                                                         458
C    *                                                                    458
C    Geant POSITION : SOSH ===> inherited arguments used <===             459
         CALL AGSRESET                                                    459
         AG_TITLE = 'SOSH'                                                459
         AG_EXNAME = 'POSITION'                                           459
         AG_PARLIST = 'NONE'                                              459
         CALL AXPOSITION                                                  459
C    *                                                                    459
C    Check SVTG_NLAYER>6                                                  459
         IF (SVTG_NLAYER.GT.6) THEN                                       460
C    Create SSSH :                                                        461
C    Call To Local Procedure SSSH                                         461
         ASSIGN 5370 TO BLOCK_SSSH                                        461
         GO TO  5360                                                      461
5370     CONTINUE                                                         461
C    *                                                                    461
C    Geant POSITION : SSSH ===> inherited arguments used <===             462
         CALL AGSRESET                                                    462
         AG_TITLE = 'SSSH'                                                462
         AG_EXNAME = 'POSITION'                                           462
         AG_PARLIST = 'NONE'                                              462
         CALL AXPOSITION                                                  462
C    *                                                                    462
         ENDIF                                                            462
C    Create SALM :                                                        464
C    Call To Local Procedure SALM                                         464
         ASSIGN 5390 TO BLOCK_SALM                                        464
         GO TO  5380                                                      464
5390     CONTINUE                                                         464
C    *                                                                    464
C    Geant POSITION : SALM ===> Z=SSLD_ALMSHPOS-SSLD_ALMSHTHK/2 <===      465
         CALL AGSRESET                                                    465
         AG_Z = SSLD_ALMSHPOS-SSLD_ALMSHTHK/2                             465
         AG_TITLE = 'SALM'                                                465
         AG_EXNAME = 'POSITION'                                           465
         AG_PARLIST = 'Z'                                                 465
         CALL AXPOSITION                                                  465
C    *                                                                    465
C    Geant POSITION : SALM ===> Z=-(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2) <===   465
         CALL AGSRESET                                                    466
         AG_Z = -(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2)                          466
         AG_TITLE = 'SALM'                                                466
         AG_EXNAME = 'POSITION'                                           466
         AG_PARLIST = 'Z'                                                 466
         CALL AXPOSITION                                                  466
C    *                                                                    466
C    Create SCBM :                                                        467
C    Call To Local Procedure SCBM                                         467
         ASSIGN 5410 TO BLOCK_SCBM                                        467
         GO TO  5400                                                      467
5410     CONTINUE                                                         467
C    *                                                                    467
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=SWAM_ZMIN-SCBP_LEN <===     468
         CALL AGSRESET                                                    468
         AG_X = 0.0                                                       468
         AG_Y = 0.0                                                       468
         AG_Z = SWAM_ZMIN-SCBP_LEN                                        468
         AG_TITLE = 'SCBM'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'X_Y_Z'                                             468
         CALL AXPOSITION                                                  468
C    *                                                                    468
C    Geant POSITION : SCBM ===> X=0.0 Y=0.0 Z=-(SWAM_ZMIN-SCBP_LEN) THET  468
C    AZ=180.0 <===                                                        468
         CALL AGSRESET                                                    469
         AG_X = 0.0                                                       469
         AG_Y = 0.0                                                       469
         AG_Z = -(SWAM_ZMIN-SCBP_LEN)                                     469
         AG_THETAZ = 180.0                                                469
         AG_TITLE = 'SCBM'                                                469
         AG_EXNAME = 'POSITION'                                           469
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      469
         CALL AXPOSITION                                                  469
C    *                                                                    469
C    use SVTL bank                                                        469
         BPATH_SVTL='SVTL*'                                               470
         IDNUM_SVTL(1)=0                                                  470
         SVTL_LAYER=6                                                     470
         CALL AGDGETP('SVTTGEO2','*',-999,'SVTL',NSVTL_LAYER,             470
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       470
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                470
C    Create SXRL :                                                        471
C    Call To Local Procedure SXRL                                         471
         ASSIGN 5430 TO BLOCK_SXRL                                        471
         GO TO  5420                                                      471
5430     CONTINUE                                                         471
C    *                                                                    471
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  473
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  473
         CALL AGSRESET                                                    473
         AG_X = 0.0                                                       473
         AG_Y = 0.0                                                       473
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               473
         AG_KONLY = 'MANY'                                                473
         AG_TITLE = 'SXRL'                                                473
         AG_EXNAME = 'POSITION'                                           473
         AG_PARLIST = 'X_Y_Z_KONLY'                                       473
         CALL AXPOSITION                                                  473
C    *                                                                    473
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  473
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 473
         CALL AGSRESET                                                    475
         AG_X = 0.0                                                       475
         AG_Y = 0.0                                                       475
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              475
         AG_KONLY = 'MANY'                                                475
         AG_TITLE = 'SXRL'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'X_Y_Z_KONLY'                                       475
         CALL AXPOSITION                                                  475
C    *                                                                    475
C    use SVTL bank                                                        475
         BPATH_SVTL='SVTL*'                                               476
         IDNUM_SVTL(1)=0                                                  476
         SVTL_LAYER=4                                                     476
         CALL AGDGETP('SVTTGEO2','*',-999,'SVTL',NSVTL_LAYER,             476
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       476
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                476
C    Create SXRL :                                                        477
C    Call To Local Procedure SXRL                                         477
         ASSIGN 5440 TO BLOCK_SXRL                                        477
         GO TO  5420                                                      477
5440     CONTINUE                                                         477
C    *                                                                    477
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  479
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  479
         CALL AGSRESET                                                    479
         AG_X = 0.0                                                       479
         AG_Y = 0.0                                                       479
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               479
         AG_KONLY = 'MANY'                                                479
         AG_TITLE = 'SXRL'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Y_Z_KONLY'                                       479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  479
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 479
         CALL AGSRESET                                                    481
         AG_X = 0.0                                                       481
         AG_Y = 0.0                                                       481
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              481
         AG_KONLY = 'MANY'                                                481
         AG_TITLE = 'SXRL'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'X_Y_Z_KONLY'                                       481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    use SVTL bank                                                        481
         BPATH_SVTL='SVTL*'                                               482
         IDNUM_SVTL(1)=0                                                  482
         SVTL_LAYER=2                                                     482
         CALL AGDGETP('SVTTGEO2','*',-999,'SVTL',NSVTL_LAYER,             482
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       482
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                482
C    Create SXRL :                                                        483
C    Call To Local Procedure SXRL                                         483
         ASSIGN 5450 TO BLOCK_SXRL                                        483
         GO TO  5420                                                      483
5450     CONTINUE                                                         483
C    *                                                                    483
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=SWCA_LENGTH/2.0-SWCX_LENGT  485
C    H/2.0-SWCX_OFFSET KONLY='MANY' <===                                  485
         CALL AGSRESET                                                    485
         AG_X = 0.0                                                       485
         AG_Y = 0.0                                                       485
         AG_Z = SWCA_LENGTH/2.0-SWCX_LENGTH/2.0-SWCX_OFFSET               485
         AG_KONLY = 'MANY'                                                485
         AG_TITLE = 'SXRL'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'X_Y_Z_KONLY'                                       485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    Geant POSITION : SXRL ===> X=0.0 Y=0.0 Z=-SWCA_LENGTH/2.0+SWCX_LENG  485
C    TH/2.0+SWCX_OFFSET KONLY='MANY' <===                                 485
         CALL AGSRESET                                                    487
         AG_X = 0.0                                                       487
         AG_Y = 0.0                                                       487
         AG_Z = -SWCA_LENGTH/2.0+SWCX_LENGTH/2.0+SWCX_OFFSET              487
         AG_KONLY = 'MANY'                                                487
         AG_TITLE = 'SXRL'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'X_Y_Z_KONLY'                                       487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Create SOUM :                                                        488
C    Call To Local Procedure SOUM                                         488
         ASSIGN 5470 TO BLOCK_SOUM                                        488
         GO TO  5460                                                      488
5470     CONTINUE                                                         488
C    *                                                                    488
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 <===                    489
         CALL AGSRESET                                                    489
         AG_X = 0.0                                                       489
         AG_Y = 0.0                                                       489
         AG_Z = 0.0                                                       489
         AG_TITLE = 'SOUM'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'X_Y_Z'                                             489
         CALL AXPOSITION                                                  489
C    *                                                                    489
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 <===       489
         CALL AGSRESET                                                    490
         AG_X = 0.0                                                       490
         AG_Y = 0.0                                                       490
         AG_Z = 0.0                                                       490
         AG_ALPHAN=AG_ALPHAN+1                                            490
         AG_ALPHAK(AG_ALPHAN)=2                                           490
         AG_ALPHAL(AG_ALPHAN)=180.0                                       490
         AG_TITLE = 'SOUM'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      490
         CALL AXPOSITION                                                  490
C    *                                                                    490
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAY=180.0 ALPHAZ=18  490
C    0 <===                                                               490
         CALL AGSRESET                                                    491
         AG_X = 0.0                                                       491
         AG_Y = 0.0                                                       491
         AG_Z = 0.0                                                       491
         AG_ALPHAN=AG_ALPHAN+1                                            491
         AG_ALPHAK(AG_ALPHAN)=2                                           491
         AG_ALPHAL(AG_ALPHAN)=180.0                                       491
         AG_ALPHAN=AG_ALPHAN+1                                            491
         AG_ALPHAK(AG_ALPHAN)=3                                           491
         AG_ALPHAL(AG_ALPHAN)=180                                         491
         AG_TITLE = 'SOUM'                                                491
         AG_EXNAME = 'POSITION'                                           491
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               491
         CALL AXPOSITION                                                  491
C    *                                                                    491
C    Geant POSITION : SOUM ===> X=0.0 Y=0.0 Z=0.0 ALPHAZ=180 <===         491
         CALL AGSRESET                                                    492
         AG_X = 0.0                                                       492
         AG_Y = 0.0                                                       492
         AG_Z = 0.0                                                       492
         AG_ALPHAN=AG_ALPHAN+1                                            492
         AG_ALPHAK(AG_ALPHAN)=3                                           492
         AG_ALPHAL(AG_ALPHAN)=180                                         492
         AG_TITLE = 'SOUM'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      492
         CALL AXPOSITION                                                  492
C    *                                                                    492
5021  CALL AGSPOPB                                                        493
      GO TO BLOCK_SVTT, (5020 ,5030)                                      493
C End of Local Procedure                                                  493
C ----------------------------------------------------------------------  494
C ------------ SXRL                                                       494
C Begin Local Procedure SXRL                                              495
C SXRL IS THE MOTHER OF THE CIRCULAR WATER PIPES                          495
5420  CONTINUE                                                            495
         AG_TITLE='SXRL'                                                  495
         AG_MODULE=MODULE                                                 495
         AG_MARK='V00'                                                    495
         CALL AGSPUSH                                                     495
         AG_MODULE=MODULE                                                 495
C    *                                                                    495
C    Geant MATERIAL : AIR ===> inherited arguments used <===              496
         CALL AGSRESET                                                    496
         AG_TITLE = 'AIR'                                                 496
         AG_EXNAME = 'MATERIAL'                                           496
         AG_PARLIST = 'NONE'                                              496
         CALL AXMATERIAL                                                  496
C    *                                                                    496
C    Geant ATTRIBUTE : SXRL ===> SEEN=0 COLO=1 <===                       496
         CALL AGSRESET                                                    497
         AG_SEEN = 0                                                      497
         AG_COLO = 1                                                      497
         AG_TITLE = 'SXRL'                                                497
         AG_EXNAME = 'ATTRIBUTE'                                          497
         AG_PARLIST = 'SEEN_COLO'                                         497
         CALL AXATTRIBUTE                                                 497
C    *                                                                    497
C    Geant SHAPE : TUBE ===> RMIN=SWCX_ROFFSET+SVTL_RADIUS, RMAX=SWCX_RO  497
C    FFSET+SVTL_RADIUS+SWCX_DR DZ=SWCX_LENGTH/2.0 <===                    497
         CALL AGSRESET                                                    499
         AG_RMIN = SWCX_ROFFSET+SVTL_RADIUS                               499
         AG_RMAX = SWCX_ROFFSET+SVTL_RADIUS+SWCX_DR                       499
         AG_DZ = SWCX_LENGTH/2.0                                          499
         AG_TITLE = 'TUBE'                                                499
         AG_EXNAME = 'SHAPE'                                              499
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      499
         CALL AXSHAPE                                                     499
C    *                                                                    499
C    Create SWRP :                                                        500
C    Call To Local Procedure SWRP                                         500
         ASSIGN 5490 TO BLOCK_SWRP                                        500
         GO TO  5480                                                      500
5490     CONTINUE                                                         500
C    *                                                                    500
C    Create SYRU :                                                        501
C    Call To Local Procedure SYRU                                         501
         ASSIGN 5510 TO BLOCK_SYRU                                        501
         GO TO  5500                                                      501
5510     CONTINUE                                                         501
C    *                                                                    501
C    *                                                                    502
         I_PHI=1                                                          502
            GO TO 5523                                                    502
5521        I_PHI=I_PHI+1                                                 502
5523        IF(I_PHI-(4*SVTL_NLADDER).GT.0) GO TO 5522                    502
            TUBE_ANGLE=(PI/(2.0*SVTL_NLADDER))*(I_PHI-0.5)                503
C       Geant POSITION : SWRP ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  505
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  505
C       SWCX_DR/2.0) Z=0.0 <===                                           505
            CALL AGSRESET                                                 505
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     505
     *      2.0)                                                          505
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     505
     *      2.0)                                                          505
            AG_Z = 0.0                                                    505
            AG_TITLE = 'SWRP'                                             505
            AG_EXNAME = 'POSITION'                                        505
            AG_PARLIST = 'X_Y_Z'                                          505
            CALL AXPOSITION                                               505
C       *                                                                 505
C       Geant POSITION : SYRU ===> X=COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_R  505
C       OFFSET+SWCX_DR/2.0) Y=SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+  505
C       SWCX_DR/2.0) Z=0.0 <===                                           505
            CALL AGSRESET                                                 507
            AG_X = COS(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     507
     *      2.0)                                                          507
            AG_Y = SIN(TUBE_ANGLE)*(SVTL_RADIUS+SWCX_ROFFSET+SWCX_DR/     507
     *      2.0)                                                          507
            AG_Z = 0.0                                                    507
            AG_TITLE = 'SYRU'                                             507
            AG_EXNAME = 'POSITION'                                        507
            AG_PARLIST = 'X_Y_Z'                                          507
            CALL AXPOSITION                                               507
C       *                                                                 507
         GO TO 5521                                                       508
5522     CONTINUE                                                         508
5421  CALL AGSPOPB                                                        509
      GO TO BLOCK_SXRL, (5420 ,5430,5440,5450)                            509
C End of Local Procedure                                                  509
C ----------------------------------------------------------------------  510
C ------------ SWRP                                                       510
C Begin Local Procedure SWRP                                              512
C SWRP IS AN APPROXIMATION OF WATER IN THE CIRCULAR PIPE, A RECTANGULAR,  512
C  ONE                                                                    512
5480  CONTINUE                                                            512
         AG_TITLE='SWRP'                                                  512
         AG_MODULE=MODULE                                                 512
         AG_MARK='V00'                                                    512
         CALL AGSPUSH                                                     512
         AG_MODULE=MODULE                                                 512
C    *                                                                    512
C    Geant MATERIAL : WATER ===> inherited arguments used <===            513
         CALL AGSRESET                                                    513
         AG_TITLE = 'WATER'                                               513
         AG_EXNAME = 'MATERIAL'                                           513
         AG_PARLIST = 'NONE'                                              513
         CALL AXMATERIAL                                                  513
C    *                                                                    513
C    Geant ATTRIBUTE : SWRP ===> SEEN=1 COLO=4 <===                       513
         CALL AGSRESET                                                    514
         AG_SEEN = 1                                                      514
         AG_COLO = 4                                                      514
         AG_TITLE = 'SWRP'                                                514
         AG_EXNAME = 'ATTRIBUTE'                                          514
         AG_PARLIST = 'SEEN_COLO'                                         514
         CALL AXATTRIBUTE                                                 514
C    *                                                                    514
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SWCX_RAD DZ=SWCX_LENGTH/2.0 <  514
C    ===                                                                  514
         CALL AGSRESET                                                    515
         AG_RMIN = 0.0                                                    515
         AG_RMAX = SWCX_RAD                                               515
         AG_DZ = SWCX_LENGTH/2.0                                          515
         AG_TITLE = 'TUBE'                                                515
         AG_EXNAME = 'SHAPE'                                              515
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      515
         CALL AXSHAPE                                                     515
C    *                                                                    515
5481  CALL AGSPOPB                                                        516
      GO TO BLOCK_SWRP, (5480 ,5490)                                      516
C End of Local Procedure                                                  516
C ----------------------------------------------------------------------  517
C ------------ SYRU                                                       517
C Begin Local Procedure SYRU                                              518
C SYRU IS THE WALL OF THE WATER PIPE                                      518
5500  CONTINUE                                                            518
         AG_TITLE='SYRU'                                                  518
         AG_MODULE=MODULE                                                 518
         AG_MARK='V00'                                                    518
         CALL AGSPUSH                                                     518
         AG_MODULE=MODULE                                                 518
C    *                                                                    518
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           519
         CALL AGSRESET                                                    519
         AG_A = 12                                                        519
         AG_Z = 6                                                         519
         AG_W = 1                                                         519
         AG_TITLE = 'C'                                                   519
         AG_EXNAME = 'COMPONENT'                                          519
         AG_PARLIST = 'A_Z_W'                                             519
         CALL AXCOMPONENT                                                 519
C    *                                                                    519
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           519
         CALL AGSRESET                                                    520
         AG_A = 1                                                         520
         AG_Z = 1                                                         520
         AG_W = 2                                                         520
         AG_TITLE = 'H2'                                                  520
         AG_EXNAME = 'COMPONENT'                                          520
         AG_PARLIST = 'A_Z_W'                                             520
         CALL AXCOMPONENT                                                 520
C    *                                                                    520
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             520
         CALL AGSRESET                                                    521
         AG_DENS = 0.935                                                  521
         AG_TITLE = 'CH2'                                                 521
         AG_EXNAME = 'MIXTURE'                                            521
         AG_PARLIST = 'DENS'                                              521
         CALL AXMIXTURE                                                   521
C    *                                                                    521
C    Geant ATTRIBUTE : SYRU ===> SEEN=1 COLO=4 <===                       521
         CALL AGSRESET                                                    522
         AG_SEEN = 1                                                      522
         AG_COLO = 4                                                      522
         AG_TITLE = 'SYRU'                                                522
         AG_EXNAME = 'ATTRIBUTE'                                          522
         AG_PARLIST = 'SEEN_COLO'                                         522
         CALL AXATTRIBUTE                                                 522
C    *                                                                    522
C    Geant SHAPE : TUBE ===> RMIN=SWCX_RAD RMAX=SWCX_RAD+SWCX_WALL, DZ=S  522
C    WCX_LENGTH/2.0 <===                                                  522
         CALL AGSRESET                                                    524
         AG_RMIN = SWCX_RAD                                               524
         AG_RMAX = SWCX_RAD+SWCX_WALL                                     524
         AG_DZ = SWCX_LENGTH/2.0                                          524
         AG_TITLE = 'TUBE'                                                524
         AG_EXNAME = 'SHAPE'                                              524
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      524
         CALL AXSHAPE                                                     524
C    *                                                                    524
5501  CALL AGSPOPB                                                        525
      GO TO BLOCK_SYRU, (5500 ,5510)                                      525
C End of Local Procedure                                                  525
C ----------------------------------------------------------------------  526
C ------------ SOUM                                                       526
C Begin Local Procedure SOUM                                              527
C SOUM IS THE MOTHER OF THE ARRAY OF THE OUTER SHILEDING SUPPORT TUBES    527
5460  CONTINUE                                                            527
         AG_TITLE='SOUM'                                                  527
         AG_MODULE=MODULE                                                 527
         AG_MARK='V00'                                                    527
         CALL AGSPUSH                                                     527
         AG_MODULE=MODULE                                                 527
C    *                                                                    527
C    Geant MATERIAL : AIR ===> inherited arguments used <===              528
         CALL AGSRESET                                                    528
         AG_TITLE = 'AIR'                                                 528
         AG_EXNAME = 'MATERIAL'                                           528
         AG_PARLIST = 'NONE'                                              528
         CALL AXMATERIAL                                                  528
C    *                                                                    528
C    Geant ATTRIBUTE : SOUM ===> SEEN=0 COLO=1 <===                       528
         CALL AGSRESET                                                    529
         AG_SEEN = 0                                                      529
         AG_COLO = 1                                                      529
         AG_TITLE = 'SOUM'                                                529
         AG_EXNAME = 'ATTRIBUTE'                                          529
         AG_PARLIST = 'SEEN_COLO'                                         529
         CALL AXATTRIBUTE                                                 529
C    *                                                                    529
C    Geant SHAPE : TUBS ===> RMIN=SOUP_ROUT-SOUP_DR RMAX=SOUP_ROUT, PHI1  529
C    =SOUP_PHI1 PHI2=SOUP_PHI2 DZ=SOUP_LENGTH/2.0 <===                    529
         CALL AGSRESET                                                    531
         AG_RMIN = SOUP_ROUT-SOUP_DR                                      531
         AG_RMAX = SOUP_ROUT                                              531
         AG_PHI1 = SOUP_PHI1                                              531
         AG_PHI2 = SOUP_PHI2                                              531
         AG_DZ = SOUP_LENGTH/2.0                                          531
         AG_TITLE = 'TUBS'                                                531
         AG_EXNAME = 'SHAPE'                                              531
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            531
         CALL AXSHAPE                                                     531
C    *                                                                    531
C    Create SOUR :                                                        532
C    Call To Local Procedure SOUR                                         532
         ASSIGN 5540 TO BLOCK_SOUR                                        532
         GO TO  5530                                                      532
5540     CONTINUE                                                         532
C    *                                                                    532
C    *                                                                    533
         I_PHI=1                                                          533
            GO TO 5553                                                    533
5551        I_PHI=I_PHI+1                                                 533
5553        IF(I_PHI-(4).GT.0) GO TO 5552                                 533
C       Geant POSITION : SOUR ===> X=COS(SHIELD_PHI(I_PHI)*3.1415926/180  535
C       .0)*(SOUP_ROUT-SOUP_DR/2.0) Y=SIN(SHIELD_PHI(I_PHI)*3.1415926/18  535
C       0.0)*(SOUP_ROUT-SOUP_DR/2.0) Z=0.0 <===                           535
            CALL AGSRESET                                                 535
            AG_X = COS(SHIELD_PHI(I_PHI)*3.1415926/                       535
     *      180.0)*(SOUP_ROUT-SOUP_DR/2.0)                                535
            AG_Y = SIN(SHIELD_PHI(I_PHI)*3.1415926/                       535
     *      180.0)*(SOUP_ROUT-SOUP_DR/2.0)                                535
            AG_Z = 0.0                                                    535
            AG_TITLE = 'SOUR'                                             535
            AG_EXNAME = 'POSITION'                                        535
            AG_PARLIST = 'X_Y_Z'                                          535
            CALL AXPOSITION                                               535
C       *                                                                 535
         GO TO 5551                                                       536
5552     CONTINUE                                                         536
5461  CALL AGSPOPB                                                        537
      GO TO BLOCK_SOUM, (5460 ,5470)                                      537
C End of Local Procedure                                                  537
C ----------------------------------------------------------------------  538
C ------------ SOUR                                                       538
C Begin Local Procedure SOUR                                              539
C SOUR IS THE OUTER SHILEDING SUPPORT TUBES (RODS)                        539
5530  CONTINUE                                                            539
         AG_TITLE='SOUR'                                                  539
         AG_MODULE=MODULE                                                 539
         AG_MARK='V00'                                                    539
         CALL AGSPUSH                                                     539
         AG_MODULE=MODULE                                                 539
C    *                                                                    539
C    Geant MATERIAL : AIR ===> inherited arguments used <===              540
         CALL AGSRESET                                                    540
         AG_TITLE = 'AIR'                                                 540
         AG_EXNAME = 'MATERIAL'                                           540
         AG_PARLIST = 'NONE'                                              540
         CALL AXMATERIAL                                                  540
C    *                                                                    540
C    Geant ATTRIBUTE : SOUM ===> SEEN=1 COLO=4 <===                       540
         CALL AGSRESET                                                    541
         AG_SEEN = 1                                                      541
         AG_COLO = 4                                                      541
         AG_TITLE = 'SOUM'                                                541
         AG_EXNAME = 'ATTRIBUTE'                                          541
         AG_PARLIST = 'SEEN_COLO'                                         541
         CALL AXATTRIBUTE                                                 541
C    *                                                                    541
C    Geant SHAPE : TUBE ===> RMIN=SOUP_DIAMIN/2.0 RMAX=SOUP_DIAMOUT/2.0,  541
C     DZ=SOUP_LENGTH/2.0 <===                                             541
         CALL AGSRESET                                                    543
         AG_RMIN = SOUP_DIAMIN/2.0                                        543
         AG_RMAX = SOUP_DIAMOUT/2.0                                       543
         AG_DZ = SOUP_LENGTH/2.0                                          543
         AG_TITLE = 'TUBE'                                                543
         AG_EXNAME = 'SHAPE'                                              543
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      543
         CALL AXSHAPE                                                     543
C    *                                                                    543
5531  CALL AGSPOPB                                                        544
      GO TO BLOCK_SOUR, (5530 ,5540)                                      544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ SLYD                                                       545
C Begin Local Procedure SLYD                                              546
C SLYD IS A SINGLE SVT LAYER                                              546
5270  CONTINUE                                                            546
         AG_TITLE='SLYD'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
         LADTHK = SWCA_WAFERTHK/2 + SWCA_ROHATHK + SWCA_WAFCARTH          547
         ELETHK=2.0*SELC_BETHK + SELC_WATTHK + SELC_BEOTHK +              548
     *   SELC_AGPDTHK                                                     548
         ELETHK=ELETHK + SELC_GLASSTHK + SELC_DYETHK                      549
         DEG=180.0/SVTL_NLADDER                                           550
         RAD=PI/SVTL_NLADDER                                              551
C    Geant MATERIAL : AIR ===> inherited arguments used <===              552
         CALL AGSRESET                                                    552
         AG_TITLE = 'AIR'                                                 552
         AG_EXNAME = 'MATERIAL'                                           552
         AG_PARLIST = 'NONE'                                              552
         CALL AXMATERIAL                                                  552
C    *                                                                    552
C    Geant ATTRIBUTE : SLYD ===> SEEN=0 COLO=1 <===                       552
         CALL AGSRESET                                                    553
         AG_SEEN = 0                                                      553
         AG_COLO = 1                                                      553
         AG_TITLE = 'SLYD'                                                553
         AG_EXNAME = 'ATTRIBUTE'                                          553
         AG_PARLIST = 'SEEN_COLO'                                         553
         CALL AXATTRIBUTE                                                 553
C    *                                                                    553
         RMIN=SVTL_RADIUS - LADTHK                                        553
         RMAX = SVTL_RADIUS*SVTL_RADIUS                                   555
         RMAX=RMAX+SWCA_WAFERWID*SWCA_WAFERWID/4.0                        556
         RMAX = SQRT(RMAX) + ELETHK                                       557
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   558
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    559
         CALL AGSRESET                                                    559
         AG_RMIN = RMIN                                                   559
         AG_RMAX = RMAX                                                   559
         AG_DZ = SWCA_LENGTH/2                                            559
         AG_TITLE = 'TUBE'                                                559
         AG_EXNAME = 'SHAPE'                                              559
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      559
         CALL AXSHAPE                                                     559
C    *                                                                    559
C    Create SLSD :                                                        560
C    Call To Local Procedure SLSD                                         560
         ASSIGN 5570 TO BLOCK_SLSD                                        560
         GO TO  5560                                                      560
5570     CONTINUE                                                         560
C    *                                                                    560
5271  CALL AGSPOPB                                                        561
      GO TO BLOCK_SLYD, (5270 ,5280,5290)                                 561
C End of Local Procedure                                                  561
C ----------------------------------------------------------------------  562
C ------------ SLSD                                                       562
C Begin Local Procedure SLSD                                              563
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         563
5560  CONTINUE                                                            563
         AG_TITLE='SLSD'                                                  563
         AG_MODULE=MODULE                                                 563
         AG_MARK='V00'                                                    563
         CALL AGSPUSH                                                     563
         AG_MODULE=MODULE                                                 563
C    *                                                                    563
C    Geant ATTRIBUTE : SLSD ===> SEEN=0 COLO=1 <===                       564
         CALL AGSRESET                                                    564
         AG_SEEN = 0                                                      564
         AG_COLO = 1                                                      564
         AG_TITLE = 'SLSD'                                                564
         AG_EXNAME = 'ATTRIBUTE'                                          564
         AG_PARLIST = 'SEEN_COLO'                                         564
         CALL AXATTRIBUTE                                                 564
C    *                                                                    564
C    Check SVTG_NLAYER<0                                                  564
         IF (SVTG_NLAYER.LT.0) THEN                                       565
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               566
         CALL AGSRESET                                                    566
         AG_NDIV = 1                                                      566
         AG_IAXIS = 2                                                     566
         AG_C0 = -90                                                      566
         AG_TITLE = 'DIVISION'                                            566
         AG_EXNAME = 'SHAPE'                                              566
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     566
         CALL AXSHAPE                                                     566
C    *                                                                    566
         ELSE                                                             566
C    Check SVTG_VERSION==1                                                568
         IF (SVTG_VERSION.EQ.1) THEN                                      568
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              569
         ELSE                                                             570
         C0=90-DEG*MOD(ILAYER,2)                                          571
         ENDIF                                                            572
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NLADDER IAXIS=2, C0=C0 <===    574
         CALL AGSRESET                                                    574
         AG_NDIV = SVTL_NLADDER                                           574
         AG_IAXIS = 2                                                     574
         AG_C0 = C0                                                       574
         AG_TITLE = 'DIVISION'                                            574
         AG_EXNAME = 'SHAPE'                                              574
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     574
         CALL AXSHAPE                                                     574
C    *                                                                    574
         ENDIF                                                            574
C    Create SLDI :                                                        576
C    Call To Local Procedure SLDI                                         576
         ASSIGN 5590 TO BLOCK_SLDI                                        576
         GO TO  5580                                                      576
5590     CONTINUE                                                         576
C    *                                                                    576
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ORT=YZX <===                577
         CALL AGSRESET                                                    577
         AG_X = SVTL_RADIUS                                               577
         AG_ALPHAN=AG_ALPHAN+1                                            577
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 577
         AG_TITLE = 'SLDI'                                                577
         AG_EXNAME = 'POSITION'                                           577
         AG_PARLIST = 'X_ORT'                                             577
         CALL AXPOSITION                                                  577
C    *                                                                    577
C    Create SELE :                                                        578
C    Call To Local Procedure SELE                                         578
         ASSIGN 5610 TO BLOCK_SELE                                        578
         GO TO  5600                                                      578
5610     CONTINUE                                                         578
C    *                                                                    578
         DEG=180.0/SVTL_NLADDER                                           579
         RAD=(TWOPI/2)/SVTL_NLADDER                                       580
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 581
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 582
C    *                                                                    583
         S=-1                                                             583
            GOTO 5623                                                     583
5621        S=S+(2)                                                       583
5623        IF((2)*(S-(1)).GT.0) GO TO 5622                               583
            SIDE=S                                                        584
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  586
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=S*DEG <===         586
            CALL AGSRESET                                                 586
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                586
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               586
            AG_ALPHAN=AG_ALPHAN+1                                         586
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              586
            AG_ALPHAN=AG_ALPHAN+1                                         586
            AG_ALPHAK(AG_ALPHAN)=1                                        586
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 586
            AG_ALPHAN=AG_ALPHAN+1                                         586
            AG_ALPHAK(AG_ALPHAN)=3                                        586
            AG_ALPHAL(AG_ALPHAN)=S*DEG                                    586
            AG_TITLE = 'SELE'                                             586
            AG_EXNAME = 'POSITION'                                        586
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          586
            CALL AXPOSITION                                               586
C       *                                                                 586
         GO TO 5621                                                       587
5622     CONTINUE                                                         587
5561  CALL AGSPOPB                                                        588
      GO TO BLOCK_SLSD, (5560 ,5570)                                      588
C End of Local Procedure                                                  588
C ----------------------------------------------------------------------  589
C ------------ SLDI                                                       589
C Begin Local Procedure SLDI                                              590
C SLDI IS A LADDER VOLUME                                                 590
5580  CONTINUE                                                            590
         AG_TITLE='SLDI'                                                  590
         AG_MODULE=MODULE                                                 590
         AG_MARK='V00'                                                    590
         CALL AGSPUSH                                                     590
         AG_MODULE=MODULE                                                 590
C    *                                                                    590
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       591
C    Geant MATERIAL : AIR ===> inherited arguments used <===              592
         CALL AGSRESET                                                    592
         AG_TITLE = 'AIR'                                                 592
         AG_EXNAME = 'MATERIAL'                                           592
         AG_PARLIST = 'NONE'                                              592
         CALL AXMATERIAL                                                  592
C    *                                                                    592
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 SERIAL=SVTL_NWAFER <===    592
         CALL AGSRESET                                                    593
         AG_SEEN = 0                                                      593
         AG_COLO = 1                                                      593
         AG_SERIAL = SVTL_NWAFER                                          593
         AG_TITLE = 'SLDI'                                                593
         AG_EXNAME = 'ATTRIBUTE'                                          593
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  593
         CALL AXATTRIBUTE                                                 593
C    *                                                                    593
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  593
C    HK <===                                                              593
         CALL AGSRESET                                                    595
         AG_DX = SWCA_WAFERWID/2                                          595
         AG_DY = SWCA_LENGTH/2                                            595
         AG_DZ = LADTHK                                                   595
         AG_TITLE = 'BOX'                                                 595
         AG_EXNAME = 'SHAPE'                                              595
         AG_PARLIST = 'DX_DY_DZ'                                          595
         CALL AXSHAPE                                                     595
C    *                                                                    595
C    Create STLI :                                                        596
C    Call To Local Procedure STLI                                         596
         ASSIGN 5640 TO BLOCK_STLI                                        596
         GO TO  5630                                                      596
5640     CONTINUE                                                         596
C    *                                                                    596
C    Geant POSITION : STLI ===> inherited arguments used <===             597
         CALL AGSRESET                                                    597
         AG_TITLE = 'STLI'                                                597
         AG_EXNAME = 'POSITION'                                           597
         AG_PARLIST = 'NONE'                                              597
         CALL AXPOSITION                                                  597
C    *                                                                    597
C    Create SBER :                                                        598
C    Call To Local Procedure SBER                                         598
         ASSIGN 5660 TO BLOCK_SBER                                        598
         GO TO  5650                                                      598
5660     CONTINUE                                                         598
C    *                                                                    598
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  599
C    DTHK+SWCA_WAFCARTH/2 <===                                            599
         CALL AGSRESET                                                    599
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          599
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   599
         AG_TITLE = 'SBER'                                                599
         AG_EXNAME = 'POSITION'                                           599
         AG_PARLIST = 'X_Z'                                               599
         CALL AXPOSITION                                                  599
C    *                                                                    599
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  599
C    DTHK+SWCA_WAFCARTH/2 <===                                            599
         CALL AGSRESET                                                    600
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          600
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   600
         AG_TITLE = 'SBER'                                                600
         AG_EXNAME = 'POSITION'                                           600
         AG_PARLIST = 'X_Z'                                               600
         CALL AXPOSITION                                                  600
C    *                                                                    600
C    Create STAB :                                                        601
C    Call To Local Procedure STAB                                         601
         ASSIGN 5680 TO BLOCK_STAB                                        601
         GO TO  5670                                                      601
5680     CONTINUE                                                         601
C    *                                                                    601
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  602
C    WAFCARTH/2 <===                                                      602
         CALL AGSRESET                                                    602
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    602
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   602
         AG_TITLE = 'STAB'                                                602
         AG_EXNAME = 'POSITION'                                           602
         AG_PARLIST = 'Y_Z'                                               602
         CALL AXPOSITION                                                  602
C    *                                                                    602
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  602
C    _WAFCARTH/2 <===                                                     602
         CALL AGSRESET                                                    603
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   603
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   603
         AG_TITLE = 'STAB'                                                603
         AG_EXNAME = 'POSITION'                                           603
         AG_PARLIST = 'Y_Z'                                               603
         CALL AXPOSITION                                                  603
C    *                                                                    603
C    Create STRU :                                                        604
C    Call To Local Procedure STRU                                         604
         ASSIGN 5700 TO BLOCK_STRU                                        604
         GO TO  5690                                                      604
5700     CONTINUE                                                         604
C    *                                                                    604
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  607
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  607
C    =                                                                    607
         CALL AGSRESET                                                    607
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              607
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  607
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   607
         AG_TITLE = 'STRU'                                                607
         AG_EXNAME = 'POSITION'                                           607
         AG_PARLIST = 'Y_Z'                                               607
         CALL AXPOSITION                                                  607
C    *                                                                    607
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  607
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  607
C    ==                                                                   607
         CALL AGSRESET                                                    610
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             610
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  610
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   610
         AG_TITLE = 'STRU'                                                610
         AG_EXNAME = 'POSITION'                                           610
         AG_PARLIST = 'Y_Z'                                               610
         CALL AXPOSITION                                                  610
C    *                                                                    610
C    Create SRHC :                                                        611
C    Call To Local Procedure SRHC                                         611
         ASSIGN 5720 TO BLOCK_SRHC                                        611
         GO TO  5710                                                      611
5720     CONTINUE                                                         611
C    *                                                                    611
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  613
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         613
         CALL AGSRESET                                                    613
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          613
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                613
         AG_TITLE = 'SRHC'                                                613
         AG_EXNAME = 'POSITION'                                           613
         AG_PARLIST = 'X_Z'                                               613
         CALL AXPOSITION                                                  613
C    *                                                                    613
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  613
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         613
         CALL AGSRESET                                                    615
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          615
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                615
         AG_TITLE = 'SRHC'                                                615
         AG_EXNAME = 'POSITION'                                           615
         AG_PARLIST = 'X_Z'                                               615
         CALL AXPOSITION                                                  615
C    *                                                                    615
C    Create SPCB :                                                        616
C    Call To Local Procedure SPCB                                         616
         ASSIGN 5740 TO BLOCK_SPCB                                        616
         GO TO  5730                                                      616
5740     CONTINUE                                                         616
C    *                                                                    616
         YPCB = SVTL_NWAFER*(SWCA_WAFERLEN + SWCA_WAFERGAP)/2.0 +         617
     *   SVTL_PCBGAP + SVTL_PCBLEN/2.0                                    617
C    Geant POSITION : SPCB ===> X=0.0 Y=YPCB Z=SVTL_PCBTHK/2.0 <===       618
         CALL AGSRESET                                                    618
         AG_X = 0.0                                                       618
         AG_Y = YPCB                                                      618
         AG_Z = SVTL_PCBTHK/2.0                                           618
         AG_TITLE = 'SPCB'                                                618
         AG_EXNAME = 'POSITION'                                           618
         AG_PARLIST = 'X_Y_Z'                                             618
         CALL AXPOSITION                                                  618
C    *                                                                    618
C    Geant POSITION : SPCB ===> X=0.0 Y=-YPCB Z=SVTL_PCBTHK/2.0 ALPHAZ=1  618
C    80.0 <===                                                            618
         CALL AGSRESET                                                    619
         AG_X = 0.0                                                       619
         AG_Y = -YPCB                                                     619
         AG_Z = SVTL_PCBTHK/2.0                                           619
         AG_ALPHAN=AG_ALPHAN+1                                            619
         AG_ALPHAK(AG_ALPHAN)=3                                           619
         AG_ALPHAL(AG_ALPHAN)=180.0                                       619
         AG_TITLE = 'SPCB'                                                619
         AG_EXNAME = 'POSITION'                                           619
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      619
         CALL AXPOSITION                                                  619
C    *                                                                    619
5581  CALL AGSPOPB                                                        620
      GO TO BLOCK_SLDI, (5580 ,5590)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ SRHC                                                       621
C Begin Local Procedure SRHC                                              622
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     622
5710  CONTINUE                                                            622
         AG_TITLE='SRHC'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           623
         CALL AGSRESET                                                    623
         AG_A = 12                                                        623
         AG_Z = 6                                                         623
         AG_W = 1                                                         623
         AG_TITLE = 'C'                                                   623
         AG_EXNAME = 'COMPONENT'                                          623
         AG_PARLIST = 'A_Z_W'                                             623
         CALL AXCOMPONENT                                                 623
C    *                                                                    623
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           623
         CALL AGSRESET                                                    624
         AG_A = 1                                                         624
         AG_Z = 1                                                         624
         AG_W = 2                                                         624
         AG_TITLE = 'H2'                                                  624
         AG_EXNAME = 'COMPONENT'                                          624
         AG_PARLIST = 'A_Z_W'                                             624
         CALL AXCOMPONENT                                                 624
C    *                                                                    624
C    Geant MIXTURE : ROHA ===> DENS=0.0304 <===                           624
         CALL AGSRESET                                                    625
         AG_DENS = 0.0304                                                 625
         AG_TITLE = 'ROHA'                                                625
         AG_EXNAME = 'MIXTURE'                                            625
         AG_PARLIST = 'DENS'                                              625
         CALL AXMIXTURE                                                   625
C    *                                                                    625
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       625
         CALL AGSRESET                                                    626
         AG_SEEN = 1                                                      626
         AG_COLO = 3                                                      626
         AG_TITLE = 'SRHC'                                                626
         AG_EXNAME = 'ATTRIBUTE'                                          626
         AG_PARLIST = 'SEEN_COLO'                                         626
         CALL AXATTRIBUTE                                                 626
C    *                                                                    626
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  626
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        626
         CALL AGSRESET                                                    628
         AG_DX = SWCA_WAFCARWD/2                                          628
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              628
         AG_DZ = SWCA_ROHATHK/2                                           628
         AG_TITLE = 'BOX'                                                 628
         AG_EXNAME = 'SHAPE'                                              628
         AG_PARLIST = 'DX_DY_DZ'                                          628
         CALL AXSHAPE                                                     628
C    *                                                                    628
5711  CALL AGSPOPB                                                        629
      GO TO BLOCK_SRHC, (5710 ,5720)                                      629
C End of Local Procedure                                                  629
C ----------------------------------------------------------------------  630
C ------------ STLI                                                       630
C Begin Local Procedure STLI                                              631
C STLI IS THE WAVER PACK CONTAINER                                        631
5630  CONTINUE                                                            631
         AG_TITLE='STLI'                                                  631
         AG_MODULE=MODULE                                                 631
         AG_MARK='V00'                                                    631
         CALL AGSPUSH                                                     631
         AG_MODULE=MODULE                                                 631
C    *                                                                    631
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              632
         CALL AGSRESET                                                    632
         AG_SEEN = 0                                                      632
         AG_COLO = 1                                                      632
         AG_SERIAL = 0                                                    632
         AG_TITLE = 'STLI'                                                632
         AG_EXNAME = 'ATTRIBUTE'                                          632
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  632
         CALL AXATTRIBUTE                                                 632
C    *                                                                    632
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  632
C    P), DZ=SWCA_WAFERTHK/2 <===                                          632
         CALL AGSRESET                                                    634
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              634
         AG_DZ = SWCA_WAFERTHK/2                                          634
         AG_TITLE = 'BOX'                                                 634
         AG_EXNAME = 'SHAPE'                                              634
         AG_PARLIST = 'DY_DZ'                                             634
         CALL AXSHAPE                                                     634
C    *                                                                    634
C    Create STSI :                                                        635
C    Call To Local Procedure STSI                                         635
         ASSIGN 5760 TO BLOCK_STSI                                        635
         GO TO  5750                                                      635
5760     CONTINUE                                                         635
C    *                                                                    635
5631  CALL AGSPOPB                                                        636
      GO TO BLOCK_STLI, (5630 ,5640)                                      636
C End of Local Procedure                                                  636
C ----------------------------------------------------------------------  637
C ------------ STSI                                                       637
C Begin Local Procedure STSI                                              638
C STSI IS A SINGLE WAVER CONTAINER                                        638
5750  CONTINUE                                                            638
         AG_TITLE='STSI'                                                  638
         AG_MODULE=MODULE                                                 638
         AG_MARK='V00'                                                    638
         CALL AGSPUSH                                                     638
         AG_MODULE=MODULE                                                 638
C    *                                                                    638
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            639
         CALL AGSRESET                                                    639
         AG_NDIV = SVTL_NWAFER                                            639
         AG_IAXIS = 2                                                     639
         AG_TITLE = 'DIVISION'                                            639
         AG_EXNAME = 'SHAPE'                                              639
         AG_PARLIST = 'NDIV_IAXIS'                                        639
         CALL AXSHAPE                                                     639
C    *                                                                    639
C    Create SVTD :                                                        640
C    Call To Local Procedure SVTD                                         640
         ASSIGN 5780 TO BLOCK_SVTD                                        640
         GO TO  5770                                                      640
5780     CONTINUE                                                         640
C    *                                                                    640
C    Geant POSITION : SVTD ===> inherited arguments used <===             641
         CALL AGSRESET                                                    641
         AG_TITLE = 'SVTD'                                                641
         AG_EXNAME = 'POSITION'                                           641
         AG_PARLIST = 'NONE'                                              641
         CALL AXPOSITION                                                  641
C    *                                                                    641
5751  CALL AGSPOPB                                                        642
      GO TO BLOCK_STSI, (5750 ,5760)                                      642
C End of Local Procedure                                                  642
C ----------------------------------------------------------------------  643
C ------------ SVTD                                                       643
C Begin Local Procedure SVTD                                              644
C SVTD IS AN ACTIVE WAFER VOLUME                                          644
5770  CONTINUE                                                            644
         AG_TITLE='SVTD'                                                  644
         AG_MODULE=MODULE                                                 644
         AG_MARK='V00'                                                    644
         CALL AGSPUSH                                                     644
         AG_MODULE=MODULE                                                 644
C    *                                                                    644
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          645
         CALL AGSRESET                                                    645
         AG_TITLE = 'SILICON'                                             645
         AG_EXNAME = 'MATERIAL'                                           645
         AG_PARLIST = 'NONE'                                              645
         CALL AXMATERIAL                                                  645
C    *                                                                    645
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         645
         CALL AGSRESET                                                    646
         AG_ISVOL = 1                                                     646
         AG_TITLE = 'SENSITIVE'                                           646
         AG_EXNAME = 'MATERIAL'                                           646
         AG_PARLIST = 'ISVOL'                                             646
         CALL AXMATERIAL                                                  646
C    *                                                                    646
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       646
         CALL AGSRESET                                                    647
         AG_SEEN = 1                                                      647
         AG_COLO = 4                                                      647
         AG_TITLE = 'SVTD'                                                647
         AG_EXNAME = 'ATTRIBUTE'                                          647
         AG_PARLIST = 'SEEN_COLO'                                         647
         CALL AXATTRIBUTE                                                 647
C    *                                                                    647
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       647
         CALL AGSRESET                                                    648
         AG_DY = SWCA_WAFERLEN/2                                          648
         AG_TITLE = 'BOX'                                                 648
         AG_EXNAME = 'SHAPE'                                              648
         AG_PARLIST = 'DY'                                                648
         CALL AXSHAPE                                                     648
C    *                                                                    648
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 648
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  651
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  651
C    .,0.01)                                                              651
         AG_TITLE = 'SVTD'                                                651
         AG_EXNAME = 'HITS'                                               651
         AG_TYPE=AG_MAGIC                                                 651
         CALL AGSDETV('SVTD')                                             651
C    *                                                                    651
         AG_PARLIST='Z'                                                   651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = AG_MAGIC                                              651
         AG_BIN=.001                                                      651
         AG_OPTION='S'                                                    651
         AG_FHMIN = AG_MAGIC                                              651
         AG_FHMAX = AG_MAGIC                                              651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='Y'                                                   651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = AG_MAGIC                                              651
         AG_BIN=.001                                                      651
         AG_FHMIN=AG_MAGIC                                                651
         AG_FHMAX=AG_MAGIC                                                651
         AG_OPTION='*'                                                    651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='X'                                                   651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = AG_MAGIC                                              651
         AG_BIN=.001                                                      651
         AG_FHMIN=AG_MAGIC                                                651
         AG_FHMAX=AG_MAGIC                                                651
         AG_OPTION='*'                                                    651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='PTOT'                                                651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = 18                                                    651
         AG_BIN=AG_MAGIC                                                  651
         AG_OPTION='*'                                                    651
         AG_FHMIN = 0                                                     651
         AG_FHMAX = 100                                                   651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='CX'                                                  651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = 10                                                    651
         AG_BIN=AG_MAGIC                                                  651
         AG_FHMIN=AG_MAGIC                                                651
         AG_FHMAX=AG_MAGIC                                                651
         AG_OPTION='*'                                                    651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='CY'                                                  651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = 10                                                    651
         AG_BIN=AG_MAGIC                                                  651
         AG_FHMIN=AG_MAGIC                                                651
         AG_FHMAX=AG_MAGIC                                                651
         AG_OPTION='*'                                                    651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='CZ'                                                  651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = 10                                                    651
         AG_BIN=AG_MAGIC                                                  651
         AG_FHMIN=AG_MAGIC                                                651
         AG_FHMAX=AG_MAGIC                                                651
         AG_OPTION='*'                                                    651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='SLENG'                                               651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = 16                                                    651
         AG_BIN=AG_MAGIC                                                  651
         AG_OPTION='*'                                                    651
         AG_FHMIN = 0                                                     651
         AG_FHMAX = 500                                                   651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='TOF'                                                 651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = 16                                                    651
         AG_BIN=AG_MAGIC                                                  651
         AG_OPTION='*'                                                    651
         AG_FHMIN = 0                                                     651
         AG_FHMAX = 1.0E-6                                                651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='STEP'                                                651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = AG_MAGIC                                              651
         AG_BIN=.01                                                       651
         AG_FHMIN=AG_MAGIC                                                651
         AG_FHMAX=AG_MAGIC                                                651
         AG_OPTION='*'                                                    651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         AG_PARLIST='ELOSS'                                               651
         AG_TITLE = 'DETH'                                                651
         AG_NBITS = 22                                                    651
         AG_BIN=AG_MAGIC                                                  651
         AG_OPTION='*'                                                    651
         AG_FHMIN = 0.                                                    651
         AG_FHMAX = 0.01                                                  651
         CALL AGSDETH (0)                                                 651
C    *                                                                    651
         CALL AGSDETU('SVTD')                                             651
C    *                                                                    651
5771  CALL AGSPOPB                                                        652
      GO TO BLOCK_SVTD, (5770 ,5780)                                      652
C End of Local Procedure                                                  652
C ----------------------------------------------------------------------  653
C ------------ SBER                                                       653
C Begin Local Procedure SBER                                              654
C SBER ARE THE BERILLIUM WAFER CARRIER RAILS                              654
5650  CONTINUE                                                            654
         AG_TITLE='SBER'                                                  654
         AG_MODULE=MODULE                                                 654
         AG_MARK='V00'                                                    654
         CALL AGSPUSH                                                     654
         AG_MODULE=MODULE                                                 654
C    *                                                                    654
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        655
         CALL AGSRESET                                                    655
         AG_TITLE = 'BERILLIUM'                                           655
         AG_EXNAME = 'MATERIAL'                                           655
         AG_PARLIST = 'NONE'                                              655
         CALL AXMATERIAL                                                  655
C    *                                                                    655
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       655
         CALL AGSRESET                                                    656
         AG_SEEN = 1                                                      656
         AG_COLO = 2                                                      656
         AG_TITLE = 'SBER'                                                656
         AG_EXNAME = 'ATTRIBUTE'                                          656
         AG_PARLIST = 'SEEN_COLO'                                         656
         CALL AXATTRIBUTE                                                 656
C    *                                                                    656
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  656
C    _WAFCARTH/2 <===                                                     656
         CALL AGSRESET                                                    658
         AG_DX = SWCA_WAFCARWD/2                                          658
         AG_DY = SWCA_LENGTH/2                                            658
         AG_DZ = SWCA_WAFCARTH/2                                          658
         AG_TITLE = 'BOX'                                                 658
         AG_EXNAME = 'SHAPE'                                              658
         AG_PARLIST = 'DX_DY_DZ'                                          658
         CALL AXSHAPE                                                     658
C    *                                                                    658
5651  CALL AGSPOPB                                                        659
      GO TO BLOCK_SBER, (5650 ,5660)                                      659
C End of Local Procedure                                                  659
C ----------------------------------------------------------------------  660
C ------------ STAB                                                       660
C Begin Local Procedure STAB                                              661
C STAB ARE THE BERILLIUM WAFER CARRIER END TABS                           661
5670  CONTINUE                                                            661
         AG_TITLE='STAB'                                                  661
         AG_MODULE=MODULE                                                 661
         AG_MARK='V00'                                                    661
         CALL AGSPUSH                                                     661
         AG_MODULE=MODULE                                                 661
C    *                                                                    661
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        662
         CALL AGSRESET                                                    662
         AG_TITLE = 'BERILLIUM'                                           662
         AG_EXNAME = 'MATERIAL'                                           662
         AG_PARLIST = 'NONE'                                              662
         CALL AXMATERIAL                                                  662
C    *                                                                    662
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       662
         CALL AGSRESET                                                    663
         AG_SEEN = 1                                                      663
         AG_COLO = 2                                                      663
         AG_TITLE = 'STAB'                                                663
         AG_EXNAME = 'ATTRIBUTE'                                          663
         AG_PARLIST = 'SEEN_COLO'                                         663
         CALL AXATTRIBUTE                                                 663
C    *                                                                    663
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  663
C    2 DZ=SWCA_WAFCARTH/2 <===                                            663
         CALL AGSRESET                                                    665
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            665
         AG_DY = TABLEN/2                                                 665
         AG_DZ = SWCA_WAFCARTH/2                                          665
         AG_TITLE = 'BOX'                                                 665
         AG_EXNAME = 'SHAPE'                                              665
         AG_PARLIST = 'DX_DY_DZ'                                          665
         CALL AXSHAPE                                                     665
C    *                                                                    665
5671  CALL AGSPOPB                                                        666
      GO TO BLOCK_STAB, (5670 ,5680)                                      666
C End of Local Procedure                                                  666
C ----------------------------------------------------------------------  667
C ------------ STRU                                                       667
C Begin Local Procedure STRU                                              668
C STRU ARE THE BERILLIUM STRUTS BETWEEN THE WAFER CARRIER RAILS           668
5690  CONTINUE                                                            668
         AG_TITLE='STRU'                                                  668
         AG_MODULE=MODULE                                                 668
         AG_MARK='V00'                                                    668
         CALL AGSPUSH                                                     668
         AG_MODULE=MODULE                                                 668
C    *                                                                    668
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        669
         CALL AGSRESET                                                    669
         AG_TITLE = 'BERILLIUM'                                           669
         AG_EXNAME = 'MATERIAL'                                           669
         AG_PARLIST = 'NONE'                                              669
         CALL AXMATERIAL                                                  669
C    *                                                                    669
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       669
         CALL AGSRESET                                                    670
         AG_SEEN = 1                                                      670
         AG_COLO = 2                                                      670
         AG_TITLE = 'STRU'                                                670
         AG_EXNAME = 'ATTRIBUTE'                                          670
         AG_PARLIST = 'SEEN_COLO'                                         670
         CALL AXATTRIBUTE                                                 670
C    *                                                                    670
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  670
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     670
         CALL AGSRESET                                                    672
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            672
         AG_DY = SWCA_STRUTLEN/2                                          672
         AG_DZ = SWCA_WAFCARTH/2                                          672
         AG_TITLE = 'BOX'                                                 672
         AG_EXNAME = 'SHAPE'                                              672
         AG_PARLIST = 'DX_DY_DZ'                                          672
         CALL AXSHAPE                                                     672
C    *                                                                    672
5691  CALL AGSPOPB                                                        673
      GO TO BLOCK_STRU, (5690 ,5700)                                      673
C End of Local Procedure                                                  673
C ----------------------------------------------------------------------  674
C ------------ SPCB                                                       674
C Begin Local Procedure SPCB                                              675
C SPCB IS THE G10 PCB                                                     675
5730  CONTINUE                                                            675
         AG_TITLE='SPCB'                                                  675
         AG_MODULE=MODULE                                                 675
         AG_MARK='V00'                                                    675
         CALL AGSPUSH                                                     675
         AG_MODULE=MODULE                                                 675
C    *                                                                    675
C    Geant MATERIAL : G10 ===> inherited arguments used <===              676
         CALL AGSRESET                                                    676
         AG_TITLE = 'G10'                                                 676
         AG_EXNAME = 'MATERIAL'                                           676
         AG_PARLIST = 'NONE'                                              676
         CALL AXMATERIAL                                                  676
C    *                                                                    676
C    Geant ATTRIBUTE : SPCB ===> SEEN=1 COLO=5 <===                       676
         CALL AGSRESET                                                    677
         AG_SEEN = 1                                                      677
         AG_COLO = 5                                                      677
         AG_TITLE = 'SPCB'                                                677
         AG_EXNAME = 'ATTRIBUTE'                                          677
         AG_PARLIST = 'SEEN_COLO'                                         677
         CALL AXATTRIBUTE                                                 677
C    *                                                                    677
C    Geant SHAPE : BOX ===> DX=SVTL_PCBWIDTH/2.0 DY=SVTL_PCBLEN/2.0, DZ=  677
C    SVTL_PCBTHK/2.0 <===                                                 677
         CALL AGSRESET                                                    679
         AG_DX = SVTL_PCBWIDTH/2.0                                        679
         AG_DY = SVTL_PCBLEN/2.0                                          679
         AG_DZ = SVTL_PCBTHK/2.0                                          679
         AG_TITLE = 'BOX'                                                 679
         AG_EXNAME = 'SHAPE'                                              679
         AG_PARLIST = 'DX_DY_DZ'                                          679
         CALL AXSHAPE                                                     679
C    *                                                                    679
5731  CALL AGSPOPB                                                        680
      GO TO BLOCK_SPCB, (5730 ,5740)                                      680
C End of Local Procedure                                                  680
C ----------------------------------------------------------------------  681
C ------------ SCBM                                                       681
C Begin Local Procedure SCBM                                              682
C SCBM IS THE MOTHER FOR THE BUNDLE OF CABLES GOING FROM PCBS             682
5400  CONTINUE                                                            682
         AG_TITLE='SCBM'                                                  682
         AG_MODULE=MODULE                                                 682
         AG_MARK='V00'                                                    682
         CALL AGSPUSH                                                     682
         AG_MODULE=MODULE                                                 682
C    *                                                                    682
C    Geant MATERIAL : AIR ===> inherited arguments used <===              683
         CALL AGSRESET                                                    683
         AG_TITLE = 'AIR'                                                 683
         AG_EXNAME = 'MATERIAL'                                           683
         AG_PARLIST = 'NONE'                                              683
         CALL AXMATERIAL                                                  683
C    *                                                                    683
C    Geant ATTRIBUTE : SCBM ===> SEEN=0 COLO=1 <===                       683
         CALL AGSRESET                                                    684
         AG_SEEN = 0                                                      684
         AG_COLO = 1                                                      684
         AG_TITLE = 'SCBM'                                                684
         AG_EXNAME = 'ATTRIBUTE'                                          684
         AG_PARLIST = 'SEEN_COLO'                                         684
         CALL AXATTRIBUTE                                                 684
C    *                                                                    684
C    Geant SHAPE : TUBE ===> RMIN=RADII(1) RMAX=SWAM_RMAX, DZ=SCBP_LEN <  684
C    ===                                                                  684
         CALL AGSRESET                                                    686
         AG_RMIN = RADII(1)                                               686
         AG_RMAX = SWAM_RMAX                                              686
         AG_DZ = SCBP_LEN                                                 686
         AG_TITLE = 'TUBE'                                                686
         AG_EXNAME = 'SHAPE'                                              686
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      686
         CALL AXSHAPE                                                     686
C    *                                                                    686
C    *                                                                    686
         ILAYER=1                                                         687
            GO TO 5793                                                    687
5791        ILAYER=ILAYER+1                                               687
5793        IF(ILAYER-(4).GT.0) GO TO 5792                                687
C       use SCBP bank                                                     688
            BPATH_SCBP='SCBP*'                                            688
            IDNUM_SCBP(1)=0                                               688
            SCBP_LAYER=ILAYER                                             688
            CALL AGDGETP('SVTTGEO2','*',-999,'SCBP',NSCBP_LAYER,          688
     *      RE_SCBP_LAYER,BPATH_SCBP,IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,    688
     *      BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)             688
C       Create SCBL :                                                     689
C       Call To Local Procedure SCBL                                      689
            ASSIGN 5810 TO BLOCK_SCBL                                     689
            GO TO  5800                                                   689
5810        CONTINUE                                                      689
C       *                                                                 689
C       Geant POSITION : SCBL ===> X=0.0 Y=0.0 Z=0.0 <===                 690
            CALL AGSRESET                                                 690
            AG_X = 0.0                                                    690
            AG_Y = 0.0                                                    690
            AG_Z = 0.0                                                    690
            AG_TITLE = 'SCBL'                                             690
            AG_EXNAME = 'POSITION'                                        690
            AG_PARLIST = 'X_Y_Z'                                          690
            CALL AXPOSITION                                               690
C       *                                                                 690
         GO TO 5791                                                       691
5792     CONTINUE                                                         691
C    *                                                                    692
         ILAYER=1                                                         692
            GO TO 5823                                                    692
5821        ILAYER=ILAYER+1                                               692
5823        IF(ILAYER-(3).GT.0) GO TO 5822                                692
C       use SFEP bank                                                     693
            BPATH_SFEP='SFEP*'                                            693
            IDNUM_SFEP(1)=0                                               693
            SFEP_LAYER=ILAYER                                             693
            CALL AGDGETP('SVTTGEO2','*',-999,'SFEP',NSFEP_LAYER,          693
     *      RE_SFEP_LAYER,BPATH_SFEP,IDNUM_SFEP,1,MAP_SFEP,NAMES_SFEP,    693
     *      BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)             693
C       Create SFED :                                                     694
C       Call To Local Procedure SFED                                      694
            ASSIGN 5840 TO BLOCK_SFED                                     694
            GO TO  5830                                                   694
5840        CONTINUE                                                      694
C       *                                                                 694
C       Geant POSITION : SFED ===> X=0.0 Y=0.0 Z=0.0 <===                 695
            CALL AGSRESET                                                 695
            AG_X = 0.0                                                    695
            AG_Y = 0.0                                                    695
            AG_Z = 0.0                                                    695
            AG_TITLE = 'SFED'                                             695
            AG_EXNAME = 'POSITION'                                        695
            AG_PARLIST = 'X_Y_Z'                                          695
            CALL AXPOSITION                                               695
C       *                                                                 695
C       Create SPLS :                                                     696
C       Call To Local Procedure SPLS                                      696
            ASSIGN 5860 TO BLOCK_SPLS                                     696
            GO TO  5850                                                   696
5860        CONTINUE                                                      696
C       *                                                                 696
C       Geant POSITION : SPLS ===> X=0.0 Y=0.0 Z=0.0 <===                 697
            CALL AGSRESET                                                 697
            AG_X = 0.0                                                    697
            AG_Y = 0.0                                                    697
            AG_Z = 0.0                                                    697
            AG_TITLE = 'SPLS'                                             697
            AG_EXNAME = 'POSITION'                                        697
            AG_PARLIST = 'X_Y_Z'                                          697
            CALL AXPOSITION                                               697
C       *                                                                 697
         GO TO 5821                                                       698
5822     CONTINUE                                                         698
5401  CALL AGSPOPB                                                        699
      GO TO BLOCK_SCBM, (5400 ,5410)                                      699
C End of Local Procedure                                                  699
C ----------------------------------------------------------------------  700
C ------------ SCBL                                                       700
C Begin Local Procedure SCBL                                              701
C SCBL IS THE BUNDLE OF CABLES GOING FROM PCBS TO MANIFOLDS               701
5800  CONTINUE                                                            701
         AG_TITLE='SCBL'                                                  701
         AG_MODULE=MODULE                                                 701
         AG_MARK='V00'                                                    701
         CALL AGSPUSH                                                     701
         AG_MODULE=MODULE                                                 701
C    *                                                                    701
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           702
         CALL AGSRESET                                                    702
         AG_TITLE = 'COPPER'                                              702
         AG_EXNAME = 'MATERIAL'                                           702
         AG_PARLIST = 'NONE'                                              702
         CALL AXMATERIAL                                                  702
C    *                                                                    702
C    Geant ATTRIBUTE : SCBL ===> SEEN=1 COLO=3 <===                       702
         CALL AGSRESET                                                    703
         AG_SEEN = 1                                                      703
         AG_COLO = 3                                                      703
         AG_TITLE = 'SCBL'                                                703
         AG_EXNAME = 'ATTRIBUTE'                                          703
         AG_PARLIST = 'SEEN_COLO'                                         703
         CALL AXATTRIBUTE                                                 703
C    *                                                                    703
         SQ =(SCBP_LEN*SCBP_LEN)                                          703
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  705
         SQ=SQ/(SCBP_RMIN2 - SCBP_RMIN1)                                  706
         A = PI                                                           707
         A=A*(SCBP_RMIN1*SCBP_RMIN1 + SCBP_RMIN2*SCBP_RMIN2)              708
         A=A*SQRT(1 + SQ)                                                 709
         CUTHK=(SCBP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            710
C    Geant SHAPE : CONE ===> DZ=SCBP_LEN RMN1=SCBP_RMIN1 RMX1=SCBP_RMIN1  712
C    +CUTHK, RMN2=SCBP_RMIN2 RMX2=SCBP_RMIN2+CUTHK <===                   712
         CALL AGSRESET                                                    712
         AG_DZ = SCBP_LEN                                                 712
         AG_RMN1 = SCBP_RMIN1                                             712
         AG_RMX1 = SCBP_RMIN1+CUTHK                                       712
         AG_RMN2 = SCBP_RMIN2                                             712
         AG_RMX2 = SCBP_RMIN2+CUTHK                                       712
         AG_TITLE = 'CONE'                                                712
         AG_EXNAME = 'SHAPE'                                              712
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            712
         CALL AXSHAPE                                                     712
C    *                                                                    712
5801  CALL AGSPOPB                                                        713
      GO TO BLOCK_SCBL, (5800 ,5810)                                      713
C End of Local Procedure                                                  713
C ----------------------------------------------------------------------  714
C ------------ SFED                                                       714
C Begin Local Procedure SFED                                              715
C SFED IS THE WATRER IN THE BUNDLE OF PIPES GOING TO MANIFOLDS            715
5830  CONTINUE                                                            715
         AG_TITLE='SFED'                                                  715
         AG_MODULE=MODULE                                                 715
         AG_MARK='V00'                                                    715
         CALL AGSPUSH                                                     715
         AG_MODULE=MODULE                                                 715
C    *                                                                    715
C    Geant MATERIAL : WATER ===> inherited arguments used <===            716
         CALL AGSRESET                                                    716
         AG_TITLE = 'WATER'                                               716
         AG_EXNAME = 'MATERIAL'                                           716
         AG_PARLIST = 'NONE'                                              716
         CALL AXMATERIAL                                                  716
C    *                                                                    716
C    Geant ATTRIBUTE : SFED ===> SEEN=1 COLO=1 <===                       716
         CALL AGSRESET                                                    717
         AG_SEEN = 1                                                      717
         AG_COLO = 1                                                      717
         AG_TITLE = 'SFED'                                                717
         AG_EXNAME = 'ATTRIBUTE'                                          717
         AG_PARLIST = 'SEEN_COLO'                                         717
         CALL AXATTRIBUTE                                                 717
C    *                                                                    717
         SQ=SFEP_LEN**2/(SFEP_RMIN2 - SFEP_RMIN1)**2                      717
         A=3.1415*(SFEP_RMIN1**2 + SFEP_RMIN2**2)*SQRT(1 + SQ)            719
         SQ = SFEP_LEN*SFEP_LEN                                           720
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  721
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  722
         A = PI                                                           723
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            724
         A=A*SQRT(1 + SQ)                                                 725
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            726
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1 RMX1=SFEP_RMIN1  728
C    +CUTHK, RMN2=SFEP_RMIN2 RMX2=SFEP_RMIN2+CUTHK <===                   728
         CALL AGSRESET                                                    728
         AG_DZ = SFEP_LEN                                                 728
         AG_RMN1 = SFEP_RMIN1                                             728
         AG_RMX1 = SFEP_RMIN1+CUTHK                                       728
         AG_RMN2 = SFEP_RMIN2                                             728
         AG_RMX2 = SFEP_RMIN2+CUTHK                                       728
         AG_TITLE = 'CONE'                                                728
         AG_EXNAME = 'SHAPE'                                              728
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            728
         CALL AXSHAPE                                                     728
C    *                                                                    728
5831  CALL AGSPOPB                                                        729
      GO TO BLOCK_SFED, (5830 ,5840)                                      729
C End of Local Procedure                                                  729
C ----------------------------------------------------------------------  730
C ------------ SPLS                                                       730
C Begin Local Procedure SPLS                                              731
C SPLS IS THE PLASTIC WALLS OF THE BUNDLE OF PIPES GOING TO MANIFOLDS     731
5850  CONTINUE                                                            731
         AG_TITLE='SPLS'                                                  731
         AG_MODULE=MODULE                                                 731
         AG_MARK='V00'                                                    731
         CALL AGSPUSH                                                     731
         AG_MODULE=MODULE                                                 731
C    *                                                                    731
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           732
         CALL AGSRESET                                                    732
         AG_A = 12                                                        732
         AG_Z = 6                                                         732
         AG_W = 1                                                         732
         AG_TITLE = 'C'                                                   732
         AG_EXNAME = 'COMPONENT'                                          732
         AG_PARLIST = 'A_Z_W'                                             732
         CALL AXCOMPONENT                                                 732
C    *                                                                    732
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           732
         CALL AGSRESET                                                    733
         AG_A = 1                                                         733
         AG_Z = 1                                                         733
         AG_W = 2                                                         733
         AG_TITLE = 'H2'                                                  733
         AG_EXNAME = 'COMPONENT'                                          733
         AG_PARLIST = 'A_Z_W'                                             733
         CALL AXCOMPONENT                                                 733
C    *                                                                    733
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             733
         CALL AGSRESET                                                    734
         AG_DENS = 0.935                                                  734
         AG_TITLE = 'CH2'                                                 734
         AG_EXNAME = 'MIXTURE'                                            734
         AG_PARLIST = 'DENS'                                              734
         CALL AXMIXTURE                                                   734
C    *                                                                    734
C    Geant ATTRIBUTE : SPLS ===> SEEN=1 COLO=6 <===                       734
         CALL AGSRESET                                                    735
         AG_SEEN = 1                                                      735
         AG_COLO = 6                                                      735
         AG_TITLE = 'SPLS'                                                735
         AG_EXNAME = 'ATTRIBUTE'                                          735
         AG_PARLIST = 'SEEN_COLO'                                         735
         CALL AXATTRIBUTE                                                 735
C    *                                                                    735
         SQ = SFEP_LEN*SFEP_LEN                                           735
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  737
         SQ=SQ/(SFEP_RMIN2 - SFEP_RMIN1)                                  738
         A = PI                                                           739
         A=A*(SFEP_RMIN1*SFEP_RMIN1 + SFEP_RMIN2 + SFEP_RMIN2)            740
         A=A*SQRT(1 + SQ)                                                 741
         CUTHK=(SFEP_VOL/A)*SQRT(1.0 + 1.0/SQ)                            742
C    Geant SHAPE : CONE ===> DZ=SFEP_LEN RMN1=SFEP_RMIN1+0.2, RMX1=SFEP_  745
C    RMIN1+0.2+CUTHK RMN2=SFEP_RMIN2+0.2, RMX2=SFEP_RMIN2+0.2+CUTHK <===  745
         CALL AGSRESET                                                    745
         AG_DZ = SFEP_LEN                                                 745
         AG_RMN1 = SFEP_RMIN1+0.2                                         745
         AG_RMX1 = SFEP_RMIN1+0.2+CUTHK                                   745
         AG_RMN2 = SFEP_RMIN2+0.2                                         745
         AG_RMX2 = SFEP_RMIN2+0.2+CUTHK                                   745
         AG_TITLE = 'CONE'                                                745
         AG_EXNAME = 'SHAPE'                                              745
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            745
         CALL AXSHAPE                                                     745
C    *                                                                    745
5851  CALL AGSPOPB                                                        746
      GO TO BLOCK_SPLS, (5850 ,5860)                                      746
C End of Local Procedure                                                  746
C ----------------------------------------------------------------------  747
C ------------ SELE                                                       747
C Begin Local Procedure SELE                                              748
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   748
5600  CONTINUE                                                            748
         AG_TITLE='SELE'                                                  748
         AG_MODULE=MODULE                                                 748
         AG_MARK='V00'                                                    748
         CALL AGSPUSH                                                     748
         AG_MODULE=MODULE                                                 748
C    *                                                                    748
C    Geant MATERIAL : AIR ===> inherited arguments used <===              749
         CALL AGSRESET                                                    749
         AG_TITLE = 'AIR'                                                 749
         AG_EXNAME = 'MATERIAL'                                           749
         AG_PARLIST = 'NONE'                                              749
         CALL AXMATERIAL                                                  749
C    *                                                                    749
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 <===                       749
         CALL AGSRESET                                                    750
         AG_SEEN = 0                                                      750
         AG_COLO = 1                                                      750
         AG_TITLE = 'SELE'                                                750
         AG_EXNAME = 'ATTRIBUTE'                                          750
         AG_PARLIST = 'SEEN_COLO'                                         750
         CALL AXATTRIBUTE                                                 750
C    *                                                                    750
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  750
C    K/2 <===                                                             750
         CALL AGSRESET                                                    752
         AG_DX = SELC_ELCAWID/2                                           752
         AG_DY = SWCA_LENGTH/2                                            752
         AG_DZ = ELETHK/2                                                 752
         AG_TITLE = 'BOX'                                                 752
         AG_EXNAME = 'SHAPE'                                              752
         AG_PARLIST = 'DX_DY_DZ'                                          752
         CALL AXSHAPE                                                     752
C    *                                                                    752
C    Create SWCH :                                                        753
C    Call To Local Procedure SWCH                                         753
         ASSIGN 5880 TO BLOCK_SWCH                                        753
         GO TO  5870                                                      753
5880     CONTINUE                                                         753
C    *                                                                    753
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             754
         CALL AGSRESET                                                    754
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    754
         AG_TITLE = 'SWCH'                                                754
         AG_EXNAME = 'POSITION'                                           754
         AG_PARLIST = 'Z'                                                 754
         CALL AXPOSITION                                                  754
C    *                                                                    754
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  754
C    BETHK/2 <===                                                         754
         CALL AGSRESET                                                    755
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             755
         AG_TITLE = 'SWCH'                                                755
         AG_EXNAME = 'POSITION'                                           755
         AG_PARLIST = 'Z'                                                 755
         CALL AXPOSITION                                                  755
C    *                                                                    755
C    Create SWCS :                                                        756
C    Call To Local Procedure SWCS                                         756
         ASSIGN 5900 TO BLOCK_SWCS                                        756
         GO TO  5890                                                      756
5900     CONTINUE                                                         756
C    *                                                                    756
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  758
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     758
         CALL AGSRESET                                                    758
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              758
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        758
         AG_TITLE = 'SWCS'                                                758
         AG_EXNAME = 'POSITION'                                           758
         AG_PARLIST = 'X_Z'                                               758
         CALL AXPOSITION                                                  758
C    *                                                                    758
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  758
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     758
         CALL AGSRESET                                                    760
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              760
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        760
         AG_TITLE = 'SWCS'                                                760
         AG_EXNAME = 'POSITION'                                           760
         AG_PARLIST = 'X_Z'                                               760
         CALL AXPOSITION                                                  760
C    *                                                                    760
C    Create SWCW :                                                        761
C    Call To Local Procedure SWCW                                         761
         ASSIGN 5920 TO BLOCK_SWCW                                        761
         GO TO  5910                                                      761
5920     CONTINUE                                                         761
C    *                                                                    761
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  762
C    =                                                                    762
         CALL AGSRESET                                                    762
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        762
         AG_TITLE = 'SWCW'                                                762
         AG_EXNAME = 'POSITION'                                           762
         AG_PARLIST = 'Z'                                                 762
         CALL AXPOSITION                                                  762
C    *                                                                    762
C    Create SBOI :                                                        763
C    Call To Local Procedure SBOI                                         763
         ASSIGN 5940 TO BLOCK_SBOI                                        763
         GO TO  5930                                                      763
5940     CONTINUE                                                         763
C    *                                                                    763
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  764
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       764
         CALL AGSRESET                                                    764
         AG_Z = (ELETHK/                                                  764
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          764
         AG_TITLE = 'SBOI'                                                764
         AG_EXNAME = 'POSITION'                                           764
         AG_PARLIST = 'Z'                                                 764
         CALL AXPOSITION                                                  764
C    *                                                                    764
C    Create SGLA :                                                        765
C    Call To Local Procedure SGLA                                         765
         ASSIGN 5960 TO BLOCK_SGLA                                        765
         GO TO  5950                                                      765
5960     CONTINUE                                                         765
C    *                                                                    765
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  766
C    _GLASSTHK/2 <===                                                     766
         CALL AGSRESET                                                    766
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         766
         AG_TITLE = 'SGLA'                                                766
         AG_EXNAME = 'POSITION'                                           766
         AG_PARLIST = 'Z'                                                 766
         CALL AXPOSITION                                                  766
C    *                                                                    766
C    Create SAGP :                                                        767
C    Call To Local Procedure SAGP                                         767
         ASSIGN 5980 TO BLOCK_SAGP                                        767
         GO TO  5970                                                      767
5980     CONTINUE                                                         767
C    *                                                                    767
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  768
C    ==                                                                   768
         CALL AGSRESET                                                    768
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       768
         AG_TITLE = 'SAGP'                                                768
         AG_EXNAME = 'POSITION'                                           768
         AG_PARLIST = 'Z'                                                 768
         CALL AXPOSITION                                                  768
C    *                                                                    768
C    Create SDYE :                                                        769
C    Call To Local Procedure SDYE                                         769
         ASSIGN 6000 TO BLOCK_SDYE                                        769
         GO TO  5990                                                      769
6000     CONTINUE                                                         769
C    *                                                                    769
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  771
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     771
         CALL AGSRESET                                                    771
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  771
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    771
         AG_TITLE = 'SDYE'                                                771
         AG_EXNAME = 'POSITION'                                           771
         AG_PARLIST = 'X_Z'                                               771
         CALL AXPOSITION                                                  771
C    *                                                                    771
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  771
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             771
         CALL AGSRESET                                                    773
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          773
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    773
         AG_TITLE = 'SDYE'                                                773
         AG_EXNAME = 'POSITION'                                           773
         AG_PARLIST = 'X_Z'                                               773
         CALL AXPOSITION                                                  773
C    *                                                                    773
C    Create SECA :                                                        774
C    Call To Local Procedure SECA                                         774
         ASSIGN 6020 TO BLOCK_SECA                                        774
         GO TO  6010                                                      774
6020     CONTINUE                                                         774
C    *                                                                    774
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  776
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    776
         CALL AGSRESET                                                    776
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             776
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        776
         AG_TITLE = 'SECA'                                                776
         AG_EXNAME = 'POSITION'                                           776
         AG_PARLIST = 'X_Z'                                               776
         CALL AXPOSITION                                                  776
C    *                                                                    776
5601  CALL AGSPOPB                                                        777
      GO TO BLOCK_SELE, (5600 ,5610)                                      777
C End of Local Procedure                                                  777
C ----------------------------------------------------------------------  778
C ------------ SDYE                                                       778
C Begin Local Procedure SDYE                                              779
C SDYE IS THE IC CHIP ON THE HYBRID                                       779
5990  CONTINUE                                                            779
         AG_TITLE='SDYE'                                                  779
         AG_MODULE=MODULE                                                 779
         AG_MARK='V00'                                                    779
         CALL AGSPUSH                                                     779
         AG_MODULE=MODULE                                                 779
C    *                                                                    779
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          780
         CALL AGSRESET                                                    780
         AG_TITLE = 'SILICON'                                             780
         AG_EXNAME = 'MATERIAL'                                           780
         AG_PARLIST = 'NONE'                                              780
         CALL AXMATERIAL                                                  780
C    *                                                                    780
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 <===                       780
         CALL AGSRESET                                                    781
         AG_SEEN = 1                                                      781
         AG_COLO = 6                                                      781
         AG_TITLE = 'SDYE'                                                781
         AG_EXNAME = 'ATTRIBUTE'                                          781
         AG_PARLIST = 'SEEN_COLO'                                         781
         CALL AXATTRIBUTE                                                 781
C    *                                                                    781
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  781
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           781
         CALL AGSRESET                                                    783
         AG_DX = SELC_DYEWID/2                                            783
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              783
         AG_DZ = SELC_DYETHK/2                                            783
         AG_TITLE = 'BOX'                                                 783
         AG_EXNAME = 'SHAPE'                                              783
         AG_PARLIST = 'DX_DY_DZ'                                          783
         CALL AXSHAPE                                                     783
C    *                                                                    783
5991  CALL AGSPOPB                                                        784
      GO TO BLOCK_SDYE, (5990 ,6000)                                      784
C End of Local Procedure                                                  784
C ----------------------------------------------------------------------  785
C ------------ SECA                                                       785
C Begin Local Procedure SECA                                              786
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            786
6010  CONTINUE                                                            786
         AG_TITLE='SECA'                                                  786
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
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 <===                       787
         CALL AGSRESET                                                    788
         AG_SEEN = 1                                                      788
         AG_COLO = 2                                                      788
         AG_TITLE = 'SECA'                                                788
         AG_EXNAME = 'ATTRIBUTE'                                          788
         AG_PARLIST = 'SEEN_COLO'                                         788
         CALL AXATTRIBUTE                                                 788
C    *                                                                    788
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  788
C    ABTHK/2 <===                                                         788
         CALL AGSRESET                                                    790
         AG_DX = SELC_CABWID/2                                            790
         AG_DY = SWCA_LENGTH/2                                            790
         AG_DZ = SELC_CABTHK/2                                            790
         AG_TITLE = 'BOX'                                                 790
         AG_EXNAME = 'SHAPE'                                              790
         AG_PARLIST = 'DX_DY_DZ'                                          790
         CALL AXSHAPE                                                     790
C    *                                                                    790
6011  CALL AGSPOPB                                                        791
      GO TO BLOCK_SECA, (6010 ,6020)                                      791
C End of Local Procedure                                                  791
C ----------------------------------------------------------------------  792
C ------------ SBOI                                                       792
C Begin Local Procedure SBOI                                              793
C SBOI IS THE BERILLIA LAYER                                              793
5930  CONTINUE                                                            793
         AG_TITLE='SBOI'                                                  793
         AG_MODULE=MODULE                                                 793
         AG_MARK='V00'                                                    793
         CALL AGSPUSH                                                     793
         AG_MODULE=MODULE                                                 793
C    *                                                                    793
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1 <===                           794
         CALL AGSRESET                                                    794
         AG_A = 9                                                         794
         AG_Z = 4                                                         794
         AG_W = 1                                                         794
         AG_TITLE = 'BE'                                                  794
         AG_EXNAME = 'COMPONENT'                                          794
         AG_PARLIST = 'A_Z_W'                                             794
         CALL AXCOMPONENT                                                 794
C    *                                                                    794
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           794
         CALL AGSRESET                                                    795
         AG_A = 16                                                        795
         AG_Z = 8                                                         795
         AG_W = 1                                                         795
         AG_TITLE = 'O'                                                   795
         AG_EXNAME = 'COMPONENT'                                          795
         AG_PARLIST = 'A_Z_W'                                             795
         CALL AXCOMPONENT                                                 795
C    *                                                                    795
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              795
         CALL AGSRESET                                                    796
         AG_DENS = 2.85                                                   796
         AG_TITLE = 'BEO'                                                 796
         AG_EXNAME = 'MIXTURE'                                            796
         AG_PARLIST = 'DENS'                                              796
         CALL AXMIXTURE                                                   796
C    *                                                                    796
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 <===                       796
         CALL AGSRESET                                                    797
         AG_SEEN = 1                                                      797
         AG_COLO = 6                                                      797
         AG_TITLE = 'SBOI'                                                797
         AG_EXNAME = 'ATTRIBUTE'                                          797
         AG_PARLIST = 'SEEN_COLO'                                         797
         CALL AXATTRIBUTE                                                 797
C    *                                                                    797
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  797
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          797
         CALL AGSRESET                                                    799
         AG_DX = SELC_ELCAWID/2                                           799
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              799
         AG_DZ = SELC_BEOTHK/2                                            799
         AG_TITLE = 'BOX'                                                 799
         AG_EXNAME = 'SHAPE'                                              799
         AG_PARLIST = 'DX_DY_DZ'                                          799
         CALL AXSHAPE                                                     799
C    *                                                                    799
5931  CALL AGSPOPB                                                        800
      GO TO BLOCK_SBOI, (5930 ,5940)                                      800
C End of Local Procedure                                                  800
C ----------------------------------------------------------------------  801
C ------------ SAGP                                                       801
C Begin Local Procedure SAGP                                              802
C SAGP IS THE SILVER-PALLADIUM LAYER                                      802
5970  CONTINUE                                                            802
         AG_TITLE='SAGP'                                                  802
         AG_MODULE=MODULE                                                 802
         AG_MARK='V00'                                                    802
         CALL AGSPUSH                                                     802
         AG_MODULE=MODULE                                                 802
C    *                                                                    802
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1 <===                        803
         CALL AGSRESET                                                    803
         AG_A = 108                                                       803
         AG_Z = 47                                                        803
         AG_W = 1                                                         803
         AG_TITLE = 'AG'                                                  803
         AG_EXNAME = 'COMPONENT'                                          803
         AG_PARLIST = 'A_Z_W'                                             803
         CALL AXCOMPONENT                                                 803
C    *                                                                    803
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1 <===                        803
         CALL AGSRESET                                                    804
         AG_A = 106                                                       804
         AG_Z = 46                                                        804
         AG_W = 1                                                         804
         AG_TITLE = 'PD'                                                  804
         AG_EXNAME = 'COMPONENT'                                          804
         AG_PARLIST = 'A_Z_W'                                             804
         CALL AXCOMPONENT                                                 804
C    *                                                                    804
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            804
         CALL AGSRESET                                                    805
         AG_DENS = 11.25                                                  805
         AG_TITLE = 'AGPD'                                                805
         AG_EXNAME = 'MIXTURE'                                            805
         AG_PARLIST = 'DENS'                                              805
         CALL AXMIXTURE                                                   805
C    *                                                                    805
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 <===                       805
         CALL AGSRESET                                                    806
         AG_SEEN = 1                                                      806
         AG_COLO = 2                                                      806
         AG_TITLE = 'SAGP'                                                806
         AG_EXNAME = 'ATTRIBUTE'                                          806
         AG_PARLIST = 'SEEN_COLO'                                         806
         CALL AXATTRIBUTE                                                 806
C    *                                                                    806
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  806
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         806
         CALL AGSRESET                                                    808
         AG_DX = SELC_ELCAWID/2                                           808
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              808
         AG_DZ = SELC_AGPDTHK/2                                           808
         AG_TITLE = 'BOX'                                                 808
         AG_EXNAME = 'SHAPE'                                              808
         AG_PARLIST = 'DX_DY_DZ'                                          808
         CALL AXSHAPE                                                     808
C    *                                                                    808
5971  CALL AGSPOPB                                                        809
      GO TO BLOCK_SAGP, (5970 ,5980)                                      809
C End of Local Procedure                                                  809
C ----------------------------------------------------------------------  810
C ------------ SGLA                                                       810
C Begin Local Procedure SGLA                                              811
C SGLA IS THE INSULATING GLASS LAYER                                      811
5950  CONTINUE                                                            811
         AG_TITLE='SGLA'                                                  811
         AG_MODULE=MODULE                                                 811
         AG_MARK='V00'                                                    811
         CALL AGSPUSH                                                     811
         AG_MODULE=MODULE                                                 811
C    *                                                                    811
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         812
         CALL AGSRESET                                                    812
         AG_A = 28                                                        812
         AG_Z = 14                                                        812
         AG_W = 1                                                         812
         AG_TITLE = 'SI'                                                  812
         AG_EXNAME = 'COMPONENT'                                          812
         AG_PARLIST = 'A_Z_W'                                             812
         CALL AXCOMPONENT                                                 812
C    *                                                                    812
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           812
         CALL AGSRESET                                                    813
         AG_A = 16                                                        813
         AG_Z = 8                                                         813
         AG_W = 2                                                         813
         AG_TITLE = 'O'                                                   813
         AG_EXNAME = 'COMPONENT'                                          813
         AG_PARLIST = 'A_Z_W'                                             813
         CALL AXCOMPONENT                                                 813
C    *                                                                    813
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             813
         CALL AGSRESET                                                    814
         AG_DENS = 2.2                                                    814
         AG_TITLE = 'GLASS'                                               814
         AG_EXNAME = 'MIXTURE'                                            814
         AG_PARLIST = 'DENS'                                              814
         CALL AXMIXTURE                                                   814
C    *                                                                    814
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 <===                       814
         CALL AGSRESET                                                    815
         AG_SEEN = 1                                                      815
         AG_COLO = 6                                                      815
         AG_TITLE = 'SGLA'                                                815
         AG_EXNAME = 'ATTRIBUTE'                                          815
         AG_PARLIST = 'SEEN_COLO'                                         815
         CALL AXATTRIBUTE                                                 815
C    *                                                                    815
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  815
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        815
         CALL AGSRESET                                                    817
         AG_DX = SELC_ELCAWID/2                                           817
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              817
         AG_DZ = SELC_GLASSTHK/2                                          817
         AG_TITLE = 'BOX'                                                 817
         AG_EXNAME = 'SHAPE'                                              817
         AG_PARLIST = 'DX_DY_DZ'                                          817
         CALL AXSHAPE                                                     817
C    *                                                                    817
5951  CALL AGSPOPB                                                        818
      GO TO BLOCK_SGLA, (5950 ,5960)                                      818
C End of Local Procedure                                                  818
C ----------------------------------------------------------------------  819
C ------------ SWCH                                                       819
C Begin Local Procedure SWCH                                              820
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      820
5870  CONTINUE                                                            820
         AG_TITLE='SWCH'                                                  820
         AG_MODULE=MODULE                                                 820
         AG_MARK='V00'                                                    820
         CALL AGSPUSH                                                     820
         AG_MODULE=MODULE                                                 820
C    *                                                                    820
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        821
         CALL AGSRESET                                                    821
         AG_TITLE = 'BERILLIUM'                                           821
         AG_EXNAME = 'MATERIAL'                                           821
         AG_PARLIST = 'NONE'                                              821
         CALL AXMATERIAL                                                  821
C    *                                                                    821
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 <===                       821
         CALL AGSRESET                                                    822
         AG_SEEN = 1                                                      822
         AG_COLO = 2                                                      822
         AG_TITLE = 'SWCH'                                                822
         AG_EXNAME = 'ATTRIBUTE'                                          822
         AG_PARLIST = 'SEEN_COLO'                                         822
         CALL AXATTRIBUTE                                                 822
C    *                                                                    822
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  822
C    BETHK/2 <===                                                         822
         CALL AGSRESET                                                    824
         AG_DX = SELC_ELCAWID/2                                           824
         AG_DY = SWCA_LENGTH/2                                            824
         AG_DZ = SELC_BETHK/2                                             824
         AG_TITLE = 'BOX'                                                 824
         AG_EXNAME = 'SHAPE'                                              824
         AG_PARLIST = 'DX_DY_DZ'                                          824
         CALL AXSHAPE                                                     824
C    *                                                                    824
5871  CALL AGSPOPB                                                        825
      GO TO BLOCK_SWCH, (5870 ,5880)                                      825
C End of Local Procedure                                                  825
C ----------------------------------------------------------------------  826
C ------------ SWCS                                                       826
C Begin Local Procedure SWCS                                              827
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                827
5890  CONTINUE                                                            827
         AG_TITLE='SWCS'                                                  827
         AG_MODULE=MODULE                                                 827
         AG_MARK='V00'                                                    827
         CALL AGSPUSH                                                     827
         AG_MODULE=MODULE                                                 827
C    *                                                                    827
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        828
         CALL AGSRESET                                                    828
         AG_TITLE = 'BERILLIUM'                                           828
         AG_EXNAME = 'MATERIAL'                                           828
         AG_PARLIST = 'NONE'                                              828
         CALL AXMATERIAL                                                  828
C    *                                                                    828
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 <===                       828
         CALL AGSRESET                                                    829
         AG_SEEN = 1                                                      829
         AG_COLO = 2                                                      829
         AG_TITLE = 'SWCS'                                                829
         AG_EXNAME = 'ATTRIBUTE'                                          829
         AG_PARLIST = 'SEEN_COLO'                                         829
         CALL AXATTRIBUTE                                                 829
C    *                                                                    829
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  829
C    TTHK/2 <===                                                          829
         CALL AGSRESET                                                    831
         AG_DX = SELC_BETHK/2                                             831
         AG_DY = SWCA_LENGTH/2                                            831
         AG_DZ = SELC_WATTHK/2                                            831
         AG_TITLE = 'BOX'                                                 831
         AG_EXNAME = 'SHAPE'                                              831
         AG_PARLIST = 'DX_DY_DZ'                                          831
         CALL AXSHAPE                                                     831
C    *                                                                    831
5891  CALL AGSPOPB                                                        832
      GO TO BLOCK_SWCS, (5890 ,5900)                                      832
C End of Local Procedure                                                  832
C ----------------------------------------------------------------------  833
C ------------ SWCW                                                       833
C Begin Local Procedure SWCW                                              834
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       834
5910  CONTINUE                                                            834
         AG_TITLE='SWCW'                                                  834
         AG_MODULE=MODULE                                                 834
         AG_MARK='V00'                                                    834
         CALL AGSPUSH                                                     834
         AG_MODULE=MODULE                                                 834
C    *                                                                    834
C    Geant MATERIAL : WATER ===> inherited arguments used <===            835
         CALL AGSRESET                                                    835
         AG_TITLE = 'WATER'                                               835
         AG_EXNAME = 'MATERIAL'                                           835
         AG_PARLIST = 'NONE'                                              835
         CALL AXMATERIAL                                                  835
C    *                                                                    835
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 <===                       835
         CALL AGSRESET                                                    836
         AG_SEEN = 1                                                      836
         AG_COLO = 6                                                      836
         AG_TITLE = 'SWCW'                                                836
         AG_EXNAME = 'ATTRIBUTE'                                          836
         AG_PARLIST = 'SEEN_COLO'                                         836
         CALL AXATTRIBUTE                                                 836
C    *                                                                    836
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  836
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     836
         CALL AGSRESET                                                    838
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          838
         AG_DY = SWCA_LENGTH/2                                            838
         AG_DZ = (SELC_WATTHK/2)                                          838
         AG_TITLE = 'BOX'                                                 838
         AG_EXNAME = 'SHAPE'                                              838
         AG_PARLIST = 'DX_DY_DZ'                                          838
         CALL AXSHAPE                                                     838
C    *                                                                    838
5911  CALL AGSPOPB                                                        839
      GO TO BLOCK_SWCW, (5910 ,5920)                                      839
C End of Local Procedure                                                  839
C ----------------------------------------------------------------------  840
C ------------ SIRT                                                       840
C Begin Local Procedure SIRT                                              841
C SIRT IS THE SVT INNER END RING TUBE                                     841
5060  CONTINUE                                                            841
         AG_TITLE='SIRT'                                                  841
         AG_MODULE=MODULE                                                 841
         AG_MARK='V00'                                                    841
         CALL AGSPUSH                                                     841
         AG_MODULE=MODULE                                                 841
C    *                                                                    841
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        842
         CALL AGSRESET                                                    842
         AG_TITLE = 'BERILLIUM'                                           842
         AG_EXNAME = 'MATERIAL'                                           842
         AG_PARLIST = 'NONE'                                              842
         CALL AXMATERIAL                                                  842
C    *                                                                    842
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       842
         CALL AGSRESET                                                    843
         AG_SEEN = 1                                                      843
         AG_COLO = 2                                                      843
         AG_TITLE = 'SIRT'                                                843
         AG_EXNAME = 'ATTRIBUTE'                                          843
         AG_PARLIST = 'SEEN_COLO'                                         843
         CALL AXATTRIBUTE                                                 843
C    *                                                                    843
         IR_RMIN = COS(PI/8.0)                                            843
         IR_RMIN=IR_RMIN+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/           845
     *   8.0)*SIN(PI/8.0))                                                845
         IR_RMIN=IR_RMIN*SERG_IRNGPRMN                                    846
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  848
C    DRNGTH/2 <===                                                        848
         CALL AGSRESET                                                    848
         AG_RMIN = IR_RMIN                                                848
         AG_RMAX = SERG_IRNGTRMX                                          848
         AG_DZ = SERG_ENDRNGTH/2                                          848
         AG_TITLE = 'TUBE'                                                848
         AG_EXNAME = 'SHAPE'                                              848
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      848
         CALL AXSHAPE                                                     848
C    *                                                                    848
5061  CALL AGSPOPB                                                        849
      GO TO BLOCK_SIRT, (5060 ,5070)                                      849
C End of Local Procedure                                                  849
C ----------------------------------------------------------------------  850
C ------------ SIRP                                                       850
C Begin Local Procedure SIRP                                              851
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 851
5040  CONTINUE                                                            851
         AG_TITLE='SIRP'                                                  851
         AG_MODULE=MODULE                                                 851
         AG_MARK='V00'                                                    851
         CALL AGSPUSH                                                     851
         AG_MODULE=MODULE                                                 851
C    *                                                                    851
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        852
         CALL AGSRESET                                                    852
         AG_TITLE = 'BERILLIUM'                                           852
         AG_EXNAME = 'MATERIAL'                                           852
         AG_PARLIST = 'NONE'                                              852
         CALL AXMATERIAL                                                  852
C    *                                                                    852
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       852
         CALL AGSRESET                                                    853
         AG_SEEN = 1                                                      853
         AG_COLO = 2                                                      853
         AG_TITLE = 'SIRP'                                                853
         AG_EXNAME = 'ATTRIBUTE'                                          853
         AG_PARLIST = 'SEEN_COLO'                                         853
         CALL AXATTRIBUTE                                                 853
C    *                                                                    853
         ROU = COS(PI/8.0)                                                853
         ROU=ROU+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/8.0)*SIN(PI/       855
     *   8.0))                                                            855
         ROU = SERG_IRNGTRMX/ROU                                          856
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  859
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  859
C    OU,ROU} <===                                                         859
         CALL AGSRESET                                                    859
         AG_PHI1 = 0                                                      859
         AG_DPHI = 360                                                    859
         AG_NPDIV = 8                                                     859
         AG_NZ = 2                                                        859
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        859
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        859
         AG_RMN(1)=SERG_IRNGPRMN                                          859
         AG_RMN(2)=SERG_IRNGPRMN                                          859
         AG_RMX(1)=ROU                                                    859
         AG_RMX(2)=ROU                                                    859
         AG_TITLE = 'PGON'                                                859
         AG_EXNAME = 'SHAPE'                                              859
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     859
         CALL AXSHAPE                                                     859
C    *                                                                    859
5041  CALL AGSPOPB                                                        860
      GO TO BLOCK_SIRP, (5040 ,5050)                                      860
C End of Local Procedure                                                  860
C ----------------------------------------------------------------------  861
C ------------ SOER                                                       861
C Begin Local Procedure SOER                                              862
C SOER IS THE SVT OUTER END RING                                          862
5080  CONTINUE                                                            862
         AG_TITLE='SOER'                                                  862
         AG_MODULE=MODULE                                                 862
         AG_MARK='V00'                                                    862
         CALL AGSPUSH                                                     862
         AG_MODULE=MODULE                                                 862
C    *                                                                    862
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        863
         CALL AGSRESET                                                    863
         AG_TITLE = 'BERILLIUM'                                           863
         AG_EXNAME = 'MATERIAL'                                           863
         AG_PARLIST = 'NONE'                                              863
         CALL AXMATERIAL                                                  863
C    *                                                                    863
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       863
         CALL AGSRESET                                                    864
         AG_SEEN = 1                                                      864
         AG_COLO = 2                                                      864
         AG_TITLE = 'SOER'                                                864
         AG_EXNAME = 'ATTRIBUTE'                                          864
         AG_PARLIST = 'SEEN_COLO'                                         864
         CALL AXATTRIBUTE                                                 864
C    *                                                                    864
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  864
C    ERG_ENDRNGTH/2 <===                                                  864
         CALL AGSRESET                                                    866
         AG_RMIN = SERG_ORNGRMIN                                          866
         AG_RMAX = SERG_ORNGRMAX                                          866
         AG_DZ = SERG_ENDRNGTH/2                                          866
         AG_TITLE = 'TUBE'                                                866
         AG_EXNAME = 'SHAPE'                                              866
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      866
         CALL AXSHAPE                                                     866
C    *                                                                    866
5081  CALL AGSPOPB                                                        867
      GO TO BLOCK_SOER, (5080 ,5090)                                      867
C End of Local Procedure                                                  867
C ----------------------------------------------------------------------  868
C ------------ SIES                                                       868
C Begin Local Procedure SIES                                              869
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         869
5140  CONTINUE                                                            869
         AG_TITLE='SIES'                                                  869
         AG_MODULE=MODULE                                                 869
         AG_MARK='V00'                                                    869
         CALL AGSPUSH                                                     869
         AG_MODULE=MODULE                                                 869
C    *                                                                    869
C    Geant MATERIAL : AIR ===> inherited arguments used <===              870
         CALL AGSRESET                                                    870
         AG_TITLE = 'AIR'                                                 870
         AG_EXNAME = 'MATERIAL'                                           870
         AG_PARLIST = 'NONE'                                              870
         CALL AXMATERIAL                                                  870
C    *                                                                    870
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       870
         CALL AGSRESET                                                    871
         AG_SEEN = 0                                                      871
         AG_COLO = 1                                                      871
         AG_TITLE = 'SIES'                                                871
         AG_EXNAME = 'ATTRIBUTE'                                          871
         AG_PARLIST = 'SEEN_COLO'                                         871
         CALL AXATTRIBUTE                                                 871
C    *                                                                    871
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  871
C    ACK_Z-ENDRNG_Z) <===                                                 871
         CALL AGSRESET                                                    873
         AG_RMIN = IR_RMIN                                                873
         AG_RMAX = SERG_IRNGTRMX                                          873
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   873
         AG_TITLE = 'TUBE'                                                873
         AG_EXNAME = 'SHAPE'                                              873
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      873
         CALL AXSHAPE                                                     873
C    *                                                                    873
C    Create SISM :                                                        874
C    Call To Local Procedure SISM                                         874
         ASSIGN 6040 TO BLOCK_SISM                                        874
         GO TO  6030                                                      874
6040     CONTINUE                                                         874
C    *                                                                    874
5141  CALL AGSPOPB                                                        875
      GO TO BLOCK_SIES, (5140 ,5150)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ SISM                                                       876
C Begin Local Procedure SISM                                              877
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        877
6030  CONTINUE                                                            877
         AG_TITLE='SISM'                                                  877
         AG_MODULE=MODULE                                                 877
         AG_MARK='V00'                                                    877
         CALL AGSPUSH                                                     877
         AG_MODULE=MODULE                                                 877
C    *                                                                    877
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       878
         CALL AGSRESET                                                    878
         AG_SEEN = 0                                                      878
         AG_COLO = 1                                                      878
         AG_TITLE = 'SISM'                                                878
         AG_EXNAME = 'ATTRIBUTE'                                          878
         AG_PARLIST = 'SEEN_COLO'                                         878
         CALL AXATTRIBUTE                                                 878
C    *                                                                    878
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                878
         CALL AGSRESET                                                    879
         AG_NDIV = 4                                                      879
         AG_IAXIS = 2                                                     879
         AG_C0 = 45                                                       879
         AG_TITLE = 'DIVISION'                                            879
         AG_EXNAME = 'SHAPE'                                              879
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     879
         CALL AXSHAPE                                                     879
C    *                                                                    879
C    Create SCRW :                                                        880
C    Call To Local Procedure SCRW                                         880
         ASSIGN 6060 TO BLOCK_SCRW                                        880
         GO TO  6050                                                      880
6060     CONTINUE                                                         880
C    *                                                                    880
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  881
C    ===                                                                  881
         CALL AGSRESET                                                    881
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      881
         AG_TITLE = 'SCRW'                                                881
         AG_EXNAME = 'POSITION'                                           881
         AG_PARLIST = 'X'                                                 881
         CALL AXPOSITION                                                  881
C    *                                                                    881
6031  CALL AGSPOPB                                                        882
      GO TO BLOCK_SISM, (6030 ,6040)                                      882
C End of Local Procedure                                                  882
C ----------------------------------------------------------------------  883
C ------------ SOES                                                       883
C Begin Local Procedure SOES                                              884
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         884
5120  CONTINUE                                                            884
         AG_TITLE='SOES'                                                  884
         AG_MODULE=MODULE                                                 884
         AG_MARK='V00'                                                    884
         CALL AGSPUSH                                                     884
         AG_MODULE=MODULE                                                 884
C    *                                                                    884
C    Geant MATERIAL : AIR ===> inherited arguments used <===              885
         CALL AGSRESET                                                    885
         AG_TITLE = 'AIR'                                                 885
         AG_EXNAME = 'MATERIAL'                                           885
         AG_PARLIST = 'NONE'                                              885
         CALL AXMATERIAL                                                  885
C    *                                                                    885
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       885
         CALL AGSRESET                                                    886
         AG_SEEN = 0                                                      886
         AG_COLO = 1                                                      886
         AG_TITLE = 'SOES'                                                886
         AG_EXNAME = 'ATTRIBUTE'                                          886
         AG_PARLIST = 'SEEN_COLO'                                         886
         CALL AXATTRIBUTE                                                 886
C    *                                                                    886
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  886
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           886
         CALL AGSRESET                                                    888
         AG_RMIN = SERG_ORNGRMIN                                          888
         AG_RMAX = SERG_ORNGRMAX                                          888
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   888
         AG_TITLE = 'TUBE'                                                888
         AG_EXNAME = 'SHAPE'                                              888
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      888
         CALL AXSHAPE                                                     888
C    *                                                                    888
C    Create SOSM :                                                        889
C    Call To Local Procedure SOSM                                         889
         ASSIGN 6080 TO BLOCK_SOSM                                        889
         GO TO  6070                                                      889
6080     CONTINUE                                                         889
C    *                                                                    889
5121  CALL AGSPOPB                                                        890
      GO TO BLOCK_SOES, (5120 ,5130)                                      890
C End of Local Procedure                                                  890
C ----------------------------------------------------------------------  891
C ------------ SOSM                                                       891
C Begin Local Procedure SOSM                                              892
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        892
6070  CONTINUE                                                            892
         AG_TITLE='SOSM'                                                  892
         AG_MODULE=MODULE                                                 892
         AG_MARK='V00'                                                    892
         CALL AGSPUSH                                                     892
         AG_MODULE=MODULE                                                 892
C    *                                                                    892
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       893
         CALL AGSRESET                                                    893
         AG_SEEN = 0                                                      893
         AG_COLO = 1                                                      893
         AG_TITLE = 'SOSM'                                                893
         AG_EXNAME = 'ATTRIBUTE'                                          893
         AG_PARLIST = 'SEEN_COLO'                                         893
         CALL AXATTRIBUTE                                                 893
C    *                                                                    893
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                893
         CALL AGSRESET                                                    894
         AG_NDIV = 4                                                      894
         AG_IAXIS = 2                                                     894
         AG_C0 = 45                                                       894
         AG_TITLE = 'DIVISION'                                            894
         AG_EXNAME = 'SHAPE'                                              894
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     894
         CALL AXSHAPE                                                     894
C    *                                                                    894
C    Create SCRW :                                                        895
C    Call To Local Procedure SCRW                                         895
         ASSIGN 6090 TO BLOCK_SCRW                                        895
         GO TO  6050                                                      895
6090     CONTINUE                                                         895
C    *                                                                    895
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  896
C    ===                                                                  896
         CALL AGSRESET                                                    896
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      896
         AG_TITLE = 'SCRW'                                                896
         AG_EXNAME = 'POSITION'                                           896
         AG_PARLIST = 'X'                                                 896
         CALL AXPOSITION                                                  896
C    *                                                                    896
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  896
C    ===                                                                  896
         CALL AGSRESET                                                    897
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      897
         AG_TITLE = 'SCRW'                                                897
         AG_EXNAME = 'POSITION'                                           897
         AG_PARLIST = 'X'                                                 897
         CALL AXPOSITION                                                  897
C    *                                                                    897
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  897
C    =SSUP_ERJ2Y <===                                                     897
         CALL AGSRESET                                                    898
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      898
         AG_Y = SSUP_ERJ2Y                                                898
         AG_TITLE = 'SCRW'                                                898
         AG_EXNAME = 'POSITION'                                           898
         AG_PARLIST = 'X_Y'                                               898
         CALL AXPOSITION                                                  898
C    *                                                                    898
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  898
C    =-SSUP_ERJ2Y <===                                                    898
         CALL AGSRESET                                                    899
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      899
         AG_Y = -SSUP_ERJ2Y                                               899
         AG_TITLE = 'SCRW'                                                899
         AG_EXNAME = 'POSITION'                                           899
         AG_PARLIST = 'X_Y'                                               899
         CALL AXPOSITION                                                  899
C    *                                                                    899
6071  CALL AGSPOPB                                                        900
      GO TO BLOCK_SOSM, (6070 ,6080)                                      900
C End of Local Procedure                                                  900
C ----------------------------------------------------------------------  901
C ------------ SBRG                                                       901
C Begin Local Procedure SBRG                                              902
C SBRG IS THE BRACKET JOINING THE END RINGS                               902
5100  CONTINUE                                                            902
         AG_TITLE='SBRG'                                                  902
         AG_MODULE=MODULE                                                 902
         AG_MARK='V00'                                                    902
         CALL AGSPUSH                                                     902
         AG_MODULE=MODULE                                                 902
C    *                                                                    902
         RIN=SERG_IRNGPRMN                                                903
         ROU=SERG_ORNGRMAX                                                904
C    Geant MATERIAL : AIR ===> inherited arguments used <===              905
         CALL AGSRESET                                                    905
         AG_TITLE = 'AIR'                                                 905
         AG_EXNAME = 'MATERIAL'                                           905
         AG_PARLIST = 'NONE'                                              905
         CALL AXMATERIAL                                                  905
C    *                                                                    905
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       905
         CALL AGSRESET                                                    906
         AG_SEEN = 0                                                      906
         AG_COLO = 1                                                      906
         AG_TITLE = 'SBRG'                                                906
         AG_EXNAME = 'ATTRIBUTE'                                          906
         AG_PARLIST = 'SEEN_COLO'                                         906
         CALL AXATTRIBUTE                                                 906
C    *                                                                    906
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      906
         CALL AGSRESET                                                    907
         AG_RMIN = RIN                                                    907
         AG_RMAX = ROU                                                    907
         AG_DZ = SSUP_ERJTHK/2                                            907
         AG_TITLE = 'TUBE'                                                907
         AG_EXNAME = 'SHAPE'                                              907
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      907
         CALL AXSHAPE                                                     907
C    *                                                                    907
C    Create SBRM :                                                        908
C    Call To Local Procedure SBRM                                         908
         ASSIGN 6110 TO BLOCK_SBRM                                        908
         GO TO  6100                                                      908
6110     CONTINUE                                                         908
C    *                                                                    908
5101  CALL AGSPOPB                                                        909
      GO TO BLOCK_SBRG, (5100 ,5110)                                      909
C End of Local Procedure                                                  909
C ----------------------------------------------------------------------  910
C ------------ SBRM                                                       910
C Begin Local Procedure SBRM                                              911
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          911
6100  CONTINUE                                                            911
         AG_TITLE='SBRM'                                                  911
         AG_MODULE=MODULE                                                 911
         AG_MARK='V00'                                                    911
         CALL AGSPUSH                                                     911
         AG_MODULE=MODULE                                                 911
C    *                                                                    911
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       912
         CALL AGSRESET                                                    912
         AG_SEEN = 0                                                      912
         AG_COLO = 1                                                      912
         AG_TITLE = 'SBRM'                                                912
         AG_EXNAME = 'ATTRIBUTE'                                          912
         AG_PARLIST = 'SEEN_COLO'                                         912
         CALL AXATTRIBUTE                                                 912
C    *                                                                    912
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                912
         CALL AGSRESET                                                    913
         AG_NDIV = 4                                                      913
         AG_IAXIS = 2                                                     913
         AG_C0 = 45                                                       913
         AG_TITLE = 'DIVISION'                                            913
         AG_EXNAME = 'SHAPE'                                              913
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     913
         CALL AXSHAPE                                                     913
C    *                                                                    913
C    Create SBRI :                                                        914
C    Call To Local Procedure SBRI                                         914
         ASSIGN 6130 TO BLOCK_SBRI                                        914
         GO TO  6120                                                      914
6130     CONTINUE                                                         914
C    *                                                                    914
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        915
         CALL AGSRESET                                                    915
         AG_X = SSUP_ERJRAD                                               915
         AG_TITLE = 'SBRI'                                                915
         AG_EXNAME = 'POSITION'                                           915
         AG_PARLIST = 'X'                                                 915
         CALL AXPOSITION                                                  915
C    *                                                                    915
6101  CALL AGSPOPB                                                        916
      GO TO BLOCK_SBRM, (6100 ,6110)                                      916
C End of Local Procedure                                                  916
C ----------------------------------------------------------------------  917
C ------------ SBRI                                                       917
C Begin Local Procedure SBRI                                              918
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               918
6120  CONTINUE                                                            918
         AG_TITLE='SBRI'                                                  918
         AG_MODULE=MODULE                                                 918
         AG_MARK='V00'                                                    918
         CALL AGSPUSH                                                     918
         AG_MODULE=MODULE                                                 918
C    *                                                                    918
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       919
         CALL AGSRESET                                                    919
         AG_SEEN = 1                                                      919
         AG_COLO = 2                                                      919
         AG_TITLE = 'SBRI'                                                919
         AG_EXNAME = 'ATTRIBUTE'                                          919
         AG_PARLIST = 'SEEN_COLO'                                         919
         CALL AXATTRIBUTE                                                 919
C    *                                                                    919
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        919
         CALL AGSRESET                                                    920
         AG_TITLE = 'BERILLIUM'                                           920
         AG_EXNAME = 'MATERIAL'                                           920
         AG_PARLIST = 'NONE'                                              920
         CALL AXMATERIAL                                                  920
C    *                                                                    920
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  920
C    RJTHK/2 <===                                                         920
         CALL AGSRESET                                                    922
         AG_DX = SSUP_ERJLEN/2                                            922
         AG_DY = SSUP_ERJWID/2                                            922
         AG_DZ = SSUP_ERJTHK/2                                            922
         AG_TITLE = 'BOX'                                                 922
         AG_EXNAME = 'SHAPE'                                              922
         AG_PARLIST = 'DX_DY_DZ'                                          922
         CALL AXSHAPE                                                     922
C    *                                                                    922
6121  CALL AGSPOPB                                                        923
      GO TO BLOCK_SBRI, (6120 ,6130)                                      923
C End of Local Procedure                                                  923
C ----------------------------------------------------------------------  924
C ------------ SROD                                                       924
C Begin Local Procedure SROD                                              925
C SROD IS THE SVT CARBON COMPOSITE SUPPORT ROD                            925
5240  CONTINUE                                                            925
         AG_TITLE='SROD'                                                  925
         AG_MODULE=MODULE                                                 925
         AG_MARK='V00'                                                    925
         CALL AGSPUSH                                                     925
         AG_MODULE=MODULE                                                 925
C    *                                                                    925
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           926
         CALL AGSRESET                                                    926
         AG_TITLE = 'CARBON'                                              926
         AG_EXNAME = 'MATERIAL'                                           926
         AG_PARLIST = 'NONE'                                              926
         CALL AXMATERIAL                                                  926
C    *                                                                    926
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       926
         CALL AGSRESET                                                    927
         AG_SEEN = 1                                                      927
         AG_COLO = 2                                                      927
         AG_TITLE = 'SROD'                                                927
         AG_EXNAME = 'ATTRIBUTE'                                          927
         AG_PARLIST = 'SEEN_COLO'                                         927
         CALL AXATTRIBUTE                                                 927
C    *                                                                    927
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  927
C    P_RODLEN/2 <===                                                      927
         CALL AGSRESET                                                    929
         AG_RMIN = SSUP_RODID/2                                           929
         AG_RMAX = SSUP_RODOD/2                                           929
         AG_DZ = SSUP_RODLEN/2                                            929
         AG_TITLE = 'TUBE'                                                929
         AG_EXNAME = 'SHAPE'                                              929
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      929
         CALL AXSHAPE                                                     929
C    *                                                                    929
5241  CALL AGSPOPB                                                        930
      GO TO BLOCK_SROD, (5240 ,5250)                                      930
C End of Local Procedure                                                  930
C ----------------------------------------------------------------------  931
C ------------ SCON                                                       931
C Begin Local Procedure SCON                                              932
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               932
5200  CONTINUE                                                            932
         AG_TITLE='SCON'                                                  932
         AG_MODULE=MODULE                                                 932
         AG_MARK='V00'                                                    932
         CALL AGSPUSH                                                     932
         AG_MODULE=MODULE                                                 932
C    *                                                                    932
         CONE_THK1=SSUP_GRPHTHK                                           933
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                934
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              935
C    Geant MATERIAL : AIR ===> inherited arguments used <===              936
         CALL AGSRESET                                                    936
         AG_TITLE = 'AIR'                                                 936
         AG_EXNAME = 'MATERIAL'                                           936
         AG_PARLIST = 'NONE'                                              936
         CALL AXMATERIAL                                                  936
C    *                                                                    936
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       936
         CALL AGSRESET                                                    937
         AG_SEEN = 0                                                      937
         AG_COLO = 1                                                      937
         AG_TITLE = 'SCON'                                                937
         AG_EXNAME = 'ATTRIBUTE'                                          937
         AG_PARLIST = 'SEEN_COLO'                                         937
         CALL AXATTRIBUTE                                                 937
C    *                                                                    937
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  937
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  937
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  937
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  937
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  937
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  937
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      937
         CALL AGSRESET                                                    947
         AG_PHI1 = 0                                                      947
         AG_DPHI = 360                                                    947
         AG_NZ = 7                                                        947
         AG_ZI(1)=SSUP_CONE1ZMN                                           947
         AG_ZI(2)=SSUP_RODLEN/2                                           947
         AG_ZI(3)=SSUP_RODLEN/2                                           947
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              947
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              947
         AG_ZI(6)=SSUP_CONE3ZMX                                           947
         AG_ZI(7)=SSUP_CONE4ZMX                                           947
         AG_RMN(1)=SSUP_CON1IDMN                                          947
         AG_RMN(2)=SSUP_CON1IDMN                                          947
         AG_RMN(3)=SSUP_CON1IDMN                                          947
         AG_RMN(4)=SSUP_CON1IDMN                                          947
         AG_RMN(5)=SSUP_CON3IDMN                                          947
         AG_RMN(6)=SSUP_CON4IDMN                                          947
         AG_RMN(7)=SSUP_CON4IDMX                                          947
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                947
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                947
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                947
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                947
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                947
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                947
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                947
         AG_TITLE = 'PCON'                                                947
         AG_EXNAME = 'SHAPE'                                              947
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           947
         CALL AXSHAPE                                                     947
C    *                                                                    947
C    Create SGRA :                                                        948
C    Call To Local Procedure SGRA                                         948
         ASSIGN 6150 TO BLOCK_SGRA                                        948
         GO TO  6140                                                      948
6150     CONTINUE                                                         948
C    *                                                                    948
C    Geant POSITION : SGRA ===> inherited arguments used <===             949
         CALL AGSRESET                                                    949
         AG_TITLE = 'SGRA'                                                949
         AG_EXNAME = 'POSITION'                                           949
         AG_PARLIST = 'NONE'                                              949
         CALL AXPOSITION                                                  949
C    *                                                                    949
C    Create STAP :                                                        950
C    Call To Local Procedure STAP                                         950
         ASSIGN 6170 TO BLOCK_STAP                                        950
         GO TO  6160                                                      950
6170     CONTINUE                                                         950
C    *                                                                    950
C    Geant POSITION : STAP ===> inherited arguments used <===             951
         CALL AGSRESET                                                    951
         AG_TITLE = 'STAP'                                                951
         AG_EXNAME = 'POSITION'                                           951
         AG_PARLIST = 'NONE'                                              951
         CALL AXPOSITION                                                  951
C    *                                                                    951
C    Create STAC :                                                        952
C    Call To Local Procedure STAC                                         952
         ASSIGN 6190 TO BLOCK_STAC                                        952
         GO TO  6180                                                      952
6190     CONTINUE                                                         952
C    *                                                                    952
C    Geant POSITION : STAC ===> inherited arguments used <===             953
         CALL AGSRESET                                                    953
         AG_TITLE = 'STAC'                                                953
         AG_EXNAME = 'POSITION'                                           953
         AG_PARLIST = 'NONE'                                              953
         CALL AXPOSITION                                                  953
C    *                                                                    953
C    Create SHLA :                                                        954
C    Call To Local Procedure SHLA                                         954
         ASSIGN 6210 TO BLOCK_SHLA                                        954
         GO TO  6200                                                      954
6210     CONTINUE                                                         954
C    *                                                                    954
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  956
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             956
         CALL AGSRESET                                                    956
         AG_Z = SSUP_RODLEN/                                              956
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    956
         AG_TITLE = 'SHLA'                                                956
         AG_EXNAME = 'POSITION'                                           956
         AG_PARLIST = 'Z'                                                 956
         CALL AXPOSITION                                                  956
C    *                                                                    956
C    Create SHLB :                                                        957
C    Call To Local Procedure SHLB                                         957
         ASSIGN 6230 TO BLOCK_SHLB                                        957
         GO TO  6220                                                      957
6230     CONTINUE                                                         957
C    *                                                                    957
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  958
C    CONE3ZMX) <===                                                       958
         CALL AGSRESET                                                    958
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           958
         AG_TITLE = 'SHLB'                                                958
         AG_EXNAME = 'POSITION'                                           958
         AG_PARLIST = 'Z'                                                 958
         CALL AXPOSITION                                                  958
C    *                                                                    958
C    Create SCMY :                                                        959
C    Call To Local Procedure SCMY                                         959
         ASSIGN 6250 TO BLOCK_SCMY                                        959
         GO TO  6240                                                      959
6250     CONTINUE                                                         959
C    *                                                                    959
C    Geant POSITION : SCMY ===> inherited arguments used <===             960
         CALL AGSRESET                                                    960
         AG_TITLE = 'SCMY'                                                960
         AG_EXNAME = 'POSITION'                                           960
         AG_PARLIST = 'NONE'                                              960
         CALL AXPOSITION                                                  960
C    *                                                                    960
C    Create SCAL :                                                        961
C    Call To Local Procedure SCAL                                         961
         ASSIGN 6270 TO BLOCK_SCAL                                        961
         GO TO  6260                                                      961
6270     CONTINUE                                                         961
C    *                                                                    961
C    Geant POSITION : SCAL ===> inherited arguments used <===             962
         CALL AGSRESET                                                    962
         AG_TITLE = 'SCAL'                                                962
         AG_EXNAME = 'POSITION'                                           962
         AG_PARLIST = 'NONE'                                              962
         CALL AXPOSITION                                                  962
C    *                                                                    962
5201  CALL AGSPOPB                                                        963
      GO TO BLOCK_SCON, (5200 ,5210)                                      963
C End of Local Procedure                                                  963
C ----------------------------------------------------------------------  964
C ------------ SGRA                                                       964
C Begin Local Procedure SGRA                                              965
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 965
6140  CONTINUE                                                            965
         AG_TITLE='SGRA'                                                  965
         AG_MODULE=MODULE                                                 965
         AG_MARK='V00'                                                    965
         CALL AGSPUSH                                                     965
         AG_MODULE=MODULE                                                 965
C    *                                                                    965
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           966
         CALL AGSRESET                                                    966
         AG_TITLE = 'CARBON'                                              966
         AG_EXNAME = 'MATERIAL'                                           966
         AG_PARLIST = 'NONE'                                              966
         CALL AXMATERIAL                                                  966
C    *                                                                    966
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       966
         CALL AGSRESET                                                    967
         AG_SEEN = 1                                                      967
         AG_COLO = 6                                                      967
         AG_TITLE = 'SGRA'                                                967
         AG_EXNAME = 'ATTRIBUTE'                                          967
         AG_PARLIST = 'SEEN_COLO'                                         967
         CALL AXATTRIBUTE                                                 967
C    *                                                                    967
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  967
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  967
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  967
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  967
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  967
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              967
         CALL AGSRESET                                                    975
         AG_PHI1 = 0                                                      975
         AG_DPHI = 360                                                    975
         AG_NZ = 5                                                        975
         AG_ZI(1)=SSUP_RODLEN/2                                           975
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              975
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              975
         AG_ZI(4)=SSUP_CONE3ZMX                                           975
         AG_ZI(5)=SSUP_CONE4ZMX                                           975
         AG_RMN(1)=SSUP_CON1IDMN                                          975
         AG_RMN(2)=SSUP_CON1IDMN                                          975
         AG_RMN(3)=SSUP_CON3IDMN                                          975
         AG_RMN(4)=SSUP_CON4IDMN                                          975
         AG_RMN(5)=SSUP_CON4IDMX                                          975
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             975
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             975
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             975
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             975
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             975
         AG_TITLE = 'PCON'                                                975
         AG_EXNAME = 'SHAPE'                                              975
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           975
         CALL AXSHAPE                                                     975
C    *                                                                    975
6141  CALL AGSPOPB                                                        976
      GO TO BLOCK_SGRA, (6140 ,6150)                                      976
C End of Local Procedure                                                  976
C ----------------------------------------------------------------------  977
C ------------ STAP                                                       977
C Begin Local Procedure STAP                                              978
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  978
C E)                                                                      978
6160  CONTINUE                                                            978
         AG_TITLE='STAP'                                                  978
         AG_MODULE=MODULE                                                 978
         AG_MARK='V00'                                                    978
         CALL AGSPUSH                                                     978
         AG_MODULE=MODULE                                                 978
C    *                                                                    978
         ROFFSET=SSUP_GRPHTHK                                             979
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           980
         CALL AGSRESET                                                    980
         AG_A = 12                                                        980
         AG_Z = 6                                                         980
         AG_W = 1                                                         980
         AG_TITLE = 'C'                                                   980
         AG_EXNAME = 'COMPONENT'                                          980
         AG_PARLIST = 'A_Z_W'                                             980
         CALL AXCOMPONENT                                                 980
C    *                                                                    980
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           980
         CALL AGSRESET                                                    981
         AG_A = 1                                                         981
         AG_Z = 1                                                         981
         AG_W = 2                                                         981
         AG_TITLE = 'H2'                                                  981
         AG_EXNAME = 'COMPONENT'                                          981
         AG_PARLIST = 'A_Z_W'                                             981
         CALL AXCOMPONENT                                                 981
C    *                                                                    981
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             981
         CALL AGSRESET                                                    982
         AG_DENS = 0.935                                                  982
         AG_TITLE = 'CH2'                                                 982
         AG_EXNAME = 'MIXTURE'                                            982
         AG_PARLIST = 'DENS'                                              982
         CALL AXMIXTURE                                                   982
C    *                                                                    982
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       982
         CALL AGSRESET                                                    983
         AG_SEEN = 1                                                      983
         AG_COLO = 3                                                      983
         AG_TITLE = 'STAP'                                                983
         AG_EXNAME = 'ATTRIBUTE'                                          983
         AG_PARLIST = 'SEEN_COLO'                                         983
         CALL AXATTRIBUTE                                                 983
C    *                                                                    983
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  983
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  983
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  983
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  983
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     983
         CALL AGSRESET                                                    989
         AG_PHI1 = 0                                                      989
         AG_DPHI = 360                                                    989
         AG_NZ = 3                                                        989
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              989
         AG_ZI(2)=SSUP_CONE3ZMX                                           989
         AG_ZI(3)=SSUP_CONE4ZMX                                           989
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  989
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  989
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  989
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    989
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    989
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    989
         AG_TITLE = 'PCON'                                                989
         AG_EXNAME = 'SHAPE'                                              989
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           989
         CALL AXSHAPE                                                     989
C    *                                                                    989
6161  CALL AGSPOPB                                                        990
      GO TO BLOCK_STAP, (6160 ,6170)                                      990
C End of Local Procedure                                                  990
C ----------------------------------------------------------------------  991
C ------------ STAC                                                       991
C Begin Local Procedure STAC                                              992
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      992
6180  CONTINUE                                                            992
         AG_TITLE='STAC'                                                  992
         AG_MODULE=MODULE                                                 992
         AG_MARK='V00'                                                    992
         CALL AGSPUSH                                                     992
         AG_MODULE=MODULE                                                 992
C    *                                                                    992
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             993
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           994
         CALL AGSRESET                                                    994
         AG_TITLE = 'COPPER'                                              994
         AG_EXNAME = 'MATERIAL'                                           994
         AG_PARLIST = 'NONE'                                              994
         CALL AXMATERIAL                                                  994
C    *                                                                    994
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       994
         CALL AGSRESET                                                    995
         AG_SEEN = 1                                                      995
         AG_COLO = 2                                                      995
         AG_TITLE = 'STAC'                                                995
         AG_EXNAME = 'ATTRIBUTE'                                          995
         AG_PARLIST = 'SEEN_COLO'                                         995
         CALL AXATTRIBUTE                                                 995
C    *                                                                    995
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  995
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  995
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  995
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  995
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     995
         CALL AGSRESET                                                    1001
         AG_PHI1 = 0                                                      1001
         AG_DPHI = 360                                                    1001
         AG_NZ = 3                                                        1001
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1001
         AG_ZI(2)=SSUP_CONE3ZMX                                           1001
         AG_ZI(3)=SSUP_CONE4ZMX                                           1001
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1001
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1001
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1001
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    1001
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    1001
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    1001
         AG_TITLE = 'PCON'                                                1001
         AG_EXNAME = 'SHAPE'                                              1001
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1001
         CALL AXSHAPE                                                     1001
C    *                                                                    1001
6181  CALL AGSPOPB                                                        1002
      GO TO BLOCK_STAC, (6180 ,6190)                                      1002
C End of Local Procedure                                                  1002
C ----------------------------------------------------------------------  1003
C ------------ SHLA                                                       1003
C Begin Local Procedure SHLA                                              1004
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              1004
6200  CONTINUE                                                            1004
         AG_TITLE='SHLA'                                                  1004
         AG_MODULE=MODULE                                                 1004
         AG_MARK='V00'                                                    1004
         CALL AGSPUSH                                                     1004
         AG_MODULE=MODULE                                                 1004
C    *                                                                    1004
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1005
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1006
         CALL AGSRESET                                                    1006
         AG_TITLE = 'AIR'                                                 1006
         AG_EXNAME = 'MATERIAL'                                           1006
         AG_PARLIST = 'NONE'                                              1006
         CALL AXMATERIAL                                                  1006
C    *                                                                    1006
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       1006
         CALL AGSRESET                                                    1007
         AG_SEEN = 0                                                      1007
         AG_COLO = 1                                                      1007
         AG_TITLE = 'SHLA'                                                1007
         AG_EXNAME = 'ATTRIBUTE'                                          1007
         AG_PARLIST = 'SEEN_COLO'                                         1007
         CALL AXATTRIBUTE                                                 1007
C    *                                                                    1007
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  1007
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  1007
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  1007
C    .0*SSUP_HOSRMX <===                                                  1007
         CALL AGSRESET                                                    1011
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            1011
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  1011
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1011
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  1011
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1011
         AG_TITLE = 'CONE'                                                1011
         AG_EXNAME = 'SHAPE'                                              1011
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1011
         CALL AXSHAPE                                                     1011
C    *                                                                    1011
C    Create SHMA :                                                        1012
C    Call To Local Procedure SHMA                                         1012
         ASSIGN 6290 TO BLOCK_SHMA                                        1012
         GO TO  6280                                                      1012
6290     CONTINUE                                                         1012
C    *                                                                    1012
6201  CALL AGSPOPB                                                        1013
      GO TO BLOCK_SHLA, (6200 ,6210)                                      1013
C End of Local Procedure                                                  1013
C ----------------------------------------------------------------------  1014
C ------------ SHMA                                                       1014
C Begin Local Procedure SHMA                                              1015
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           1015
6280  CONTINUE                                                            1015
         AG_TITLE='SHMA'                                                  1015
         AG_MODULE=MODULE                                                 1015
         AG_MARK='V00'                                                    1015
         CALL AGSPUSH                                                     1015
         AG_MODULE=MODULE                                                 1015
C    *                                                                    1015
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1016
         RMIN=SSUP_CON3IDMN + ROFFSET                                     1017
         RMAX=SSUP_CON4IDMN + ROFFSET                                     1018
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                1019
         ZMAX=SSUP_CONE3ZMX                                               1020
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1021
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1022
         CONE_LEN = SQRT(CONE_LEN)                                        1023
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1024
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1025
         ANGLE=ASIN(CONE_SIN)*180/3.1416                                  1026
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1027
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1028
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       1029
         CALL AGSRESET                                                    1029
         AG_SEEN = 0                                                      1029
         AG_COLO = 1                                                      1029
         AG_TITLE = 'SHMA'                                                1029
         AG_EXNAME = 'ATTRIBUTE'                                          1029
         AG_PARLIST = 'SEEN_COLO'                                         1029
         CALL AXATTRIBUTE                                                 1029
C    *                                                                    1029
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1029
         CALL AGSRESET                                                    1031
         AG_NDIV = SSUP_NHOSES                                            1031
         AG_IAXIS = 2                                                     1031
         AG_C0 = 0.0                                                      1031
         AG_TITLE = 'DIVISION'                                            1031
         AG_EXNAME = 'SHAPE'                                              1031
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1031
         CALL AXSHAPE                                                     1031
C    *                                                                    1031
C    Create SWHO :                                                        1032
C    Call To Local Procedure SWHO                                         1032
         ASSIGN 6310 TO BLOCK_SWHO                                        1032
         GO TO  6300                                                      1032
6310     CONTINUE                                                         1032
C    *                                                                    1032
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1033
         CALL AGSRESET                                                    1033
         AG_X = XPOS                                                      1033
         AG_Y = 0                                                         1033
         AG_Z = ZPOS                                                      1033
         AG_ALPHAN=AG_ALPHAN+1                                            1033
         AG_ALPHAK(AG_ALPHAN)=2                                           1033
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1033
         AG_TITLE = 'SWHO'                                                1033
         AG_EXNAME = 'POSITION'                                           1033
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1033
         CALL AXPOSITION                                                  1033
C    *                                                                    1033
6281  CALL AGSPOPB                                                        1034
      GO TO BLOCK_SHMA, (6280 ,6290)                                      1034
C End of Local Procedure                                                  1034
C ----------------------------------------------------------------------  1035
C ------------ SHLB                                                       1035
C Begin Local Procedure SHLB                                              1036
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               1036
6220  CONTINUE                                                            1036
         AG_TITLE='SHLB'                                                  1036
         AG_MODULE=MODULE                                                 1036
         AG_MARK='V00'                                                    1036
         CALL AGSPUSH                                                     1036
         AG_MODULE=MODULE                                                 1036
C    *                                                                    1036
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1037
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1038
         CALL AGSRESET                                                    1038
         AG_TITLE = 'AIR'                                                 1038
         AG_EXNAME = 'MATERIAL'                                           1038
         AG_PARLIST = 'NONE'                                              1038
         CALL AXMATERIAL                                                  1038
C    *                                                                    1038
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       1038
         CALL AGSRESET                                                    1039
         AG_SEEN = 0                                                      1039
         AG_COLO = 1                                                      1039
         AG_TITLE = 'SHLB'                                                1039
         AG_EXNAME = 'ATTRIBUTE'                                          1039
         AG_PARLIST = 'SEEN_COLO'                                         1039
         CALL AXATTRIBUTE                                                 1039
C    *                                                                    1039
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  1039
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  1039
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  1039
C    X <===                                                               1039
         CALL AGSRESET                                                    1043
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         1043
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  1043
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  1043
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  1043
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  1043
         AG_TITLE = 'CONE'                                                1043
         AG_EXNAME = 'SHAPE'                                              1043
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1043
         CALL AXSHAPE                                                     1043
C    *                                                                    1043
C    Create SHMB :                                                        1044
C    Call To Local Procedure SHMB                                         1044
         ASSIGN 6330 TO BLOCK_SHMB                                        1044
         GO TO  6320                                                      1044
6330     CONTINUE                                                         1044
C    *                                                                    1044
6221  CALL AGSPOPB                                                        1045
      GO TO BLOCK_SHLB, (6220 ,6230)                                      1045
C End of Local Procedure                                                  1045
C ----------------------------------------------------------------------  1046
C ------------ SHMB                                                       1046
C Begin Local Procedure SHMB                                              1047
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           1047
6320  CONTINUE                                                            1047
         AG_TITLE='SHMB'                                                  1047
         AG_MODULE=MODULE                                                 1047
         AG_MARK='V00'                                                    1047
         CALL AGSPUSH                                                     1047
         AG_MODULE=MODULE                                                 1047
C    *                                                                    1047
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               1048
         RMIN=SSUP_CON4IDMN + ROFFSET                                     1049
         RMAX=SSUP_CON4IDMX + ROFFSET                                     1050
         ZMIN=SSUP_CONE3ZMX                                               1051
         ZMAX=SSUP_CONE4ZMX                                               1052
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            1053
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    1054
         CONE_LEN = SQRT(CONE_LEN)                                        1055
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  1056
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  1057
         ANGLE=ASIN(CONE_SIN)*180/3.1416                                  1058
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             1059
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     1060
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       1061
         CALL AGSRESET                                                    1061
         AG_SEEN = 0                                                      1061
         AG_COLO = 1                                                      1061
         AG_TITLE = 'SHMB'                                                1061
         AG_EXNAME = 'ATTRIBUTE'                                          1061
         AG_PARLIST = 'SEEN_COLO'                                         1061
         CALL AXATTRIBUTE                                                 1061
C    *                                                                    1061
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    1061
         CALL AGSRESET                                                    1063
         AG_NDIV = SSUP_NHOSES                                            1063
         AG_IAXIS = 2                                                     1063
         AG_C0 = 0.0                                                      1063
         AG_TITLE = 'DIVISION'                                            1063
         AG_EXNAME = 'SHAPE'                                              1063
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1063
         CALL AXSHAPE                                                     1063
C    *                                                                    1063
C    Create SWHO :                                                        1064
C    Call To Local Procedure SWHO                                         1064
         ASSIGN 6340 TO BLOCK_SWHO                                        1064
         GO TO  6300                                                      1064
6340     CONTINUE                                                         1064
C    *                                                                    1064
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       1065
         CALL AGSRESET                                                    1065
         AG_X = XPOS                                                      1065
         AG_Y = 0                                                         1065
         AG_Z = ZPOS                                                      1065
         AG_ALPHAN=AG_ALPHAN+1                                            1065
         AG_ALPHAK(AG_ALPHAN)=2                                           1065
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       1065
         AG_TITLE = 'SWHO'                                                1065
         AG_EXNAME = 'POSITION'                                           1065
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      1065
         CALL AXPOSITION                                                  1065
C    *                                                                    1065
6321  CALL AGSPOPB                                                        1066
      GO TO BLOCK_SHMB, (6320 ,6330)                                      1066
C End of Local Procedure                                                  1066
C ----------------------------------------------------------------------  1067
C ------------ SWHO                                                       1067
C Begin Local Procedure SWHO                                              1068
C SWHO IS A WATER HOSE                                                    1068
6300  CONTINUE                                                            1068
         AG_TITLE='SWHO'                                                  1068
         AG_MODULE=MODULE                                                 1068
         AG_MARK='V00'                                                    1068
         CALL AGSPUSH                                                     1068
         AG_MODULE=MODULE                                                 1068
C    *                                                                    1068
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1069
         CALL AGSRESET                                                    1069
         AG_SEEN = 1                                                      1069
         AG_COLO = 3                                                      1069
         AG_TITLE = 'SWHO'                                                1069
         AG_EXNAME = 'ATTRIBUTE'                                          1069
         AG_PARLIST = 'SEEN_COLO'                                         1069
         CALL AXATTRIBUTE                                                 1069
C    *                                                                    1069
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1069
         CALL AGSRESET                                                    1070
         AG_A = 12                                                        1070
         AG_Z = 6                                                         1070
         AG_W = 1                                                         1070
         AG_TITLE = 'C'                                                   1070
         AG_EXNAME = 'COMPONENT'                                          1070
         AG_PARLIST = 'A_Z_W'                                             1070
         CALL AXCOMPONENT                                                 1070
C    *                                                                    1070
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1070
         CALL AGSRESET                                                    1071
         AG_A = 1                                                         1071
         AG_Z = 1                                                         1071
         AG_W = 2                                                         1071
         AG_TITLE = 'H2'                                                  1071
         AG_EXNAME = 'COMPONENT'                                          1071
         AG_PARLIST = 'A_Z_W'                                             1071
         CALL AXCOMPONENT                                                 1071
C    *                                                                    1071
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1071
         CALL AGSRESET                                                    1072
         AG_DENS = 0.935                                                  1072
         AG_TITLE = 'CH2'                                                 1072
         AG_EXNAME = 'MIXTURE'                                            1072
         AG_PARLIST = 'DENS'                                              1072
         CALL AXMIXTURE                                                   1072
C    *                                                                    1072
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       1072
         CALL AGSRESET                                                    1073
         AG_SEEN = 1                                                      1073
         AG_COLO = 3                                                      1073
         AG_TITLE = 'SWHO'                                                1073
         AG_EXNAME = 'ATTRIBUTE'                                          1073
         AG_PARLIST = 'SEEN_COLO'                                         1073
         CALL AXATTRIBUTE                                                 1073
C    *                                                                    1073
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  1073
C    =                                                                    1073
         CALL AGSRESET                                                    1074
         AG_RMIN = 0                                                      1074
         AG_RMAX = SSUP_HOSRMX                                            1074
         AG_DZ = 0.5*CONE_LEN                                             1074
         AG_TITLE = 'TUBE'                                                1074
         AG_EXNAME = 'SHAPE'                                              1074
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1074
         CALL AXSHAPE                                                     1074
C    *                                                                    1074
C    Create SHWA :                                                        1075
C    Call To Local Procedure SHWA                                         1075
         ASSIGN 6360 TO BLOCK_SHWA                                        1075
         GO TO  6350                                                      1075
6360     CONTINUE                                                         1075
C    *                                                                    1075
C    Geant POSITION : SHWA ===> inherited arguments used <===             1076
         CALL AGSRESET                                                    1076
         AG_TITLE = 'SHWA'                                                1076
         AG_EXNAME = 'POSITION'                                           1076
         AG_PARLIST = 'NONE'                                              1076
         CALL AXPOSITION                                                  1076
C    *                                                                    1076
6301  CALL AGSPOPB                                                        1077
      GO TO BLOCK_SWHO, (6300 ,6310,6340)                                 1077
C End of Local Procedure                                                  1077
C ----------------------------------------------------------------------  1078
C ------------ SHWA                                                       1078
C Begin Local Procedure SHWA                                              1079
C SHWA IS THE WATER IN THE HOSE                                           1079
6350  CONTINUE                                                            1079
         AG_TITLE='SHWA'                                                  1079
         AG_MODULE=MODULE                                                 1079
         AG_MARK='V00'                                                    1079
         CALL AGSPUSH                                                     1079
         AG_MODULE=MODULE                                                 1079
C    *                                                                    1079
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       1080
         CALL AGSRESET                                                    1080
         AG_SEEN = 1                                                      1080
         AG_COLO = 6                                                      1080
         AG_TITLE = 'SHWA'                                                1080
         AG_EXNAME = 'ATTRIBUTE'                                          1080
         AG_PARLIST = 'SEEN_COLO'                                         1080
         CALL AXATTRIBUTE                                                 1080
C    *                                                                    1080
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1080
         CALL AGSRESET                                                    1081
         AG_TITLE = 'WATER'                                               1081
         AG_EXNAME = 'MATERIAL'                                           1081
         AG_PARLIST = 'NONE'                                              1081
         CALL AXMATERIAL                                                  1081
C    *                                                                    1081
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        1081
         CALL AGSRESET                                                    1082
         AG_RMAX = SSUP_HOSRMN                                            1082
         AG_TITLE = 'TUBE'                                                1082
         AG_EXNAME = 'SHAPE'                                              1082
         AG_PARLIST = 'RMAX'                                              1082
         CALL AXSHAPE                                                     1082
C    *                                                                    1082
6351  CALL AGSPOPB                                                        1083
      GO TO BLOCK_SHWA, (6350 ,6360)                                      1083
C End of Local Procedure                                                  1083
C ----------------------------------------------------------------------  1084
C ------------ SCMY                                                       1084
C Begin Local Procedure SCMY                                              1085
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            1085
6240  CONTINUE                                                            1085
         AG_TITLE='SCMY'                                                  1085
         AG_MODULE=MODULE                                                 1085
         AG_MARK='V00'                                                    1085
         CALL AGSPUSH                                                     1085
         AG_MODULE=MODULE                                                 1085
C    *                                                                    1085
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             1086
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          1087
         CALL AGSRESET                                                    1087
         AG_A = 12                                                        1087
         AG_Z = 6                                                         1087
         AG_W = 5                                                         1087
         AG_TITLE = 'C5'                                                  1087
         AG_EXNAME = 'COMPONENT'                                          1087
         AG_PARLIST = 'A_Z_W'                                             1087
         CALL AXCOMPONENT                                                 1087
C    *                                                                    1087
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           1087
         CALL AGSRESET                                                    1088
         AG_A = 1                                                         1088
         AG_Z = 1                                                         1088
         AG_W = 4                                                         1088
         AG_TITLE = 'H4'                                                  1088
         AG_EXNAME = 'COMPONENT'                                          1088
         AG_PARLIST = 'A_Z_W'                                             1088
         CALL AXCOMPONENT                                                 1088
C    *                                                                    1088
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          1088
         CALL AGSRESET                                                    1089
         AG_A = 16                                                        1089
         AG_Z = 8                                                         1089
         AG_W = 2                                                         1089
         AG_TITLE = 'O2'                                                  1089
         AG_EXNAME = 'COMPONENT'                                          1089
         AG_PARLIST = 'A_Z_W'                                             1089
         CALL AXCOMPONENT                                                 1089
C    *                                                                    1089
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            1089
         CALL AGSRESET                                                    1090
         AG_DENS = 1.39                                                   1090
         AG_TITLE = 'MYLAR'                                               1090
         AG_EXNAME = 'MIXTURE'                                            1090
         AG_PARLIST = 'DENS'                                              1090
         CALL AXMIXTURE                                                   1090
C    *                                                                    1090
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       1090
         CALL AGSRESET                                                    1091
         AG_SEEN = 1                                                      1091
         AG_COLO = 3                                                      1091
         AG_TITLE = 'SCMY'                                                1091
         AG_EXNAME = 'ATTRIBUTE'                                          1091
         AG_PARLIST = 'SEEN_COLO'                                         1091
         CALL AXATTRIBUTE                                                 1091
C    *                                                                    1091
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1091
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1091
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1091
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  1091
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     1091
         CALL AGSRESET                                                    1097
         AG_PHI1 = 0                                                      1097
         AG_DPHI = 360                                                    1097
         AG_NZ = 3                                                        1097
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1097
         AG_ZI(2)=SSUP_CONE3ZMX                                           1097
         AG_ZI(3)=SSUP_CONE4ZMX                                           1097
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1097
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1097
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1097
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    1097
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    1097
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    1097
         AG_TITLE = 'PCON'                                                1097
         AG_EXNAME = 'SHAPE'                                              1097
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1097
         CALL AXSHAPE                                                     1097
C    *                                                                    1097
6241  CALL AGSPOPB                                                        1098
      GO TO BLOCK_SCMY, (6240 ,6250)                                      1098
C End of Local Procedure                                                  1098
C ----------------------------------------------------------------------  1099
C ------------ SCAL                                                       1099
C Begin Local Procedure SCAL                                              1100
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     1100
6260  CONTINUE                                                            1100
         AG_TITLE='SCAL'                                                  1100
         AG_MODULE=MODULE                                                 1100
         AG_MARK='V00'                                                    1100
         CALL AGSPUSH                                                     1100
         AG_MODULE=MODULE                                                 1100
C    *                                                                    1100
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           1101
     *   SSUP_WRPMYTHK                                                    1101
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1102
         CALL AGSRESET                                                    1102
         AG_TITLE = 'ALUMINIUM'                                           1102
         AG_EXNAME = 'MATERIAL'                                           1102
         AG_PARLIST = 'NONE'                                              1102
         CALL AXMATERIAL                                                  1102
C    *                                                                    1102
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       1102
         CALL AGSRESET                                                    1103
         AG_SEEN = 1                                                      1103
         AG_COLO = 2                                                      1103
         AG_TITLE = 'SCAL'                                                1103
         AG_EXNAME = 'ATTRIBUTE'                                          1103
         AG_PARLIST = 'SEEN_COLO'                                         1103
         CALL AXATTRIBUTE                                                 1103
C    *                                                                    1103
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  1103
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  1103
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  1103
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  1103
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     1103
         CALL AGSRESET                                                    1109
         AG_PHI1 = 0                                                      1109
         AG_DPHI = 360                                                    1109
         AG_NZ = 3                                                        1109
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              1109
         AG_ZI(2)=SSUP_CONE3ZMX                                           1109
         AG_ZI(3)=SSUP_CONE4ZMX                                           1109
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  1109
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  1109
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  1109
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    1109
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    1109
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    1109
         AG_TITLE = 'PCON'                                                1109
         AG_EXNAME = 'SHAPE'                                              1109
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1109
         CALL AXSHAPE                                                     1109
C    *                                                                    1109
6261  CALL AGSPOPB                                                        1110
      GO TO BLOCK_SCAL, (6260 ,6270)                                      1110
C End of Local Procedure                                                  1110
C ----------------------------------------------------------------------  1111
C ------------ SWMM                                                       1111
C Begin Local Procedure SWMM                                              1112
C SWMM IS THE WATER MANIFOLD MOTHER                                       1112
5160  CONTINUE                                                            1112
         AG_TITLE='SWMM'                                                  1112
         AG_MODULE=MODULE                                                 1112
         AG_MARK='V00'                                                    1112
         CALL AGSPUSH                                                     1112
         AG_MODULE=MODULE                                                 1112
C    *                                                                    1112
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     1113
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     1114
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1115
         CALL AGSRESET                                                    1115
         AG_TITLE = 'AIR'                                                 1115
         AG_EXNAME = 'MATERIAL'                                           1115
         AG_PARLIST = 'NONE'                                              1115
         CALL AXMATERIAL                                                  1115
C    *                                                                    1115
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       1115
         CALL AGSRESET                                                    1116
         AG_SEEN = 0                                                      1116
         AG_COLO = 1                                                      1116
         AG_TITLE = 'SWMM'                                                1116
         AG_EXNAME = 'ATTRIBUTE'                                          1116
         AG_PARLIST = 'SEEN_COLO'                                         1116
         CALL AXATTRIBUTE                                                 1116
C    *                                                                    1116
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1116
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   1116
         CALL AGSRESET                                                    1119
         AG_PHI1 = 0                                                      1119
         AG_DPHI = 360                                                    1119
         AG_NPDIV = 18                                                    1119
         AG_NZ = 2                                                        1119
         AG_ZI(1)=-SWAM_LEN/2                                             1119
         AG_ZI(2)=+SWAM_LEN/2                                             1119
         AG_RMN(1)=RIN                                                    1119
         AG_RMN(2)=RIN                                                    1119
         AG_RMX(1)=ROU                                                    1119
         AG_RMX(2)=ROU                                                    1119
         AG_TITLE = 'PGON'                                                1119
         AG_EXNAME = 'SHAPE'                                              1119
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1119
         CALL AXSHAPE                                                     1119
C    *                                                                    1119
C    Create SWMB :                                                        1120
C    Call To Local Procedure SWMB                                         1120
         ASSIGN 6380 TO BLOCK_SWMB                                        1120
         GO TO  6370                                                      1120
6380     CONTINUE                                                         1120
C    *                                                                    1120
C    Geant POSITION : SWMB ===> inherited arguments used <===             1121
         CALL AGSRESET                                                    1121
         AG_TITLE = 'SWMB'                                                1121
         AG_EXNAME = 'POSITION'                                           1121
         AG_PARLIST = 'NONE'                                              1121
         CALL AXPOSITION                                                  1121
C    *                                                                    1121
C    Create SWMT :                                                        1122
C    Call To Local Procedure SWMT                                         1122
         ASSIGN 6400 TO BLOCK_SWMT                                        1122
         GO TO  6390                                                      1122
6400     CONTINUE                                                         1122
C    *                                                                    1122
C    Geant POSITION : SWMT ===> inherited arguments used <===             1123
         CALL AGSRESET                                                    1123
         AG_TITLE = 'SWMT'                                                1123
         AG_EXNAME = 'POSITION'                                           1123
         AG_PARLIST = 'NONE'                                              1123
         CALL AXPOSITION                                                  1123
C    *                                                                    1123
C    Create SWMS :                                                        1124
C    Call To Local Procedure SWMS                                         1124
         ASSIGN 6420 TO BLOCK_SWMS                                        1124
         GO TO  6410                                                      1124
6420     CONTINUE                                                         1124
C    *                                                                    1124
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         1125
         CALL AGSRESET                                                    1125
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                1125
         AG_TITLE = 'SWMS'                                                1125
         AG_EXNAME = 'POSITION'                                           1125
         AG_PARLIST = 'Z'                                                 1125
         CALL AXPOSITION                                                  1125
C    *                                                                    1125
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          1125
         CALL AGSRESET                                                    1126
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 1126
         AG_TITLE = 'SWMS'                                                1126
         AG_EXNAME = 'POSITION'                                           1126
         AG_PARLIST = 'Z'                                                 1126
         CALL AXPOSITION                                                  1126
C    *                                                                    1126
C    Create SWMW :                                                        1127
C    Call To Local Procedure SWMW                                         1127
         ASSIGN 6440 TO BLOCK_SWMW                                        1127
         GO TO  6430                                                      1127
6440     CONTINUE                                                         1127
C    *                                                                    1127
C    Geant POSITION : SWMW ===> inherited arguments used <===             1128
         CALL AGSRESET                                                    1128
         AG_TITLE = 'SWMW'                                                1128
         AG_EXNAME = 'POSITION'                                           1128
         AG_PARLIST = 'NONE'                                              1128
         CALL AXPOSITION                                                  1128
C    *                                                                    1128
C    Create SOTB :                                                        1129
C    Call To Local Procedure SOTB                                         1129
         ASSIGN 6460 TO BLOCK_SOTB                                        1129
         GO TO  6450                                                      1129
6460     CONTINUE                                                         1129
C    *                                                                    1129
C    Geant POSITION : SOTB ===> inherited arguments used <===             1130
         CALL AGSRESET                                                    1130
         AG_TITLE = 'SOTB'                                                1130
         AG_EXNAME = 'POSITION'                                           1130
         AG_PARLIST = 'NONE'                                              1130
         CALL AXPOSITION                                                  1130
C    *                                                                    1130
C    Create SITB :                                                        1131
C    Call To Local Procedure SITB                                         1131
         ASSIGN 6480 TO BLOCK_SITB                                        1131
         GO TO  6470                                                      1131
6480     CONTINUE                                                         1131
C    *                                                                    1131
C    Geant POSITION : SITB ===> inherited arguments used <===             1132
         CALL AGSRESET                                                    1132
         AG_TITLE = 'SITB'                                                1132
         AG_EXNAME = 'POSITION'                                           1132
         AG_PARLIST = 'NONE'                                              1132
         CALL AXPOSITION                                                  1132
C    *                                                                    1132
5161  CALL AGSPOPB                                                        1133
      GO TO BLOCK_SWMM, (5160 ,5170)                                      1133
C End of Local Procedure                                                  1133
C ----------------------------------------------------------------------  1134
C ------------ SWMB                                                       1134
C Begin Local Procedure SWMB                                              1135
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       1135
6370  CONTINUE                                                            1135
         AG_TITLE='SWMB'                                                  1135
         AG_MODULE=MODULE                                                 1135
         AG_MARK='V00'                                                    1135
         CALL AGSPUSH                                                     1135
         AG_MODULE=MODULE                                                 1135
C    *                                                                    1135
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1136
         CALL AGSRESET                                                    1136
         AG_TITLE = 'ALUMINIUM'                                           1136
         AG_EXNAME = 'MATERIAL'                                           1136
         AG_PARLIST = 'NONE'                                              1136
         CALL AXMATERIAL                                                  1136
C    *                                                                    1136
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       1136
         CALL AGSRESET                                                    1137
         AG_SEEN = 1                                                      1137
         AG_COLO = 2                                                      1137
         AG_TITLE = 'SWMB'                                                1137
         AG_EXNAME = 'ATTRIBUTE'                                          1137
         AG_PARLIST = 'SEEN_COLO'                                         1137
         CALL AXATTRIBUTE                                                 1137
C    *                                                                    1137
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1137
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  1137
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          1137
         CALL AGSRESET                                                    1141
         AG_PHI1 = 0                                                      1141
         AG_DPHI = 360                                                    1141
         AG_NPDIV = 18                                                    1141
         AG_NZ = 2                                                        1141
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1141
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1141
         AG_RMN(1)=SWAM_RMIN                                              1141
         AG_RMN(2)=SWAM_RMIN                                              1141
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 1141
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 1141
         AG_TITLE = 'PGON'                                                1141
         AG_EXNAME = 'SHAPE'                                              1141
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1141
         CALL AXSHAPE                                                     1141
C    *                                                                    1141
6371  CALL AGSPOPB                                                        1142
      GO TO BLOCK_SWMB, (6370 ,6380)                                      1142
C End of Local Procedure                                                  1142
C ----------------------------------------------------------------------  1143
C ------------ SWMT                                                       1143
C Begin Local Procedure SWMT                                              1144
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            1144
6390  CONTINUE                                                            1144
         AG_TITLE='SWMT'                                                  1144
         AG_MODULE=MODULE                                                 1144
         AG_MARK='V00'                                                    1144
         CALL AGSPUSH                                                     1144
         AG_MODULE=MODULE                                                 1144
C    *                                                                    1144
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1145
         CALL AGSRESET                                                    1145
         AG_TITLE = 'ALUMINIUM'                                           1145
         AG_EXNAME = 'MATERIAL'                                           1145
         AG_PARLIST = 'NONE'                                              1145
         CALL AXMATERIAL                                                  1145
C    *                                                                    1145
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       1145
         CALL AGSRESET                                                    1146
         AG_SEEN = 1                                                      1146
         AG_COLO = 2                                                      1146
         AG_TITLE = 'SWMT'                                                1146
         AG_EXNAME = 'ATTRIBUTE'                                          1146
         AG_PARLIST = 'SEEN_COLO'                                         1146
         CALL AXATTRIBUTE                                                 1146
C    *                                                                    1146
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1146
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  1146
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          1146
         CALL AGSRESET                                                    1150
         AG_PHI1 = 0                                                      1150
         AG_DPHI = 360                                                    1150
         AG_NPDIV = 18                                                    1150
         AG_NZ = 2                                                        1150
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1150
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1150
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 1150
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 1150
         AG_RMX(1)=SWAM_RMAX                                              1150
         AG_RMX(2)=SWAM_RMAX                                              1150
         AG_TITLE = 'PGON'                                                1150
         AG_EXNAME = 'SHAPE'                                              1150
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1150
         CALL AXSHAPE                                                     1150
C    *                                                                    1150
6391  CALL AGSPOPB                                                        1151
      GO TO BLOCK_SWMT, (6390 ,6400)                                      1151
C End of Local Procedure                                                  1151
C ----------------------------------------------------------------------  1152
C ------------ SWMS                                                       1152
C Begin Local Procedure SWMS                                              1153
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  1153
6410  CONTINUE                                                            1153
         AG_TITLE='SWMS'                                                  1153
         AG_MODULE=MODULE                                                 1153
         AG_MARK='V00'                                                    1153
         CALL AGSPUSH                                                     1153
         AG_MODULE=MODULE                                                 1153
C    *                                                                    1153
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1154
         CALL AGSRESET                                                    1154
         AG_TITLE = 'ALUMINIUM'                                           1154
         AG_EXNAME = 'MATERIAL'                                           1154
         AG_PARLIST = 'NONE'                                              1154
         CALL AXMATERIAL                                                  1154
C    *                                                                    1154
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       1154
         CALL AGSRESET                                                    1155
         AG_SEEN = 1                                                      1155
         AG_COLO = 2                                                      1155
         AG_TITLE = 'SWMS'                                                1155
         AG_EXNAME = 'ATTRIBUTE'                                          1155
         AG_PARLIST = 'SEEN_COLO'                                         1155
         CALL AXATTRIBUTE                                                 1155
C    *                                                                    1155
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  1155
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  1155
C    WAM_RMAX} <===                                                       1155
         CALL AGSRESET                                                    1158
         AG_PHI1 = 0                                                      1158
         AG_DPHI = 360                                                    1158
         AG_NPDIV = 18                                                    1158
         AG_NZ = 2                                                        1158
         AG_ZI(1)=-SWAM_WALLTHK/2                                         1158
         AG_ZI(2)=SWAM_WALLTHK/2                                          1158
         AG_RMN(1)=SWAM_RMIN                                              1158
         AG_RMN(2)=SWAM_RMIN                                              1158
         AG_RMX(1)=SWAM_RMAX                                              1158
         AG_RMX(2)=SWAM_RMAX                                              1158
         AG_TITLE = 'PGON'                                                1158
         AG_EXNAME = 'SHAPE'                                              1158
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1158
         CALL AXSHAPE                                                     1158
C    *                                                                    1158
6411  CALL AGSPOPB                                                        1159
      GO TO BLOCK_SWMS, (6410 ,6420)                                      1159
C End of Local Procedure                                                  1159
C ----------------------------------------------------------------------  1160
C ------------ SWMW                                                       1160
C Begin Local Procedure SWMW                                              1161
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 1161
6430  CONTINUE                                                            1161
         AG_TITLE='SWMW'                                                  1161
         AG_MODULE=MODULE                                                 1161
         AG_MARK='V00'                                                    1161
         CALL AGSPUSH                                                     1161
         AG_MODULE=MODULE                                                 1161
C    *                                                                    1161
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       1162
         CALL AGSRESET                                                    1162
         AG_SEEN = 1                                                      1162
         AG_COLO = 6                                                      1162
         AG_TITLE = 'SWMW'                                                1162
         AG_EXNAME = 'ATTRIBUTE'                                          1162
         AG_PARLIST = 'SEEN_COLO'                                         1162
         CALL AXATTRIBUTE                                                 1162
C    *                                                                    1162
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1162
         CALL AGSRESET                                                    1163
         AG_TITLE = 'WATER'                                               1163
         AG_EXNAME = 'MATERIAL'                                           1163
         AG_PARLIST = 'NONE'                                              1163
         CALL AXMATERIAL                                                  1163
C    *                                                                    1163
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1163
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  1163
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  1163
C    -SWAM_WALLTHK} <===                                                  1163
         CALL AGSRESET                                                    1168
         AG_PHI1 = 0                                                      1168
         AG_DPHI = 360                                                    1168
         AG_NPDIV = 18                                                    1168
         AG_NZ = 2                                                        1168
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                1168
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 1168
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 1168
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 1168
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 1168
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 1168
         AG_TITLE = 'PGON'                                                1168
         AG_EXNAME = 'SHAPE'                                              1168
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1168
         CALL AXSHAPE                                                     1168
C    *                                                                    1168
6431  CALL AGSPOPB                                                        1169
      GO TO BLOCK_SWMW, (6430 ,6440)                                      1169
C End of Local Procedure                                                  1169
C ----------------------------------------------------------------------  1170
C ------------ SOTB                                                       1170
C Begin Local Procedure SOTB                                              1171
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            1171
6450  CONTINUE                                                            1171
         AG_TITLE='SOTB'                                                  1171
         AG_MODULE=MODULE                                                 1171
         AG_MARK='V00'                                                    1171
         CALL AGSPUSH                                                     1171
         AG_MODULE=MODULE                                                 1171
C    *                                                                    1171
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1172
         CALL AGSRESET                                                    1172
         AG_TITLE = 'G10'                                                 1172
         AG_EXNAME = 'MATERIAL'                                           1172
         AG_PARLIST = 'NONE'                                              1172
         CALL AXMATERIAL                                                  1172
C    *                                                                    1172
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       1172
         CALL AGSRESET                                                    1173
         AG_SEEN = 1                                                      1173
         AG_COLO = 3                                                      1173
         AG_TITLE = 'SOTB'                                                1173
         AG_EXNAME = 'ATTRIBUTE'                                          1173
         AG_PARLIST = 'SEEN_COLO'                                         1173
         CALL AXATTRIBUTE                                                 1173
C    *                                                                    1173
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1173
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  1173
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    1173
         CALL AGSRESET                                                    1176
         AG_PHI1 = 0                                                      1176
         AG_DPHI = 360                                                    1176
         AG_NPDIV = 18                                                    1176
         AG_NZ = 2                                                        1176
         AG_ZI(1)=-SWAM_LEN/2                                             1176
         AG_ZI(2)=+SWAM_LEN/2                                             1176
         AG_RMN(1)=SWAM_RMAX                                              1176
         AG_RMN(2)=SWAM_RMAX                                              1176
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 1176
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 1176
         AG_TITLE = 'PGON'                                                1176
         AG_EXNAME = 'SHAPE'                                              1176
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1176
         CALL AXSHAPE                                                     1176
C    *                                                                    1176
6451  CALL AGSPOPB                                                        1177
      GO TO BLOCK_SOTB, (6450 ,6460)                                      1177
C End of Local Procedure                                                  1177
C ----------------------------------------------------------------------  1178
C ------------ SITB                                                       1178
C Begin Local Procedure SITB                                              1179
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            1179
6470  CONTINUE                                                            1179
         AG_TITLE='SITB'                                                  1179
         AG_MODULE=MODULE                                                 1179
         AG_MARK='V00'                                                    1179
         CALL AGSPUSH                                                     1179
         AG_MODULE=MODULE                                                 1179
C    *                                                                    1179
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           1180
         CALL AGSRESET                                                    1180
         AG_A = 12                                                        1180
         AG_Z = 6                                                         1180
         AG_W = 1                                                         1180
         AG_TITLE = 'C'                                                   1180
         AG_EXNAME = 'COMPONENT'                                          1180
         AG_PARLIST = 'A_Z_W'                                             1180
         CALL AXCOMPONENT                                                 1180
C    *                                                                    1180
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1180
         CALL AGSRESET                                                    1181
         AG_A = 1                                                         1181
         AG_Z = 1                                                         1181
         AG_W = 2                                                         1181
         AG_TITLE = 'H2'                                                  1181
         AG_EXNAME = 'COMPONENT'                                          1181
         AG_PARLIST = 'A_Z_W'                                             1181
         CALL AXCOMPONENT                                                 1181
C    *                                                                    1181
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             1181
         CALL AGSRESET                                                    1182
         AG_DENS = 0.935                                                  1182
         AG_TITLE = 'CH2'                                                 1182
         AG_EXNAME = 'MIXTURE'                                            1182
         AG_PARLIST = 'DENS'                                              1182
         CALL AXMIXTURE                                                   1182
C    *                                                                    1182
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       1182
         CALL AGSRESET                                                    1183
         AG_SEEN = 1                                                      1183
         AG_COLO = 3                                                      1183
         AG_TITLE = 'SITB'                                                1183
         AG_EXNAME = 'ATTRIBUTE'                                          1183
         AG_PARLIST = 'SEEN_COLO'                                         1183
         CALL AXATTRIBUTE                                                 1183
C    *                                                                    1183
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  1183
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  1183
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   1183
         CALL AGSRESET                                                    1187
         AG_PHI1 = 0                                                      1187
         AG_DPHI = 360                                                    1187
         AG_NPDIV = 18                                                    1187
         AG_NZ = 2                                                        1187
         AG_ZI(1)=-SWAM_LEN/2                                             1187
         AG_ZI(2)=+SWAM_LEN/2                                             1187
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 1187
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 1187
         AG_RMX(1)=SWAM_RMIN                                              1187
         AG_RMX(2)=SWAM_RMIN                                              1187
         AG_TITLE = 'PGON'                                                1187
         AG_EXNAME = 'SHAPE'                                              1187
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1187
         CALL AXSHAPE                                                     1187
C    *                                                                    1187
6471  CALL AGSPOPB                                                        1188
      GO TO BLOCK_SITB, (6470 ,6480)                                      1188
C End of Local Procedure                                                  1188
C ----------------------------------------------------------------------  1189
C ------------ SBWC                                                       1189
C Begin Local Procedure SBWC                                              1190
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           1190
5180  CONTINUE                                                            1190
         AG_TITLE='SBWC'                                                  1190
         AG_MODULE=MODULE                                                 1190
         AG_MARK='V00'                                                    1190
         CALL AGSPUSH                                                     1190
         AG_MODULE=MODULE                                                 1190
C    *                                                                    1190
         RIN=SWAM_RMIN                                                    1191
         ROU=SSUP_CON1IDMN                                                1192
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1193
         CALL AGSRESET                                                    1193
         AG_TITLE = 'AIR'                                                 1193
         AG_EXNAME = 'MATERIAL'                                           1193
         AG_PARLIST = 'NONE'                                              1193
         CALL AXMATERIAL                                                  1193
C    *                                                                    1193
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       1193
         CALL AGSRESET                                                    1194
         AG_SEEN = 0                                                      1194
         AG_COLO = 1                                                      1194
         AG_TITLE = 'SBWC'                                                1194
         AG_EXNAME = 'ATTRIBUTE'                                          1194
         AG_PARLIST = 'SEEN_COLO'                                         1194
         CALL AXATTRIBUTE                                                 1194
C    *                                                                    1194
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  1194
C    MIN+SWAM_LEN))/2 <===                                                1194
         CALL AGSRESET                                                    1195
         AG_RMIN = RIN                                                    1195
         AG_RMAX = ROU                                                    1195
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   1195
         AG_TITLE = 'TUBE'                                                1195
         AG_EXNAME = 'SHAPE'                                              1195
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1195
         CALL AXSHAPE                                                     1195
C    *                                                                    1195
C    Create SWCM :                                                        1196
C    Call To Local Procedure SWCM                                         1196
         ASSIGN 6500 TO BLOCK_SWCM                                        1196
         GO TO  6490                                                      1196
6500     CONTINUE                                                         1196
C    *                                                                    1196
5181  CALL AGSPOPB                                                        1197
      GO TO BLOCK_SBWC, (5180 ,5190)                                      1197
C End of Local Procedure                                                  1197
C ----------------------------------------------------------------------  1198
C ------------ SWCM                                                       1198
C Begin Local Procedure SWCM                                              1199
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   1199
6490  CONTINUE                                                            1199
         AG_TITLE='SWCM'                                                  1199
         AG_MODULE=MODULE                                                 1199
         AG_MARK='V00'                                                    1199
         CALL AGSPUSH                                                     1199
         AG_MODULE=MODULE                                                 1199
C    *                                                                    1199
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       1200
         CALL AGSRESET                                                    1200
         AG_SEEN = 0                                                      1200
         AG_COLO = 1                                                      1200
         AG_TITLE = 'SWCM'                                                1200
         AG_EXNAME = 'ATTRIBUTE'                                          1200
         AG_PARLIST = 'SEEN_COLO'                                         1200
         CALL AXATTRIBUTE                                                 1200
C    *                                                                    1200
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 1200
         CALL AGSRESET                                                    1201
         AG_NDIV = 3                                                      1201
         AG_IAXIS = 2                                                     1201
         AG_C0 = 0                                                        1201
         AG_TITLE = 'DIVISION'                                            1201
         AG_EXNAME = 'SHAPE'                                              1201
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     1201
         CALL AXSHAPE                                                     1201
C    *                                                                    1201
C    Create SXAI :                                                        1202
C    Call To Local Procedure SXAI                                         1202
         ASSIGN 6520 TO BLOCK_SXAI                                        1202
         GO TO  6510                                                      1202
6520     CONTINUE                                                         1202
C    *                                                                    1202
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  1203
C    /2+SSUP_BRATHK/2) <===                                               1203
         CALL AGSRESET                                                    1203
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   1203
         AG_TITLE = 'SXAI'                                                1203
         AG_EXNAME = 'POSITION'                                           1203
         AG_PARLIST = 'Z'                                                 1203
         CALL AXPOSITION                                                  1203
C    *                                                                    1203
C    Create SXBI :                                                        1204
C    Call To Local Procedure SXBI                                         1204
         ASSIGN 6540 TO BLOCK_SXBI                                        1204
         GO TO  6530                                                      1204
6540     CONTINUE                                                         1204
C    *                                                                    1204
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    1205
         CALL AGSRESET                                                    1205
         AG_Z = (SSUP_BRATHK/2)                                           1205
         AG_TITLE = 'SXBI'                                                1205
         AG_EXNAME = 'POSITION'                                           1205
         AG_PARLIST = 'Z'                                                 1205
         CALL AXPOSITION                                                  1205
C    *                                                                    1205
6491  CALL AGSPOPB                                                        1206
      GO TO BLOCK_SWCM, (6490 ,6500)                                      1206
C End of Local Procedure                                                  1206
C ----------------------------------------------------------------------  1207
C ------------ SXAI                                                       1207
C Begin Local Procedure SXAI                                              1208
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      1208
6510  CONTINUE                                                            1208
         AG_TITLE='SXAI'                                                  1208
         AG_MODULE=MODULE                                                 1208
         AG_MARK='V00'                                                    1208
         CALL AGSPUSH                                                     1208
         AG_MODULE=MODULE                                                 1208
C    *                                                                    1208
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       1209
         CALL AGSRESET                                                    1209
         AG_SEEN = 1                                                      1209
         AG_COLO = 2                                                      1209
         AG_TITLE = 'SXAI'                                                1209
         AG_EXNAME = 'ATTRIBUTE'                                          1209
         AG_PARLIST = 'SEEN_COLO'                                         1209
         CALL AXATTRIBUTE                                                 1209
C    *                                                                    1209
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1209
         CALL AGSRESET                                                    1210
         AG_TITLE = 'ALUMINIUM'                                           1210
         AG_EXNAME = 'MATERIAL'                                           1210
         AG_PARLIST = 'NONE'                                              1210
         CALL AXMATERIAL                                                  1210
C    *                                                                    1210
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   1210
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         1210
         CALL AGSRESET                                                    1212
         AG_RMIN = SWAM_RMIN                                              1212
         AG_RMAX = SSUP_CON1IDMN                                          1212
         AG_PHI1 = -5                                                     1212
         AG_PHI2 = 5                                                      1212
         AG_DZ = SSUP_BRATHK/2                                            1212
         AG_TITLE = 'TUBS'                                                1212
         AG_EXNAME = 'SHAPE'                                              1212
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1212
         CALL AXSHAPE                                                     1212
C    *                                                                    1212
6511  CALL AGSPOPB                                                        1213
      GO TO BLOCK_SXAI, (6510 ,6520)                                      1213
C End of Local Procedure                                                  1213
C ----------------------------------------------------------------------  1214
C ------------ SXBI                                                       1214
C Begin Local Procedure SXBI                                              1215
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     1215
6530  CONTINUE                                                            1215
         AG_TITLE='SXBI'                                                  1215
         AG_MODULE=MODULE                                                 1215
         AG_MARK='V00'                                                    1215
         CALL AGSPUSH                                                     1215
         AG_MODULE=MODULE                                                 1215
C    *                                                                    1215
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       1216
         CALL AGSRESET                                                    1216
         AG_SEEN = 1                                                      1216
         AG_COLO = 2                                                      1216
         AG_TITLE = 'SXBI'                                                1216
         AG_EXNAME = 'ATTRIBUTE'                                          1216
         AG_PARLIST = 'SEEN_COLO'                                         1216
         CALL AXATTRIBUTE                                                 1216
C    *                                                                    1216
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1216
         CALL AGSRESET                                                    1217
         AG_TITLE = 'ALUMINIUM'                                           1217
         AG_EXNAME = 'MATERIAL'                                           1217
         AG_PARLIST = 'NONE'                                              1217
         CALL AXMATERIAL                                                  1217
C    *                                                                    1217
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  1217
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  1217
C    _BRATHK)/2) <===                                                     1217
         CALL AGSRESET                                                    1219
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              1219
         AG_RMAX = SSUP_CON1IDMN                                          1219
         AG_PHI1 = -5                                                     1219
         AG_PHI2 = 5                                                      1219
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     1219
         AG_TITLE = 'TUBS'                                                1219
         AG_EXNAME = 'SHAPE'                                              1219
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1219
         CALL AXSHAPE                                                     1219
C    *                                                                    1219
6531  CALL AGSPOPB                                                        1220
      GO TO BLOCK_SXBI, (6530 ,6540)                                      1220
C End of Local Procedure                                                  1220
C ----------------------------------------------------------------------  1221
C ------------ SCRW                                                       1221
C Begin Local Procedure SCRW                                              1222
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   1222
6050  CONTINUE                                                            1222
         AG_TITLE='SCRW'                                                  1222
         AG_MODULE=MODULE                                                 1222
         AG_MARK='V00'                                                    1222
         CALL AGSPUSH                                                     1222
         AG_MODULE=MODULE                                                 1222
C    *                                                                    1222
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        1223
         CALL AGSRESET                                                    1223
         AG_TITLE = 'BERILLIUM'                                           1223
         AG_EXNAME = 'MATERIAL'                                           1223
         AG_PARLIST = 'NONE'                                              1223
         CALL AXMATERIAL                                                  1223
C    *                                                                    1223
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       1223
         CALL AGSRESET                                                    1224
         AG_SEEN = 1                                                      1224
         AG_COLO = 2                                                      1224
         AG_TITLE = 'SCRW'                                                1224
         AG_EXNAME = 'ATTRIBUTE'                                          1224
         AG_PARLIST = 'SEEN_COLO'                                         1224
         CALL AXATTRIBUTE                                                 1224
C    *                                                                    1224
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  1224
C    NDRNG_Z) <===                                                        1224
         CALL AGSRESET                                                    1225
         AG_RMIN = 0                                                      1225
         AG_RMAX = SSUP_ERJDIA/2                                          1225
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   1225
         AG_TITLE = 'TUBE'                                                1225
         AG_EXNAME = 'SHAPE'                                              1225
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1225
         CALL AXSHAPE                                                     1225
C    *                                                                    1225
6051  CALL AGSPOPB                                                        1226
      GO TO BLOCK_SCRW, (6050 ,6060,6090)                                 1226
C End of Local Procedure                                                  1226
C ----------------------------------------------------------------------  1227
C ------------ SFMO                                                       1227
C Begin Local Procedure SFMO                                              1228
C SFMO IS THE MOTHER OF THE FOURTH LAYER (STRIP DETECTORS)                1228
5300  CONTINUE                                                            1228
         AG_TITLE='SFMO'                                                  1228
         AG_MODULE=MODULE                                                 1228
         AG_MARK='V00'                                                    1228
         CALL AGSPUSH                                                     1228
         AG_MODULE=MODULE                                                 1228
C    *                                                                    1228
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1229
         CALL AGSRESET                                                    1229
         AG_TITLE = 'AIR'                                                 1229
         AG_EXNAME = 'MATERIAL'                                           1229
         AG_PARLIST = 'NONE'                                              1229
         CALL AXMATERIAL                                                  1229
C    *                                                                    1229
C    Geant ATTRIBUTE : SFMO ===> SEEN=0 COLO=1 <===                       1229
         CALL AGSRESET                                                    1230
         AG_SEEN = 0                                                      1230
         AG_COLO = 1                                                      1230
         AG_TITLE = 'SFMO'                                                1230
         AG_EXNAME = 'ATTRIBUTE'                                          1230
         AG_PARLIST = 'SEEN_COLO'                                         1230
         CALL AXATTRIBUTE                                                 1230
C    *                                                                    1230
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  1230
C    2 <===                                                               1230
         CALL AGSRESET                                                    1232
         AG_RMIN = SFPA_RMIN                                              1232
         AG_RMAX = SFPA_RMAX                                              1232
         AG_DZ = SFPA_LEN/2                                               1232
         AG_TITLE = 'TUBE'                                                1232
         AG_EXNAME = 'SHAPE'                                              1232
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1232
         CALL AXSHAPE                                                     1232
C    *                                                                    1232
         DTHK=SFPA_SMTHK + SFPA_GPTHK                                     1232
         RADTILT=(SFPA_TILT*PI)/180.                                      1234
C    *                                                                    1235
         ILAD=1                                                           1235
            GO TO 6553                                                    1235
6551        ILAD=ILAD+1                                                   1235
6553        IF(ILAD-(20).GT.0) GO TO 6552                                 1235
C       Create SFLM :                                                     1236
C       Call To Local Procedure SFLM                                      1236
            ASSIGN 6570 TO BLOCK_SFLM                                     1236
            GO TO  6560                                                   1236
6570        CONTINUE                                                      1236
C       *                                                                 1236
            ANG=(FLOAT(ILAD)-1)*2.0*PI/20.0                               1237
C       Geant POSITION : SFLM ===> X=-(SFPA_RAD*SIN(ANG)+(DTHK*SIN(ANG+R  1240
C       ADTILT))/2) Y=(SFPA_RAD*COS(ANG)+(DTHK*COS(ANG+RADTILT))/2) Z=0   1240
C       ALPHAZ=(180.*ANG/PI)+SFPA_TILT <===                               1240
            CALL AGSRESET                                                 1240
            AG_X = -(SFPA_RAD*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/2)         1240
            AG_Y = (SFPA_RAD*COS(ANG)+(DTHK*COS(ANG+RADTILT))/2)          1240
            AG_Z = 0                                                      1240
            AG_ALPHAN=AG_ALPHAN+1                                         1240
            AG_ALPHAK(AG_ALPHAN)=3                                        1240
            AG_ALPHAL(AG_ALPHAN)=(180.*ANG/PI)+SFPA_TILT                  1240
            AG_TITLE = 'SFLM'                                             1240
            AG_EXNAME = 'POSITION'                                        1240
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   1240
            CALL AXPOSITION                                               1240
C       *                                                                 1240
         GO TO 6551                                                       1241
6552     CONTINUE                                                         1241
5301  CALL AGSPOPB                                                        1242
      GO TO BLOCK_SFMO, (5300 ,5310)                                      1242
C End of Local Procedure                                                  1242
C ----------------------------------------------------------------------  1243
C ------------ SFLM                                                       1243
C Begin Local Procedure SFLM                                              1244
C SFLM IS THE MOTHER OF THE 4TH LAYER LADDER                              1244
6560  CONTINUE                                                            1244
         AG_TITLE='SFLM'                                                  1244
         AG_MODULE=MODULE                                                 1244
         AG_MARK='V00'                                                    1244
         CALL AGSPUSH                                                     1244
         AG_MODULE=MODULE                                                 1244
C    *                                                                    1244
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1245
         CALL AGSRESET                                                    1245
         AG_TITLE = 'AIR'                                                 1245
         AG_EXNAME = 'MATERIAL'                                           1245
         AG_PARLIST = 'NONE'                                              1245
         CALL AXMATERIAL                                                  1245
C    *                                                                    1245
C    Geant ATTRIBUTE : SFLM ===> SEEN=0 COLO=1 <===                       1245
         CALL AGSRESET                                                    1246
         AG_SEEN = 0                                                      1246
         AG_COLO = 1                                                      1246
         AG_TITLE = 'SFLM'                                                1246
         AG_EXNAME = 'ATTRIBUTE'                                          1246
         AG_PARLIST = 'SEEN_COLO'                                         1246
         CALL AXATTRIBUTE                                                 1246
C    *                                                                    1246
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  1246
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    1246
         CALL AGSRESET                                                    1248
         AG_DX = SFPA_DMWID/2                                             1248
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     1248
         AG_DZ = SFPA_SMLEN/2                                             1248
         AG_TITLE = 'BOX'                                                 1248
         AG_EXNAME = 'SHAPE'                                              1248
         AG_PARLIST = 'DX_DY_DZ'                                          1248
         CALL AXSHAPE                                                     1248
C    *                                                                    1248
C    Create SFDM :                                                        1249
C    Call To Local Procedure SFDM                                         1249
         ASSIGN 6590 TO BLOCK_SFDM                                        1249
         GO TO  6580                                                      1249
6590     CONTINUE                                                         1249
C    *                                                                    1249
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         1250
         CALL AGSRESET                                                    1250
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                1250
         AG_TITLE = 'SFDM'                                                1250
         AG_EXNAME = 'POSITION'                                           1250
         AG_PARLIST = 'Y'                                                 1250
         CALL AXPOSITION                                                  1250
C    *                                                                    1250
C    Create SFSM :                                                        1251
C    Call To Local Procedure SFSM                                         1251
         ASSIGN 6610 TO BLOCK_SFSM                                        1251
         GO TO  6600                                                      1251
6610     CONTINUE                                                         1251
C    *                                                                    1251
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          1252
         CALL AGSRESET                                                    1252
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 1252
         AG_TITLE = 'SFSM'                                                1252
         AG_EXNAME = 'POSITION'                                           1252
         AG_PARLIST = 'Y'                                                 1252
         CALL AXPOSITION                                                  1252
C    *                                                                    1252
6561  CALL AGSPOPB                                                        1253
      GO TO BLOCK_SFLM, (6560 ,6570)                                      1253
C End of Local Procedure                                                  1253
C ----------------------------------------------------------------------  1254
C ------------ SFDM                                                       1254
C Begin Local Procedure SFDM                                              1255
C SFDM IS THE MOTHER OF THE DETECTORS                                     1255
6580  CONTINUE                                                            1255
         AG_TITLE='SFDM'                                                  1255
         AG_MODULE=MODULE                                                 1255
         AG_MARK='V00'                                                    1255
         CALL AGSPUSH                                                     1255
         AG_MODULE=MODULE                                                 1255
C    *                                                                    1255
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1256
         CALL AGSRESET                                                    1256
         AG_TITLE = 'AIR'                                                 1256
         AG_EXNAME = 'MATERIAL'                                           1256
         AG_PARLIST = 'NONE'                                              1256
         CALL AXMATERIAL                                                  1256
C    *                                                                    1256
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=1 <===                       1256
         CALL AGSRESET                                                    1257
         AG_SEEN = 0                                                      1257
         AG_COLO = 1                                                      1257
         AG_TITLE = 'SFDM'                                                1257
         AG_EXNAME = 'ATTRIBUTE'                                          1257
         AG_PARLIST = 'SEEN_COLO'                                         1257
         CALL AXATTRIBUTE                                                 1257
C    *                                                                    1257
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  1257
C    EN/2 <===                                                            1257
         CALL AGSRESET                                                    1259
         AG_DX = SFPA_DMWID/2                                             1259
         AG_DY = SFPA_DMTHK/2                                             1259
         AG_DZ = SFPA_WPLEN/2                                             1259
         AG_TITLE = 'BOX'                                                 1259
         AG_EXNAME = 'SHAPE'                                              1259
         AG_PARLIST = 'DX_DY_DZ'                                          1259
         CALL AXSHAPE                                                     1259
C    *                                                                    1259
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              1259
C    *                                                                    1261
         IWAF=1                                                           1261
            GO TO 6623                                                    1261
6621        IWAF=IWAF+1                                                   1261
6623        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 6622                          1261
C       Create SFSW :                                                     1262
C       Call To Local Procedure SFSW                                      1262
            ASSIGN 6640 TO BLOCK_SFSW                                     1262
            GO TO  6630                                                   1262
6640        CONTINUE                                                      1262
C       *                                                                 1262
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  1263
C       CKLEN <===                                                        1263
            CALL AGSRESET                                                 1263
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               1263
            AG_TITLE = 'SFSW'                                             1263
            AG_EXNAME = 'POSITION'                                        1263
            AG_PARLIST = 'Z'                                              1263
            CALL AXPOSITION                                               1263
C       *                                                                 1263
         GO TO 6621                                                       1264
6622     CONTINUE                                                         1264
6581  CALL AGSPOPB                                                        1265
      GO TO BLOCK_SFDM, (6580 ,6590)                                      1265
C End of Local Procedure                                                  1265
C ----------------------------------------------------------------------  1266
C ------------ SFSW                                                       1266
C Begin Local Procedure SFSW                                              1267
C SFSW IS A SINGLE WAFER CONTAINER                                        1267
6630  CONTINUE                                                            1267
         AG_TITLE='SFSW'                                                  1267
         AG_MODULE=MODULE                                                 1267
         AG_MARK='V00'                                                    1267
         CALL AGSPUSH                                                     1267
         AG_MODULE=MODULE                                                 1267
C    *                                                                    1267
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=1 <===                       1268
         CALL AGSRESET                                                    1268
         AG_SEEN = 0                                                      1268
         AG_COLO = 1                                                      1268
         AG_TITLE = 'SFSW'                                                1268
         AG_EXNAME = 'ATTRIBUTE'                                          1268
         AG_PARLIST = 'SEEN_COLO'                                         1268
         CALL AXATTRIBUTE                                                 1268
C    *                                                                    1268
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=WAFPCKLE  1268
C    N/2 <===                                                             1268
         CALL AGSRESET                                                    1270
         AG_DX = SFPA_DMWID/2                                             1270
         AG_DY = SFPA_DMTHK/2                                             1270
         AG_DZ = WAFPCKLEN/2                                              1270
         AG_TITLE = 'BOX'                                                 1270
         AG_EXNAME = 'SHAPE'                                              1270
         AG_PARLIST = 'DX_DY_DZ'                                          1270
         CALL AXSHAPE                                                     1270
C    *                                                                    1270
C    Create SFSD :                                                        1271
C    Call To Local Procedure SFSD                                         1271
         ASSIGN 6660 TO BLOCK_SFSD                                        1271
         GO TO  6650                                                      1271
6660     CONTINUE                                                         1271
C    *                                                                    1271
C    Geant POSITION : SFSD ===> inherited arguments used <===             1272
         CALL AGSRESET                                                    1272
         AG_TITLE = 'SFSD'                                                1272
         AG_EXNAME = 'POSITION'                                           1272
         AG_PARLIST = 'NONE'                                              1272
         CALL AXPOSITION                                                  1272
C    *                                                                    1272
6631  CALL AGSPOPB                                                        1273
      GO TO BLOCK_SFSW, (6630 ,6640)                                      1273
C End of Local Procedure                                                  1273
C ----------------------------------------------------------------------  1274
C ------------ SFSD                                                       1274
C Begin Local Procedure SFSD                                              1275
C SFSD IS THE STRIP DETECTOR                                              1275
6650  CONTINUE                                                            1275
         AG_TITLE='SFSD'                                                  1275
         AG_MODULE=MODULE                                                 1275
         AG_MARK='V00'                                                    1275
         CALL AGSPUSH                                                     1275
         AG_MODULE=MODULE                                                 1275
C    *                                                                    1275
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1276
         CALL AGSRESET                                                    1276
         AG_TITLE = 'SILICON'                                             1276
         AG_EXNAME = 'MATERIAL'                                           1276
         AG_PARLIST = 'NONE'                                              1276
         CALL AXMATERIAL                                                  1276
C    *                                                                    1276
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         1276
         CALL AGSRESET                                                    1277
         AG_ISVOL = 1                                                     1277
         AG_TITLE = 'SENSITIVE'                                           1277
         AG_EXNAME = 'MATERIAL'                                           1277
         AG_PARLIST = 'ISVOL'                                             1277
         CALL AXMATERIAL                                                  1277
C    *                                                                    1277
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=4 <===                       1277
         CALL AGSRESET                                                    1278
         AG_SEEN = 2                                                      1278
         AG_COLO = 4                                                      1278
         AG_TITLE = 'SFSD'                                                1278
         AG_EXNAME = 'ATTRIBUTE'                                          1278
         AG_PARLIST = 'SEEN_COLO'                                         1278
         CALL AXATTRIBUTE                                                 1278
C    *                                                                    1278
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_SDL  1278
C    EN/2 <===                                                            1278
         CALL AGSRESET                                                    1280
         AG_DX = SFPA_DMWID/2                                             1280
         AG_DY = SFPA_DMTHK/2                                             1280
         AG_DZ = SFPA_SDLEN/2                                             1280
         AG_TITLE = 'BOX'                                                 1280
         AG_EXNAME = 'SHAPE'                                              1280
         AG_PARLIST = 'DX_DY_DZ'                                          1280
         CALL AXSHAPE                                                     1280
C    *                                                                    1280
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 1280
C    Defining Hits: X:.001:S Y:.001: Z:.001: CX:10: CY:10: CZ:10: STEP:.  1283
C    01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100) ELOSS:16:(0  1283
C    ,0.001)                                                              1283
         AG_TITLE = 'SFSD'                                                1283
         AG_EXNAME = 'HITS'                                               1283
         AG_TYPE=AG_MAGIC                                                 1283
         CALL AGSDETV('SFSD')                                             1283
C    *                                                                    1283
         AG_PARLIST='X'                                                   1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = AG_MAGIC                                              1283
         AG_BIN=.001                                                      1283
         AG_OPTION='S'                                                    1283
         AG_FHMIN = AG_MAGIC                                              1283
         AG_FHMAX = AG_MAGIC                                              1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='Y'                                                   1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = AG_MAGIC                                              1283
         AG_BIN=.001                                                      1283
         AG_FHMIN=AG_MAGIC                                                1283
         AG_FHMAX=AG_MAGIC                                                1283
         AG_OPTION='*'                                                    1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='Z'                                                   1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = AG_MAGIC                                              1283
         AG_BIN=.001                                                      1283
         AG_FHMIN=AG_MAGIC                                                1283
         AG_FHMAX=AG_MAGIC                                                1283
         AG_OPTION='*'                                                    1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='CX'                                                  1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = 10                                                    1283
         AG_BIN=AG_MAGIC                                                  1283
         AG_FHMIN=AG_MAGIC                                                1283
         AG_FHMAX=AG_MAGIC                                                1283
         AG_OPTION='*'                                                    1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='CY'                                                  1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = 10                                                    1283
         AG_BIN=AG_MAGIC                                                  1283
         AG_FHMIN=AG_MAGIC                                                1283
         AG_FHMAX=AG_MAGIC                                                1283
         AG_OPTION='*'                                                    1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='CZ'                                                  1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = 10                                                    1283
         AG_BIN=AG_MAGIC                                                  1283
         AG_FHMIN=AG_MAGIC                                                1283
         AG_FHMAX=AG_MAGIC                                                1283
         AG_OPTION='*'                                                    1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='STEP'                                                1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = AG_MAGIC                                              1283
         AG_BIN=.01                                                       1283
         AG_FHMIN=AG_MAGIC                                                1283
         AG_FHMAX=AG_MAGIC                                                1283
         AG_OPTION='*'                                                    1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='SLENG'                                               1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = 16                                                    1283
         AG_BIN=AG_MAGIC                                                  1283
         AG_OPTION='*'                                                    1283
         AG_FHMIN = 0                                                     1283
         AG_FHMAX = 500                                                   1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='TOF'                                                 1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = 16                                                    1283
         AG_BIN=AG_MAGIC                                                  1283
         AG_OPTION='*'                                                    1283
         AG_FHMIN = 0                                                     1283
         AG_FHMAX = 1.0E-6                                                1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='PTOT'                                                1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = 16                                                    1283
         AG_BIN=AG_MAGIC                                                  1283
         AG_OPTION='*'                                                    1283
         AG_FHMIN = 0                                                     1283
         AG_FHMAX = 100                                                   1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         AG_PARLIST='ELOSS'                                               1283
         AG_TITLE = 'DETH'                                                1283
         AG_NBITS = 16                                                    1283
         AG_BIN=AG_MAGIC                                                  1283
         AG_OPTION='*'                                                    1283
         AG_FHMIN = 0                                                     1283
         AG_FHMAX = 0.001                                                 1283
         CALL AGSDETH (0)                                                 1283
C    *                                                                    1283
         CALL AGSDETU('SFSD')                                             1283
C    *                                                                    1283
6651  CALL AGSPOPB                                                        1284
      GO TO BLOCK_SFSD, (6650 ,6660)                                      1284
C End of Local Procedure                                                  1284
C ----------------------------------------------------------------------  1285
C ------------ SFSM                                                       1285
C Begin Local Procedure SFSM                                              1286
C SFSM IS THE MOTHER OF THE LADDER STRUCT.                                1286
6600  CONTINUE                                                            1286
         AG_TITLE='SFSM'                                                  1286
         AG_MODULE=MODULE                                                 1286
         AG_MARK='V00'                                                    1286
         CALL AGSPUSH                                                     1286
         AG_MODULE=MODULE                                                 1286
C    *                                                                    1286
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1287
         CALL AGSRESET                                                    1287
         AG_TITLE = 'AIR'                                                 1287
         AG_EXNAME = 'MATERIAL'                                           1287
         AG_PARLIST = 'NONE'                                              1287
         CALL AXMATERIAL                                                  1287
C    *                                                                    1287
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=1 <===                       1287
         CALL AGSRESET                                                    1288
         AG_SEEN = 0                                                      1288
         AG_COLO = 1                                                      1288
         AG_TITLE = 'SFSM'                                                1288
         AG_EXNAME = 'ATTRIBUTE'                                          1288
         AG_PARLIST = 'SEEN_COLO'                                         1288
         CALL AXATTRIBUTE                                                 1288
C    *                                                                    1288
C    Geant SHAPE : BOX ===> DX=SFPA_SMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  1288
C    EN/2 <===                                                            1288
         CALL AGSRESET                                                    1290
         AG_DX = SFPA_SMWID/2                                             1290
         AG_DY = SFPA_SMTHK/2                                             1290
         AG_DZ = SFPA_SMLEN/2                                             1290
         AG_TITLE = 'BOX'                                                 1290
         AG_EXNAME = 'SHAPE'                                              1290
         AG_PARLIST = 'DX_DY_DZ'                                          1290
         CALL AXSHAPE                                                     1290
C    *                                                                    1290
C    Create SFSS :                                                        1291
C    Call To Local Procedure SFSS                                         1291
         ASSIGN 6680 TO BLOCK_SFSS                                        1291
         GO TO  6670                                                      1291
6680     CONTINUE                                                         1291
C    *                                                                    1291
6601  CALL AGSPOPB                                                        1292
      GO TO BLOCK_SFSM, (6600 ,6610)                                      1292
C End of Local Procedure                                                  1292
C ----------------------------------------------------------------------  1293
C ------------ SFSS                                                       1293
C Begin Local Procedure SFSS                                              1294
C SFSS IS THE SUBVOLUME OF THE MOTHER STRUCT.                             1294
6670  CONTINUE                                                            1294
         AG_TITLE='SFSS'                                                  1294
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
C    Geant ATTRIBUTE : SFSS ===> SEEN=0 COLO=1 <===                       1295
         CALL AGSRESET                                                    1296
         AG_SEEN = 0                                                      1296
         AG_COLO = 1                                                      1296
         AG_TITLE = 'SFSS'                                                1296
         AG_EXNAME = 'ATTRIBUTE'                                          1296
         AG_PARLIST = 'SEEN_COLO'                                         1296
         CALL AXATTRIBUTE                                                 1296
C    *                                                                    1296
C    Geant SHAPE : DIVISION ===> NDIV=20 IAXIS=3 <===                     1296
         CALL AGSRESET                                                    1297
         AG_NDIV = 20                                                     1297
         AG_IAXIS = 3                                                     1297
         AG_TITLE = 'DIVISION'                                            1297
         AG_EXNAME = 'SHAPE'                                              1297
         AG_PARLIST = 'NDIV_IAXIS'                                        1297
         CALL AXSHAPE                                                     1297
C    *                                                                    1297
C    Create SFCP :                                                        1298
C    Call To Local Procedure SFCP                                         1298
         ASSIGN 6700 TO BLOCK_SFCP                                        1298
         GO TO  6690                                                      1298
6700     CONTINUE                                                         1298
C    *                                                                    1298
C    Geant POSITION : SFCP ===> X=SFPA_SMWID/2-5.*SFPA_CPRAD Y=-SFPA_SMT  1299
C    HK/2+SFPA_CPRAD <===                                                 1299
         CALL AGSRESET                                                    1299
         AG_X = SFPA_SMWID/2-5.*SFPA_CPRAD                                1299
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  1299
         AG_TITLE = 'SFCP'                                                1299
         AG_EXNAME = 'POSITION'                                           1299
         AG_PARLIST = 'X_Y'                                               1299
         CALL AXPOSITION                                                  1299
C    *                                                                    1299
C    Geant POSITION : SFCP ===> X=-SFPA_SMWID/2+5.*SFPA_CPRAD Y=-SFPA_SM  1299
C    THK/2+SFPA_CPRAD <===                                                1299
         CALL AGSRESET                                                    1300
         AG_X = -SFPA_SMWID/2+5.*SFPA_CPRAD                               1300
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  1300
         AG_TITLE = 'SFCP'                                                1300
         AG_EXNAME = 'POSITION'                                           1300
         AG_PARLIST = 'X_Y'                                               1300
         CALL AXPOSITION                                                  1300
C    *                                                                    1300
C    Create SFCF :                                                        1301
C    Call To Local Procedure SFCF                                         1301
         ASSIGN 6720 TO BLOCK_SFCF                                        1301
         GO TO  6710                                                      1301
6720     CONTINUE                                                         1301
C    *                                                                    1301
C    Geant POSITION : SFCF ===> inherited arguments used <===             1302
         CALL AGSRESET                                                    1302
         AG_TITLE = 'SFCF'                                                1302
         AG_EXNAME = 'POSITION'                                           1302
         AG_PARLIST = 'NONE'                                              1302
         CALL AXPOSITION                                                  1302
C    *                                                                    1302
6671  CALL AGSPOPB                                                        1303
      GO TO BLOCK_SFSS, (6670 ,6680)                                      1303
C End of Local Procedure                                                  1303
C ----------------------------------------------------------------------  1304
C ------------ SFCP                                                       1304
C Begin Local Procedure SFCP                                              1305
C SFCP IS THE COOLING PIPE                                                1305
6690  CONTINUE                                                            1305
         AG_TITLE='SFCP'                                                  1305
         AG_MODULE=MODULE                                                 1305
         AG_MARK='V00'                                                    1305
         CALL AGSPUSH                                                     1305
         AG_MODULE=MODULE                                                 1305
C    *                                                                    1305
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1306
         CALL AGSRESET                                                    1306
         AG_TITLE = 'CARBON'                                              1306
         AG_EXNAME = 'MATERIAL'                                           1306
         AG_PARLIST = 'NONE'                                              1306
         CALL AXMATERIAL                                                  1306
C    *                                                                    1306
C    Geant ATTRIBUTE : SFCP ===> SEEN=1 COLO=6 <===                       1306
         CALL AGSRESET                                                    1307
         AG_SEEN = 1                                                      1307
         AG_COLO = 6                                                      1307
         AG_TITLE = 'SFCP'                                                1307
         AG_EXNAME = 'ATTRIBUTE'                                          1307
         AG_PARLIST = 'SEEN_COLO'                                         1307
         CALL AXATTRIBUTE                                                 1307
C    *                                                                    1307
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CPRAD DZ=SFPA_SSLEN/2 <===  1307
         CALL AGSRESET                                                    1308
         AG_RMIN = 0                                                      1308
         AG_RMAX = SFPA_CPRAD                                             1308
         AG_DZ = SFPA_SSLEN/2                                             1308
         AG_TITLE = 'TUBE'                                                1308
         AG_EXNAME = 'SHAPE'                                              1308
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1308
         CALL AXSHAPE                                                     1308
C    *                                                                    1308
C    Create SFCW :                                                        1309
C    Call To Local Procedure SFCW                                         1309
         ASSIGN 6740 TO BLOCK_SFCW                                        1309
         GO TO  6730                                                      1309
6740     CONTINUE                                                         1309
C    *                                                                    1309
C    Geant POSITION : SFCW ===> inherited arguments used <===             1310
         CALL AGSRESET                                                    1310
         AG_TITLE = 'SFCW'                                                1310
         AG_EXNAME = 'POSITION'                                           1310
         AG_PARLIST = 'NONE'                                              1310
         CALL AXPOSITION                                                  1310
C    *                                                                    1310
6691  CALL AGSPOPB                                                        1311
      GO TO BLOCK_SFCP, (6690 ,6700)                                      1311
C End of Local Procedure                                                  1311
C ----------------------------------------------------------------------  1312
C ------------ SFCW                                                       1312
C Begin Local Procedure SFCW                                              1313
C SFCW IS THE WATER CYLINDER IN THE COOLING PIPE                          1313
6730  CONTINUE                                                            1313
         AG_TITLE='SFCW'                                                  1313
         AG_MODULE=MODULE                                                 1313
         AG_MARK='V00'                                                    1313
         CALL AGSPUSH                                                     1313
         AG_MODULE=MODULE                                                 1313
C    *                                                                    1313
C    Geant ATTRIBUTE : SFCW ===> SEEN=1 COLO=6 <===                       1314
         CALL AGSRESET                                                    1314
         AG_SEEN = 1                                                      1314
         AG_COLO = 6                                                      1314
         AG_TITLE = 'SFCW'                                                1314
         AG_EXNAME = 'ATTRIBUTE'                                          1314
         AG_PARLIST = 'SEEN_COLO'                                         1314
         CALL AXATTRIBUTE                                                 1314
C    *                                                                    1314
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1314
         CALL AGSRESET                                                    1315
         AG_TITLE = 'WATER'                                               1315
         AG_EXNAME = 'MATERIAL'                                           1315
         AG_PARLIST = 'NONE'                                              1315
         CALL AXMATERIAL                                                  1315
C    *                                                                    1315
C    Geant SHAPE : TUBE ===> RMAX=SFPA_CPRAL <===                         1315
         CALL AGSRESET                                                    1316
         AG_RMAX = SFPA_CPRAL                                             1316
         AG_TITLE = 'TUBE'                                                1316
         AG_EXNAME = 'SHAPE'                                              1316
         AG_PARLIST = 'RMAX'                                              1316
         CALL AXSHAPE                                                     1316
C    *                                                                    1316
6731  CALL AGSPOPB                                                        1317
      GO TO BLOCK_SFCW, (6730 ,6740)                                      1317
C End of Local Procedure                                                  1317
C ----------------------------------------------------------------------  1318
C ------------ SFCF                                                       1318
C Begin Local Procedure SFCF                                              1319
C SFCF IS THE CARBON FIBER STRUCTURE CONTAINER                            1319
6710  CONTINUE                                                            1319
         AG_TITLE='SFCF'                                                  1319
         AG_MODULE=MODULE                                                 1319
         AG_MARK='V00'                                                    1319
         CALL AGSPUSH                                                     1319
         AG_MODULE=MODULE                                                 1319
C    *                                                                    1319
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1320
         CALL AGSRESET                                                    1320
         AG_TITLE = 'AIR'                                                 1320
         AG_EXNAME = 'MATERIAL'                                           1320
         AG_PARLIST = 'NONE'                                              1320
         CALL AXMATERIAL                                                  1320
C    *                                                                    1320
C    Geant ATTRIBUTE : SFCF ===> SEEN=0 COLO=3 <===                       1320
         CALL AGSRESET                                                    1321
         AG_SEEN = 0                                                      1321
         AG_COLO = 3                                                      1321
         AG_TITLE = 'SFCF'                                                1321
         AG_EXNAME = 'ATTRIBUTE'                                          1321
         AG_PARLIST = 'SEEN_COLO'                                         1321
         CALL AXATTRIBUTE                                                 1321
C    *                                                                    1321
C    Geant SHAPE : BOX ===> DX=SFPA_SMTHK*TAN(PI/6.) DY=SFPA_SMTHK/2., D  1321
C    Z=SFPA_SSLEN/2 <===                                                  1321
         CALL AGSRESET                                                    1323
         AG_DX = SFPA_SMTHK*TAN(PI/6.)                                    1323
         AG_DY = SFPA_SMTHK/2.                                            1323
         AG_DZ = SFPA_SSLEN/2                                             1323
         AG_TITLE = 'BOX'                                                 1323
         AG_EXNAME = 'SHAPE'                                              1323
         AG_PARLIST = 'DX_DY_DZ'                                          1323
         CALL AXSHAPE                                                     1323
C    *                                                                    1323
C    Create SFCT :                                                        1324
C    Call To Local Procedure SFCT                                         1324
         ASSIGN 6760 TO BLOCK_SFCT                                        1324
         GO TO  6750                                                      1324
6760     CONTINUE                                                         1324
C    *                                                                    1324
C    Geant POSITION : SFCT ===> Y=SFPA_SMTHK/2.-SFPA_CFRAD <===           1325
         CALL AGSRESET                                                    1325
         AG_Y = SFPA_SMTHK/2.-SFPA_CFRAD                                  1325
         AG_TITLE = 'SFCT'                                                1325
         AG_EXNAME = 'POSITION'                                           1325
         AG_PARLIST = 'Y'                                                 1325
         CALL AXPOSITION                                                  1325
C    *                                                                    1325
C    Geant POSITION : SFCT ===> X=SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD Y=-SF  1325
C    PA_SMTHK/2.+SFPA_CFRAD <===                                          1325
         CALL AGSRESET                                                    1326
         AG_X = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                          1326
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1326
         AG_TITLE = 'SFCT'                                                1326
         AG_EXNAME = 'POSITION'                                           1326
         AG_PARLIST = 'X_Y'                                               1326
         CALL AXPOSITION                                                  1326
C    *                                                                    1326
C    Geant POSITION : SFCT ===> X=-SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD Y=-S  1326
C    FPA_SMTHK/2.+SFPA_CFRAD <===                                         1326
         CALL AGSRESET                                                    1327
         AG_X = -SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD                         1327
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1327
         AG_TITLE = 'SFCT'                                                1327
         AG_EXNAME = 'POSITION'                                           1327
         AG_PARLIST = 'X_Y'                                               1327
         CALL AXPOSITION                                                  1327
C    *                                                                    1327
C    Create SFCX :                                                        1328
C    Call To Local Procedure SFCX                                         1328
         ASSIGN 6780 TO BLOCK_SFCX                                        1328
         GO TO  6770                                                      1328
6780     CONTINUE                                                         1328
C    *                                                                    1328
C    Geant POSITION : SFCX ===> Y=-SFPA_SMTHK/2.+SFPA_CFRAD ORT=YZX <===  1329
         CALL AGSRESET                                                    1329
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1329
         AG_ALPHAN=AG_ALPHAN+1                                            1329
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1329
         AG_TITLE = 'SFCX'                                                1329
         AG_EXNAME = 'POSITION'                                           1329
         AG_PARLIST = 'Y_ORT'                                             1329
         CALL AXPOSITION                                                  1329
C    *                                                                    1329
C    Geant POSITION : SFCX ===> X=SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.  1329
C     ORT=YZX ALPHAZ=-60 <===                                             1329
         CALL AGSRESET                                                    1330
         AG_X = SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.                    1330
         AG_ALPHAN=AG_ALPHAN+1                                            1330
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1330
         AG_ALPHAN=AG_ALPHAN+1                                            1330
         AG_ALPHAK(AG_ALPHAN)=3                                           1330
         AG_ALPHAL(AG_ALPHAN)=-60                                         1330
         AG_TITLE = 'SFCX'                                                1330
         AG_EXNAME = 'POSITION'                                           1330
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      1330
         CALL AXPOSITION                                                  1330
C    *                                                                    1330
C    Geant POSITION : SFCX ===> X=-SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3  1330
C    . ORT=YZX ALPHAZ=+60 <===                                            1330
         CALL AGSRESET                                                    1331
         AG_X = -SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3.                   1331
         AG_ALPHAN=AG_ALPHAN+1                                            1331
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1331
         AG_ALPHAN=AG_ALPHAN+1                                            1331
         AG_ALPHAK(AG_ALPHAN)=3                                           1331
         AG_ALPHAL(AG_ALPHAN)=+60                                         1331
         AG_TITLE = 'SFCX'                                                1331
         AG_EXNAME = 'POSITION'                                           1331
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      1331
         CALL AXPOSITION                                                  1331
C    *                                                                    1331
6711  CALL AGSPOPB                                                        1332
      GO TO BLOCK_SFCF, (6710 ,6720)                                      1332
C End of Local Procedure                                                  1332
C ----------------------------------------------------------------------  1333
C ------------ SFCT                                                       1333
C Begin Local Procedure SFCT                                              1334
C SFCT IS THE CARBON FIBER TUBE                                           1334
6750  CONTINUE                                                            1334
         AG_TITLE='SFCT'                                                  1334
         AG_MODULE=MODULE                                                 1334
         AG_MARK='V00'                                                    1334
         CALL AGSPUSH                                                     1334
         AG_MODULE=MODULE                                                 1334
C    *                                                                    1334
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1335
         CALL AGSRESET                                                    1335
         AG_TITLE = 'CARBON'                                              1335
         AG_EXNAME = 'MATERIAL'                                           1335
         AG_PARLIST = 'NONE'                                              1335
         CALL AXMATERIAL                                                  1335
C    *                                                                    1335
C    Geant ATTRIBUTE : SFCT ===> SEEN=1 COLO=6 <===                       1335
         CALL AGSRESET                                                    1336
         AG_SEEN = 1                                                      1336
         AG_COLO = 6                                                      1336
         AG_TITLE = 'SFCT'                                                1336
         AG_EXNAME = 'ATTRIBUTE'                                          1336
         AG_PARLIST = 'SEEN_COLO'                                         1336
         CALL AXATTRIBUTE                                                 1336
C    *                                                                    1336
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SSLEN/2 <===  1336
         CALL AGSRESET                                                    1337
         AG_RMIN = 0                                                      1337
         AG_RMAX = SFPA_CFRAD                                             1337
         AG_DZ = SFPA_SSLEN/2                                             1337
         AG_TITLE = 'TUBE'                                                1337
         AG_EXNAME = 'SHAPE'                                              1337
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1337
         CALL AXSHAPE                                                     1337
C    *                                                                    1337
6751  CALL AGSPOPB                                                        1338
      GO TO BLOCK_SFCT, (6750 ,6760)                                      1338
C End of Local Procedure                                                  1338
C ----------------------------------------------------------------------  1339
C ------------ SFCX                                                       1339
C Begin Local Procedure SFCX                                              1340
C SFCX IS THE CARBON FIBER TUBE                                           1340
6770  CONTINUE                                                            1340
         AG_TITLE='SFCX'                                                  1340
         AG_MODULE=MODULE                                                 1340
         AG_MARK='V00'                                                    1340
         CALL AGSPUSH                                                     1340
         AG_MODULE=MODULE                                                 1340
C    *                                                                    1340
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1341
         CALL AGSRESET                                                    1341
         AG_TITLE = 'CARBON'                                              1341
         AG_EXNAME = 'MATERIAL'                                           1341
         AG_PARLIST = 'NONE'                                              1341
         CALL AXMATERIAL                                                  1341
C    *                                                                    1341
C    Geant ATTRIBUTE : SFCX ===> SEEN=1 COLO=7 <===                       1341
         CALL AGSRESET                                                    1342
         AG_SEEN = 1                                                      1342
         AG_COLO = 7                                                      1342
         AG_TITLE = 'SFCX'                                                1342
         AG_EXNAME = 'ATTRIBUTE'                                          1342
         AG_PARLIST = 'SEEN_COLO'                                         1342
         CALL AXATTRIBUTE                                                 1342
C    *                                                                    1342
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SMTHK*TAN(PI  1342
C    /6.)-SFPA_CFRAD <===                                                 1342
         CALL AGSRESET                                                    1343
         AG_RMIN = 0                                                      1343
         AG_RMAX = SFPA_CFRAD                                             1343
         AG_DZ = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                         1343
         AG_TITLE = 'TUBE'                                                1343
         AG_EXNAME = 'SHAPE'                                              1343
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1343
         CALL AXSHAPE                                                     1343
C    *                                                                    1343
6771  CALL AGSPOPB                                                        1344
      GO TO BLOCK_SFCX, (6770 ,6780)                                      1344
C End of Local Procedure                                                  1344
C ----------------------------------------------------------------------  1345
C ------------ SBSP                                                       1345
C Begin Local Procedure SBSP                                              1346
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1346
5220  CONTINUE                                                            1346
         AG_TITLE='SBSP'                                                  1346
         AG_MODULE=MODULE                                                 1346
         AG_MARK='V00'                                                    1346
         CALL AGSPUSH                                                     1346
         AG_MODULE=MODULE                                                 1346
C    *                                                                    1346
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1347
         CALL AGSRESET                                                    1347
         AG_TITLE = 'AIR'                                                 1347
         AG_EXNAME = 'MATERIAL'                                           1347
         AG_PARLIST = 'NONE'                                              1347
         CALL AXMATERIAL                                                  1347
C    *                                                                    1347
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1347
         CALL AGSRESET                                                    1348
         AG_SEEN = 0                                                      1348
         AG_COLO = 1                                                      1348
         AG_TITLE = 'SBSP'                                                1348
         AG_EXNAME = 'ATTRIBUTE'                                          1348
         AG_PARLIST = 'SEEN_COLO'                                         1348
         CALL AXATTRIBUTE                                                 1348
C    *                                                                    1348
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1348
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1348
         CALL AGSRESET                                                    1350
         AG_RMIN = SVTG_RSIZEMIN                                          1350
         AG_RMAX = SSUB_KMOUNTOD/2                                        1350
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1350
         AG_TITLE = 'TUBE'                                                1350
         AG_EXNAME = 'SHAPE'                                              1350
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1350
         CALL AXSHAPE                                                     1350
C    *                                                                    1350
C    Create SAKM :                                                        1351
C    Call To Local Procedure SAKM                                         1351
         ASSIGN 6800 TO BLOCK_SAKM                                        1351
         GO TO  6790                                                      1351
6800     CONTINUE                                                         1351
C    *                                                                    1351
C    Geant POSITION : SAKM ===> inherited arguments used <===             1352
         CALL AGSRESET                                                    1352
         AG_TITLE = 'SAKM'                                                1352
         AG_EXNAME = 'POSITION'                                           1352
         AG_PARLIST = 'NONE'                                              1352
         CALL AXPOSITION                                                  1352
C    *                                                                    1352
C    Create SBMM :                                                        1353
C    Call To Local Procedure SBMM                                         1353
         ASSIGN 6820 TO BLOCK_SBMM                                        1353
         GO TO  6810                                                      1353
6820     CONTINUE                                                         1353
C    *                                                                    1353
C    Create SBMM :                                                        1354
C    Call To Local Procedure SBMM                                         1354
         ASSIGN 6830 TO BLOCK_SBMM                                        1354
         GO TO  6810                                                      1354
6830     CONTINUE                                                         1354
C    *                                                                    1354
C    Create SBRL :                                                        1355
C    Call To Local Procedure SBRL                                         1355
         ASSIGN 6850 TO BLOCK_SBRL                                        1355
         GO TO  6840                                                      1355
6850     CONTINUE                                                         1355
C    *                                                                    1355
C    Create SBRX :                                                        1356
C    Call To Local Procedure SBRX                                         1356
         ASSIGN 6870 TO BLOCK_SBRX                                        1356
         GO TO  6860                                                      1356
6870     CONTINUE                                                         1356
C    *                                                                    1356
C    *                                                                    1357
         I=-1                                                             1357
            GOTO 6883                                                     1357
6881        I=I+(2)                                                       1357
6883        IF((2)*(I-(1)).GT.0) GO TO 6882                               1357
C       *                                                                 1358
            J=0                                                           1358
               GO TO 6893                                                 1358
6891           J=J+1                                                      1358
6893           IF(J-(1).GT.0) GO TO 6892                                  1358
               PHI=I*SSUB_SRCUTPHI+180*J                                  1359
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1360
     *         2.0)                                                       1360
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1362
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1362
C          ==                                                             1362
               CALL AGSRESET                                              1362
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1362
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1362
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1362
               AG_ALPHAN=AG_ALPHAN+1                                      1362
               AG_ALPHAK(AG_ALPHAN)=3                                     1362
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1362
               AG_TITLE = 'SBMM'                                          1362
               AG_EXNAME = 'POSITION'                                     1362
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1362
               CALL AXPOSITION                                            1362
C          *                                                              1362
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1362
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1365
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1365
C           ORT=ZXY <===                                                  1365
               CALL AGSRESET                                              1365
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1365
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1365
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1365
               AG_ALPHAN=AG_ALPHAN+1                                      1365
               AG_ALPHAK(AG_ALPHAN)=3                                     1365
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1365
               AG_ALPHAN=AG_ALPHAN+1                                      1365
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1365
               AG_TITLE = 'SBRL'                                          1365
               AG_EXNAME = 'POSITION'                                     1365
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1365
               CALL AXPOSITION                                            1365
C          *                                                              1365
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1365
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1365
C           ORT=ZXY <===                                                  1365
               CALL AGSRESET                                              1367
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1367
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1367
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1367
               AG_ALPHAN=AG_ALPHAN+1                                      1367
               AG_ALPHAK(AG_ALPHAN)=3                                     1367
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1367
               AG_ALPHAN=AG_ALPHAN+1                                      1367
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1367
               AG_TITLE = 'SBRX'                                          1367
               AG_EXNAME = 'POSITION'                                     1367
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1367
               CALL AXPOSITION                                            1367
C          *                                                              1367
            GO TO 6891                                                    1368
6892        CONTINUE                                                      1368
         GO TO 6881                                                       1369
6882     CONTINUE                                                         1369
C    Create SBSR :                                                        1370
C    Call To Local Procedure SBSR                                         1370
         ASSIGN 6910 TO BLOCK_SBSR                                        1370
         GO TO  6900                                                      1370
6910     CONTINUE                                                         1370
C    *                                                                    1370
C    Geant POSITION : SBSR ===> inherited arguments used <===             1371
         CALL AGSRESET                                                    1371
         AG_TITLE = 'SBSR'                                                1371
         AG_EXNAME = 'POSITION'                                           1371
         AG_PARLIST = 'NONE'                                              1371
         CALL AXPOSITION                                                  1371
C    *                                                                    1371
5221  CALL AGSPOPB                                                        1372
      GO TO BLOCK_SBSP, (5220 ,5230)                                      1372
C End of Local Procedure                                                  1372
C ----------------------------------------------------------------------  1373
C ------------ SAKM                                                       1373
C Begin Local Procedure SAKM                                              1374
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1374
6790  CONTINUE                                                            1374
         AG_TITLE='SAKM'                                                  1374
         AG_MODULE=MODULE                                                 1374
         AG_MARK='V00'                                                    1374
         CALL AGSPUSH                                                     1374
         AG_MODULE=MODULE                                                 1374
C    *                                                                    1374
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1375
         CALL AGSRESET                                                    1375
         AG_TITLE = 'ALUMINIUM'                                           1375
         AG_EXNAME = 'MATERIAL'                                           1375
         AG_PARLIST = 'NONE'                                              1375
         CALL AXMATERIAL                                                  1375
C    *                                                                    1375
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1375
         CALL AGSRESET                                                    1376
         AG_SEEN = 1                                                      1376
         AG_COLO = 2                                                      1376
         AG_TITLE = 'SAKM'                                                1376
         AG_EXNAME = 'ATTRIBUTE'                                          1376
         AG_PARLIST = 'SEEN_COLO'                                         1376
         CALL AXATTRIBUTE                                                 1376
C    *                                                                    1376
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1376
C    DZ=SSUB_KMNTTHK/2 <===                                               1376
         CALL AGSRESET                                                    1378
         AG_RMIN = SSUB_KMOUNTID/2                                        1378
         AG_RMAX = SSUB_KMOUNTOD/2                                        1378
         AG_DZ = SSUB_KMNTTHK/2                                           1378
         AG_TITLE = 'TUBE'                                                1378
         AG_EXNAME = 'SHAPE'                                              1378
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1378
         CALL AXSHAPE                                                     1378
C    *                                                                    1378
C    Create SCKM :                                                        1379
C    Call To Local Procedure SCKM                                         1379
         ASSIGN 6930 TO BLOCK_SCKM                                        1379
         GO TO  6920                                                      1379
6930     CONTINUE                                                         1379
C    *                                                                    1379
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1380
         CALL AGSRESET                                                    1380
         AG_Y = SSUB_KMCUTOFF                                             1380
         AG_KONLY = 'MANY'                                                1380
         AG_TITLE = 'SCKM'                                                1380
         AG_EXNAME = 'POSITION'                                           1380
         AG_PARLIST = 'Y_KONLY'                                           1380
         CALL AXPOSITION                                                  1380
C    *                                                                    1380
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1380
C     <===                                                                1380
         CALL AGSRESET                                                    1381
         AG_Y = -SSUB_KMCUTOFF                                            1381
         AG_ALPHAN=AG_ALPHAN+1                                            1381
         AG_ALPHAK(AG_ALPHAN)=3                                           1381
         AG_ALPHAL(AG_ALPHAN)=180                                         1381
         AG_KONLY = 'MANY'                                                1381
         AG_TITLE = 'SCKM'                                                1381
         AG_EXNAME = 'POSITION'                                           1381
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1381
         CALL AXPOSITION                                                  1381
C    *                                                                    1381
6791  CALL AGSPOPB                                                        1382
      GO TO BLOCK_SAKM, (6790 ,6800)                                      1382
C End of Local Procedure                                                  1382
C ----------------------------------------------------------------------  1383
C ------------ SCKM                                                       1383
C Begin Local Procedure SCKM                                              1384
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1384
6920  CONTINUE                                                            1384
         AG_TITLE='SCKM'                                                  1384
         AG_MODULE=MODULE                                                 1384
         AG_MARK='V00'                                                    1384
         CALL AGSPUSH                                                     1384
         AG_MODULE=MODULE                                                 1384
C    *                                                                    1384
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1385
         CALL AGSRESET                                                    1385
         AG_TITLE = 'AIR'                                                 1385
         AG_EXNAME = 'MATERIAL'                                           1385
         AG_PARLIST = 'NONE'                                              1385
         CALL AXMATERIAL                                                  1385
C    *                                                                    1385
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1385
         CALL AGSRESET                                                    1386
         AG_SEEN = 1                                                      1386
         AG_COLO = 1                                                      1386
         AG_TITLE = 'SCKM'                                                1386
         AG_EXNAME = 'ATTRIBUTE'                                          1386
         AG_PARLIST = 'SEEN_COLO'                                         1386
         CALL AXATTRIBUTE                                                 1386
C    *                                                                    1386
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1386
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1386
         CALL AGSRESET                                                    1389
         AG_RMIN = SSUB_KMCUTID/2                                         1389
         AG_RMAX = SSUB_KMCUTOD/2                                         1389
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1389
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1389
         AG_DZ = SSUB_KMNTTHK/2                                           1389
         AG_TITLE = 'TUBS'                                                1389
         AG_EXNAME = 'SHAPE'                                              1389
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1389
         CALL AXSHAPE                                                     1389
C    *                                                                    1389
6921  CALL AGSPOPB                                                        1390
      GO TO BLOCK_SCKM, (6920 ,6930)                                      1390
C End of Local Procedure                                                  1390
C ----------------------------------------------------------------------  1391
C ------------ SBSR                                                       1391
C Begin Local Procedure SBSR                                              1392
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1392
6900  CONTINUE                                                            1392
         AG_TITLE='SBSR'                                                  1392
         AG_MODULE=MODULE                                                 1392
         AG_MARK='V00'                                                    1392
         CALL AGSPUSH                                                     1392
         AG_MODULE=MODULE                                                 1392
C    *                                                                    1392
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1393
         CALL AGSRESET                                                    1393
         AG_TITLE = 'G10'                                                 1393
         AG_EXNAME = 'MATERIAL'                                           1393
         AG_PARLIST = 'NONE'                                              1393
         CALL AXMATERIAL                                                  1393
C    *                                                                    1393
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1393
         CALL AGSRESET                                                    1394
         AG_SEEN = 1                                                      1394
         AG_COLO = 3                                                      1394
         AG_TITLE = 'SBSR'                                                1394
         AG_EXNAME = 'ATTRIBUTE'                                          1394
         AG_PARLIST = 'SEEN_COLO'                                         1394
         CALL AXATTRIBUTE                                                 1394
C    *                                                                    1394
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1394
C    =SSUB_SRINGTHK/2 <===                                                1394
         CALL AGSRESET                                                    1396
         AG_RMIN = SSUB_SRINGID/2                                         1396
         AG_RMAX = SSUB_SRINGOD/2                                         1396
         AG_DZ = SSUB_SRINGTHK/2                                          1396
         AG_TITLE = 'TUBE'                                                1396
         AG_EXNAME = 'SHAPE'                                              1396
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1396
         CALL AXSHAPE                                                     1396
C    *                                                                    1396
C    Create SBCR :                                                        1397
C    Call To Local Procedure SBCR                                         1397
         ASSIGN 6950 TO BLOCK_SBCR                                        1397
         GO TO  6940                                                      1397
6950     CONTINUE                                                         1397
C    *                                                                    1397
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1398
C    *                                                                    1399
         I=-1                                                             1399
            GOTO 6963                                                     1399
6961        I=I+(2)                                                       1399
6963        IF((2)*(I-(1)).GT.0) GO TO 6962                               1399
C       *                                                                 1400
            J=0                                                           1400
               GO TO 6973                                                 1400
6971           J=J+1                                                      1400
6973           IF(J-(1).GT.0) GO TO 6972                                  1400
               PHI=I*SSUB_SRCUTPHI+180*J                                  1401
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1403
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1403
               CALL AGSRESET                                              1403
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1403
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1403
               AG_ALPHAN=AG_ALPHAN+1                                      1403
               AG_ALPHAK(AG_ALPHAN)=3                                     1403
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1403
               AG_KONLY = 'MANY'                                          1403
               AG_TITLE = 'SBCR'                                          1403
               AG_EXNAME = 'POSITION'                                     1403
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1403
               CALL AXPOSITION                                            1403
C          *                                                              1403
            GO TO 6971                                                    1404
6972        CONTINUE                                                      1404
         GO TO 6961                                                       1405
6962     CONTINUE                                                         1405
6901  CALL AGSPOPB                                                        1406
      GO TO BLOCK_SBSR, (6900 ,6910)                                      1406
C End of Local Procedure                                                  1406
C ----------------------------------------------------------------------  1407
C ------------ SBCR                                                       1407
C Begin Local Procedure SBCR                                              1408
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1408
6940  CONTINUE                                                            1408
         AG_TITLE='SBCR'                                                  1408
         AG_MODULE=MODULE                                                 1408
         AG_MARK='V00'                                                    1408
         CALL AGSPUSH                                                     1408
         AG_MODULE=MODULE                                                 1408
C    *                                                                    1408
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1409
         CALL AGSRESET                                                    1409
         AG_TITLE = 'AIR'                                                 1409
         AG_EXNAME = 'MATERIAL'                                           1409
         AG_PARLIST = 'NONE'                                              1409
         CALL AXMATERIAL                                                  1409
C    *                                                                    1409
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1409
         CALL AGSRESET                                                    1410
         AG_SEEN = 1                                                      1410
         AG_COLO = 1                                                      1410
         AG_TITLE = 'SBCR'                                                1410
         AG_EXNAME = 'ATTRIBUTE'                                          1410
         AG_PARLIST = 'SEEN_COLO'                                         1410
         CALL AXATTRIBUTE                                                 1410
C    *                                                                    1410
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1410
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1410
         CALL AGSRESET                                                    1412
         AG_DX = SSUB_SRCUTWID/2                                          1412
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1412
         AG_DZ = SSUB_SRINGTHK/2                                          1412
         AG_TITLE = 'BOX'                                                 1412
         AG_EXNAME = 'SHAPE'                                              1412
         AG_PARLIST = 'DX_DY_DZ'                                          1412
         CALL AXSHAPE                                                     1412
C    *                                                                    1412
6941  CALL AGSPOPB                                                        1413
      GO TO BLOCK_SBCR, (6940 ,6950)                                      1413
C End of Local Procedure                                                  1413
C ----------------------------------------------------------------------  1414
C ------------ SBRL                                                       1414
C Begin Local Procedure SBRL                                              1415
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1415
6840  CONTINUE                                                            1415
         AG_TITLE='SBRL'                                                  1415
         AG_MODULE=MODULE                                                 1415
         AG_MARK='V00'                                                    1415
         CALL AGSPUSH                                                     1415
         AG_MODULE=MODULE                                                 1415
C    *                                                                    1415
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1416
C    ADL=12.6 <===                                                        1416
         CALL AGSRESET                                                    1416
         AG_A = 20.719                                                    1416
         AG_Z = 10.307                                                    1416
         AG_DENS = 2.23                                                   1416
         AG_ABSL = 50.7                                                   1416
         AG_RADL = 12.6                                                   1416
         AG_TITLE = 'PYREX'                                               1416
         AG_EXNAME = 'MATERIAL'                                           1416
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1416
         CALL AXMATERIAL                                                  1416
C    *                                                                    1416
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1416
         CALL AGSRESET                                                    1417
         AG_SEEN = 1                                                      1417
         AG_COLO = 6                                                      1417
         AG_TITLE = 'SBRL'                                                1417
         AG_EXNAME = 'ATTRIBUTE'                                          1417
         AG_PARLIST = 'SEEN_COLO'                                         1417
         CALL AXATTRIBUTE                                                 1417
C    *                                                                    1417
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1417
C    =                                                                    1417
         CALL AGSRESET                                                    1418
         AG_RMIN = SSUB_SROLLID/2                                         1418
         AG_RMAX = SSUB_SROLLOD/2                                         1418
         AG_TITLE = 'TUBE'                                                1418
         AG_EXNAME = 'SHAPE'                                              1418
         AG_PARLIST = 'RMIN_RMAX'                                         1418
         CALL AXSHAPE                                                     1418
C    *                                                                    1418
6841  CALL AGSPOPB                                                        1419
      GO TO BLOCK_SBRL, (6840 ,6850)                                      1419
C End of Local Procedure                                                  1419
C ----------------------------------------------------------------------  1420
C ------------ SBRX                                                       1420
C Begin Local Procedure SBRX                                              1421
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1421
6860  CONTINUE                                                            1421
         AG_TITLE='SBRX'                                                  1421
         AG_MODULE=MODULE                                                 1421
         AG_MARK='V00'                                                    1421
         CALL AGSPUSH                                                     1421
         AG_MODULE=MODULE                                                 1421
C    *                                                                    1421
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1422
         CALL AGSRESET                                                    1422
         AG_TITLE = 'IRON'                                                1422
         AG_EXNAME = 'MATERIAL'                                           1422
         AG_PARLIST = 'NONE'                                              1422
         CALL AXMATERIAL                                                  1422
C    *                                                                    1422
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1422
         CALL AGSRESET                                                    1423
         AG_SEEN = 1                                                      1423
         AG_COLO = 2                                                      1423
         AG_TITLE = 'SBRX'                                                1423
         AG_EXNAME = 'ATTRIBUTE'                                          1423
         AG_PARLIST = 'SEEN_COLO'                                         1423
         CALL AXATTRIBUTE                                                 1423
C    *                                                                    1423
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1423
C    LEN/2 <===                                                           1423
         CALL AGSRESET                                                    1425
         AG_RMIN = 0.0                                                    1425
         AG_RMAX = SSUB_SROLLID/2                                         1425
         AG_DZ = SSUB_SWIRELEN/2                                          1425
         AG_TITLE = 'TUBE'                                                1425
         AG_EXNAME = 'SHAPE'                                              1425
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1425
         CALL AXSHAPE                                                     1425
C    *                                                                    1425
6861  CALL AGSPOPB                                                        1426
      GO TO BLOCK_SBRX, (6860 ,6870)                                      1426
C End of Local Procedure                                                  1426
C ----------------------------------------------------------------------  1427
C ------------ SBMM                                                       1427
C Begin Local Procedure SBMM                                              1428
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1428
6810  CONTINUE                                                            1428
         AG_TITLE='SBMM'                                                  1428
         AG_MODULE=MODULE                                                 1428
         AG_MARK='V00'                                                    1428
         CALL AGSPUSH                                                     1428
         AG_MODULE=MODULE                                                 1428
C    *                                                                    1428
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1429
         CALL AGSRESET                                                    1429
         AG_TITLE = 'AIR'                                                 1429
         AG_EXNAME = 'MATERIAL'                                           1429
         AG_PARLIST = 'NONE'                                              1429
         CALL AXMATERIAL                                                  1429
C    *                                                                    1429
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1429
         CALL AGSRESET                                                    1430
         AG_SEEN = 0                                                      1430
         AG_COLO = 1                                                      1430
         AG_TITLE = 'SBMM'                                                1430
         AG_EXNAME = 'ATTRIBUTE'                                          1430
         AG_PARLIST = 'SEEN_COLO'                                         1430
         CALL AXATTRIBUTE                                                 1430
C    *                                                                    1430
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1430
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1430
         CALL AGSRESET                                                    1432
         AG_DX = SSUB_MBLKIWID/2                                          1432
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1432
         AG_DZ = SSUB_MBLKHGH/2                                           1432
         AG_TITLE = 'BOX'                                                 1432
         AG_EXNAME = 'SHAPE'                                              1432
         AG_PARLIST = 'DX_DY_DZ'                                          1432
         CALL AXSHAPE                                                     1432
C    *                                                                    1432
C    Create SMRD :                                                        1433
C    Call To Local Procedure SMRD                                         1433
         ASSIGN 6990 TO BLOCK_SMRD                                        1433
         GO TO  6980                                                      1433
6990     CONTINUE                                                         1433
C    *                                                                    1433
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1434
     *   SSUB_SROLLOD                                                     1434
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1435
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1435
         CALL AGSRESET                                                    1435
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1435
         AG_ALPHAN=AG_ALPHAN+1                                            1435
         AG_ALPHAK(AG_ALPHAN)=1                                           1435
         AG_ALPHAL(AG_ALPHAN)=90                                          1435
         AG_TITLE = 'SMRD'                                                1435
         AG_EXNAME = 'POSITION'                                           1435
         AG_PARLIST = 'Y_ALPHAX'                                          1435
         CALL AXPOSITION                                                  1435
C    *                                                                    1435
C    Create SBMO :                                                        1436
C    Call To Local Procedure SBMO                                         1436
         ASSIGN 7010 TO BLOCK_SBMO                                        1436
         GO TO  7000                                                      1436
7010     CONTINUE                                                         1436
C    *                                                                    1436
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1437
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1438
C    UB_SROLLOD)/2 <===                                                   1438
         CALL AGSRESET                                                    1438
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1438
         AG_TITLE = 'SBMO'                                                1438
         AG_EXNAME = 'POSITION'                                           1438
         AG_PARLIST = 'Y'                                                 1438
         CALL AXPOSITION                                                  1438
C    *                                                                    1438
C    Create SBMI :                                                        1439
C    Call To Local Procedure SBMI                                         1439
         ASSIGN 7030 TO BLOCK_SBMI                                        1439
         GO TO  7020                                                      1439
7030     CONTINUE                                                         1439
C    *                                                                    1439
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1440
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1441
C    UB_SROLLOD)/2 <===                                                   1441
         CALL AGSRESET                                                    1441
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1441
         AG_TITLE = 'SBMI'                                                1441
         AG_EXNAME = 'POSITION'                                           1441
         AG_PARLIST = 'Y'                                                 1441
         CALL AXPOSITION                                                  1441
C    *                                                                    1441
6811  CALL AGSPOPB                                                        1442
      GO TO BLOCK_SBMM, (6810 ,6820,6830)                                 1442
C End of Local Procedure                                                  1442
C ----------------------------------------------------------------------  1443
C ------------ SMRD                                                       1443
C Begin Local Procedure SMRD                                              1444
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1444
6980  CONTINUE                                                            1444
         AG_TITLE='SMRD'                                                  1444
         AG_MODULE=MODULE                                                 1444
         AG_MARK='V00'                                                    1444
         CALL AGSPUSH                                                     1444
         AG_MODULE=MODULE                                                 1444
C    *                                                                    1444
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1445
         CALL AGSRESET                                                    1445
         AG_TITLE = 'ALUMINIUM'                                           1445
         AG_EXNAME = 'MATERIAL'                                           1445
         AG_PARLIST = 'NONE'                                              1445
         CALL AXMATERIAL                                                  1445
C    *                                                                    1445
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1445
         CALL AGSRESET                                                    1446
         AG_SEEN = 1                                                      1446
         AG_COLO = 2                                                      1446
         AG_TITLE = 'SMRD'                                                1446
         AG_EXNAME = 'ATTRIBUTE'                                          1446
         AG_PARLIST = 'SEEN_COLO'                                         1446
         CALL AXATTRIBUTE                                                 1446
C    *                                                                    1446
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1446
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1446
         CALL AGSRESET                                                    1448
         AG_RMIN = 0.0                                                    1448
         AG_RMAX = SSUB_MRODDIA/2                                         1448
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1448
         AG_TITLE = 'TUBE'                                                1448
         AG_EXNAME = 'SHAPE'                                              1448
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1448
         CALL AXSHAPE                                                     1448
C    *                                                                    1448
6981  CALL AGSPOPB                                                        1449
      GO TO BLOCK_SMRD, (6980 ,6990)                                      1449
C End of Local Procedure                                                  1449
C ----------------------------------------------------------------------  1450
C ------------ SBMO                                                       1450
C Begin Local Procedure SBMO                                              1451
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1451
7000  CONTINUE                                                            1451
         AG_TITLE='SBMO'                                                  1451
         AG_MODULE=MODULE                                                 1451
         AG_MARK='V00'                                                    1451
         CALL AGSPUSH                                                     1451
         AG_MODULE=MODULE                                                 1451
C    *                                                                    1451
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1452
         CALL AGSRESET                                                    1452
         AG_TITLE = 'G10'                                                 1452
         AG_EXNAME = 'MATERIAL'                                           1452
         AG_PARLIST = 'NONE'                                              1452
         CALL AXMATERIAL                                                  1452
C    *                                                                    1452
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1452
         CALL AGSRESET                                                    1453
         AG_SEEN = 1                                                      1453
         AG_COLO = 3                                                      1453
         AG_TITLE = 'SBMO'                                                1453
         AG_EXNAME = 'ATTRIBUTE'                                          1453
         AG_PARLIST = 'SEEN_COLO'                                         1453
         CALL AXATTRIBUTE                                                 1453
C    *                                                                    1453
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1453
C    UB_MBLKHGH/2 <===                                                    1453
         CALL AGSRESET                                                    1455
         AG_DX = SSUB_MBLKOWID/2                                          1455
         AG_DY = SSUB_MBLKOLEN/2                                          1455
         AG_DZ = SSUB_MBLKHGH/2                                           1455
         AG_TITLE = 'BOX'                                                 1455
         AG_EXNAME = 'SHAPE'                                              1455
         AG_PARLIST = 'DX_DY_DZ'                                          1455
         CALL AXSHAPE                                                     1455
C    *                                                                    1455
7001  CALL AGSPOPB                                                        1456
      GO TO BLOCK_SBMO, (7000 ,7010)                                      1456
C End of Local Procedure                                                  1456
C ----------------------------------------------------------------------  1457
C ------------ SBMI                                                       1457
C Begin Local Procedure SBMI                                              1458
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1458
7020  CONTINUE                                                            1458
         AG_TITLE='SBMI'                                                  1458
         AG_MODULE=MODULE                                                 1458
         AG_MARK='V00'                                                    1458
         CALL AGSPUSH                                                     1458
         AG_MODULE=MODULE                                                 1458
C    *                                                                    1458
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1459
         CALL AGSRESET                                                    1459
         AG_TITLE = 'G10'                                                 1459
         AG_EXNAME = 'MATERIAL'                                           1459
         AG_PARLIST = 'NONE'                                              1459
         CALL AXMATERIAL                                                  1459
C    *                                                                    1459
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1459
         CALL AGSRESET                                                    1460
         AG_SEEN = 1                                                      1460
         AG_COLO = 3                                                      1460
         AG_TITLE = 'SBMO'                                                1460
         AG_EXNAME = 'ATTRIBUTE'                                          1460
         AG_PARLIST = 'SEEN_COLO'                                         1460
         CALL AXATTRIBUTE                                                 1460
C    *                                                                    1460
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1460
C    UB_MBLKHGH/2 <===                                                    1460
         CALL AGSRESET                                                    1462
         AG_DX = SSUB_MBLKIWID/2                                          1462
         AG_DY = SSUB_MBLKILEN/2                                          1462
         AG_DZ = SSUB_MBLKHGH/2                                           1462
         AG_TITLE = 'BOX'                                                 1462
         AG_EXNAME = 'SHAPE'                                              1462
         AG_PARLIST = 'DX_DY_DZ'                                          1462
         CALL AXSHAPE                                                     1462
C    *                                                                    1462
7021  CALL AGSPOPB                                                        1463
      GO TO BLOCK_SBMI, (7020 ,7030)                                      1463
C End of Local Procedure                                                  1463
C ----------------------------------------------------------------------  1464
C ------------ SALM                                                       1464
C Begin Local Procedure SALM                                              1465
C SALM IS THE ALUMINUM SHIELD MESH                                        1465
5380  CONTINUE                                                            1465
         AG_TITLE='SALM'                                                  1465
         AG_MODULE=MODULE                                                 1465
         AG_MARK='V00'                                                    1465
         CALL AGSPUSH                                                     1465
         AG_MODULE=MODULE                                                 1465
C    *                                                                    1465
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1466
         CALL AGSRESET                                                    1466
         AG_TITLE = 'ALUMINIUM'                                           1466
         AG_EXNAME = 'MATERIAL'                                           1466
         AG_PARLIST = 'NONE'                                              1466
         CALL AXMATERIAL                                                  1466
C    *                                                                    1466
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1466
         CALL AGSRESET                                                    1467
         AG_SEEN = 1                                                      1467
         AG_COLO = 2                                                      1467
         AG_TITLE = 'SALM'                                                1467
         AG_EXNAME = 'ATTRIBUTE'                                          1467
         AG_PARLIST = 'SEEN_COLO'                                         1467
         CALL AXATTRIBUTE                                                 1467
C    *                                                                    1467
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1467
C    DZ=SSLD_ALMSHTHK/2 <===                                              1467
         CALL AGSRESET                                                    1469
         AG_RMIN = SSLD_ALMESHID/2                                        1469
         AG_RMAX = SSLD_ALMESHOD/2                                        1469
         AG_DZ = SSLD_ALMSHTHK/2                                          1469
         AG_TITLE = 'TUBE'                                                1469
         AG_EXNAME = 'SHAPE'                                              1469
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1469
         CALL AXSHAPE                                                     1469
C    *                                                                    1469
5381  CALL AGSPOPB                                                        1470
      GO TO BLOCK_SALM, (5380 ,5390)                                      1470
C End of Local Procedure                                                  1470
C ----------------------------------------------------------------------  1471
C ------------ SISH                                                       1471
C Begin Local Procedure SISH                                              1472
C SISH IS THE INNER SHIELD CYLINDER                                       1472
5320  CONTINUE                                                            1472
         AG_TITLE='SISH'                                                  1472
         AG_MODULE=MODULE                                                 1472
         AG_MARK='V00'                                                    1472
         CALL AGSPUSH                                                     1472
         AG_MODULE=MODULE                                                 1472
C    *                                                                    1472
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1473
         CALL AGSRESET                                                    1473
         AG_TITLE = 'ALKAP'                                               1473
         AG_EXNAME = 'MATERIAL'                                           1473
         AG_PARLIST = 'NONE'                                              1473
         CALL AXMATERIAL                                                  1473
C    *                                                                    1473
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1473
         CALL AGSRESET                                                    1474
         AG_SEEN = 1                                                      1474
         AG_COLO = 3                                                      1474
         AG_TITLE = 'SISH'                                                1474
         AG_EXNAME = 'ATTRIBUTE'                                          1474
         AG_PARLIST = 'SEEN_COLO'                                         1474
         CALL AXATTRIBUTE                                                 1474
C    *                                                                    1474
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1474
C    D_SINLEN <===                                                        1474
         CALL AGSRESET                                                    1476
         AG_RMIN = SSLD_SINRINN                                           1476
         AG_RMAX = SSLD_SINROUT                                           1476
         AG_DZ = SSLD_SINLEN                                              1476
         AG_TITLE = 'TUBE'                                                1476
         AG_EXNAME = 'SHAPE'                                              1476
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1476
         CALL AXSHAPE                                                     1476
C    *                                                                    1476
5321  CALL AGSPOPB                                                        1477
      GO TO BLOCK_SISH, (5320 ,5330)                                      1477
C End of Local Procedure                                                  1477
C ----------------------------------------------------------------------  1478
C ------------ SSSH                                                       1478
C Begin Local Procedure SSSH                                              1479
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1479
5360  CONTINUE                                                            1479
         AG_TITLE='SSSH'                                                  1479
         AG_MODULE=MODULE                                                 1479
         AG_MARK='V00'                                                    1479
         CALL AGSPUSH                                                     1479
         AG_MODULE=MODULE                                                 1479
C    *                                                                    1479
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1480
         CALL AGSRESET                                                    1480
         AG_TITLE = 'SSDALMY'                                             1480
         AG_EXNAME = 'MATERIAL'                                           1480
         AG_PARLIST = 'NONE'                                              1480
         CALL AXMATERIAL                                                  1480
C    *                                                                    1480
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1480
         CALL AGSRESET                                                    1481
         AG_SEEN = 1                                                      1481
         AG_COLO = 3                                                      1481
         AG_TITLE = 'SSSH'                                                1481
         AG_EXNAME = 'ATTRIBUTE'                                          1481
         AG_PARLIST = 'SEEN_COLO'                                         1481
         CALL AXATTRIBUTE                                                 1481
C    *                                                                    1481
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1481
C    SLD_SSEPLEN <===                                                     1481
         CALL AGSRESET                                                    1483
         AG_RMIN = SSLD_SSEPRINN                                          1483
         AG_RMAX = SSLD_SSEPROUT                                          1483
         AG_DZ = SSLD_SSEPLEN                                             1483
         AG_TITLE = 'TUBE'                                                1483
         AG_EXNAME = 'SHAPE'                                              1483
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1483
         CALL AXSHAPE                                                     1483
C    *                                                                    1483
5361  CALL AGSPOPB                                                        1484
      GO TO BLOCK_SSSH, (5360 ,5370)                                      1484
C End of Local Procedure                                                  1484
C ----------------------------------------------------------------------  1485
C ------------ SOSH                                                       1485
C Begin Local Procedure SOSH                                              1486
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1486
5340  CONTINUE                                                            1486
         AG_TITLE='SOSH'                                                  1486
         AG_MODULE=MODULE                                                 1486
         AG_MARK='V00'                                                    1486
         CALL AGSPUSH                                                     1486
         AG_MODULE=MODULE                                                 1486
C    *                                                                    1486
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1487
         CALL AGSRESET                                                    1487
         AG_TITLE = 'SSDALMY'                                             1487
         AG_EXNAME = 'MATERIAL'                                           1487
         AG_PARLIST = 'NONE'                                              1487
         CALL AXMATERIAL                                                  1487
C    *                                                                    1487
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1487
         CALL AGSRESET                                                    1488
         AG_SEEN = 1                                                      1488
         AG_COLO = 3                                                      1488
         AG_TITLE = 'SOSH'                                                1488
         AG_EXNAME = 'ATTRIBUTE'                                          1488
         AG_PARLIST = 'SEEN_COLO'                                         1488
         CALL AXATTRIBUTE                                                 1488
C    *                                                                    1488
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1488
C    SLD_SOUTLEN <===                                                     1488
         CALL AGSRESET                                                    1490
         AG_RMIN = SSLD_SOUTRINN                                          1490
         AG_RMAX = SSLD_SOUTROUT                                          1490
         AG_DZ = SSLD_SOUTLEN                                             1490
         AG_TITLE = 'TUBE'                                                1490
         AG_EXNAME = 'SHAPE'                                              1490
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1490
         CALL AXSHAPE                                                     1490
C    *                                                                    1490
5341  CALL AGSPOPB                                                        1491
      GO TO BLOCK_SOSH, (5340 ,5350)                                      1491
C End of Local Procedure                                                  1491
      WRITE(LOUT,7040)'===>>  [End AgML Module SVTTGEO2]  <<==='          1491
7040  FORMAT(A32,/,/)                                                     1491
C MODULE SVTTGEO2                                                         1493
      END                                                                 1493
