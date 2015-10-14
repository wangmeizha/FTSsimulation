C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO                                                  1
C SVTTGEO IS THE SVT GEOMETRY FOR STAR                                    1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SVTTGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE SVT GEOMETRY FOR STAR'/                       1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR PAVEL NEVSKI, KEN WILSON                                         1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'PAVEL NEVSKI, KEN WILSON'/                    2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 05 APR 96                                                       2
      CHARACTER*40 CREATED /'05 APR 96'/                                  3
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
      INTEGER BLOCK_SFMO                                                  12
      INTEGER BLOCK_SFLM                                                  12
      INTEGER BLOCK_SFDM                                                  12
      INTEGER BLOCK_SFSW                                                  12
      INTEGER BLOCK_SFSD                                                  12
      INTEGER BLOCK_SFSM                                                  12
      INTEGER BLOCK_SFSS                                                  12
      INTEGER BLOCK_SFCP                                                  12
      INTEGER BLOCK_SFCF                                                  12
      INTEGER BLOCK_SFCT                                                  12
      INTEGER BLOCK_SFCX                                                  12
      INTEGER BLOCK_SFCW                                                  12
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
C -                                                                       13
      REAL SVTG_VERSION                                                   13
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            13
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       13
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          13
     *MSVTG_VERSION)                                                      13
      CHARACTER CSVTG_VERSION*40/' '/                                     13
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             13
      INTEGER MSVTG_VERSION(2)/1,1/                                       13
      REAL RE_SVTG_VERSION                                                13
C -                                                                       13
      INTEGER SVTG_NLAYER                                                 13
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              13
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        13
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           13
     *MSVTG_NLAYER)                                                       13
      CHARACTER CSVTG_NLAYER*40/' '/                                      13
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               13
      INTEGER MSVTG_NLAYER(2)/1,1/                                        13
      REAL RE_SVTG_NLAYER                                                 13
C -                                                                       13
      REAL SVTG_RSIZEMIN                                                  13
      EQUIVALENCE (BANK_SVTG(5),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          13
     *(NAMES_SVTG(3),NSVTG_RSIZEMIN)                                      13
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_RSIZEMIN),(MAP_SVTG(1,3),         13
     *MSVTG_RSIZEMIN)                                                     13
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    13
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           13
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      13
      REAL RE_SVTG_RSIZEMIN                                               13
C -                                                                       13
      REAL SVTG_RSIZEMAX                                                  13
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          13
     *(NAMES_SVTG(4),NSVTG_RSIZEMAX)                                      13
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMAX),(MAP_SVTG(1,4),         13
     *MSVTG_RSIZEMAX)                                                     13
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    13
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           13
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      13
      REAL RE_SVTG_RSIZEMAX                                               13
C -                                                                       13
      REAL SVTG_ZSIZEMAX                                                  13
      EQUIVALENCE (BANK_SVTG(7),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          13
     *(NAMES_SVTG(5),NSVTG_ZSIZEMAX)                                      13
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_ZSIZEMAX),(MAP_SVTG(1,5),         13
     *MSVTG_ZSIZEMAX)                                                     13
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    13
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           13
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      13
      REAL RE_SVTG_ZSIZEMAX                                               13
C -                                                                       13
      REAL SVTG_ANGOFF                                                    13
      EQUIVALENCE (BANK_SVTG(8),SVTG_ANGOFF,RE_SVTG_ANGOFF),              13
     *(NAMES_SVTG(6),NSVTG_ANGOFF)                                        13
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ANGOFF),(MAP_SVTG(1,6),           13
     *MSVTG_ANGOFF)                                                       13
      CHARACTER CSVTG_ANGOFF*40/' '/                                      13
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               13
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        13
      REAL RE_SVTG_ANGOFF                                                 13
C -                                                                       13
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                13
      PARAMETER (LEN_SVTG=8)                                              13
      PARAMETER (LLEN_SVTG=6)                                             13
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           13
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        13
      DATA IDNUM_SVTG/10*0/                                               13
      REAL BANK_SVTG(LEN_SVTG)                                            13
      CHARACTER*60 BPATH_SVTG/' '/                                        13
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          13
     *BPATH_SVTG                                                          13
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             13
C *                                                                       13
C -                                                                       15
      REAL SWCA_VERSION                                                   15
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            15
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       15
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          15
     *MSWCA_VERSION)                                                      15
      CHARACTER CSWCA_VERSION*40/' '/                                     15
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             15
      INTEGER MSWCA_VERSION(2)/1,1/                                       15
      REAL RE_SWCA_VERSION                                                15
C -                                                                       15
      REAL SWCA_LENGTH                                                    15
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              15
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        15
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           15
     *MSWCA_LENGTH)                                                       15
      CHARACTER CSWCA_LENGTH*40/' '/                                      15
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               15
      INTEGER MSWCA_LENGTH(2)/1,1/                                        15
      REAL RE_SWCA_LENGTH                                                 15
C -                                                                       15
      REAL SWCA_WAFERWID                                                  15
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          15
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      15
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         15
     *MSWCA_WAFERWID)                                                     15
      CHARACTER CSWCA_WAFERWID*40/' '/                                    15
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           15
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      15
      REAL RE_SWCA_WAFERWID                                               15
C -                                                                       15
      REAL SWCA_WAFERLEN                                                  15
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          15
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      15
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         15
     *MSWCA_WAFERLEN)                                                     15
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    15
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           15
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      15
      REAL RE_SWCA_WAFERLEN                                               15
C -                                                                       15
      REAL SWCA_WAFERTHK                                                  15
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          15
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      15
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         15
     *MSWCA_WAFERTHK)                                                     15
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    15
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           15
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      15
      REAL RE_SWCA_WAFERTHK                                               15
C -                                                                       15
      REAL SWCA_ROHATHK                                                   15
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            15
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       15
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          15
     *MSWCA_ROHATHK)                                                      15
      CHARACTER CSWCA_ROHATHK*40/' '/                                     15
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             15
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       15
      REAL RE_SWCA_ROHATHK                                                15
C -                                                                       15
      REAL SWCA_WAFCARWD                                                  15
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          15
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      15
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         15
     *MSWCA_WAFCARWD)                                                     15
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    15
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           15
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      15
      REAL RE_SWCA_WAFCARWD                                               15
C -                                                                       15
      REAL SWCA_WAFCARTH                                                  15
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         15
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      15
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         15
     *MSWCA_WAFCARTH)                                                     15
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    15
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           15
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      15
      REAL RE_SWCA_WAFCARTH                                               15
C -                                                                       15
      REAL SWCA_WAFERGAP                                                  15
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         15
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      15
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         15
     *MSWCA_WAFERGAP)                                                     15
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    15
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           15
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      15
      REAL RE_SWCA_WAFERGAP                                               15
C -                                                                       15
      REAL SWCA_DRIFT                                                     15
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               15
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        15
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          15
     *MSWCA_DRIFT)                                                        15
      CHARACTER CSWCA_DRIFT*40/' '/                                       15
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 15
      INTEGER MSWCA_DRIFT(2)/1,1/                                         15
      REAL RE_SWCA_DRIFT                                                  15
C -                                                                       15
      REAL SWCA_STRUTLEN                                                  15
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         15
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     15
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       15
     *MSWCA_STRUTLEN)                                                     15
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    15
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           15
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      15
      REAL RE_SWCA_STRUTLEN                                               15
C -                                                                       15
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                15
      PARAMETER (LEN_SWCA=13)                                             15
      PARAMETER (LLEN_SWCA=11)                                            15
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           15
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        15
      DATA IDNUM_SWCA/10*0/                                               15
      REAL BANK_SWCA(LEN_SWCA)                                            15
      CHARACTER*60 BPATH_SWCA/' '/                                        15
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          15
     *BPATH_SWCA                                                          15
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             15
C *                                                                       15
C -                                                                       18
      REAL SSUP_VERSION                                                   18
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            18
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       18
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          18
     *MSSUP_VERSION)                                                      18
      CHARACTER CSSUP_VERSION*40/' '/                                     18
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             18
      INTEGER MSSUP_VERSION(2)/1,1/                                       18
      REAL RE_SSUP_VERSION                                                18
C -                                                                       18
      REAL SSUP_CABTHK                                                    18
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              18
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        18
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           18
     *MSSUP_CABTHK)                                                       18
      CHARACTER CSSUP_CABTHK*40/' '/                                      18
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               18
      INTEGER MSSUP_CABTHK(2)/1,1/                                        18
      REAL RE_SSUP_CABTHK                                                 18
C -                                                                       18
      REAL SSUP_HOSRMN                                                    18
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              18
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        18
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           18
     *MSSUP_HOSRMN)                                                       18
      CHARACTER CSSUP_HOSRMN*40/' '/                                      18
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               18
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        18
      REAL RE_SSUP_HOSRMN                                                 18
C -                                                                       18
      REAL SSUP_HOSRMX                                                    18
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              18
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        18
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           18
     *MSSUP_HOSRMX)                                                       18
      CHARACTER CSSUP_HOSRMX*40/' '/                                      18
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               18
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        18
      REAL RE_SSUP_HOSRMX                                                 18
C -                                                                       18
      REAL SSUP_NHOSES                                                    18
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              18
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        18
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           18
     *MSSUP_NHOSES)                                                       18
      CHARACTER CSSUP_NHOSES*40/' '/                                      18
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               18
      INTEGER MSSUP_NHOSES(2)/1,1/                                        18
      REAL RE_SSUP_NHOSES                                                 18
C -                                                                       18
      REAL SSUP_WRPMYTHK                                                  18
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          18
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      18
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         18
     *MSSUP_WRPMYTHK)                                                     18
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    18
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           18
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      18
      REAL RE_SSUP_WRPMYTHK                                               18
C -                                                                       18
      REAL SSUP_WRPALTHK                                                  18
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          18
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      18
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         18
     *MSSUP_WRPALTHK)                                                     18
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    18
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           18
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      18
      REAL RE_SSUP_WRPALTHK                                               18
C -                                                                       18
      REAL SSUP_GRPHTHK                                                   18
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           18
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       18
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          18
     *MSSUP_GRPHTHK)                                                      18
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     18
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             18
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       18
      REAL RE_SSUP_GRPHTHK                                                18
C -                                                                       18
      REAL SSUP_CONE1ZMN                                                  18
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         18
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      18
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         18
     *MSSUP_CONE1ZMN)                                                     18
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    18
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           18
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      18
      REAL RE_SSUP_CONE1ZMN                                               18
C -                                                                       18
      REAL SSUP_RODLEN                                                    18
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             18
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       18
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         18
     *MSSUP_RODLEN)                                                       18
      CHARACTER CSSUP_RODLEN*40/' '/                                      18
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               18
      INTEGER MSSUP_RODLEN(2)/1,1/                                        18
      REAL RE_SSUP_RODLEN                                                 18
C -                                                                       18
      REAL SSUP_RODDIST                                                   18
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           18
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      18
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        18
     *MSSUP_RODDIST)                                                      18
      CHARACTER CSSUP_RODDIST*40/' '/                                     18
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             18
      INTEGER MSSUP_RODDIST(2)/1,1/                                       18
      REAL RE_SSUP_RODDIST                                                18
C -                                                                       18
      REAL SSUP_RODID                                                     18
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               18
     *(NAMES_SSUP(12),NSSUP_RODID)                                        18
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          18
     *MSSUP_RODID)                                                        18
      CHARACTER CSSUP_RODID*40/' '/                                       18
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 18
      INTEGER MSSUP_RODID(2)/1,1/                                         18
      REAL RE_SSUP_RODID                                                  18
C -                                                                       18
      REAL SSUP_RODOD                                                     18
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               18
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        18
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          18
     *MSSUP_RODOD)                                                        18
      CHARACTER CSSUP_RODOD*40/' '/                                       18
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 18
      INTEGER MSSUP_RODOD(2)/1,1/                                         18
      REAL RE_SSUP_RODOD                                                  18
C -                                                                       18
      REAL SSUP_CON1IDMN                                                  18
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         18
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     18
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       18
     *MSSUP_CON1IDMN)                                                     18
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    18
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           18
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      18
      REAL RE_SSUP_CON1IDMN                                               18
C -                                                                       18
      REAL SSUP_CON3IDMN                                                  18
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         18
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     18
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       18
     *MSSUP_CON3IDMN)                                                     18
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    18
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           18
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      18
      REAL RE_SSUP_CON3IDMN                                               18
C -                                                                       18
      REAL SSUP_CON4IDMN                                                  18
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         18
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     18
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       18
     *MSSUP_CON4IDMN)                                                     18
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    18
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           18
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      18
      REAL RE_SSUP_CON4IDMN                                               18
C -                                                                       18
      REAL SSUP_CON4IDMX                                                  18
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         18
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     18
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       18
     *MSSUP_CON4IDMX)                                                     18
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    18
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           18
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      18
      REAL RE_SSUP_CON4IDMX                                               18
C -                                                                       18
      REAL SSUP_CONE3ZMX                                                  18
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         18
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     18
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       18
     *MSSUP_CONE3ZMX)                                                     18
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    18
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           18
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      18
      REAL RE_SSUP_CONE3ZMX                                               18
C -                                                                       18
      REAL SSUP_CONE4ZMX                                                  18
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         18
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     18
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       18
     *MSSUP_CONE4ZMX)                                                     18
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    18
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           18
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      18
      REAL RE_SSUP_CONE4ZMX                                               18
C -                                                                       18
      REAL SSUP_BRATHK                                                    18
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             18
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       18
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         18
     *MSSUP_BRATHK)                                                       18
      CHARACTER CSSUP_BRATHK*40/' '/                                      18
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               18
      INTEGER MSSUP_BRATHK(2)/1,1/                                        18
      REAL RE_SSUP_BRATHK                                                 18
C -                                                                       18
      REAL SSUP_ERJTHK                                                    18
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             18
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       18
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         18
     *MSSUP_ERJTHK)                                                       18
      CHARACTER CSSUP_ERJTHK*40/' '/                                      18
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               18
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        18
      REAL RE_SSUP_ERJTHK                                                 18
C -                                                                       18
      REAL SSUP_ERJWID                                                    18
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             18
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       18
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         18
     *MSSUP_ERJWID)                                                       18
      CHARACTER CSSUP_ERJWID*40/' '/                                      18
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               18
      INTEGER MSSUP_ERJWID(2)/1,1/                                        18
      REAL RE_SSUP_ERJWID                                                 18
C -                                                                       18
      REAL SSUP_ERJLEN                                                    18
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             18
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       18
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         18
     *MSSUP_ERJLEN)                                                       18
      CHARACTER CSSUP_ERJLEN*40/' '/                                      18
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               18
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        18
      REAL RE_SSUP_ERJLEN                                                 18
C -                                                                       18
      REAL SSUP_ERJZDIS                                                   18
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           18
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      18
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        18
     *MSSUP_ERJZDIS)                                                      18
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     18
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             18
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       18
      REAL RE_SSUP_ERJZDIS                                                18
C -                                                                       18
      REAL SSUP_ERJ1X                                                     18
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               18
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        18
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          18
     *MSSUP_ERJ1X)                                                        18
      CHARACTER CSSUP_ERJ1X*40/' '/                                       18
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 18
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         18
      REAL RE_SSUP_ERJ1X                                                  18
C -                                                                       18
      REAL SSUP_ERJ2X                                                     18
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               18
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        18
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          18
     *MSSUP_ERJ2X)                                                        18
      CHARACTER CSSUP_ERJ2X*40/' '/                                       18
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 18
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         18
      REAL RE_SSUP_ERJ2X                                                  18
C -                                                                       18
      REAL SSUP_ERJ2Y                                                     18
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               18
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        18
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          18
     *MSSUP_ERJ2Y)                                                        18
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       18
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 18
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         18
      REAL RE_SSUP_ERJ2Y                                                  18
C -                                                                       18
      REAL SSUP_ERJRAD                                                    18
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             18
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       18
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         18
     *MSSUP_ERJRAD)                                                       18
      CHARACTER CSSUP_ERJRAD*40/' '/                                      18
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               18
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        18
      REAL RE_SSUP_ERJRAD                                                 18
C -                                                                       18
      REAL SSUP_ERJDIA                                                    18
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             18
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       18
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         18
     *MSSUP_ERJDIA)                                                       18
      CHARACTER CSSUP_ERJDIA*40/' '/                                      18
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               18
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        18
      REAL RE_SSUP_ERJDIA                                                 18
C -                                                                       18
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                18
      PARAMETER (LEN_SSUP=31)                                             18
      PARAMETER (LLEN_SSUP=29)                                            18
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           18
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        18
      DATA IDNUM_SSUP/10*0/                                               18
      REAL BANK_SSUP(LEN_SSUP)                                            18
      CHARACTER*60 BPATH_SSUP/' '/                                        18
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          18
     *BPATH_SSUP                                                          18
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             18
C *                                                                       18
C -                                                                       22
      REAL SSUB_VERSION                                                   22
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            22
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       22
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          22
     *MSSUB_VERSION)                                                      22
      CHARACTER CSSUB_VERSION*40/' '/                                     22
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             22
      INTEGER MSSUB_VERSION(2)/1,1/                                       22
      REAL RE_SSUB_VERSION                                                22
C -                                                                       22
      REAL SSUB_KMOUNTID                                                  22
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          22
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      22
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         22
     *MSSUB_KMOUNTID)                                                     22
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    22
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           22
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      22
      REAL RE_SSUB_KMOUNTID                                               22
C -                                                                       22
      REAL SSUB_KMOUNTOD                                                  22
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          22
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      22
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         22
     *MSSUB_KMOUNTOD)                                                     22
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    22
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           22
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      22
      REAL RE_SSUB_KMOUNTOD                                               22
C -                                                                       22
      REAL SSUB_KMNTTHK                                                   22
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            22
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       22
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          22
     *MSSUB_KMNTTHK)                                                      22
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     22
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             22
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       22
      REAL RE_SSUB_KMNTTHK                                                22
C -                                                                       22
      REAL SSUB_KMCUTOD                                                   22
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            22
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       22
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          22
     *MSSUB_KMCUTOD)                                                      22
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     22
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             22
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       22
      REAL RE_SSUB_KMCUTOD                                                22
C -                                                                       22
      REAL SSUB_KMCUTID                                                   22
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            22
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       22
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          22
     *MSSUB_KMCUTID)                                                      22
      CHARACTER CSSUB_KMCUTID*40/' '/                                     22
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             22
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       22
      REAL RE_SSUB_KMCUTID                                                22
C -                                                                       22
      REAL SSUB_KMCUTOA                                                   22
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            22
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       22
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          22
     *MSSUB_KMCUTOA)                                                      22
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     22
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             22
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       22
      REAL RE_SSUB_KMCUTOA                                                22
C -                                                                       22
      REAL SSUB_KMCUTOFF                                                  22
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         22
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      22
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         22
     *MSSUB_KMCUTOFF)                                                     22
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    22
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           22
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      22
      REAL RE_SSUB_KMCUTOFF                                               22
C -                                                                       22
      REAL SSUB_SRINGID                                                   22
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           22
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       22
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          22
     *MSSUB_SRINGID)                                                      22
      CHARACTER CSSUB_SRINGID*40/' '/                                     22
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             22
      INTEGER MSSUB_SRINGID(2)/1,1/                                       22
      REAL RE_SSUB_SRINGID                                                22
C -                                                                       22
      REAL SSUB_SRINGOD                                                   22
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           22
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      22
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        22
     *MSSUB_SRINGOD)                                                      22
      CHARACTER CSSUB_SRINGOD*40/' '/                                     22
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             22
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       22
      REAL RE_SSUB_SRINGOD                                                22
C -                                                                       22
      REAL SSUB_SRINGTHK                                                  22
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         22
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     22
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       22
     *MSSUB_SRINGTHK)                                                     22
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    22
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           22
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      22
      REAL RE_SSUB_SRINGTHK                                               22
C -                                                                       22
      REAL SSUB_SRCUTPHI                                                  22
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         22
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     22
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       22
     *MSSUB_SRCUTPHI)                                                     22
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    22
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           22
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      22
      REAL RE_SSUB_SRCUTPHI                                               22
C -                                                                       22
      REAL SSUB_SRCUTWID                                                  22
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         22
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     22
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       22
     *MSSUB_SRCUTWID)                                                     22
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    22
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           22
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      22
      REAL RE_SSUB_SRCUTWID                                               22
C -                                                                       22
      REAL SSUB_SRCUTOUT                                                  22
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         22
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     22
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       22
     *MSSUB_SRCUTOUT)                                                     22
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    22
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           22
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      22
      REAL RE_SSUB_SRCUTOUT                                               22
C -                                                                       22
      REAL SSUB_SRCUTIN                                                   22
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           22
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      22
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        22
     *MSSUB_SRCUTIN)                                                      22
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     22
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             22
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       22
      REAL RE_SSUB_SRCUTIN                                                22
C -                                                                       22
      REAL SSUB_SROLLID                                                   22
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           22
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      22
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        22
     *MSSUB_SROLLID)                                                      22
      CHARACTER CSSUB_SROLLID*40/' '/                                     22
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             22
      INTEGER MSSUB_SROLLID(2)/1,1/                                       22
      REAL RE_SSUB_SROLLID                                                22
C -                                                                       22
      REAL SSUB_SROLLOD                                                   22
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           22
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      22
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        22
     *MSSUB_SROLLOD)                                                      22
      CHARACTER CSSUB_SROLLOD*40/' '/                                     22
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             22
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       22
      REAL RE_SSUB_SROLLOD                                                22
C -                                                                       22
      REAL SSUB_SROLLLEN                                                  22
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         22
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     22
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       22
     *MSSUB_SROLLLEN)                                                     22
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    22
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           22
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      22
      REAL RE_SSUB_SROLLLEN                                               22
C -                                                                       22
      REAL SSUB_SWIRELEN                                                  22
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         22
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     22
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       22
     *MSSUB_SWIRELEN)                                                     22
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    22
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           22
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      22
      REAL RE_SSUB_SWIRELEN                                               22
C -                                                                       22
      REAL SSUB_MBLKHGH                                                   22
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           22
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      22
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        22
     *MSSUB_MBLKHGH)                                                      22
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     22
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             22
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       22
      REAL RE_SSUB_MBLKHGH                                                22
C -                                                                       22
      REAL SSUB_MBLKOWID                                                  22
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         22
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     22
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       22
     *MSSUB_MBLKOWID)                                                     22
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    22
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           22
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      22
      REAL RE_SSUB_MBLKOWID                                               22
C -                                                                       22
      REAL SSUB_MBLKOLEN                                                  22
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         22
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     22
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       22
     *MSSUB_MBLKOLEN)                                                     22
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    22
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           22
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      22
      REAL RE_SSUB_MBLKOLEN                                               22
C -                                                                       22
      REAL SSUB_MBLKIWID                                                  22
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         22
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     22
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       22
     *MSSUB_MBLKIWID)                                                     22
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    22
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           22
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      22
      REAL RE_SSUB_MBLKIWID                                               22
C -                                                                       22
      REAL SSUB_MBLKILEN                                                  22
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         22
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     22
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       22
     *MSSUB_MBLKILEN)                                                     22
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    22
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           22
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      22
      REAL RE_SSUB_MBLKILEN                                               22
C -                                                                       22
      REAL SSUB_MBLKORAD                                                  22
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         22
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     22
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       22
     *MSSUB_MBLKORAD)                                                     22
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    22
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           22
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      22
      REAL RE_SSUB_MBLKORAD                                               22
C -                                                                       22
      REAL SSUB_MBLKIRAD                                                  22
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         22
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     22
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       22
     *MSSUB_MBLKIRAD)                                                     22
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    22
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           22
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      22
      REAL RE_SSUB_MBLKIRAD                                               22
C -                                                                       22
      REAL SSUB_MRODDIA                                                   22
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           22
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      22
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        22
     *MSSUB_MRODDIA)                                                      22
      CHARACTER CSSUB_MRODDIA*40/' '/                                     22
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             22
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       22
      REAL RE_SSUB_MRODDIA                                                22
C -                                                                       22
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                22
      PARAMETER (LEN_SSUB=29)                                             22
      PARAMETER (LLEN_SSUB=27)                                            22
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           22
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        22
      DATA IDNUM_SSUB/10*0/                                               22
      REAL BANK_SSUB(LEN_SSUB)                                            22
      CHARACTER*60 BPATH_SSUB/' '/                                        22
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          22
     *BPATH_SSUB                                                          22
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             22
C *                                                                       22
C -                                                                       23
      REAL SWAM_VERSION                                                   23
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            23
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       23
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          23
     *MSWAM_VERSION)                                                      23
      CHARACTER CSWAM_VERSION*40/' '/                                     23
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             23
      INTEGER MSWAM_VERSION(2)/1,1/                                       23
      REAL RE_SWAM_VERSION                                                23
C -                                                                       23
      REAL SWAM_ZMIN                                                      23
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   23
     *NSWAM_ZMIN)                                                         23
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             23
     *MSWAM_ZMIN)                                                         23
      CHARACTER CSWAM_ZMIN*40/' '/                                        23
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   23
      INTEGER MSWAM_ZMIN(2)/1,1/                                          23
      REAL RE_SWAM_ZMIN                                                   23
C -                                                                       23
      REAL SWAM_LEN                                                       23
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     23
     *NSWAM_LEN)                                                          23
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    23
      CHARACTER CSWAM_LEN*40/' '/                                         23
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     23
      INTEGER MSWAM_LEN(2)/1,1/                                           23
      REAL RE_SWAM_LEN                                                    23
C -                                                                       23
      REAL SWAM_RMIN                                                      23
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   23
     *NSWAM_RMIN)                                                         23
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             23
     *MSWAM_RMIN)                                                         23
      CHARACTER CSWAM_RMIN*40/' '/                                        23
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   23
      INTEGER MSWAM_RMIN(2)/1,1/                                          23
      REAL RE_SWAM_RMIN                                                   23
C -                                                                       23
      REAL SWAM_RMAX                                                      23
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   23
     *NSWAM_RMAX)                                                         23
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             23
     *MSWAM_RMAX)                                                         23
      CHARACTER CSWAM_RMAX*40/' '/                                        23
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   23
      INTEGER MSWAM_RMAX(2)/1,1/                                          23
      REAL RE_SWAM_RMAX                                                   23
C -                                                                       23
      REAL SWAM_TBRDTHK                                                   23
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            23
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       23
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          23
     *MSWAM_TBRDTHK)                                                      23
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     23
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             23
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       23
      REAL RE_SWAM_TBRDTHK                                                23
C -                                                                       23
      REAL SWAM_WALLTHK                                                   23
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            23
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       23
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          23
     *MSWAM_WALLTHK)                                                      23
      CHARACTER CSWAM_WALLTHK*40/' '/                                     23
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             23
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       23
      REAL RE_SWAM_WALLTHK                                                23
C -                                                                       23
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                23
      PARAMETER (LEN_SWAM=9)                                              23
      PARAMETER (LLEN_SWAM=7)                                             23
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           23
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        23
      DATA IDNUM_SWAM/10*0/                                               23
      REAL BANK_SWAM(LEN_SWAM)                                            23
      CHARACTER*60 BPATH_SWAM/' '/                                        23
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          23
     *BPATH_SWAM                                                          23
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             23
C *                                                                       23
C -                                                                       24
      REAL SERG_VERSION                                                   24
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            24
     *(NAMES_SERG(1),NSERG_VERSION)                                       24
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          24
     *MSERG_VERSION)                                                      24
      CHARACTER CSERG_VERSION*40/' '/                                     24
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             24
      INTEGER MSERG_VERSION(2)/1,1/                                       24
      REAL RE_SERG_VERSION                                                24
C -                                                                       24
      REAL SERG_IRNGTRMX                                                  24
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          24
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      24
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         24
     *MSERG_IRNGTRMX)                                                     24
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    24
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           24
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      24
      REAL RE_SERG_IRNGTRMX                                               24
C -                                                                       24
      REAL SERG_IRNGPRMN                                                  24
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          24
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      24
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         24
     *MSERG_IRNGPRMN)                                                     24
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    24
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           24
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      24
      REAL RE_SERG_IRNGPRMN                                               24
C -                                                                       24
      REAL SERG_ORNGRMIN                                                  24
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          24
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      24
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         24
     *MSERG_ORNGRMIN)                                                     24
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    24
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           24
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      24
      REAL RE_SERG_ORNGRMIN                                               24
C -                                                                       24
      REAL SERG_ORNGRMAX                                                  24
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          24
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      24
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         24
     *MSERG_ORNGRMAX)                                                     24
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    24
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           24
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      24
      REAL RE_SERG_ORNGRMAX                                               24
C -                                                                       24
      REAL SERG_ENDRNGTH                                                  24
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          24
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      24
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         24
     *MSERG_ENDRNGTH)                                                     24
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    24
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           24
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      24
      REAL RE_SERG_ENDRNGTH                                               24
C -                                                                       24
      REAL SERG_ENDRNGZM                                                  24
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          24
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      24
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         24
     *MSERG_ENDRNGZM)                                                     24
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    24
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           24
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      24
      REAL RE_SERG_ENDRNGZM                                               24
C -                                                                       24
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                24
      PARAMETER (LEN_SERG=9)                                              24
      PARAMETER (LLEN_SERG=7)                                             24
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           24
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        24
      DATA IDNUM_SERG/10*0/                                               24
      REAL BANK_SERG(LEN_SERG)                                            24
      CHARACTER*60 BPATH_SERG/' '/                                        24
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          24
     *BPATH_SERG                                                          24
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             24
C *                                                                       24
C -                                                                       26
      REAL SELC_VERSION                                                   26
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            26
     *(NAMES_SELC(1),NSELC_VERSION)                                       26
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          26
     *MSELC_VERSION)                                                      26
      CHARACTER CSELC_VERSION*40/' '/                                     26
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             26
      INTEGER MSELC_VERSION(2)/1,1/                                       26
      REAL RE_SELC_VERSION                                                26
C -                                                                       26
      REAL SELC_BETHK                                                     26
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                26
     *(NAMES_SELC(2),NSELC_BETHK)                                         26
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            26
     *MSELC_BETHK)                                                        26
      CHARACTER CSELC_BETHK*40/' '/                                       26
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 26
      INTEGER MSELC_BETHK(2)/1,1/                                         26
      REAL RE_SELC_BETHK                                                  26
C -                                                                       26
      REAL SELC_WATTHK                                                    26
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              26
     *(NAMES_SELC(3),NSELC_WATTHK)                                        26
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           26
     *MSELC_WATTHK)                                                       26
      CHARACTER CSELC_WATTHK*40/' '/                                      26
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               26
      INTEGER MSELC_WATTHK(2)/1,1/                                        26
      REAL RE_SELC_WATTHK                                                 26
C -                                                                       26
      REAL SELC_BEOTHK                                                    26
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              26
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        26
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           26
     *MSELC_BEOTHK)                                                       26
      CHARACTER CSELC_BEOTHK*40/' '/                                      26
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               26
      INTEGER MSELC_BEOTHK(2)/1,1/                                        26
      REAL RE_SELC_BEOTHK                                                 26
C -                                                                       26
      REAL SELC_DYETHK                                                    26
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              26
     *(NAMES_SELC(5),NSELC_DYETHK)                                        26
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           26
     *MSELC_DYETHK)                                                       26
      CHARACTER CSELC_DYETHK*40/' '/                                      26
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               26
      INTEGER MSELC_DYETHK(2)/1,1/                                        26
      REAL RE_SELC_DYETHK                                                 26
C -                                                                       26
      REAL SELC_DYEWID                                                    26
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              26
     *(NAMES_SELC(6),NSELC_DYEWID)                                        26
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           26
     *MSELC_DYEWID)                                                       26
      CHARACTER CSELC_DYEWID*40/' '/                                      26
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               26
      INTEGER MSELC_DYEWID(2)/1,1/                                        26
      REAL RE_SELC_DYEWID                                                 26
C -                                                                       26
      REAL SELC_DYESPC                                                    26
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              26
     *(NAMES_SELC(7),NSELC_DYESPC)                                        26
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           26
     *MSELC_DYESPC)                                                       26
      CHARACTER CSELC_DYESPC*40/' '/                                      26
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               26
      INTEGER MSELC_DYESPC(2)/1,1/                                        26
      REAL RE_SELC_DYESPC                                                 26
C -                                                                       26
      REAL SELC_ELCAWID                                                   26
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           26
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       26
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          26
     *MSELC_ELCAWID)                                                      26
      CHARACTER CSELC_ELCAWID*40/' '/                                     26
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             26
      INTEGER MSELC_ELCAWID(2)/1,1/                                       26
      REAL RE_SELC_ELCAWID                                                26
C -                                                                       26
      REAL SELC_AGPDTHK                                                   26
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           26
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       26
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          26
     *MSELC_AGPDTHK)                                                      26
      CHARACTER CSELC_AGPDTHK*40/' '/                                     26
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             26
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       26
      REAL RE_SELC_AGPDTHK                                                26
C -                                                                       26
      REAL SELC_GLASSTHK                                                  26
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         26
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     26
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       26
     *MSELC_GLASSTHK)                                                     26
      CHARACTER CSELC_GLASSTHK*40/' '/                                    26
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           26
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      26
      REAL RE_SELC_GLASSTHK                                               26
C -                                                                       26
      REAL SELC_CABTHK                                                    26
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             26
     *(NAMES_SELC(11),NSELC_CABTHK)                                       26
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         26
     *MSELC_CABTHK)                                                       26
      CHARACTER CSELC_CABTHK*40/' '/                                      26
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               26
      INTEGER MSELC_CABTHK(2)/1,1/                                        26
      REAL RE_SELC_CABTHK                                                 26
C -                                                                       26
      REAL SELC_CABWID                                                    26
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             26
     *(NAMES_SELC(12),NSELC_CABWID)                                       26
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         26
     *MSELC_CABWID)                                                       26
      CHARACTER CSELC_CABWID*40/' '/                                      26
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               26
      INTEGER MSELC_CABWID(2)/1,1/                                        26
      REAL RE_SELC_CABWID                                                 26
C -                                                                       26
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                26
      PARAMETER (LEN_SELC=14)                                             26
      PARAMETER (LLEN_SELC=12)                                            26
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           26
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        26
      DATA IDNUM_SELC/10*0/                                               26
      REAL BANK_SELC(LEN_SELC)                                            26
      CHARACTER*60 BPATH_SELC/' '/                                        26
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          26
     *BPATH_SELC                                                          26
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             26
C *                                                                       26
C -                                                                       27
      INTEGER SVTL_LAYER                                                  27
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                27
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         27
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            27
     *MSVTL_LAYER)                                                        27
      CHARACTER CSVTL_LAYER*40/' '/                                       27
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 27
      INTEGER MSVTL_LAYER(2)/1,1/                                         27
      REAL RE_SVTL_LAYER                                                  27
C -                                                                       27
      REAL SVTL_NLADDER                                                   27
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            27
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       27
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          27
     *MSVTL_NLADDER)                                                      27
      CHARACTER CSVTL_NLADDER*40/' '/                                     27
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             27
      INTEGER MSVTL_NLADDER(2)/1,1/                                       27
      REAL RE_SVTL_NLADDER                                                27
C -                                                                       27
      REAL SVTL_NWAFER                                                    27
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              27
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        27
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           27
     *MSVTL_NWAFER)                                                       27
      CHARACTER CSVTL_NWAFER*40/' '/                                      27
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               27
      INTEGER MSVTL_NWAFER(2)/1,1/                                        27
      REAL RE_SVTL_NWAFER                                                 27
C -                                                                       27
      REAL SVTL_RADIUS                                                    27
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              27
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        27
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           27
     *MSVTL_RADIUS)                                                       27
      CHARACTER CSVTL_RADIUS*40/' '/                                      27
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               27
      INTEGER MSVTL_RADIUS(2)/1,1/                                        27
      REAL RE_SVTL_RADIUS                                                 27
C -                                                                       27
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                27
      PARAMETER (LEN_SVTL=6)                                              27
      PARAMETER (LLEN_SVTL=4)                                             27
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           27
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        27
      DATA IDNUM_SVTL/10*0/                                               27
      REAL BANK_SVTL(LEN_SVTL)                                            27
      CHARACTER*60 BPATH_SVTL/' '/                                        27
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          27
     *BPATH_SVTL                                                          27
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             27
C *                                                                       27
C -                                                                       29
      REAL SFPA_VERSION                                                   29
      EQUIVALENCE (BANK_SFPA(3),SFPA_VERSION,RE_SFPA_VERSION),            29
     *(NAMES_SFPA(1),NSFPA_VERSION)                                       29
      EQUIVALENCE (COMMEN_SFPA(1),CSFPA_VERSION),(MAP_SFPA(1,1),          29
     *MSFPA_VERSION)                                                      29
      CHARACTER CSFPA_VERSION*40/' '/                                     29
      CHARACTER NSFPA_VERSION*12/'R.VERSION'/                             29
      INTEGER MSFPA_VERSION(2)/1,1/                                       29
      REAL RE_SFPA_VERSION                                                29
C -                                                                       29
      REAL SFPA_RMIN                                                      29
      EQUIVALENCE (BANK_SFPA(4),SFPA_RMIN,RE_SFPA_RMIN),(NAMES_SFPA(2),   29
     *NSFPA_RMIN)                                                         29
      EQUIVALENCE (COMMEN_SFPA(2),CSFPA_RMIN),(MAP_SFPA(1,2),             29
     *MSFPA_RMIN)                                                         29
      CHARACTER CSFPA_RMIN*40/' '/                                        29
      CHARACTER NSFPA_RMIN*12/'R.RMIN'/                                   29
      INTEGER MSFPA_RMIN(2)/1,1/                                          29
      REAL RE_SFPA_RMIN                                                   29
C -                                                                       29
      REAL SFPA_RMAX                                                      29
      EQUIVALENCE (BANK_SFPA(5),SFPA_RMAX,RE_SFPA_RMAX),(NAMES_SFPA(3),   29
     *NSFPA_RMAX)                                                         29
      EQUIVALENCE (COMMEN_SFPA(3),CSFPA_RMAX),(MAP_SFPA(1,3),             29
     *MSFPA_RMAX)                                                         29
      CHARACTER CSFPA_RMAX*40/' '/                                        29
      CHARACTER NSFPA_RMAX*12/'R.RMAX'/                                   29
      INTEGER MSFPA_RMAX(2)/1,1/                                          29
      REAL RE_SFPA_RMAX                                                   29
C -                                                                       29
      REAL SFPA_LEN                                                       29
      EQUIVALENCE (BANK_SFPA(6),SFPA_LEN,RE_SFPA_LEN),(NAMES_SFPA(4),     29
     *NSFPA_LEN)                                                          29
      EQUIVALENCE (COMMEN_SFPA(4),CSFPA_LEN),(MAP_SFPA(1,4),MSFPA_LEN)    29
      CHARACTER CSFPA_LEN*40/' '/                                         29
      CHARACTER NSFPA_LEN*12/'R.LEN'/                                     29
      INTEGER MSFPA_LEN(2)/1,1/                                           29
      REAL RE_SFPA_LEN                                                    29
C -                                                                       29
      REAL SFPA_RAD                                                       29
      EQUIVALENCE (BANK_SFPA(7),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(5),     29
     *NSFPA_RAD)                                                          29
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_RAD),(MAP_SFPA(1,5),MSFPA_RAD)    29
      CHARACTER CSFPA_RAD*40/' '/                                         29
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     29
      INTEGER MSFPA_RAD(2)/1,1/                                           29
      REAL RE_SFPA_RAD                                                    29
C -                                                                       29
      REAL SFPA_NSSD                                                      29
      EQUIVALENCE (BANK_SFPA(8),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(6),   29
     *NSFPA_NSSD)                                                         29
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_NSSD),(MAP_SFPA(1,6),             29
     *MSFPA_NSSD)                                                         29
      CHARACTER CSFPA_NSSD*40/' '/                                        29
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   29
      INTEGER MSFPA_NSSD(2)/1,1/                                          29
      REAL RE_SFPA_NSSD                                                   29
C -                                                                       29
      REAL SFPA_DMWID                                                     29
      EQUIVALENCE (BANK_SFPA(9),SFPA_DMWID,RE_SFPA_DMWID),                29
     *(NAMES_SFPA(7),NSFPA_DMWID)                                         29
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_DMWID),(MAP_SFPA(1,7),            29
     *MSFPA_DMWID)                                                        29
      CHARACTER CSFPA_DMWID*40/' '/                                       29
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 29
      INTEGER MSFPA_DMWID(2)/1,1/                                         29
      REAL RE_SFPA_DMWID                                                  29
C -                                                                       29
      REAL SFPA_DMTHK                                                     29
      EQUIVALENCE (BANK_SFPA(10),SFPA_DMTHK,RE_SFPA_DMTHK),               29
     *(NAMES_SFPA(8),NSFPA_DMTHK)                                         29
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_DMTHK),(MAP_SFPA(1,8),            29
     *MSFPA_DMTHK)                                                        29
      CHARACTER CSFPA_DMTHK*40/' '/                                       29
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 29
      INTEGER MSFPA_DMTHK(2)/1,1/                                         29
      REAL RE_SFPA_DMTHK                                                  29
C -                                                                       29
      REAL SFPA_DMLEN                                                     29
      EQUIVALENCE (BANK_SFPA(11),SFPA_DMLEN,RE_SFPA_DMLEN),               29
     *(NAMES_SFPA(9),NSFPA_DMLEN)                                         29
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_DMLEN),(MAP_SFPA(1,9),            29
     *MSFPA_DMLEN)                                                        29
      CHARACTER CSFPA_DMLEN*40/' '/                                       29
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 29
      INTEGER MSFPA_DMLEN(2)/1,1/                                         29
      REAL RE_SFPA_DMLEN                                                  29
C -                                                                       29
      REAL SFPA_SMWID                                                     29
      EQUIVALENCE (BANK_SFPA(12),SFPA_SMWID,RE_SFPA_SMWID),               29
     *(NAMES_SFPA(10),NSFPA_SMWID)                                        29
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_SMWID),(MAP_SFPA(1,10),          29
     *MSFPA_SMWID)                                                        29
      CHARACTER CSFPA_SMWID*40/' '/                                       29
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 29
      INTEGER MSFPA_SMWID(2)/1,1/                                         29
      REAL RE_SFPA_SMWID                                                  29
C -                                                                       29
      REAL SFPA_SMTHK                                                     29
      EQUIVALENCE (BANK_SFPA(13),SFPA_SMTHK,RE_SFPA_SMTHK),               29
     *(NAMES_SFPA(11),NSFPA_SMTHK)                                        29
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_SMTHK),(MAP_SFPA(1,11),          29
     *MSFPA_SMTHK)                                                        29
      CHARACTER CSFPA_SMTHK*40/' '/                                       29
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 29
      INTEGER MSFPA_SMTHK(2)/1,1/                                         29
      REAL RE_SFPA_SMTHK                                                  29
C -                                                                       29
      REAL SFPA_SMLEN                                                     29
      EQUIVALENCE (BANK_SFPA(14),SFPA_SMLEN,RE_SFPA_SMLEN),               29
     *(NAMES_SFPA(12),NSFPA_SMLEN)                                        29
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_SMLEN),(MAP_SFPA(1,12),          29
     *MSFPA_SMLEN)                                                        29
      CHARACTER CSFPA_SMLEN*40/' '/                                       29
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 29
      INTEGER MSFPA_SMLEN(2)/1,1/                                         29
      REAL RE_SFPA_SMLEN                                                  29
C -                                                                       29
      REAL SFPA_SSLEN                                                     29
      EQUIVALENCE (BANK_SFPA(15),SFPA_SSLEN,RE_SFPA_SSLEN),               29
     *(NAMES_SFPA(13),NSFPA_SSLEN)                                        29
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SSLEN),(MAP_SFPA(1,13),          29
     *MSFPA_SSLEN)                                                        29
      CHARACTER CSFPA_SSLEN*40/' '/                                       29
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 29
      INTEGER MSFPA_SSLEN(2)/1,1/                                         29
      REAL RE_SFPA_SSLEN                                                  29
C -                                                                       29
      REAL SFPA_WPLEN                                                     29
      EQUIVALENCE (BANK_SFPA(16),SFPA_WPLEN,RE_SFPA_WPLEN),               29
     *(NAMES_SFPA(14),NSFPA_WPLEN)                                        29
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_WPLEN),(MAP_SFPA(1,14),          29
     *MSFPA_WPLEN)                                                        29
      CHARACTER CSFPA_WPLEN*40/' '/                                       29
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 29
      INTEGER MSFPA_WPLEN(2)/1,1/                                         29
      REAL RE_SFPA_WPLEN                                                  29
C -                                                                       29
      REAL SFPA_SDLEN                                                     29
      EQUIVALENCE (BANK_SFPA(17),SFPA_SDLEN,RE_SFPA_SDLEN),               29
     *(NAMES_SFPA(15),NSFPA_SDLEN)                                        29
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SDLEN),(MAP_SFPA(1,15),          29
     *MSFPA_SDLEN)                                                        29
      CHARACTER CSFPA_SDLEN*40/' '/                                       29
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 29
      INTEGER MSFPA_SDLEN(2)/1,1/                                         29
      REAL RE_SFPA_SDLEN                                                  29
C -                                                                       29
      REAL SFPA_TILT                                                      29
      EQUIVALENCE (BANK_SFPA(18),SFPA_TILT,RE_SFPA_TILT),                 29
     *(NAMES_SFPA(16),NSFPA_TILT)                                         29
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_TILT),(MAP_SFPA(1,16),           29
     *MSFPA_TILT)                                                         29
      CHARACTER CSFPA_TILT*40/' '/                                        29
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   29
      INTEGER MSFPA_TILT(2)/1,1/                                          29
      REAL RE_SFPA_TILT                                                   29
C -                                                                       29
      REAL SFPA_CPRAD                                                     29
      EQUIVALENCE (BANK_SFPA(19),SFPA_CPRAD,RE_SFPA_CPRAD),               29
     *(NAMES_SFPA(17),NSFPA_CPRAD)                                        29
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_CPRAD),(MAP_SFPA(1,17),          29
     *MSFPA_CPRAD)                                                        29
      CHARACTER CSFPA_CPRAD*40/' '/                                       29
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 29
      INTEGER MSFPA_CPRAD(2)/1,1/                                         29
      REAL RE_SFPA_CPRAD                                                  29
C -                                                                       29
      REAL SFPA_CPRAL                                                     29
      EQUIVALENCE (BANK_SFPA(20),SFPA_CPRAL,RE_SFPA_CPRAL),               29
     *(NAMES_SFPA(18),NSFPA_CPRAL)                                        29
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_CPRAL),(MAP_SFPA(1,18),          29
     *MSFPA_CPRAL)                                                        29
      CHARACTER CSFPA_CPRAL*40/' '/                                       29
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 29
      INTEGER MSFPA_CPRAL(2)/1,1/                                         29
      REAL RE_SFPA_CPRAL                                                  29
C -                                                                       29
      REAL SFPA_CFRAD                                                     29
      EQUIVALENCE (BANK_SFPA(21),SFPA_CFRAD,RE_SFPA_CFRAD),               29
     *(NAMES_SFPA(19),NSFPA_CFRAD)                                        29
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_CFRAD),(MAP_SFPA(1,19),          29
     *MSFPA_CFRAD)                                                        29
      CHARACTER CSFPA_CFRAD*40/' '/                                       29
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 29
      INTEGER MSFPA_CFRAD(2)/1,1/                                         29
      REAL RE_SFPA_CFRAD                                                  29
C -                                                                       29
      REAL SFPA_GPTHK                                                     29
      EQUIVALENCE (BANK_SFPA(22),SFPA_GPTHK,RE_SFPA_GPTHK),               29
     *(NAMES_SFPA(20),NSFPA_GPTHK)                                        29
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_GPTHK),(MAP_SFPA(1,20),          29
     *MSFPA_GPTHK)                                                        29
      CHARACTER CSFPA_GPTHK*40/' '/                                       29
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 29
      INTEGER MSFPA_GPTHK(2)/1,1/                                         29
      REAL RE_SFPA_GPTHK                                                  29
C -                                                                       29
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                29
      PARAMETER (LEN_SFPA=22)                                             29
      PARAMETER (LLEN_SFPA=20)                                            29
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           29
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        29
      DATA IDNUM_SFPA/10*0/                                               29
      REAL BANK_SFPA(LEN_SFPA)                                            29
      CHARACTER*60 BPATH_SFPA/' '/                                        29
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          29
     *BPATH_SFPA                                                          29
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             29
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
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 32
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         34
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           36
      REAL XPOS,YPOS,ZPOS,CLEARENCE,RIN,ROU,ELETHK,TABLEN,RADMAX          38
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    40
      REAL XBUF,PHI,XBUF1,XBUF2                                           42
C *                                                                       44
      CALL AGSBEGM(MODULE,IPRIN)                                          44
      IF (IPRIN.LT.0) RETURN                                              44
C Filling bank SVTG - BASIC SVT DIMENSIONS                                44
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   44
      BPATH_SVTG='SVTG*'                                                  44
      IDNUM_SVTG(1)=0                                                     45
C GEOMETRY VERSION                                                        45
      SVTG_VERSION=2                                                      45
      CSVTG_VERSION='GEOMETRY VERSION'                                    45
C NUMBER OF SVT LAYERS (WAS 7)                                            46
      SVTG_NLAYER=7                                                       46
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         46
C STV INNERMOST RADIUS                                                    47
      SVTG_RSIZEMIN=4.100                                                 47
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               47
C STV OUTERMOST RADIUS                                                    48
      SVTG_RSIZEMAX=46.107                                                48
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               48
C SVT+FTPC LENGTH                                                         49
      SVTG_ZSIZEMAX=270                                                   49
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    49
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          50
      SVTG_ANGOFF=0                                                       50
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       50
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    51
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           51
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   51
C *                                                                       51
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    51
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           51
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   51
C *                                                                       52
      CALL AGSBEGM(MODULE,IPRIN)                                          52
      IF (IPRIN.LT.0) RETURN                                              52
C Filling bank SWCA - WAFER CARRIER                                       52
      BANK_TITLE='WAFER CARRIER'                                          52
      BPATH_SWCA='SWCA*'                                                  52
      IDNUM_SWCA(1)=0                                                     53
C GEOMETRY VERSION                                                        53
      SWCA_VERSION=1                                                      53
      CSWCA_VERSION='GEOMETRY VERSION'                                    53
C LADDER LENGTH                                                           54
      SWCA_LENGTH=56.04                                                   54
      CSWCA_LENGTH='LADDER LENGTH'                                        54
C WAFER WIDTH                                                             55
      SWCA_WAFERWID=6.305                                                 55
      CSWCA_WAFERWID='WAFER WIDTH'                                        55
C WAFER LENGTH                                                            56
      SWCA_WAFERLEN=6.305                                                 56
      CSWCA_WAFERLEN='WAFER LENGTH'                                       56
C WAFER THICKNESS                                                         57
      SWCA_WAFERTHK=0.0300                                                57
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    57
C ROHA CELL PLUS GLUE THICKNESS                                           58
      SWCA_ROHATHK=0.0381                                                 58
      CSWCA_ROHATHK='ROHA CELL PLUS GLUE THICKNESS'                       58
C WAFER CARRIER RAILS WIDTH                                               59
      SWCA_WAFCARWD=1.5                                                   59
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          59
C WAFER CARRIER THICKNESS                                                 60
      SWCA_WAFCARTH=0.0300                                                60
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            60
C WIDTH OF THE INTER-WAFER GAP                                            61
      SWCA_WAFERGAP=0.0                                                   61
      CSWCA_WAFERGAP='WIDTH OF THE INTER-WAFER GAP'                       61
C DRIFT DIRECTION                                                         62
      SWCA_DRIFT=1                                                        62
      CSWCA_DRIFT='DRIFT DIRECTION'                                       62
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        63
      SWCA_STRUTLEN=1.0                                                   63
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   63
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    64
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           64
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   64
C *                                                                       64
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    64
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           64
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   64
C *                                                                       65
      CALL AGSBEGM(MODULE,IPRIN)                                          65
      IF (IPRIN.LT.0) RETURN                                              65
C Filling bank SSUP - SUPPORT STRUCTURES                                  65
      BANK_TITLE='SUPPORT STRUCTURES'                                     65
      BPATH_SSUP='SSUP*'                                                  65
      IDNUM_SSUP(1)=0                                                     66
C GEOMETRY VERSION                                                        66
      SSUP_VERSION=1                                                      66
      CSSUP_VERSION='GEOMETRY VERSION'                                    66
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            67
      SSUP_CABTHK=0.05                                                    67
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         67
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             68
      SSUP_HOSRMN=0.75                                                    68
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          68
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             69
      SSUP_HOSRMX=0.95                                                    69
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          69
C NUMBER OF WATER HOSES                                                   70
      SSUP_NHOSES=10                                                      70
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                70
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             71
      SSUP_WRPMYTHK=0.10                                                  71
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        71
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   72
      SSUP_WRPALTHK=0.01                                                  72
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              72
C SUPPORT CONE THICKNESS                                                  73
      SSUP_GRPHTHK=0.16                                                   73
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              73
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              74
      SSUP_CONE1ZMN=52.23                                                 74
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         74
C LENGTH OF SUPPORT RODS                                                  75
      SSUP_RODLEN=110.8                                                   75
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               75
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               76
      SSUP_RODDIST=17.5                                                   76
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           76
C ID OF BE SUPPORT RODS                                                   77
      SSUP_RODID=3.5                                                      77
      CSSUP_RODID='ID OF BE SUPPORT RODS'                                 77
C OD OF BE SUPPORT RODS                                                   78
      SSUP_RODOD=3.81                                                     78
      CSSUP_RODOD='OD OF BE SUPPORT RODS'                                 78
C MINIMUM ID OF CONE 1                                                    79
      SSUP_CON1IDMN=15.67                                                 79
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               79
C MINIMUM ID OF CONE 3 (TBD)                                              80
      SSUP_CON3IDMN=21.67                                                 80
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         80
C MINIMUM ID OF CONE 4 (TBD)                                              81
      SSUP_CON4IDMN=37.4                                                  81
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         81
C MAXIMUM ID OF CONE 4 (TBD)                                              82
      SSUP_CON4IDMX=37.4                                                  82
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         82
C MAXIMUM Z OF CONE 3 (TBD)                                               83
      SSUP_CONE3ZMX=150.0                                                 83
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          83
C MAXIMUM Z OF CONE 4 (TBD)                                               84
      SSUP_CONE4ZMX=229.36                                                84
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          84
C THICKNESS OF AL BRACKETS                                                85
      SSUP_BRATHK=.2                                                      85
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             85
C (Z) THICKNESS OF END RING JOINING BRACKETS                              86
      SSUP_ERJTHK=.1                                                      86
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           86
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          87
      SSUP_ERJWID=2.07                                                    87
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       87
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            88
      SSUP_ERJLEN=5.19                                                    88
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         88
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    89
      SSUP_ERJZDIS=2.0                                                    89
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                89
C ERJ SCREW 1 X POSITION (RADIAL)                                         90
      SSUP_ERJ1X=0.31                                                     90
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       90
C ERJ SCREW 2 X POSITION                                                  91
      SSUP_ERJ2X=1.15                                                     91
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                91
C ERJ SCREW 2 Y POSITION                                                  92
      SSUP_ERJ2Y=0.72                                                     92
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                92
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   93
      SSUP_ERJRAD=10.80                                                   93
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                93
C ERJ SCREW DIAMETER                                                      94
      SSUP_ERJDIA=0.17                                                    94
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   94
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    95
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           95
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   95
C *                                                                       95
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    95
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           95
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   95
C *                                                                       96
      CALL AGSBEGM(MODULE,IPRIN)                                          96
      IF (IPRIN.LT.0) RETURN                                              96
C Filling bank SSUB - BEAMPIPE SUPPORT                                    96
      BANK_TITLE='BEAMPIPE SUPPORT'                                       96
      BPATH_SSUB='SSUB*'                                                  96
      IDNUM_SSUB(1)=0                                                     97
C GEOMETRY VERSION                                                        97
      SSUB_VERSION=1                                                      97
      CSSUB_VERSION='GEOMETRY VERSION'                                    97
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  98
      SSUB_KMOUNTID=31.34                                                 98
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             98
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  99
      SSUB_KMOUNTOD=38.96                                                 99
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             99
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    100
      SSUB_KMNTTHK=0.64                                                   100
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                100
C OD OF CUTOUT IN KINEMATIC MOUNT                                         101
      SSUB_KMCUTOD=18.31                                                  101
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     101
C ID OF CUTOUT IN KINEMATIC MOUNT                                         102
      SSUB_KMCUTID=14                                                     102
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     102
C OPENING ANGLE OF CUTOUT                                                 103
      SSUB_KMCUTOA=38                                                     103
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             103
C OFFSET OF CUTOUT CENTER FROM AXIS                                       104
      SSUB_KMCUTOFF=26.58                                                 104
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  104
C ID OF BEAMPIPE SUPPORT RING                                             105
      SSUB_SRINGID=8.47                                                   105
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         105
C OD OF BEAMPIPE SUPPORT RING                                             106
      SSUB_SRINGOD=12.78                                                  106
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         106
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     107
      SSUB_SRINGTHK=0.64                                                  107
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                107
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     108
      SSUB_SRCUTPHI=38                                                    108
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                108
C SUPPORT RING CUTOUT WIDTH                                               109
      SSUB_SRCUTWID=3.63                                                  109
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          109
C SUPPORT RING CUTOUT DEPTH                                               110
      SSUB_SRCUTOUT=5.08                                                  110
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          110
C SUPPORT RING CUTOUT START                                               111
      SSUB_SRCUTIN=3.5                                                    111
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           111
C SUPPORT ROLLER ID                                                       112
      SSUB_SROLLID=0.2                                                    112
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   112
C SUPPORT ROLLER OD                                                       113
      SSUB_SROLLOD=0.62                                                   113
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   113
C SUPPORT ROLLER LENGTH                                                   114
      SSUB_SROLLLEN=2.54                                                  114
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              114
C SUPPORT ROLLER AXIS LENGTH                                              115
      SSUB_SWIRELEN=5.08                                                  115
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         115
C MOUNTING BLOCK HEIGHT                                                   116
      SSUB_MBLKHGH=0.97                                                   116
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               116
C OUTER MOUNTING BLOCK WIDTH                                              117
      SSUB_MBLKOWID=2.54                                                  117
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         117
C OUTER MOUNTING BLOCK LENGTH                                             118
      SSUB_MBLKOLEN=1.27                                                  118
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        118
C INNER MOUNTING BLOCK WIDTH                                              119
      SSUB_MBLKIWID=3.175                                                 119
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         119
C INNER MOUNTING BLOCK LENGTH                                             120
      SSUB_MBLKILEN=1.27                                                  120
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        120
C OUTER MOUNTING BLOCK AT RADIUS                                          121
      SSUB_MBLKORAD=17.4                                                  121
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     121
C INNER MOUNTING BLOCK AT RADIUS                                          122
      SSUB_MBLKIRAD=5.42                                                  122
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     122
C MOUNTING SUPPORT ROD DIAMETER                                           123
      SSUB_MRODDIA=0.32                                                   123
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       123
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    124
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           124
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   124
C *                                                                       124
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    124
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           124
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   124
C *                                                                       125
      CALL AGSBEGM(MODULE,IPRIN)                                          125
      IF (IPRIN.LT.0) RETURN                                              125
C Filling bank SERG - END RINGS                                           125
      BANK_TITLE='END RINGS'                                              125
      BPATH_SERG='SERG*'                                                  125
      IDNUM_SERG(1)=0                                                     126
C GEOMETRY VERSION                                                        126
      SERG_VERSION=1                                                      126
      CSERG_VERSION='GEOMETRY VERSION'                                    126
C INNER END RING TUBE MAXIMUM RADIUS                                      127
      SERG_IRNGTRMX=9.703                                                 127
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 127
C INNER END RING POLYGON MINIMUM RADIUS                                   128
      SERG_IRNGPRMN=7.671                                                 128
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              128
C OUTER END RING MINIMUM RADIUS                                           129
      SERG_ORNGRMIN=11.900                                                129
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      129
C OUTER END RING MAXIMUM RADIUS                                           130
      SERG_ORNGRMAX=13.805                                                130
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      130
C END RING THICKNESS                                                      131
      SERG_ENDRNGTH=0.2                                                   131
      CSERG_ENDRNGTH='END RING THICKNESS'                                 131
C MINIMUM Z FOR END RINGS                                                 132
      SERG_ENDRNGZM=23.01                                                 132
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            132
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    133
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           133
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   133
C *                                                                       133
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    133
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           133
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   133
C *                                                                       134
      CALL AGSBEGM(MODULE,IPRIN)                                          134
      IF (IPRIN.LT.0) RETURN                                              134
C Filling bank SWAM - WATER MANIFOLD                                      134
      BANK_TITLE='WATER MANIFOLD'                                         134
      BPATH_SWAM='SWAM*'                                                  134
      IDNUM_SWAM(1)=0                                                     135
C GEOMETRY VERSION                                                        135
      SWAM_VERSION=1                                                      135
      CSWAM_VERSION='GEOMETRY VERSION'                                    135
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                136
      SWAM_ZMIN=33.9                                                      136
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               136
C WATER MANIFOLD FULL LENGTH                                              137
      SWAM_LEN=16.0                                                       137
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              137
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         138
      SWAM_RMIN=15.24                                                     138
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        138
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         139
      SWAM_RMAX=16.83                                                     139
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        139
C TRANSITION BOARD THICKNESS                                              140
      SWAM_TBRDTHK=0.1                                                    140
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          140
C WATER MANIFOLD WALL THICKNESS                                           141
      SWAM_WALLTHK=0.1                                                    141
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       141
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    142
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           142
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   142
C *                                                                       142
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    142
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           142
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   142
C *                                                                       143
      CALL AGSBEGM(MODULE,IPRIN)                                          143
      IF (IPRIN.LT.0) RETURN                                              143
C Filling bank SELC - ELECTRONICS CARRIER DATA                            143
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               143
      BPATH_SELC='SELC*'                                                  143
      IDNUM_SELC(1)=0                                                     144
C GEOMETRY VERSION                                                        144
      SELC_VERSION=1                                                      144
      CSELC_VERSION='GEOMETRY VERSION'                                    144
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              145
      SELC_BETHK=0.0500                                                   145
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            145
C THICKNESS OF WATER CHANNEL                                              146
      SELC_WATTHK=0.0750                                                  146
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           146
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    147
      SELC_BEOTHK=0.0500                                                  147
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 147
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                148
      SELC_DYETHK=0.0340                                                  148
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             148
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                149
      SELC_DYEWID=0.3                                                     149
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             149
C SPACE IC CHIPS                                                          150
      SELC_DYESPC=0.1                                                     150
      CSELC_DYESPC='SPACE IC CHIPS'                                       150
C ELECTRONICS CARRIER WIDTH                                               151
      SELC_ELCAWID=2.0                                                    151
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           151
C THICKNESS OF AG-PD CONDUCTOR                                            152
      SELC_AGPDTHK=0.0030                                                 152
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        152
C THICKNESS OF GLASS INSULATOR                                            153
      SELC_GLASSTHK=0.0150                                                153
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       153
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        154
      SELC_CABTHK=0.0033                                                  154
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     154
C CABLE WIDTH                                                             155
      SELC_CABWID=0.6                                                     155
      CSELC_CABWID='CABLE WIDTH'                                          155
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    156
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           156
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   156
C *                                                                       156
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    156
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           156
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   156
C *                                                                       157
      CALL AGSBEGM(MODULE,IPRIN)                                          157
      IF (IPRIN.LT.0) RETURN                                              157
C Filling bank SVTL - SINGLE LAYER DATA                                   157
      BANK_TITLE='SINGLE LAYER DATA'                                      157
      BPATH_SVTL='SVTL*'                                                  157
      IDNUM_SVTL(1)=0                                                     158
C LAYER NUMBER                                                            158
      SVTL_LAYER=1                                                        158
      CSVTL_LAYER='LAYER NUMBER'                                          158
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 159
      SVTL_RADIUS=6.37+0.025                                              159
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              159
C NUMBER OF LADDER                                                        160
      SVTL_NLADDER=4                                                      160
      CSVTL_NLADDER='NUMBER OF LADDER'                                    160
C NUMBER OF WAFERS                                                        161
      SVTL_NWAFER=4                                                       161
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     161
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    162
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           162
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   162
C *                                                                       162
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    162
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           162
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   162
C *                                                                       163
      CALL AGSBEGM(MODULE,IPRIN)                                          163
      IF (IPRIN.LT.0) RETURN                                              163
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             163
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                163
      BPATH_SVTL='SVTL*'                                                  163
      IDNUM_SVTL(1)=0                                                     164
C LAYER NUMBER                                                            164
      SVTL_LAYER=2                                                        164
      CSVTL_LAYER='LAYER NUMBER'                                          164
C LAYER RADIUS                                                            165
      SVTL_RADIUS=7.38+0.025                                              165
      CSVTL_RADIUS='LAYER RADIUS'                                         165
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    166
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           166
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   166
C *                                                                       166
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    166
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           166
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   166
C *                                                                       167
      CALL AGSBEGM(MODULE,IPRIN)                                          167
      IF (IPRIN.LT.0) RETURN                                              167
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             167
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                167
      BPATH_SVTL='SVTL*'                                                  167
      IDNUM_SVTL(1)=0                                                     168
C LAYER NUMBER                                                            168
      SVTL_LAYER=3                                                        168
      CSVTL_LAYER='LAYER NUMBER'                                          168
C LAYER RADIUS                                                            169
      SVTL_RADIUS=10.38+0.025                                             169
      CSVTL_RADIUS='LAYER RADIUS'                                         169
C NUMBER OF LADDER                                                        170
      SVTL_NLADDER=6                                                      170
      CSVTL_NLADDER='NUMBER OF LADDER'                                    170
C NUMBER OF WAFERS                                                        171
      SVTL_NWAFER=6                                                       171
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     171
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    172
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           172
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   172
C *                                                                       172
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    172
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           172
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   172
C *                                                                       173
      CALL AGSBEGM(MODULE,IPRIN)                                          173
      IF (IPRIN.LT.0) RETURN                                              173
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             173
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                173
      BPATH_SVTL='SVTL*'                                                  173
      IDNUM_SVTL(1)=0                                                     174
C LAYER NUMBER                                                            174
      SVTL_LAYER=4                                                        174
      CSVTL_LAYER='LAYER NUMBER'                                          174
C LAYER RADIUS                                                            175
      SVTL_RADIUS=11.27+0.025                                             175
      CSVTL_RADIUS='LAYER RADIUS'                                         175
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    176
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           176
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   176
C *                                                                       176
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    176
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           176
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   176
C *                                                                       177
      CALL AGSBEGM(MODULE,IPRIN)                                          177
      IF (IPRIN.LT.0) RETURN                                              177
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             177
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                177
      BPATH_SVTL='SVTL*'                                                  177
      IDNUM_SVTL(1)=0                                                     178
C LAYER NUMBER                                                            178
      SVTL_LAYER=5                                                        178
      CSVTL_LAYER='LAYER NUMBER'                                          178
C LAYER RADIUS                                                            179
      SVTL_RADIUS=14.19+0.025                                             179
      CSVTL_RADIUS='LAYER RADIUS'                                         179
C NUMBER OF LADDER                                                        180
      SVTL_NLADDER=8                                                      180
      CSVTL_NLADDER='NUMBER OF LADDER'                                    180
C NUMBER OF WAFERS                                                        181
      SVTL_NWAFER=7                                                       181
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     181
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    182
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           182
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   182
C *                                                                       182
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    182
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           182
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   182
C *                                                                       183
      CALL AGSBEGM(MODULE,IPRIN)                                          183
      IF (IPRIN.LT.0) RETURN                                              183
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             183
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                183
      BPATH_SVTL='SVTL*'                                                  183
      IDNUM_SVTL(1)=0                                                     184
C LAYER NUMBER                                                            184
      SVTL_LAYER=6                                                        184
      CSVTL_LAYER='LAYER NUMBER'                                          184
C LAYER RADIUS                                                            185
      SVTL_RADIUS=15.13+0.025                                             185
      CSVTL_RADIUS='LAYER RADIUS'                                         185
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    186
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           186
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   186
C *                                                                       186
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    186
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           186
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   186
C *                                                                       187
      CALL AGSBEGM(MODULE,IPRIN)                                          187
      IF (IPRIN.LT.0) RETURN                                              187
C Filling bank SFPA - FOURTH LAYER PARAMETERS                             187
      BANK_TITLE='FOURTH LAYER PARAMETERS'                                187
      BPATH_SFPA='SFPA*'                                                  187
      IDNUM_SFPA(1)=0                                                     188
C GEOMETRY VERSION                                                        188
      SFPA_VERSION=1                                                      188
      CSFPA_VERSION='GEOMETRY VERSION'                                    188
C MOTHER RMIN                                                             189
      SFPA_RMIN=22.0                                                      189
      CSFPA_RMIN='MOTHER RMIN'                                            189
C MOTHER RMAX                                                             190
      SFPA_RMAX=28.0                                                      190
      CSFPA_RMAX='MOTHER RMAX'                                            190
C MOTHER LEN                                                              191
      SFPA_LEN=100.                                                       191
      CSFPA_LEN='MOTHER LEN'                                              191
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              192
      SFPA_RAD=23.                                                        192
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              192
C NUMBER OF SILICON STRIP DETECTORS                                       193
      SFPA_NSSD=16                                                        193
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      193
C DETECTOR MOTHER WIDTH                                                   194
      SFPA_DMWID=7.5                                                      194
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 194
C DETECTOR MOTHER THICKNESS                                               195
      SFPA_DMTHK=0.03                                                     195
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             195
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          196
      SFPA_DMLEN=90.                                                      196
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        196
C STRUCTURE MOTHER WIDTH                                                  197
      SFPA_SMWID=7.5                                                      197
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                197
C STRUCTURE MOTHER THICKNESS                                              198
      SFPA_SMTHK=3.5                                                      198
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            198
C STRUCTURE MOTHER LENGTH (COOL. PIPE+CARBON FIBER)                       199
      SFPA_SMLEN=95.                                                      199
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH (COOL. PIPE+CARBON FIBER)'     199
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  200
      SFPA_SSLEN=95./20.                                                  200
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                200
C LENGTH OF WAFER PACK                                                    201
      SFPA_WPLEN=68.8                                                     201
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  201
C LENGHT OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          202
      SFPA_SDLEN=4.2                                                      202
      CSFPA_SDLEN='LENGHT OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        202
C TILING ANGLE (DEGREES)                                                  203
      SFPA_TILT=5.0                                                       203
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 203
C COOLING PIPE OUTER RADIUS                                               204
      SFPA_CPRAD=0.1                                                      204
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             204
C COOLING PIPE INNER RADIUS                                               205
      SFPA_CPRAL=0.09                                                     205
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             205
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            206
      SFPA_CFRAD=0.1                                                      206
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          206
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               207
      SFPA_GPTHK=0.5                                                      207
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             207
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    208
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           208
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   208
C *                                                                       208
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    208
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           208
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   208
C *                                                                       209
      CALL AGSBEGM(MODULE,IPRIN)                                          209
      IF (IPRIN.LT.0) RETURN                                              209
C Filling bank SSLD - SHIELDING PARAMETERS                                209
      BANK_TITLE='SHIELDING PARAMETERS'                                   209
      BPATH_SSLD='SSLD*'                                                  209
      IDNUM_SSLD(1)=0                                                     210
C GEOMETRY VERSION                                                        210
      SSLD_VERSION=1                                                      210
      CSSLD_VERSION='GEOMETRY VERSION'                                    210
C INNER SHIELD CYLINDER, INNER RADIUS                                     211
      SSLD_SINRINN=5                                                      211
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 211
C INNER SHIELD CYLINDER, OUTER RADIUS                                     212
      SSLD_SINROUT=5.008                                                  212
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 212
C INNER SHIELD CYLINDER, HALF LENGTH                                      213
      SSLD_SINLEN=53.5                                                    213
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   213
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                214
      SSLD_SSEPRINN=22                                                    214
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           214
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                215
      SSLD_SSEPROUT=22.018                                                215
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           215
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 216
      SSLD_SSEPLEN=55.4                                                   216
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             216
C OUTER SHIELD CYLINDER, INNER RADIUS                                     217
      SSLD_SOUTRINN=28                                                    217
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                217
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     218
      SSLD_SOUTROUT=28.02                                                 218
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                218
C OUTER SHIELD CYLINDER, HALF LENGTH                                      219
      SSLD_SOUTLEN=65.4                                                   219
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  219
C ALUMINUM SHIELD MESH INNER DIAMETER                                     220
      SSLD_ALMESHID=9.7                                                   220
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                220
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     221
      SSLD_ALMESHOD=44                                                    221
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                221
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                222
      SSLD_ALMSHTHK=0.03                                                  222
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           222
C ALUMINUM SHIELD MESH Z POSITION                                         223
      SSLD_ALMSHPOS=53.5                                                  223
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    223
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    224
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           224
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   224
C *                                                                       224
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    224
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           224
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   224
C use default SVTG bank                                                   225
      BPATH_SVTG='SVTG*'                                                  225
      IDNUM_SVTG(1)=0                                                     225
      CALL AGDGETP('SVTTGEO','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,   225
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      225
     *LINK_SVTG,FLAG_SVTG)                                                225
C use default SWCA bank                                                   226
      BPATH_SWCA='SWCA*'                                                  226
      IDNUM_SWCA(1)=0                                                     226
      CALL AGDGETP('SVTTGEO','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,   226
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      226
     *LINK_SWCA,FLAG_SWCA)                                                226
C use default SELC bank                                                   227
      BPATH_SELC='SELC*'                                                  227
      IDNUM_SELC(1)=0                                                     227
      CALL AGDGETP('SVTTGEO','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,   227
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      227
     *LINK_SELC,FLAG_SELC)                                                227
C use default SSUP bank                                                   228
      BPATH_SSUP='SSUP*'                                                  228
      IDNUM_SSUP(1)=0                                                     228
      CALL AGDGETP('SVTTGEO','*',-999,'SSUP',' ',BANK_SSUP, BPATH_SSUP,   228
     *IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,BANK_SSUP,LEN_SSUP,LLEN_SSUP,      228
     *LINK_SSUP,FLAG_SSUP)                                                228
C use default SSUB bank                                                   229
      BPATH_SSUB='SSUB*'                                                  229
      IDNUM_SSUB(1)=0                                                     229
      CALL AGDGETP('SVTTGEO','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,   229
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      229
     *LINK_SSUB,FLAG_SSUB)                                                229
C use default SERG bank                                                   230
      BPATH_SERG='SERG*'                                                  230
      IDNUM_SERG(1)=0                                                     230
      CALL AGDGETP('SVTTGEO','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,   230
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      230
     *LINK_SERG,FLAG_SERG)                                                230
C use default SWAM bank                                                   231
      BPATH_SWAM='SWAM*'                                                  231
      IDNUM_SWAM(1)=0                                                     231
      CALL AGDGETP('SVTTGEO','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,   231
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      231
     *LINK_SWAM,FLAG_SWAM)                                                231
C use default SELC bank                                                   232
      BPATH_SELC='SELC*'                                                  232
      IDNUM_SELC(1)=0                                                     232
      CALL AGDGETP('SVTTGEO','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,   232
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      232
     *LINK_SELC,FLAG_SELC)                                                232
C use default SFPA bank                                                   233
      BPATH_SFPA='SFPA*'                                                  233
      IDNUM_SFPA(1)=0                                                     233
      CALL AGDGETP('SVTTGEO','*',-999,'SFPA',' ',BANK_SFPA, BPATH_SFPA,   233
     *IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,BANK_SFPA,LEN_SFPA,LLEN_SFPA,      233
     *LINK_SFPA,FLAG_SFPA)                                                233
C use default SSLD bank                                                   234
      BPATH_SSLD='SSLD*'                                                  234
      IDNUM_SSLD(1)=0                                                     234
      CALL AGDGETP('SVTTGEO','*',-999,'SSLD',' ',BANK_SSLD, BPATH_SSLD,   234
     *IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,BANK_SSLD,LEN_SSLD,LLEN_SSLD,      234
     *LINK_SSLD,FLAG_SSLD)                                                234
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             235
      CALL AGSRESET                                                       235
      AG_A = 28.08                                                        235
      AG_Z = 14                                                           235
      AG_W = 0.6*1*28./60.                                                235
      AG_TITLE = 'SI'                                                     235
      AG_EXNAME = 'COMPONENT'                                             235
      AG_PARLIST = 'A_Z_W'                                                235
      CALL AXCOMPONENT                                                    235
C *                                                                       235
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  235
C =                                                                       235
      CALL AGSRESET                                                       236
      AG_A = 16                                                           236
      AG_Z = 8                                                            236
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               236
      AG_TITLE = 'O'                                                      236
      AG_EXNAME = 'COMPONENT'                                             236
      AG_PARLIST = 'A_Z_W'                                                236
      CALL AXCOMPONENT                                                    236
C *                                                                       236
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 236
      CALL AGSRESET                                                       237
      AG_A = 12                                                           237
      AG_Z = 6                                                            237
      AG_W = 0.4*8*12./174.                                               237
      AG_TITLE = 'C'                                                      237
      AG_EXNAME = 'COMPONENT'                                             237
      AG_PARLIST = 'A_Z_W'                                                237
      CALL AXCOMPONENT                                                    237
C *                                                                       237
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  237
      CALL AGSRESET                                                       238
      AG_A = 1                                                            238
      AG_Z = 1                                                            238
      AG_W = 0.4*14*1./174.                                               238
      AG_TITLE = 'H'                                                      238
      AG_EXNAME = 'COMPONENT'                                             238
      AG_PARLIST = 'A_Z_W'                                                238
      CALL AXCOMPONENT                                                    238
C *                                                                       238
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  238
      CALL AGSRESET                                                       239
      AG_DENS = 1.7                                                       239
      AG_TITLE = 'G10'                                                    239
      AG_EXNAME = 'MIXTURE'                                               239
      AG_PARLIST = 'DENS'                                                 239
      CALL AXMIXTURE                                                      239
C *                                                                       239
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       239
      CALL AGSRESET                                                       240
      AG_A = 12                                                           240
      AG_Z = 6                                                            240
      AG_W = 10000*5                                                      240
      AG_TITLE = 'C5'                                                     240
      AG_EXNAME = 'COMPONENT'                                             240
      AG_PARLIST = 'A_Z_W'                                                240
      CALL AXCOMPONENT                                                    240
C *                                                                       240
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        240
      CALL AGSRESET                                                       241
      AG_A = 1                                                            241
      AG_Z = 1                                                            241
      AG_W = 10000*4                                                      241
      AG_TITLE = 'H4'                                                     241
      AG_EXNAME = 'COMPONENT'                                             241
      AG_PARLIST = 'A_Z_W'                                                241
      CALL AXCOMPONENT                                                    241
C *                                                                       241
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       241
      CALL AGSRESET                                                       242
      AG_A = 16                                                           242
      AG_Z = 8                                                            242
      AG_W = 10000*2                                                      242
      AG_TITLE = 'O2'                                                     242
      AG_EXNAME = 'COMPONENT'                                             242
      AG_PARLIST = 'A_Z_W'                                                242
      CALL AXCOMPONENT                                                    242
C *                                                                       242
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 242
      CALL AGSRESET                                                       243
      AG_A = 27                                                           243
      AG_Z = 13                                                           243
      AG_W = 10000*0.2302                                                 243
      AG_TITLE = 'AL'                                                     243
      AG_EXNAME = 'COMPONENT'                                             243
      AG_PARLIST = 'A_Z_W'                                                243
      CALL AXCOMPONENT                                                    243
C *                                                                       243
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              243
      CALL AGSRESET                                                       244
      AG_DENS = 1.432                                                     244
      AG_TITLE = 'ALKAP'                                                  244
      AG_EXNAME = 'MIXTURE'                                               244
      AG_PARLIST = 'DENS'                                                 244
      CALL AXMIXTURE                                                      244
C *                                                                       244
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       244
      CALL AGSRESET                                                       245
      AG_A = 12                                                           245
      AG_Z = 6                                                            245
      AG_W = 10000*5                                                      245
      AG_TITLE = 'C5'                                                     245
      AG_EXNAME = 'COMPONENT'                                             245
      AG_PARLIST = 'A_Z_W'                                                245
      CALL AXCOMPONENT                                                    245
C *                                                                       245
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        245
      CALL AGSRESET                                                       246
      AG_A = 1                                                            246
      AG_Z = 1                                                            246
      AG_W = 10000*4                                                      246
      AG_TITLE = 'H4'                                                     246
      AG_EXNAME = 'COMPONENT'                                             246
      AG_PARLIST = 'A_Z_W'                                                246
      CALL AXCOMPONENT                                                    246
C *                                                                       246
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       246
      CALL AGSRESET                                                       247
      AG_A = 16                                                           247
      AG_Z = 8                                                            247
      AG_W = 10000*2                                                      247
      AG_TITLE = 'O2'                                                     247
      AG_EXNAME = 'COMPONENT'                                             247
      AG_PARLIST = 'A_Z_W'                                                247
      CALL AXCOMPONENT                                                    247
C *                                                                       247
C Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                          247
      CALL AGSRESET                                                       248
      AG_A = 27                                                           248
      AG_Z = 13                                                           248
      AG_W = 986                                                          248
      AG_TITLE = 'AL'                                                     248
      AG_EXNAME = 'COMPONENT'                                             248
      AG_PARLIST = 'A_Z_W'                                                248
      CALL AXCOMPONENT                                                    248
C *                                                                       248
C Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                          248
      CALL AGSRESET                                                       249
      AG_DENS = 1.40845                                                   249
      AG_TITLE = 'SSDALMY'                                                249
      AG_EXNAME = 'MIXTURE'                                               249
      AG_PARLIST = 'DENS'                                                 249
      CALL AXMIXTURE                                                      249
C *                                                                       249
C Check SWAM_LEN>0                                                        249
      IF (SWAM_LEN.GT.0) THEN                                             250
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              251
      CALL AGSRESET                                                       251
      AG_A = 1                                                            251
      AG_Z = 1                                                            251
      AG_W = 2                                                            251
      AG_TITLE = 'H2'                                                     251
      AG_EXNAME = 'COMPONENT'                                             251
      AG_PARLIST = 'A_Z_W'                                                251
      CALL AXCOMPONENT                                                    251
C *                                                                       251
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              251
      CALL AGSRESET                                                       252
      AG_A = 16                                                           252
      AG_Z = 8                                                            252
      AG_W = 1                                                            252
      AG_TITLE = 'O'                                                      252
      AG_EXNAME = 'COMPONENT'                                             252
      AG_PARLIST = 'A_Z_W'                                                252
      CALL AXCOMPONENT                                                    252
C *                                                                       252
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                252
      CALL AGSRESET                                                       253
      AG_DENS = 1.0                                                       253
      AG_TITLE = 'WATER'                                                  253
      AG_EXNAME = 'MIXTURE'                                               253
      AG_PARLIST = 'DENS'                                                 253
      CALL AXMIXTURE                                                      253
C *                                                                       253
      ELSE                                                                253
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             255
      CALL AGSRESET                                                       255
      AG_DENS = 0.0009                                                    255
      AG_TITLE = 'WATER'                                                  255
      AG_EXNAME = 'MIXTURE'                                               255
      AG_PARLIST = 'DENS'                                                 255
      CALL AXMIXTURE                                                      255
C *                                                                       255
      ENDIF                                                               255
      CALL AGSBEGM(MODULE,IPRIN)                                          257
C Check IPRIN<0                                                           257
      IF (IPRIN.LT.0) RETURN                                              257
C Create SVTT :                                                           257
C Call To Local Procedure SVTT                                            257
      ASSIGN 5020 TO BLOCK_SVTT                                           257
      GO TO  5010                                                         257
5020  CONTINUE                                                            257
C *                                                                       257
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           258
      CALL AGSRESET                                                       258
      AG_MOTHER = 'CAVE'                                                  258
      AG_TITLE = 'SVTT'                                                   258
      AG_EXNAME = 'POSITION'                                              258
      AG_PARLIST = 'MOTHER'                                               258
      CALL AXPOSITION                                                     258
C *                                                                       258
C ----------------------------------------------------------------------  258
C ------------ SVTT                                                       258
      CALL AGSENDM                                                        260
      RETURN                                                              260
C Begin Local Procedure SVTT                                              260
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   260
5010  CONTINUE                                                            260
         AG_TITLE='SVTT'                                                  260
         AG_MODULE=MODULE                                                 260
         AG_MARK='V00'                                                    260
         CALL AGSPUSH                                                     260
         AG_MODULE=MODULE                                                 260
C    *                                                                    260
         RSIZEMAX=SSUP_CON4IDMX                                           261
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 262
     *   2.0*SSUP_HOSRMX                                                  262
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                263
         CLEARENCE=SVTG_RSIZEMAX - RSIZEMAX                               264
C    Geant MATERIAL : AIR ===> inherited arguments used <===              265
         CALL AGSRESET                                                    265
         AG_TITLE = 'AIR'                                                 265
         AG_EXNAME = 'MATERIAL'                                           265
         AG_PARLIST = 'NONE'                                              265
         CALL AXMATERIAL                                                  265
C    *                                                                    265
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       265
         CALL AGSRESET                                                    266
         AG_SEEN = 0                                                      266
         AG_COLO = 1                                                      266
         AG_TITLE = 'SVTT'                                                266
         AG_EXNAME = 'ATTRIBUTE'                                          266
         AG_PARLIST = 'SEEN_COLO'                                         266
         CALL AXATTRIBUTE                                                 266
C    *                                                                    266
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  266
C    VTG_ZSIZEMAX <===                                                    266
         CALL AGSRESET                                                    268
         AG_RMIN = SVTG_RSIZEMIN                                          268
         AG_RMAX = SVTG_RSIZEMAX                                          268
         AG_DZ = SVTG_ZSIZEMAX                                            268
         AG_TITLE = 'TUBE'                                                268
         AG_EXNAME = 'SHAPE'                                              268
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      268
         CALL AXSHAPE                                                     268
C    *                                                                    268
C    Create SIRP :                                                        269
C    Call To Local Procedure SIRP                                         269
         ASSIGN 5040 TO BLOCK_SIRP                                        269
         GO TO  5030                                                      269
5040     CONTINUE                                                         269
C    *                                                                    269
C    Geant POSITION : SIRP ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  270
C    2.5 <===                                                             270
         CALL AGSRESET                                                    270
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             270
         AG_ALPHAN=AG_ALPHAN+1                                            270
         AG_ALPHAK(AG_ALPHAN)=3                                           270
         AG_ALPHAL(AG_ALPHAN)=22.5                                        270
         AG_TITLE = 'SIRP'                                                270
         AG_EXNAME = 'POSITION'                                           270
         AG_PARLIST = 'Z_ALPHAZ'                                          270
         CALL AXPOSITION                                                  270
C    *                                                                    270
C    Geant POSITION : SIRP ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  270
C    22.5 <===                                                            270
         CALL AGSRESET                                                    271
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            271
         AG_ALPHAN=AG_ALPHAN+1                                            271
         AG_ALPHAK(AG_ALPHAN)=3                                           271
         AG_ALPHAL(AG_ALPHAN)=22.5                                        271
         AG_TITLE = 'SIRP'                                                271
         AG_EXNAME = 'POSITION'                                           271
         AG_PARLIST = 'Z_ALPHAZ'                                          271
         CALL AXPOSITION                                                  271
C    *                                                                    271
C    Create SIRT :                                                        272
C    Call To Local Procedure SIRT                                         272
         ASSIGN 5060 TO BLOCK_SIRT                                        272
         GO TO  5050                                                      272
5060     CONTINUE                                                         272
C    *                                                                    272
C    Geant POSITION : SIRT ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 ALPHAZ=2  273
C    2.5 <===                                                             273
         CALL AGSRESET                                                    273
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             273
         AG_ALPHAN=AG_ALPHAN+1                                            273
         AG_ALPHAK(AG_ALPHAN)=3                                           273
         AG_ALPHAL(AG_ALPHAN)=22.5                                        273
         AG_TITLE = 'SIRT'                                                273
         AG_EXNAME = 'POSITION'                                           273
         AG_PARLIST = 'Z_ALPHAZ'                                          273
         CALL AXPOSITION                                                  273
C    *                                                                    273
C    Geant POSITION : SIRT ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 ALPHAZ=  273
C    22.5 <===                                                            273
         CALL AGSRESET                                                    274
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            274
         AG_ALPHAN=AG_ALPHAN+1                                            274
         AG_ALPHAK(AG_ALPHAN)=3                                           274
         AG_ALPHAL(AG_ALPHAN)=22.5                                        274
         AG_TITLE = 'SIRT'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'Z_ALPHAZ'                                          274
         CALL AXPOSITION                                                  274
C    *                                                                    274
C    Create SOER :                                                        275
C    Call To Local Procedure SOER                                         275
         ASSIGN 5080 TO BLOCK_SOER                                        275
         GO TO  5070                                                      275
5080     CONTINUE                                                         275
C    *                                                                    275
C    Geant POSITION : SOER ===> Z=SERG_ENDRNGZM+SERG_ENDRNGTH/2 <===      276
         CALL AGSRESET                                                    276
         AG_Z = SERG_ENDRNGZM+SERG_ENDRNGTH/2                             276
         AG_TITLE = 'SOER'                                                276
         AG_EXNAME = 'POSITION'                                           276
         AG_PARLIST = 'Z'                                                 276
         CALL AXPOSITION                                                  276
C    *                                                                    276
C    Geant POSITION : SOER ===> Z=-SERG_ENDRNGZM-SERG_ENDRNGTH/2 <===     276
         CALL AGSRESET                                                    277
         AG_Z = -SERG_ENDRNGZM-SERG_ENDRNGTH/2                            277
         AG_TITLE = 'SOER'                                                277
         AG_EXNAME = 'POSITION'                                           277
         AG_PARLIST = 'Z'                                                 277
         CALL AXPOSITION                                                  277
C    *                                                                    277
C    Create SBRG :                                                        278
C    Call To Local Procedure SBRG                                         278
         ASSIGN 5100 TO BLOCK_SBRG                                        278
         GO TO  5090                                                      278
5100     CONTINUE                                                         278
C    *                                                                    278
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  279
C    /2 <===                                                              279
         CALL AGSRESET                                                    279
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  279
         AG_TITLE = 'SBRG'                                                279
         AG_EXNAME = 'POSITION'                                           279
         AG_PARLIST = 'Z'                                                 279
         CALL AXPOSITION                                                  279
C    *                                                                    279
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  279
C    K/2 <===                                                             279
         CALL AGSRESET                                                    280
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 280
         AG_TITLE = 'SBRG'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'Z'                                                 280
         CALL AXPOSITION                                                  280
C    *                                                                    280
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           280
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             282
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      283
C    Create SOES :                                                        284
C    Call To Local Procedure SOES                                         284
         ASSIGN 5120 TO BLOCK_SOES                                        284
         GO TO  5110                                                      284
5120     CONTINUE                                                         284
C    *                                                                    284
C    Geant POSITION : SOES ===> Z=SCREW_Z <===                            285
         CALL AGSRESET                                                    285
         AG_Z = SCREW_Z                                                   285
         AG_TITLE = 'SOES'                                                285
         AG_EXNAME = 'POSITION'                                           285
         AG_PARLIST = 'Z'                                                 285
         CALL AXPOSITION                                                  285
C    *                                                                    285
C    Geant POSITION : SOES ===> Z=-SCREW_Z <===                           285
         CALL AGSRESET                                                    286
         AG_Z = -SCREW_Z                                                  286
         AG_TITLE = 'SOES'                                                286
         AG_EXNAME = 'POSITION'                                           286
         AG_PARLIST = 'Z'                                                 286
         CALL AXPOSITION                                                  286
C    *                                                                    286
C    Create SIES :                                                        287
C    Call To Local Procedure SIES                                         287
         ASSIGN 5140 TO BLOCK_SIES                                        287
         GO TO  5130                                                      287
5140     CONTINUE                                                         287
C    *                                                                    287
C    Geant POSITION : SIES ===> Z=SCREW_Z <===                            288
         CALL AGSRESET                                                    288
         AG_Z = SCREW_Z                                                   288
         AG_TITLE = 'SIES'                                                288
         AG_EXNAME = 'POSITION'                                           288
         AG_PARLIST = 'Z'                                                 288
         CALL AXPOSITION                                                  288
C    *                                                                    288
C    Geant POSITION : SIES ===> Z=-SCREW_Z <===                           288
         CALL AGSRESET                                                    289
         AG_Z = -SCREW_Z                                                  289
         AG_TITLE = 'SIES'                                                289
         AG_EXNAME = 'POSITION'                                           289
         AG_PARLIST = 'Z'                                                 289
         CALL AXPOSITION                                                  289
C    *                                                                    289
C    Check SWAM_LEN>0                                                     289
         IF (SWAM_LEN.GT.0) THEN                                          290
C    Create SWMM :                                                        291
C    Call To Local Procedure SWMM                                         291
         ASSIGN 5160 TO BLOCK_SWMM                                        291
         GO TO  5150                                                      291
5160     CONTINUE                                                         291
C    *                                                                    291
C    Geant POSITION : SWMM ===> Z=SWAM_ZMIN+SWAM_LEN/2 <===               292
         CALL AGSRESET                                                    292
         AG_Z = SWAM_ZMIN+SWAM_LEN/2                                      292
         AG_TITLE = 'SWMM'                                                292
         AG_EXNAME = 'POSITION'                                           292
         AG_PARLIST = 'Z'                                                 292
         CALL AXPOSITION                                                  292
C    *                                                                    292
C    Geant POSITION : SWMM ===> Z=-SWAM_ZMIN-SWAM_LEN/2 <===              292
         CALL AGSRESET                                                    293
         AG_Z = -SWAM_ZMIN-SWAM_LEN/2                                     293
         AG_TITLE = 'SWMM'                                                293
         AG_EXNAME = 'POSITION'                                           293
         AG_PARLIST = 'Z'                                                 293
         CALL AXPOSITION                                                  293
C    *                                                                    293
         ENDIF                                                            293
C    Check SWAM_LEN>0                                                     295
         IF (SWAM_LEN.GT.0) THEN                                          295
C    Create SBWC :                                                        296
C    Call To Local Procedure SBWC                                         296
         ASSIGN 5180 TO BLOCK_SBWC                                        296
         GO TO  5170                                                      296
5180     CONTINUE                                                         296
C    *                                                                    296
C    Geant POSITION : SBWC ===> Z=(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SW  297
C    AM_ZMIN+SWAM_LEN))/2) <===                                           297
         CALL AGSRESET                                                    297
         AG_Z =                                                           297
     *   (SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)      297
         AG_TITLE = 'SBWC'                                                297
         AG_EXNAME = 'POSITION'                                           297
         AG_PARLIST = 'Z'                                                 297
         CALL AXPOSITION                                                  297
C    *                                                                    297
C    Geant POSITION : SBWC ===> Z=-(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(S  297
C    WAM_ZMIN+SWAM_LEN))/2) THETAZ=180 <===                               297
         CALL AGSRESET                                                    299
         AG_Z =                                                           299
     *   -(SWAM_ZMIN+SWAM_LEN+(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2)     299
         AG_THETAZ = 180                                                  299
         AG_TITLE = 'SBWC'                                                299
         AG_EXNAME = 'POSITION'                                           299
         AG_PARLIST = 'Z_THETAZ'                                          299
         CALL AXPOSITION                                                  299
C    *                                                                    299
         ENDIF                                                            299
C    Create SCON :                                                        301
C    Call To Local Procedure SCON                                         301
         ASSIGN 5200 TO BLOCK_SCON                                        301
         GO TO  5190                                                      301
5200     CONTINUE                                                         301
C    *                                                                    301
C    Geant POSITION : SCON ===> inherited arguments used <===             302
         CALL AGSRESET                                                    302
         AG_TITLE = 'SCON'                                                302
         AG_EXNAME = 'POSITION'                                           302
         AG_PARLIST = 'NONE'                                              302
         CALL AXPOSITION                                                  302
C    *                                                                    302
C    Geant POSITION : SCON ===> THETAZ=180 <===                           302
         CALL AGSRESET                                                    303
         AG_THETAZ = 180                                                  303
         AG_TITLE = 'SCON'                                                303
         AG_EXNAME = 'POSITION'                                           303
         AG_PARLIST = 'THETAZ'                                            303
         CALL AXPOSITION                                                  303
C    *                                                                    303
C    Create SBSP :                                                        304
C    Call To Local Procedure SBSP                                         304
         ASSIGN 5220 TO BLOCK_SBSP                                        304
         GO TO  5210                                                      304
5220     CONTINUE                                                         304
C    *                                                                    304
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     305
         CALL AGSRESET                                                    305
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            305
         AG_TITLE = 'SBSP'                                                305
         AG_EXNAME = 'POSITION'                                           305
         AG_PARLIST = 'Z'                                                 305
         CALL AXPOSITION                                                  305
C    *                                                                    305
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  305
C    =180 <===                                                            305
         CALL AGSRESET                                                    306
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           306
         AG_THETAZ = 180                                                  306
         AG_TITLE = 'SBSP'                                                306
         AG_EXNAME = 'POSITION'                                           306
         AG_PARLIST = 'Z_THETAZ'                                          306
         CALL AXPOSITION                                                  306
C    *                                                                    306
C    Create SROD :                                                        307
C    Call To Local Procedure SROD                                         307
         ASSIGN 5240 TO BLOCK_SROD                                        307
         GO TO  5230                                                      307
5240     CONTINUE                                                         307
C    *                                                                    307
C    Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===          308
         CALL AGSRESET                                                    308
         AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                 308
         AG_TITLE = 'SROD'                                                308
         AG_EXNAME = 'POSITION'                                           308
         AG_PARLIST = 'Y'                                                 308
         CALL AXPOSITION                                                  308
C    *                                                                    308
C    Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===         308
         CALL AGSRESET                                                    309
         AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                309
         AG_TITLE = 'SROD'                                                309
         AG_EXNAME = 'POSITION'                                           309
         AG_PARLIST = 'Y'                                                 309
         CALL AXPOSITION                                                  309
C    *                                                                    309
         RADMAX=SVTG_RSIZEMAX                                             309
C    *                                                                    311
         ILAYER=1                                                         311
            GO TO 5253                                                    311
5251        ILAYER=ILAYER+1                                               311
5253        IF(ILAYER-(MIN(6,SVTG_NLAYER)).GT.0) GO TO 5252               311
C       Check ILAYER<6                                                    312
            IF (ILAYER.LT.6) THEN                                         312
C       use SVTL bank                                                     313
            BPATH_SVTL='SVTL*'                                            313
            IDNUM_SVTL(1)=0                                               313
            SVTL_LAYER=ILAYER+1                                           313
            CALL AGDGETP('SVTTGEO','*',-999,'SVTL',NSVTL_LAYER,           313
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    313
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             313
            RADMAX=SVTL_RADIUS                                            314
            ELSE                                                          315
            RADMAX=SVTG_RSIZEMAX                                          316
            ENDIF                                                         317
C       use SVTL bank                                                     318
            BPATH_SVTL='SVTL*'                                            318
            IDNUM_SVTL(1)=0                                               318
            SVTL_LAYER=ILAYER                                             318
            CALL AGDGETP('SVTTGEO','*',-999,'SVTL',NSVTL_LAYER,           318
     *      RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,    318
     *      BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)             318
C       Create SLYD :                                                     319
C       Call To Local Procedure SLYD                                      319
            ASSIGN 5270 TO BLOCK_SLYD                                     319
            GO TO  5260                                                   319
5270        CONTINUE                                                      319
C       *                                                                 319
C       Geant POSITION : SLYD ===> inherited arguments used <===          320
            CALL AGSRESET                                                 320
            AG_TITLE = 'SLYD'                                             320
            AG_EXNAME = 'POSITION'                                        320
            AG_PARLIST = 'NONE'                                           320
            CALL AXPOSITION                                               320
C       *                                                                 320
         GO TO 5251                                                       321
5252     CONTINUE                                                         321
C    Check SVTG_NLAYER<0                                                  322
         IF (SVTG_NLAYER.LT.0) THEN                                       322
C    use SVTL bank                                                        323
         BPATH_SVTL='SVTL*'                                               323
         IDNUM_SVTL(1)=0                                                  323
         SVTL_LAYER=-SVTG_NLAYER                                          323
         CALL AGDGETP('SVTTGEO','*',-999,'SVTL',NSVTL_LAYER,              323
     *   RE_SVTL_LAYER,BPATH_SVTL,IDNUM_SVTL,1,MAP_SVTL,NAMES_SVTL,       323
     *   BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                323
C    Create SLYD :                                                        324
C    Call To Local Procedure SLYD                                         324
         ASSIGN 5280 TO BLOCK_SLYD                                        324
         GO TO  5260                                                      324
5280     CONTINUE                                                         324
C    *                                                                    324
C    Geant POSITION : SLYD ===> inherited arguments used <===             325
         CALL AGSRESET                                                    325
         AG_TITLE = 'SLYD'                                                325
         AG_EXNAME = 'POSITION'                                           325
         AG_PARLIST = 'NONE'                                              325
         CALL AXPOSITION                                                  325
C    *                                                                    325
         ENDIF                                                            325
C    Check SVTG_NLAYER>6                                                  327
         IF (SVTG_NLAYER.GT.6) THEN                                       327
C    Create SFMO :                                                        328
C    Call To Local Procedure SFMO                                         328
         ASSIGN 5300 TO BLOCK_SFMO                                        328
         GO TO  5290                                                      328
5300     CONTINUE                                                         328
C    *                                                                    328
C    Geant POSITION : SFMO ===> inherited arguments used <===             329
         CALL AGSRESET                                                    329
         AG_TITLE = 'SFMO'                                                329
         AG_EXNAME = 'POSITION'                                           329
         AG_PARLIST = 'NONE'                                              329
         CALL AXPOSITION                                                  329
C    *                                                                    329
         ENDIF                                                            329
C    Create SISH :                                                        331
C    Call To Local Procedure SISH                                         331
         ASSIGN 5320 TO BLOCK_SISH                                        331
         GO TO  5310                                                      331
5320     CONTINUE                                                         331
C    *                                                                    331
C    Geant POSITION : SISH ===> inherited arguments used <===             332
         CALL AGSRESET                                                    332
         AG_TITLE = 'SISH'                                                332
         AG_EXNAME = 'POSITION'                                           332
         AG_PARLIST = 'NONE'                                              332
         CALL AXPOSITION                                                  332
C    *                                                                    332
C    Create SOSH :                                                        333
C    Call To Local Procedure SOSH                                         333
         ASSIGN 5340 TO BLOCK_SOSH                                        333
         GO TO  5330                                                      333
5340     CONTINUE                                                         333
C    *                                                                    333
C    Geant POSITION : SOSH ===> inherited arguments used <===             334
         CALL AGSRESET                                                    334
         AG_TITLE = 'SOSH'                                                334
         AG_EXNAME = 'POSITION'                                           334
         AG_PARLIST = 'NONE'                                              334
         CALL AXPOSITION                                                  334
C    *                                                                    334
C    Check SVTG_NLAYER>6                                                  334
         IF (SVTG_NLAYER.GT.6) THEN                                       335
C    Create SSSH :                                                        336
C    Call To Local Procedure SSSH                                         336
         ASSIGN 5360 TO BLOCK_SSSH                                        336
         GO TO  5350                                                      336
5360     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : SSSH ===> inherited arguments used <===             337
         CALL AGSRESET                                                    337
         AG_TITLE = 'SSSH'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'NONE'                                              337
         CALL AXPOSITION                                                  337
C    *                                                                    337
         ENDIF                                                            337
C    Create SALM :                                                        339
C    Call To Local Procedure SALM                                         339
         ASSIGN 5380 TO BLOCK_SALM                                        339
         GO TO  5370                                                      339
5380     CONTINUE                                                         339
C    *                                                                    339
C    Geant POSITION : SALM ===> Z=SSLD_ALMSHPOS-SSLD_ALMSHTHK/2 <===      340
         CALL AGSRESET                                                    340
         AG_Z = SSLD_ALMSHPOS-SSLD_ALMSHTHK/2                             340
         AG_TITLE = 'SALM'                                                340
         AG_EXNAME = 'POSITION'                                           340
         AG_PARLIST = 'Z'                                                 340
         CALL AXPOSITION                                                  340
C    *                                                                    340
C    Geant POSITION : SALM ===> Z=-(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2) <===   340
         CALL AGSRESET                                                    341
         AG_Z = -(SSLD_ALMSHPOS-SSLD_ALMSHTHK/2)                          341
         AG_TITLE = 'SALM'                                                341
         AG_EXNAME = 'POSITION'                                           341
         AG_PARLIST = 'Z'                                                 341
         CALL AXPOSITION                                                  341
C    *                                                                    341
5011  CALL AGSPOPB                                                        342
      GO TO BLOCK_SVTT, (5010 ,5020)                                      342
C End of Local Procedure                                                  342
C ----------------------------------------------------------------------  343
C ------------ SLYD                                                       343
C Begin Local Procedure SLYD                                              344
C SLYD IS A SINGLE SVT LAYER                                              344
5260  CONTINUE                                                            344
         AG_TITLE='SLYD'                                                  344
         AG_MODULE=MODULE                                                 344
         AG_MARK='V00'                                                    344
         CALL AGSPUSH                                                     344
         AG_MODULE=MODULE                                                 344
C    *                                                                    344
         LADTHK = SWCA_WAFERTHK/2 + SWCA_ROHATHK + SWCA_WAFCARTH          345
         ELETHK=2.0*SELC_BETHK + SELC_WATTHK + SELC_BEOTHK +              346
     *   SELC_AGPDTHK                                                     346
         ELETHK=ELETHK + SELC_GLASSTHK + SELC_DYETHK                      347
         DEG=180.0/SVTL_NLADDER                                           348
         RAD=PI/SVTL_NLADDER                                              349
C    Geant MATERIAL : AIR ===> inherited arguments used <===              350
         CALL AGSRESET                                                    350
         AG_TITLE = 'AIR'                                                 350
         AG_EXNAME = 'MATERIAL'                                           350
         AG_PARLIST = 'NONE'                                              350
         CALL AXMATERIAL                                                  350
C    *                                                                    350
C    Geant ATTRIBUTE : SLYD ===> SEEN=0 COLO=1 <===                       350
         CALL AGSRESET                                                    351
         AG_SEEN = 0                                                      351
         AG_COLO = 1                                                      351
         AG_TITLE = 'SLYD'                                                351
         AG_EXNAME = 'ATTRIBUTE'                                          351
         AG_PARLIST = 'SEEN_COLO'                                         351
         CALL AXATTRIBUTE                                                 351
C    *                                                                    351
         RMIN=SVTL_RADIUS - LADTHK                                        351
         RMAX = SVTL_RADIUS*SVTL_RADIUS                                   353
         RMAX=RMAX+SWCA_WAFERWID*SWCA_WAFERWID/4.0                        354
         RMAX = SQRT(RMAX) + ELETHK                                       355
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   356
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    357
         CALL AGSRESET                                                    357
         AG_RMIN = RMIN                                                   357
         AG_RMAX = RMAX                                                   357
         AG_DZ = SWCA_LENGTH/2                                            357
         AG_TITLE = 'TUBE'                                                357
         AG_EXNAME = 'SHAPE'                                              357
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      357
         CALL AXSHAPE                                                     357
C    *                                                                    357
C    Create SLSD :                                                        358
C    Call To Local Procedure SLSD                                         358
         ASSIGN 5400 TO BLOCK_SLSD                                        358
         GO TO  5390                                                      358
5400     CONTINUE                                                         358
C    *                                                                    358
5261  CALL AGSPOPB                                                        359
      GO TO BLOCK_SLYD, (5260 ,5270,5280)                                 359
C End of Local Procedure                                                  359
C ----------------------------------------------------------------------  360
C ------------ SLSD                                                       360
C Begin Local Procedure SLSD                                              361
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         361
5390  CONTINUE                                                            361
         AG_TITLE='SLSD'                                                  361
         AG_MODULE=MODULE                                                 361
         AG_MARK='V00'                                                    361
         CALL AGSPUSH                                                     361
         AG_MODULE=MODULE                                                 361
C    *                                                                    361
C    Geant ATTRIBUTE : SLSD ===> SEEN=0 COLO=1 <===                       362
         CALL AGSRESET                                                    362
         AG_SEEN = 0                                                      362
         AG_COLO = 1                                                      362
         AG_TITLE = 'SLSD'                                                362
         AG_EXNAME = 'ATTRIBUTE'                                          362
         AG_PARLIST = 'SEEN_COLO'                                         362
         CALL AXATTRIBUTE                                                 362
C    *                                                                    362
C    Check SVTG_NLAYER<0                                                  362
         IF (SVTG_NLAYER.LT.0) THEN                                       363
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               364
         CALL AGSRESET                                                    364
         AG_NDIV = 1                                                      364
         AG_IAXIS = 2                                                     364
         AG_C0 = -90                                                      364
         AG_TITLE = 'DIVISION'                                            364
         AG_EXNAME = 'SHAPE'                                              364
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     364
         CALL AXSHAPE                                                     364
C    *                                                                    364
         ELSE                                                             364
C    Check SVTG_VERSION==1                                                366
         IF (SVTG_VERSION.EQ.1) THEN                                      366
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              367
         ELSE                                                             368
         C0=90-DEG*MOD(ILAYER,2)                                          369
         ENDIF                                                            370
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NLADDER IAXIS=2, C0=C0 <===    372
         CALL AGSRESET                                                    372
         AG_NDIV = SVTL_NLADDER                                           372
         AG_IAXIS = 2                                                     372
         AG_C0 = C0                                                       372
         AG_TITLE = 'DIVISION'                                            372
         AG_EXNAME = 'SHAPE'                                              372
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     372
         CALL AXSHAPE                                                     372
C    *                                                                    372
         ENDIF                                                            372
C    Create SLDI :                                                        374
C    Call To Local Procedure SLDI                                         374
         ASSIGN 5420 TO BLOCK_SLDI                                        374
         GO TO  5410                                                      374
5420     CONTINUE                                                         374
C    *                                                                    374
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ORT=YZX <===                375
         CALL AGSRESET                                                    375
         AG_X = SVTL_RADIUS                                               375
         AG_ALPHAN=AG_ALPHAN+1                                            375
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 375
         AG_TITLE = 'SLDI'                                                375
         AG_EXNAME = 'POSITION'                                           375
         AG_PARLIST = 'X_ORT'                                             375
         CALL AXPOSITION                                                  375
C    *                                                                    375
C    Create SELE :                                                        376
C    Call To Local Procedure SELE                                         376
         ASSIGN 5440 TO BLOCK_SELE                                        376
         GO TO  5430                                                      376
5440     CONTINUE                                                         376
C    *                                                                    376
         DEG=180.0/SVTL_NLADDER                                           377
         RAD=(TWOPI/2)/SVTL_NLADDER                                       378
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 379
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 380
C    *                                                                    381
         S=-1                                                             381
            GOTO 5453                                                     381
5451        S=S+(2)                                                       381
5453        IF((2)*(S-(1)).GT.0) GO TO 5452                               381
            SIDE=S                                                        382
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  384
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=S*DEG <===         384
            CALL AGSRESET                                                 384
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                384
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               384
            AG_ALPHAN=AG_ALPHAN+1                                         384
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              384
            AG_ALPHAN=AG_ALPHAN+1                                         384
            AG_ALPHAK(AG_ALPHAN)=1                                        384
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 384
            AG_ALPHAN=AG_ALPHAN+1                                         384
            AG_ALPHAK(AG_ALPHAN)=3                                        384
            AG_ALPHAL(AG_ALPHAN)=S*DEG                                    384
            AG_TITLE = 'SELE'                                             384
            AG_EXNAME = 'POSITION'                                        384
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          384
            CALL AXPOSITION                                               384
C       *                                                                 384
         GO TO 5451                                                       385
5452     CONTINUE                                                         385
5391  CALL AGSPOPB                                                        386
      GO TO BLOCK_SLSD, (5390 ,5400)                                      386
C End of Local Procedure                                                  386
C ----------------------------------------------------------------------  387
C ------------ SLDI                                                       387
C Begin Local Procedure SLDI                                              388
C SLDI IS A LADDER VOLUME                                                 388
5410  CONTINUE                                                            388
         AG_TITLE='SLDI'                                                  388
         AG_MODULE=MODULE                                                 388
         AG_MARK='V00'                                                    388
         CALL AGSPUSH                                                     388
         AG_MODULE=MODULE                                                 388
C    *                                                                    388
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       389
C    Geant MATERIAL : AIR ===> inherited arguments used <===              390
         CALL AGSRESET                                                    390
         AG_TITLE = 'AIR'                                                 390
         AG_EXNAME = 'MATERIAL'                                           390
         AG_PARLIST = 'NONE'                                              390
         CALL AXMATERIAL                                                  390
C    *                                                                    390
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 SERIAL=SVTL_NWAFER <===    390
         CALL AGSRESET                                                    391
         AG_SEEN = 0                                                      391
         AG_COLO = 1                                                      391
         AG_SERIAL = SVTL_NWAFER                                          391
         AG_TITLE = 'SLDI'                                                391
         AG_EXNAME = 'ATTRIBUTE'                                          391
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  391
         CALL AXATTRIBUTE                                                 391
C    *                                                                    391
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  391
C    HK <===                                                              391
         CALL AGSRESET                                                    393
         AG_DX = SWCA_WAFERWID/2                                          393
         AG_DY = SWCA_LENGTH/2                                            393
         AG_DZ = LADTHK                                                   393
         AG_TITLE = 'BOX'                                                 393
         AG_EXNAME = 'SHAPE'                                              393
         AG_PARLIST = 'DX_DY_DZ'                                          393
         CALL AXSHAPE                                                     393
C    *                                                                    393
C    Create STLI :                                                        394
C    Call To Local Procedure STLI                                         394
         ASSIGN 5470 TO BLOCK_STLI                                        394
         GO TO  5460                                                      394
5470     CONTINUE                                                         394
C    *                                                                    394
C    Geant POSITION : STLI ===> inherited arguments used <===             395
         CALL AGSRESET                                                    395
         AG_TITLE = 'STLI'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'NONE'                                              395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Create SBER :                                                        396
C    Call To Local Procedure SBER                                         396
         ASSIGN 5490 TO BLOCK_SBER                                        396
         GO TO  5480                                                      396
5490     CONTINUE                                                         396
C    *                                                                    396
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  397
C    DTHK+SWCA_WAFCARTH/2 <===                                            397
         CALL AGSRESET                                                    397
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          397
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   397
         AG_TITLE = 'SBER'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'X_Z'                                               397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  397
C    DTHK+SWCA_WAFCARTH/2 <===                                            397
         CALL AGSRESET                                                    398
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          398
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   398
         AG_TITLE = 'SBER'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'X_Z'                                               398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Create STAB :                                                        399
C    Call To Local Procedure STAB                                         399
         ASSIGN 5510 TO BLOCK_STAB                                        399
         GO TO  5500                                                      399
5510     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  400
C    WAFCARTH/2 <===                                                      400
         CALL AGSRESET                                                    400
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    400
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   400
         AG_TITLE = 'STAB'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'Y_Z'                                               400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  400
C    _WAFCARTH/2 <===                                                     400
         CALL AGSRESET                                                    401
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   401
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   401
         AG_TITLE = 'STAB'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'Y_Z'                                               401
         CALL AXPOSITION                                                  401
C    *                                                                    401
C    Create STRU :                                                        402
C    Call To Local Procedure STRU                                         402
         ASSIGN 5530 TO BLOCK_STRU                                        402
         GO TO  5520                                                      402
5530     CONTINUE                                                         402
C    *                                                                    402
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  405
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  405
C    =                                                                    405
         CALL AGSRESET                                                    405
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              405
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  405
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   405
         AG_TITLE = 'STRU'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'Y_Z'                                               405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  405
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  405
C    ==                                                                   405
         CALL AGSRESET                                                    408
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             408
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  408
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   408
         AG_TITLE = 'STRU'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'Y_Z'                                               408
         CALL AXPOSITION                                                  408
C    *                                                                    408
C    Create SRHC :                                                        409
C    Call To Local Procedure SRHC                                         409
         ASSIGN 5550 TO BLOCK_SRHC                                        409
         GO TO  5540                                                      409
5550     CONTINUE                                                         409
C    *                                                                    409
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  411
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         411
         CALL AGSRESET                                                    411
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          411
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                411
         AG_TITLE = 'SRHC'                                                411
         AG_EXNAME = 'POSITION'                                           411
         AG_PARLIST = 'X_Z'                                               411
         CALL AXPOSITION                                                  411
C    *                                                                    411
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  411
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         411
         CALL AGSRESET                                                    413
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          413
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                413
         AG_TITLE = 'SRHC'                                                413
         AG_EXNAME = 'POSITION'                                           413
         AG_PARLIST = 'X_Z'                                               413
         CALL AXPOSITION                                                  413
C    *                                                                    413
5411  CALL AGSPOPB                                                        414
      GO TO BLOCK_SLDI, (5410 ,5420)                                      414
C End of Local Procedure                                                  414
C ----------------------------------------------------------------------  415
C ------------ SRHC                                                       415
C Begin Local Procedure SRHC                                              416
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     416
5540  CONTINUE                                                            416
         AG_TITLE='SRHC'                                                  416
         AG_MODULE=MODULE                                                 416
         AG_MARK='V00'                                                    416
         CALL AGSPUSH                                                     416
         AG_MODULE=MODULE                                                 416
C    *                                                                    416
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           417
         CALL AGSRESET                                                    417
         AG_A = 12                                                        417
         AG_Z = 6                                                         417
         AG_W = 1                                                         417
         AG_TITLE = 'C'                                                   417
         AG_EXNAME = 'COMPONENT'                                          417
         AG_PARLIST = 'A_Z_W'                                             417
         CALL AXCOMPONENT                                                 417
C    *                                                                    417
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           417
         CALL AGSRESET                                                    418
         AG_A = 1                                                         418
         AG_Z = 1                                                         418
         AG_W = 2                                                         418
         AG_TITLE = 'H2'                                                  418
         AG_EXNAME = 'COMPONENT'                                          418
         AG_PARLIST = 'A_Z_W'                                             418
         CALL AXCOMPONENT                                                 418
C    *                                                                    418
C    Geant MIXTURE : ROHA ===> DENS=0.0304 <===                           418
         CALL AGSRESET                                                    419
         AG_DENS = 0.0304                                                 419
         AG_TITLE = 'ROHA'                                                419
         AG_EXNAME = 'MIXTURE'                                            419
         AG_PARLIST = 'DENS'                                              419
         CALL AXMIXTURE                                                   419
C    *                                                                    419
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       419
         CALL AGSRESET                                                    420
         AG_SEEN = 1                                                      420
         AG_COLO = 3                                                      420
         AG_TITLE = 'SRHC'                                                420
         AG_EXNAME = 'ATTRIBUTE'                                          420
         AG_PARLIST = 'SEEN_COLO'                                         420
         CALL AXATTRIBUTE                                                 420
C    *                                                                    420
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  420
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        420
         CALL AGSRESET                                                    422
         AG_DX = SWCA_WAFCARWD/2                                          422
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              422
         AG_DZ = SWCA_ROHATHK/2                                           422
         AG_TITLE = 'BOX'                                                 422
         AG_EXNAME = 'SHAPE'                                              422
         AG_PARLIST = 'DX_DY_DZ'                                          422
         CALL AXSHAPE                                                     422
C    *                                                                    422
5541  CALL AGSPOPB                                                        423
      GO TO BLOCK_SRHC, (5540 ,5550)                                      423
C End of Local Procedure                                                  423
C ----------------------------------------------------------------------  424
C ------------ STLI                                                       424
C Begin Local Procedure STLI                                              425
C STLI IS THE WAVER PACK CONTAINER                                        425
5460  CONTINUE                                                            425
         AG_TITLE='STLI'                                                  425
         AG_MODULE=MODULE                                                 425
         AG_MARK='V00'                                                    425
         CALL AGSPUSH                                                     425
         AG_MODULE=MODULE                                                 425
C    *                                                                    425
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              426
         CALL AGSRESET                                                    426
         AG_SEEN = 0                                                      426
         AG_COLO = 1                                                      426
         AG_SERIAL = 0                                                    426
         AG_TITLE = 'STLI'                                                426
         AG_EXNAME = 'ATTRIBUTE'                                          426
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  426
         CALL AXATTRIBUTE                                                 426
C    *                                                                    426
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  426
C    P), DZ=SWCA_WAFERTHK/2 <===                                          426
         CALL AGSRESET                                                    428
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              428
         AG_DZ = SWCA_WAFERTHK/2                                          428
         AG_TITLE = 'BOX'                                                 428
         AG_EXNAME = 'SHAPE'                                              428
         AG_PARLIST = 'DY_DZ'                                             428
         CALL AXSHAPE                                                     428
C    *                                                                    428
C    Create STSI :                                                        429
C    Call To Local Procedure STSI                                         429
         ASSIGN 5570 TO BLOCK_STSI                                        429
         GO TO  5560                                                      429
5570     CONTINUE                                                         429
C    *                                                                    429
5461  CALL AGSPOPB                                                        430
      GO TO BLOCK_STLI, (5460 ,5470)                                      430
C End of Local Procedure                                                  430
C ----------------------------------------------------------------------  431
C ------------ STSI                                                       431
C Begin Local Procedure STSI                                              432
C STSI IS A SINGLE WAVER CONTAINER                                        432
5560  CONTINUE                                                            432
         AG_TITLE='STSI'                                                  432
         AG_MODULE=MODULE                                                 432
         AG_MARK='V00'                                                    432
         CALL AGSPUSH                                                     432
         AG_MODULE=MODULE                                                 432
C    *                                                                    432
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            433
         CALL AGSRESET                                                    433
         AG_NDIV = SVTL_NWAFER                                            433
         AG_IAXIS = 2                                                     433
         AG_TITLE = 'DIVISION'                                            433
         AG_EXNAME = 'SHAPE'                                              433
         AG_PARLIST = 'NDIV_IAXIS'                                        433
         CALL AXSHAPE                                                     433
C    *                                                                    433
C    Create SVTD :                                                        434
C    Call To Local Procedure SVTD                                         434
         ASSIGN 5590 TO BLOCK_SVTD                                        434
         GO TO  5580                                                      434
5590     CONTINUE                                                         434
C    *                                                                    434
C    Geant POSITION : SVTD ===> inherited arguments used <===             435
         CALL AGSRESET                                                    435
         AG_TITLE = 'SVTD'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'NONE'                                              435
         CALL AXPOSITION                                                  435
C    *                                                                    435
5561  CALL AGSPOPB                                                        436
      GO TO BLOCK_STSI, (5560 ,5570)                                      436
C End of Local Procedure                                                  436
C ----------------------------------------------------------------------  437
C ------------ SVTD                                                       437
C Begin Local Procedure SVTD                                              438
C SVTD IS AN ACTIVE WAFER VOLUME                                          438
5580  CONTINUE                                                            438
         AG_TITLE='SVTD'                                                  438
         AG_MODULE=MODULE                                                 438
         AG_MARK='V00'                                                    438
         CALL AGSPUSH                                                     438
         AG_MODULE=MODULE                                                 438
C    *                                                                    438
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          439
         CALL AGSRESET                                                    439
         AG_TITLE = 'SILICON'                                             439
         AG_EXNAME = 'MATERIAL'                                           439
         AG_PARLIST = 'NONE'                                              439
         CALL AXMATERIAL                                                  439
C    *                                                                    439
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         439
         CALL AGSRESET                                                    440
         AG_ISVOL = 1                                                     440
         AG_TITLE = 'SENSITIVE'                                           440
         AG_EXNAME = 'MATERIAL'                                           440
         AG_PARLIST = 'ISVOL'                                             440
         CALL AXMATERIAL                                                  440
C    *                                                                    440
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       440
         CALL AGSRESET                                                    441
         AG_SEEN = 1                                                      441
         AG_COLO = 4                                                      441
         AG_TITLE = 'SVTD'                                                441
         AG_EXNAME = 'ATTRIBUTE'                                          441
         AG_PARLIST = 'SEEN_COLO'                                         441
         CALL AXATTRIBUTE                                                 441
C    *                                                                    441
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       441
         CALL AGSRESET                                                    442
         AG_DY = SWCA_WAFERLEN/2                                          442
         AG_TITLE = 'BOX'                                                 442
         AG_EXNAME = 'SHAPE'                                              442
         AG_PARLIST = 'DY'                                                442
         CALL AXSHAPE                                                     442
C    *                                                                    442
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 442
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  445
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  445
C    .,0.01)                                                              445
         AG_TITLE = 'SVTD'                                                445
         AG_EXNAME = 'HITS'                                               445
         AG_TYPE=AG_MAGIC                                                 445
         CALL AGSDETV('SVTD')                                             445
C    *                                                                    445
         AG_PARLIST='Z'                                                   445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = AG_MAGIC                                              445
         AG_BIN=.001                                                      445
         AG_OPTION='S'                                                    445
         AG_FHMIN = AG_MAGIC                                              445
         AG_FHMAX = AG_MAGIC                                              445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='Y'                                                   445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = AG_MAGIC                                              445
         AG_BIN=.001                                                      445
         AG_FHMIN=AG_MAGIC                                                445
         AG_FHMAX=AG_MAGIC                                                445
         AG_OPTION='*'                                                    445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='X'                                                   445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = AG_MAGIC                                              445
         AG_BIN=.001                                                      445
         AG_FHMIN=AG_MAGIC                                                445
         AG_FHMAX=AG_MAGIC                                                445
         AG_OPTION='*'                                                    445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='PTOT'                                                445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = 18                                                    445
         AG_BIN=AG_MAGIC                                                  445
         AG_OPTION='*'                                                    445
         AG_FHMIN = 0                                                     445
         AG_FHMAX = 100                                                   445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='CX'                                                  445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = 10                                                    445
         AG_BIN=AG_MAGIC                                                  445
         AG_FHMIN=AG_MAGIC                                                445
         AG_FHMAX=AG_MAGIC                                                445
         AG_OPTION='*'                                                    445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='CY'                                                  445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = 10                                                    445
         AG_BIN=AG_MAGIC                                                  445
         AG_FHMIN=AG_MAGIC                                                445
         AG_FHMAX=AG_MAGIC                                                445
         AG_OPTION='*'                                                    445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='CZ'                                                  445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = 10                                                    445
         AG_BIN=AG_MAGIC                                                  445
         AG_FHMIN=AG_MAGIC                                                445
         AG_FHMAX=AG_MAGIC                                                445
         AG_OPTION='*'                                                    445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='SLENG'                                               445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = 16                                                    445
         AG_BIN=AG_MAGIC                                                  445
         AG_OPTION='*'                                                    445
         AG_FHMIN = 0                                                     445
         AG_FHMAX = 500                                                   445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='TOF'                                                 445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = 16                                                    445
         AG_BIN=AG_MAGIC                                                  445
         AG_OPTION='*'                                                    445
         AG_FHMIN = 0                                                     445
         AG_FHMAX = 1.0E-6                                                445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='STEP'                                                445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = AG_MAGIC                                              445
         AG_BIN=.01                                                       445
         AG_FHMIN=AG_MAGIC                                                445
         AG_FHMAX=AG_MAGIC                                                445
         AG_OPTION='*'                                                    445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         AG_PARLIST='ELOSS'                                               445
         AG_TITLE = 'DETH'                                                445
         AG_NBITS = 22                                                    445
         AG_BIN=AG_MAGIC                                                  445
         AG_OPTION='*'                                                    445
         AG_FHMIN = 0.                                                    445
         AG_FHMAX = 0.01                                                  445
         CALL AGSDETH (0)                                                 445
C    *                                                                    445
         CALL AGSDETU('SVTD')                                             445
C    *                                                                    445
5581  CALL AGSPOPB                                                        446
      GO TO BLOCK_SVTD, (5580 ,5590)                                      446
C End of Local Procedure                                                  446
C ----------------------------------------------------------------------  447
C ------------ SBER                                                       447
C Begin Local Procedure SBER                                              448
C SBER ARE THE BERILLIUM WAFER CARRIER RAILS                              448
5480  CONTINUE                                                            448
         AG_TITLE='SBER'                                                  448
         AG_MODULE=MODULE                                                 448
         AG_MARK='V00'                                                    448
         CALL AGSPUSH                                                     448
         AG_MODULE=MODULE                                                 448
C    *                                                                    448
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        449
         CALL AGSRESET                                                    449
         AG_TITLE = 'BERILLIUM'                                           449
         AG_EXNAME = 'MATERIAL'                                           449
         AG_PARLIST = 'NONE'                                              449
         CALL AXMATERIAL                                                  449
C    *                                                                    449
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       449
         CALL AGSRESET                                                    450
         AG_SEEN = 1                                                      450
         AG_COLO = 2                                                      450
         AG_TITLE = 'SBER'                                                450
         AG_EXNAME = 'ATTRIBUTE'                                          450
         AG_PARLIST = 'SEEN_COLO'                                         450
         CALL AXATTRIBUTE                                                 450
C    *                                                                    450
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  450
C    _WAFCARTH/2 <===                                                     450
         CALL AGSRESET                                                    452
         AG_DX = SWCA_WAFCARWD/2                                          452
         AG_DY = SWCA_LENGTH/2                                            452
         AG_DZ = SWCA_WAFCARTH/2                                          452
         AG_TITLE = 'BOX'                                                 452
         AG_EXNAME = 'SHAPE'                                              452
         AG_PARLIST = 'DX_DY_DZ'                                          452
         CALL AXSHAPE                                                     452
C    *                                                                    452
5481  CALL AGSPOPB                                                        453
      GO TO BLOCK_SBER, (5480 ,5490)                                      453
C End of Local Procedure                                                  453
C ----------------------------------------------------------------------  454
C ------------ STAB                                                       454
C Begin Local Procedure STAB                                              455
C STAB ARE THE BERILLIUM WAFER CARRIER END TABS                           455
5500  CONTINUE                                                            455
         AG_TITLE='STAB'                                                  455
         AG_MODULE=MODULE                                                 455
         AG_MARK='V00'                                                    455
         CALL AGSPUSH                                                     455
         AG_MODULE=MODULE                                                 455
C    *                                                                    455
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        456
         CALL AGSRESET                                                    456
         AG_TITLE = 'BERILLIUM'                                           456
         AG_EXNAME = 'MATERIAL'                                           456
         AG_PARLIST = 'NONE'                                              456
         CALL AXMATERIAL                                                  456
C    *                                                                    456
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       456
         CALL AGSRESET                                                    457
         AG_SEEN = 1                                                      457
         AG_COLO = 2                                                      457
         AG_TITLE = 'STAB'                                                457
         AG_EXNAME = 'ATTRIBUTE'                                          457
         AG_PARLIST = 'SEEN_COLO'                                         457
         CALL AXATTRIBUTE                                                 457
C    *                                                                    457
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  457
C    2 DZ=SWCA_WAFCARTH/2 <===                                            457
         CALL AGSRESET                                                    459
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            459
         AG_DY = TABLEN/2                                                 459
         AG_DZ = SWCA_WAFCARTH/2                                          459
         AG_TITLE = 'BOX'                                                 459
         AG_EXNAME = 'SHAPE'                                              459
         AG_PARLIST = 'DX_DY_DZ'                                          459
         CALL AXSHAPE                                                     459
C    *                                                                    459
5501  CALL AGSPOPB                                                        460
      GO TO BLOCK_STAB, (5500 ,5510)                                      460
C End of Local Procedure                                                  460
C ----------------------------------------------------------------------  461
C ------------ STRU                                                       461
C Begin Local Procedure STRU                                              462
C STRU ARE THE BERILLIUM STRUTS BETWEEN THE WAFER CARRIER RAILS           462
5520  CONTINUE                                                            462
         AG_TITLE='STRU'                                                  462
         AG_MODULE=MODULE                                                 462
         AG_MARK='V00'                                                    462
         CALL AGSPUSH                                                     462
         AG_MODULE=MODULE                                                 462
C    *                                                                    462
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        463
         CALL AGSRESET                                                    463
         AG_TITLE = 'BERILLIUM'                                           463
         AG_EXNAME = 'MATERIAL'                                           463
         AG_PARLIST = 'NONE'                                              463
         CALL AXMATERIAL                                                  463
C    *                                                                    463
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       463
         CALL AGSRESET                                                    464
         AG_SEEN = 1                                                      464
         AG_COLO = 2                                                      464
         AG_TITLE = 'STRU'                                                464
         AG_EXNAME = 'ATTRIBUTE'                                          464
         AG_PARLIST = 'SEEN_COLO'                                         464
         CALL AXATTRIBUTE                                                 464
C    *                                                                    464
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  464
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     464
         CALL AGSRESET                                                    466
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            466
         AG_DY = SWCA_STRUTLEN/2                                          466
         AG_DZ = SWCA_WAFCARTH/2                                          466
         AG_TITLE = 'BOX'                                                 466
         AG_EXNAME = 'SHAPE'                                              466
         AG_PARLIST = 'DX_DY_DZ'                                          466
         CALL AXSHAPE                                                     466
C    *                                                                    466
5521  CALL AGSPOPB                                                        467
      GO TO BLOCK_STRU, (5520 ,5530)                                      467
C End of Local Procedure                                                  467
C ----------------------------------------------------------------------  468
C ------------ SELE                                                       468
C Begin Local Procedure SELE                                              469
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   469
5430  CONTINUE                                                            469
         AG_TITLE='SELE'                                                  469
         AG_MODULE=MODULE                                                 469
         AG_MARK='V00'                                                    469
         CALL AGSPUSH                                                     469
         AG_MODULE=MODULE                                                 469
C    *                                                                    469
C    Geant MATERIAL : AIR ===> inherited arguments used <===              470
         CALL AGSRESET                                                    470
         AG_TITLE = 'AIR'                                                 470
         AG_EXNAME = 'MATERIAL'                                           470
         AG_PARLIST = 'NONE'                                              470
         CALL AXMATERIAL                                                  470
C    *                                                                    470
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 <===                       470
         CALL AGSRESET                                                    471
         AG_SEEN = 0                                                      471
         AG_COLO = 1                                                      471
         AG_TITLE = 'SELE'                                                471
         AG_EXNAME = 'ATTRIBUTE'                                          471
         AG_PARLIST = 'SEEN_COLO'                                         471
         CALL AXATTRIBUTE                                                 471
C    *                                                                    471
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  471
C    K/2 <===                                                             471
         CALL AGSRESET                                                    473
         AG_DX = SELC_ELCAWID/2                                           473
         AG_DY = SWCA_LENGTH/2                                            473
         AG_DZ = ELETHK/2                                                 473
         AG_TITLE = 'BOX'                                                 473
         AG_EXNAME = 'SHAPE'                                              473
         AG_PARLIST = 'DX_DY_DZ'                                          473
         CALL AXSHAPE                                                     473
C    *                                                                    473
C    Create SWCH :                                                        474
C    Call To Local Procedure SWCH                                         474
         ASSIGN 5610 TO BLOCK_SWCH                                        474
         GO TO  5600                                                      474
5610     CONTINUE                                                         474
C    *                                                                    474
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             475
         CALL AGSRESET                                                    475
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    475
         AG_TITLE = 'SWCH'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'Z'                                                 475
         CALL AXPOSITION                                                  475
C    *                                                                    475
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  475
C    BETHK/2 <===                                                         475
         CALL AGSRESET                                                    476
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             476
         AG_TITLE = 'SWCH'                                                476
         AG_EXNAME = 'POSITION'                                           476
         AG_PARLIST = 'Z'                                                 476
         CALL AXPOSITION                                                  476
C    *                                                                    476
C    Create SWCS :                                                        477
C    Call To Local Procedure SWCS                                         477
         ASSIGN 5630 TO BLOCK_SWCS                                        477
         GO TO  5620                                                      477
5630     CONTINUE                                                         477
C    *                                                                    477
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  479
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     479
         CALL AGSRESET                                                    479
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              479
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        479
         AG_TITLE = 'SWCS'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Z'                                               479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  479
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     479
         CALL AGSRESET                                                    481
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              481
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        481
         AG_TITLE = 'SWCS'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'X_Z'                                               481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    Create SWCW :                                                        482
C    Call To Local Procedure SWCW                                         482
         ASSIGN 5650 TO BLOCK_SWCW                                        482
         GO TO  5640                                                      482
5650     CONTINUE                                                         482
C    *                                                                    482
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  483
C    =                                                                    483
         CALL AGSRESET                                                    483
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        483
         AG_TITLE = 'SWCW'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'Z'                                                 483
         CALL AXPOSITION                                                  483
C    *                                                                    483
C    Create SBOI :                                                        484
C    Call To Local Procedure SBOI                                         484
         ASSIGN 5670 TO BLOCK_SBOI                                        484
         GO TO  5660                                                      484
5670     CONTINUE                                                         484
C    *                                                                    484
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  485
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       485
         CALL AGSRESET                                                    485
         AG_Z = (ELETHK/                                                  485
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          485
         AG_TITLE = 'SBOI'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'Z'                                                 485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    Create SGLA :                                                        486
C    Call To Local Procedure SGLA                                         486
         ASSIGN 5690 TO BLOCK_SGLA                                        486
         GO TO  5680                                                      486
5690     CONTINUE                                                         486
C    *                                                                    486
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  487
C    _GLASSTHK/2 <===                                                     487
         CALL AGSRESET                                                    487
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         487
         AG_TITLE = 'SGLA'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'Z'                                                 487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Create SAGP :                                                        488
C    Call To Local Procedure SAGP                                         488
         ASSIGN 5710 TO BLOCK_SAGP                                        488
         GO TO  5700                                                      488
5710     CONTINUE                                                         488
C    *                                                                    488
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  489
C    ==                                                                   489
         CALL AGSRESET                                                    489
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       489
         AG_TITLE = 'SAGP'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'Z'                                                 489
         CALL AXPOSITION                                                  489
C    *                                                                    489
C    Create SDYE :                                                        490
C    Call To Local Procedure SDYE                                         490
         ASSIGN 5730 TO BLOCK_SDYE                                        490
         GO TO  5720                                                      490
5730     CONTINUE                                                         490
C    *                                                                    490
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  492
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     492
         CALL AGSRESET                                                    492
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  492
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    492
         AG_TITLE = 'SDYE'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Z'                                               492
         CALL AXPOSITION                                                  492
C    *                                                                    492
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  492
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             492
         CALL AGSRESET                                                    494
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          494
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    494
         AG_TITLE = 'SDYE'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'X_Z'                                               494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Create SECA :                                                        495
C    Call To Local Procedure SECA                                         495
         ASSIGN 5750 TO BLOCK_SECA                                        495
         GO TO  5740                                                      495
5750     CONTINUE                                                         495
C    *                                                                    495
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  497
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    497
         CALL AGSRESET                                                    497
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             497
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        497
         AG_TITLE = 'SECA'                                                497
         AG_EXNAME = 'POSITION'                                           497
         AG_PARLIST = 'X_Z'                                               497
         CALL AXPOSITION                                                  497
C    *                                                                    497
5431  CALL AGSPOPB                                                        498
      GO TO BLOCK_SELE, (5430 ,5440)                                      498
C End of Local Procedure                                                  498
C ----------------------------------------------------------------------  499
C ------------ SDYE                                                       499
C Begin Local Procedure SDYE                                              500
C SDYE IS THE IC CHIP ON THE HYBRID                                       500
5720  CONTINUE                                                            500
         AG_TITLE='SDYE'                                                  500
         AG_MODULE=MODULE                                                 500
         AG_MARK='V00'                                                    500
         CALL AGSPUSH                                                     500
         AG_MODULE=MODULE                                                 500
C    *                                                                    500
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          501
         CALL AGSRESET                                                    501
         AG_TITLE = 'SILICON'                                             501
         AG_EXNAME = 'MATERIAL'                                           501
         AG_PARLIST = 'NONE'                                              501
         CALL AXMATERIAL                                                  501
C    *                                                                    501
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 <===                       501
         CALL AGSRESET                                                    502
         AG_SEEN = 1                                                      502
         AG_COLO = 6                                                      502
         AG_TITLE = 'SDYE'                                                502
         AG_EXNAME = 'ATTRIBUTE'                                          502
         AG_PARLIST = 'SEEN_COLO'                                         502
         CALL AXATTRIBUTE                                                 502
C    *                                                                    502
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  502
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           502
         CALL AGSRESET                                                    504
         AG_DX = SELC_DYEWID/2                                            504
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              504
         AG_DZ = SELC_DYETHK/2                                            504
         AG_TITLE = 'BOX'                                                 504
         AG_EXNAME = 'SHAPE'                                              504
         AG_PARLIST = 'DX_DY_DZ'                                          504
         CALL AXSHAPE                                                     504
C    *                                                                    504
5721  CALL AGSPOPB                                                        505
      GO TO BLOCK_SDYE, (5720 ,5730)                                      505
C End of Local Procedure                                                  505
C ----------------------------------------------------------------------  506
C ------------ SECA                                                       506
C Begin Local Procedure SECA                                              507
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            507
5740  CONTINUE                                                            507
         AG_TITLE='SECA'                                                  507
         AG_MODULE=MODULE                                                 507
         AG_MARK='V00'                                                    507
         CALL AGSPUSH                                                     507
         AG_MODULE=MODULE                                                 507
C    *                                                                    507
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           508
         CALL AGSRESET                                                    508
         AG_TITLE = 'COPPER'                                              508
         AG_EXNAME = 'MATERIAL'                                           508
         AG_PARLIST = 'NONE'                                              508
         CALL AXMATERIAL                                                  508
C    *                                                                    508
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 <===                       508
         CALL AGSRESET                                                    509
         AG_SEEN = 1                                                      509
         AG_COLO = 2                                                      509
         AG_TITLE = 'SECA'                                                509
         AG_EXNAME = 'ATTRIBUTE'                                          509
         AG_PARLIST = 'SEEN_COLO'                                         509
         CALL AXATTRIBUTE                                                 509
C    *                                                                    509
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  509
C    ABTHK/2 <===                                                         509
         CALL AGSRESET                                                    511
         AG_DX = SELC_CABWID/2                                            511
         AG_DY = SWCA_LENGTH/2                                            511
         AG_DZ = SELC_CABTHK/2                                            511
         AG_TITLE = 'BOX'                                                 511
         AG_EXNAME = 'SHAPE'                                              511
         AG_PARLIST = 'DX_DY_DZ'                                          511
         CALL AXSHAPE                                                     511
C    *                                                                    511
5741  CALL AGSPOPB                                                        512
      GO TO BLOCK_SECA, (5740 ,5750)                                      512
C End of Local Procedure                                                  512
C ----------------------------------------------------------------------  513
C ------------ SBOI                                                       513
C Begin Local Procedure SBOI                                              514
C SBOI IS THE BERILLIA LAYER                                              514
5660  CONTINUE                                                            514
         AG_TITLE='SBOI'                                                  514
         AG_MODULE=MODULE                                                 514
         AG_MARK='V00'                                                    514
         CALL AGSPUSH                                                     514
         AG_MODULE=MODULE                                                 514
C    *                                                                    514
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1 <===                           515
         CALL AGSRESET                                                    515
         AG_A = 9                                                         515
         AG_Z = 4                                                         515
         AG_W = 1                                                         515
         AG_TITLE = 'BE'                                                  515
         AG_EXNAME = 'COMPONENT'                                          515
         AG_PARLIST = 'A_Z_W'                                             515
         CALL AXCOMPONENT                                                 515
C    *                                                                    515
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           515
         CALL AGSRESET                                                    516
         AG_A = 16                                                        516
         AG_Z = 8                                                         516
         AG_W = 1                                                         516
         AG_TITLE = 'O'                                                   516
         AG_EXNAME = 'COMPONENT'                                          516
         AG_PARLIST = 'A_Z_W'                                             516
         CALL AXCOMPONENT                                                 516
C    *                                                                    516
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              516
         CALL AGSRESET                                                    517
         AG_DENS = 2.85                                                   517
         AG_TITLE = 'BEO'                                                 517
         AG_EXNAME = 'MIXTURE'                                            517
         AG_PARLIST = 'DENS'                                              517
         CALL AXMIXTURE                                                   517
C    *                                                                    517
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 <===                       517
         CALL AGSRESET                                                    518
         AG_SEEN = 1                                                      518
         AG_COLO = 6                                                      518
         AG_TITLE = 'SBOI'                                                518
         AG_EXNAME = 'ATTRIBUTE'                                          518
         AG_PARLIST = 'SEEN_COLO'                                         518
         CALL AXATTRIBUTE                                                 518
C    *                                                                    518
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  518
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          518
         CALL AGSRESET                                                    520
         AG_DX = SELC_ELCAWID/2                                           520
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              520
         AG_DZ = SELC_BEOTHK/2                                            520
         AG_TITLE = 'BOX'                                                 520
         AG_EXNAME = 'SHAPE'                                              520
         AG_PARLIST = 'DX_DY_DZ'                                          520
         CALL AXSHAPE                                                     520
C    *                                                                    520
5661  CALL AGSPOPB                                                        521
      GO TO BLOCK_SBOI, (5660 ,5670)                                      521
C End of Local Procedure                                                  521
C ----------------------------------------------------------------------  522
C ------------ SAGP                                                       522
C Begin Local Procedure SAGP                                              523
C SAGP IS THE SILVER-PALLADIUM LAYER                                      523
5700  CONTINUE                                                            523
         AG_TITLE='SAGP'                                                  523
         AG_MODULE=MODULE                                                 523
         AG_MARK='V00'                                                    523
         CALL AGSPUSH                                                     523
         AG_MODULE=MODULE                                                 523
C    *                                                                    523
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1 <===                        524
         CALL AGSRESET                                                    524
         AG_A = 108                                                       524
         AG_Z = 47                                                        524
         AG_W = 1                                                         524
         AG_TITLE = 'AG'                                                  524
         AG_EXNAME = 'COMPONENT'                                          524
         AG_PARLIST = 'A_Z_W'                                             524
         CALL AXCOMPONENT                                                 524
C    *                                                                    524
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1 <===                        524
         CALL AGSRESET                                                    525
         AG_A = 106                                                       525
         AG_Z = 46                                                        525
         AG_W = 1                                                         525
         AG_TITLE = 'PD'                                                  525
         AG_EXNAME = 'COMPONENT'                                          525
         AG_PARLIST = 'A_Z_W'                                             525
         CALL AXCOMPONENT                                                 525
C    *                                                                    525
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            525
         CALL AGSRESET                                                    526
         AG_DENS = 11.25                                                  526
         AG_TITLE = 'AGPD'                                                526
         AG_EXNAME = 'MIXTURE'                                            526
         AG_PARLIST = 'DENS'                                              526
         CALL AXMIXTURE                                                   526
C    *                                                                    526
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 <===                       526
         CALL AGSRESET                                                    527
         AG_SEEN = 1                                                      527
         AG_COLO = 2                                                      527
         AG_TITLE = 'SAGP'                                                527
         AG_EXNAME = 'ATTRIBUTE'                                          527
         AG_PARLIST = 'SEEN_COLO'                                         527
         CALL AXATTRIBUTE                                                 527
C    *                                                                    527
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  527
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         527
         CALL AGSRESET                                                    529
         AG_DX = SELC_ELCAWID/2                                           529
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              529
         AG_DZ = SELC_AGPDTHK/2                                           529
         AG_TITLE = 'BOX'                                                 529
         AG_EXNAME = 'SHAPE'                                              529
         AG_PARLIST = 'DX_DY_DZ'                                          529
         CALL AXSHAPE                                                     529
C    *                                                                    529
5701  CALL AGSPOPB                                                        530
      GO TO BLOCK_SAGP, (5700 ,5710)                                      530
C End of Local Procedure                                                  530
C ----------------------------------------------------------------------  531
C ------------ SGLA                                                       531
C Begin Local Procedure SGLA                                              532
C SGLA IS THE INSULATING GLASS LAYER                                      532
5680  CONTINUE                                                            532
         AG_TITLE='SGLA'                                                  532
         AG_MODULE=MODULE                                                 532
         AG_MARK='V00'                                                    532
         CALL AGSPUSH                                                     532
         AG_MODULE=MODULE                                                 532
C    *                                                                    532
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         533
         CALL AGSRESET                                                    533
         AG_A = 28                                                        533
         AG_Z = 14                                                        533
         AG_W = 1                                                         533
         AG_TITLE = 'SI'                                                  533
         AG_EXNAME = 'COMPONENT'                                          533
         AG_PARLIST = 'A_Z_W'                                             533
         CALL AXCOMPONENT                                                 533
C    *                                                                    533
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           533
         CALL AGSRESET                                                    534
         AG_A = 16                                                        534
         AG_Z = 8                                                         534
         AG_W = 2                                                         534
         AG_TITLE = 'O'                                                   534
         AG_EXNAME = 'COMPONENT'                                          534
         AG_PARLIST = 'A_Z_W'                                             534
         CALL AXCOMPONENT                                                 534
C    *                                                                    534
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             534
         CALL AGSRESET                                                    535
         AG_DENS = 2.2                                                    535
         AG_TITLE = 'GLASS'                                               535
         AG_EXNAME = 'MIXTURE'                                            535
         AG_PARLIST = 'DENS'                                              535
         CALL AXMIXTURE                                                   535
C    *                                                                    535
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 <===                       535
         CALL AGSRESET                                                    536
         AG_SEEN = 1                                                      536
         AG_COLO = 6                                                      536
         AG_TITLE = 'SGLA'                                                536
         AG_EXNAME = 'ATTRIBUTE'                                          536
         AG_PARLIST = 'SEEN_COLO'                                         536
         CALL AXATTRIBUTE                                                 536
C    *                                                                    536
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  536
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        536
         CALL AGSRESET                                                    538
         AG_DX = SELC_ELCAWID/2                                           538
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              538
         AG_DZ = SELC_GLASSTHK/2                                          538
         AG_TITLE = 'BOX'                                                 538
         AG_EXNAME = 'SHAPE'                                              538
         AG_PARLIST = 'DX_DY_DZ'                                          538
         CALL AXSHAPE                                                     538
C    *                                                                    538
5681  CALL AGSPOPB                                                        539
      GO TO BLOCK_SGLA, (5680 ,5690)                                      539
C End of Local Procedure                                                  539
C ----------------------------------------------------------------------  540
C ------------ SWCH                                                       540
C Begin Local Procedure SWCH                                              541
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      541
5600  CONTINUE                                                            541
         AG_TITLE='SWCH'                                                  541
         AG_MODULE=MODULE                                                 541
         AG_MARK='V00'                                                    541
         CALL AGSPUSH                                                     541
         AG_MODULE=MODULE                                                 541
C    *                                                                    541
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        542
         CALL AGSRESET                                                    542
         AG_TITLE = 'BERILLIUM'                                           542
         AG_EXNAME = 'MATERIAL'                                           542
         AG_PARLIST = 'NONE'                                              542
         CALL AXMATERIAL                                                  542
C    *                                                                    542
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 <===                       542
         CALL AGSRESET                                                    543
         AG_SEEN = 1                                                      543
         AG_COLO = 2                                                      543
         AG_TITLE = 'SWCH'                                                543
         AG_EXNAME = 'ATTRIBUTE'                                          543
         AG_PARLIST = 'SEEN_COLO'                                         543
         CALL AXATTRIBUTE                                                 543
C    *                                                                    543
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  543
C    BETHK/2 <===                                                         543
         CALL AGSRESET                                                    545
         AG_DX = SELC_ELCAWID/2                                           545
         AG_DY = SWCA_LENGTH/2                                            545
         AG_DZ = SELC_BETHK/2                                             545
         AG_TITLE = 'BOX'                                                 545
         AG_EXNAME = 'SHAPE'                                              545
         AG_PARLIST = 'DX_DY_DZ'                                          545
         CALL AXSHAPE                                                     545
C    *                                                                    545
5601  CALL AGSPOPB                                                        546
      GO TO BLOCK_SWCH, (5600 ,5610)                                      546
C End of Local Procedure                                                  546
C ----------------------------------------------------------------------  547
C ------------ SWCS                                                       547
C Begin Local Procedure SWCS                                              548
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                548
5620  CONTINUE                                                            548
         AG_TITLE='SWCS'                                                  548
         AG_MODULE=MODULE                                                 548
         AG_MARK='V00'                                                    548
         CALL AGSPUSH                                                     548
         AG_MODULE=MODULE                                                 548
C    *                                                                    548
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        549
         CALL AGSRESET                                                    549
         AG_TITLE = 'BERILLIUM'                                           549
         AG_EXNAME = 'MATERIAL'                                           549
         AG_PARLIST = 'NONE'                                              549
         CALL AXMATERIAL                                                  549
C    *                                                                    549
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 <===                       549
         CALL AGSRESET                                                    550
         AG_SEEN = 1                                                      550
         AG_COLO = 2                                                      550
         AG_TITLE = 'SWCS'                                                550
         AG_EXNAME = 'ATTRIBUTE'                                          550
         AG_PARLIST = 'SEEN_COLO'                                         550
         CALL AXATTRIBUTE                                                 550
C    *                                                                    550
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  550
C    TTHK/2 <===                                                          550
         CALL AGSRESET                                                    552
         AG_DX = SELC_BETHK/2                                             552
         AG_DY = SWCA_LENGTH/2                                            552
         AG_DZ = SELC_WATTHK/2                                            552
         AG_TITLE = 'BOX'                                                 552
         AG_EXNAME = 'SHAPE'                                              552
         AG_PARLIST = 'DX_DY_DZ'                                          552
         CALL AXSHAPE                                                     552
C    *                                                                    552
5621  CALL AGSPOPB                                                        553
      GO TO BLOCK_SWCS, (5620 ,5630)                                      553
C End of Local Procedure                                                  553
C ----------------------------------------------------------------------  554
C ------------ SWCW                                                       554
C Begin Local Procedure SWCW                                              555
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       555
5640  CONTINUE                                                            555
         AG_TITLE='SWCW'                                                  555
         AG_MODULE=MODULE                                                 555
         AG_MARK='V00'                                                    555
         CALL AGSPUSH                                                     555
         AG_MODULE=MODULE                                                 555
C    *                                                                    555
C    Geant MATERIAL : WATER ===> inherited arguments used <===            556
         CALL AGSRESET                                                    556
         AG_TITLE = 'WATER'                                               556
         AG_EXNAME = 'MATERIAL'                                           556
         AG_PARLIST = 'NONE'                                              556
         CALL AXMATERIAL                                                  556
C    *                                                                    556
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 <===                       556
         CALL AGSRESET                                                    557
         AG_SEEN = 1                                                      557
         AG_COLO = 6                                                      557
         AG_TITLE = 'SWCW'                                                557
         AG_EXNAME = 'ATTRIBUTE'                                          557
         AG_PARLIST = 'SEEN_COLO'                                         557
         CALL AXATTRIBUTE                                                 557
C    *                                                                    557
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  557
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     557
         CALL AGSRESET                                                    559
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          559
         AG_DY = SWCA_LENGTH/2                                            559
         AG_DZ = (SELC_WATTHK/2)                                          559
         AG_TITLE = 'BOX'                                                 559
         AG_EXNAME = 'SHAPE'                                              559
         AG_PARLIST = 'DX_DY_DZ'                                          559
         CALL AXSHAPE                                                     559
C    *                                                                    559
5641  CALL AGSPOPB                                                        560
      GO TO BLOCK_SWCW, (5640 ,5650)                                      560
C End of Local Procedure                                                  560
C ----------------------------------------------------------------------  561
C ------------ SIRT                                                       561
C Begin Local Procedure SIRT                                              562
C SIRT IS THE SVT INNER END RING TUBE                                     562
5050  CONTINUE                                                            562
         AG_TITLE='SIRT'                                                  562
         AG_MODULE=MODULE                                                 562
         AG_MARK='V00'                                                    562
         CALL AGSPUSH                                                     562
         AG_MODULE=MODULE                                                 562
C    *                                                                    562
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        563
         CALL AGSRESET                                                    563
         AG_TITLE = 'BERILLIUM'                                           563
         AG_EXNAME = 'MATERIAL'                                           563
         AG_PARLIST = 'NONE'                                              563
         CALL AXMATERIAL                                                  563
C    *                                                                    563
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       563
         CALL AGSRESET                                                    564
         AG_SEEN = 1                                                      564
         AG_COLO = 2                                                      564
         AG_TITLE = 'SIRT'                                                564
         AG_EXNAME = 'ATTRIBUTE'                                          564
         AG_PARLIST = 'SEEN_COLO'                                         564
         CALL AXATTRIBUTE                                                 564
C    *                                                                    564
         IR_RMIN = COS(PI/8.0)                                            564
         IR_RMIN=IR_RMIN+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/           566
     *   8.0)*SIN(PI/8.0))                                                566
         IR_RMIN=IR_RMIN*SERG_IRNGPRMN                                    567
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  569
C    DRNGTH/2 <===                                                        569
         CALL AGSRESET                                                    569
         AG_RMIN = IR_RMIN                                                569
         AG_RMAX = SERG_IRNGTRMX                                          569
         AG_DZ = SERG_ENDRNGTH/2                                          569
         AG_TITLE = 'TUBE'                                                569
         AG_EXNAME = 'SHAPE'                                              569
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      569
         CALL AXSHAPE                                                     569
C    *                                                                    569
5051  CALL AGSPOPB                                                        570
      GO TO BLOCK_SIRT, (5050 ,5060)                                      570
C End of Local Procedure                                                  570
C ----------------------------------------------------------------------  571
C ------------ SIRP                                                       571
C Begin Local Procedure SIRP                                              572
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 572
5030  CONTINUE                                                            572
         AG_TITLE='SIRP'                                                  572
         AG_MODULE=MODULE                                                 572
         AG_MARK='V00'                                                    572
         CALL AGSPUSH                                                     572
         AG_MODULE=MODULE                                                 572
C    *                                                                    572
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        573
         CALL AGSRESET                                                    573
         AG_TITLE = 'BERILLIUM'                                           573
         AG_EXNAME = 'MATERIAL'                                           573
         AG_PARLIST = 'NONE'                                              573
         CALL AXMATERIAL                                                  573
C    *                                                                    573
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       573
         CALL AGSRESET                                                    574
         AG_SEEN = 1                                                      574
         AG_COLO = 2                                                      574
         AG_TITLE = 'SIRP'                                                574
         AG_EXNAME = 'ATTRIBUTE'                                          574
         AG_PARLIST = 'SEEN_COLO'                                         574
         CALL AXATTRIBUTE                                                 574
C    *                                                                    574
         ROU = COS(PI/8.0)                                                574
         ROU=ROU+SQRT(TAN(PI/8.0)*TAN(PI/8.0) - SIN(PI/8.0)*SIN(PI/       576
     *   8.0))                                                            576
         ROU = SERG_IRNGTRMX/ROU                                          577
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  580
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  580
C    OU,ROU} <===                                                         580
         CALL AGSRESET                                                    580
         AG_PHI1 = 0                                                      580
         AG_DPHI = 360                                                    580
         AG_NPDIV = 8                                                     580
         AG_NZ = 2                                                        580
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        580
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        580
         AG_RMN(1)=SERG_IRNGPRMN                                          580
         AG_RMN(2)=SERG_IRNGPRMN                                          580
         AG_RMX(1)=ROU                                                    580
         AG_RMX(2)=ROU                                                    580
         AG_TITLE = 'PGON'                                                580
         AG_EXNAME = 'SHAPE'                                              580
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     580
         CALL AXSHAPE                                                     580
C    *                                                                    580
5031  CALL AGSPOPB                                                        581
      GO TO BLOCK_SIRP, (5030 ,5040)                                      581
C End of Local Procedure                                                  581
C ----------------------------------------------------------------------  582
C ------------ SOER                                                       582
C Begin Local Procedure SOER                                              583
C SOER IS THE SVT OUTER END RING                                          583
5070  CONTINUE                                                            583
         AG_TITLE='SOER'                                                  583
         AG_MODULE=MODULE                                                 583
         AG_MARK='V00'                                                    583
         CALL AGSPUSH                                                     583
         AG_MODULE=MODULE                                                 583
C    *                                                                    583
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        584
         CALL AGSRESET                                                    584
         AG_TITLE = 'BERILLIUM'                                           584
         AG_EXNAME = 'MATERIAL'                                           584
         AG_PARLIST = 'NONE'                                              584
         CALL AXMATERIAL                                                  584
C    *                                                                    584
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       584
         CALL AGSRESET                                                    585
         AG_SEEN = 1                                                      585
         AG_COLO = 2                                                      585
         AG_TITLE = 'SOER'                                                585
         AG_EXNAME = 'ATTRIBUTE'                                          585
         AG_PARLIST = 'SEEN_COLO'                                         585
         CALL AXATTRIBUTE                                                 585
C    *                                                                    585
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  585
C    ERG_ENDRNGTH/2 <===                                                  585
         CALL AGSRESET                                                    587
         AG_RMIN = SERG_ORNGRMIN                                          587
         AG_RMAX = SERG_ORNGRMAX                                          587
         AG_DZ = SERG_ENDRNGTH/2                                          587
         AG_TITLE = 'TUBE'                                                587
         AG_EXNAME = 'SHAPE'                                              587
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      587
         CALL AXSHAPE                                                     587
C    *                                                                    587
5071  CALL AGSPOPB                                                        588
      GO TO BLOCK_SOER, (5070 ,5080)                                      588
C End of Local Procedure                                                  588
C ----------------------------------------------------------------------  589
C ------------ SIES                                                       589
C Begin Local Procedure SIES                                              590
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         590
5130  CONTINUE                                                            590
         AG_TITLE='SIES'                                                  590
         AG_MODULE=MODULE                                                 590
         AG_MARK='V00'                                                    590
         CALL AGSPUSH                                                     590
         AG_MODULE=MODULE                                                 590
C    *                                                                    590
C    Geant MATERIAL : AIR ===> inherited arguments used <===              591
         CALL AGSRESET                                                    591
         AG_TITLE = 'AIR'                                                 591
         AG_EXNAME = 'MATERIAL'                                           591
         AG_PARLIST = 'NONE'                                              591
         CALL AXMATERIAL                                                  591
C    *                                                                    591
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       591
         CALL AGSRESET                                                    592
         AG_SEEN = 0                                                      592
         AG_COLO = 1                                                      592
         AG_TITLE = 'SIES'                                                592
         AG_EXNAME = 'ATTRIBUTE'                                          592
         AG_PARLIST = 'SEEN_COLO'                                         592
         CALL AXATTRIBUTE                                                 592
C    *                                                                    592
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  592
C    ACK_Z-ENDRNG_Z) <===                                                 592
         CALL AGSRESET                                                    594
         AG_RMIN = IR_RMIN                                                594
         AG_RMAX = SERG_IRNGTRMX                                          594
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   594
         AG_TITLE = 'TUBE'                                                594
         AG_EXNAME = 'SHAPE'                                              594
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      594
         CALL AXSHAPE                                                     594
C    *                                                                    594
C    Create SISM :                                                        595
C    Call To Local Procedure SISM                                         595
         ASSIGN 5770 TO BLOCK_SISM                                        595
         GO TO  5760                                                      595
5770     CONTINUE                                                         595
C    *                                                                    595
5131  CALL AGSPOPB                                                        596
      GO TO BLOCK_SIES, (5130 ,5140)                                      596
C End of Local Procedure                                                  596
C ----------------------------------------------------------------------  597
C ------------ SISM                                                       597
C Begin Local Procedure SISM                                              598
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        598
5760  CONTINUE                                                            598
         AG_TITLE='SISM'                                                  598
         AG_MODULE=MODULE                                                 598
         AG_MARK='V00'                                                    598
         CALL AGSPUSH                                                     598
         AG_MODULE=MODULE                                                 598
C    *                                                                    598
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       599
         CALL AGSRESET                                                    599
         AG_SEEN = 0                                                      599
         AG_COLO = 1                                                      599
         AG_TITLE = 'SISM'                                                599
         AG_EXNAME = 'ATTRIBUTE'                                          599
         AG_PARLIST = 'SEEN_COLO'                                         599
         CALL AXATTRIBUTE                                                 599
C    *                                                                    599
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                599
         CALL AGSRESET                                                    600
         AG_NDIV = 4                                                      600
         AG_IAXIS = 2                                                     600
         AG_C0 = 45                                                       600
         AG_TITLE = 'DIVISION'                                            600
         AG_EXNAME = 'SHAPE'                                              600
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     600
         CALL AXSHAPE                                                     600
C    *                                                                    600
C    Create SCRW :                                                        601
C    Call To Local Procedure SCRW                                         601
         ASSIGN 5790 TO BLOCK_SCRW                                        601
         GO TO  5780                                                      601
5790     CONTINUE                                                         601
C    *                                                                    601
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  602
C    ===                                                                  602
         CALL AGSRESET                                                    602
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      602
         AG_TITLE = 'SCRW'                                                602
         AG_EXNAME = 'POSITION'                                           602
         AG_PARLIST = 'X'                                                 602
         CALL AXPOSITION                                                  602
C    *                                                                    602
5761  CALL AGSPOPB                                                        603
      GO TO BLOCK_SISM, (5760 ,5770)                                      603
C End of Local Procedure                                                  603
C ----------------------------------------------------------------------  604
C ------------ SOES                                                       604
C Begin Local Procedure SOES                                              605
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         605
5110  CONTINUE                                                            605
         AG_TITLE='SOES'                                                  605
         AG_MODULE=MODULE                                                 605
         AG_MARK='V00'                                                    605
         CALL AGSPUSH                                                     605
         AG_MODULE=MODULE                                                 605
C    *                                                                    605
C    Geant MATERIAL : AIR ===> inherited arguments used <===              606
         CALL AGSRESET                                                    606
         AG_TITLE = 'AIR'                                                 606
         AG_EXNAME = 'MATERIAL'                                           606
         AG_PARLIST = 'NONE'                                              606
         CALL AXMATERIAL                                                  606
C    *                                                                    606
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       606
         CALL AGSRESET                                                    607
         AG_SEEN = 0                                                      607
         AG_COLO = 1                                                      607
         AG_TITLE = 'SOES'                                                607
         AG_EXNAME = 'ATTRIBUTE'                                          607
         AG_PARLIST = 'SEEN_COLO'                                         607
         CALL AXATTRIBUTE                                                 607
C    *                                                                    607
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  607
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           607
         CALL AGSRESET                                                    609
         AG_RMIN = SERG_ORNGRMIN                                          609
         AG_RMAX = SERG_ORNGRMAX                                          609
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   609
         AG_TITLE = 'TUBE'                                                609
         AG_EXNAME = 'SHAPE'                                              609
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      609
         CALL AXSHAPE                                                     609
C    *                                                                    609
C    Create SOSM :                                                        610
C    Call To Local Procedure SOSM                                         610
         ASSIGN 5810 TO BLOCK_SOSM                                        610
         GO TO  5800                                                      610
5810     CONTINUE                                                         610
C    *                                                                    610
5111  CALL AGSPOPB                                                        611
      GO TO BLOCK_SOES, (5110 ,5120)                                      611
C End of Local Procedure                                                  611
C ----------------------------------------------------------------------  612
C ------------ SOSM                                                       612
C Begin Local Procedure SOSM                                              613
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        613
5800  CONTINUE                                                            613
         AG_TITLE='SOSM'                                                  613
         AG_MODULE=MODULE                                                 613
         AG_MARK='V00'                                                    613
         CALL AGSPUSH                                                     613
         AG_MODULE=MODULE                                                 613
C    *                                                                    613
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       614
         CALL AGSRESET                                                    614
         AG_SEEN = 0                                                      614
         AG_COLO = 1                                                      614
         AG_TITLE = 'SOSM'                                                614
         AG_EXNAME = 'ATTRIBUTE'                                          614
         AG_PARLIST = 'SEEN_COLO'                                         614
         CALL AXATTRIBUTE                                                 614
C    *                                                                    614
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                614
         CALL AGSRESET                                                    615
         AG_NDIV = 4                                                      615
         AG_IAXIS = 2                                                     615
         AG_C0 = 45                                                       615
         AG_TITLE = 'DIVISION'                                            615
         AG_EXNAME = 'SHAPE'                                              615
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     615
         CALL AXSHAPE                                                     615
C    *                                                                    615
C    Create SCRW :                                                        616
C    Call To Local Procedure SCRW                                         616
         ASSIGN 5820 TO BLOCK_SCRW                                        616
         GO TO  5780                                                      616
5820     CONTINUE                                                         616
C    *                                                                    616
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  617
C    ===                                                                  617
         CALL AGSRESET                                                    617
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      617
         AG_TITLE = 'SCRW'                                                617
         AG_EXNAME = 'POSITION'                                           617
         AG_PARLIST = 'X'                                                 617
         CALL AXPOSITION                                                  617
C    *                                                                    617
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  617
C    ===                                                                  617
         CALL AGSRESET                                                    618
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      618
         AG_TITLE = 'SCRW'                                                618
         AG_EXNAME = 'POSITION'                                           618
         AG_PARLIST = 'X'                                                 618
         CALL AXPOSITION                                                  618
C    *                                                                    618
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  618
C    =SSUP_ERJ2Y <===                                                     618
         CALL AGSRESET                                                    619
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      619
         AG_Y = SSUP_ERJ2Y                                                619
         AG_TITLE = 'SCRW'                                                619
         AG_EXNAME = 'POSITION'                                           619
         AG_PARLIST = 'X_Y'                                               619
         CALL AXPOSITION                                                  619
C    *                                                                    619
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  619
C    =-SSUP_ERJ2Y <===                                                    619
         CALL AGSRESET                                                    620
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      620
         AG_Y = -SSUP_ERJ2Y                                               620
         AG_TITLE = 'SCRW'                                                620
         AG_EXNAME = 'POSITION'                                           620
         AG_PARLIST = 'X_Y'                                               620
         CALL AXPOSITION                                                  620
C    *                                                                    620
5801  CALL AGSPOPB                                                        621
      GO TO BLOCK_SOSM, (5800 ,5810)                                      621
C End of Local Procedure                                                  621
C ----------------------------------------------------------------------  622
C ------------ SBRG                                                       622
C Begin Local Procedure SBRG                                              623
C SBRG IS THE BRACKET JOINING THE END RINGS                               623
5090  CONTINUE                                                            623
         AG_TITLE='SBRG'                                                  623
         AG_MODULE=MODULE                                                 623
         AG_MARK='V00'                                                    623
         CALL AGSPUSH                                                     623
         AG_MODULE=MODULE                                                 623
C    *                                                                    623
         RIN=SERG_IRNGPRMN                                                624
         ROU=SERG_ORNGRMAX                                                625
C    Geant MATERIAL : AIR ===> inherited arguments used <===              626
         CALL AGSRESET                                                    626
         AG_TITLE = 'AIR'                                                 626
         AG_EXNAME = 'MATERIAL'                                           626
         AG_PARLIST = 'NONE'                                              626
         CALL AXMATERIAL                                                  626
C    *                                                                    626
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       626
         CALL AGSRESET                                                    627
         AG_SEEN = 0                                                      627
         AG_COLO = 1                                                      627
         AG_TITLE = 'SBRG'                                                627
         AG_EXNAME = 'ATTRIBUTE'                                          627
         AG_PARLIST = 'SEEN_COLO'                                         627
         CALL AXATTRIBUTE                                                 627
C    *                                                                    627
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      627
         CALL AGSRESET                                                    628
         AG_RMIN = RIN                                                    628
         AG_RMAX = ROU                                                    628
         AG_DZ = SSUP_ERJTHK/2                                            628
         AG_TITLE = 'TUBE'                                                628
         AG_EXNAME = 'SHAPE'                                              628
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      628
         CALL AXSHAPE                                                     628
C    *                                                                    628
C    Create SBRM :                                                        629
C    Call To Local Procedure SBRM                                         629
         ASSIGN 5840 TO BLOCK_SBRM                                        629
         GO TO  5830                                                      629
5840     CONTINUE                                                         629
C    *                                                                    629
5091  CALL AGSPOPB                                                        630
      GO TO BLOCK_SBRG, (5090 ,5100)                                      630
C End of Local Procedure                                                  630
C ----------------------------------------------------------------------  631
C ------------ SBRM                                                       631
C Begin Local Procedure SBRM                                              632
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          632
5830  CONTINUE                                                            632
         AG_TITLE='SBRM'                                                  632
         AG_MODULE=MODULE                                                 632
         AG_MARK='V00'                                                    632
         CALL AGSPUSH                                                     632
         AG_MODULE=MODULE                                                 632
C    *                                                                    632
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       633
         CALL AGSRESET                                                    633
         AG_SEEN = 0                                                      633
         AG_COLO = 1                                                      633
         AG_TITLE = 'SBRM'                                                633
         AG_EXNAME = 'ATTRIBUTE'                                          633
         AG_PARLIST = 'SEEN_COLO'                                         633
         CALL AXATTRIBUTE                                                 633
C    *                                                                    633
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                633
         CALL AGSRESET                                                    634
         AG_NDIV = 4                                                      634
         AG_IAXIS = 2                                                     634
         AG_C0 = 45                                                       634
         AG_TITLE = 'DIVISION'                                            634
         AG_EXNAME = 'SHAPE'                                              634
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     634
         CALL AXSHAPE                                                     634
C    *                                                                    634
C    Create SBRI :                                                        635
C    Call To Local Procedure SBRI                                         635
         ASSIGN 5860 TO BLOCK_SBRI                                        635
         GO TO  5850                                                      635
5860     CONTINUE                                                         635
C    *                                                                    635
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        636
         CALL AGSRESET                                                    636
         AG_X = SSUP_ERJRAD                                               636
         AG_TITLE = 'SBRI'                                                636
         AG_EXNAME = 'POSITION'                                           636
         AG_PARLIST = 'X'                                                 636
         CALL AXPOSITION                                                  636
C    *                                                                    636
5831  CALL AGSPOPB                                                        637
      GO TO BLOCK_SBRM, (5830 ,5840)                                      637
C End of Local Procedure                                                  637
C ----------------------------------------------------------------------  638
C ------------ SBRI                                                       638
C Begin Local Procedure SBRI                                              639
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               639
5850  CONTINUE                                                            639
         AG_TITLE='SBRI'                                                  639
         AG_MODULE=MODULE                                                 639
         AG_MARK='V00'                                                    639
         CALL AGSPUSH                                                     639
         AG_MODULE=MODULE                                                 639
C    *                                                                    639
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       640
         CALL AGSRESET                                                    640
         AG_SEEN = 1                                                      640
         AG_COLO = 2                                                      640
         AG_TITLE = 'SBRI'                                                640
         AG_EXNAME = 'ATTRIBUTE'                                          640
         AG_PARLIST = 'SEEN_COLO'                                         640
         CALL AXATTRIBUTE                                                 640
C    *                                                                    640
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        640
         CALL AGSRESET                                                    641
         AG_TITLE = 'BERILLIUM'                                           641
         AG_EXNAME = 'MATERIAL'                                           641
         AG_PARLIST = 'NONE'                                              641
         CALL AXMATERIAL                                                  641
C    *                                                                    641
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  641
C    RJTHK/2 <===                                                         641
         CALL AGSRESET                                                    643
         AG_DX = SSUP_ERJLEN/2                                            643
         AG_DY = SSUP_ERJWID/2                                            643
         AG_DZ = SSUP_ERJTHK/2                                            643
         AG_TITLE = 'BOX'                                                 643
         AG_EXNAME = 'SHAPE'                                              643
         AG_PARLIST = 'DX_DY_DZ'                                          643
         CALL AXSHAPE                                                     643
C    *                                                                    643
5851  CALL AGSPOPB                                                        644
      GO TO BLOCK_SBRI, (5850 ,5860)                                      644
C End of Local Procedure                                                  644
C ----------------------------------------------------------------------  645
C ------------ SROD                                                       645
C Begin Local Procedure SROD                                              646
C SROD IS THE SVT BE SUPPORT ROD                                          646
5230  CONTINUE                                                            646
         AG_TITLE='SROD'                                                  646
         AG_MODULE=MODULE                                                 646
         AG_MARK='V00'                                                    646
         CALL AGSPUSH                                                     646
         AG_MODULE=MODULE                                                 646
C    *                                                                    646
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        647
         CALL AGSRESET                                                    647
         AG_TITLE = 'BERILLIUM'                                           647
         AG_EXNAME = 'MATERIAL'                                           647
         AG_PARLIST = 'NONE'                                              647
         CALL AXMATERIAL                                                  647
C    *                                                                    647
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       647
         CALL AGSRESET                                                    648
         AG_SEEN = 1                                                      648
         AG_COLO = 2                                                      648
         AG_TITLE = 'SROD'                                                648
         AG_EXNAME = 'ATTRIBUTE'                                          648
         AG_PARLIST = 'SEEN_COLO'                                         648
         CALL AXATTRIBUTE                                                 648
C    *                                                                    648
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  648
C    P_RODLEN/2 <===                                                      648
         CALL AGSRESET                                                    650
         AG_RMIN = SSUP_RODID/2                                           650
         AG_RMAX = SSUP_RODOD/2                                           650
         AG_DZ = SSUP_RODLEN/2                                            650
         AG_TITLE = 'TUBE'                                                650
         AG_EXNAME = 'SHAPE'                                              650
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      650
         CALL AXSHAPE                                                     650
C    *                                                                    650
5231  CALL AGSPOPB                                                        651
      GO TO BLOCK_SROD, (5230 ,5240)                                      651
C End of Local Procedure                                                  651
C ----------------------------------------------------------------------  652
C ------------ SCON                                                       652
C Begin Local Procedure SCON                                              653
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               653
5190  CONTINUE                                                            653
         AG_TITLE='SCON'                                                  653
         AG_MODULE=MODULE                                                 653
         AG_MARK='V00'                                                    653
         CALL AGSPUSH                                                     653
         AG_MODULE=MODULE                                                 653
C    *                                                                    653
         CONE_THK1=SSUP_GRPHTHK                                           654
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                655
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              656
C    Geant MATERIAL : AIR ===> inherited arguments used <===              657
         CALL AGSRESET                                                    657
         AG_TITLE = 'AIR'                                                 657
         AG_EXNAME = 'MATERIAL'                                           657
         AG_PARLIST = 'NONE'                                              657
         CALL AXMATERIAL                                                  657
C    *                                                                    657
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       657
         CALL AGSRESET                                                    658
         AG_SEEN = 0                                                      658
         AG_COLO = 1                                                      658
         AG_TITLE = 'SCON'                                                658
         AG_EXNAME = 'ATTRIBUTE'                                          658
         AG_PARLIST = 'SEEN_COLO'                                         658
         CALL AXATTRIBUTE                                                 658
C    *                                                                    658
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  658
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  658
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  658
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  658
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  658
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  658
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      658
         CALL AGSRESET                                                    668
         AG_PHI1 = 0                                                      668
         AG_DPHI = 360                                                    668
         AG_NZ = 7                                                        668
         AG_ZI(1)=SSUP_CONE1ZMN                                           668
         AG_ZI(2)=SSUP_RODLEN/2                                           668
         AG_ZI(3)=SSUP_RODLEN/2                                           668
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              668
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              668
         AG_ZI(6)=SSUP_CONE3ZMX                                           668
         AG_ZI(7)=SSUP_CONE4ZMX                                           668
         AG_RMN(1)=SSUP_CON1IDMN                                          668
         AG_RMN(2)=SSUP_CON1IDMN                                          668
         AG_RMN(3)=SSUP_CON1IDMN                                          668
         AG_RMN(4)=SSUP_CON1IDMN                                          668
         AG_RMN(5)=SSUP_CON3IDMN                                          668
         AG_RMN(6)=SSUP_CON4IDMN                                          668
         AG_RMN(7)=SSUP_CON4IDMX                                          668
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                668
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                668
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                668
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                668
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                668
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                668
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                668
         AG_TITLE = 'PCON'                                                668
         AG_EXNAME = 'SHAPE'                                              668
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           668
         CALL AXSHAPE                                                     668
C    *                                                                    668
C    Create SGRA :                                                        669
C    Call To Local Procedure SGRA                                         669
         ASSIGN 5880 TO BLOCK_SGRA                                        669
         GO TO  5870                                                      669
5880     CONTINUE                                                         669
C    *                                                                    669
C    Geant POSITION : SGRA ===> inherited arguments used <===             670
         CALL AGSRESET                                                    670
         AG_TITLE = 'SGRA'                                                670
         AG_EXNAME = 'POSITION'                                           670
         AG_PARLIST = 'NONE'                                              670
         CALL AXPOSITION                                                  670
C    *                                                                    670
C    Create STAP :                                                        671
C    Call To Local Procedure STAP                                         671
         ASSIGN 5900 TO BLOCK_STAP                                        671
         GO TO  5890                                                      671
5900     CONTINUE                                                         671
C    *                                                                    671
C    Geant POSITION : STAP ===> inherited arguments used <===             672
         CALL AGSRESET                                                    672
         AG_TITLE = 'STAP'                                                672
         AG_EXNAME = 'POSITION'                                           672
         AG_PARLIST = 'NONE'                                              672
         CALL AXPOSITION                                                  672
C    *                                                                    672
C    Create STAC :                                                        673
C    Call To Local Procedure STAC                                         673
         ASSIGN 5920 TO BLOCK_STAC                                        673
         GO TO  5910                                                      673
5920     CONTINUE                                                         673
C    *                                                                    673
C    Geant POSITION : STAC ===> inherited arguments used <===             674
         CALL AGSRESET                                                    674
         AG_TITLE = 'STAC'                                                674
         AG_EXNAME = 'POSITION'                                           674
         AG_PARLIST = 'NONE'                                              674
         CALL AXPOSITION                                                  674
C    *                                                                    674
C    Create SHLA :                                                        675
C    Call To Local Procedure SHLA                                         675
         ASSIGN 5940 TO BLOCK_SHLA                                        675
         GO TO  5930                                                      675
5940     CONTINUE                                                         675
C    *                                                                    675
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  677
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             677
         CALL AGSRESET                                                    677
         AG_Z = SSUP_RODLEN/                                              677
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    677
         AG_TITLE = 'SHLA'                                                677
         AG_EXNAME = 'POSITION'                                           677
         AG_PARLIST = 'Z'                                                 677
         CALL AXPOSITION                                                  677
C    *                                                                    677
C    Create SHLB :                                                        678
C    Call To Local Procedure SHLB                                         678
         ASSIGN 5960 TO BLOCK_SHLB                                        678
         GO TO  5950                                                      678
5960     CONTINUE                                                         678
C    *                                                                    678
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  679
C    CONE3ZMX) <===                                                       679
         CALL AGSRESET                                                    679
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           679
         AG_TITLE = 'SHLB'                                                679
         AG_EXNAME = 'POSITION'                                           679
         AG_PARLIST = 'Z'                                                 679
         CALL AXPOSITION                                                  679
C    *                                                                    679
C    Create SCMY :                                                        680
C    Call To Local Procedure SCMY                                         680
         ASSIGN 5980 TO BLOCK_SCMY                                        680
         GO TO  5970                                                      680
5980     CONTINUE                                                         680
C    *                                                                    680
C    Geant POSITION : SCMY ===> inherited arguments used <===             681
         CALL AGSRESET                                                    681
         AG_TITLE = 'SCMY'                                                681
         AG_EXNAME = 'POSITION'                                           681
         AG_PARLIST = 'NONE'                                              681
         CALL AXPOSITION                                                  681
C    *                                                                    681
C    Create SCAL :                                                        682
C    Call To Local Procedure SCAL                                         682
         ASSIGN 6000 TO BLOCK_SCAL                                        682
         GO TO  5990                                                      682
6000     CONTINUE                                                         682
C    *                                                                    682
C    Geant POSITION : SCAL ===> inherited arguments used <===             683
         CALL AGSRESET                                                    683
         AG_TITLE = 'SCAL'                                                683
         AG_EXNAME = 'POSITION'                                           683
         AG_PARLIST = 'NONE'                                              683
         CALL AXPOSITION                                                  683
C    *                                                                    683
5191  CALL AGSPOPB                                                        684
      GO TO BLOCK_SCON, (5190 ,5200)                                      684
C End of Local Procedure                                                  684
C ----------------------------------------------------------------------  685
C ------------ SGRA                                                       685
C Begin Local Procedure SGRA                                              686
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 686
5870  CONTINUE                                                            686
         AG_TITLE='SGRA'                                                  686
         AG_MODULE=MODULE                                                 686
         AG_MARK='V00'                                                    686
         CALL AGSPUSH                                                     686
         AG_MODULE=MODULE                                                 686
C    *                                                                    686
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           687
         CALL AGSRESET                                                    687
         AG_TITLE = 'CARBON'                                              687
         AG_EXNAME = 'MATERIAL'                                           687
         AG_PARLIST = 'NONE'                                              687
         CALL AXMATERIAL                                                  687
C    *                                                                    687
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       687
         CALL AGSRESET                                                    688
         AG_SEEN = 1                                                      688
         AG_COLO = 6                                                      688
         AG_TITLE = 'SGRA'                                                688
         AG_EXNAME = 'ATTRIBUTE'                                          688
         AG_PARLIST = 'SEEN_COLO'                                         688
         CALL AXATTRIBUTE                                                 688
C    *                                                                    688
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  688
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  688
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  688
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  688
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  688
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              688
         CALL AGSRESET                                                    696
         AG_PHI1 = 0                                                      696
         AG_DPHI = 360                                                    696
         AG_NZ = 5                                                        696
         AG_ZI(1)=SSUP_RODLEN/2                                           696
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              696
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              696
         AG_ZI(4)=SSUP_CONE3ZMX                                           696
         AG_ZI(5)=SSUP_CONE4ZMX                                           696
         AG_RMN(1)=SSUP_CON1IDMN                                          696
         AG_RMN(2)=SSUP_CON1IDMN                                          696
         AG_RMN(3)=SSUP_CON3IDMN                                          696
         AG_RMN(4)=SSUP_CON4IDMN                                          696
         AG_RMN(5)=SSUP_CON4IDMX                                          696
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             696
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             696
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             696
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             696
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             696
         AG_TITLE = 'PCON'                                                696
         AG_EXNAME = 'SHAPE'                                              696
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           696
         CALL AXSHAPE                                                     696
C    *                                                                    696
5871  CALL AGSPOPB                                                        697
      GO TO BLOCK_SGRA, (5870 ,5880)                                      697
C End of Local Procedure                                                  697
C ----------------------------------------------------------------------  698
C ------------ STAP                                                       698
C Begin Local Procedure STAP                                              699
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  699
C E)                                                                      699
5890  CONTINUE                                                            699
         AG_TITLE='STAP'                                                  699
         AG_MODULE=MODULE                                                 699
         AG_MARK='V00'                                                    699
         CALL AGSPUSH                                                     699
         AG_MODULE=MODULE                                                 699
C    *                                                                    699
         ROFFSET=SSUP_GRPHTHK                                             700
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           701
         CALL AGSRESET                                                    701
         AG_A = 12                                                        701
         AG_Z = 6                                                         701
         AG_W = 1                                                         701
         AG_TITLE = 'C'                                                   701
         AG_EXNAME = 'COMPONENT'                                          701
         AG_PARLIST = 'A_Z_W'                                             701
         CALL AXCOMPONENT                                                 701
C    *                                                                    701
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           701
         CALL AGSRESET                                                    702
         AG_A = 1                                                         702
         AG_Z = 1                                                         702
         AG_W = 2                                                         702
         AG_TITLE = 'H2'                                                  702
         AG_EXNAME = 'COMPONENT'                                          702
         AG_PARLIST = 'A_Z_W'                                             702
         CALL AXCOMPONENT                                                 702
C    *                                                                    702
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             702
         CALL AGSRESET                                                    703
         AG_DENS = 0.935                                                  703
         AG_TITLE = 'CH2'                                                 703
         AG_EXNAME = 'MIXTURE'                                            703
         AG_PARLIST = 'DENS'                                              703
         CALL AXMIXTURE                                                   703
C    *                                                                    703
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       703
         CALL AGSRESET                                                    704
         AG_SEEN = 1                                                      704
         AG_COLO = 3                                                      704
         AG_TITLE = 'STAP'                                                704
         AG_EXNAME = 'ATTRIBUTE'                                          704
         AG_PARLIST = 'SEEN_COLO'                                         704
         CALL AXATTRIBUTE                                                 704
C    *                                                                    704
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  704
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  704
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  704
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  704
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     704
         CALL AGSRESET                                                    710
         AG_PHI1 = 0                                                      710
         AG_DPHI = 360                                                    710
         AG_NZ = 3                                                        710
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              710
         AG_ZI(2)=SSUP_CONE3ZMX                                           710
         AG_ZI(3)=SSUP_CONE4ZMX                                           710
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  710
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  710
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  710
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    710
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    710
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    710
         AG_TITLE = 'PCON'                                                710
         AG_EXNAME = 'SHAPE'                                              710
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           710
         CALL AXSHAPE                                                     710
C    *                                                                    710
5891  CALL AGSPOPB                                                        711
      GO TO BLOCK_STAP, (5890 ,5900)                                      711
C End of Local Procedure                                                  711
C ----------------------------------------------------------------------  712
C ------------ STAC                                                       712
C Begin Local Procedure STAC                                              713
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      713
5910  CONTINUE                                                            713
         AG_TITLE='STAC'                                                  713
         AG_MODULE=MODULE                                                 713
         AG_MARK='V00'                                                    713
         CALL AGSPUSH                                                     713
         AG_MODULE=MODULE                                                 713
C    *                                                                    713
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             714
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           715
         CALL AGSRESET                                                    715
         AG_TITLE = 'COPPER'                                              715
         AG_EXNAME = 'MATERIAL'                                           715
         AG_PARLIST = 'NONE'                                              715
         CALL AXMATERIAL                                                  715
C    *                                                                    715
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       715
         CALL AGSRESET                                                    716
         AG_SEEN = 1                                                      716
         AG_COLO = 2                                                      716
         AG_TITLE = 'STAC'                                                716
         AG_EXNAME = 'ATTRIBUTE'                                          716
         AG_PARLIST = 'SEEN_COLO'                                         716
         CALL AXATTRIBUTE                                                 716
C    *                                                                    716
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  716
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  716
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  716
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  716
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     716
         CALL AGSRESET                                                    722
         AG_PHI1 = 0                                                      722
         AG_DPHI = 360                                                    722
         AG_NZ = 3                                                        722
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              722
         AG_ZI(2)=SSUP_CONE3ZMX                                           722
         AG_ZI(3)=SSUP_CONE4ZMX                                           722
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  722
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  722
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  722
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    722
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    722
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    722
         AG_TITLE = 'PCON'                                                722
         AG_EXNAME = 'SHAPE'                                              722
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           722
         CALL AXSHAPE                                                     722
C    *                                                                    722
5911  CALL AGSPOPB                                                        723
      GO TO BLOCK_STAC, (5910 ,5920)                                      723
C End of Local Procedure                                                  723
C ----------------------------------------------------------------------  724
C ------------ SHLA                                                       724
C Begin Local Procedure SHLA                                              725
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              725
5930  CONTINUE                                                            725
         AG_TITLE='SHLA'                                                  725
         AG_MODULE=MODULE                                                 725
         AG_MARK='V00'                                                    725
         CALL AGSPUSH                                                     725
         AG_MODULE=MODULE                                                 725
C    *                                                                    725
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               726
C    Geant MATERIAL : AIR ===> inherited arguments used <===              727
         CALL AGSRESET                                                    727
         AG_TITLE = 'AIR'                                                 727
         AG_EXNAME = 'MATERIAL'                                           727
         AG_PARLIST = 'NONE'                                              727
         CALL AXMATERIAL                                                  727
C    *                                                                    727
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       727
         CALL AGSRESET                                                    728
         AG_SEEN = 0                                                      728
         AG_COLO = 1                                                      728
         AG_TITLE = 'SHLA'                                                728
         AG_EXNAME = 'ATTRIBUTE'                                          728
         AG_PARLIST = 'SEEN_COLO'                                         728
         CALL AXATTRIBUTE                                                 728
C    *                                                                    728
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  728
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  728
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  728
C    .0*SSUP_HOSRMX <===                                                  728
         CALL AGSRESET                                                    732
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            732
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  732
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  732
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  732
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  732
         AG_TITLE = 'CONE'                                                732
         AG_EXNAME = 'SHAPE'                                              732
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            732
         CALL AXSHAPE                                                     732
C    *                                                                    732
C    Create SHMA :                                                        733
C    Call To Local Procedure SHMA                                         733
         ASSIGN 6020 TO BLOCK_SHMA                                        733
         GO TO  6010                                                      733
6020     CONTINUE                                                         733
C    *                                                                    733
5931  CALL AGSPOPB                                                        734
      GO TO BLOCK_SHLA, (5930 ,5940)                                      734
C End of Local Procedure                                                  734
C ----------------------------------------------------------------------  735
C ------------ SHMA                                                       735
C Begin Local Procedure SHMA                                              736
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           736
6010  CONTINUE                                                            736
         AG_TITLE='SHMA'                                                  736
         AG_MODULE=MODULE                                                 736
         AG_MARK='V00'                                                    736
         CALL AGSPUSH                                                     736
         AG_MODULE=MODULE                                                 736
C    *                                                                    736
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               737
         RMIN=SSUP_CON3IDMN + ROFFSET                                     738
         RMAX=SSUP_CON4IDMN + ROFFSET                                     739
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                740
         ZMAX=SSUP_CONE3ZMX                                               741
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            742
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    743
         CONE_LEN = SQRT(CONE_LEN)                                        744
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  745
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  746
         ANGLE=ASIN(CONE_SIN)*180/3.1416                                  747
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             748
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     749
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       750
         CALL AGSRESET                                                    750
         AG_SEEN = 0                                                      750
         AG_COLO = 1                                                      750
         AG_TITLE = 'SHMA'                                                750
         AG_EXNAME = 'ATTRIBUTE'                                          750
         AG_PARLIST = 'SEEN_COLO'                                         750
         CALL AXATTRIBUTE                                                 750
C    *                                                                    750
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    750
         CALL AGSRESET                                                    752
         AG_NDIV = SSUP_NHOSES                                            752
         AG_IAXIS = 2                                                     752
         AG_C0 = 0.0                                                      752
         AG_TITLE = 'DIVISION'                                            752
         AG_EXNAME = 'SHAPE'                                              752
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     752
         CALL AXSHAPE                                                     752
C    *                                                                    752
C    Create SWHO :                                                        753
C    Call To Local Procedure SWHO                                         753
         ASSIGN 6040 TO BLOCK_SWHO                                        753
         GO TO  6030                                                      753
6040     CONTINUE                                                         753
C    *                                                                    753
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       754
         CALL AGSRESET                                                    754
         AG_X = XPOS                                                      754
         AG_Y = 0                                                         754
         AG_Z = ZPOS                                                      754
         AG_ALPHAN=AG_ALPHAN+1                                            754
         AG_ALPHAK(AG_ALPHAN)=2                                           754
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       754
         AG_TITLE = 'SWHO'                                                754
         AG_EXNAME = 'POSITION'                                           754
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      754
         CALL AXPOSITION                                                  754
C    *                                                                    754
6011  CALL AGSPOPB                                                        755
      GO TO BLOCK_SHMA, (6010 ,6020)                                      755
C End of Local Procedure                                                  755
C ----------------------------------------------------------------------  756
C ------------ SHLB                                                       756
C Begin Local Procedure SHLB                                              757
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               757
5950  CONTINUE                                                            757
         AG_TITLE='SHLB'                                                  757
         AG_MODULE=MODULE                                                 757
         AG_MARK='V00'                                                    757
         CALL AGSPUSH                                                     757
         AG_MODULE=MODULE                                                 757
C    *                                                                    757
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               758
C    Geant MATERIAL : AIR ===> inherited arguments used <===              759
         CALL AGSRESET                                                    759
         AG_TITLE = 'AIR'                                                 759
         AG_EXNAME = 'MATERIAL'                                           759
         AG_PARLIST = 'NONE'                                              759
         CALL AXMATERIAL                                                  759
C    *                                                                    759
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       759
         CALL AGSRESET                                                    760
         AG_SEEN = 0                                                      760
         AG_COLO = 1                                                      760
         AG_TITLE = 'SHLB'                                                760
         AG_EXNAME = 'ATTRIBUTE'                                          760
         AG_PARLIST = 'SEEN_COLO'                                         760
         CALL AXATTRIBUTE                                                 760
C    *                                                                    760
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  760
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  760
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  760
C    X <===                                                               760
         CALL AGSRESET                                                    764
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         764
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  764
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  764
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  764
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  764
         AG_TITLE = 'CONE'                                                764
         AG_EXNAME = 'SHAPE'                                              764
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            764
         CALL AXSHAPE                                                     764
C    *                                                                    764
C    Create SHMB :                                                        765
C    Call To Local Procedure SHMB                                         765
         ASSIGN 6060 TO BLOCK_SHMB                                        765
         GO TO  6050                                                      765
6060     CONTINUE                                                         765
C    *                                                                    765
5951  CALL AGSPOPB                                                        766
      GO TO BLOCK_SHLB, (5950 ,5960)                                      766
C End of Local Procedure                                                  766
C ----------------------------------------------------------------------  767
C ------------ SHMB                                                       767
C Begin Local Procedure SHMB                                              768
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           768
6050  CONTINUE                                                            768
         AG_TITLE='SHMB'                                                  768
         AG_MODULE=MODULE                                                 768
         AG_MARK='V00'                                                    768
         CALL AGSPUSH                                                     768
         AG_MODULE=MODULE                                                 768
C    *                                                                    768
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               769
         RMIN=SSUP_CON4IDMN + ROFFSET                                     770
         RMAX=SSUP_CON4IDMX + ROFFSET                                     771
         ZMIN=SSUP_CONE3ZMX                                               772
         ZMAX=SSUP_CONE4ZMX                                               773
         CONE_LEN =(ZMAX - ZMIN)*(ZMAX - ZMIN)                            774
         CONE_LEN=CONE_LEN+(RMAX - RMIN)*(RMAX - RMIN)                    775
         CONE_LEN = SQRT(CONE_LEN)                                        776
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  777
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  778
         ANGLE=ASIN(CONE_SIN)*180/3.1416                                  779
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             780
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     781
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       782
         CALL AGSRESET                                                    782
         AG_SEEN = 0                                                      782
         AG_COLO = 1                                                      782
         AG_TITLE = 'SHMB'                                                782
         AG_EXNAME = 'ATTRIBUTE'                                          782
         AG_PARLIST = 'SEEN_COLO'                                         782
         CALL AXATTRIBUTE                                                 782
C    *                                                                    782
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    782
         CALL AGSRESET                                                    784
         AG_NDIV = SSUP_NHOSES                                            784
         AG_IAXIS = 2                                                     784
         AG_C0 = 0.0                                                      784
         AG_TITLE = 'DIVISION'                                            784
         AG_EXNAME = 'SHAPE'                                              784
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     784
         CALL AXSHAPE                                                     784
C    *                                                                    784
C    Create SWHO :                                                        785
C    Call To Local Procedure SWHO                                         785
         ASSIGN 6070 TO BLOCK_SWHO                                        785
         GO TO  6030                                                      785
6070     CONTINUE                                                         785
C    *                                                                    785
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       786
         CALL AGSRESET                                                    786
         AG_X = XPOS                                                      786
         AG_Y = 0                                                         786
         AG_Z = ZPOS                                                      786
         AG_ALPHAN=AG_ALPHAN+1                                            786
         AG_ALPHAK(AG_ALPHAN)=2                                           786
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       786
         AG_TITLE = 'SWHO'                                                786
         AG_EXNAME = 'POSITION'                                           786
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      786
         CALL AXPOSITION                                                  786
C    *                                                                    786
6051  CALL AGSPOPB                                                        787
      GO TO BLOCK_SHMB, (6050 ,6060)                                      787
C End of Local Procedure                                                  787
C ----------------------------------------------------------------------  788
C ------------ SWHO                                                       788
C Begin Local Procedure SWHO                                              789
C SWHO IS A WATER HOSE                                                    789
6030  CONTINUE                                                            789
         AG_TITLE='SWHO'                                                  789
         AG_MODULE=MODULE                                                 789
         AG_MARK='V00'                                                    789
         CALL AGSPUSH                                                     789
         AG_MODULE=MODULE                                                 789
C    *                                                                    789
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       790
         CALL AGSRESET                                                    790
         AG_SEEN = 1                                                      790
         AG_COLO = 3                                                      790
         AG_TITLE = 'SWHO'                                                790
         AG_EXNAME = 'ATTRIBUTE'                                          790
         AG_PARLIST = 'SEEN_COLO'                                         790
         CALL AXATTRIBUTE                                                 790
C    *                                                                    790
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           790
         CALL AGSRESET                                                    791
         AG_A = 12                                                        791
         AG_Z = 6                                                         791
         AG_W = 1                                                         791
         AG_TITLE = 'C'                                                   791
         AG_EXNAME = 'COMPONENT'                                          791
         AG_PARLIST = 'A_Z_W'                                             791
         CALL AXCOMPONENT                                                 791
C    *                                                                    791
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           791
         CALL AGSRESET                                                    792
         AG_A = 1                                                         792
         AG_Z = 1                                                         792
         AG_W = 2                                                         792
         AG_TITLE = 'H2'                                                  792
         AG_EXNAME = 'COMPONENT'                                          792
         AG_PARLIST = 'A_Z_W'                                             792
         CALL AXCOMPONENT                                                 792
C    *                                                                    792
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             792
         CALL AGSRESET                                                    793
         AG_DENS = 0.935                                                  793
         AG_TITLE = 'CH2'                                                 793
         AG_EXNAME = 'MIXTURE'                                            793
         AG_PARLIST = 'DENS'                                              793
         CALL AXMIXTURE                                                   793
C    *                                                                    793
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       793
         CALL AGSRESET                                                    794
         AG_SEEN = 1                                                      794
         AG_COLO = 3                                                      794
         AG_TITLE = 'SWHO'                                                794
         AG_EXNAME = 'ATTRIBUTE'                                          794
         AG_PARLIST = 'SEEN_COLO'                                         794
         CALL AXATTRIBUTE                                                 794
C    *                                                                    794
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  794
C    =                                                                    794
         CALL AGSRESET                                                    795
         AG_RMIN = 0                                                      795
         AG_RMAX = SSUP_HOSRMX                                            795
         AG_DZ = 0.5*CONE_LEN                                             795
         AG_TITLE = 'TUBE'                                                795
         AG_EXNAME = 'SHAPE'                                              795
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      795
         CALL AXSHAPE                                                     795
C    *                                                                    795
C    Create SHWA :                                                        796
C    Call To Local Procedure SHWA                                         796
         ASSIGN 6090 TO BLOCK_SHWA                                        796
         GO TO  6080                                                      796
6090     CONTINUE                                                         796
C    *                                                                    796
C    Geant POSITION : SHWA ===> inherited arguments used <===             797
         CALL AGSRESET                                                    797
         AG_TITLE = 'SHWA'                                                797
         AG_EXNAME = 'POSITION'                                           797
         AG_PARLIST = 'NONE'                                              797
         CALL AXPOSITION                                                  797
C    *                                                                    797
6031  CALL AGSPOPB                                                        798
      GO TO BLOCK_SWHO, (6030 ,6040,6070)                                 798
C End of Local Procedure                                                  798
C ----------------------------------------------------------------------  799
C ------------ SHWA                                                       799
C Begin Local Procedure SHWA                                              800
C SHWA IS THE WATER IN THE HOSE                                           800
6080  CONTINUE                                                            800
         AG_TITLE='SHWA'                                                  800
         AG_MODULE=MODULE                                                 800
         AG_MARK='V00'                                                    800
         CALL AGSPUSH                                                     800
         AG_MODULE=MODULE                                                 800
C    *                                                                    800
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       801
         CALL AGSRESET                                                    801
         AG_SEEN = 1                                                      801
         AG_COLO = 6                                                      801
         AG_TITLE = 'SHWA'                                                801
         AG_EXNAME = 'ATTRIBUTE'                                          801
         AG_PARLIST = 'SEEN_COLO'                                         801
         CALL AXATTRIBUTE                                                 801
C    *                                                                    801
C    Geant MATERIAL : WATER ===> inherited arguments used <===            801
         CALL AGSRESET                                                    802
         AG_TITLE = 'WATER'                                               802
         AG_EXNAME = 'MATERIAL'                                           802
         AG_PARLIST = 'NONE'                                              802
         CALL AXMATERIAL                                                  802
C    *                                                                    802
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        802
         CALL AGSRESET                                                    803
         AG_RMAX = SSUP_HOSRMN                                            803
         AG_TITLE = 'TUBE'                                                803
         AG_EXNAME = 'SHAPE'                                              803
         AG_PARLIST = 'RMAX'                                              803
         CALL AXSHAPE                                                     803
C    *                                                                    803
6081  CALL AGSPOPB                                                        804
      GO TO BLOCK_SHWA, (6080 ,6090)                                      804
C End of Local Procedure                                                  804
C ----------------------------------------------------------------------  805
C ------------ SCMY                                                       805
C Begin Local Procedure SCMY                                              806
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            806
5970  CONTINUE                                                            806
         AG_TITLE='SCMY'                                                  806
         AG_MODULE=MODULE                                                 806
         AG_MARK='V00'                                                    806
         CALL AGSPUSH                                                     806
         AG_MODULE=MODULE                                                 806
C    *                                                                    806
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             807
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          808
         CALL AGSRESET                                                    808
         AG_A = 12                                                        808
         AG_Z = 6                                                         808
         AG_W = 5                                                         808
         AG_TITLE = 'C5'                                                  808
         AG_EXNAME = 'COMPONENT'                                          808
         AG_PARLIST = 'A_Z_W'                                             808
         CALL AXCOMPONENT                                                 808
C    *                                                                    808
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           808
         CALL AGSRESET                                                    809
         AG_A = 1                                                         809
         AG_Z = 1                                                         809
         AG_W = 4                                                         809
         AG_TITLE = 'H4'                                                  809
         AG_EXNAME = 'COMPONENT'                                          809
         AG_PARLIST = 'A_Z_W'                                             809
         CALL AXCOMPONENT                                                 809
C    *                                                                    809
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          809
         CALL AGSRESET                                                    810
         AG_A = 16                                                        810
         AG_Z = 8                                                         810
         AG_W = 2                                                         810
         AG_TITLE = 'O2'                                                  810
         AG_EXNAME = 'COMPONENT'                                          810
         AG_PARLIST = 'A_Z_W'                                             810
         CALL AXCOMPONENT                                                 810
C    *                                                                    810
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            810
         CALL AGSRESET                                                    811
         AG_DENS = 1.39                                                   811
         AG_TITLE = 'MYLAR'                                               811
         AG_EXNAME = 'MIXTURE'                                            811
         AG_PARLIST = 'DENS'                                              811
         CALL AXMIXTURE                                                   811
C    *                                                                    811
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       811
         CALL AGSRESET                                                    812
         AG_SEEN = 1                                                      812
         AG_COLO = 3                                                      812
         AG_TITLE = 'SCMY'                                                812
         AG_EXNAME = 'ATTRIBUTE'                                          812
         AG_PARLIST = 'SEEN_COLO'                                         812
         CALL AXATTRIBUTE                                                 812
C    *                                                                    812
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  812
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  812
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  812
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  812
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     812
         CALL AGSRESET                                                    818
         AG_PHI1 = 0                                                      818
         AG_DPHI = 360                                                    818
         AG_NZ = 3                                                        818
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              818
         AG_ZI(2)=SSUP_CONE3ZMX                                           818
         AG_ZI(3)=SSUP_CONE4ZMX                                           818
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  818
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  818
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  818
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    818
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    818
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    818
         AG_TITLE = 'PCON'                                                818
         AG_EXNAME = 'SHAPE'                                              818
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           818
         CALL AXSHAPE                                                     818
C    *                                                                    818
5971  CALL AGSPOPB                                                        819
      GO TO BLOCK_SCMY, (5970 ,5980)                                      819
C End of Local Procedure                                                  819
C ----------------------------------------------------------------------  820
C ------------ SCAL                                                       820
C Begin Local Procedure SCAL                                              821
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     821
5990  CONTINUE                                                            821
         AG_TITLE='SCAL'                                                  821
         AG_MODULE=MODULE                                                 821
         AG_MARK='V00'                                                    821
         CALL AGSPUSH                                                     821
         AG_MODULE=MODULE                                                 821
C    *                                                                    821
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           822
     *   SSUP_WRPMYTHK                                                    822
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        823
         CALL AGSRESET                                                    823
         AG_TITLE = 'ALUMINIUM'                                           823
         AG_EXNAME = 'MATERIAL'                                           823
         AG_PARLIST = 'NONE'                                              823
         CALL AXMATERIAL                                                  823
C    *                                                                    823
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       823
         CALL AGSRESET                                                    824
         AG_SEEN = 1                                                      824
         AG_COLO = 2                                                      824
         AG_TITLE = 'SCAL'                                                824
         AG_EXNAME = 'ATTRIBUTE'                                          824
         AG_PARLIST = 'SEEN_COLO'                                         824
         CALL AXATTRIBUTE                                                 824
C    *                                                                    824
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  824
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  824
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  824
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  824
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     824
         CALL AGSRESET                                                    830
         AG_PHI1 = 0                                                      830
         AG_DPHI = 360                                                    830
         AG_NZ = 3                                                        830
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              830
         AG_ZI(2)=SSUP_CONE3ZMX                                           830
         AG_ZI(3)=SSUP_CONE4ZMX                                           830
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  830
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  830
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  830
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    830
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    830
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    830
         AG_TITLE = 'PCON'                                                830
         AG_EXNAME = 'SHAPE'                                              830
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           830
         CALL AXSHAPE                                                     830
C    *                                                                    830
5991  CALL AGSPOPB                                                        831
      GO TO BLOCK_SCAL, (5990 ,6000)                                      831
C End of Local Procedure                                                  831
C ----------------------------------------------------------------------  832
C ------------ SWMM                                                       832
C Begin Local Procedure SWMM                                              833
C SWMM IS THE WATER MANIFOLD MOTHER                                       833
5150  CONTINUE                                                            833
         AG_TITLE='SWMM'                                                  833
         AG_MODULE=MODULE                                                 833
         AG_MARK='V00'                                                    833
         CALL AGSPUSH                                                     833
         AG_MODULE=MODULE                                                 833
C    *                                                                    833
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     834
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     835
C    Geant MATERIAL : AIR ===> inherited arguments used <===              836
         CALL AGSRESET                                                    836
         AG_TITLE = 'AIR'                                                 836
         AG_EXNAME = 'MATERIAL'                                           836
         AG_PARLIST = 'NONE'                                              836
         CALL AXMATERIAL                                                  836
C    *                                                                    836
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       836
         CALL AGSRESET                                                    837
         AG_SEEN = 0                                                      837
         AG_COLO = 1                                                      837
         AG_TITLE = 'SWMM'                                                837
         AG_EXNAME = 'ATTRIBUTE'                                          837
         AG_PARLIST = 'SEEN_COLO'                                         837
         CALL AXATTRIBUTE                                                 837
C    *                                                                    837
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  837
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   837
         CALL AGSRESET                                                    840
         AG_PHI1 = 0                                                      840
         AG_DPHI = 360                                                    840
         AG_NPDIV = 18                                                    840
         AG_NZ = 2                                                        840
         AG_ZI(1)=-SWAM_LEN/2                                             840
         AG_ZI(2)=+SWAM_LEN/2                                             840
         AG_RMN(1)=RIN                                                    840
         AG_RMN(2)=RIN                                                    840
         AG_RMX(1)=ROU                                                    840
         AG_RMX(2)=ROU                                                    840
         AG_TITLE = 'PGON'                                                840
         AG_EXNAME = 'SHAPE'                                              840
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     840
         CALL AXSHAPE                                                     840
C    *                                                                    840
C    Create SWMB :                                                        841
C    Call To Local Procedure SWMB                                         841
         ASSIGN 6110 TO BLOCK_SWMB                                        841
         GO TO  6100                                                      841
6110     CONTINUE                                                         841
C    *                                                                    841
C    Geant POSITION : SWMB ===> inherited arguments used <===             842
         CALL AGSRESET                                                    842
         AG_TITLE = 'SWMB'                                                842
         AG_EXNAME = 'POSITION'                                           842
         AG_PARLIST = 'NONE'                                              842
         CALL AXPOSITION                                                  842
C    *                                                                    842
C    Create SWMT :                                                        843
C    Call To Local Procedure SWMT                                         843
         ASSIGN 6130 TO BLOCK_SWMT                                        843
         GO TO  6120                                                      843
6130     CONTINUE                                                         843
C    *                                                                    843
C    Geant POSITION : SWMT ===> inherited arguments used <===             844
         CALL AGSRESET                                                    844
         AG_TITLE = 'SWMT'                                                844
         AG_EXNAME = 'POSITION'                                           844
         AG_PARLIST = 'NONE'                                              844
         CALL AXPOSITION                                                  844
C    *                                                                    844
C    Create SWMS :                                                        845
C    Call To Local Procedure SWMS                                         845
         ASSIGN 6150 TO BLOCK_SWMS                                        845
         GO TO  6140                                                      845
6150     CONTINUE                                                         845
C    *                                                                    845
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         846
         CALL AGSRESET                                                    846
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                846
         AG_TITLE = 'SWMS'                                                846
         AG_EXNAME = 'POSITION'                                           846
         AG_PARLIST = 'Z'                                                 846
         CALL AXPOSITION                                                  846
C    *                                                                    846
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          846
         CALL AGSRESET                                                    847
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 847
         AG_TITLE = 'SWMS'                                                847
         AG_EXNAME = 'POSITION'                                           847
         AG_PARLIST = 'Z'                                                 847
         CALL AXPOSITION                                                  847
C    *                                                                    847
C    Create SWMW :                                                        848
C    Call To Local Procedure SWMW                                         848
         ASSIGN 6170 TO BLOCK_SWMW                                        848
         GO TO  6160                                                      848
6170     CONTINUE                                                         848
C    *                                                                    848
C    Geant POSITION : SWMW ===> inherited arguments used <===             849
         CALL AGSRESET                                                    849
         AG_TITLE = 'SWMW'                                                849
         AG_EXNAME = 'POSITION'                                           849
         AG_PARLIST = 'NONE'                                              849
         CALL AXPOSITION                                                  849
C    *                                                                    849
C    Create SOTB :                                                        850
C    Call To Local Procedure SOTB                                         850
         ASSIGN 6190 TO BLOCK_SOTB                                        850
         GO TO  6180                                                      850
6190     CONTINUE                                                         850
C    *                                                                    850
C    Geant POSITION : SOTB ===> inherited arguments used <===             851
         CALL AGSRESET                                                    851
         AG_TITLE = 'SOTB'                                                851
         AG_EXNAME = 'POSITION'                                           851
         AG_PARLIST = 'NONE'                                              851
         CALL AXPOSITION                                                  851
C    *                                                                    851
C    Create SITB :                                                        852
C    Call To Local Procedure SITB                                         852
         ASSIGN 6210 TO BLOCK_SITB                                        852
         GO TO  6200                                                      852
6210     CONTINUE                                                         852
C    *                                                                    852
C    Geant POSITION : SITB ===> inherited arguments used <===             853
         CALL AGSRESET                                                    853
         AG_TITLE = 'SITB'                                                853
         AG_EXNAME = 'POSITION'                                           853
         AG_PARLIST = 'NONE'                                              853
         CALL AXPOSITION                                                  853
C    *                                                                    853
5151  CALL AGSPOPB                                                        854
      GO TO BLOCK_SWMM, (5150 ,5160)                                      854
C End of Local Procedure                                                  854
C ----------------------------------------------------------------------  855
C ------------ SWMB                                                       855
C Begin Local Procedure SWMB                                              856
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       856
6100  CONTINUE                                                            856
         AG_TITLE='SWMB'                                                  856
         AG_MODULE=MODULE                                                 856
         AG_MARK='V00'                                                    856
         CALL AGSPUSH                                                     856
         AG_MODULE=MODULE                                                 856
C    *                                                                    856
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        857
         CALL AGSRESET                                                    857
         AG_TITLE = 'ALUMINIUM'                                           857
         AG_EXNAME = 'MATERIAL'                                           857
         AG_PARLIST = 'NONE'                                              857
         CALL AXMATERIAL                                                  857
C    *                                                                    857
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       857
         CALL AGSRESET                                                    858
         AG_SEEN = 1                                                      858
         AG_COLO = 2                                                      858
         AG_TITLE = 'SWMB'                                                858
         AG_EXNAME = 'ATTRIBUTE'                                          858
         AG_PARLIST = 'SEEN_COLO'                                         858
         CALL AXATTRIBUTE                                                 858
C    *                                                                    858
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  858
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  858
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          858
         CALL AGSRESET                                                    862
         AG_PHI1 = 0                                                      862
         AG_DPHI = 360                                                    862
         AG_NPDIV = 18                                                    862
         AG_NZ = 2                                                        862
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                862
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 862
         AG_RMN(1)=SWAM_RMIN                                              862
         AG_RMN(2)=SWAM_RMIN                                              862
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 862
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 862
         AG_TITLE = 'PGON'                                                862
         AG_EXNAME = 'SHAPE'                                              862
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     862
         CALL AXSHAPE                                                     862
C    *                                                                    862
6101  CALL AGSPOPB                                                        863
      GO TO BLOCK_SWMB, (6100 ,6110)                                      863
C End of Local Procedure                                                  863
C ----------------------------------------------------------------------  864
C ------------ SWMT                                                       864
C Begin Local Procedure SWMT                                              865
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            865
6120  CONTINUE                                                            865
         AG_TITLE='SWMT'                                                  865
         AG_MODULE=MODULE                                                 865
         AG_MARK='V00'                                                    865
         CALL AGSPUSH                                                     865
         AG_MODULE=MODULE                                                 865
C    *                                                                    865
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        866
         CALL AGSRESET                                                    866
         AG_TITLE = 'ALUMINIUM'                                           866
         AG_EXNAME = 'MATERIAL'                                           866
         AG_PARLIST = 'NONE'                                              866
         CALL AXMATERIAL                                                  866
C    *                                                                    866
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       866
         CALL AGSRESET                                                    867
         AG_SEEN = 1                                                      867
         AG_COLO = 2                                                      867
         AG_TITLE = 'SWMT'                                                867
         AG_EXNAME = 'ATTRIBUTE'                                          867
         AG_PARLIST = 'SEEN_COLO'                                         867
         CALL AXATTRIBUTE                                                 867
C    *                                                                    867
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  867
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  867
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          867
         CALL AGSRESET                                                    871
         AG_PHI1 = 0                                                      871
         AG_DPHI = 360                                                    871
         AG_NPDIV = 18                                                    871
         AG_NZ = 2                                                        871
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                871
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 871
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 871
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 871
         AG_RMX(1)=SWAM_RMAX                                              871
         AG_RMX(2)=SWAM_RMAX                                              871
         AG_TITLE = 'PGON'                                                871
         AG_EXNAME = 'SHAPE'                                              871
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     871
         CALL AXSHAPE                                                     871
C    *                                                                    871
6121  CALL AGSPOPB                                                        872
      GO TO BLOCK_SWMT, (6120 ,6130)                                      872
C End of Local Procedure                                                  872
C ----------------------------------------------------------------------  873
C ------------ SWMS                                                       873
C Begin Local Procedure SWMS                                              874
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  874
6140  CONTINUE                                                            874
         AG_TITLE='SWMS'                                                  874
         AG_MODULE=MODULE                                                 874
         AG_MARK='V00'                                                    874
         CALL AGSPUSH                                                     874
         AG_MODULE=MODULE                                                 874
C    *                                                                    874
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        875
         CALL AGSRESET                                                    875
         AG_TITLE = 'ALUMINIUM'                                           875
         AG_EXNAME = 'MATERIAL'                                           875
         AG_PARLIST = 'NONE'                                              875
         CALL AXMATERIAL                                                  875
C    *                                                                    875
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       875
         CALL AGSRESET                                                    876
         AG_SEEN = 1                                                      876
         AG_COLO = 2                                                      876
         AG_TITLE = 'SWMS'                                                876
         AG_EXNAME = 'ATTRIBUTE'                                          876
         AG_PARLIST = 'SEEN_COLO'                                         876
         CALL AXATTRIBUTE                                                 876
C    *                                                                    876
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  876
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  876
C    WAM_RMAX} <===                                                       876
         CALL AGSRESET                                                    879
         AG_PHI1 = 0                                                      879
         AG_DPHI = 360                                                    879
         AG_NPDIV = 18                                                    879
         AG_NZ = 2                                                        879
         AG_ZI(1)=-SWAM_WALLTHK/2                                         879
         AG_ZI(2)=SWAM_WALLTHK/2                                          879
         AG_RMN(1)=SWAM_RMIN                                              879
         AG_RMN(2)=SWAM_RMIN                                              879
         AG_RMX(1)=SWAM_RMAX                                              879
         AG_RMX(2)=SWAM_RMAX                                              879
         AG_TITLE = 'PGON'                                                879
         AG_EXNAME = 'SHAPE'                                              879
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     879
         CALL AXSHAPE                                                     879
C    *                                                                    879
6141  CALL AGSPOPB                                                        880
      GO TO BLOCK_SWMS, (6140 ,6150)                                      880
C End of Local Procedure                                                  880
C ----------------------------------------------------------------------  881
C ------------ SWMW                                                       881
C Begin Local Procedure SWMW                                              882
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 882
6160  CONTINUE                                                            882
         AG_TITLE='SWMW'                                                  882
         AG_MODULE=MODULE                                                 882
         AG_MARK='V00'                                                    882
         CALL AGSPUSH                                                     882
         AG_MODULE=MODULE                                                 882
C    *                                                                    882
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       883
         CALL AGSRESET                                                    883
         AG_SEEN = 1                                                      883
         AG_COLO = 6                                                      883
         AG_TITLE = 'SWMW'                                                883
         AG_EXNAME = 'ATTRIBUTE'                                          883
         AG_PARLIST = 'SEEN_COLO'                                         883
         CALL AXATTRIBUTE                                                 883
C    *                                                                    883
C    Geant MATERIAL : WATER ===> inherited arguments used <===            883
         CALL AGSRESET                                                    884
         AG_TITLE = 'WATER'                                               884
         AG_EXNAME = 'MATERIAL'                                           884
         AG_PARLIST = 'NONE'                                              884
         CALL AXMATERIAL                                                  884
C    *                                                                    884
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  884
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  884
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  884
C    -SWAM_WALLTHK} <===                                                  884
         CALL AGSRESET                                                    889
         AG_PHI1 = 0                                                      889
         AG_DPHI = 360                                                    889
         AG_NPDIV = 18                                                    889
         AG_NZ = 2                                                        889
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                889
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 889
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 889
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 889
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 889
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 889
         AG_TITLE = 'PGON'                                                889
         AG_EXNAME = 'SHAPE'                                              889
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     889
         CALL AXSHAPE                                                     889
C    *                                                                    889
6161  CALL AGSPOPB                                                        890
      GO TO BLOCK_SWMW, (6160 ,6170)                                      890
C End of Local Procedure                                                  890
C ----------------------------------------------------------------------  891
C ------------ SOTB                                                       891
C Begin Local Procedure SOTB                                              892
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            892
6180  CONTINUE                                                            892
         AG_TITLE='SOTB'                                                  892
         AG_MODULE=MODULE                                                 892
         AG_MARK='V00'                                                    892
         CALL AGSPUSH                                                     892
         AG_MODULE=MODULE                                                 892
C    *                                                                    892
C    Geant MATERIAL : G10 ===> inherited arguments used <===              893
         CALL AGSRESET                                                    893
         AG_TITLE = 'G10'                                                 893
         AG_EXNAME = 'MATERIAL'                                           893
         AG_PARLIST = 'NONE'                                              893
         CALL AXMATERIAL                                                  893
C    *                                                                    893
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       893
         CALL AGSRESET                                                    894
         AG_SEEN = 1                                                      894
         AG_COLO = 3                                                      894
         AG_TITLE = 'SOTB'                                                894
         AG_EXNAME = 'ATTRIBUTE'                                          894
         AG_PARLIST = 'SEEN_COLO'                                         894
         CALL AXATTRIBUTE                                                 894
C    *                                                                    894
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  894
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  894
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    894
         CALL AGSRESET                                                    897
         AG_PHI1 = 0                                                      897
         AG_DPHI = 360                                                    897
         AG_NPDIV = 18                                                    897
         AG_NZ = 2                                                        897
         AG_ZI(1)=-SWAM_LEN/2                                             897
         AG_ZI(2)=+SWAM_LEN/2                                             897
         AG_RMN(1)=SWAM_RMAX                                              897
         AG_RMN(2)=SWAM_RMAX                                              897
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 897
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 897
         AG_TITLE = 'PGON'                                                897
         AG_EXNAME = 'SHAPE'                                              897
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     897
         CALL AXSHAPE                                                     897
C    *                                                                    897
6181  CALL AGSPOPB                                                        898
      GO TO BLOCK_SOTB, (6180 ,6190)                                      898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ SITB                                                       899
C Begin Local Procedure SITB                                              900
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            900
6200  CONTINUE                                                            900
         AG_TITLE='SITB'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           901
         CALL AGSRESET                                                    901
         AG_A = 12                                                        901
         AG_Z = 6                                                         901
         AG_W = 1                                                         901
         AG_TITLE = 'C'                                                   901
         AG_EXNAME = 'COMPONENT'                                          901
         AG_PARLIST = 'A_Z_W'                                             901
         CALL AXCOMPONENT                                                 901
C    *                                                                    901
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           901
         CALL AGSRESET                                                    902
         AG_A = 1                                                         902
         AG_Z = 1                                                         902
         AG_W = 2                                                         902
         AG_TITLE = 'H2'                                                  902
         AG_EXNAME = 'COMPONENT'                                          902
         AG_PARLIST = 'A_Z_W'                                             902
         CALL AXCOMPONENT                                                 902
C    *                                                                    902
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             902
         CALL AGSRESET                                                    903
         AG_DENS = 0.935                                                  903
         AG_TITLE = 'CH2'                                                 903
         AG_EXNAME = 'MIXTURE'                                            903
         AG_PARLIST = 'DENS'                                              903
         CALL AXMIXTURE                                                   903
C    *                                                                    903
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       903
         CALL AGSRESET                                                    904
         AG_SEEN = 1                                                      904
         AG_COLO = 3                                                      904
         AG_TITLE = 'SITB'                                                904
         AG_EXNAME = 'ATTRIBUTE'                                          904
         AG_PARLIST = 'SEEN_COLO'                                         904
         CALL AXATTRIBUTE                                                 904
C    *                                                                    904
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  904
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  904
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   904
         CALL AGSRESET                                                    908
         AG_PHI1 = 0                                                      908
         AG_DPHI = 360                                                    908
         AG_NPDIV = 18                                                    908
         AG_NZ = 2                                                        908
         AG_ZI(1)=-SWAM_LEN/2                                             908
         AG_ZI(2)=+SWAM_LEN/2                                             908
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 908
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 908
         AG_RMX(1)=SWAM_RMIN                                              908
         AG_RMX(2)=SWAM_RMIN                                              908
         AG_TITLE = 'PGON'                                                908
         AG_EXNAME = 'SHAPE'                                              908
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     908
         CALL AXSHAPE                                                     908
C    *                                                                    908
6201  CALL AGSPOPB                                                        909
      GO TO BLOCK_SITB, (6200 ,6210)                                      909
C End of Local Procedure                                                  909
C ----------------------------------------------------------------------  910
C ------------ SBWC                                                       910
C Begin Local Procedure SBWC                                              911
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           911
5170  CONTINUE                                                            911
         AG_TITLE='SBWC'                                                  911
         AG_MODULE=MODULE                                                 911
         AG_MARK='V00'                                                    911
         CALL AGSPUSH                                                     911
         AG_MODULE=MODULE                                                 911
C    *                                                                    911
         RIN=SWAM_RMIN                                                    912
         ROU=SSUP_CON1IDMN                                                913
C    Geant MATERIAL : AIR ===> inherited arguments used <===              914
         CALL AGSRESET                                                    914
         AG_TITLE = 'AIR'                                                 914
         AG_EXNAME = 'MATERIAL'                                           914
         AG_PARLIST = 'NONE'                                              914
         CALL AXMATERIAL                                                  914
C    *                                                                    914
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       914
         CALL AGSRESET                                                    915
         AG_SEEN = 0                                                      915
         AG_COLO = 1                                                      915
         AG_TITLE = 'SBWC'                                                915
         AG_EXNAME = 'ATTRIBUTE'                                          915
         AG_PARLIST = 'SEEN_COLO'                                         915
         CALL AXATTRIBUTE                                                 915
C    *                                                                    915
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  915
C    MIN+SWAM_LEN))/2 <===                                                915
         CALL AGSRESET                                                    916
         AG_RMIN = RIN                                                    916
         AG_RMAX = ROU                                                    916
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   916
         AG_TITLE = 'TUBE'                                                916
         AG_EXNAME = 'SHAPE'                                              916
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      916
         CALL AXSHAPE                                                     916
C    *                                                                    916
C    Create SWCM :                                                        917
C    Call To Local Procedure SWCM                                         917
         ASSIGN 6230 TO BLOCK_SWCM                                        917
         GO TO  6220                                                      917
6230     CONTINUE                                                         917
C    *                                                                    917
5171  CALL AGSPOPB                                                        918
      GO TO BLOCK_SBWC, (5170 ,5180)                                      918
C End of Local Procedure                                                  918
C ----------------------------------------------------------------------  919
C ------------ SWCM                                                       919
C Begin Local Procedure SWCM                                              920
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   920
6220  CONTINUE                                                            920
         AG_TITLE='SWCM'                                                  920
         AG_MODULE=MODULE                                                 920
         AG_MARK='V00'                                                    920
         CALL AGSPUSH                                                     920
         AG_MODULE=MODULE                                                 920
C    *                                                                    920
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       921
         CALL AGSRESET                                                    921
         AG_SEEN = 0                                                      921
         AG_COLO = 1                                                      921
         AG_TITLE = 'SWCM'                                                921
         AG_EXNAME = 'ATTRIBUTE'                                          921
         AG_PARLIST = 'SEEN_COLO'                                         921
         CALL AXATTRIBUTE                                                 921
C    *                                                                    921
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 921
         CALL AGSRESET                                                    922
         AG_NDIV = 3                                                      922
         AG_IAXIS = 2                                                     922
         AG_C0 = 0                                                        922
         AG_TITLE = 'DIVISION'                                            922
         AG_EXNAME = 'SHAPE'                                              922
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     922
         CALL AXSHAPE                                                     922
C    *                                                                    922
C    Create SXAI :                                                        923
C    Call To Local Procedure SXAI                                         923
         ASSIGN 6250 TO BLOCK_SXAI                                        923
         GO TO  6240                                                      923
6250     CONTINUE                                                         923
C    *                                                                    923
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  924
C    /2+SSUP_BRATHK/2) <===                                               924
         CALL AGSRESET                                                    924
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   924
         AG_TITLE = 'SXAI'                                                924
         AG_EXNAME = 'POSITION'                                           924
         AG_PARLIST = 'Z'                                                 924
         CALL AXPOSITION                                                  924
C    *                                                                    924
C    Create SXBI :                                                        925
C    Call To Local Procedure SXBI                                         925
         ASSIGN 6270 TO BLOCK_SXBI                                        925
         GO TO  6260                                                      925
6270     CONTINUE                                                         925
C    *                                                                    925
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    926
         CALL AGSRESET                                                    926
         AG_Z = (SSUP_BRATHK/2)                                           926
         AG_TITLE = 'SXBI'                                                926
         AG_EXNAME = 'POSITION'                                           926
         AG_PARLIST = 'Z'                                                 926
         CALL AXPOSITION                                                  926
C    *                                                                    926
6221  CALL AGSPOPB                                                        927
      GO TO BLOCK_SWCM, (6220 ,6230)                                      927
C End of Local Procedure                                                  927
C ----------------------------------------------------------------------  928
C ------------ SXAI                                                       928
C Begin Local Procedure SXAI                                              929
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      929
6240  CONTINUE                                                            929
         AG_TITLE='SXAI'                                                  929
         AG_MODULE=MODULE                                                 929
         AG_MARK='V00'                                                    929
         CALL AGSPUSH                                                     929
         AG_MODULE=MODULE                                                 929
C    *                                                                    929
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       930
         CALL AGSRESET                                                    930
         AG_SEEN = 1                                                      930
         AG_COLO = 2                                                      930
         AG_TITLE = 'SXAI'                                                930
         AG_EXNAME = 'ATTRIBUTE'                                          930
         AG_PARLIST = 'SEEN_COLO'                                         930
         CALL AXATTRIBUTE                                                 930
C    *                                                                    930
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        930
         CALL AGSRESET                                                    931
         AG_TITLE = 'ALUMINIUM'                                           931
         AG_EXNAME = 'MATERIAL'                                           931
         AG_PARLIST = 'NONE'                                              931
         CALL AXMATERIAL                                                  931
C    *                                                                    931
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   931
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         931
         CALL AGSRESET                                                    933
         AG_RMIN = SWAM_RMIN                                              933
         AG_RMAX = SSUP_CON1IDMN                                          933
         AG_PHI1 = -5                                                     933
         AG_PHI2 = 5                                                      933
         AG_DZ = SSUP_BRATHK/2                                            933
         AG_TITLE = 'TUBS'                                                933
         AG_EXNAME = 'SHAPE'                                              933
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            933
         CALL AXSHAPE                                                     933
C    *                                                                    933
6241  CALL AGSPOPB                                                        934
      GO TO BLOCK_SXAI, (6240 ,6250)                                      934
C End of Local Procedure                                                  934
C ----------------------------------------------------------------------  935
C ------------ SXBI                                                       935
C Begin Local Procedure SXBI                                              936
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     936
6260  CONTINUE                                                            936
         AG_TITLE='SXBI'                                                  936
         AG_MODULE=MODULE                                                 936
         AG_MARK='V00'                                                    936
         CALL AGSPUSH                                                     936
         AG_MODULE=MODULE                                                 936
C    *                                                                    936
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       937
         CALL AGSRESET                                                    937
         AG_SEEN = 1                                                      937
         AG_COLO = 2                                                      937
         AG_TITLE = 'SXBI'                                                937
         AG_EXNAME = 'ATTRIBUTE'                                          937
         AG_PARLIST = 'SEEN_COLO'                                         937
         CALL AXATTRIBUTE                                                 937
C    *                                                                    937
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        937
         CALL AGSRESET                                                    938
         AG_TITLE = 'ALUMINIUM'                                           938
         AG_EXNAME = 'MATERIAL'                                           938
         AG_PARLIST = 'NONE'                                              938
         CALL AXMATERIAL                                                  938
C    *                                                                    938
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  938
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  938
C    _BRATHK)/2) <===                                                     938
         CALL AGSRESET                                                    940
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              940
         AG_RMAX = SSUP_CON1IDMN                                          940
         AG_PHI1 = -5                                                     940
         AG_PHI2 = 5                                                      940
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     940
         AG_TITLE = 'TUBS'                                                940
         AG_EXNAME = 'SHAPE'                                              940
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            940
         CALL AXSHAPE                                                     940
C    *                                                                    940
6261  CALL AGSPOPB                                                        941
      GO TO BLOCK_SXBI, (6260 ,6270)                                      941
C End of Local Procedure                                                  941
C ----------------------------------------------------------------------  942
C ------------ SCRW                                                       942
C Begin Local Procedure SCRW                                              943
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   943
5780  CONTINUE                                                            943
         AG_TITLE='SCRW'                                                  943
         AG_MODULE=MODULE                                                 943
         AG_MARK='V00'                                                    943
         CALL AGSPUSH                                                     943
         AG_MODULE=MODULE                                                 943
C    *                                                                    943
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        944
         CALL AGSRESET                                                    944
         AG_TITLE = 'BERILLIUM'                                           944
         AG_EXNAME = 'MATERIAL'                                           944
         AG_PARLIST = 'NONE'                                              944
         CALL AXMATERIAL                                                  944
C    *                                                                    944
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       944
         CALL AGSRESET                                                    945
         AG_SEEN = 1                                                      945
         AG_COLO = 2                                                      945
         AG_TITLE = 'SCRW'                                                945
         AG_EXNAME = 'ATTRIBUTE'                                          945
         AG_PARLIST = 'SEEN_COLO'                                         945
         CALL AXATTRIBUTE                                                 945
C    *                                                                    945
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  945
C    NDRNG_Z) <===                                                        945
         CALL AGSRESET                                                    946
         AG_RMIN = 0                                                      946
         AG_RMAX = SSUP_ERJDIA/2                                          946
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   946
         AG_TITLE = 'TUBE'                                                946
         AG_EXNAME = 'SHAPE'                                              946
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      946
         CALL AXSHAPE                                                     946
C    *                                                                    946
5781  CALL AGSPOPB                                                        947
      GO TO BLOCK_SCRW, (5780 ,5790,5820)                                 947
C End of Local Procedure                                                  947
C ----------------------------------------------------------------------  948
C ------------ SFMO                                                       948
C Begin Local Procedure SFMO                                              949
C SFMO IS THE MOTHER OF THE FOURTH LAYER (STRIP DETECTORS)                949
5290  CONTINUE                                                            949
         AG_TITLE='SFMO'                                                  949
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
C    Geant ATTRIBUTE : SFMO ===> SEEN=0 COLO=1 <===                       950
         CALL AGSRESET                                                    951
         AG_SEEN = 0                                                      951
         AG_COLO = 1                                                      951
         AG_TITLE = 'SFMO'                                                951
         AG_EXNAME = 'ATTRIBUTE'                                          951
         AG_PARLIST = 'SEEN_COLO'                                         951
         CALL AXATTRIBUTE                                                 951
C    *                                                                    951
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  951
C    2 <===                                                               951
         CALL AGSRESET                                                    953
         AG_RMIN = SFPA_RMIN                                              953
         AG_RMAX = SFPA_RMAX                                              953
         AG_DZ = SFPA_LEN/2                                               953
         AG_TITLE = 'TUBE'                                                953
         AG_EXNAME = 'SHAPE'                                              953
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      953
         CALL AXSHAPE                                                     953
C    *                                                                    953
         DTHK=SFPA_SMTHK + SFPA_GPTHK                                     953
         RADTILT=(SFPA_TILT*PI)/180.                                      955
C    *                                                                    956
         ILAD=1                                                           956
            GO TO 6283                                                    956
6281        ILAD=ILAD+1                                                   956
6283        IF(ILAD-(20).GT.0) GO TO 6282                                 956
C       Create SFLM :                                                     957
C       Call To Local Procedure SFLM                                      957
            ASSIGN 6300 TO BLOCK_SFLM                                     957
            GO TO  6290                                                   957
6300        CONTINUE                                                      957
C       *                                                                 957
            ANG=(FLOAT(ILAD)-1)*2.0*PI/20.0                               958
C       Geant POSITION : SFLM ===> X=-(SFPA_RAD*SIN(ANG)+(DTHK*SIN(ANG+R  961
C       ADTILT))/2) Y=(SFPA_RAD*COS(ANG)+(DTHK*COS(ANG+RADTILT))/2) Z=0   961
C       ALPHAZ=(180.*ANG/PI)+SFPA_TILT <===                               961
            CALL AGSRESET                                                 961
            AG_X = -(SFPA_RAD*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/2)         961
            AG_Y = (SFPA_RAD*COS(ANG)+(DTHK*COS(ANG+RADTILT))/2)          961
            AG_Z = 0                                                      961
            AG_ALPHAN=AG_ALPHAN+1                                         961
            AG_ALPHAK(AG_ALPHAN)=3                                        961
            AG_ALPHAL(AG_ALPHAN)=(180.*ANG/PI)+SFPA_TILT                  961
            AG_TITLE = 'SFLM'                                             961
            AG_EXNAME = 'POSITION'                                        961
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   961
            CALL AXPOSITION                                               961
C       *                                                                 961
         GO TO 6281                                                       962
6282     CONTINUE                                                         962
5291  CALL AGSPOPB                                                        963
      GO TO BLOCK_SFMO, (5290 ,5300)                                      963
C End of Local Procedure                                                  963
C ----------------------------------------------------------------------  964
C ------------ SFLM                                                       964
C Begin Local Procedure SFLM                                              965
C SFLM IS THE MOTHER OF THE 4TH LAYER LADDER                              965
6290  CONTINUE                                                            965
         AG_TITLE='SFLM'                                                  965
         AG_MODULE=MODULE                                                 965
         AG_MARK='V00'                                                    965
         CALL AGSPUSH                                                     965
         AG_MODULE=MODULE                                                 965
C    *                                                                    965
C    Geant MATERIAL : AIR ===> inherited arguments used <===              966
         CALL AGSRESET                                                    966
         AG_TITLE = 'AIR'                                                 966
         AG_EXNAME = 'MATERIAL'                                           966
         AG_PARLIST = 'NONE'                                              966
         CALL AXMATERIAL                                                  966
C    *                                                                    966
C    Geant ATTRIBUTE : SFLM ===> SEEN=0 COLO=1 <===                       966
         CALL AGSRESET                                                    967
         AG_SEEN = 0                                                      967
         AG_COLO = 1                                                      967
         AG_TITLE = 'SFLM'                                                967
         AG_EXNAME = 'ATTRIBUTE'                                          967
         AG_PARLIST = 'SEEN_COLO'                                         967
         CALL AXATTRIBUTE                                                 967
C    *                                                                    967
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  967
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    967
         CALL AGSRESET                                                    969
         AG_DX = SFPA_DMWID/2                                             969
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     969
         AG_DZ = SFPA_SMLEN/2                                             969
         AG_TITLE = 'BOX'                                                 969
         AG_EXNAME = 'SHAPE'                                              969
         AG_PARLIST = 'DX_DY_DZ'                                          969
         CALL AXSHAPE                                                     969
C    *                                                                    969
C    Create SFDM :                                                        970
C    Call To Local Procedure SFDM                                         970
         ASSIGN 6320 TO BLOCK_SFDM                                        970
         GO TO  6310                                                      970
6320     CONTINUE                                                         970
C    *                                                                    970
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         971
         CALL AGSRESET                                                    971
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                971
         AG_TITLE = 'SFDM'                                                971
         AG_EXNAME = 'POSITION'                                           971
         AG_PARLIST = 'Y'                                                 971
         CALL AXPOSITION                                                  971
C    *                                                                    971
C    Create SFSM :                                                        972
C    Call To Local Procedure SFSM                                         972
         ASSIGN 6340 TO BLOCK_SFSM                                        972
         GO TO  6330                                                      972
6340     CONTINUE                                                         972
C    *                                                                    972
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          973
         CALL AGSRESET                                                    973
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 973
         AG_TITLE = 'SFSM'                                                973
         AG_EXNAME = 'POSITION'                                           973
         AG_PARLIST = 'Y'                                                 973
         CALL AXPOSITION                                                  973
C    *                                                                    973
6291  CALL AGSPOPB                                                        974
      GO TO BLOCK_SFLM, (6290 ,6300)                                      974
C End of Local Procedure                                                  974
C ----------------------------------------------------------------------  975
C ------------ SFDM                                                       975
C Begin Local Procedure SFDM                                              976
C SFDM IS THE MOTHER OF THE DETECTORS                                     976
6310  CONTINUE                                                            976
         AG_TITLE='SFDM'                                                  976
         AG_MODULE=MODULE                                                 976
         AG_MARK='V00'                                                    976
         CALL AGSPUSH                                                     976
         AG_MODULE=MODULE                                                 976
C    *                                                                    976
C    Geant MATERIAL : AIR ===> inherited arguments used <===              977
         CALL AGSRESET                                                    977
         AG_TITLE = 'AIR'                                                 977
         AG_EXNAME = 'MATERIAL'                                           977
         AG_PARLIST = 'NONE'                                              977
         CALL AXMATERIAL                                                  977
C    *                                                                    977
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=1 <===                       977
         CALL AGSRESET                                                    978
         AG_SEEN = 0                                                      978
         AG_COLO = 1                                                      978
         AG_TITLE = 'SFDM'                                                978
         AG_EXNAME = 'ATTRIBUTE'                                          978
         AG_PARLIST = 'SEEN_COLO'                                         978
         CALL AXATTRIBUTE                                                 978
C    *                                                                    978
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  978
C    EN/2 <===                                                            978
         CALL AGSRESET                                                    980
         AG_DX = SFPA_DMWID/2                                             980
         AG_DY = SFPA_DMTHK/2                                             980
         AG_DZ = SFPA_WPLEN/2                                             980
         AG_TITLE = 'BOX'                                                 980
         AG_EXNAME = 'SHAPE'                                              980
         AG_PARLIST = 'DX_DY_DZ'                                          980
         CALL AXSHAPE                                                     980
C    *                                                                    980
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              980
C    *                                                                    982
         IWAF=1                                                           982
            GO TO 6353                                                    982
6351        IWAF=IWAF+1                                                   982
6353        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 6352                          982
C       Create SFSW :                                                     983
C       Call To Local Procedure SFSW                                      983
            ASSIGN 6370 TO BLOCK_SFSW                                     983
            GO TO  6360                                                   983
6370        CONTINUE                                                      983
C       *                                                                 983
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  984
C       CKLEN <===                                                        984
            CALL AGSRESET                                                 984
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               984
            AG_TITLE = 'SFSW'                                             984
            AG_EXNAME = 'POSITION'                                        984
            AG_PARLIST = 'Z'                                              984
            CALL AXPOSITION                                               984
C       *                                                                 984
         GO TO 6351                                                       985
6352     CONTINUE                                                         985
6311  CALL AGSPOPB                                                        986
      GO TO BLOCK_SFDM, (6310 ,6320)                                      986
C End of Local Procedure                                                  986
C ----------------------------------------------------------------------  987
C ------------ SFSW                                                       987
C Begin Local Procedure SFSW                                              988
C SFSW IS A SINGLE WAFER CONTAINER                                        988
6360  CONTINUE                                                            988
         AG_TITLE='SFSW'                                                  988
         AG_MODULE=MODULE                                                 988
         AG_MARK='V00'                                                    988
         CALL AGSPUSH                                                     988
         AG_MODULE=MODULE                                                 988
C    *                                                                    988
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=1 <===                       989
         CALL AGSRESET                                                    989
         AG_SEEN = 0                                                      989
         AG_COLO = 1                                                      989
         AG_TITLE = 'SFSW'                                                989
         AG_EXNAME = 'ATTRIBUTE'                                          989
         AG_PARLIST = 'SEEN_COLO'                                         989
         CALL AXATTRIBUTE                                                 989
C    *                                                                    989
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=WAFPCKLE  989
C    N/2 <===                                                             989
         CALL AGSRESET                                                    991
         AG_DX = SFPA_DMWID/2                                             991
         AG_DY = SFPA_DMTHK/2                                             991
         AG_DZ = WAFPCKLEN/2                                              991
         AG_TITLE = 'BOX'                                                 991
         AG_EXNAME = 'SHAPE'                                              991
         AG_PARLIST = 'DX_DY_DZ'                                          991
         CALL AXSHAPE                                                     991
C    *                                                                    991
C    Create SFSD :                                                        992
C    Call To Local Procedure SFSD                                         992
         ASSIGN 6390 TO BLOCK_SFSD                                        992
         GO TO  6380                                                      992
6390     CONTINUE                                                         992
C    *                                                                    992
C    Geant POSITION : SFSD ===> inherited arguments used <===             993
         CALL AGSRESET                                                    993
         AG_TITLE = 'SFSD'                                                993
         AG_EXNAME = 'POSITION'                                           993
         AG_PARLIST = 'NONE'                                              993
         CALL AXPOSITION                                                  993
C    *                                                                    993
6361  CALL AGSPOPB                                                        994
      GO TO BLOCK_SFSW, (6360 ,6370)                                      994
C End of Local Procedure                                                  994
C ----------------------------------------------------------------------  995
C ------------ SFSD                                                       995
C Begin Local Procedure SFSD                                              996
C SFSD IS THE STRIP DETECTOR                                              996
6380  CONTINUE                                                            996
         AG_TITLE='SFSD'                                                  996
         AG_MODULE=MODULE                                                 996
         AG_MARK='V00'                                                    996
         CALL AGSPUSH                                                     996
         AG_MODULE=MODULE                                                 996
C    *                                                                    996
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          997
         CALL AGSRESET                                                    997
         AG_TITLE = 'SILICON'                                             997
         AG_EXNAME = 'MATERIAL'                                           997
         AG_PARLIST = 'NONE'                                              997
         CALL AXMATERIAL                                                  997
C    *                                                                    997
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         997
         CALL AGSRESET                                                    998
         AG_ISVOL = 1                                                     998
         AG_TITLE = 'SENSITIVE'                                           998
         AG_EXNAME = 'MATERIAL'                                           998
         AG_PARLIST = 'ISVOL'                                             998
         CALL AXMATERIAL                                                  998
C    *                                                                    998
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=4 <===                       998
         CALL AGSRESET                                                    999
         AG_SEEN = 2                                                      999
         AG_COLO = 4                                                      999
         AG_TITLE = 'SFSD'                                                999
         AG_EXNAME = 'ATTRIBUTE'                                          999
         AG_PARLIST = 'SEEN_COLO'                                         999
         CALL AXATTRIBUTE                                                 999
C    *                                                                    999
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_SDL  999
C    EN/2 <===                                                            999
         CALL AGSRESET                                                    1001
         AG_DX = SFPA_DMWID/2                                             1001
         AG_DY = SFPA_DMTHK/2                                             1001
         AG_DZ = SFPA_SDLEN/2                                             1001
         AG_TITLE = 'BOX'                                                 1001
         AG_EXNAME = 'SHAPE'                                              1001
         AG_PARLIST = 'DX_DY_DZ'                                          1001
         CALL AXSHAPE                                                     1001
C    *                                                                    1001
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 1001
C    Defining Hits: X:.001:S Y:.001: Z:.001: CX:10: CY:10: CZ:10: STEP:.  1004
C    01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100) ELOSS:16:(0  1004
C    ,0.001)                                                              1004
         AG_TITLE = 'SFSD'                                                1004
         AG_EXNAME = 'HITS'                                               1004
         AG_TYPE=AG_MAGIC                                                 1004
         CALL AGSDETV('SFSD')                                             1004
C    *                                                                    1004
         AG_PARLIST='X'                                                   1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = AG_MAGIC                                              1004
         AG_BIN=.001                                                      1004
         AG_OPTION='S'                                                    1004
         AG_FHMIN = AG_MAGIC                                              1004
         AG_FHMAX = AG_MAGIC                                              1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='Y'                                                   1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = AG_MAGIC                                              1004
         AG_BIN=.001                                                      1004
         AG_FHMIN=AG_MAGIC                                                1004
         AG_FHMAX=AG_MAGIC                                                1004
         AG_OPTION='*'                                                    1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='Z'                                                   1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = AG_MAGIC                                              1004
         AG_BIN=.001                                                      1004
         AG_FHMIN=AG_MAGIC                                                1004
         AG_FHMAX=AG_MAGIC                                                1004
         AG_OPTION='*'                                                    1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='CX'                                                  1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = 10                                                    1004
         AG_BIN=AG_MAGIC                                                  1004
         AG_FHMIN=AG_MAGIC                                                1004
         AG_FHMAX=AG_MAGIC                                                1004
         AG_OPTION='*'                                                    1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='CY'                                                  1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = 10                                                    1004
         AG_BIN=AG_MAGIC                                                  1004
         AG_FHMIN=AG_MAGIC                                                1004
         AG_FHMAX=AG_MAGIC                                                1004
         AG_OPTION='*'                                                    1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='CZ'                                                  1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = 10                                                    1004
         AG_BIN=AG_MAGIC                                                  1004
         AG_FHMIN=AG_MAGIC                                                1004
         AG_FHMAX=AG_MAGIC                                                1004
         AG_OPTION='*'                                                    1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='STEP'                                                1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = AG_MAGIC                                              1004
         AG_BIN=.01                                                       1004
         AG_FHMIN=AG_MAGIC                                                1004
         AG_FHMAX=AG_MAGIC                                                1004
         AG_OPTION='*'                                                    1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='SLENG'                                               1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = 16                                                    1004
         AG_BIN=AG_MAGIC                                                  1004
         AG_OPTION='*'                                                    1004
         AG_FHMIN = 0                                                     1004
         AG_FHMAX = 500                                                   1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='TOF'                                                 1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = 16                                                    1004
         AG_BIN=AG_MAGIC                                                  1004
         AG_OPTION='*'                                                    1004
         AG_FHMIN = 0                                                     1004
         AG_FHMAX = 1.0E-6                                                1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='PTOT'                                                1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = 16                                                    1004
         AG_BIN=AG_MAGIC                                                  1004
         AG_OPTION='*'                                                    1004
         AG_FHMIN = 0                                                     1004
         AG_FHMAX = 100                                                   1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         AG_PARLIST='ELOSS'                                               1004
         AG_TITLE = 'DETH'                                                1004
         AG_NBITS = 16                                                    1004
         AG_BIN=AG_MAGIC                                                  1004
         AG_OPTION='*'                                                    1004
         AG_FHMIN = 0                                                     1004
         AG_FHMAX = 0.001                                                 1004
         CALL AGSDETH (0)                                                 1004
C    *                                                                    1004
         CALL AGSDETU('SFSD')                                             1004
C    *                                                                    1004
6381  CALL AGSPOPB                                                        1005
      GO TO BLOCK_SFSD, (6380 ,6390)                                      1005
C End of Local Procedure                                                  1005
C ----------------------------------------------------------------------  1006
C ------------ SFSM                                                       1006
C Begin Local Procedure SFSM                                              1007
C SFSM IS THE MOTHER OF THE LADDER STRUCT.                                1007
6330  CONTINUE                                                            1007
         AG_TITLE='SFSM'                                                  1007
         AG_MODULE=MODULE                                                 1007
         AG_MARK='V00'                                                    1007
         CALL AGSPUSH                                                     1007
         AG_MODULE=MODULE                                                 1007
C    *                                                                    1007
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1008
         CALL AGSRESET                                                    1008
         AG_TITLE = 'AIR'                                                 1008
         AG_EXNAME = 'MATERIAL'                                           1008
         AG_PARLIST = 'NONE'                                              1008
         CALL AXMATERIAL                                                  1008
C    *                                                                    1008
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=1 <===                       1008
         CALL AGSRESET                                                    1009
         AG_SEEN = 0                                                      1009
         AG_COLO = 1                                                      1009
         AG_TITLE = 'SFSM'                                                1009
         AG_EXNAME = 'ATTRIBUTE'                                          1009
         AG_PARLIST = 'SEEN_COLO'                                         1009
         CALL AXATTRIBUTE                                                 1009
C    *                                                                    1009
C    Geant SHAPE : BOX ===> DX=SFPA_SMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  1009
C    EN/2 <===                                                            1009
         CALL AGSRESET                                                    1011
         AG_DX = SFPA_SMWID/2                                             1011
         AG_DY = SFPA_SMTHK/2                                             1011
         AG_DZ = SFPA_SMLEN/2                                             1011
         AG_TITLE = 'BOX'                                                 1011
         AG_EXNAME = 'SHAPE'                                              1011
         AG_PARLIST = 'DX_DY_DZ'                                          1011
         CALL AXSHAPE                                                     1011
C    *                                                                    1011
C    Create SFSS :                                                        1012
C    Call To Local Procedure SFSS                                         1012
         ASSIGN 6410 TO BLOCK_SFSS                                        1012
         GO TO  6400                                                      1012
6410     CONTINUE                                                         1012
C    *                                                                    1012
6331  CALL AGSPOPB                                                        1013
      GO TO BLOCK_SFSM, (6330 ,6340)                                      1013
C End of Local Procedure                                                  1013
C ----------------------------------------------------------------------  1014
C ------------ SFSS                                                       1014
C Begin Local Procedure SFSS                                              1015
C SFSS IS THE SUBVOLUME OF THE MOTHER STRUCT.                             1015
6400  CONTINUE                                                            1015
         AG_TITLE='SFSS'                                                  1015
         AG_MODULE=MODULE                                                 1015
         AG_MARK='V00'                                                    1015
         CALL AGSPUSH                                                     1015
         AG_MODULE=MODULE                                                 1015
C    *                                                                    1015
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1016
         CALL AGSRESET                                                    1016
         AG_TITLE = 'AIR'                                                 1016
         AG_EXNAME = 'MATERIAL'                                           1016
         AG_PARLIST = 'NONE'                                              1016
         CALL AXMATERIAL                                                  1016
C    *                                                                    1016
C    Geant ATTRIBUTE : SFSS ===> SEEN=0 COLO=1 <===                       1016
         CALL AGSRESET                                                    1017
         AG_SEEN = 0                                                      1017
         AG_COLO = 1                                                      1017
         AG_TITLE = 'SFSS'                                                1017
         AG_EXNAME = 'ATTRIBUTE'                                          1017
         AG_PARLIST = 'SEEN_COLO'                                         1017
         CALL AXATTRIBUTE                                                 1017
C    *                                                                    1017
C    Geant SHAPE : DIVISION ===> NDIV=20 IAXIS=3 <===                     1017
         CALL AGSRESET                                                    1018
         AG_NDIV = 20                                                     1018
         AG_IAXIS = 3                                                     1018
         AG_TITLE = 'DIVISION'                                            1018
         AG_EXNAME = 'SHAPE'                                              1018
         AG_PARLIST = 'NDIV_IAXIS'                                        1018
         CALL AXSHAPE                                                     1018
C    *                                                                    1018
C    Create SFCP :                                                        1019
C    Call To Local Procedure SFCP                                         1019
         ASSIGN 6430 TO BLOCK_SFCP                                        1019
         GO TO  6420                                                      1019
6430     CONTINUE                                                         1019
C    *                                                                    1019
C    Geant POSITION : SFCP ===> X=SFPA_SMWID/2-5.*SFPA_CPRAD Y=-SFPA_SMT  1020
C    HK/2+SFPA_CPRAD <===                                                 1020
         CALL AGSRESET                                                    1020
         AG_X = SFPA_SMWID/2-5.*SFPA_CPRAD                                1020
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  1020
         AG_TITLE = 'SFCP'                                                1020
         AG_EXNAME = 'POSITION'                                           1020
         AG_PARLIST = 'X_Y'                                               1020
         CALL AXPOSITION                                                  1020
C    *                                                                    1020
C    Geant POSITION : SFCP ===> X=-SFPA_SMWID/2+5.*SFPA_CPRAD Y=-SFPA_SM  1020
C    THK/2+SFPA_CPRAD <===                                                1020
         CALL AGSRESET                                                    1021
         AG_X = -SFPA_SMWID/2+5.*SFPA_CPRAD                               1021
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  1021
         AG_TITLE = 'SFCP'                                                1021
         AG_EXNAME = 'POSITION'                                           1021
         AG_PARLIST = 'X_Y'                                               1021
         CALL AXPOSITION                                                  1021
C    *                                                                    1021
C    Create SFCF :                                                        1022
C    Call To Local Procedure SFCF                                         1022
         ASSIGN 6450 TO BLOCK_SFCF                                        1022
         GO TO  6440                                                      1022
6450     CONTINUE                                                         1022
C    *                                                                    1022
C    Geant POSITION : SFCF ===> inherited arguments used <===             1023
         CALL AGSRESET                                                    1023
         AG_TITLE = 'SFCF'                                                1023
         AG_EXNAME = 'POSITION'                                           1023
         AG_PARLIST = 'NONE'                                              1023
         CALL AXPOSITION                                                  1023
C    *                                                                    1023
6401  CALL AGSPOPB                                                        1024
      GO TO BLOCK_SFSS, (6400 ,6410)                                      1024
C End of Local Procedure                                                  1024
C ----------------------------------------------------------------------  1025
C ------------ SFCP                                                       1025
C Begin Local Procedure SFCP                                              1026
C SFCP IS THE COOLING PIPE                                                1026
6420  CONTINUE                                                            1026
         AG_TITLE='SFCP'                                                  1026
         AG_MODULE=MODULE                                                 1026
         AG_MARK='V00'                                                    1026
         CALL AGSPUSH                                                     1026
         AG_MODULE=MODULE                                                 1026
C    *                                                                    1026
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1027
         CALL AGSRESET                                                    1027
         AG_TITLE = 'CARBON'                                              1027
         AG_EXNAME = 'MATERIAL'                                           1027
         AG_PARLIST = 'NONE'                                              1027
         CALL AXMATERIAL                                                  1027
C    *                                                                    1027
C    Geant ATTRIBUTE : SFCP ===> SEEN=1 COLO=6 <===                       1027
         CALL AGSRESET                                                    1028
         AG_SEEN = 1                                                      1028
         AG_COLO = 6                                                      1028
         AG_TITLE = 'SFCP'                                                1028
         AG_EXNAME = 'ATTRIBUTE'                                          1028
         AG_PARLIST = 'SEEN_COLO'                                         1028
         CALL AXATTRIBUTE                                                 1028
C    *                                                                    1028
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CPRAD DZ=SFPA_SSLEN/2 <===  1028
         CALL AGSRESET                                                    1029
         AG_RMIN = 0                                                      1029
         AG_RMAX = SFPA_CPRAD                                             1029
         AG_DZ = SFPA_SSLEN/2                                             1029
         AG_TITLE = 'TUBE'                                                1029
         AG_EXNAME = 'SHAPE'                                              1029
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1029
         CALL AXSHAPE                                                     1029
C    *                                                                    1029
C    Create SFCW :                                                        1030
C    Call To Local Procedure SFCW                                         1030
         ASSIGN 6470 TO BLOCK_SFCW                                        1030
         GO TO  6460                                                      1030
6470     CONTINUE                                                         1030
C    *                                                                    1030
C    Geant POSITION : SFCW ===> inherited arguments used <===             1031
         CALL AGSRESET                                                    1031
         AG_TITLE = 'SFCW'                                                1031
         AG_EXNAME = 'POSITION'                                           1031
         AG_PARLIST = 'NONE'                                              1031
         CALL AXPOSITION                                                  1031
C    *                                                                    1031
6421  CALL AGSPOPB                                                        1032
      GO TO BLOCK_SFCP, (6420 ,6430)                                      1032
C End of Local Procedure                                                  1032
C ----------------------------------------------------------------------  1033
C ------------ SFCW                                                       1033
C Begin Local Procedure SFCW                                              1034
C SFCW IS THE WATER CYLINDER IN THE COOLING PIPE                          1034
6460  CONTINUE                                                            1034
         AG_TITLE='SFCW'                                                  1034
         AG_MODULE=MODULE                                                 1034
         AG_MARK='V00'                                                    1034
         CALL AGSPUSH                                                     1034
         AG_MODULE=MODULE                                                 1034
C    *                                                                    1034
C    Geant ATTRIBUTE : SFCW ===> SEEN=1 COLO=6 <===                       1035
         CALL AGSRESET                                                    1035
         AG_SEEN = 1                                                      1035
         AG_COLO = 6                                                      1035
         AG_TITLE = 'SFCW'                                                1035
         AG_EXNAME = 'ATTRIBUTE'                                          1035
         AG_PARLIST = 'SEEN_COLO'                                         1035
         CALL AXATTRIBUTE                                                 1035
C    *                                                                    1035
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1035
         CALL AGSRESET                                                    1036
         AG_TITLE = 'WATER'                                               1036
         AG_EXNAME = 'MATERIAL'                                           1036
         AG_PARLIST = 'NONE'                                              1036
         CALL AXMATERIAL                                                  1036
C    *                                                                    1036
C    Geant SHAPE : TUBE ===> RMAX=SFPA_CPRAL <===                         1036
         CALL AGSRESET                                                    1037
         AG_RMAX = SFPA_CPRAL                                             1037
         AG_TITLE = 'TUBE'                                                1037
         AG_EXNAME = 'SHAPE'                                              1037
         AG_PARLIST = 'RMAX'                                              1037
         CALL AXSHAPE                                                     1037
C    *                                                                    1037
6461  CALL AGSPOPB                                                        1038
      GO TO BLOCK_SFCW, (6460 ,6470)                                      1038
C End of Local Procedure                                                  1038
C ----------------------------------------------------------------------  1039
C ------------ SFCF                                                       1039
C Begin Local Procedure SFCF                                              1040
C SFCF IS THE CARBON FIBER STRUCTURE CONTAINER                            1040
6440  CONTINUE                                                            1040
         AG_TITLE='SFCF'                                                  1040
         AG_MODULE=MODULE                                                 1040
         AG_MARK='V00'                                                    1040
         CALL AGSPUSH                                                     1040
         AG_MODULE=MODULE                                                 1040
C    *                                                                    1040
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1041
         CALL AGSRESET                                                    1041
         AG_TITLE = 'AIR'                                                 1041
         AG_EXNAME = 'MATERIAL'                                           1041
         AG_PARLIST = 'NONE'                                              1041
         CALL AXMATERIAL                                                  1041
C    *                                                                    1041
C    Geant ATTRIBUTE : SFCF ===> SEEN=0 COLO=3 <===                       1041
         CALL AGSRESET                                                    1042
         AG_SEEN = 0                                                      1042
         AG_COLO = 3                                                      1042
         AG_TITLE = 'SFCF'                                                1042
         AG_EXNAME = 'ATTRIBUTE'                                          1042
         AG_PARLIST = 'SEEN_COLO'                                         1042
         CALL AXATTRIBUTE                                                 1042
C    *                                                                    1042
C    Geant SHAPE : BOX ===> DX=SFPA_SMTHK*TAN(PI/6.) DY=SFPA_SMTHK/2., D  1042
C    Z=SFPA_SSLEN/2 <===                                                  1042
         CALL AGSRESET                                                    1044
         AG_DX = SFPA_SMTHK*TAN(PI/6.)                                    1044
         AG_DY = SFPA_SMTHK/2.                                            1044
         AG_DZ = SFPA_SSLEN/2                                             1044
         AG_TITLE = 'BOX'                                                 1044
         AG_EXNAME = 'SHAPE'                                              1044
         AG_PARLIST = 'DX_DY_DZ'                                          1044
         CALL AXSHAPE                                                     1044
C    *                                                                    1044
C    Create SFCT :                                                        1045
C    Call To Local Procedure SFCT                                         1045
         ASSIGN 6490 TO BLOCK_SFCT                                        1045
         GO TO  6480                                                      1045
6490     CONTINUE                                                         1045
C    *                                                                    1045
C    Geant POSITION : SFCT ===> Y=SFPA_SMTHK/2.-SFPA_CFRAD <===           1046
         CALL AGSRESET                                                    1046
         AG_Y = SFPA_SMTHK/2.-SFPA_CFRAD                                  1046
         AG_TITLE = 'SFCT'                                                1046
         AG_EXNAME = 'POSITION'                                           1046
         AG_PARLIST = 'Y'                                                 1046
         CALL AXPOSITION                                                  1046
C    *                                                                    1046
C    Geant POSITION : SFCT ===> X=SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD Y=-SF  1046
C    PA_SMTHK/2.+SFPA_CFRAD <===                                          1046
         CALL AGSRESET                                                    1047
         AG_X = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                          1047
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1047
         AG_TITLE = 'SFCT'                                                1047
         AG_EXNAME = 'POSITION'                                           1047
         AG_PARLIST = 'X_Y'                                               1047
         CALL AXPOSITION                                                  1047
C    *                                                                    1047
C    Geant POSITION : SFCT ===> X=-SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD Y=-S  1047
C    FPA_SMTHK/2.+SFPA_CFRAD <===                                         1047
         CALL AGSRESET                                                    1048
         AG_X = -SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD                         1048
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1048
         AG_TITLE = 'SFCT'                                                1048
         AG_EXNAME = 'POSITION'                                           1048
         AG_PARLIST = 'X_Y'                                               1048
         CALL AXPOSITION                                                  1048
C    *                                                                    1048
C    Create SFCX :                                                        1049
C    Call To Local Procedure SFCX                                         1049
         ASSIGN 6510 TO BLOCK_SFCX                                        1049
         GO TO  6500                                                      1049
6510     CONTINUE                                                         1049
C    *                                                                    1049
C    Geant POSITION : SFCX ===> Y=-SFPA_SMTHK/2.+SFPA_CFRAD ORT=YZX <===  1050
         CALL AGSRESET                                                    1050
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1050
         AG_ALPHAN=AG_ALPHAN+1                                            1050
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1050
         AG_TITLE = 'SFCX'                                                1050
         AG_EXNAME = 'POSITION'                                           1050
         AG_PARLIST = 'Y_ORT'                                             1050
         CALL AXPOSITION                                                  1050
C    *                                                                    1050
C    Geant POSITION : SFCX ===> X=SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.  1050
C     ORT=YZX ALPHAZ=-60 <===                                             1050
         CALL AGSRESET                                                    1051
         AG_X = SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.                    1051
         AG_ALPHAN=AG_ALPHAN+1                                            1051
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1051
         AG_ALPHAN=AG_ALPHAN+1                                            1051
         AG_ALPHAK(AG_ALPHAN)=3                                           1051
         AG_ALPHAL(AG_ALPHAN)=-60                                         1051
         AG_TITLE = 'SFCX'                                                1051
         AG_EXNAME = 'POSITION'                                           1051
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      1051
         CALL AXPOSITION                                                  1051
C    *                                                                    1051
C    Geant POSITION : SFCX ===> X=-SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3  1051
C    . ORT=YZX ALPHAZ=+60 <===                                            1051
         CALL AGSRESET                                                    1052
         AG_X = -SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3.                   1052
         AG_ALPHAN=AG_ALPHAN+1                                            1052
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1052
         AG_ALPHAN=AG_ALPHAN+1                                            1052
         AG_ALPHAK(AG_ALPHAN)=3                                           1052
         AG_ALPHAL(AG_ALPHAN)=+60                                         1052
         AG_TITLE = 'SFCX'                                                1052
         AG_EXNAME = 'POSITION'                                           1052
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      1052
         CALL AXPOSITION                                                  1052
C    *                                                                    1052
6441  CALL AGSPOPB                                                        1053
      GO TO BLOCK_SFCF, (6440 ,6450)                                      1053
C End of Local Procedure                                                  1053
C ----------------------------------------------------------------------  1054
C ------------ SFCT                                                       1054
C Begin Local Procedure SFCT                                              1055
C SFCT IS THE CARBON FIBER TUBE                                           1055
6480  CONTINUE                                                            1055
         AG_TITLE='SFCT'                                                  1055
         AG_MODULE=MODULE                                                 1055
         AG_MARK='V00'                                                    1055
         CALL AGSPUSH                                                     1055
         AG_MODULE=MODULE                                                 1055
C    *                                                                    1055
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1056
         CALL AGSRESET                                                    1056
         AG_TITLE = 'CARBON'                                              1056
         AG_EXNAME = 'MATERIAL'                                           1056
         AG_PARLIST = 'NONE'                                              1056
         CALL AXMATERIAL                                                  1056
C    *                                                                    1056
C    Geant ATTRIBUTE : SFCT ===> SEEN=1 COLO=6 <===                       1056
         CALL AGSRESET                                                    1057
         AG_SEEN = 1                                                      1057
         AG_COLO = 6                                                      1057
         AG_TITLE = 'SFCT'                                                1057
         AG_EXNAME = 'ATTRIBUTE'                                          1057
         AG_PARLIST = 'SEEN_COLO'                                         1057
         CALL AXATTRIBUTE                                                 1057
C    *                                                                    1057
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SSLEN/2 <===  1057
         CALL AGSRESET                                                    1058
         AG_RMIN = 0                                                      1058
         AG_RMAX = SFPA_CFRAD                                             1058
         AG_DZ = SFPA_SSLEN/2                                             1058
         AG_TITLE = 'TUBE'                                                1058
         AG_EXNAME = 'SHAPE'                                              1058
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1058
         CALL AXSHAPE                                                     1058
C    *                                                                    1058
6481  CALL AGSPOPB                                                        1059
      GO TO BLOCK_SFCT, (6480 ,6490)                                      1059
C End of Local Procedure                                                  1059
C ----------------------------------------------------------------------  1060
C ------------ SFCX                                                       1060
C Begin Local Procedure SFCX                                              1061
C SFCX IS THE CARBON FIBER TUBE                                           1061
6500  CONTINUE                                                            1061
         AG_TITLE='SFCX'                                                  1061
         AG_MODULE=MODULE                                                 1061
         AG_MARK='V00'                                                    1061
         CALL AGSPUSH                                                     1061
         AG_MODULE=MODULE                                                 1061
C    *                                                                    1061
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1062
         CALL AGSRESET                                                    1062
         AG_TITLE = 'CARBON'                                              1062
         AG_EXNAME = 'MATERIAL'                                           1062
         AG_PARLIST = 'NONE'                                              1062
         CALL AXMATERIAL                                                  1062
C    *                                                                    1062
C    Geant ATTRIBUTE : SFCX ===> SEEN=1 COLO=7 <===                       1062
         CALL AGSRESET                                                    1063
         AG_SEEN = 1                                                      1063
         AG_COLO = 7                                                      1063
         AG_TITLE = 'SFCX'                                                1063
         AG_EXNAME = 'ATTRIBUTE'                                          1063
         AG_PARLIST = 'SEEN_COLO'                                         1063
         CALL AXATTRIBUTE                                                 1063
C    *                                                                    1063
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SMTHK*TAN(PI  1063
C    /6.)-SFPA_CFRAD <===                                                 1063
         CALL AGSRESET                                                    1064
         AG_RMIN = 0                                                      1064
         AG_RMAX = SFPA_CFRAD                                             1064
         AG_DZ = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                         1064
         AG_TITLE = 'TUBE'                                                1064
         AG_EXNAME = 'SHAPE'                                              1064
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1064
         CALL AXSHAPE                                                     1064
C    *                                                                    1064
6501  CALL AGSPOPB                                                        1065
      GO TO BLOCK_SFCX, (6500 ,6510)                                      1065
C End of Local Procedure                                                  1065
C ----------------------------------------------------------------------  1066
C ------------ SBSP                                                       1066
C Begin Local Procedure SBSP                                              1067
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1067
5210  CONTINUE                                                            1067
         AG_TITLE='SBSP'                                                  1067
         AG_MODULE=MODULE                                                 1067
         AG_MARK='V00'                                                    1067
         CALL AGSPUSH                                                     1067
         AG_MODULE=MODULE                                                 1067
C    *                                                                    1067
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1068
         CALL AGSRESET                                                    1068
         AG_TITLE = 'AIR'                                                 1068
         AG_EXNAME = 'MATERIAL'                                           1068
         AG_PARLIST = 'NONE'                                              1068
         CALL AXMATERIAL                                                  1068
C    *                                                                    1068
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1068
         CALL AGSRESET                                                    1069
         AG_SEEN = 0                                                      1069
         AG_COLO = 1                                                      1069
         AG_TITLE = 'SBSP'                                                1069
         AG_EXNAME = 'ATTRIBUTE'                                          1069
         AG_PARLIST = 'SEEN_COLO'                                         1069
         CALL AXATTRIBUTE                                                 1069
C    *                                                                    1069
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1069
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1069
         CALL AGSRESET                                                    1071
         AG_RMIN = SVTG_RSIZEMIN                                          1071
         AG_RMAX = SSUB_KMOUNTOD/2                                        1071
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1071
         AG_TITLE = 'TUBE'                                                1071
         AG_EXNAME = 'SHAPE'                                              1071
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1071
         CALL AXSHAPE                                                     1071
C    *                                                                    1071
C    Create SAKM :                                                        1072
C    Call To Local Procedure SAKM                                         1072
         ASSIGN 6530 TO BLOCK_SAKM                                        1072
         GO TO  6520                                                      1072
6530     CONTINUE                                                         1072
C    *                                                                    1072
C    Geant POSITION : SAKM ===> inherited arguments used <===             1073
         CALL AGSRESET                                                    1073
         AG_TITLE = 'SAKM'                                                1073
         AG_EXNAME = 'POSITION'                                           1073
         AG_PARLIST = 'NONE'                                              1073
         CALL AXPOSITION                                                  1073
C    *                                                                    1073
C    Create SBMM :                                                        1074
C    Call To Local Procedure SBMM                                         1074
         ASSIGN 6550 TO BLOCK_SBMM                                        1074
         GO TO  6540                                                      1074
6550     CONTINUE                                                         1074
C    *                                                                    1074
C    Create SBMM :                                                        1075
C    Call To Local Procedure SBMM                                         1075
         ASSIGN 6560 TO BLOCK_SBMM                                        1075
         GO TO  6540                                                      1075
6560     CONTINUE                                                         1075
C    *                                                                    1075
C    Create SBRL :                                                        1076
C    Call To Local Procedure SBRL                                         1076
         ASSIGN 6580 TO BLOCK_SBRL                                        1076
         GO TO  6570                                                      1076
6580     CONTINUE                                                         1076
C    *                                                                    1076
C    Create SBRX :                                                        1077
C    Call To Local Procedure SBRX                                         1077
         ASSIGN 6600 TO BLOCK_SBRX                                        1077
         GO TO  6590                                                      1077
6600     CONTINUE                                                         1077
C    *                                                                    1077
C    *                                                                    1078
         I=-1                                                             1078
            GOTO 6613                                                     1078
6611        I=I+(2)                                                       1078
6613        IF((2)*(I-(1)).GT.0) GO TO 6612                               1078
C       *                                                                 1079
            J=0                                                           1079
               GO TO 6623                                                 1079
6621           J=J+1                                                      1079
6623           IF(J-(1).GT.0) GO TO 6622                                  1079
               PHI=I*SSUB_SRCUTPHI+180*J                                  1080
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1081
     *         2.0)                                                       1081
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1083
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1083
C          ==                                                             1083
               CALL AGSRESET                                              1083
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1083
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1083
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1083
               AG_ALPHAN=AG_ALPHAN+1                                      1083
               AG_ALPHAK(AG_ALPHAN)=3                                     1083
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1083
               AG_TITLE = 'SBMM'                                          1083
               AG_EXNAME = 'POSITION'                                     1083
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1083
               CALL AXPOSITION                                            1083
C          *                                                              1083
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1083
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1086
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1086
C           ORT=ZXY <===                                                  1086
               CALL AGSRESET                                              1086
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1086
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1086
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1086
               AG_ALPHAN=AG_ALPHAN+1                                      1086
               AG_ALPHAK(AG_ALPHAN)=3                                     1086
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1086
               AG_ALPHAN=AG_ALPHAN+1                                      1086
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1086
               AG_TITLE = 'SBRL'                                          1086
               AG_EXNAME = 'POSITION'                                     1086
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1086
               CALL AXPOSITION                                            1086
C          *                                                              1086
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1086
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1086
C           ORT=ZXY <===                                                  1086
               CALL AGSRESET                                              1088
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1088
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1088
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1088
               AG_ALPHAN=AG_ALPHAN+1                                      1088
               AG_ALPHAK(AG_ALPHAN)=3                                     1088
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1088
               AG_ALPHAN=AG_ALPHAN+1                                      1088
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1088
               AG_TITLE = 'SBRX'                                          1088
               AG_EXNAME = 'POSITION'                                     1088
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1088
               CALL AXPOSITION                                            1088
C          *                                                              1088
            GO TO 6621                                                    1089
6622        CONTINUE                                                      1089
         GO TO 6611                                                       1090
6612     CONTINUE                                                         1090
C    Create SBSR :                                                        1091
C    Call To Local Procedure SBSR                                         1091
         ASSIGN 6640 TO BLOCK_SBSR                                        1091
         GO TO  6630                                                      1091
6640     CONTINUE                                                         1091
C    *                                                                    1091
C    Geant POSITION : SBSR ===> inherited arguments used <===             1092
         CALL AGSRESET                                                    1092
         AG_TITLE = 'SBSR'                                                1092
         AG_EXNAME = 'POSITION'                                           1092
         AG_PARLIST = 'NONE'                                              1092
         CALL AXPOSITION                                                  1092
C    *                                                                    1092
5211  CALL AGSPOPB                                                        1093
      GO TO BLOCK_SBSP, (5210 ,5220)                                      1093
C End of Local Procedure                                                  1093
C ----------------------------------------------------------------------  1094
C ------------ SAKM                                                       1094
C Begin Local Procedure SAKM                                              1095
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1095
6520  CONTINUE                                                            1095
         AG_TITLE='SAKM'                                                  1095
         AG_MODULE=MODULE                                                 1095
         AG_MARK='V00'                                                    1095
         CALL AGSPUSH                                                     1095
         AG_MODULE=MODULE                                                 1095
C    *                                                                    1095
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1096
         CALL AGSRESET                                                    1096
         AG_TITLE = 'ALUMINIUM'                                           1096
         AG_EXNAME = 'MATERIAL'                                           1096
         AG_PARLIST = 'NONE'                                              1096
         CALL AXMATERIAL                                                  1096
C    *                                                                    1096
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1096
         CALL AGSRESET                                                    1097
         AG_SEEN = 1                                                      1097
         AG_COLO = 2                                                      1097
         AG_TITLE = 'SAKM'                                                1097
         AG_EXNAME = 'ATTRIBUTE'                                          1097
         AG_PARLIST = 'SEEN_COLO'                                         1097
         CALL AXATTRIBUTE                                                 1097
C    *                                                                    1097
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1097
C    DZ=SSUB_KMNTTHK/2 <===                                               1097
         CALL AGSRESET                                                    1099
         AG_RMIN = SSUB_KMOUNTID/2                                        1099
         AG_RMAX = SSUB_KMOUNTOD/2                                        1099
         AG_DZ = SSUB_KMNTTHK/2                                           1099
         AG_TITLE = 'TUBE'                                                1099
         AG_EXNAME = 'SHAPE'                                              1099
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1099
         CALL AXSHAPE                                                     1099
C    *                                                                    1099
C    Create SCKM :                                                        1100
C    Call To Local Procedure SCKM                                         1100
         ASSIGN 6660 TO BLOCK_SCKM                                        1100
         GO TO  6650                                                      1100
6660     CONTINUE                                                         1100
C    *                                                                    1100
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1101
         CALL AGSRESET                                                    1101
         AG_Y = SSUB_KMCUTOFF                                             1101
         AG_KONLY = 'MANY'                                                1101
         AG_TITLE = 'SCKM'                                                1101
         AG_EXNAME = 'POSITION'                                           1101
         AG_PARLIST = 'Y_KONLY'                                           1101
         CALL AXPOSITION                                                  1101
C    *                                                                    1101
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1101
C     <===                                                                1101
         CALL AGSRESET                                                    1102
         AG_Y = -SSUB_KMCUTOFF                                            1102
         AG_ALPHAN=AG_ALPHAN+1                                            1102
         AG_ALPHAK(AG_ALPHAN)=3                                           1102
         AG_ALPHAL(AG_ALPHAN)=180                                         1102
         AG_KONLY = 'MANY'                                                1102
         AG_TITLE = 'SCKM'                                                1102
         AG_EXNAME = 'POSITION'                                           1102
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1102
         CALL AXPOSITION                                                  1102
C    *                                                                    1102
6521  CALL AGSPOPB                                                        1103
      GO TO BLOCK_SAKM, (6520 ,6530)                                      1103
C End of Local Procedure                                                  1103
C ----------------------------------------------------------------------  1104
C ------------ SCKM                                                       1104
C Begin Local Procedure SCKM                                              1105
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1105
6650  CONTINUE                                                            1105
         AG_TITLE='SCKM'                                                  1105
         AG_MODULE=MODULE                                                 1105
         AG_MARK='V00'                                                    1105
         CALL AGSPUSH                                                     1105
         AG_MODULE=MODULE                                                 1105
C    *                                                                    1105
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1106
         CALL AGSRESET                                                    1106
         AG_TITLE = 'AIR'                                                 1106
         AG_EXNAME = 'MATERIAL'                                           1106
         AG_PARLIST = 'NONE'                                              1106
         CALL AXMATERIAL                                                  1106
C    *                                                                    1106
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1106
         CALL AGSRESET                                                    1107
         AG_SEEN = 1                                                      1107
         AG_COLO = 1                                                      1107
         AG_TITLE = 'SCKM'                                                1107
         AG_EXNAME = 'ATTRIBUTE'                                          1107
         AG_PARLIST = 'SEEN_COLO'                                         1107
         CALL AXATTRIBUTE                                                 1107
C    *                                                                    1107
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1107
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1107
         CALL AGSRESET                                                    1110
         AG_RMIN = SSUB_KMCUTID/2                                         1110
         AG_RMAX = SSUB_KMCUTOD/2                                         1110
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1110
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1110
         AG_DZ = SSUB_KMNTTHK/2                                           1110
         AG_TITLE = 'TUBS'                                                1110
         AG_EXNAME = 'SHAPE'                                              1110
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1110
         CALL AXSHAPE                                                     1110
C    *                                                                    1110
6651  CALL AGSPOPB                                                        1111
      GO TO BLOCK_SCKM, (6650 ,6660)                                      1111
C End of Local Procedure                                                  1111
C ----------------------------------------------------------------------  1112
C ------------ SBSR                                                       1112
C Begin Local Procedure SBSR                                              1113
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1113
6630  CONTINUE                                                            1113
         AG_TITLE='SBSR'                                                  1113
         AG_MODULE=MODULE                                                 1113
         AG_MARK='V00'                                                    1113
         CALL AGSPUSH                                                     1113
         AG_MODULE=MODULE                                                 1113
C    *                                                                    1113
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1114
         CALL AGSRESET                                                    1114
         AG_TITLE = 'G10'                                                 1114
         AG_EXNAME = 'MATERIAL'                                           1114
         AG_PARLIST = 'NONE'                                              1114
         CALL AXMATERIAL                                                  1114
C    *                                                                    1114
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1114
         CALL AGSRESET                                                    1115
         AG_SEEN = 1                                                      1115
         AG_COLO = 3                                                      1115
         AG_TITLE = 'SBSR'                                                1115
         AG_EXNAME = 'ATTRIBUTE'                                          1115
         AG_PARLIST = 'SEEN_COLO'                                         1115
         CALL AXATTRIBUTE                                                 1115
C    *                                                                    1115
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1115
C    =SSUB_SRINGTHK/2 <===                                                1115
         CALL AGSRESET                                                    1117
         AG_RMIN = SSUB_SRINGID/2                                         1117
         AG_RMAX = SSUB_SRINGOD/2                                         1117
         AG_DZ = SSUB_SRINGTHK/2                                          1117
         AG_TITLE = 'TUBE'                                                1117
         AG_EXNAME = 'SHAPE'                                              1117
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1117
         CALL AXSHAPE                                                     1117
C    *                                                                    1117
C    Create SBCR :                                                        1118
C    Call To Local Procedure SBCR                                         1118
         ASSIGN 6680 TO BLOCK_SBCR                                        1118
         GO TO  6670                                                      1118
6680     CONTINUE                                                         1118
C    *                                                                    1118
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1119
C    *                                                                    1120
         I=-1                                                             1120
            GOTO 6693                                                     1120
6691        I=I+(2)                                                       1120
6693        IF((2)*(I-(1)).GT.0) GO TO 6692                               1120
C       *                                                                 1121
            J=0                                                           1121
               GO TO 6703                                                 1121
6701           J=J+1                                                      1121
6703           IF(J-(1).GT.0) GO TO 6702                                  1121
               PHI=I*SSUB_SRCUTPHI+180*J                                  1122
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1124
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1124
               CALL AGSRESET                                              1124
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1124
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1124
               AG_ALPHAN=AG_ALPHAN+1                                      1124
               AG_ALPHAK(AG_ALPHAN)=3                                     1124
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1124
               AG_KONLY = 'MANY'                                          1124
               AG_TITLE = 'SBCR'                                          1124
               AG_EXNAME = 'POSITION'                                     1124
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1124
               CALL AXPOSITION                                            1124
C          *                                                              1124
            GO TO 6701                                                    1125
6702        CONTINUE                                                      1125
         GO TO 6691                                                       1126
6692     CONTINUE                                                         1126
6631  CALL AGSPOPB                                                        1127
      GO TO BLOCK_SBSR, (6630 ,6640)                                      1127
C End of Local Procedure                                                  1127
C ----------------------------------------------------------------------  1128
C ------------ SBCR                                                       1128
C Begin Local Procedure SBCR                                              1129
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1129
6670  CONTINUE                                                            1129
         AG_TITLE='SBCR'                                                  1129
         AG_MODULE=MODULE                                                 1129
         AG_MARK='V00'                                                    1129
         CALL AGSPUSH                                                     1129
         AG_MODULE=MODULE                                                 1129
C    *                                                                    1129
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1130
         CALL AGSRESET                                                    1130
         AG_TITLE = 'AIR'                                                 1130
         AG_EXNAME = 'MATERIAL'                                           1130
         AG_PARLIST = 'NONE'                                              1130
         CALL AXMATERIAL                                                  1130
C    *                                                                    1130
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1130
         CALL AGSRESET                                                    1131
         AG_SEEN = 1                                                      1131
         AG_COLO = 1                                                      1131
         AG_TITLE = 'SBCR'                                                1131
         AG_EXNAME = 'ATTRIBUTE'                                          1131
         AG_PARLIST = 'SEEN_COLO'                                         1131
         CALL AXATTRIBUTE                                                 1131
C    *                                                                    1131
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1131
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1131
         CALL AGSRESET                                                    1133
         AG_DX = SSUB_SRCUTWID/2                                          1133
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1133
         AG_DZ = SSUB_SRINGTHK/2                                          1133
         AG_TITLE = 'BOX'                                                 1133
         AG_EXNAME = 'SHAPE'                                              1133
         AG_PARLIST = 'DX_DY_DZ'                                          1133
         CALL AXSHAPE                                                     1133
C    *                                                                    1133
6671  CALL AGSPOPB                                                        1134
      GO TO BLOCK_SBCR, (6670 ,6680)                                      1134
C End of Local Procedure                                                  1134
C ----------------------------------------------------------------------  1135
C ------------ SBRL                                                       1135
C Begin Local Procedure SBRL                                              1136
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1136
6570  CONTINUE                                                            1136
         AG_TITLE='SBRL'                                                  1136
         AG_MODULE=MODULE                                                 1136
         AG_MARK='V00'                                                    1136
         CALL AGSPUSH                                                     1136
         AG_MODULE=MODULE                                                 1136
C    *                                                                    1136
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1137
C    ADL=12.6 <===                                                        1137
         CALL AGSRESET                                                    1137
         AG_A = 20.719                                                    1137
         AG_Z = 10.307                                                    1137
         AG_DENS = 2.23                                                   1137
         AG_ABSL = 50.7                                                   1137
         AG_RADL = 12.6                                                   1137
         AG_TITLE = 'PYREX'                                               1137
         AG_EXNAME = 'MATERIAL'                                           1137
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1137
         CALL AXMATERIAL                                                  1137
C    *                                                                    1137
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1137
         CALL AGSRESET                                                    1138
         AG_SEEN = 1                                                      1138
         AG_COLO = 6                                                      1138
         AG_TITLE = 'SBRL'                                                1138
         AG_EXNAME = 'ATTRIBUTE'                                          1138
         AG_PARLIST = 'SEEN_COLO'                                         1138
         CALL AXATTRIBUTE                                                 1138
C    *                                                                    1138
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1138
C    =                                                                    1138
         CALL AGSRESET                                                    1139
         AG_RMIN = SSUB_SROLLID/2                                         1139
         AG_RMAX = SSUB_SROLLOD/2                                         1139
         AG_TITLE = 'TUBE'                                                1139
         AG_EXNAME = 'SHAPE'                                              1139
         AG_PARLIST = 'RMIN_RMAX'                                         1139
         CALL AXSHAPE                                                     1139
C    *                                                                    1139
6571  CALL AGSPOPB                                                        1140
      GO TO BLOCK_SBRL, (6570 ,6580)                                      1140
C End of Local Procedure                                                  1140
C ----------------------------------------------------------------------  1141
C ------------ SBRX                                                       1141
C Begin Local Procedure SBRX                                              1142
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1142
6590  CONTINUE                                                            1142
         AG_TITLE='SBRX'                                                  1142
         AG_MODULE=MODULE                                                 1142
         AG_MARK='V00'                                                    1142
         CALL AGSPUSH                                                     1142
         AG_MODULE=MODULE                                                 1142
C    *                                                                    1142
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1143
         CALL AGSRESET                                                    1143
         AG_TITLE = 'IRON'                                                1143
         AG_EXNAME = 'MATERIAL'                                           1143
         AG_PARLIST = 'NONE'                                              1143
         CALL AXMATERIAL                                                  1143
C    *                                                                    1143
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1143
         CALL AGSRESET                                                    1144
         AG_SEEN = 1                                                      1144
         AG_COLO = 2                                                      1144
         AG_TITLE = 'SBRX'                                                1144
         AG_EXNAME = 'ATTRIBUTE'                                          1144
         AG_PARLIST = 'SEEN_COLO'                                         1144
         CALL AXATTRIBUTE                                                 1144
C    *                                                                    1144
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1144
C    LEN/2 <===                                                           1144
         CALL AGSRESET                                                    1146
         AG_RMIN = 0.0                                                    1146
         AG_RMAX = SSUB_SROLLID/2                                         1146
         AG_DZ = SSUB_SWIRELEN/2                                          1146
         AG_TITLE = 'TUBE'                                                1146
         AG_EXNAME = 'SHAPE'                                              1146
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1146
         CALL AXSHAPE                                                     1146
C    *                                                                    1146
6591  CALL AGSPOPB                                                        1147
      GO TO BLOCK_SBRX, (6590 ,6600)                                      1147
C End of Local Procedure                                                  1147
C ----------------------------------------------------------------------  1148
C ------------ SBMM                                                       1148
C Begin Local Procedure SBMM                                              1149
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1149
6540  CONTINUE                                                            1149
         AG_TITLE='SBMM'                                                  1149
         AG_MODULE=MODULE                                                 1149
         AG_MARK='V00'                                                    1149
         CALL AGSPUSH                                                     1149
         AG_MODULE=MODULE                                                 1149
C    *                                                                    1149
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1150
         CALL AGSRESET                                                    1150
         AG_TITLE = 'AIR'                                                 1150
         AG_EXNAME = 'MATERIAL'                                           1150
         AG_PARLIST = 'NONE'                                              1150
         CALL AXMATERIAL                                                  1150
C    *                                                                    1150
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1150
         CALL AGSRESET                                                    1151
         AG_SEEN = 0                                                      1151
         AG_COLO = 1                                                      1151
         AG_TITLE = 'SBMM'                                                1151
         AG_EXNAME = 'ATTRIBUTE'                                          1151
         AG_PARLIST = 'SEEN_COLO'                                         1151
         CALL AXATTRIBUTE                                                 1151
C    *                                                                    1151
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1151
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1151
         CALL AGSRESET                                                    1153
         AG_DX = SSUB_MBLKIWID/2                                          1153
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1153
         AG_DZ = SSUB_MBLKHGH/2                                           1153
         AG_TITLE = 'BOX'                                                 1153
         AG_EXNAME = 'SHAPE'                                              1153
         AG_PARLIST = 'DX_DY_DZ'                                          1153
         CALL AXSHAPE                                                     1153
C    *                                                                    1153
C    Create SMRD :                                                        1154
C    Call To Local Procedure SMRD                                         1154
         ASSIGN 6720 TO BLOCK_SMRD                                        1154
         GO TO  6710                                                      1154
6720     CONTINUE                                                         1154
C    *                                                                    1154
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1155
     *   SSUB_SROLLOD                                                     1155
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1156
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1156
         CALL AGSRESET                                                    1156
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1156
         AG_ALPHAN=AG_ALPHAN+1                                            1156
         AG_ALPHAK(AG_ALPHAN)=1                                           1156
         AG_ALPHAL(AG_ALPHAN)=90                                          1156
         AG_TITLE = 'SMRD'                                                1156
         AG_EXNAME = 'POSITION'                                           1156
         AG_PARLIST = 'Y_ALPHAX'                                          1156
         CALL AXPOSITION                                                  1156
C    *                                                                    1156
C    Create SBMO :                                                        1157
C    Call To Local Procedure SBMO                                         1157
         ASSIGN 6740 TO BLOCK_SBMO                                        1157
         GO TO  6730                                                      1157
6740     CONTINUE                                                         1157
C    *                                                                    1157
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1158
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1159
C    UB_SROLLOD)/2 <===                                                   1159
         CALL AGSRESET                                                    1159
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1159
         AG_TITLE = 'SBMO'                                                1159
         AG_EXNAME = 'POSITION'                                           1159
         AG_PARLIST = 'Y'                                                 1159
         CALL AXPOSITION                                                  1159
C    *                                                                    1159
C    Create SBMI :                                                        1160
C    Call To Local Procedure SBMI                                         1160
         ASSIGN 6760 TO BLOCK_SBMI                                        1160
         GO TO  6750                                                      1160
6760     CONTINUE                                                         1160
C    *                                                                    1160
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1161
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1162
C    UB_SROLLOD)/2 <===                                                   1162
         CALL AGSRESET                                                    1162
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1162
         AG_TITLE = 'SBMI'                                                1162
         AG_EXNAME = 'POSITION'                                           1162
         AG_PARLIST = 'Y'                                                 1162
         CALL AXPOSITION                                                  1162
C    *                                                                    1162
6541  CALL AGSPOPB                                                        1163
      GO TO BLOCK_SBMM, (6540 ,6550,6560)                                 1163
C End of Local Procedure                                                  1163
C ----------------------------------------------------------------------  1164
C ------------ SMRD                                                       1164
C Begin Local Procedure SMRD                                              1165
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1165
6710  CONTINUE                                                            1165
         AG_TITLE='SMRD'                                                  1165
         AG_MODULE=MODULE                                                 1165
         AG_MARK='V00'                                                    1165
         CALL AGSPUSH                                                     1165
         AG_MODULE=MODULE                                                 1165
C    *                                                                    1165
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1166
         CALL AGSRESET                                                    1166
         AG_TITLE = 'ALUMINIUM'                                           1166
         AG_EXNAME = 'MATERIAL'                                           1166
         AG_PARLIST = 'NONE'                                              1166
         CALL AXMATERIAL                                                  1166
C    *                                                                    1166
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1166
         CALL AGSRESET                                                    1167
         AG_SEEN = 1                                                      1167
         AG_COLO = 2                                                      1167
         AG_TITLE = 'SMRD'                                                1167
         AG_EXNAME = 'ATTRIBUTE'                                          1167
         AG_PARLIST = 'SEEN_COLO'                                         1167
         CALL AXATTRIBUTE                                                 1167
C    *                                                                    1167
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1167
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1167
         CALL AGSRESET                                                    1169
         AG_RMIN = 0.0                                                    1169
         AG_RMAX = SSUB_MRODDIA/2                                         1169
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1169
         AG_TITLE = 'TUBE'                                                1169
         AG_EXNAME = 'SHAPE'                                              1169
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1169
         CALL AXSHAPE                                                     1169
C    *                                                                    1169
6711  CALL AGSPOPB                                                        1170
      GO TO BLOCK_SMRD, (6710 ,6720)                                      1170
C End of Local Procedure                                                  1170
C ----------------------------------------------------------------------  1171
C ------------ SBMO                                                       1171
C Begin Local Procedure SBMO                                              1172
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1172
6730  CONTINUE                                                            1172
         AG_TITLE='SBMO'                                                  1172
         AG_MODULE=MODULE                                                 1172
         AG_MARK='V00'                                                    1172
         CALL AGSPUSH                                                     1172
         AG_MODULE=MODULE                                                 1172
C    *                                                                    1172
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1173
         CALL AGSRESET                                                    1173
         AG_TITLE = 'G10'                                                 1173
         AG_EXNAME = 'MATERIAL'                                           1173
         AG_PARLIST = 'NONE'                                              1173
         CALL AXMATERIAL                                                  1173
C    *                                                                    1173
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1173
         CALL AGSRESET                                                    1174
         AG_SEEN = 1                                                      1174
         AG_COLO = 3                                                      1174
         AG_TITLE = 'SBMO'                                                1174
         AG_EXNAME = 'ATTRIBUTE'                                          1174
         AG_PARLIST = 'SEEN_COLO'                                         1174
         CALL AXATTRIBUTE                                                 1174
C    *                                                                    1174
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1174
C    UB_MBLKHGH/2 <===                                                    1174
         CALL AGSRESET                                                    1176
         AG_DX = SSUB_MBLKOWID/2                                          1176
         AG_DY = SSUB_MBLKOLEN/2                                          1176
         AG_DZ = SSUB_MBLKHGH/2                                           1176
         AG_TITLE = 'BOX'                                                 1176
         AG_EXNAME = 'SHAPE'                                              1176
         AG_PARLIST = 'DX_DY_DZ'                                          1176
         CALL AXSHAPE                                                     1176
C    *                                                                    1176
6731  CALL AGSPOPB                                                        1177
      GO TO BLOCK_SBMO, (6730 ,6740)                                      1177
C End of Local Procedure                                                  1177
C ----------------------------------------------------------------------  1178
C ------------ SBMI                                                       1178
C Begin Local Procedure SBMI                                              1179
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1179
6750  CONTINUE                                                            1179
         AG_TITLE='SBMI'                                                  1179
         AG_MODULE=MODULE                                                 1179
         AG_MARK='V00'                                                    1179
         CALL AGSPUSH                                                     1179
         AG_MODULE=MODULE                                                 1179
C    *                                                                    1179
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1180
         CALL AGSRESET                                                    1180
         AG_TITLE = 'G10'                                                 1180
         AG_EXNAME = 'MATERIAL'                                           1180
         AG_PARLIST = 'NONE'                                              1180
         CALL AXMATERIAL                                                  1180
C    *                                                                    1180
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1180
         CALL AGSRESET                                                    1181
         AG_SEEN = 1                                                      1181
         AG_COLO = 3                                                      1181
         AG_TITLE = 'SBMO'                                                1181
         AG_EXNAME = 'ATTRIBUTE'                                          1181
         AG_PARLIST = 'SEEN_COLO'                                         1181
         CALL AXATTRIBUTE                                                 1181
C    *                                                                    1181
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1181
C    UB_MBLKHGH/2 <===                                                    1181
         CALL AGSRESET                                                    1183
         AG_DX = SSUB_MBLKIWID/2                                          1183
         AG_DY = SSUB_MBLKILEN/2                                          1183
         AG_DZ = SSUB_MBLKHGH/2                                           1183
         AG_TITLE = 'BOX'                                                 1183
         AG_EXNAME = 'SHAPE'                                              1183
         AG_PARLIST = 'DX_DY_DZ'                                          1183
         CALL AXSHAPE                                                     1183
C    *                                                                    1183
6751  CALL AGSPOPB                                                        1184
      GO TO BLOCK_SBMI, (6750 ,6760)                                      1184
C End of Local Procedure                                                  1184
C ----------------------------------------------------------------------  1185
C ------------ SALM                                                       1185
C Begin Local Procedure SALM                                              1186
C SALM IS THE ALUMINUM SHIELD MESH                                        1186
5370  CONTINUE                                                            1186
         AG_TITLE='SALM'                                                  1186
         AG_MODULE=MODULE                                                 1186
         AG_MARK='V00'                                                    1186
         CALL AGSPUSH                                                     1186
         AG_MODULE=MODULE                                                 1186
C    *                                                                    1186
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1187
         CALL AGSRESET                                                    1187
         AG_TITLE = 'ALUMINIUM'                                           1187
         AG_EXNAME = 'MATERIAL'                                           1187
         AG_PARLIST = 'NONE'                                              1187
         CALL AXMATERIAL                                                  1187
C    *                                                                    1187
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1187
         CALL AGSRESET                                                    1188
         AG_SEEN = 1                                                      1188
         AG_COLO = 2                                                      1188
         AG_TITLE = 'SALM'                                                1188
         AG_EXNAME = 'ATTRIBUTE'                                          1188
         AG_PARLIST = 'SEEN_COLO'                                         1188
         CALL AXATTRIBUTE                                                 1188
C    *                                                                    1188
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1188
C    DZ=SSLD_ALMSHTHK/2 <===                                              1188
         CALL AGSRESET                                                    1190
         AG_RMIN = SSLD_ALMESHID/2                                        1190
         AG_RMAX = SSLD_ALMESHOD/2                                        1190
         AG_DZ = SSLD_ALMSHTHK/2                                          1190
         AG_TITLE = 'TUBE'                                                1190
         AG_EXNAME = 'SHAPE'                                              1190
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1190
         CALL AXSHAPE                                                     1190
C    *                                                                    1190
5371  CALL AGSPOPB                                                        1191
      GO TO BLOCK_SALM, (5370 ,5380)                                      1191
C End of Local Procedure                                                  1191
C ----------------------------------------------------------------------  1192
C ------------ SISH                                                       1192
C Begin Local Procedure SISH                                              1193
C SISH IS THE INNER SHIELD CYLINDER                                       1193
5310  CONTINUE                                                            1193
         AG_TITLE='SISH'                                                  1193
         AG_MODULE=MODULE                                                 1193
         AG_MARK='V00'                                                    1193
         CALL AGSPUSH                                                     1193
         AG_MODULE=MODULE                                                 1193
C    *                                                                    1193
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1194
         CALL AGSRESET                                                    1194
         AG_TITLE = 'ALKAP'                                               1194
         AG_EXNAME = 'MATERIAL'                                           1194
         AG_PARLIST = 'NONE'                                              1194
         CALL AXMATERIAL                                                  1194
C    *                                                                    1194
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1194
         CALL AGSRESET                                                    1195
         AG_SEEN = 1                                                      1195
         AG_COLO = 3                                                      1195
         AG_TITLE = 'SISH'                                                1195
         AG_EXNAME = 'ATTRIBUTE'                                          1195
         AG_PARLIST = 'SEEN_COLO'                                         1195
         CALL AXATTRIBUTE                                                 1195
C    *                                                                    1195
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1195
C    D_SINLEN <===                                                        1195
         CALL AGSRESET                                                    1197
         AG_RMIN = SSLD_SINRINN                                           1197
         AG_RMAX = SSLD_SINROUT                                           1197
         AG_DZ = SSLD_SINLEN                                              1197
         AG_TITLE = 'TUBE'                                                1197
         AG_EXNAME = 'SHAPE'                                              1197
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1197
         CALL AXSHAPE                                                     1197
C    *                                                                    1197
5311  CALL AGSPOPB                                                        1198
      GO TO BLOCK_SISH, (5310 ,5320)                                      1198
C End of Local Procedure                                                  1198
C ----------------------------------------------------------------------  1199
C ------------ SSSH                                                       1199
C Begin Local Procedure SSSH                                              1200
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1200
5350  CONTINUE                                                            1200
         AG_TITLE='SSSH'                                                  1200
         AG_MODULE=MODULE                                                 1200
         AG_MARK='V00'                                                    1200
         CALL AGSPUSH                                                     1200
         AG_MODULE=MODULE                                                 1200
C    *                                                                    1200
C    Geant MATERIAL : SSDAMY ===> inherited arguments used <===           1201
         CALL AGSRESET                                                    1201
         AG_TITLE = 'SSDAMY'                                              1201
         AG_EXNAME = 'MATERIAL'                                           1201
         AG_PARLIST = 'NONE'                                              1201
         CALL AXMATERIAL                                                  1201
C    *                                                                    1201
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1201
         CALL AGSRESET                                                    1202
         AG_SEEN = 1                                                      1202
         AG_COLO = 3                                                      1202
         AG_TITLE = 'SSSH'                                                1202
         AG_EXNAME = 'ATTRIBUTE'                                          1202
         AG_PARLIST = 'SEEN_COLO'                                         1202
         CALL AXATTRIBUTE                                                 1202
C    *                                                                    1202
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1202
C    SLD_SSEPLEN <===                                                     1202
         CALL AGSRESET                                                    1204
         AG_RMIN = SSLD_SSEPRINN                                          1204
         AG_RMAX = SSLD_SSEPROUT                                          1204
         AG_DZ = SSLD_SSEPLEN                                             1204
         AG_TITLE = 'TUBE'                                                1204
         AG_EXNAME = 'SHAPE'                                              1204
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1204
         CALL AXSHAPE                                                     1204
C    *                                                                    1204
5351  CALL AGSPOPB                                                        1205
      GO TO BLOCK_SSSH, (5350 ,5360)                                      1205
C End of Local Procedure                                                  1205
C ----------------------------------------------------------------------  1206
C ------------ SOSH                                                       1206
C Begin Local Procedure SOSH                                              1207
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1207
5330  CONTINUE                                                            1207
         AG_TITLE='SOSH'                                                  1207
         AG_MODULE=MODULE                                                 1207
         AG_MARK='V00'                                                    1207
         CALL AGSPUSH                                                     1207
         AG_MODULE=MODULE                                                 1207
C    *                                                                    1207
C    Geant MATERIAL : SSDAMY ===> inherited arguments used <===           1208
         CALL AGSRESET                                                    1208
         AG_TITLE = 'SSDAMY'                                              1208
         AG_EXNAME = 'MATERIAL'                                           1208
         AG_PARLIST = 'NONE'                                              1208
         CALL AXMATERIAL                                                  1208
C    *                                                                    1208
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1208
         CALL AGSRESET                                                    1209
         AG_SEEN = 1                                                      1209
         AG_COLO = 3                                                      1209
         AG_TITLE = 'SOSH'                                                1209
         AG_EXNAME = 'ATTRIBUTE'                                          1209
         AG_PARLIST = 'SEEN_COLO'                                         1209
         CALL AXATTRIBUTE                                                 1209
C    *                                                                    1209
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1209
C    SLD_SOUTLEN <===                                                     1209
         CALL AGSRESET                                                    1211
         AG_RMIN = SSLD_SOUTRINN                                          1211
         AG_RMAX = SSLD_SOUTROUT                                          1211
         AG_DZ = SSLD_SOUTLEN                                             1211
         AG_TITLE = 'TUBE'                                                1211
         AG_EXNAME = 'SHAPE'                                              1211
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1211
         CALL AXSHAPE                                                     1211
C    *                                                                    1211
5331  CALL AGSPOPB                                                        1212
      GO TO BLOCK_SOSH, (5330 ,5340)                                      1212
C End of Local Procedure                                                  1212
      WRITE(LOUT,6770)'===>>  [End AgML Module SVTTGEO]  <<==='           1212
6770  FORMAT(A32,/,/)                                                     1212
C MODULE SVTTGEO                                                          1214
      END                                                                 1214
