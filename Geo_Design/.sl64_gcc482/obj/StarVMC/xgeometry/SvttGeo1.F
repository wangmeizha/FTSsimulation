C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO1                                                 1
C SVTTGEO1 IS THE SVT GEOMETRY FOR STAR                                   1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SVTTGEO1'/, BANK_TITLE/'*'/                            1
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
C AUTHOR MAXIM POTEKHIN                                                   1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 05 AUG 03                                                       2
      CHARACTER*40 CREATED /'05 AUG 03'/                                  3
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
      SVTL_RADIUS=6.37+0.015                                              159
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
      SVTL_RADIUS=7.38+0.015                                              165
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
      SVTL_RADIUS=10.38+0.015                                             169
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
      SVTL_RADIUS=11.27+0.015                                             175
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
      SVTL_RADIUS=14.19+0.015                                             179
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
      SVTL_RADIUS=15.13+0.015                                             185
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
      CALL AGDGETP('SVTTGEO1','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,  225
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      225
     *LINK_SVTG,FLAG_SVTG)                                                225
C use default SWCA bank                                                   226
      BPATH_SWCA='SWCA*'                                                  226
      IDNUM_SWCA(1)=0                                                     226
      CALL AGDGETP('SVTTGEO1','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,  226
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      226
     *LINK_SWCA,FLAG_SWCA)                                                226
C use default SELC bank                                                   227
      BPATH_SELC='SELC*'                                                  227
      IDNUM_SELC(1)=0                                                     227
      CALL AGDGETP('SVTTGEO1','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  227
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      227
     *LINK_SELC,FLAG_SELC)                                                227
C use default SSUP bank                                                   228
      BPATH_SSUP='SSUP*'                                                  228
      IDNUM_SSUP(1)=0                                                     228
      CALL AGDGETP('SVTTGEO1','*',-999,'SSUP',' ',BANK_SSUP, BPATH_SSUP,  228
     *IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,BANK_SSUP,LEN_SSUP,LLEN_SSUP,      228
     *LINK_SSUP,FLAG_SSUP)                                                228
C use default SSUB bank                                                   229
      BPATH_SSUB='SSUB*'                                                  229
      IDNUM_SSUB(1)=0                                                     229
      CALL AGDGETP('SVTTGEO1','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,  229
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      229
     *LINK_SSUB,FLAG_SSUB)                                                229
C use default SERG bank                                                   230
      BPATH_SERG='SERG*'                                                  230
      IDNUM_SERG(1)=0                                                     230
      CALL AGDGETP('SVTTGEO1','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,  230
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      230
     *LINK_SERG,FLAG_SERG)                                                230
C use default SWAM bank                                                   231
      BPATH_SWAM='SWAM*'                                                  231
      IDNUM_SWAM(1)=0                                                     231
      CALL AGDGETP('SVTTGEO1','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,  231
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      231
     *LINK_SWAM,FLAG_SWAM)                                                231
C use default SELC bank                                                   232
      BPATH_SELC='SELC*'                                                  232
      IDNUM_SELC(1)=0                                                     232
      CALL AGDGETP('SVTTGEO1','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  232
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      232
     *LINK_SELC,FLAG_SELC)                                                232
C use default SFPA bank                                                   233
      BPATH_SFPA='SFPA*'                                                  233
      IDNUM_SFPA(1)=0                                                     233
      CALL AGDGETP('SVTTGEO1','*',-999,'SFPA',' ',BANK_SFPA, BPATH_SFPA,  233
     *IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,BANK_SFPA,LEN_SFPA,LLEN_SFPA,      233
     *LINK_SFPA,FLAG_SFPA)                                                233
C use default SSLD bank                                                   234
      BPATH_SSLD='SSLD*'                                                  234
      IDNUM_SSLD(1)=0                                                     234
      CALL AGDGETP('SVTTGEO1','*',-999,'SSLD',' ',BANK_SSLD, BPATH_SSLD,  234
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
C Geant COMPONENT : AL ===> A=27 Z=13 W=2302 <===                         242
      CALL AGSRESET                                                       243
      AG_A = 27                                                           243
      AG_Z = 13                                                           243
      AG_W = 2302                                                         243
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
            CALL AGDGETP('SVTTGEO1','*',-999,'SVTL',NSVTL_LAYER,          313
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
            CALL AGDGETP('SVTTGEO1','*',-999,'SVTL',NSVTL_LAYER,          318
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
         CALL AGDGETP('SVTTGEO1','*',-999,'SVTL',NSVTL_LAYER,             323
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
         RMAX=SQRT((SVTL_RADIUS)**2 + (SWCA_WAFERWID/2)**2) + ELETHK      353
         RMAX=MIN(RMAX,RADMAX - LADTHK)                                   354
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=SWCA_LENGTH/2 <===    355
         CALL AGSRESET                                                    355
         AG_RMIN = RMIN                                                   355
         AG_RMAX = RMAX                                                   355
         AG_DZ = SWCA_LENGTH/2                                            355
         AG_TITLE = 'TUBE'                                                355
         AG_EXNAME = 'SHAPE'                                              355
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      355
         CALL AXSHAPE                                                     355
C    *                                                                    355
C    Create SLSD :                                                        356
C    Call To Local Procedure SLSD                                         356
         ASSIGN 5400 TO BLOCK_SLSD                                        356
         GO TO  5390                                                      356
5400     CONTINUE                                                         356
C    *                                                                    356
5261  CALL AGSPOPB                                                        357
      GO TO BLOCK_SLYD, (5260 ,5270,5280)                                 357
C End of Local Procedure                                                  357
C ----------------------------------------------------------------------  358
C ------------ SLSD                                                       358
C Begin Local Procedure SLSD                                              359
C SLSD IS A SINGLE LADDER MOTHER (SECTOR OF TUBE)                         359
5390  CONTINUE                                                            359
         AG_TITLE='SLSD'                                                  359
         AG_MODULE=MODULE                                                 359
         AG_MARK='V00'                                                    359
         CALL AGSPUSH                                                     359
         AG_MODULE=MODULE                                                 359
C    *                                                                    359
C    Geant ATTRIBUTE : SLSD ===> SEEN=0 COLO=1 <===                       360
         CALL AGSRESET                                                    360
         AG_SEEN = 0                                                      360
         AG_COLO = 1                                                      360
         AG_TITLE = 'SLSD'                                                360
         AG_EXNAME = 'ATTRIBUTE'                                          360
         AG_PARLIST = 'SEEN_COLO'                                         360
         CALL AXATTRIBUTE                                                 360
C    *                                                                    360
C    Check SVTG_NLAYER<0                                                  360
         IF (SVTG_NLAYER.LT.0) THEN                                       361
C    Geant SHAPE : DIVISION ===> NDIV=1 IAXIS=2 C0=-90 <===               362
         CALL AGSRESET                                                    362
         AG_NDIV = 1                                                      362
         AG_IAXIS = 2                                                     362
         AG_C0 = -90                                                      362
         AG_TITLE = 'DIVISION'                                            362
         AG_EXNAME = 'SHAPE'                                              362
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     362
         CALL AXSHAPE                                                     362
C    *                                                                    362
         ELSE                                                             362
C    Check SVTG_VERSION==1                                                364
         IF (SVTG_VERSION.EQ.1) THEN                                      364
         C0=-DEG*MOD(ILAYER,2)+SVTG_ANGOFF*INT((ILAYER-1)/2)              365
         ELSE                                                             366
         C0=90-DEG*MOD(ILAYER,2)                                          367
         ENDIF                                                            368
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NLADDER IAXIS=2, C0=C0 <===    370
         CALL AGSRESET                                                    370
         AG_NDIV = SVTL_NLADDER                                           370
         AG_IAXIS = 2                                                     370
         AG_C0 = C0                                                       370
         AG_TITLE = 'DIVISION'                                            370
         AG_EXNAME = 'SHAPE'                                              370
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     370
         CALL AXSHAPE                                                     370
C    *                                                                    370
         ENDIF                                                            370
C    Create SLDI :                                                        372
C    Call To Local Procedure SLDI                                         372
         ASSIGN 5420 TO BLOCK_SLDI                                        372
         GO TO  5410                                                      372
5420     CONTINUE                                                         372
C    *                                                                    372
C    Geant POSITION : SLDI ===> X=SVTL_RADIUS ORT=YZX <===                373
         CALL AGSRESET                                                    373
         AG_X = SVTL_RADIUS                                               373
         AG_ALPHAN=AG_ALPHAN+1                                            373
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 373
         AG_TITLE = 'SLDI'                                                373
         AG_EXNAME = 'POSITION'                                           373
         AG_PARLIST = 'X_ORT'                                             373
         CALL AXPOSITION                                                  373
C    *                                                                    373
C    Create SELE :                                                        374
C    Call To Local Procedure SELE                                         374
         ASSIGN 5440 TO BLOCK_SELE                                        374
         GO TO  5430                                                      374
5440     CONTINUE                                                         374
C    *                                                                    374
         DEG=180.0/SVTL_NLADDER                                           375
         RAD=(TWOPI/2)/SVTL_NLADDER                                       376
         XPOS=SIN(RAD)*SELC_ELCAWID/2 - COS(RAD)*ELETHK/2                 377
         YPOS=COS(RAD)*SELC_ELCAWID/2 + SIN(RAD)*ELETHK/2                 378
C    *                                                                    379
         S=-1                                                             379
            GOTO 5453                                                     379
5451        S=S+(2)                                                       379
5453        IF((2)*(S-(1)).GT.0) GO TO 5452                               379
            SIDE=S                                                        380
C       Geant POSITION : SELE ===> X=SVTL_RADIUS-LADTHK-XPOS Y=S*(SWCA_W  382
C       AFERWID/2+YPOS) ORT=YZX ALPHAX=90*(1-S) ALPHAZ=S*DEG <===         382
            CALL AGSRESET                                                 382
            AG_X = SVTL_RADIUS-LADTHK-XPOS                                382
            AG_Y = S*(SWCA_WAFERWID/2+YPOS)                               382
            AG_ALPHAN=AG_ALPHAN+1                                         382
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              382
            AG_ALPHAN=AG_ALPHAN+1                                         382
            AG_ALPHAK(AG_ALPHAN)=1                                        382
            AG_ALPHAL(AG_ALPHAN)=90*(1-S)                                 382
            AG_ALPHAN=AG_ALPHAN+1                                         382
            AG_ALPHAK(AG_ALPHAN)=3                                        382
            AG_ALPHAL(AG_ALPHAN)=S*DEG                                    382
            AG_TITLE = 'SELE'                                             382
            AG_EXNAME = 'POSITION'                                        382
            AG_PARLIST = 'X_Y_ORT_ALPHAX_ALPHAZ'                          382
            CALL AXPOSITION                                               382
C       *                                                                 382
         GO TO 5451                                                       383
5452     CONTINUE                                                         383
5391  CALL AGSPOPB                                                        384
      GO TO BLOCK_SLSD, (5390 ,5400)                                      384
C End of Local Procedure                                                  384
C ----------------------------------------------------------------------  385
C ------------ SLDI                                                       385
C Begin Local Procedure SLDI                                              386
C SLDI IS A LADDER VOLUME                                                 386
5410  CONTINUE                                                            386
         AG_TITLE='SLDI'                                                  386
         AG_MODULE=MODULE                                                 386
         AG_MARK='V00'                                                    386
         CALL AGSPUSH                                                     386
         AG_MODULE=MODULE                                                 386
C    *                                                                    386
         TABLEN=SWCA_LENGTH/2 - 7*(SWCA_WAFERWID/2 + SWCA_WAFERGAP)       387
C    Geant MATERIAL : AIR ===> inherited arguments used <===              388
         CALL AGSRESET                                                    388
         AG_TITLE = 'AIR'                                                 388
         AG_EXNAME = 'MATERIAL'                                           388
         AG_PARLIST = 'NONE'                                              388
         CALL AXMATERIAL                                                  388
C    *                                                                    388
C    Geant ATTRIBUTE : SLDI ===> SEEN=0 COLO=1 SERIAL=SVTL_NWAFER <===    388
         CALL AGSRESET                                                    389
         AG_SEEN = 0                                                      389
         AG_COLO = 1                                                      389
         AG_SERIAL = SVTL_NWAFER                                          389
         AG_TITLE = 'SLDI'                                                389
         AG_EXNAME = 'ATTRIBUTE'                                          389
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  389
         CALL AXATTRIBUTE                                                 389
C    *                                                                    389
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2 DY=SWCA_LENGTH/2, DZ=LADT  389
C    HK <===                                                              389
         CALL AGSRESET                                                    391
         AG_DX = SWCA_WAFERWID/2                                          391
         AG_DY = SWCA_LENGTH/2                                            391
         AG_DZ = LADTHK                                                   391
         AG_TITLE = 'BOX'                                                 391
         AG_EXNAME = 'SHAPE'                                              391
         AG_PARLIST = 'DX_DY_DZ'                                          391
         CALL AXSHAPE                                                     391
C    *                                                                    391
C    Create STLI :                                                        392
C    Call To Local Procedure STLI                                         392
         ASSIGN 5470 TO BLOCK_STLI                                        392
         GO TO  5460                                                      392
5470     CONTINUE                                                         392
C    *                                                                    392
C    Geant POSITION : STLI ===> inherited arguments used <===             393
         CALL AGSRESET                                                    393
         AG_TITLE = 'STLI'                                                393
         AG_EXNAME = 'POSITION'                                           393
         AG_PARLIST = 'NONE'                                              393
         CALL AXPOSITION                                                  393
C    *                                                                    393
C    Create SBER :                                                        394
C    Call To Local Procedure SBER                                         394
         ASSIGN 5490 TO BLOCK_SBER                                        394
         GO TO  5480                                                      394
5490     CONTINUE                                                         394
C    *                                                                    394
C    Geant POSITION : SBER ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  395
C    DTHK+SWCA_WAFCARTH/2 <===                                            395
         CALL AGSRESET                                                    395
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          395
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   395
         AG_TITLE = 'SBER'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'X_Z'                                               395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Geant POSITION : SBER ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  395
C    DTHK+SWCA_WAFCARTH/2 <===                                            395
         CALL AGSRESET                                                    396
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          396
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   396
         AG_TITLE = 'SBER'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Z'                                               396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Create STAB :                                                        397
C    Call To Local Procedure STAB                                         397
         ASSIGN 5510 TO BLOCK_STAB                                        397
         GO TO  5500                                                      397
5510     CONTINUE                                                         397
C    *                                                                    397
C    Geant POSITION : STAB ===> Y=SWCA_LENGTH/2-TABLEN/2 Z=-LADTHK+SWCA_  398
C    WAFCARTH/2 <===                                                      398
         CALL AGSRESET                                                    398
         AG_Y = SWCA_LENGTH/2-TABLEN/2                                    398
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   398
         AG_TITLE = 'STAB'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'Y_Z'                                               398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Geant POSITION : STAB ===> Y=-SWCA_LENGTH/2+TABLEN/2 Z=-LADTHK+SWCA  398
C    _WAFCARTH/2 <===                                                     398
         CALL AGSRESET                                                    399
         AG_Y = -SWCA_LENGTH/2+TABLEN/2                                   399
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   399
         AG_TITLE = 'STAB'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'Y_Z'                                               399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Create STRU :                                                        400
C    Call To Local Procedure STRU                                         400
         ASSIGN 5530 TO BLOCK_STRU                                        400
         GO TO  5520                                                      400
5530     CONTINUE                                                         400
C    *                                                                    400
C    Geant POSITION : STRU ===> Y=(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAF  403
C    ERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <==  403
C    =                                                                    403
         CALL AGSRESET                                                    403
         AG_Y = (SVTL_NWAFER*(SWCA_WAFERLEN/                              403
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  403
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   403
         AG_TITLE = 'STRU'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'Y_Z'                                               403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Geant POSITION : STRU ===> Y=-(SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WA  403
C    FERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2) Z=-LADTHK+SWCA_WAFCARTH/2 <=  403
C    ==                                                                   403
         CALL AGSRESET                                                    406
         AG_Y = -(SVTL_NWAFER*(SWCA_WAFERLEN/                             406
     *   2+SWCA_WAFERGAP)+SWCA_WAFERGAP+SWCA_STRUTLEN/2)                  406
         AG_Z = -LADTHK+SWCA_WAFCARTH/2                                   406
         AG_TITLE = 'STRU'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'Y_Z'                                               406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Create SRHC :                                                        407
C    Call To Local Procedure SRHC                                         407
         ASSIGN 5550 TO BLOCK_SRHC                                        407
         GO TO  5540                                                      407
5550     CONTINUE                                                         407
C    *                                                                    407
C    Geant POSITION : SRHC ===> X=+SWCA_WAFERWID/2-SWCA_WAFCARWD/2 Z=-LA  409
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         409
         CALL AGSRESET                                                    409
         AG_X = +SWCA_WAFERWID/2-SWCA_WAFCARWD/2                          409
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                409
         AG_TITLE = 'SRHC'                                                409
         AG_EXNAME = 'POSITION'                                           409
         AG_PARLIST = 'X_Z'                                               409
         CALL AXPOSITION                                                  409
C    *                                                                    409
C    Geant POSITION : SRHC ===> X=-SWCA_WAFERWID/2+SWCA_WAFCARWD/2 Z=-LA  409
C    DTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2 <===                         409
         CALL AGSRESET                                                    411
         AG_X = -SWCA_WAFERWID/2+SWCA_WAFCARWD/2                          411
         AG_Z = -LADTHK+2.0*SWCA_WAFCARTH/2+SWCA_ROHATHK/2                411
         AG_TITLE = 'SRHC'                                                411
         AG_EXNAME = 'POSITION'                                           411
         AG_PARLIST = 'X_Z'                                               411
         CALL AXPOSITION                                                  411
C    *                                                                    411
5411  CALL AGSPOPB                                                        412
      GO TO BLOCK_SLDI, (5410 ,5420)                                      412
C End of Local Procedure                                                  412
C ----------------------------------------------------------------------  413
C ------------ SRHC                                                       413
C Begin Local Procedure SRHC                                              414
C SRHC IS THE ROHA CELL WAFER SUPPORT                                     414
5540  CONTINUE                                                            414
         AG_TITLE='SRHC'                                                  414
         AG_MODULE=MODULE                                                 414
         AG_MARK='V00'                                                    414
         CALL AGSPUSH                                                     414
         AG_MODULE=MODULE                                                 414
C    *                                                                    414
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           415
         CALL AGSRESET                                                    415
         AG_A = 12                                                        415
         AG_Z = 6                                                         415
         AG_W = 1                                                         415
         AG_TITLE = 'C'                                                   415
         AG_EXNAME = 'COMPONENT'                                          415
         AG_PARLIST = 'A_Z_W'                                             415
         CALL AXCOMPONENT                                                 415
C    *                                                                    415
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           415
         CALL AGSRESET                                                    416
         AG_A = 1                                                         416
         AG_Z = 1                                                         416
         AG_W = 2                                                         416
         AG_TITLE = 'H2'                                                  416
         AG_EXNAME = 'COMPONENT'                                          416
         AG_PARLIST = 'A_Z_W'                                             416
         CALL AXCOMPONENT                                                 416
C    *                                                                    416
C    Geant MIXTURE : ROHA ===> DENS=0.0304 <===                           416
         CALL AGSRESET                                                    417
         AG_DENS = 0.0304                                                 417
         AG_TITLE = 'ROHA'                                                417
         AG_EXNAME = 'MIXTURE'                                            417
         AG_PARLIST = 'DENS'                                              417
         CALL AXMIXTURE                                                   417
C    *                                                                    417
C    Geant ATTRIBUTE : SRHC ===> SEEN=1 COLO=3 <===                       417
         CALL AGSRESET                                                    418
         AG_SEEN = 1                                                      418
         AG_COLO = 3                                                      418
         AG_TITLE = 'SRHC'                                                418
         AG_EXNAME = 'ATTRIBUTE'                                          418
         AG_PARLIST = 'SEEN_COLO'                                         418
         CALL AXATTRIBUTE                                                 418
C    *                                                                    418
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SVTL_NWAFER*(SWCA_WAFE  418
C    RLEN/2+SWCA_WAFERGAP), DZ=SWCA_ROHATHK/2 <===                        418
         CALL AGSRESET                                                    420
         AG_DX = SWCA_WAFCARWD/2                                          420
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              420
         AG_DZ = SWCA_ROHATHK/2                                           420
         AG_TITLE = 'BOX'                                                 420
         AG_EXNAME = 'SHAPE'                                              420
         AG_PARLIST = 'DX_DY_DZ'                                          420
         CALL AXSHAPE                                                     420
C    *                                                                    420
5541  CALL AGSPOPB                                                        421
      GO TO BLOCK_SRHC, (5540 ,5550)                                      421
C End of Local Procedure                                                  421
C ----------------------------------------------------------------------  422
C ------------ STLI                                                       422
C Begin Local Procedure STLI                                              423
C STLI IS THE WAVER PACK CONTAINER                                        423
5460  CONTINUE                                                            423
         AG_TITLE='STLI'                                                  423
         AG_MODULE=MODULE                                                 423
         AG_MARK='V00'                                                    423
         CALL AGSPUSH                                                     423
         AG_MODULE=MODULE                                                 423
C    *                                                                    423
C    Geant ATTRIBUTE : STLI ===> SEEN=0 COLO=1 SERIAL=0 <===              424
         CALL AGSRESET                                                    424
         AG_SEEN = 0                                                      424
         AG_COLO = 1                                                      424
         AG_SERIAL = 0                                                    424
         AG_TITLE = 'STLI'                                                424
         AG_EXNAME = 'ATTRIBUTE'                                          424
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  424
         CALL AXATTRIBUTE                                                 424
C    *                                                                    424
C    Geant SHAPE : BOX ===> DY=SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGA  424
C    P), DZ=SWCA_WAFERTHK/2 <===                                          424
         CALL AGSRESET                                                    426
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              426
         AG_DZ = SWCA_WAFERTHK/2                                          426
         AG_TITLE = 'BOX'                                                 426
         AG_EXNAME = 'SHAPE'                                              426
         AG_PARLIST = 'DY_DZ'                                             426
         CALL AXSHAPE                                                     426
C    *                                                                    426
C    Create STSI :                                                        427
C    Call To Local Procedure STSI                                         427
         ASSIGN 5570 TO BLOCK_STSI                                        427
         GO TO  5560                                                      427
5570     CONTINUE                                                         427
C    *                                                                    427
5461  CALL AGSPOPB                                                        428
      GO TO BLOCK_STLI, (5460 ,5470)                                      428
C End of Local Procedure                                                  428
C ----------------------------------------------------------------------  429
C ------------ STSI                                                       429
C Begin Local Procedure STSI                                              430
C STSI IS A SINGLE WAVER CONTAINER                                        430
5560  CONTINUE                                                            430
         AG_TITLE='STSI'                                                  430
         AG_MODULE=MODULE                                                 430
         AG_MARK='V00'                                                    430
         CALL AGSPUSH                                                     430
         AG_MODULE=MODULE                                                 430
C    *                                                                    430
C    Geant SHAPE : DIVISION ===> NDIV=SVTL_NWAFER IAXIS=2 <===            431
         CALL AGSRESET                                                    431
         AG_NDIV = SVTL_NWAFER                                            431
         AG_IAXIS = 2                                                     431
         AG_TITLE = 'DIVISION'                                            431
         AG_EXNAME = 'SHAPE'                                              431
         AG_PARLIST = 'NDIV_IAXIS'                                        431
         CALL AXSHAPE                                                     431
C    *                                                                    431
C    Create SVTD :                                                        432
C    Call To Local Procedure SVTD                                         432
         ASSIGN 5590 TO BLOCK_SVTD                                        432
         GO TO  5580                                                      432
5590     CONTINUE                                                         432
C    *                                                                    432
C    Geant POSITION : SVTD ===> inherited arguments used <===             433
         CALL AGSRESET                                                    433
         AG_TITLE = 'SVTD'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'NONE'                                              433
         CALL AXPOSITION                                                  433
C    *                                                                    433
5561  CALL AGSPOPB                                                        434
      GO TO BLOCK_STSI, (5560 ,5570)                                      434
C End of Local Procedure                                                  434
C ----------------------------------------------------------------------  435
C ------------ SVTD                                                       435
C Begin Local Procedure SVTD                                              436
C SVTD IS AN ACTIVE WAFER VOLUME                                          436
5580  CONTINUE                                                            436
         AG_TITLE='SVTD'                                                  436
         AG_MODULE=MODULE                                                 436
         AG_MARK='V00'                                                    436
         CALL AGSPUSH                                                     436
         AG_MODULE=MODULE                                                 436
C    *                                                                    436
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          437
         CALL AGSRESET                                                    437
         AG_TITLE = 'SILICON'                                             437
         AG_EXNAME = 'MATERIAL'                                           437
         AG_PARLIST = 'NONE'                                              437
         CALL AXMATERIAL                                                  437
C    *                                                                    437
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         437
         CALL AGSRESET                                                    438
         AG_ISVOL = 1                                                     438
         AG_TITLE = 'SENSITIVE'                                           438
         AG_EXNAME = 'MATERIAL'                                           438
         AG_PARLIST = 'ISVOL'                                             438
         CALL AXMATERIAL                                                  438
C    *                                                                    438
C    Geant ATTRIBUTE : SVTD ===> SEEN=1 COLO=4 <===                       438
         CALL AGSRESET                                                    439
         AG_SEEN = 1                                                      439
         AG_COLO = 4                                                      439
         AG_TITLE = 'SVTD'                                                439
         AG_EXNAME = 'ATTRIBUTE'                                          439
         AG_PARLIST = 'SEEN_COLO'                                         439
         CALL AXATTRIBUTE                                                 439
C    *                                                                    439
C    Geant SHAPE : BOX ===> DY=SWCA_WAFERLEN/2 <===                       439
         CALL AGSRESET                                                    440
         AG_DY = SWCA_WAFERLEN/2                                          440
         AG_TITLE = 'BOX'                                                 440
         AG_EXNAME = 'SHAPE'                                              440
         AG_PARLIST = 'DY'                                                440
         CALL AXSHAPE                                                     440
C    *                                                                    440
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 440
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:18:(0,100) CX:10: CY:1  443
C    0: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.0E-6) STEP:.01: ELOSS:22:(0  443
C    .,0.01)                                                              443
         AG_TITLE = 'SVTD'                                                443
         AG_EXNAME = 'HITS'                                               443
         AG_TYPE=AG_MAGIC                                                 443
         CALL AGSDETV('SVTD')                                             443
C    *                                                                    443
         AG_PARLIST='Z'                                                   443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = AG_MAGIC                                              443
         AG_BIN=.001                                                      443
         AG_OPTION='S'                                                    443
         AG_FHMIN = AG_MAGIC                                              443
         AG_FHMAX = AG_MAGIC                                              443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='Y'                                                   443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = AG_MAGIC                                              443
         AG_BIN=.001                                                      443
         AG_FHMIN=AG_MAGIC                                                443
         AG_FHMAX=AG_MAGIC                                                443
         AG_OPTION='*'                                                    443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='X'                                                   443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = AG_MAGIC                                              443
         AG_BIN=.001                                                      443
         AG_FHMIN=AG_MAGIC                                                443
         AG_FHMAX=AG_MAGIC                                                443
         AG_OPTION='*'                                                    443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='PTOT'                                                443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = 18                                                    443
         AG_BIN=AG_MAGIC                                                  443
         AG_OPTION='*'                                                    443
         AG_FHMIN = 0                                                     443
         AG_FHMAX = 100                                                   443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='CX'                                                  443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = 10                                                    443
         AG_BIN=AG_MAGIC                                                  443
         AG_FHMIN=AG_MAGIC                                                443
         AG_FHMAX=AG_MAGIC                                                443
         AG_OPTION='*'                                                    443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='CY'                                                  443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = 10                                                    443
         AG_BIN=AG_MAGIC                                                  443
         AG_FHMIN=AG_MAGIC                                                443
         AG_FHMAX=AG_MAGIC                                                443
         AG_OPTION='*'                                                    443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='CZ'                                                  443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = 10                                                    443
         AG_BIN=AG_MAGIC                                                  443
         AG_FHMIN=AG_MAGIC                                                443
         AG_FHMAX=AG_MAGIC                                                443
         AG_OPTION='*'                                                    443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='SLENG'                                               443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = 16                                                    443
         AG_BIN=AG_MAGIC                                                  443
         AG_OPTION='*'                                                    443
         AG_FHMIN = 0                                                     443
         AG_FHMAX = 500                                                   443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='TOF'                                                 443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = 16                                                    443
         AG_BIN=AG_MAGIC                                                  443
         AG_OPTION='*'                                                    443
         AG_FHMIN = 0                                                     443
         AG_FHMAX = 1.0E-6                                                443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='STEP'                                                443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = AG_MAGIC                                              443
         AG_BIN=.01                                                       443
         AG_FHMIN=AG_MAGIC                                                443
         AG_FHMAX=AG_MAGIC                                                443
         AG_OPTION='*'                                                    443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         AG_PARLIST='ELOSS'                                               443
         AG_TITLE = 'DETH'                                                443
         AG_NBITS = 22                                                    443
         AG_BIN=AG_MAGIC                                                  443
         AG_OPTION='*'                                                    443
         AG_FHMIN = 0.                                                    443
         AG_FHMAX = 0.01                                                  443
         CALL AGSDETH (0)                                                 443
C    *                                                                    443
         CALL AGSDETU('SVTD')                                             443
C    *                                                                    443
5581  CALL AGSPOPB                                                        444
      GO TO BLOCK_SVTD, (5580 ,5590)                                      444
C End of Local Procedure                                                  444
C ----------------------------------------------------------------------  445
C ------------ SBER                                                       445
C Begin Local Procedure SBER                                              446
C SBER ARE THE BERILLIUM WAFER CARRIER RAILS                              446
5480  CONTINUE                                                            446
         AG_TITLE='SBER'                                                  446
         AG_MODULE=MODULE                                                 446
         AG_MARK='V00'                                                    446
         CALL AGSPUSH                                                     446
         AG_MODULE=MODULE                                                 446
C    *                                                                    446
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        447
         CALL AGSRESET                                                    447
         AG_TITLE = 'BERILLIUM'                                           447
         AG_EXNAME = 'MATERIAL'                                           447
         AG_PARLIST = 'NONE'                                              447
         CALL AXMATERIAL                                                  447
C    *                                                                    447
C    Geant ATTRIBUTE : SBER ===> SEEN=1 COLO=2 <===                       447
         CALL AGSRESET                                                    448
         AG_SEEN = 1                                                      448
         AG_COLO = 2                                                      448
         AG_TITLE = 'SBER'                                                448
         AG_EXNAME = 'ATTRIBUTE'                                          448
         AG_PARLIST = 'SEEN_COLO'                                         448
         CALL AXATTRIBUTE                                                 448
C    *                                                                    448
C    Geant SHAPE : BOX ===> DX=SWCA_WAFCARWD/2 DY=SWCA_LENGTH/2, DZ=SWCA  448
C    _WAFCARTH/2 <===                                                     448
         CALL AGSRESET                                                    450
         AG_DX = SWCA_WAFCARWD/2                                          450
         AG_DY = SWCA_LENGTH/2                                            450
         AG_DZ = SWCA_WAFCARTH/2                                          450
         AG_TITLE = 'BOX'                                                 450
         AG_EXNAME = 'SHAPE'                                              450
         AG_PARLIST = 'DX_DY_DZ'                                          450
         CALL AXSHAPE                                                     450
C    *                                                                    450
5481  CALL AGSPOPB                                                        451
      GO TO BLOCK_SBER, (5480 ,5490)                                      451
C End of Local Procedure                                                  451
C ----------------------------------------------------------------------  452
C ------------ STAB                                                       452
C Begin Local Procedure STAB                                              453
C STAB ARE THE BERILLIUM WAFER CARRIER END TABS                           453
5500  CONTINUE                                                            453
         AG_TITLE='STAB'                                                  453
         AG_MODULE=MODULE                                                 453
         AG_MARK='V00'                                                    453
         CALL AGSPUSH                                                     453
         AG_MODULE=MODULE                                                 453
C    *                                                                    453
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        454
         CALL AGSRESET                                                    454
         AG_TITLE = 'BERILLIUM'                                           454
         AG_EXNAME = 'MATERIAL'                                           454
         AG_PARLIST = 'NONE'                                              454
         CALL AXMATERIAL                                                  454
C    *                                                                    454
C    Geant ATTRIBUTE : STAB ===> SEEN=1 COLO=2 <===                       454
         CALL AGSRESET                                                    455
         AG_SEEN = 1                                                      455
         AG_COLO = 2                                                      455
         AG_TITLE = 'STAB'                                                455
         AG_EXNAME = 'ATTRIBUTE'                                          455
         AG_PARLIST = 'SEEN_COLO'                                         455
         CALL AXATTRIBUTE                                                 455
C    *                                                                    455
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=TABLEN/  455
C    2 DZ=SWCA_WAFCARTH/2 <===                                            455
         CALL AGSRESET                                                    457
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            457
         AG_DY = TABLEN/2                                                 457
         AG_DZ = SWCA_WAFCARTH/2                                          457
         AG_TITLE = 'BOX'                                                 457
         AG_EXNAME = 'SHAPE'                                              457
         AG_PARLIST = 'DX_DY_DZ'                                          457
         CALL AXSHAPE                                                     457
C    *                                                                    457
5501  CALL AGSPOPB                                                        458
      GO TO BLOCK_STAB, (5500 ,5510)                                      458
C End of Local Procedure                                                  458
C ----------------------------------------------------------------------  459
C ------------ STRU                                                       459
C Begin Local Procedure STRU                                              460
C STRU ARE THE BERILLIUM STRUTS BETWEEN THE WAFER CARRIER RAILS           460
5520  CONTINUE                                                            460
         AG_TITLE='STRU'                                                  460
         AG_MODULE=MODULE                                                 460
         AG_MARK='V00'                                                    460
         CALL AGSPUSH                                                     460
         AG_MODULE=MODULE                                                 460
C    *                                                                    460
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        461
         CALL AGSRESET                                                    461
         AG_TITLE = 'BERILLIUM'                                           461
         AG_EXNAME = 'MATERIAL'                                           461
         AG_PARLIST = 'NONE'                                              461
         CALL AXMATERIAL                                                  461
C    *                                                                    461
C    Geant ATTRIBUTE : STRU ===> SEEN=1 COLO=2 <===                       461
         CALL AGSRESET                                                    462
         AG_SEEN = 1                                                      462
         AG_COLO = 2                                                      462
         AG_TITLE = 'STRU'                                                462
         AG_EXNAME = 'ATTRIBUTE'                                          462
         AG_PARLIST = 'SEEN_COLO'                                         462
         CALL AXATTRIBUTE                                                 462
C    *                                                                    462
C    Geant SHAPE : BOX ===> DX=SWCA_WAFERWID/2-SWCA_WAFCARWD, DY=SWCA_ST  462
C    RUTLEN/2 DZ=SWCA_WAFCARTH/2 <===                                     462
         CALL AGSRESET                                                    464
         AG_DX = SWCA_WAFERWID/2-SWCA_WAFCARWD                            464
         AG_DY = SWCA_STRUTLEN/2                                          464
         AG_DZ = SWCA_WAFCARTH/2                                          464
         AG_TITLE = 'BOX'                                                 464
         AG_EXNAME = 'SHAPE'                                              464
         AG_PARLIST = 'DX_DY_DZ'                                          464
         CALL AXSHAPE                                                     464
C    *                                                                    464
5521  CALL AGSPOPB                                                        465
      GO TO BLOCK_STRU, (5520 ,5530)                                      465
C End of Local Procedure                                                  465
C ----------------------------------------------------------------------  466
C ------------ SELE                                                       466
C Begin Local Procedure SELE                                              467
C SELE IS THE ELECTRONICS MOTHER VOLUME                                   467
5430  CONTINUE                                                            467
         AG_TITLE='SELE'                                                  467
         AG_MODULE=MODULE                                                 467
         AG_MARK='V00'                                                    467
         CALL AGSPUSH                                                     467
         AG_MODULE=MODULE                                                 467
C    *                                                                    467
C    Geant MATERIAL : AIR ===> inherited arguments used <===              468
         CALL AGSRESET                                                    468
         AG_TITLE = 'AIR'                                                 468
         AG_EXNAME = 'MATERIAL'                                           468
         AG_PARLIST = 'NONE'                                              468
         CALL AXMATERIAL                                                  468
C    *                                                                    468
C    Geant ATTRIBUTE : SELE ===> SEEN=0 COLO=1 <===                       468
         CALL AGSRESET                                                    469
         AG_SEEN = 0                                                      469
         AG_COLO = 1                                                      469
         AG_TITLE = 'SELE'                                                469
         AG_EXNAME = 'ATTRIBUTE'                                          469
         AG_PARLIST = 'SEEN_COLO'                                         469
         CALL AXATTRIBUTE                                                 469
C    *                                                                    469
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=ELETH  469
C    K/2 <===                                                             469
         CALL AGSRESET                                                    471
         AG_DX = SELC_ELCAWID/2                                           471
         AG_DY = SWCA_LENGTH/2                                            471
         AG_DZ = ELETHK/2                                                 471
         AG_TITLE = 'BOX'                                                 471
         AG_EXNAME = 'SHAPE'                                              471
         AG_PARLIST = 'DX_DY_DZ'                                          471
         CALL AXSHAPE                                                     471
C    *                                                                    471
C    Create SWCH :                                                        472
C    Call To Local Procedure SWCH                                         472
         ASSIGN 5610 TO BLOCK_SWCH                                        472
         GO TO  5600                                                      472
5610     CONTINUE                                                         472
C    *                                                                    472
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK/2 <===             473
         CALL AGSRESET                                                    473
         AG_Z = -ELETHK/2+SELC_BETHK/2                                    473
         AG_TITLE = 'SWCH'                                                473
         AG_EXNAME = 'POSITION'                                           473
         AG_PARLIST = 'Z'                                                 473
         CALL AXPOSITION                                                  473
C    *                                                                    473
C    Geant POSITION : SWCH ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_  473
C    BETHK/2 <===                                                         473
         CALL AGSRESET                                                    474
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK+SELC_BETHK/2             474
         AG_TITLE = 'SWCH'                                                474
         AG_EXNAME = 'POSITION'                                           474
         AG_PARLIST = 'Z'                                                 474
         CALL AXPOSITION                                                  474
C    *                                                                    474
C    Create SWCS :                                                        475
C    Call To Local Procedure SWCS                                         475
         ASSIGN 5630 TO BLOCK_SWCS                                        475
         GO TO  5620                                                      475
5630     CONTINUE                                                         475
C    *                                                                    475
C    Geant POSITION : SWCS ===> X=-SELC_ELCAWID/2+SELC_BETHK/2 Z=-ELETHK  477
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     477
         CALL AGSRESET                                                    477
         AG_X = -SELC_ELCAWID/2+SELC_BETHK/2                              477
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        477
         AG_TITLE = 'SWCS'                                                477
         AG_EXNAME = 'POSITION'                                           477
         AG_PARLIST = 'X_Z'                                               477
         CALL AXPOSITION                                                  477
C    *                                                                    477
C    Geant POSITION : SWCS ===> X=+SELC_ELCAWID/2-SELC_BETHK/2 Z=-ELETHK  477
C    /2+SELC_BETHK+SELC_WATTHK/2 <===                                     477
         CALL AGSRESET                                                    479
         AG_X = +SELC_ELCAWID/2-SELC_BETHK/2                              479
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        479
         AG_TITLE = 'SWCS'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Z'                                               479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    Create SWCW :                                                        480
C    Call To Local Procedure SWCW                                         480
         ASSIGN 5650 TO BLOCK_SWCW                                        480
         GO TO  5640                                                      480
5650     CONTINUE                                                         480
C    *                                                                    480
C    Geant POSITION : SWCW ===> Z=-ELETHK/2+SELC_BETHK+SELC_WATTHK/2 <==  481
C    =                                                                    481
         CALL AGSRESET                                                    481
         AG_Z = -ELETHK/2+SELC_BETHK+SELC_WATTHK/2                        481
         AG_TITLE = 'SWCW'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'Z'                                                 481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    Create SBOI :                                                        482
C    Call To Local Procedure SBOI                                         482
         ASSIGN 5670 TO BLOCK_SBOI                                        482
         GO TO  5660                                                      482
5670     CONTINUE                                                         482
C    *                                                                    482
C    Geant POSITION : SBOI ===> Z=(ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SEL  483
C    C_GLASSTHK-SELC_BEOTHK/2) <===                                       483
         CALL AGSRESET                                                    483
         AG_Z = (ELETHK/                                                  483
     *   2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK-SELC_BEOTHK/2)          483
         AG_TITLE = 'SBOI'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'Z'                                                 483
         CALL AXPOSITION                                                  483
C    *                                                                    483
C    Create SGLA :                                                        484
C    Call To Local Procedure SGLA                                         484
         ASSIGN 5690 TO BLOCK_SGLA                                        484
         GO TO  5680                                                      484
5690     CONTINUE                                                         484
C    *                                                                    484
C    Geant POSITION : SGLA ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC  485
C    _GLASSTHK/2 <===                                                     485
         CALL AGSRESET                                                    485
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK-SELC_GLASSTHK/2         485
         AG_TITLE = 'SGLA'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'Z'                                                 485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    Create SAGP :                                                        486
C    Call To Local Procedure SAGP                                         486
         ASSIGN 5710 TO BLOCK_SAGP                                        486
         GO TO  5700                                                      486
5710     CONTINUE                                                         486
C    *                                                                    486
C    Geant POSITION : SAGP ===> Z=ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2 <=  487
C    ==                                                                   487
         CALL AGSRESET                                                    487
         AG_Z = ELETHK/2-SELC_DYETHK-SELC_AGPDTHK/2                       487
         AG_TITLE = 'SAGP'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'Z'                                                 487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Create SDYE :                                                        488
C    Call To Local Procedure SDYE                                         488
         ASSIGN 5730 TO BLOCK_SDYE                                        488
         GO TO  5720                                                      488
5730     CONTINUE                                                         488
C    *                                                                    488
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID  490
C    /2 Z=ELETHK/2-SELC_DYETHK/2 <===                                     490
         CALL AGSRESET                                                    490
         AG_X = SELC_ELCAWID/2-SELC_DYESPC-SELC_DYEWID/2                  490
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    490
         AG_TITLE = 'SDYE'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Z'                                               490
         CALL AXPOSITION                                                  490
C    *                                                                    490
C    Geant POSITION : SDYE ===> X=SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SEL  490
C    C_DYEWID/2 Z=ELETHK/2-SELC_DYETHK/2 <===                             490
         CALL AGSRESET                                                    492
         AG_X = SELC_ELCAWID/2-2.0*SELC_DYESPC-3.0*SELC_DYEWID/2          492
         AG_Z = ELETHK/2-SELC_DYETHK/2                                    492
         AG_TITLE = 'SDYE'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Z'                                               492
         CALL AXPOSITION                                                  492
C    *                                                                    492
C    Create SECA :                                                        493
C    Call To Local Procedure SECA                                         493
         ASSIGN 5750 TO BLOCK_SECA                                        493
         GO TO  5740                                                      493
5750     CONTINUE                                                         493
C    *                                                                    493
C    Geant POSITION : SECA ===> X=-SELC_ELCAWID/2+SELC_CABWID/2 Z=ELETHK  495
C    /2-SELC_DYETHK+SELC_CABTHK/2 <===                                    495
         CALL AGSRESET                                                    495
         AG_X = -SELC_ELCAWID/2+SELC_CABWID/2                             495
         AG_Z = ELETHK/2-SELC_DYETHK+SELC_CABTHK/2                        495
         AG_TITLE = 'SECA'                                                495
         AG_EXNAME = 'POSITION'                                           495
         AG_PARLIST = 'X_Z'                                               495
         CALL AXPOSITION                                                  495
C    *                                                                    495
5431  CALL AGSPOPB                                                        496
      GO TO BLOCK_SELE, (5430 ,5440)                                      496
C End of Local Procedure                                                  496
C ----------------------------------------------------------------------  497
C ------------ SDYE                                                       497
C Begin Local Procedure SDYE                                              498
C SDYE IS THE IC CHIP ON THE HYBRID                                       498
5720  CONTINUE                                                            498
         AG_TITLE='SDYE'                                                  498
         AG_MODULE=MODULE                                                 498
         AG_MARK='V00'                                                    498
         CALL AGSPUSH                                                     498
         AG_MODULE=MODULE                                                 498
C    *                                                                    498
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          499
         CALL AGSRESET                                                    499
         AG_TITLE = 'SILICON'                                             499
         AG_EXNAME = 'MATERIAL'                                           499
         AG_PARLIST = 'NONE'                                              499
         CALL AXMATERIAL                                                  499
C    *                                                                    499
C    Geant ATTRIBUTE : SDYE ===> SEEN=1 COLO=6 <===                       499
         CALL AGSRESET                                                    500
         AG_SEEN = 1                                                      500
         AG_COLO = 6                                                      500
         AG_TITLE = 'SDYE'                                                500
         AG_EXNAME = 'ATTRIBUTE'                                          500
         AG_PARLIST = 'SEEN_COLO'                                         500
         CALL AXATTRIBUTE                                                 500
C    *                                                                    500
C    Geant SHAPE : BOX ===> DX=SELC_DYEWID/2 DY=SVTL_NWAFER*(SWCA_WAFERL  500
C    EN/2+SWCA_WAFERGAP), DZ=SELC_DYETHK/2 <===                           500
         CALL AGSRESET                                                    502
         AG_DX = SELC_DYEWID/2                                            502
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              502
         AG_DZ = SELC_DYETHK/2                                            502
         AG_TITLE = 'BOX'                                                 502
         AG_EXNAME = 'SHAPE'                                              502
         AG_PARLIST = 'DX_DY_DZ'                                          502
         CALL AXSHAPE                                                     502
C    *                                                                    502
5721  CALL AGSPOPB                                                        503
      GO TO BLOCK_SDYE, (5720 ,5730)                                      503
C End of Local Procedure                                                  503
C ----------------------------------------------------------------------  504
C ------------ SECA                                                       504
C Begin Local Procedure SECA                                              505
C SECA IS THE CABLE ON THE ELECTRONICS CARRIER                            505
5740  CONTINUE                                                            505
         AG_TITLE='SECA'                                                  505
         AG_MODULE=MODULE                                                 505
         AG_MARK='V00'                                                    505
         CALL AGSPUSH                                                     505
         AG_MODULE=MODULE                                                 505
C    *                                                                    505
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           506
         CALL AGSRESET                                                    506
         AG_TITLE = 'COPPER'                                              506
         AG_EXNAME = 'MATERIAL'                                           506
         AG_PARLIST = 'NONE'                                              506
         CALL AXMATERIAL                                                  506
C    *                                                                    506
C    Geant ATTRIBUTE : SECA ===> SEEN=1 COLO=2 <===                       506
         CALL AGSRESET                                                    507
         AG_SEEN = 1                                                      507
         AG_COLO = 2                                                      507
         AG_TITLE = 'SECA'                                                507
         AG_EXNAME = 'ATTRIBUTE'                                          507
         AG_PARLIST = 'SEEN_COLO'                                         507
         CALL AXATTRIBUTE                                                 507
C    *                                                                    507
C    Geant SHAPE : BOX ===> DX=SELC_CABWID/2 DY=SWCA_LENGTH/2, DZ=SELC_C  507
C    ABTHK/2 <===                                                         507
         CALL AGSRESET                                                    509
         AG_DX = SELC_CABWID/2                                            509
         AG_DY = SWCA_LENGTH/2                                            509
         AG_DZ = SELC_CABTHK/2                                            509
         AG_TITLE = 'BOX'                                                 509
         AG_EXNAME = 'SHAPE'                                              509
         AG_PARLIST = 'DX_DY_DZ'                                          509
         CALL AXSHAPE                                                     509
C    *                                                                    509
5741  CALL AGSPOPB                                                        510
      GO TO BLOCK_SECA, (5740 ,5750)                                      510
C End of Local Procedure                                                  510
C ----------------------------------------------------------------------  511
C ------------ SBOI                                                       511
C Begin Local Procedure SBOI                                              512
C SBOI IS THE BERILLIA LAYER                                              512
5660  CONTINUE                                                            512
         AG_TITLE='SBOI'                                                  512
         AG_MODULE=MODULE                                                 512
         AG_MARK='V00'                                                    512
         CALL AGSPUSH                                                     512
         AG_MODULE=MODULE                                                 512
C    *                                                                    512
C    Geant COMPONENT : BE ===> A=9 Z=4 W=1 <===                           513
         CALL AGSRESET                                                    513
         AG_A = 9                                                         513
         AG_Z = 4                                                         513
         AG_W = 1                                                         513
         AG_TITLE = 'BE'                                                  513
         AG_EXNAME = 'COMPONENT'                                          513
         AG_PARLIST = 'A_Z_W'                                             513
         CALL AXCOMPONENT                                                 513
C    *                                                                    513
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           513
         CALL AGSRESET                                                    514
         AG_A = 16                                                        514
         AG_Z = 8                                                         514
         AG_W = 1                                                         514
         AG_TITLE = 'O'                                                   514
         AG_EXNAME = 'COMPONENT'                                          514
         AG_PARLIST = 'A_Z_W'                                             514
         CALL AXCOMPONENT                                                 514
C    *                                                                    514
C    Geant MIXTURE : BEO ===> DENS=2.85 <===                              514
         CALL AGSRESET                                                    515
         AG_DENS = 2.85                                                   515
         AG_TITLE = 'BEO'                                                 515
         AG_EXNAME = 'MIXTURE'                                            515
         AG_PARLIST = 'DENS'                                              515
         CALL AXMIXTURE                                                   515
C    *                                                                    515
C    Geant ATTRIBUTE : SBOI ===> SEEN=1 COLO=6 <===                       515
         CALL AGSRESET                                                    516
         AG_SEEN = 1                                                      516
         AG_COLO = 6                                                      516
         AG_TITLE = 'SBOI'                                                516
         AG_EXNAME = 'ATTRIBUTE'                                          516
         AG_PARLIST = 'SEEN_COLO'                                         516
         CALL AXATTRIBUTE                                                 516
C    *                                                                    516
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  516
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_BEOTHK/2 <===                          516
         CALL AGSRESET                                                    518
         AG_DX = SELC_ELCAWID/2                                           518
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              518
         AG_DZ = SELC_BEOTHK/2                                            518
         AG_TITLE = 'BOX'                                                 518
         AG_EXNAME = 'SHAPE'                                              518
         AG_PARLIST = 'DX_DY_DZ'                                          518
         CALL AXSHAPE                                                     518
C    *                                                                    518
5661  CALL AGSPOPB                                                        519
      GO TO BLOCK_SBOI, (5660 ,5670)                                      519
C End of Local Procedure                                                  519
C ----------------------------------------------------------------------  520
C ------------ SAGP                                                       520
C Begin Local Procedure SAGP                                              521
C SAGP IS THE SILVER-PALLADIUM LAYER                                      521
5700  CONTINUE                                                            521
         AG_TITLE='SAGP'                                                  521
         AG_MODULE=MODULE                                                 521
         AG_MARK='V00'                                                    521
         CALL AGSPUSH                                                     521
         AG_MODULE=MODULE                                                 521
C    *                                                                    521
C    Geant COMPONENT : AG ===> A=108 Z=47 W=1 <===                        522
         CALL AGSRESET                                                    522
         AG_A = 108                                                       522
         AG_Z = 47                                                        522
         AG_W = 1                                                         522
         AG_TITLE = 'AG'                                                  522
         AG_EXNAME = 'COMPONENT'                                          522
         AG_PARLIST = 'A_Z_W'                                             522
         CALL AXCOMPONENT                                                 522
C    *                                                                    522
C    Geant COMPONENT : PD ===> A=106 Z=46 W=1 <===                        522
         CALL AGSRESET                                                    523
         AG_A = 106                                                       523
         AG_Z = 46                                                        523
         AG_W = 1                                                         523
         AG_TITLE = 'PD'                                                  523
         AG_EXNAME = 'COMPONENT'                                          523
         AG_PARLIST = 'A_Z_W'                                             523
         CALL AXCOMPONENT                                                 523
C    *                                                                    523
C    Geant MIXTURE : AGPD ===> DENS=11.25 <===                            523
         CALL AGSRESET                                                    524
         AG_DENS = 11.25                                                  524
         AG_TITLE = 'AGPD'                                                524
         AG_EXNAME = 'MIXTURE'                                            524
         AG_PARLIST = 'DENS'                                              524
         CALL AXMIXTURE                                                   524
C    *                                                                    524
C    Geant ATTRIBUTE : SAGP ===> SEEN=1 COLO=2 <===                       524
         CALL AGSRESET                                                    525
         AG_SEEN = 1                                                      525
         AG_COLO = 2                                                      525
         AG_TITLE = 'SAGP'                                                525
         AG_EXNAME = 'ATTRIBUTE'                                          525
         AG_PARLIST = 'SEEN_COLO'                                         525
         CALL AXATTRIBUTE                                                 525
C    *                                                                    525
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  525
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_AGPDTHK/2 <===                         525
         CALL AGSRESET                                                    527
         AG_DX = SELC_ELCAWID/2                                           527
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              527
         AG_DZ = SELC_AGPDTHK/2                                           527
         AG_TITLE = 'BOX'                                                 527
         AG_EXNAME = 'SHAPE'                                              527
         AG_PARLIST = 'DX_DY_DZ'                                          527
         CALL AXSHAPE                                                     527
C    *                                                                    527
5701  CALL AGSPOPB                                                        528
      GO TO BLOCK_SAGP, (5700 ,5710)                                      528
C End of Local Procedure                                                  528
C ----------------------------------------------------------------------  529
C ------------ SGLA                                                       529
C Begin Local Procedure SGLA                                              530
C SGLA IS THE INSULATING GLASS LAYER                                      530
5680  CONTINUE                                                            530
         AG_TITLE='SGLA'                                                  530
         AG_MODULE=MODULE                                                 530
         AG_MARK='V00'                                                    530
         CALL AGSPUSH                                                     530
         AG_MODULE=MODULE                                                 530
C    *                                                                    530
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1 <===                         531
         CALL AGSRESET                                                    531
         AG_A = 28                                                        531
         AG_Z = 14                                                        531
         AG_W = 1                                                         531
         AG_TITLE = 'SI'                                                  531
         AG_EXNAME = 'COMPONENT'                                          531
         AG_PARLIST = 'A_Z_W'                                             531
         CALL AXCOMPONENT                                                 531
C    *                                                                    531
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           531
         CALL AGSRESET                                                    532
         AG_A = 16                                                        532
         AG_Z = 8                                                         532
         AG_W = 2                                                         532
         AG_TITLE = 'O'                                                   532
         AG_EXNAME = 'COMPONENT'                                          532
         AG_PARLIST = 'A_Z_W'                                             532
         CALL AXCOMPONENT                                                 532
C    *                                                                    532
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             532
         CALL AGSRESET                                                    533
         AG_DENS = 2.2                                                    533
         AG_TITLE = 'GLASS'                                               533
         AG_EXNAME = 'MIXTURE'                                            533
         AG_PARLIST = 'DENS'                                              533
         CALL AXMIXTURE                                                   533
C    *                                                                    533
C    Geant ATTRIBUTE : SGLA ===> SEEN=1 COLO=6 <===                       533
         CALL AGSRESET                                                    534
         AG_SEEN = 1                                                      534
         AG_COLO = 6                                                      534
         AG_TITLE = 'SGLA'                                                534
         AG_EXNAME = 'ATTRIBUTE'                                          534
         AG_PARLIST = 'SEEN_COLO'                                         534
         CALL AXATTRIBUTE                                                 534
C    *                                                                    534
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SVTL_NWAFER*(SWCA_WAFER  534
C    LEN/2+SWCA_WAFERGAP), DZ=SELC_GLASSTHK/2 <===                        534
         CALL AGSRESET                                                    536
         AG_DX = SELC_ELCAWID/2                                           536
         AG_DY = SVTL_NWAFER*(SWCA_WAFERLEN/2+SWCA_WAFERGAP)              536
         AG_DZ = SELC_GLASSTHK/2                                          536
         AG_TITLE = 'BOX'                                                 536
         AG_EXNAME = 'SHAPE'                                              536
         AG_PARLIST = 'DX_DY_DZ'                                          536
         CALL AXSHAPE                                                     536
C    *                                                                    536
5681  CALL AGSPOPB                                                        537
      GO TO BLOCK_SGLA, (5680 ,5690)                                      537
C End of Local Procedure                                                  537
C ----------------------------------------------------------------------  538
C ------------ SWCH                                                       538
C Begin Local Procedure SWCH                                              539
C SWCH IS THE BE TOP AND BOTTOM OF THE WATER CHANNEL                      539
5600  CONTINUE                                                            539
         AG_TITLE='SWCH'                                                  539
         AG_MODULE=MODULE                                                 539
         AG_MARK='V00'                                                    539
         CALL AGSPUSH                                                     539
         AG_MODULE=MODULE                                                 539
C    *                                                                    539
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        540
         CALL AGSRESET                                                    540
         AG_TITLE = 'BERILLIUM'                                           540
         AG_EXNAME = 'MATERIAL'                                           540
         AG_PARLIST = 'NONE'                                              540
         CALL AXMATERIAL                                                  540
C    *                                                                    540
C    Geant ATTRIBUTE : SWCH ===> SEEN=1 COLO=2 <===                       540
         CALL AGSRESET                                                    541
         AG_SEEN = 1                                                      541
         AG_COLO = 2                                                      541
         AG_TITLE = 'SWCH'                                                541
         AG_EXNAME = 'ATTRIBUTE'                                          541
         AG_PARLIST = 'SEEN_COLO'                                         541
         CALL AXATTRIBUTE                                                 541
C    *                                                                    541
C    Geant SHAPE : BOX ===> DX=SELC_ELCAWID/2 DY=SWCA_LENGTH/2, DZ=SELC_  541
C    BETHK/2 <===                                                         541
         CALL AGSRESET                                                    543
         AG_DX = SELC_ELCAWID/2                                           543
         AG_DY = SWCA_LENGTH/2                                            543
         AG_DZ = SELC_BETHK/2                                             543
         AG_TITLE = 'BOX'                                                 543
         AG_EXNAME = 'SHAPE'                                              543
         AG_PARLIST = 'DX_DY_DZ'                                          543
         CALL AXSHAPE                                                     543
C    *                                                                    543
5601  CALL AGSPOPB                                                        544
      GO TO BLOCK_SWCH, (5600 ,5610)                                      544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ SWCS                                                       545
C Begin Local Procedure SWCS                                              546
C SWCS IS THE BE SIDE OF THE WATER CHANNEL                                546
5620  CONTINUE                                                            546
         AG_TITLE='SWCS'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        547
         CALL AGSRESET                                                    547
         AG_TITLE = 'BERILLIUM'                                           547
         AG_EXNAME = 'MATERIAL'                                           547
         AG_PARLIST = 'NONE'                                              547
         CALL AXMATERIAL                                                  547
C    *                                                                    547
C    Geant ATTRIBUTE : SWCS ===> SEEN=1 COLO=2 <===                       547
         CALL AGSRESET                                                    548
         AG_SEEN = 1                                                      548
         AG_COLO = 2                                                      548
         AG_TITLE = 'SWCS'                                                548
         AG_EXNAME = 'ATTRIBUTE'                                          548
         AG_PARLIST = 'SEEN_COLO'                                         548
         CALL AXATTRIBUTE                                                 548
C    *                                                                    548
C    Geant SHAPE : BOX ===> DX=SELC_BETHK/2 DY=SWCA_LENGTH/2, DZ=SELC_WA  548
C    TTHK/2 <===                                                          548
         CALL AGSRESET                                                    550
         AG_DX = SELC_BETHK/2                                             550
         AG_DY = SWCA_LENGTH/2                                            550
         AG_DZ = SELC_WATTHK/2                                            550
         AG_TITLE = 'BOX'                                                 550
         AG_EXNAME = 'SHAPE'                                              550
         AG_PARLIST = 'DX_DY_DZ'                                          550
         CALL AXSHAPE                                                     550
C    *                                                                    550
5621  CALL AGSPOPB                                                        551
      GO TO BLOCK_SWCS, (5620 ,5630)                                      551
C End of Local Procedure                                                  551
C ----------------------------------------------------------------------  552
C ------------ SWCW                                                       552
C Begin Local Procedure SWCW                                              553
C SWCW IS THE WATER CHANNEL WATER (PROBABLY EVIAN?)                       553
5640  CONTINUE                                                            553
         AG_TITLE='SWCW'                                                  553
         AG_MODULE=MODULE                                                 553
         AG_MARK='V00'                                                    553
         CALL AGSPUSH                                                     553
         AG_MODULE=MODULE                                                 553
C    *                                                                    553
C    Geant MATERIAL : WATER ===> inherited arguments used <===            554
         CALL AGSRESET                                                    554
         AG_TITLE = 'WATER'                                               554
         AG_EXNAME = 'MATERIAL'                                           554
         AG_PARLIST = 'NONE'                                              554
         CALL AXMATERIAL                                                  554
C    *                                                                    554
C    Geant ATTRIBUTE : SWCW ===> SEEN=1 COLO=6 <===                       554
         CALL AGSRESET                                                    555
         AG_SEEN = 1                                                      555
         AG_COLO = 6                                                      555
         AG_TITLE = 'SWCW'                                                555
         AG_EXNAME = 'ATTRIBUTE'                                          555
         AG_PARLIST = 'SEEN_COLO'                                         555
         CALL AXATTRIBUTE                                                 555
C    *                                                                    555
C    Geant SHAPE : BOX ===> DX=(SELC_ELCAWID-2.0*SELC_BETHK)/2, DY=SWCA_  555
C    LENGTH/2 DZ=(SELC_WATTHK/2) <===                                     555
         CALL AGSRESET                                                    557
         AG_DX = (SELC_ELCAWID-2.0*SELC_BETHK)/2                          557
         AG_DY = SWCA_LENGTH/2                                            557
         AG_DZ = (SELC_WATTHK/2)                                          557
         AG_TITLE = 'BOX'                                                 557
         AG_EXNAME = 'SHAPE'                                              557
         AG_PARLIST = 'DX_DY_DZ'                                          557
         CALL AXSHAPE                                                     557
C    *                                                                    557
5641  CALL AGSPOPB                                                        558
      GO TO BLOCK_SWCW, (5640 ,5650)                                      558
C End of Local Procedure                                                  558
C ----------------------------------------------------------------------  559
C ------------ SIRT                                                       559
C Begin Local Procedure SIRT                                              560
C SIRT IS THE SVT INNER END RING TUBE                                     560
5050  CONTINUE                                                            560
         AG_TITLE='SIRT'                                                  560
         AG_MODULE=MODULE                                                 560
         AG_MARK='V00'                                                    560
         CALL AGSPUSH                                                     560
         AG_MODULE=MODULE                                                 560
C    *                                                                    560
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        561
         CALL AGSRESET                                                    561
         AG_TITLE = 'BERILLIUM'                                           561
         AG_EXNAME = 'MATERIAL'                                           561
         AG_PARLIST = 'NONE'                                              561
         CALL AXMATERIAL                                                  561
C    *                                                                    561
C    Geant ATTRIBUTE : SIRT ===> SEEN=1 COLO=2 <===                       561
         CALL AGSRESET                                                    562
         AG_SEEN = 1                                                      562
         AG_COLO = 2                                                      562
         AG_TITLE = 'SIRT'                                                562
         AG_EXNAME = 'ATTRIBUTE'                                          562
         AG_PARLIST = 'SEEN_COLO'                                         562
         CALL AXATTRIBUTE                                                 562
C    *                                                                    562
         IR_RMIN=SERG_IRNGPRMN*(COS(PI/8.) + SQRT(TAN(PI/8.)**2. -        562
     *   SIN(PI/8.)**2.))                                                 562
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=SERG_EN  565
C    DRNGTH/2 <===                                                        565
         CALL AGSRESET                                                    565
         AG_RMIN = IR_RMIN                                                565
         AG_RMAX = SERG_IRNGTRMX                                          565
         AG_DZ = SERG_ENDRNGTH/2                                          565
         AG_TITLE = 'TUBE'                                                565
         AG_EXNAME = 'SHAPE'                                              565
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      565
         CALL AXSHAPE                                                     565
C    *                                                                    565
5051  CALL AGSPOPB                                                        566
      GO TO BLOCK_SIRT, (5050 ,5060)                                      566
C End of Local Procedure                                                  566
C ----------------------------------------------------------------------  567
C ------------ SIRP                                                       567
C Begin Local Procedure SIRP                                              568
C SIRP IS THE SVT INNER END RING POLYCONE (OVERLAPS TUBE)                 568
5030  CONTINUE                                                            568
         AG_TITLE='SIRP'                                                  568
         AG_MODULE=MODULE                                                 568
         AG_MARK='V00'                                                    568
         CALL AGSPUSH                                                     568
         AG_MODULE=MODULE                                                 568
C    *                                                                    568
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        569
         CALL AGSRESET                                                    569
         AG_TITLE = 'BERILLIUM'                                           569
         AG_EXNAME = 'MATERIAL'                                           569
         AG_PARLIST = 'NONE'                                              569
         CALL AXMATERIAL                                                  569
C    *                                                                    569
C    Geant ATTRIBUTE : SIRP ===> SEEN=1 COLO=2 <===                       569
         CALL AGSRESET                                                    570
         AG_SEEN = 1                                                      570
         AG_COLO = 2                                                      570
         AG_TITLE = 'SIRP'                                                570
         AG_EXNAME = 'ATTRIBUTE'                                          570
         AG_PARLIST = 'SEEN_COLO'                                         570
         CALL AXATTRIBUTE                                                 570
C    *                                                                    570
         ROU=SERG_IRNGTRMX/(COS(PI/8.) + SQRT(TAN(PI/8.)**2. - SIN(PI/    570
     *   8.)**2.))                                                        570
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=8 NZ=2, ZI={-SERG_END  574
C    RNGTH/2,+SERG_ENDRNGTH/2} RMN={SERG_IRNGPRMN, SERG_IRNGPRMN} RMX={R  574
C    OU,ROU} <===                                                         574
         CALL AGSRESET                                                    574
         AG_PHI1 = 0                                                      574
         AG_DPHI = 360                                                    574
         AG_NPDIV = 8                                                     574
         AG_NZ = 2                                                        574
         AG_ZI(1)=-SERG_ENDRNGTH/2                                        574
         AG_ZI(2)=+SERG_ENDRNGTH/2                                        574
         AG_RMN(1)=SERG_IRNGPRMN                                          574
         AG_RMN(2)=SERG_IRNGPRMN                                          574
         AG_RMX(1)=ROU                                                    574
         AG_RMX(2)=ROU                                                    574
         AG_TITLE = 'PGON'                                                574
         AG_EXNAME = 'SHAPE'                                              574
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     574
         CALL AXSHAPE                                                     574
C    *                                                                    574
5031  CALL AGSPOPB                                                        575
      GO TO BLOCK_SIRP, (5030 ,5040)                                      575
C End of Local Procedure                                                  575
C ----------------------------------------------------------------------  576
C ------------ SOER                                                       576
C Begin Local Procedure SOER                                              577
C SOER IS THE SVT OUTER END RING                                          577
5070  CONTINUE                                                            577
         AG_TITLE='SOER'                                                  577
         AG_MODULE=MODULE                                                 577
         AG_MARK='V00'                                                    577
         CALL AGSPUSH                                                     577
         AG_MODULE=MODULE                                                 577
C    *                                                                    577
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        578
         CALL AGSRESET                                                    578
         AG_TITLE = 'BERILLIUM'                                           578
         AG_EXNAME = 'MATERIAL'                                           578
         AG_PARLIST = 'NONE'                                              578
         CALL AXMATERIAL                                                  578
C    *                                                                    578
C    Geant ATTRIBUTE : SOER ===> SEEN=1 COLO=2 <===                       578
         CALL AGSRESET                                                    579
         AG_SEEN = 1                                                      579
         AG_COLO = 2                                                      579
         AG_TITLE = 'SOER'                                                579
         AG_EXNAME = 'ATTRIBUTE'                                          579
         AG_PARLIST = 'SEEN_COLO'                                         579
         CALL AXATTRIBUTE                                                 579
C    *                                                                    579
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=S  579
C    ERG_ENDRNGTH/2 <===                                                  579
         CALL AGSRESET                                                    581
         AG_RMIN = SERG_ORNGRMIN                                          581
         AG_RMAX = SERG_ORNGRMAX                                          581
         AG_DZ = SERG_ENDRNGTH/2                                          581
         AG_TITLE = 'TUBE'                                                581
         AG_EXNAME = 'SHAPE'                                              581
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      581
         CALL AXSHAPE                                                     581
C    *                                                                    581
5071  CALL AGSPOPB                                                        582
      GO TO BLOCK_SOER, (5070 ,5080)                                      582
C End of Local Procedure                                                  582
C ----------------------------------------------------------------------  583
C ------------ SIES                                                       583
C Begin Local Procedure SIES                                              584
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         584
5130  CONTINUE                                                            584
         AG_TITLE='SIES'                                                  584
         AG_MODULE=MODULE                                                 584
         AG_MARK='V00'                                                    584
         CALL AGSPUSH                                                     584
         AG_MODULE=MODULE                                                 584
C    *                                                                    584
C    Geant MATERIAL : AIR ===> inherited arguments used <===              585
         CALL AGSRESET                                                    585
         AG_TITLE = 'AIR'                                                 585
         AG_EXNAME = 'MATERIAL'                                           585
         AG_PARLIST = 'NONE'                                              585
         CALL AXMATERIAL                                                  585
C    *                                                                    585
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       585
         CALL AGSRESET                                                    586
         AG_SEEN = 0                                                      586
         AG_COLO = 1                                                      586
         AG_TITLE = 'SIES'                                                586
         AG_EXNAME = 'ATTRIBUTE'                                          586
         AG_PARLIST = 'SEEN_COLO'                                         586
         CALL AXATTRIBUTE                                                 586
C    *                                                                    586
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  586
C    ACK_Z-ENDRNG_Z) <===                                                 586
         CALL AGSRESET                                                    588
         AG_RMIN = IR_RMIN                                                588
         AG_RMAX = SERG_IRNGTRMX                                          588
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   588
         AG_TITLE = 'TUBE'                                                588
         AG_EXNAME = 'SHAPE'                                              588
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      588
         CALL AXSHAPE                                                     588
C    *                                                                    588
C    Create SISM :                                                        589
C    Call To Local Procedure SISM                                         589
         ASSIGN 5770 TO BLOCK_SISM                                        589
         GO TO  5760                                                      589
5770     CONTINUE                                                         589
C    *                                                                    589
5131  CALL AGSPOPB                                                        590
      GO TO BLOCK_SIES, (5130 ,5140)                                      590
C End of Local Procedure                                                  590
C ----------------------------------------------------------------------  591
C ------------ SISM                                                       591
C Begin Local Procedure SISM                                              592
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        592
5760  CONTINUE                                                            592
         AG_TITLE='SISM'                                                  592
         AG_MODULE=MODULE                                                 592
         AG_MARK='V00'                                                    592
         CALL AGSPUSH                                                     592
         AG_MODULE=MODULE                                                 592
C    *                                                                    592
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       593
         CALL AGSRESET                                                    593
         AG_SEEN = 0                                                      593
         AG_COLO = 1                                                      593
         AG_TITLE = 'SISM'                                                593
         AG_EXNAME = 'ATTRIBUTE'                                          593
         AG_PARLIST = 'SEEN_COLO'                                         593
         CALL AXATTRIBUTE                                                 593
C    *                                                                    593
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                593
         CALL AGSRESET                                                    594
         AG_NDIV = 4                                                      594
         AG_IAXIS = 2                                                     594
         AG_C0 = 45                                                       594
         AG_TITLE = 'DIVISION'                                            594
         AG_EXNAME = 'SHAPE'                                              594
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     594
         CALL AXSHAPE                                                     594
C    *                                                                    594
C    Create SCRW :                                                        595
C    Call To Local Procedure SCRW                                         595
         ASSIGN 5790 TO BLOCK_SCRW                                        595
         GO TO  5780                                                      595
5790     CONTINUE                                                         595
C    *                                                                    595
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  596
C    ===                                                                  596
         CALL AGSRESET                                                    596
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      596
         AG_TITLE = 'SCRW'                                                596
         AG_EXNAME = 'POSITION'                                           596
         AG_PARLIST = 'X'                                                 596
         CALL AXPOSITION                                                  596
C    *                                                                    596
5761  CALL AGSPOPB                                                        597
      GO TO BLOCK_SISM, (5760 ,5770)                                      597
C End of Local Procedure                                                  597
C ----------------------------------------------------------------------  598
C ------------ SOES                                                       598
C Begin Local Procedure SOES                                              599
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         599
5110  CONTINUE                                                            599
         AG_TITLE='SOES'                                                  599
         AG_MODULE=MODULE                                                 599
         AG_MARK='V00'                                                    599
         CALL AGSPUSH                                                     599
         AG_MODULE=MODULE                                                 599
C    *                                                                    599
C    Geant MATERIAL : AIR ===> inherited arguments used <===              600
         CALL AGSRESET                                                    600
         AG_TITLE = 'AIR'                                                 600
         AG_EXNAME = 'MATERIAL'                                           600
         AG_PARLIST = 'NONE'                                              600
         CALL AXMATERIAL                                                  600
C    *                                                                    600
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       600
         CALL AGSRESET                                                    601
         AG_SEEN = 0                                                      601
         AG_COLO = 1                                                      601
         AG_TITLE = 'SOES'                                                601
         AG_EXNAME = 'ATTRIBUTE'                                          601
         AG_PARLIST = 'SEEN_COLO'                                         601
         CALL AXATTRIBUTE                                                 601
C    *                                                                    601
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  601
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           601
         CALL AGSRESET                                                    603
         AG_RMIN = SERG_ORNGRMIN                                          603
         AG_RMAX = SERG_ORNGRMAX                                          603
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   603
         AG_TITLE = 'TUBE'                                                603
         AG_EXNAME = 'SHAPE'                                              603
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      603
         CALL AXSHAPE                                                     603
C    *                                                                    603
C    Create SOSM :                                                        604
C    Call To Local Procedure SOSM                                         604
         ASSIGN 5810 TO BLOCK_SOSM                                        604
         GO TO  5800                                                      604
5810     CONTINUE                                                         604
C    *                                                                    604
5111  CALL AGSPOPB                                                        605
      GO TO BLOCK_SOES, (5110 ,5120)                                      605
C End of Local Procedure                                                  605
C ----------------------------------------------------------------------  606
C ------------ SOSM                                                       606
C Begin Local Procedure SOSM                                              607
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        607
5800  CONTINUE                                                            607
         AG_TITLE='SOSM'                                                  607
         AG_MODULE=MODULE                                                 607
         AG_MARK='V00'                                                    607
         CALL AGSPUSH                                                     607
         AG_MODULE=MODULE                                                 607
C    *                                                                    607
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       608
         CALL AGSRESET                                                    608
         AG_SEEN = 0                                                      608
         AG_COLO = 1                                                      608
         AG_TITLE = 'SOSM'                                                608
         AG_EXNAME = 'ATTRIBUTE'                                          608
         AG_PARLIST = 'SEEN_COLO'                                         608
         CALL AXATTRIBUTE                                                 608
C    *                                                                    608
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                608
         CALL AGSRESET                                                    609
         AG_NDIV = 4                                                      609
         AG_IAXIS = 2                                                     609
         AG_C0 = 45                                                       609
         AG_TITLE = 'DIVISION'                                            609
         AG_EXNAME = 'SHAPE'                                              609
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     609
         CALL AXSHAPE                                                     609
C    *                                                                    609
C    Create SCRW :                                                        610
C    Call To Local Procedure SCRW                                         610
         ASSIGN 5820 TO BLOCK_SCRW                                        610
         GO TO  5780                                                      610
5820     CONTINUE                                                         610
C    *                                                                    610
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  611
C    ===                                                                  611
         CALL AGSRESET                                                    611
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      611
         AG_TITLE = 'SCRW'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'X'                                                 611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  611
C    ===                                                                  611
         CALL AGSRESET                                                    612
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      612
         AG_TITLE = 'SCRW'                                                612
         AG_EXNAME = 'POSITION'                                           612
         AG_PARLIST = 'X'                                                 612
         CALL AXPOSITION                                                  612
C    *                                                                    612
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  612
C    =SSUP_ERJ2Y <===                                                     612
         CALL AGSRESET                                                    613
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      613
         AG_Y = SSUP_ERJ2Y                                                613
         AG_TITLE = 'SCRW'                                                613
         AG_EXNAME = 'POSITION'                                           613
         AG_PARLIST = 'X_Y'                                               613
         CALL AXPOSITION                                                  613
C    *                                                                    613
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  613
C    =-SSUP_ERJ2Y <===                                                    613
         CALL AGSRESET                                                    614
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      614
         AG_Y = -SSUP_ERJ2Y                                               614
         AG_TITLE = 'SCRW'                                                614
         AG_EXNAME = 'POSITION'                                           614
         AG_PARLIST = 'X_Y'                                               614
         CALL AXPOSITION                                                  614
C    *                                                                    614
5801  CALL AGSPOPB                                                        615
      GO TO BLOCK_SOSM, (5800 ,5810)                                      615
C End of Local Procedure                                                  615
C ----------------------------------------------------------------------  616
C ------------ SBRG                                                       616
C Begin Local Procedure SBRG                                              617
C SBRG IS THE BRACKET JOINING THE END RINGS                               617
5090  CONTINUE                                                            617
         AG_TITLE='SBRG'                                                  617
         AG_MODULE=MODULE                                                 617
         AG_MARK='V00'                                                    617
         CALL AGSPUSH                                                     617
         AG_MODULE=MODULE                                                 617
C    *                                                                    617
         RIN=SERG_IRNGPRMN                                                618
         ROU=SERG_ORNGRMAX                                                619
C    Geant MATERIAL : AIR ===> inherited arguments used <===              620
         CALL AGSRESET                                                    620
         AG_TITLE = 'AIR'                                                 620
         AG_EXNAME = 'MATERIAL'                                           620
         AG_PARLIST = 'NONE'                                              620
         CALL AXMATERIAL                                                  620
C    *                                                                    620
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       620
         CALL AGSRESET                                                    621
         AG_SEEN = 0                                                      621
         AG_COLO = 1                                                      621
         AG_TITLE = 'SBRG'                                                621
         AG_EXNAME = 'ATTRIBUTE'                                          621
         AG_PARLIST = 'SEEN_COLO'                                         621
         CALL AXATTRIBUTE                                                 621
C    *                                                                    621
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      621
         CALL AGSRESET                                                    622
         AG_RMIN = RIN                                                    622
         AG_RMAX = ROU                                                    622
         AG_DZ = SSUP_ERJTHK/2                                            622
         AG_TITLE = 'TUBE'                                                622
         AG_EXNAME = 'SHAPE'                                              622
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      622
         CALL AXSHAPE                                                     622
C    *                                                                    622
C    Create SBRM :                                                        623
C    Call To Local Procedure SBRM                                         623
         ASSIGN 5840 TO BLOCK_SBRM                                        623
         GO TO  5830                                                      623
5840     CONTINUE                                                         623
C    *                                                                    623
5091  CALL AGSPOPB                                                        624
      GO TO BLOCK_SBRG, (5090 ,5100)                                      624
C End of Local Procedure                                                  624
C ----------------------------------------------------------------------  625
C ------------ SBRM                                                       625
C Begin Local Procedure SBRM                                              626
C SBRM IS A THE MOTHER OF A SINGLE BRACKET JOINING THE END RINGS          626
5830  CONTINUE                                                            626
         AG_TITLE='SBRM'                                                  626
         AG_MODULE=MODULE                                                 626
         AG_MARK='V00'                                                    626
         CALL AGSPUSH                                                     626
         AG_MODULE=MODULE                                                 626
C    *                                                                    626
C    Geant ATTRIBUTE : SBRM ===> SEEN=0 COLO=1 <===                       627
         CALL AGSRESET                                                    627
         AG_SEEN = 0                                                      627
         AG_COLO = 1                                                      627
         AG_TITLE = 'SBRM'                                                627
         AG_EXNAME = 'ATTRIBUTE'                                          627
         AG_PARLIST = 'SEEN_COLO'                                         627
         CALL AXATTRIBUTE                                                 627
C    *                                                                    627
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                627
         CALL AGSRESET                                                    628
         AG_NDIV = 4                                                      628
         AG_IAXIS = 2                                                     628
         AG_C0 = 45                                                       628
         AG_TITLE = 'DIVISION'                                            628
         AG_EXNAME = 'SHAPE'                                              628
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     628
         CALL AXSHAPE                                                     628
C    *                                                                    628
C    Create SBRI :                                                        629
C    Call To Local Procedure SBRI                                         629
         ASSIGN 5860 TO BLOCK_SBRI                                        629
         GO TO  5850                                                      629
5860     CONTINUE                                                         629
C    *                                                                    629
C    Geant POSITION : SBRI ===> X=SSUP_ERJRAD <===                        630
         CALL AGSRESET                                                    630
         AG_X = SSUP_ERJRAD                                               630
         AG_TITLE = 'SBRI'                                                630
         AG_EXNAME = 'POSITION'                                           630
         AG_PARLIST = 'X'                                                 630
         CALL AXPOSITION                                                  630
C    *                                                                    630
5831  CALL AGSPOPB                                                        631
      GO TO BLOCK_SBRM, (5830 ,5840)                                      631
C End of Local Procedure                                                  631
C ----------------------------------------------------------------------  632
C ------------ SBRI                                                       632
C Begin Local Procedure SBRI                                              633
C SBRI IS THE BRACKET WHICH JOINS THE RINGS                               633
5850  CONTINUE                                                            633
         AG_TITLE='SBRI'                                                  633
         AG_MODULE=MODULE                                                 633
         AG_MARK='V00'                                                    633
         CALL AGSPUSH                                                     633
         AG_MODULE=MODULE                                                 633
C    *                                                                    633
C    Geant ATTRIBUTE : SBRI ===> SEEN=1 COLO=2 <===                       634
         CALL AGSRESET                                                    634
         AG_SEEN = 1                                                      634
         AG_COLO = 2                                                      634
         AG_TITLE = 'SBRI'                                                634
         AG_EXNAME = 'ATTRIBUTE'                                          634
         AG_PARLIST = 'SEEN_COLO'                                         634
         CALL AXATTRIBUTE                                                 634
C    *                                                                    634
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        634
         CALL AGSRESET                                                    635
         AG_TITLE = 'BERILLIUM'                                           635
         AG_EXNAME = 'MATERIAL'                                           635
         AG_PARLIST = 'NONE'                                              635
         CALL AXMATERIAL                                                  635
C    *                                                                    635
C    Geant SHAPE : BOX ===> DX=SSUP_ERJLEN/2 DY=SSUP_ERJWID/2, DZ=SSUP_E  635
C    RJTHK/2 <===                                                         635
         CALL AGSRESET                                                    637
         AG_DX = SSUP_ERJLEN/2                                            637
         AG_DY = SSUP_ERJWID/2                                            637
         AG_DZ = SSUP_ERJTHK/2                                            637
         AG_TITLE = 'BOX'                                                 637
         AG_EXNAME = 'SHAPE'                                              637
         AG_PARLIST = 'DX_DY_DZ'                                          637
         CALL AXSHAPE                                                     637
C    *                                                                    637
5851  CALL AGSPOPB                                                        638
      GO TO BLOCK_SBRI, (5850 ,5860)                                      638
C End of Local Procedure                                                  638
C ----------------------------------------------------------------------  639
C ------------ SROD                                                       639
C Begin Local Procedure SROD                                              640
C SROD IS THE SVT BE SUPPORT ROD                                          640
5230  CONTINUE                                                            640
         AG_TITLE='SROD'                                                  640
         AG_MODULE=MODULE                                                 640
         AG_MARK='V00'                                                    640
         CALL AGSPUSH                                                     640
         AG_MODULE=MODULE                                                 640
C    *                                                                    640
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        641
         CALL AGSRESET                                                    641
         AG_TITLE = 'BERILLIUM'                                           641
         AG_EXNAME = 'MATERIAL'                                           641
         AG_PARLIST = 'NONE'                                              641
         CALL AXMATERIAL                                                  641
C    *                                                                    641
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=2 <===                       641
         CALL AGSRESET                                                    642
         AG_SEEN = 1                                                      642
         AG_COLO = 2                                                      642
         AG_TITLE = 'SROD'                                                642
         AG_EXNAME = 'ATTRIBUTE'                                          642
         AG_PARLIST = 'SEEN_COLO'                                         642
         CALL AXATTRIBUTE                                                 642
C    *                                                                    642
C    Geant SHAPE : TUBE ===> RMIN=SSUP_RODID/2 RMAX=SSUP_RODOD/2, DZ=SSU  642
C    P_RODLEN/2 <===                                                      642
         CALL AGSRESET                                                    644
         AG_RMIN = SSUP_RODID/2                                           644
         AG_RMAX = SSUP_RODOD/2                                           644
         AG_DZ = SSUP_RODLEN/2                                            644
         AG_TITLE = 'TUBE'                                                644
         AG_EXNAME = 'SHAPE'                                              644
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      644
         CALL AXSHAPE                                                     644
C    *                                                                    644
5231  CALL AGSPOPB                                                        645
      GO TO BLOCK_SROD, (5230 ,5240)                                      645
C End of Local Procedure                                                  645
C ----------------------------------------------------------------------  646
C ------------ SCON                                                       646
C Begin Local Procedure SCON                                              647
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               647
5190  CONTINUE                                                            647
         AG_TITLE='SCON'                                                  647
         AG_MODULE=MODULE                                                 647
         AG_MARK='V00'                                                    647
         CALL AGSPUSH                                                     647
         AG_MODULE=MODULE                                                 647
C    *                                                                    647
         CONE_THK1=SSUP_GRPHTHK                                           648
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                649
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              650
C    Geant MATERIAL : AIR ===> inherited arguments used <===              651
         CALL AGSRESET                                                    651
         AG_TITLE = 'AIR'                                                 651
         AG_EXNAME = 'MATERIAL'                                           651
         AG_PARLIST = 'NONE'                                              651
         CALL AXMATERIAL                                                  651
C    *                                                                    651
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       651
         CALL AGSRESET                                                    652
         AG_SEEN = 0                                                      652
         AG_COLO = 1                                                      652
         AG_TITLE = 'SCON'                                                652
         AG_EXNAME = 'ATTRIBUTE'                                          652
         AG_PARLIST = 'SEEN_COLO'                                         652
         CALL AXATTRIBUTE                                                 652
C    *                                                                    652
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  652
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  652
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  652
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  652
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  652
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  652
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      652
         CALL AGSRESET                                                    662
         AG_PHI1 = 0                                                      662
         AG_DPHI = 360                                                    662
         AG_NZ = 7                                                        662
         AG_ZI(1)=SSUP_CONE1ZMN                                           662
         AG_ZI(2)=SSUP_RODLEN/2                                           662
         AG_ZI(3)=SSUP_RODLEN/2                                           662
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              662
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              662
         AG_ZI(6)=SSUP_CONE3ZMX                                           662
         AG_ZI(7)=SSUP_CONE4ZMX                                           662
         AG_RMN(1)=SSUP_CON1IDMN                                          662
         AG_RMN(2)=SSUP_CON1IDMN                                          662
         AG_RMN(3)=SSUP_CON1IDMN                                          662
         AG_RMN(4)=SSUP_CON1IDMN                                          662
         AG_RMN(5)=SSUP_CON3IDMN                                          662
         AG_RMN(6)=SSUP_CON4IDMN                                          662
         AG_RMN(7)=SSUP_CON4IDMX                                          662
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                662
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                662
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                662
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                662
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                662
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                662
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                662
         AG_TITLE = 'PCON'                                                662
         AG_EXNAME = 'SHAPE'                                              662
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           662
         CALL AXSHAPE                                                     662
C    *                                                                    662
C    Create SGRA :                                                        663
C    Call To Local Procedure SGRA                                         663
         ASSIGN 5880 TO BLOCK_SGRA                                        663
         GO TO  5870                                                      663
5880     CONTINUE                                                         663
C    *                                                                    663
C    Geant POSITION : SGRA ===> inherited arguments used <===             664
         CALL AGSRESET                                                    664
         AG_TITLE = 'SGRA'                                                664
         AG_EXNAME = 'POSITION'                                           664
         AG_PARLIST = 'NONE'                                              664
         CALL AXPOSITION                                                  664
C    *                                                                    664
C    Create STAP :                                                        665
C    Call To Local Procedure STAP                                         665
         ASSIGN 5900 TO BLOCK_STAP                                        665
         GO TO  5890                                                      665
5900     CONTINUE                                                         665
C    *                                                                    665
C    Geant POSITION : STAP ===> inherited arguments used <===             666
         CALL AGSRESET                                                    666
         AG_TITLE = 'STAP'                                                666
         AG_EXNAME = 'POSITION'                                           666
         AG_PARLIST = 'NONE'                                              666
         CALL AXPOSITION                                                  666
C    *                                                                    666
C    Create STAC :                                                        667
C    Call To Local Procedure STAC                                         667
         ASSIGN 5920 TO BLOCK_STAC                                        667
         GO TO  5910                                                      667
5920     CONTINUE                                                         667
C    *                                                                    667
C    Geant POSITION : STAC ===> inherited arguments used <===             668
         CALL AGSRESET                                                    668
         AG_TITLE = 'STAC'                                                668
         AG_EXNAME = 'POSITION'                                           668
         AG_PARLIST = 'NONE'                                              668
         CALL AXPOSITION                                                  668
C    *                                                                    668
C    Create SHLA :                                                        669
C    Call To Local Procedure SHLA                                         669
         ASSIGN 5940 TO BLOCK_SHLA                                        669
         GO TO  5930                                                      669
5940     CONTINUE                                                         669
C    *                                                                    669
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  671
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             671
         CALL AGSRESET                                                    671
         AG_Z = SSUP_RODLEN/                                              671
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    671
         AG_TITLE = 'SHLA'                                                671
         AG_EXNAME = 'POSITION'                                           671
         AG_PARLIST = 'Z'                                                 671
         CALL AXPOSITION                                                  671
C    *                                                                    671
C    Create SHLB :                                                        672
C    Call To Local Procedure SHLB                                         672
         ASSIGN 5960 TO BLOCK_SHLB                                        672
         GO TO  5950                                                      672
5960     CONTINUE                                                         672
C    *                                                                    672
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  673
C    CONE3ZMX) <===                                                       673
         CALL AGSRESET                                                    673
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           673
         AG_TITLE = 'SHLB'                                                673
         AG_EXNAME = 'POSITION'                                           673
         AG_PARLIST = 'Z'                                                 673
         CALL AXPOSITION                                                  673
C    *                                                                    673
C    Create SCMY :                                                        674
C    Call To Local Procedure SCMY                                         674
         ASSIGN 5980 TO BLOCK_SCMY                                        674
         GO TO  5970                                                      674
5980     CONTINUE                                                         674
C    *                                                                    674
C    Geant POSITION : SCMY ===> inherited arguments used <===             675
         CALL AGSRESET                                                    675
         AG_TITLE = 'SCMY'                                                675
         AG_EXNAME = 'POSITION'                                           675
         AG_PARLIST = 'NONE'                                              675
         CALL AXPOSITION                                                  675
C    *                                                                    675
C    Create SCAL :                                                        676
C    Call To Local Procedure SCAL                                         676
         ASSIGN 6000 TO BLOCK_SCAL                                        676
         GO TO  5990                                                      676
6000     CONTINUE                                                         676
C    *                                                                    676
C    Geant POSITION : SCAL ===> inherited arguments used <===             677
         CALL AGSRESET                                                    677
         AG_TITLE = 'SCAL'                                                677
         AG_EXNAME = 'POSITION'                                           677
         AG_PARLIST = 'NONE'                                              677
         CALL AXPOSITION                                                  677
C    *                                                                    677
5191  CALL AGSPOPB                                                        678
      GO TO BLOCK_SCON, (5190 ,5200)                                      678
C End of Local Procedure                                                  678
C ----------------------------------------------------------------------  679
C ------------ SGRA                                                       679
C Begin Local Procedure SGRA                                              680
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 680
5870  CONTINUE                                                            680
         AG_TITLE='SGRA'                                                  680
         AG_MODULE=MODULE                                                 680
         AG_MARK='V00'                                                    680
         CALL AGSPUSH                                                     680
         AG_MODULE=MODULE                                                 680
C    *                                                                    680
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           681
         CALL AGSRESET                                                    681
         AG_TITLE = 'CARBON'                                              681
         AG_EXNAME = 'MATERIAL'                                           681
         AG_PARLIST = 'NONE'                                              681
         CALL AXMATERIAL                                                  681
C    *                                                                    681
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       681
         CALL AGSRESET                                                    682
         AG_SEEN = 1                                                      682
         AG_COLO = 6                                                      682
         AG_TITLE = 'SGRA'                                                682
         AG_EXNAME = 'ATTRIBUTE'                                          682
         AG_PARLIST = 'SEEN_COLO'                                         682
         CALL AXATTRIBUTE                                                 682
C    *                                                                    682
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  682
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  682
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  682
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  682
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  682
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              682
         CALL AGSRESET                                                    690
         AG_PHI1 = 0                                                      690
         AG_DPHI = 360                                                    690
         AG_NZ = 5                                                        690
         AG_ZI(1)=SSUP_RODLEN/2                                           690
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              690
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              690
         AG_ZI(4)=SSUP_CONE3ZMX                                           690
         AG_ZI(5)=SSUP_CONE4ZMX                                           690
         AG_RMN(1)=SSUP_CON1IDMN                                          690
         AG_RMN(2)=SSUP_CON1IDMN                                          690
         AG_RMN(3)=SSUP_CON3IDMN                                          690
         AG_RMN(4)=SSUP_CON4IDMN                                          690
         AG_RMN(5)=SSUP_CON4IDMX                                          690
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             690
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             690
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             690
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             690
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             690
         AG_TITLE = 'PCON'                                                690
         AG_EXNAME = 'SHAPE'                                              690
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           690
         CALL AXSHAPE                                                     690
C    *                                                                    690
5871  CALL AGSPOPB                                                        691
      GO TO BLOCK_SGRA, (5870 ,5880)                                      691
C End of Local Procedure                                                  691
C ----------------------------------------------------------------------  692
C ------------ STAP                                                       692
C Begin Local Procedure STAP                                              693
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  693
C E)                                                                      693
5890  CONTINUE                                                            693
         AG_TITLE='STAP'                                                  693
         AG_MODULE=MODULE                                                 693
         AG_MARK='V00'                                                    693
         CALL AGSPUSH                                                     693
         AG_MODULE=MODULE                                                 693
C    *                                                                    693
         ROFFSET=SSUP_GRPHTHK                                             694
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           695
         CALL AGSRESET                                                    695
         AG_A = 12                                                        695
         AG_Z = 6                                                         695
         AG_W = 1                                                         695
         AG_TITLE = 'C'                                                   695
         AG_EXNAME = 'COMPONENT'                                          695
         AG_PARLIST = 'A_Z_W'                                             695
         CALL AXCOMPONENT                                                 695
C    *                                                                    695
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           695
         CALL AGSRESET                                                    696
         AG_A = 1                                                         696
         AG_Z = 1                                                         696
         AG_W = 2                                                         696
         AG_TITLE = 'H2'                                                  696
         AG_EXNAME = 'COMPONENT'                                          696
         AG_PARLIST = 'A_Z_W'                                             696
         CALL AXCOMPONENT                                                 696
C    *                                                                    696
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             696
         CALL AGSRESET                                                    697
         AG_DENS = 0.935                                                  697
         AG_TITLE = 'CH2'                                                 697
         AG_EXNAME = 'MIXTURE'                                            697
         AG_PARLIST = 'DENS'                                              697
         CALL AXMIXTURE                                                   697
C    *                                                                    697
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       697
         CALL AGSRESET                                                    698
         AG_SEEN = 1                                                      698
         AG_COLO = 3                                                      698
         AG_TITLE = 'STAP'                                                698
         AG_EXNAME = 'ATTRIBUTE'                                          698
         AG_PARLIST = 'SEEN_COLO'                                         698
         CALL AXATTRIBUTE                                                 698
C    *                                                                    698
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  698
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  698
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  698
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  698
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     698
         CALL AGSRESET                                                    704
         AG_PHI1 = 0                                                      704
         AG_DPHI = 360                                                    704
         AG_NZ = 3                                                        704
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              704
         AG_ZI(2)=SSUP_CONE3ZMX                                           704
         AG_ZI(3)=SSUP_CONE4ZMX                                           704
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  704
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  704
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  704
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    704
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    704
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    704
         AG_TITLE = 'PCON'                                                704
         AG_EXNAME = 'SHAPE'                                              704
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           704
         CALL AXSHAPE                                                     704
C    *                                                                    704
5891  CALL AGSPOPB                                                        705
      GO TO BLOCK_STAP, (5890 ,5900)                                      705
C End of Local Procedure                                                  705
C ----------------------------------------------------------------------  706
C ------------ STAC                                                       706
C Begin Local Procedure STAC                                              707
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      707
5910  CONTINUE                                                            707
         AG_TITLE='STAC'                                                  707
         AG_MODULE=MODULE                                                 707
         AG_MARK='V00'                                                    707
         CALL AGSPUSH                                                     707
         AG_MODULE=MODULE                                                 707
C    *                                                                    707
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             708
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           709
         CALL AGSRESET                                                    709
         AG_TITLE = 'COPPER'                                              709
         AG_EXNAME = 'MATERIAL'                                           709
         AG_PARLIST = 'NONE'                                              709
         CALL AXMATERIAL                                                  709
C    *                                                                    709
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       709
         CALL AGSRESET                                                    710
         AG_SEEN = 1                                                      710
         AG_COLO = 2                                                      710
         AG_TITLE = 'STAC'                                                710
         AG_EXNAME = 'ATTRIBUTE'                                          710
         AG_PARLIST = 'SEEN_COLO'                                         710
         CALL AXATTRIBUTE                                                 710
C    *                                                                    710
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  710
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  710
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  710
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  710
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     710
         CALL AGSRESET                                                    716
         AG_PHI1 = 0                                                      716
         AG_DPHI = 360                                                    716
         AG_NZ = 3                                                        716
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              716
         AG_ZI(2)=SSUP_CONE3ZMX                                           716
         AG_ZI(3)=SSUP_CONE4ZMX                                           716
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  716
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  716
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  716
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    716
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    716
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    716
         AG_TITLE = 'PCON'                                                716
         AG_EXNAME = 'SHAPE'                                              716
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           716
         CALL AXSHAPE                                                     716
C    *                                                                    716
5911  CALL AGSPOPB                                                        717
      GO TO BLOCK_STAC, (5910 ,5920)                                      717
C End of Local Procedure                                                  717
C ----------------------------------------------------------------------  718
C ------------ SHLA                                                       718
C Begin Local Procedure SHLA                                              719
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              719
5930  CONTINUE                                                            719
         AG_TITLE='SHLA'                                                  719
         AG_MODULE=MODULE                                                 719
         AG_MARK='V00'                                                    719
         CALL AGSPUSH                                                     719
         AG_MODULE=MODULE                                                 719
C    *                                                                    719
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               720
C    Geant MATERIAL : AIR ===> inherited arguments used <===              721
         CALL AGSRESET                                                    721
         AG_TITLE = 'AIR'                                                 721
         AG_EXNAME = 'MATERIAL'                                           721
         AG_PARLIST = 'NONE'                                              721
         CALL AXMATERIAL                                                  721
C    *                                                                    721
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       721
         CALL AGSRESET                                                    722
         AG_SEEN = 0                                                      722
         AG_COLO = 1                                                      722
         AG_TITLE = 'SHLA'                                                722
         AG_EXNAME = 'ATTRIBUTE'                                          722
         AG_PARLIST = 'SEEN_COLO'                                         722
         CALL AXATTRIBUTE                                                 722
C    *                                                                    722
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  722
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  722
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  722
C    .0*SSUP_HOSRMX <===                                                  722
         CALL AGSRESET                                                    726
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            726
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  726
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  726
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  726
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  726
         AG_TITLE = 'CONE'                                                726
         AG_EXNAME = 'SHAPE'                                              726
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            726
         CALL AXSHAPE                                                     726
C    *                                                                    726
C    Create SHMA :                                                        727
C    Call To Local Procedure SHMA                                         727
         ASSIGN 6020 TO BLOCK_SHMA                                        727
         GO TO  6010                                                      727
6020     CONTINUE                                                         727
C    *                                                                    727
5931  CALL AGSPOPB                                                        728
      GO TO BLOCK_SHLA, (5930 ,5940)                                      728
C End of Local Procedure                                                  728
C ----------------------------------------------------------------------  729
C ------------ SHMA                                                       729
C Begin Local Procedure SHMA                                              730
C SHMA IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 3           730
6010  CONTINUE                                                            730
         AG_TITLE='SHMA'                                                  730
         AG_MODULE=MODULE                                                 730
         AG_MARK='V00'                                                    730
         CALL AGSPUSH                                                     730
         AG_MODULE=MODULE                                                 730
C    *                                                                    730
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               731
         RMIN=SSUP_CON3IDMN + ROFFSET                                     732
         RMAX=SSUP_CON4IDMN + ROFFSET                                     733
         ZMIN=SSUP_RODLEN/2 + SSUP_GRPHTHK                                734
         ZMAX=SSUP_CONE3ZMX                                               735
         CONE_LEN=SQRT((ZMAX - ZMIN)**2. + (RMAX - RMIN)**2.)             736
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  737
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  738
         ANGLE=ASIN(CONE_SIN)*180/3.1416                                  739
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             740
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     741
C    Geant ATTRIBUTE : SHMA ===> SEEN=0 COLO=1 <===                       742
         CALL AGSRESET                                                    742
         AG_SEEN = 0                                                      742
         AG_COLO = 1                                                      742
         AG_TITLE = 'SHMA'                                                742
         AG_EXNAME = 'ATTRIBUTE'                                          742
         AG_PARLIST = 'SEEN_COLO'                                         742
         CALL AXATTRIBUTE                                                 742
C    *                                                                    742
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    742
         CALL AGSRESET                                                    744
         AG_NDIV = SSUP_NHOSES                                            744
         AG_IAXIS = 2                                                     744
         AG_C0 = 0.0                                                      744
         AG_TITLE = 'DIVISION'                                            744
         AG_EXNAME = 'SHAPE'                                              744
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     744
         CALL AXSHAPE                                                     744
C    *                                                                    744
C    Create SWHO :                                                        745
C    Call To Local Procedure SWHO                                         745
         ASSIGN 6040 TO BLOCK_SWHO                                        745
         GO TO  6030                                                      745
6040     CONTINUE                                                         745
C    *                                                                    745
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       746
         CALL AGSRESET                                                    746
         AG_X = XPOS                                                      746
         AG_Y = 0                                                         746
         AG_Z = ZPOS                                                      746
         AG_ALPHAN=AG_ALPHAN+1                                            746
         AG_ALPHAK(AG_ALPHAN)=2                                           746
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       746
         AG_TITLE = 'SWHO'                                                746
         AG_EXNAME = 'POSITION'                                           746
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      746
         CALL AXPOSITION                                                  746
C    *                                                                    746
6011  CALL AGSPOPB                                                        747
      GO TO BLOCK_SHMA, (6010 ,6020)                                      747
C End of Local Procedure                                                  747
C ----------------------------------------------------------------------  748
C ------------ SHLB                                                       748
C Begin Local Procedure SHLB                                              749
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               749
5950  CONTINUE                                                            749
         AG_TITLE='SHLB'                                                  749
         AG_MODULE=MODULE                                                 749
         AG_MARK='V00'                                                    749
         CALL AGSPUSH                                                     749
         AG_MODULE=MODULE                                                 749
C    *                                                                    749
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               750
C    Geant MATERIAL : AIR ===> inherited arguments used <===              751
         CALL AGSRESET                                                    751
         AG_TITLE = 'AIR'                                                 751
         AG_EXNAME = 'MATERIAL'                                           751
         AG_PARLIST = 'NONE'                                              751
         CALL AXMATERIAL                                                  751
C    *                                                                    751
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       751
         CALL AGSRESET                                                    752
         AG_SEEN = 0                                                      752
         AG_COLO = 1                                                      752
         AG_TITLE = 'SHLB'                                                752
         AG_EXNAME = 'ATTRIBUTE'                                          752
         AG_PARLIST = 'SEEN_COLO'                                         752
         CALL AXATTRIBUTE                                                 752
C    *                                                                    752
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  752
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  752
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  752
C    X <===                                                               752
         CALL AGSRESET                                                    756
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         756
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  756
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  756
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  756
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  756
         AG_TITLE = 'CONE'                                                756
         AG_EXNAME = 'SHAPE'                                              756
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            756
         CALL AXSHAPE                                                     756
C    *                                                                    756
C    Create SHMB :                                                        757
C    Call To Local Procedure SHMB                                         757
         ASSIGN 6060 TO BLOCK_SHMB                                        757
         GO TO  6050                                                      757
6060     CONTINUE                                                         757
C    *                                                                    757
5951  CALL AGSPOPB                                                        758
      GO TO BLOCK_SHLB, (5950 ,5960)                                      758
C End of Local Procedure                                                  758
C ----------------------------------------------------------------------  759
C ------------ SHMB                                                       759
C Begin Local Procedure SHMB                                              760
C SHMB IS A SINGLE MOTHER VOLUME FOR A WATER HOSE ON THE CONE 4           760
6050  CONTINUE                                                            760
         AG_TITLE='SHMB'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               761
         RMIN=SSUP_CON4IDMN + ROFFSET                                     762
         RMAX=SSUP_CON4IDMX + ROFFSET                                     763
         ZMIN=SSUP_CONE3ZMX                                               764
         ZMAX=SSUP_CONE4ZMX                                               765
         CONE_LEN=SQRT((ZMAX - ZMIN)**2. + (RMAX - RMIN)**2.)             766
         CONE_SIN=(RMAX - RMIN)/CONE_LEN                                  767
         CONE_COS=(ZMAX - ZMIN)/CONE_LEN                                  768
         ANGLE=ASIN(CONE_SIN)*180/3.1416                                  769
         XPOS=RMIN + 0.5*(RMAX - RMIN) + SSUP_HOSRMX*CONE_COS             770
         ZPOS= - SSUP_HOSRMX*CONE_SIN                                     771
C    Geant ATTRIBUTE : SHMB ===> SEEN=0 COLO=1 <===                       772
         CALL AGSRESET                                                    772
         AG_SEEN = 0                                                      772
         AG_COLO = 1                                                      772
         AG_TITLE = 'SHMB'                                                772
         AG_EXNAME = 'ATTRIBUTE'                                          772
         AG_PARLIST = 'SEEN_COLO'                                         772
         CALL AXATTRIBUTE                                                 772
C    *                                                                    772
C    Geant SHAPE : DIVISION ===> NDIV=SSUP_NHOSES IAXIS=2, C0=0.0 <===    772
         CALL AGSRESET                                                    774
         AG_NDIV = SSUP_NHOSES                                            774
         AG_IAXIS = 2                                                     774
         AG_C0 = 0.0                                                      774
         AG_TITLE = 'DIVISION'                                            774
         AG_EXNAME = 'SHAPE'                                              774
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     774
         CALL AXSHAPE                                                     774
C    *                                                                    774
C    Create SWHO :                                                        775
C    Call To Local Procedure SWHO                                         775
         ASSIGN 6070 TO BLOCK_SWHO                                        775
         GO TO  6030                                                      775
6070     CONTINUE                                                         775
C    *                                                                    775
C    Geant POSITION : SWHO ===> X=XPOS Y=0 Z=ZPOS ALPHAY=ANGLE <===       776
         CALL AGSRESET                                                    776
         AG_X = XPOS                                                      776
         AG_Y = 0                                                         776
         AG_Z = ZPOS                                                      776
         AG_ALPHAN=AG_ALPHAN+1                                            776
         AG_ALPHAK(AG_ALPHAN)=2                                           776
         AG_ALPHAL(AG_ALPHAN)=ANGLE                                       776
         AG_TITLE = 'SWHO'                                                776
         AG_EXNAME = 'POSITION'                                           776
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      776
         CALL AXPOSITION                                                  776
C    *                                                                    776
6051  CALL AGSPOPB                                                        777
      GO TO BLOCK_SHMB, (6050 ,6060)                                      777
C End of Local Procedure                                                  777
C ----------------------------------------------------------------------  778
C ------------ SWHO                                                       778
C Begin Local Procedure SWHO                                              779
C SWHO IS A WATER HOSE                                                    779
6030  CONTINUE                                                            779
         AG_TITLE='SWHO'                                                  779
         AG_MODULE=MODULE                                                 779
         AG_MARK='V00'                                                    779
         CALL AGSPUSH                                                     779
         AG_MODULE=MODULE                                                 779
C    *                                                                    779
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       780
         CALL AGSRESET                                                    780
         AG_SEEN = 1                                                      780
         AG_COLO = 3                                                      780
         AG_TITLE = 'SWHO'                                                780
         AG_EXNAME = 'ATTRIBUTE'                                          780
         AG_PARLIST = 'SEEN_COLO'                                         780
         CALL AXATTRIBUTE                                                 780
C    *                                                                    780
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           780
         CALL AGSRESET                                                    781
         AG_A = 12                                                        781
         AG_Z = 6                                                         781
         AG_W = 1                                                         781
         AG_TITLE = 'C'                                                   781
         AG_EXNAME = 'COMPONENT'                                          781
         AG_PARLIST = 'A_Z_W'                                             781
         CALL AXCOMPONENT                                                 781
C    *                                                                    781
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           781
         CALL AGSRESET                                                    782
         AG_A = 1                                                         782
         AG_Z = 1                                                         782
         AG_W = 2                                                         782
         AG_TITLE = 'H2'                                                  782
         AG_EXNAME = 'COMPONENT'                                          782
         AG_PARLIST = 'A_Z_W'                                             782
         CALL AXCOMPONENT                                                 782
C    *                                                                    782
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             782
         CALL AGSRESET                                                    783
         AG_DENS = 0.935                                                  783
         AG_TITLE = 'CH2'                                                 783
         AG_EXNAME = 'MIXTURE'                                            783
         AG_PARLIST = 'DENS'                                              783
         CALL AXMIXTURE                                                   783
C    *                                                                    783
C    Geant ATTRIBUTE : SWHO ===> SEEN=1 COLO=3 <===                       783
         CALL AGSRESET                                                    784
         AG_SEEN = 1                                                      784
         AG_COLO = 3                                                      784
         AG_TITLE = 'SWHO'                                                784
         AG_EXNAME = 'ATTRIBUTE'                                          784
         AG_PARLIST = 'SEEN_COLO'                                         784
         CALL AXATTRIBUTE                                                 784
C    *                                                                    784
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_HOSRMX DZ=0.5*CONE_LEN <==  784
C    =                                                                    784
         CALL AGSRESET                                                    785
         AG_RMIN = 0                                                      785
         AG_RMAX = SSUP_HOSRMX                                            785
         AG_DZ = 0.5*CONE_LEN                                             785
         AG_TITLE = 'TUBE'                                                785
         AG_EXNAME = 'SHAPE'                                              785
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      785
         CALL AXSHAPE                                                     785
C    *                                                                    785
C    Create SHWA :                                                        786
C    Call To Local Procedure SHWA                                         786
         ASSIGN 6090 TO BLOCK_SHWA                                        786
         GO TO  6080                                                      786
6090     CONTINUE                                                         786
C    *                                                                    786
C    Geant POSITION : SHWA ===> inherited arguments used <===             787
         CALL AGSRESET                                                    787
         AG_TITLE = 'SHWA'                                                787
         AG_EXNAME = 'POSITION'                                           787
         AG_PARLIST = 'NONE'                                              787
         CALL AXPOSITION                                                  787
C    *                                                                    787
6031  CALL AGSPOPB                                                        788
      GO TO BLOCK_SWHO, (6030 ,6040,6070)                                 788
C End of Local Procedure                                                  788
C ----------------------------------------------------------------------  789
C ------------ SHWA                                                       789
C Begin Local Procedure SHWA                                              790
C SHWA IS THE WATER IN THE HOSE                                           790
6080  CONTINUE                                                            790
         AG_TITLE='SHWA'                                                  790
         AG_MODULE=MODULE                                                 790
         AG_MARK='V00'                                                    790
         CALL AGSPUSH                                                     790
         AG_MODULE=MODULE                                                 790
C    *                                                                    790
C    Geant ATTRIBUTE : SHWA ===> SEEN=1 COLO=6 <===                       791
         CALL AGSRESET                                                    791
         AG_SEEN = 1                                                      791
         AG_COLO = 6                                                      791
         AG_TITLE = 'SHWA'                                                791
         AG_EXNAME = 'ATTRIBUTE'                                          791
         AG_PARLIST = 'SEEN_COLO'                                         791
         CALL AXATTRIBUTE                                                 791
C    *                                                                    791
C    Geant MATERIAL : WATER ===> inherited arguments used <===            791
         CALL AGSRESET                                                    792
         AG_TITLE = 'WATER'                                               792
         AG_EXNAME = 'MATERIAL'                                           792
         AG_PARLIST = 'NONE'                                              792
         CALL AXMATERIAL                                                  792
C    *                                                                    792
C    Geant SHAPE : TUBE ===> RMAX=SSUP_HOSRMN <===                        792
         CALL AGSRESET                                                    793
         AG_RMAX = SSUP_HOSRMN                                            793
         AG_TITLE = 'TUBE'                                                793
         AG_EXNAME = 'SHAPE'                                              793
         AG_PARLIST = 'RMAX'                                              793
         CALL AXSHAPE                                                     793
C    *                                                                    793
6081  CALL AGSPOPB                                                        794
      GO TO BLOCK_SHWA, (6080 ,6090)                                      794
C End of Local Procedure                                                  794
C ----------------------------------------------------------------------  795
C ------------ SCMY                                                       795
C Begin Local Procedure SCMY                                              796
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            796
5970  CONTINUE                                                            796
         AG_TITLE='SCMY'                                                  796
         AG_MODULE=MODULE                                                 796
         AG_MARK='V00'                                                    796
         CALL AGSPUSH                                                     796
         AG_MODULE=MODULE                                                 796
C    *                                                                    796
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             797
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          798
         CALL AGSRESET                                                    798
         AG_A = 12                                                        798
         AG_Z = 6                                                         798
         AG_W = 5                                                         798
         AG_TITLE = 'C5'                                                  798
         AG_EXNAME = 'COMPONENT'                                          798
         AG_PARLIST = 'A_Z_W'                                             798
         CALL AXCOMPONENT                                                 798
C    *                                                                    798
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           798
         CALL AGSRESET                                                    799
         AG_A = 1                                                         799
         AG_Z = 1                                                         799
         AG_W = 4                                                         799
         AG_TITLE = 'H4'                                                  799
         AG_EXNAME = 'COMPONENT'                                          799
         AG_PARLIST = 'A_Z_W'                                             799
         CALL AXCOMPONENT                                                 799
C    *                                                                    799
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          799
         CALL AGSRESET                                                    800
         AG_A = 16                                                        800
         AG_Z = 8                                                         800
         AG_W = 2                                                         800
         AG_TITLE = 'O2'                                                  800
         AG_EXNAME = 'COMPONENT'                                          800
         AG_PARLIST = 'A_Z_W'                                             800
         CALL AXCOMPONENT                                                 800
C    *                                                                    800
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            800
         CALL AGSRESET                                                    801
         AG_DENS = 1.39                                                   801
         AG_TITLE = 'MYLAR'                                               801
         AG_EXNAME = 'MIXTURE'                                            801
         AG_PARLIST = 'DENS'                                              801
         CALL AXMIXTURE                                                   801
C    *                                                                    801
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       801
         CALL AGSRESET                                                    802
         AG_SEEN = 1                                                      802
         AG_COLO = 3                                                      802
         AG_TITLE = 'SCMY'                                                802
         AG_EXNAME = 'ATTRIBUTE'                                          802
         AG_PARLIST = 'SEEN_COLO'                                         802
         CALL AXATTRIBUTE                                                 802
C    *                                                                    802
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  802
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  802
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  802
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  802
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     802
         CALL AGSRESET                                                    808
         AG_PHI1 = 0                                                      808
         AG_DPHI = 360                                                    808
         AG_NZ = 3                                                        808
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              808
         AG_ZI(2)=SSUP_CONE3ZMX                                           808
         AG_ZI(3)=SSUP_CONE4ZMX                                           808
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  808
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  808
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  808
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    808
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    808
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    808
         AG_TITLE = 'PCON'                                                808
         AG_EXNAME = 'SHAPE'                                              808
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           808
         CALL AXSHAPE                                                     808
C    *                                                                    808
5971  CALL AGSPOPB                                                        809
      GO TO BLOCK_SCMY, (5970 ,5980)                                      809
C End of Local Procedure                                                  809
C ----------------------------------------------------------------------  810
C ------------ SCAL                                                       810
C Begin Local Procedure SCAL                                              811
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     811
5990  CONTINUE                                                            811
         AG_TITLE='SCAL'                                                  811
         AG_MODULE=MODULE                                                 811
         AG_MARK='V00'                                                    811
         CALL AGSPUSH                                                     811
         AG_MODULE=MODULE                                                 811
C    *                                                                    811
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX +           812
     *   SSUP_WRPMYTHK                                                    812
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        813
         CALL AGSRESET                                                    813
         AG_TITLE = 'ALUMINIUM'                                           813
         AG_EXNAME = 'MATERIAL'                                           813
         AG_PARLIST = 'NONE'                                              813
         CALL AXMATERIAL                                                  813
C    *                                                                    813
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       813
         CALL AGSRESET                                                    814
         AG_SEEN = 1                                                      814
         AG_COLO = 2                                                      814
         AG_TITLE = 'SCAL'                                                814
         AG_EXNAME = 'ATTRIBUTE'                                          814
         AG_PARLIST = 'SEEN_COLO'                                         814
         CALL AXATTRIBUTE                                                 814
C    *                                                                    814
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  814
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  814
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  814
C    FFSET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4  814
C    IDMX+ROFFSET+SSUP_WRPALTHK} <===                                     814
         CALL AGSRESET                                                    820
         AG_PHI1 = 0                                                      820
         AG_DPHI = 360                                                    820
         AG_NZ = 3                                                        820
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              820
         AG_ZI(2)=SSUP_CONE3ZMX                                           820
         AG_ZI(3)=SSUP_CONE4ZMX                                           820
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  820
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  820
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  820
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    820
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    820
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    820
         AG_TITLE = 'PCON'                                                820
         AG_EXNAME = 'SHAPE'                                              820
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           820
         CALL AXSHAPE                                                     820
C    *                                                                    820
5991  CALL AGSPOPB                                                        821
      GO TO BLOCK_SCAL, (5990 ,6000)                                      821
C End of Local Procedure                                                  821
C ----------------------------------------------------------------------  822
C ------------ SWMM                                                       822
C Begin Local Procedure SWMM                                              823
C SWMM IS THE WATER MANIFOLD MOTHER                                       823
5150  CONTINUE                                                            823
         AG_TITLE='SWMM'                                                  823
         AG_MODULE=MODULE                                                 823
         AG_MARK='V00'                                                    823
         CALL AGSPUSH                                                     823
         AG_MODULE=MODULE                                                 823
C    *                                                                    823
         RIN=SWAM_RMIN - SWAM_TBRDTHK                                     824
         ROU=SWAM_RMAX + SWAM_TBRDTHK                                     825
C    Geant MATERIAL : AIR ===> inherited arguments used <===              826
         CALL AGSRESET                                                    826
         AG_TITLE = 'AIR'                                                 826
         AG_EXNAME = 'MATERIAL'                                           826
         AG_PARLIST = 'NONE'                                              826
         CALL AXMATERIAL                                                  826
C    *                                                                    826
C    Geant ATTRIBUTE : SWMM ===> SEEN=0 COLO=1 <===                       826
         CALL AGSRESET                                                    827
         AG_SEEN = 0                                                      827
         AG_COLO = 1                                                      827
         AG_TITLE = 'SWMM'                                                827
         AG_EXNAME = 'ATTRIBUTE'                                          827
         AG_PARLIST = 'SEEN_COLO'                                         827
         CALL AXATTRIBUTE                                                 827
C    *                                                                    827
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  827
C    N/2,+SWAM_LEN/2} RMN={RIN,RIN} RMX={ROU, ROU} <===                   827
         CALL AGSRESET                                                    830
         AG_PHI1 = 0                                                      830
         AG_DPHI = 360                                                    830
         AG_NPDIV = 18                                                    830
         AG_NZ = 2                                                        830
         AG_ZI(1)=-SWAM_LEN/2                                             830
         AG_ZI(2)=+SWAM_LEN/2                                             830
         AG_RMN(1)=RIN                                                    830
         AG_RMN(2)=RIN                                                    830
         AG_RMX(1)=ROU                                                    830
         AG_RMX(2)=ROU                                                    830
         AG_TITLE = 'PGON'                                                830
         AG_EXNAME = 'SHAPE'                                              830
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     830
         CALL AXSHAPE                                                     830
C    *                                                                    830
C    Create SWMB :                                                        831
C    Call To Local Procedure SWMB                                         831
         ASSIGN 6110 TO BLOCK_SWMB                                        831
         GO TO  6100                                                      831
6110     CONTINUE                                                         831
C    *                                                                    831
C    Geant POSITION : SWMB ===> inherited arguments used <===             832
         CALL AGSRESET                                                    832
         AG_TITLE = 'SWMB'                                                832
         AG_EXNAME = 'POSITION'                                           832
         AG_PARLIST = 'NONE'                                              832
         CALL AXPOSITION                                                  832
C    *                                                                    832
C    Create SWMT :                                                        833
C    Call To Local Procedure SWMT                                         833
         ASSIGN 6130 TO BLOCK_SWMT                                        833
         GO TO  6120                                                      833
6130     CONTINUE                                                         833
C    *                                                                    833
C    Geant POSITION : SWMT ===> inherited arguments used <===             834
         CALL AGSRESET                                                    834
         AG_TITLE = 'SWMT'                                                834
         AG_EXNAME = 'POSITION'                                           834
         AG_PARLIST = 'NONE'                                              834
         CALL AXPOSITION                                                  834
C    *                                                                    834
C    Create SWMS :                                                        835
C    Call To Local Procedure SWMS                                         835
         ASSIGN 6150 TO BLOCK_SWMS                                        835
         GO TO  6140                                                      835
6150     CONTINUE                                                         835
C    *                                                                    835
C    Geant POSITION : SWMS ===> Z=-SWAM_LEN/2+SWAM_WALLTHK/2 <===         836
         CALL AGSRESET                                                    836
         AG_Z = -SWAM_LEN/2+SWAM_WALLTHK/2                                836
         AG_TITLE = 'SWMS'                                                836
         AG_EXNAME = 'POSITION'                                           836
         AG_PARLIST = 'Z'                                                 836
         CALL AXPOSITION                                                  836
C    *                                                                    836
C    Geant POSITION : SWMS ===> Z=SWAM_LEN/2-SWAM_WALLTHK/2 <===          836
         CALL AGSRESET                                                    837
         AG_Z = SWAM_LEN/2-SWAM_WALLTHK/2                                 837
         AG_TITLE = 'SWMS'                                                837
         AG_EXNAME = 'POSITION'                                           837
         AG_PARLIST = 'Z'                                                 837
         CALL AXPOSITION                                                  837
C    *                                                                    837
C    Create SWMW :                                                        838
C    Call To Local Procedure SWMW                                         838
         ASSIGN 6170 TO BLOCK_SWMW                                        838
         GO TO  6160                                                      838
6170     CONTINUE                                                         838
C    *                                                                    838
C    Geant POSITION : SWMW ===> inherited arguments used <===             839
         CALL AGSRESET                                                    839
         AG_TITLE = 'SWMW'                                                839
         AG_EXNAME = 'POSITION'                                           839
         AG_PARLIST = 'NONE'                                              839
         CALL AXPOSITION                                                  839
C    *                                                                    839
C    Create SOTB :                                                        840
C    Call To Local Procedure SOTB                                         840
         ASSIGN 6190 TO BLOCK_SOTB                                        840
         GO TO  6180                                                      840
6190     CONTINUE                                                         840
C    *                                                                    840
C    Geant POSITION : SOTB ===> inherited arguments used <===             841
         CALL AGSRESET                                                    841
         AG_TITLE = 'SOTB'                                                841
         AG_EXNAME = 'POSITION'                                           841
         AG_PARLIST = 'NONE'                                              841
         CALL AXPOSITION                                                  841
C    *                                                                    841
C    Create SITB :                                                        842
C    Call To Local Procedure SITB                                         842
         ASSIGN 6210 TO BLOCK_SITB                                        842
         GO TO  6200                                                      842
6210     CONTINUE                                                         842
C    *                                                                    842
C    Geant POSITION : SITB ===> inherited arguments used <===             843
         CALL AGSRESET                                                    843
         AG_TITLE = 'SITB'                                                843
         AG_EXNAME = 'POSITION'                                           843
         AG_PARLIST = 'NONE'                                              843
         CALL AXPOSITION                                                  843
C    *                                                                    843
5151  CALL AGSPOPB                                                        844
      GO TO BLOCK_SWMM, (5150 ,5160)                                      844
C End of Local Procedure                                                  844
C ----------------------------------------------------------------------  845
C ------------ SWMB                                                       845
C Begin Local Procedure SWMB                                              846
C SWMB IS THE WATER MANIFOLD BOTTOM PIECE (SMALL R)                       846
6100  CONTINUE                                                            846
         AG_TITLE='SWMB'                                                  846
         AG_MODULE=MODULE                                                 846
         AG_MARK='V00'                                                    846
         CALL AGSPUSH                                                     846
         AG_MODULE=MODULE                                                 846
C    *                                                                    846
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        847
         CALL AGSRESET                                                    847
         AG_TITLE = 'ALUMINIUM'                                           847
         AG_EXNAME = 'MATERIAL'                                           847
         AG_PARLIST = 'NONE'                                              847
         CALL AXMATERIAL                                                  847
C    *                                                                    847
C    Geant ATTRIBUTE : SWMB ===> SEEN=1 COLO=2 <===                       847
         CALL AGSRESET                                                    848
         AG_SEEN = 1                                                      848
         AG_COLO = 2                                                      848
         AG_TITLE = 'SWMB'                                                848
         AG_EXNAME = 'ATTRIBUTE'                                          848
         AG_PARLIST = 'SEEN_COLO'                                         848
         CALL AXATTRIBUTE                                                 848
C    *                                                                    848
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  848
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN,SWAM_RMIN  848
C    } RMX={SWAM_RMIN+SWAM_WALLTHK, SWAM_RMIN+SWAM_WALLTHK} <===          848
         CALL AGSRESET                                                    852
         AG_PHI1 = 0                                                      852
         AG_DPHI = 360                                                    852
         AG_NPDIV = 18                                                    852
         AG_NZ = 2                                                        852
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                852
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 852
         AG_RMN(1)=SWAM_RMIN                                              852
         AG_RMN(2)=SWAM_RMIN                                              852
         AG_RMX(1)=SWAM_RMIN+SWAM_WALLTHK                                 852
         AG_RMX(2)=SWAM_RMIN+SWAM_WALLTHK                                 852
         AG_TITLE = 'PGON'                                                852
         AG_EXNAME = 'SHAPE'                                              852
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     852
         CALL AXSHAPE                                                     852
C    *                                                                    852
6101  CALL AGSPOPB                                                        853
      GO TO BLOCK_SWMB, (6100 ,6110)                                      853
C End of Local Procedure                                                  853
C ----------------------------------------------------------------------  854
C ------------ SWMT                                                       854
C Begin Local Procedure SWMT                                              855
C SWMT IS THE WATER MANIFOLD TOP PIECE (BIG R)                            855
6120  CONTINUE                                                            855
         AG_TITLE='SWMT'                                                  855
         AG_MODULE=MODULE                                                 855
         AG_MARK='V00'                                                    855
         CALL AGSPUSH                                                     855
         AG_MODULE=MODULE                                                 855
C    *                                                                    855
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        856
         CALL AGSRESET                                                    856
         AG_TITLE = 'ALUMINIUM'                                           856
         AG_EXNAME = 'MATERIAL'                                           856
         AG_PARLIST = 'NONE'                                              856
         CALL AXMATERIAL                                                  856
C    *                                                                    856
C    Geant ATTRIBUTE : SWMT ===> SEEN=1 COLO=2 <===                       856
         CALL AGSRESET                                                    857
         AG_SEEN = 1                                                      857
         AG_COLO = 2                                                      857
         AG_TITLE = 'SWMT'                                                857
         AG_EXNAME = 'ATTRIBUTE'                                          857
         AG_PARLIST = 'SEEN_COLO'                                         857
         CALL AXATTRIBUTE                                                 857
C    *                                                                    857
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  857
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMAX-SWAM_WALL  857
C    THK,SWAM_RMAX-SWAM_WALLTHK}, RMX={SWAM_RMAX,SWAM_RMAX} <===          857
         CALL AGSRESET                                                    861
         AG_PHI1 = 0                                                      861
         AG_DPHI = 360                                                    861
         AG_NPDIV = 18                                                    861
         AG_NZ = 2                                                        861
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                861
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 861
         AG_RMN(1)=SWAM_RMAX-SWAM_WALLTHK                                 861
         AG_RMN(2)=SWAM_RMAX-SWAM_WALLTHK                                 861
         AG_RMX(1)=SWAM_RMAX                                              861
         AG_RMX(2)=SWAM_RMAX                                              861
         AG_TITLE = 'PGON'                                                861
         AG_EXNAME = 'SHAPE'                                              861
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     861
         CALL AXSHAPE                                                     861
C    *                                                                    861
6121  CALL AGSPOPB                                                        862
      GO TO BLOCK_SWMT, (6120 ,6130)                                      862
C End of Local Procedure                                                  862
C ----------------------------------------------------------------------  863
C ------------ SWMS                                                       863
C Begin Local Procedure SWMS                                              864
C SWMS IS THE WATER MANIFOLD SIDE PIECES                                  864
6140  CONTINUE                                                            864
         AG_TITLE='SWMS'                                                  864
         AG_MODULE=MODULE                                                 864
         AG_MARK='V00'                                                    864
         CALL AGSPUSH                                                     864
         AG_MODULE=MODULE                                                 864
C    *                                                                    864
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        865
         CALL AGSRESET                                                    865
         AG_TITLE = 'ALUMINIUM'                                           865
         AG_EXNAME = 'MATERIAL'                                           865
         AG_PARLIST = 'NONE'                                              865
         CALL AXMATERIAL                                                  865
C    *                                                                    865
C    Geant ATTRIBUTE : SWMS ===> SEEN=1 COLO=2 <===                       865
         CALL AGSRESET                                                    866
         AG_SEEN = 1                                                      866
         AG_COLO = 2                                                      866
         AG_TITLE = 'SWMS'                                                866
         AG_EXNAME = 'ATTRIBUTE'                                          866
         AG_PARLIST = 'SEEN_COLO'                                         866
         CALL AXATTRIBUTE                                                 866
C    *                                                                    866
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_WA  866
C    LLTHK/2,SWAM_WALLTHK/2} RMN={SWAM_RMIN, SWAM_RMIN} RMX={SWAM_RMAX,S  866
C    WAM_RMAX} <===                                                       866
         CALL AGSRESET                                                    869
         AG_PHI1 = 0                                                      869
         AG_DPHI = 360                                                    869
         AG_NPDIV = 18                                                    869
         AG_NZ = 2                                                        869
         AG_ZI(1)=-SWAM_WALLTHK/2                                         869
         AG_ZI(2)=SWAM_WALLTHK/2                                          869
         AG_RMN(1)=SWAM_RMIN                                              869
         AG_RMN(2)=SWAM_RMIN                                              869
         AG_RMX(1)=SWAM_RMAX                                              869
         AG_RMX(2)=SWAM_RMAX                                              869
         AG_TITLE = 'PGON'                                                869
         AG_EXNAME = 'SHAPE'                                              869
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     869
         CALL AXSHAPE                                                     869
C    *                                                                    869
6141  CALL AGSPOPB                                                        870
      GO TO BLOCK_SWMS, (6140 ,6150)                                      870
C End of Local Procedure                                                  870
C ----------------------------------------------------------------------  871
C ------------ SWMW                                                       871
C Begin Local Procedure SWMW                                              872
C SWMW IS THE WATER IN THE WATER MANIFOLD                                 872
6160  CONTINUE                                                            872
         AG_TITLE='SWMW'                                                  872
         AG_MODULE=MODULE                                                 872
         AG_MARK='V00'                                                    872
         CALL AGSPUSH                                                     872
         AG_MODULE=MODULE                                                 872
C    *                                                                    872
C    Geant ATTRIBUTE : SWMW ===> SEEN=1 COLO=6 <===                       873
         CALL AGSRESET                                                    873
         AG_SEEN = 1                                                      873
         AG_COLO = 6                                                      873
         AG_TITLE = 'SWMW'                                                873
         AG_EXNAME = 'ATTRIBUTE'                                          873
         AG_PARLIST = 'SEEN_COLO'                                         873
         CALL AXATTRIBUTE                                                 873
C    *                                                                    873
C    Geant MATERIAL : WATER ===> inherited arguments used <===            873
         CALL AGSRESET                                                    874
         AG_TITLE = 'WATER'                                               874
         AG_EXNAME = 'MATERIAL'                                           874
         AG_PARLIST = 'NONE'                                              874
         CALL AXMATERIAL                                                  874
C    *                                                                    874
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  874
C    N/2+SWAM_WALLTHK,SWAM_LEN/2-SWAM_WALLTHK}, RMN={SWAM_RMIN+SWAM_WALL  874
C    THK,SWAM_RMIN+SWAM_WALLTHK}, RMX={SWAM_RMAX-SWAM_WALLTHK, SWAM_RMAX  874
C    -SWAM_WALLTHK} <===                                                  874
         CALL AGSRESET                                                    879
         AG_PHI1 = 0                                                      879
         AG_DPHI = 360                                                    879
         AG_NPDIV = 18                                                    879
         AG_NZ = 2                                                        879
         AG_ZI(1)=-SWAM_LEN/2+SWAM_WALLTHK                                879
         AG_ZI(2)=SWAM_LEN/2-SWAM_WALLTHK                                 879
         AG_RMN(1)=SWAM_RMIN+SWAM_WALLTHK                                 879
         AG_RMN(2)=SWAM_RMIN+SWAM_WALLTHK                                 879
         AG_RMX(1)=SWAM_RMAX-SWAM_WALLTHK                                 879
         AG_RMX(2)=SWAM_RMAX-SWAM_WALLTHK                                 879
         AG_TITLE = 'PGON'                                                879
         AG_EXNAME = 'SHAPE'                                              879
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     879
         CALL AXSHAPE                                                     879
C    *                                                                    879
6161  CALL AGSPOPB                                                        880
      GO TO BLOCK_SWMW, (6160 ,6170)                                      880
C End of Local Procedure                                                  880
C ----------------------------------------------------------------------  881
C ------------ SOTB                                                       881
C Begin Local Procedure SOTB                                              882
C SOTB IS THE OUTER TRANSITION BOARD (LARGE R)                            882
6180  CONTINUE                                                            882
         AG_TITLE='SOTB'                                                  882
         AG_MODULE=MODULE                                                 882
         AG_MARK='V00'                                                    882
         CALL AGSPUSH                                                     882
         AG_MODULE=MODULE                                                 882
C    *                                                                    882
C    Geant MATERIAL : G10 ===> inherited arguments used <===              883
         CALL AGSRESET                                                    883
         AG_TITLE = 'G10'                                                 883
         AG_EXNAME = 'MATERIAL'                                           883
         AG_PARLIST = 'NONE'                                              883
         CALL AXMATERIAL                                                  883
C    *                                                                    883
C    Geant ATTRIBUTE : SOTB ===> SEEN=1 COLO=3 <===                       883
         CALL AGSRESET                                                    884
         AG_SEEN = 1                                                      884
         AG_COLO = 3                                                      884
         AG_TITLE = 'SOTB'                                                884
         AG_EXNAME = 'ATTRIBUTE'                                          884
         AG_PARLIST = 'SEEN_COLO'                                         884
         CALL AXATTRIBUTE                                                 884
C    *                                                                    884
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  884
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMAX, SWAM_RMAX} RMX={SWAM_RMAX+SWAM_TBR  884
C    DTHK,SWAM_RMAX+SWAM_TBRDTHK} <===                                    884
         CALL AGSRESET                                                    887
         AG_PHI1 = 0                                                      887
         AG_DPHI = 360                                                    887
         AG_NPDIV = 18                                                    887
         AG_NZ = 2                                                        887
         AG_ZI(1)=-SWAM_LEN/2                                             887
         AG_ZI(2)=+SWAM_LEN/2                                             887
         AG_RMN(1)=SWAM_RMAX                                              887
         AG_RMN(2)=SWAM_RMAX                                              887
         AG_RMX(1)=SWAM_RMAX+SWAM_TBRDTHK                                 887
         AG_RMX(2)=SWAM_RMAX+SWAM_TBRDTHK                                 887
         AG_TITLE = 'PGON'                                                887
         AG_EXNAME = 'SHAPE'                                              887
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     887
         CALL AXSHAPE                                                     887
C    *                                                                    887
6181  CALL AGSPOPB                                                        888
      GO TO BLOCK_SOTB, (6180 ,6190)                                      888
C End of Local Procedure                                                  888
C ----------------------------------------------------------------------  889
C ------------ SITB                                                       889
C Begin Local Procedure SITB                                              890
C SITB IS THE INNER TRANSITION BOARD (SMALL R)                            890
6200  CONTINUE                                                            890
         AG_TITLE='SITB'                                                  890
         AG_MODULE=MODULE                                                 890
         AG_MARK='V00'                                                    890
         CALL AGSPUSH                                                     890
         AG_MODULE=MODULE                                                 890
C    *                                                                    890
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           891
         CALL AGSRESET                                                    891
         AG_A = 12                                                        891
         AG_Z = 6                                                         891
         AG_W = 1                                                         891
         AG_TITLE = 'C'                                                   891
         AG_EXNAME = 'COMPONENT'                                          891
         AG_PARLIST = 'A_Z_W'                                             891
         CALL AXCOMPONENT                                                 891
C    *                                                                    891
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           891
         CALL AGSRESET                                                    892
         AG_A = 1                                                         892
         AG_Z = 1                                                         892
         AG_W = 2                                                         892
         AG_TITLE = 'H2'                                                  892
         AG_EXNAME = 'COMPONENT'                                          892
         AG_PARLIST = 'A_Z_W'                                             892
         CALL AXCOMPONENT                                                 892
C    *                                                                    892
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             892
         CALL AGSRESET                                                    893
         AG_DENS = 0.935                                                  893
         AG_TITLE = 'CH2'                                                 893
         AG_EXNAME = 'MIXTURE'                                            893
         AG_PARLIST = 'DENS'                                              893
         CALL AXMIXTURE                                                   893
C    *                                                                    893
C    Geant ATTRIBUTE : SITB ===> SEEN=1 COLO=3 <===                       893
         CALL AGSRESET                                                    894
         AG_SEEN = 1                                                      894
         AG_COLO = 3                                                      894
         AG_TITLE = 'SITB'                                                894
         AG_EXNAME = 'ATTRIBUTE'                                          894
         AG_PARLIST = 'SEEN_COLO'                                         894
         CALL AXATTRIBUTE                                                 894
C    *                                                                    894
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=18 NZ=2, ZI={-SWAM_LE  894
C    N/2,+SWAM_LEN/2} RMN={SWAM_RMIN-SWAM_TBRDTHK, SWAM_RMIN-SWAM_TBRDTH  894
C    K} RMX={SWAM_RMIN, SWAM_RMIN} <===                                   894
         CALL AGSRESET                                                    898
         AG_PHI1 = 0                                                      898
         AG_DPHI = 360                                                    898
         AG_NPDIV = 18                                                    898
         AG_NZ = 2                                                        898
         AG_ZI(1)=-SWAM_LEN/2                                             898
         AG_ZI(2)=+SWAM_LEN/2                                             898
         AG_RMN(1)=SWAM_RMIN-SWAM_TBRDTHK                                 898
         AG_RMN(2)=SWAM_RMIN-SWAM_TBRDTHK                                 898
         AG_RMX(1)=SWAM_RMIN                                              898
         AG_RMX(2)=SWAM_RMIN                                              898
         AG_TITLE = 'PGON'                                                898
         AG_EXNAME = 'SHAPE'                                              898
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     898
         CALL AXSHAPE                                                     898
C    *                                                                    898
6201  CALL AGSPOPB                                                        899
      GO TO BLOCK_SITB, (6200 ,6210)                                      899
C End of Local Procedure                                                  899
C ----------------------------------------------------------------------  900
C ------------ SBWC                                                       900
C Begin Local Procedure SBWC                                              901
C SBWC IS THE BRACKET CONNECTING THE WATER MANIFOLD TO THE CONE           901
5170  CONTINUE                                                            901
         AG_TITLE='SBWC'                                                  901
         AG_MODULE=MODULE                                                 901
         AG_MARK='V00'                                                    901
         CALL AGSPUSH                                                     901
         AG_MODULE=MODULE                                                 901
C    *                                                                    901
         RIN=SWAM_RMIN                                                    902
         ROU=SSUP_CON1IDMN                                                903
C    Geant MATERIAL : AIR ===> inherited arguments used <===              904
         CALL AGSRESET                                                    904
         AG_TITLE = 'AIR'                                                 904
         AG_EXNAME = 'MATERIAL'                                           904
         AG_PARLIST = 'NONE'                                              904
         CALL AXMATERIAL                                                  904
C    *                                                                    904
C    Geant ATTRIBUTE : SBWC ===> SEEN=0 COLO=1 <===                       904
         CALL AGSRESET                                                    905
         AG_SEEN = 0                                                      905
         AG_COLO = 1                                                      905
         AG_TITLE = 'SBWC'                                                905
         AG_EXNAME = 'ATTRIBUTE'                                          905
         AG_PARLIST = 'SEEN_COLO'                                         905
         CALL AXATTRIBUTE                                                 905
C    *                                                                    905
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=(SSUP_CONE1ZMN-(SWAM_Z  905
C    MIN+SWAM_LEN))/2 <===                                                905
         CALL AGSRESET                                                    906
         AG_RMIN = RIN                                                    906
         AG_RMAX = ROU                                                    906
         AG_DZ = (SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2                   906
         AG_TITLE = 'TUBE'                                                906
         AG_EXNAME = 'SHAPE'                                              906
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      906
         CALL AXSHAPE                                                     906
C    *                                                                    906
C    Create SWCM :                                                        907
C    Call To Local Procedure SWCM                                         907
         ASSIGN 6230 TO BLOCK_SWCM                                        907
         GO TO  6220                                                      907
6230     CONTINUE                                                         907
C    *                                                                    907
5171  CALL AGSPOPB                                                        908
      GO TO BLOCK_SBWC, (5170 ,5180)                                      908
C End of Local Procedure                                                  908
C ----------------------------------------------------------------------  909
C ------------ SWCM                                                       909
C Begin Local Procedure SWCM                                              910
C SWCM IS A SINGLE BRACKET MOTHER BETWEEN MANI AND CONE                   910
6220  CONTINUE                                                            910
         AG_TITLE='SWCM'                                                  910
         AG_MODULE=MODULE                                                 910
         AG_MARK='V00'                                                    910
         CALL AGSPUSH                                                     910
         AG_MODULE=MODULE                                                 910
C    *                                                                    910
C    Geant ATTRIBUTE : SWCM ===> SEEN=0 COLO=1 <===                       911
         CALL AGSRESET                                                    911
         AG_SEEN = 0                                                      911
         AG_COLO = 1                                                      911
         AG_TITLE = 'SWCM'                                                911
         AG_EXNAME = 'ATTRIBUTE'                                          911
         AG_PARLIST = 'SEEN_COLO'                                         911
         CALL AXATTRIBUTE                                                 911
C    *                                                                    911
C    Geant SHAPE : DIVISION ===> NDIV=3 IAXIS=2 C0=0 <===                 911
         CALL AGSRESET                                                    912
         AG_NDIV = 3                                                      912
         AG_IAXIS = 2                                                     912
         AG_C0 = 0                                                        912
         AG_TITLE = 'DIVISION'                                            912
         AG_EXNAME = 'SHAPE'                                              912
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     912
         CALL AXSHAPE                                                     912
C    *                                                                    912
C    Create SXAI :                                                        913
C    Call To Local Procedure SXAI                                         913
         ASSIGN 6250 TO BLOCK_SXAI                                        913
         GO TO  6240                                                      913
6250     CONTINUE                                                         913
C    *                                                                    913
C    Geant POSITION : SXAI ===> Z=(-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))  914
C    /2+SSUP_BRATHK/2) <===                                               914
         CALL AGSRESET                                                    914
         AG_Z = (-(SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN))/2+SSUP_BRATHK/2)   914
         AG_TITLE = 'SXAI'                                                914
         AG_EXNAME = 'POSITION'                                           914
         AG_PARLIST = 'Z'                                                 914
         CALL AXPOSITION                                                  914
C    *                                                                    914
C    Create SXBI :                                                        915
C    Call To Local Procedure SXBI                                         915
         ASSIGN 6270 TO BLOCK_SXBI                                        915
         GO TO  6260                                                      915
6270     CONTINUE                                                         915
C    *                                                                    915
C    Geant POSITION : SXBI ===> Z=(SSUP_BRATHK/2) <===                    916
         CALL AGSRESET                                                    916
         AG_Z = (SSUP_BRATHK/2)                                           916
         AG_TITLE = 'SXBI'                                                916
         AG_EXNAME = 'POSITION'                                           916
         AG_PARLIST = 'Z'                                                 916
         CALL AXPOSITION                                                  916
C    *                                                                    916
6221  CALL AGSPOPB                                                        917
      GO TO BLOCK_SWCM, (6220 ,6230)                                      917
C End of Local Procedure                                                  917
C ----------------------------------------------------------------------  918
C ------------ SXAI                                                       918
C Begin Local Procedure SXAI                                              919
C SXAI IS A FIRST PIECE (A) OF THE BRACKET BETWEEN MANI AND CONE (X)      919
6240  CONTINUE                                                            919
         AG_TITLE='SXAI'                                                  919
         AG_MODULE=MODULE                                                 919
         AG_MARK='V00'                                                    919
         CALL AGSPUSH                                                     919
         AG_MODULE=MODULE                                                 919
C    *                                                                    919
C    Geant ATTRIBUTE : SXAI ===> SEEN=1 COLO=2 <===                       920
         CALL AGSRESET                                                    920
         AG_SEEN = 1                                                      920
         AG_COLO = 2                                                      920
         AG_TITLE = 'SXAI'                                                920
         AG_EXNAME = 'ATTRIBUTE'                                          920
         AG_PARLIST = 'SEEN_COLO'                                         920
         CALL AXATTRIBUTE                                                 920
C    *                                                                    920
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        920
         CALL AGSRESET                                                    921
         AG_TITLE = 'ALUMINIUM'                                           921
         AG_EXNAME = 'MATERIAL'                                           921
         AG_PARLIST = 'NONE'                                              921
         CALL AXMATERIAL                                                  921
C    *                                                                    921
C    Geant SHAPE : TUBS ===> RMIN=SWAM_RMIN RMAX=SSUP_CON1IDMN, PHI1=-5   921
C    PHI2=5 DZ=SSUP_BRATHK/2 <===                                         921
         CALL AGSRESET                                                    923
         AG_RMIN = SWAM_RMIN                                              923
         AG_RMAX = SSUP_CON1IDMN                                          923
         AG_PHI1 = -5                                                     923
         AG_PHI2 = 5                                                      923
         AG_DZ = SSUP_BRATHK/2                                            923
         AG_TITLE = 'TUBS'                                                923
         AG_EXNAME = 'SHAPE'                                              923
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            923
         CALL AXSHAPE                                                     923
C    *                                                                    923
6241  CALL AGSPOPB                                                        924
      GO TO BLOCK_SXAI, (6240 ,6250)                                      924
C End of Local Procedure                                                  924
C ----------------------------------------------------------------------  925
C ------------ SXBI                                                       925
C Begin Local Procedure SXBI                                              926
C SXBI IS A SECOND PIECE (B) OF THE BRACKET BETWEEN MANI AND CONE (X)     926
6260  CONTINUE                                                            926
         AG_TITLE='SXBI'                                                  926
         AG_MODULE=MODULE                                                 926
         AG_MARK='V00'                                                    926
         CALL AGSPUSH                                                     926
         AG_MODULE=MODULE                                                 926
C    *                                                                    926
C    Geant ATTRIBUTE : SXBI ===> SEEN=1 COLO=2 <===                       927
         CALL AGSRESET                                                    927
         AG_SEEN = 1                                                      927
         AG_COLO = 2                                                      927
         AG_TITLE = 'SXBI'                                                927
         AG_EXNAME = 'ATTRIBUTE'                                          927
         AG_PARLIST = 'SEEN_COLO'                                         927
         CALL AXATTRIBUTE                                                 927
C    *                                                                    927
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        927
         CALL AGSRESET                                                    928
         AG_TITLE = 'ALUMINIUM'                                           928
         AG_EXNAME = 'MATERIAL'                                           928
         AG_PARLIST = 'NONE'                                              928
         CALL AXMATERIAL                                                  928
C    *                                                                    928
C    Geant SHAPE : TUBS ===> RMIN=SSUP_CON1IDMN-SSUP_BRATHK, RMAX=SSUP_C  928
C    ON1IDMN PHI1=-5 PHI2=5 DZ=((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP  928
C    _BRATHK)/2) <===                                                     928
         CALL AGSRESET                                                    930
         AG_RMIN = SSUP_CON1IDMN-SSUP_BRATHK                              930
         AG_RMAX = SSUP_CON1IDMN                                          930
         AG_PHI1 = -5                                                     930
         AG_PHI2 = 5                                                      930
         AG_DZ = ((SSUP_CONE1ZMN-(SWAM_ZMIN+SWAM_LEN)-SSUP_BRATHK)/2)     930
         AG_TITLE = 'TUBS'                                                930
         AG_EXNAME = 'SHAPE'                                              930
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            930
         CALL AXSHAPE                                                     930
C    *                                                                    930
6261  CALL AGSPOPB                                                        931
      GO TO BLOCK_SXBI, (6260 ,6270)                                      931
C End of Local Procedure                                                  931
C ----------------------------------------------------------------------  932
C ------------ SCRW                                                       932
C Begin Local Procedure SCRW                                              933
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   933
5780  CONTINUE                                                            933
         AG_TITLE='SCRW'                                                  933
         AG_MODULE=MODULE                                                 933
         AG_MARK='V00'                                                    933
         CALL AGSPUSH                                                     933
         AG_MODULE=MODULE                                                 933
C    *                                                                    933
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        934
         CALL AGSRESET                                                    934
         AG_TITLE = 'BERILLIUM'                                           934
         AG_EXNAME = 'MATERIAL'                                           934
         AG_PARLIST = 'NONE'                                              934
         CALL AXMATERIAL                                                  934
C    *                                                                    934
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       934
         CALL AGSRESET                                                    935
         AG_SEEN = 1                                                      935
         AG_COLO = 2                                                      935
         AG_TITLE = 'SCRW'                                                935
         AG_EXNAME = 'ATTRIBUTE'                                          935
         AG_PARLIST = 'SEEN_COLO'                                         935
         CALL AXATTRIBUTE                                                 935
C    *                                                                    935
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  935
C    NDRNG_Z) <===                                                        935
         CALL AGSRESET                                                    936
         AG_RMIN = 0                                                      936
         AG_RMAX = SSUP_ERJDIA/2                                          936
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   936
         AG_TITLE = 'TUBE'                                                936
         AG_EXNAME = 'SHAPE'                                              936
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      936
         CALL AXSHAPE                                                     936
C    *                                                                    936
5781  CALL AGSPOPB                                                        937
      GO TO BLOCK_SCRW, (5780 ,5790,5820)                                 937
C End of Local Procedure                                                  937
C ----------------------------------------------------------------------  938
C ------------ SFMO                                                       938
C Begin Local Procedure SFMO                                              939
C SFMO IS THE MOTHER OF THE FOURTH LAYER (STRIP DETECTORS)                939
5290  CONTINUE                                                            939
         AG_TITLE='SFMO'                                                  939
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
C    Geant ATTRIBUTE : SFMO ===> SEEN=0 COLO=1 <===                       940
         CALL AGSRESET                                                    941
         AG_SEEN = 0                                                      941
         AG_COLO = 1                                                      941
         AG_TITLE = 'SFMO'                                                941
         AG_EXNAME = 'ATTRIBUTE'                                          941
         AG_PARLIST = 'SEEN_COLO'                                         941
         CALL AXATTRIBUTE                                                 941
C    *                                                                    941
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  941
C    2 <===                                                               941
         CALL AGSRESET                                                    943
         AG_RMIN = SFPA_RMIN                                              943
         AG_RMAX = SFPA_RMAX                                              943
         AG_DZ = SFPA_LEN/2                                               943
         AG_TITLE = 'TUBE'                                                943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      943
         CALL AXSHAPE                                                     943
C    *                                                                    943
         DTHK=SFPA_SMTHK + SFPA_GPTHK                                     943
         RADTILT=(SFPA_TILT*PI)/180.                                      945
C    *                                                                    946
         ILAD=1                                                           946
            GO TO 6283                                                    946
6281        ILAD=ILAD+1                                                   946
6283        IF(ILAD-(20).GT.0) GO TO 6282                                 946
C       Create SFLM :                                                     947
C       Call To Local Procedure SFLM                                      947
            ASSIGN 6300 TO BLOCK_SFLM                                     947
            GO TO  6290                                                   947
6300        CONTINUE                                                      947
C       *                                                                 947
            ANG=(FLOAT(ILAD)-1)*2.0*PI/20.0                               948
C       Geant POSITION : SFLM ===> X=-(SFPA_RAD*SIN(ANG)+(DTHK*SIN(ANG+R  951
C       ADTILT))/2) Y=(SFPA_RAD*COS(ANG)+(DTHK*COS(ANG+RADTILT))/2) Z=0   951
C       ALPHAZ=(180.*ANG/PI)+SFPA_TILT <===                               951
            CALL AGSRESET                                                 951
            AG_X = -(SFPA_RAD*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/2)         951
            AG_Y = (SFPA_RAD*COS(ANG)+(DTHK*COS(ANG+RADTILT))/2)          951
            AG_Z = 0                                                      951
            AG_ALPHAN=AG_ALPHAN+1                                         951
            AG_ALPHAK(AG_ALPHAN)=3                                        951
            AG_ALPHAL(AG_ALPHAN)=(180.*ANG/PI)+SFPA_TILT                  951
            AG_TITLE = 'SFLM'                                             951
            AG_EXNAME = 'POSITION'                                        951
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   951
            CALL AXPOSITION                                               951
C       *                                                                 951
         GO TO 6281                                                       952
6282     CONTINUE                                                         952
5291  CALL AGSPOPB                                                        953
      GO TO BLOCK_SFMO, (5290 ,5300)                                      953
C End of Local Procedure                                                  953
C ----------------------------------------------------------------------  954
C ------------ SFLM                                                       954
C Begin Local Procedure SFLM                                              955
C SFLM IS THE MOTHER OF THE 4TH LAYER LADDER                              955
6290  CONTINUE                                                            955
         AG_TITLE='SFLM'                                                  955
         AG_MODULE=MODULE                                                 955
         AG_MARK='V00'                                                    955
         CALL AGSPUSH                                                     955
         AG_MODULE=MODULE                                                 955
C    *                                                                    955
C    Geant MATERIAL : AIR ===> inherited arguments used <===              956
         CALL AGSRESET                                                    956
         AG_TITLE = 'AIR'                                                 956
         AG_EXNAME = 'MATERIAL'                                           956
         AG_PARLIST = 'NONE'                                              956
         CALL AXMATERIAL                                                  956
C    *                                                                    956
C    Geant ATTRIBUTE : SFLM ===> SEEN=0 COLO=1 <===                       956
         CALL AGSRESET                                                    957
         AG_SEEN = 0                                                      957
         AG_COLO = 1                                                      957
         AG_TITLE = 'SFLM'                                                957
         AG_EXNAME = 'ATTRIBUTE'                                          957
         AG_PARLIST = 'SEEN_COLO'                                         957
         CALL AXATTRIBUTE                                                 957
C    *                                                                    957
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  957
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    957
         CALL AGSRESET                                                    959
         AG_DX = SFPA_DMWID/2                                             959
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     959
         AG_DZ = SFPA_SMLEN/2                                             959
         AG_TITLE = 'BOX'                                                 959
         AG_EXNAME = 'SHAPE'                                              959
         AG_PARLIST = 'DX_DY_DZ'                                          959
         CALL AXSHAPE                                                     959
C    *                                                                    959
C    Create SFDM :                                                        960
C    Call To Local Procedure SFDM                                         960
         ASSIGN 6320 TO BLOCK_SFDM                                        960
         GO TO  6310                                                      960
6320     CONTINUE                                                         960
C    *                                                                    960
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         961
         CALL AGSRESET                                                    961
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                961
         AG_TITLE = 'SFDM'                                                961
         AG_EXNAME = 'POSITION'                                           961
         AG_PARLIST = 'Y'                                                 961
         CALL AXPOSITION                                                  961
C    *                                                                    961
C    Create SFSM :                                                        962
C    Call To Local Procedure SFSM                                         962
         ASSIGN 6340 TO BLOCK_SFSM                                        962
         GO TO  6330                                                      962
6340     CONTINUE                                                         962
C    *                                                                    962
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          963
         CALL AGSRESET                                                    963
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 963
         AG_TITLE = 'SFSM'                                                963
         AG_EXNAME = 'POSITION'                                           963
         AG_PARLIST = 'Y'                                                 963
         CALL AXPOSITION                                                  963
C    *                                                                    963
6291  CALL AGSPOPB                                                        964
      GO TO BLOCK_SFLM, (6290 ,6300)                                      964
C End of Local Procedure                                                  964
C ----------------------------------------------------------------------  965
C ------------ SFDM                                                       965
C Begin Local Procedure SFDM                                              966
C SFDM IS THE MOTHER OF THE DETECTORS                                     966
6310  CONTINUE                                                            966
         AG_TITLE='SFDM'                                                  966
         AG_MODULE=MODULE                                                 966
         AG_MARK='V00'                                                    966
         CALL AGSPUSH                                                     966
         AG_MODULE=MODULE                                                 966
C    *                                                                    966
C    Geant MATERIAL : AIR ===> inherited arguments used <===              967
         CALL AGSRESET                                                    967
         AG_TITLE = 'AIR'                                                 967
         AG_EXNAME = 'MATERIAL'                                           967
         AG_PARLIST = 'NONE'                                              967
         CALL AXMATERIAL                                                  967
C    *                                                                    967
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=1 <===                       967
         CALL AGSRESET                                                    968
         AG_SEEN = 0                                                      968
         AG_COLO = 1                                                      968
         AG_TITLE = 'SFDM'                                                968
         AG_EXNAME = 'ATTRIBUTE'                                          968
         AG_PARLIST = 'SEEN_COLO'                                         968
         CALL AXATTRIBUTE                                                 968
C    *                                                                    968
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  968
C    EN/2 <===                                                            968
         CALL AGSRESET                                                    970
         AG_DX = SFPA_DMWID/2                                             970
         AG_DY = SFPA_DMTHK/2                                             970
         AG_DZ = SFPA_WPLEN/2                                             970
         AG_TITLE = 'BOX'                                                 970
         AG_EXNAME = 'SHAPE'                                              970
         AG_PARLIST = 'DX_DY_DZ'                                          970
         CALL AXSHAPE                                                     970
C    *                                                                    970
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              970
C    *                                                                    972
         IWAF=1                                                           972
            GO TO 6353                                                    972
6351        IWAF=IWAF+1                                                   972
6353        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 6352                          972
C       Create SFSW :                                                     973
C       Call To Local Procedure SFSW                                      973
            ASSIGN 6370 TO BLOCK_SFSW                                     973
            GO TO  6360                                                   973
6370        CONTINUE                                                      973
C       *                                                                 973
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  974
C       CKLEN <===                                                        974
            CALL AGSRESET                                                 974
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               974
            AG_TITLE = 'SFSW'                                             974
            AG_EXNAME = 'POSITION'                                        974
            AG_PARLIST = 'Z'                                              974
            CALL AXPOSITION                                               974
C       *                                                                 974
         GO TO 6351                                                       975
6352     CONTINUE                                                         975
6311  CALL AGSPOPB                                                        976
      GO TO BLOCK_SFDM, (6310 ,6320)                                      976
C End of Local Procedure                                                  976
C ----------------------------------------------------------------------  977
C ------------ SFSW                                                       977
C Begin Local Procedure SFSW                                              978
C SFSW IS A SINGLE WAFER CONTAINER                                        978
6360  CONTINUE                                                            978
         AG_TITLE='SFSW'                                                  978
         AG_MODULE=MODULE                                                 978
         AG_MARK='V00'                                                    978
         CALL AGSPUSH                                                     978
         AG_MODULE=MODULE                                                 978
C    *                                                                    978
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=1 <===                       979
         CALL AGSRESET                                                    979
         AG_SEEN = 0                                                      979
         AG_COLO = 1                                                      979
         AG_TITLE = 'SFSW'                                                979
         AG_EXNAME = 'ATTRIBUTE'                                          979
         AG_PARLIST = 'SEEN_COLO'                                         979
         CALL AXATTRIBUTE                                                 979
C    *                                                                    979
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=WAFPCKLE  979
C    N/2 <===                                                             979
         CALL AGSRESET                                                    981
         AG_DX = SFPA_DMWID/2                                             981
         AG_DY = SFPA_DMTHK/2                                             981
         AG_DZ = WAFPCKLEN/2                                              981
         AG_TITLE = 'BOX'                                                 981
         AG_EXNAME = 'SHAPE'                                              981
         AG_PARLIST = 'DX_DY_DZ'                                          981
         CALL AXSHAPE                                                     981
C    *                                                                    981
C    Create SFSD :                                                        982
C    Call To Local Procedure SFSD                                         982
         ASSIGN 6390 TO BLOCK_SFSD                                        982
         GO TO  6380                                                      982
6390     CONTINUE                                                         982
C    *                                                                    982
C    Geant POSITION : SFSD ===> inherited arguments used <===             983
         CALL AGSRESET                                                    983
         AG_TITLE = 'SFSD'                                                983
         AG_EXNAME = 'POSITION'                                           983
         AG_PARLIST = 'NONE'                                              983
         CALL AXPOSITION                                                  983
C    *                                                                    983
6361  CALL AGSPOPB                                                        984
      GO TO BLOCK_SFSW, (6360 ,6370)                                      984
C End of Local Procedure                                                  984
C ----------------------------------------------------------------------  985
C ------------ SFSD                                                       985
C Begin Local Procedure SFSD                                              986
C SFSD IS THE STRIP DETECTOR                                              986
6380  CONTINUE                                                            986
         AG_TITLE='SFSD'                                                  986
         AG_MODULE=MODULE                                                 986
         AG_MARK='V00'                                                    986
         CALL AGSPUSH                                                     986
         AG_MODULE=MODULE                                                 986
C    *                                                                    986
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          987
         CALL AGSRESET                                                    987
         AG_TITLE = 'SILICON'                                             987
         AG_EXNAME = 'MATERIAL'                                           987
         AG_PARLIST = 'NONE'                                              987
         CALL AXMATERIAL                                                  987
C    *                                                                    987
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         987
         CALL AGSRESET                                                    988
         AG_ISVOL = 1                                                     988
         AG_TITLE = 'SENSITIVE'                                           988
         AG_EXNAME = 'MATERIAL'                                           988
         AG_PARLIST = 'ISVOL'                                             988
         CALL AXMATERIAL                                                  988
C    *                                                                    988
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=4 <===                       988
         CALL AGSRESET                                                    989
         AG_SEEN = 2                                                      989
         AG_COLO = 4                                                      989
         AG_TITLE = 'SFSD'                                                989
         AG_EXNAME = 'ATTRIBUTE'                                          989
         AG_PARLIST = 'SEEN_COLO'                                         989
         CALL AXATTRIBUTE                                                 989
C    *                                                                    989
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_SDL  989
C    EN/2 <===                                                            989
         CALL AGSRESET                                                    991
         AG_DX = SFPA_DMWID/2                                             991
         AG_DY = SFPA_DMTHK/2                                             991
         AG_DZ = SFPA_SDLEN/2                                             991
         AG_TITLE = 'BOX'                                                 991
         AG_EXNAME = 'SHAPE'                                              991
         AG_PARLIST = 'DX_DY_DZ'                                          991
         CALL AXSHAPE                                                     991
C    *                                                                    991
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 991
C    Defining Hits: X:.001:S Y:.001: Z:.001: CX:10: CY:10: CZ:10: STEP:.  994
C    01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100) ELOSS:16:(0  994
C    ,0.001)                                                              994
         AG_TITLE = 'SFSD'                                                994
         AG_EXNAME = 'HITS'                                               994
         AG_TYPE=AG_MAGIC                                                 994
         CALL AGSDETV('SFSD')                                             994
C    *                                                                    994
         AG_PARLIST='X'                                                   994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = AG_MAGIC                                              994
         AG_BIN=.001                                                      994
         AG_OPTION='S'                                                    994
         AG_FHMIN = AG_MAGIC                                              994
         AG_FHMAX = AG_MAGIC                                              994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='Y'                                                   994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = AG_MAGIC                                              994
         AG_BIN=.001                                                      994
         AG_FHMIN=AG_MAGIC                                                994
         AG_FHMAX=AG_MAGIC                                                994
         AG_OPTION='*'                                                    994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='Z'                                                   994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = AG_MAGIC                                              994
         AG_BIN=.001                                                      994
         AG_FHMIN=AG_MAGIC                                                994
         AG_FHMAX=AG_MAGIC                                                994
         AG_OPTION='*'                                                    994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='CX'                                                  994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = 10                                                    994
         AG_BIN=AG_MAGIC                                                  994
         AG_FHMIN=AG_MAGIC                                                994
         AG_FHMAX=AG_MAGIC                                                994
         AG_OPTION='*'                                                    994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='CY'                                                  994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = 10                                                    994
         AG_BIN=AG_MAGIC                                                  994
         AG_FHMIN=AG_MAGIC                                                994
         AG_FHMAX=AG_MAGIC                                                994
         AG_OPTION='*'                                                    994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='CZ'                                                  994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = 10                                                    994
         AG_BIN=AG_MAGIC                                                  994
         AG_FHMIN=AG_MAGIC                                                994
         AG_FHMAX=AG_MAGIC                                                994
         AG_OPTION='*'                                                    994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='STEP'                                                994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = AG_MAGIC                                              994
         AG_BIN=.01                                                       994
         AG_FHMIN=AG_MAGIC                                                994
         AG_FHMAX=AG_MAGIC                                                994
         AG_OPTION='*'                                                    994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='SLENG'                                               994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = 16                                                    994
         AG_BIN=AG_MAGIC                                                  994
         AG_OPTION='*'                                                    994
         AG_FHMIN = 0                                                     994
         AG_FHMAX = 500                                                   994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='TOF'                                                 994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = 16                                                    994
         AG_BIN=AG_MAGIC                                                  994
         AG_OPTION='*'                                                    994
         AG_FHMIN = 0                                                     994
         AG_FHMAX = 1.0E-6                                                994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='PTOT'                                                994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = 16                                                    994
         AG_BIN=AG_MAGIC                                                  994
         AG_OPTION='*'                                                    994
         AG_FHMIN = 0                                                     994
         AG_FHMAX = 100                                                   994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         AG_PARLIST='ELOSS'                                               994
         AG_TITLE = 'DETH'                                                994
         AG_NBITS = 16                                                    994
         AG_BIN=AG_MAGIC                                                  994
         AG_OPTION='*'                                                    994
         AG_FHMIN = 0                                                     994
         AG_FHMAX = 0.001                                                 994
         CALL AGSDETH (0)                                                 994
C    *                                                                    994
         CALL AGSDETU('SFSD')                                             994
C    *                                                                    994
6381  CALL AGSPOPB                                                        995
      GO TO BLOCK_SFSD, (6380 ,6390)                                      995
C End of Local Procedure                                                  995
C ----------------------------------------------------------------------  996
C ------------ SFSM                                                       996
C Begin Local Procedure SFSM                                              997
C SFSM IS THE MOTHER OF THE LADDER STRUCT.                                997
6330  CONTINUE                                                            997
         AG_TITLE='SFSM'                                                  997
         AG_MODULE=MODULE                                                 997
         AG_MARK='V00'                                                    997
         CALL AGSPUSH                                                     997
         AG_MODULE=MODULE                                                 997
C    *                                                                    997
C    Geant MATERIAL : AIR ===> inherited arguments used <===              998
         CALL AGSRESET                                                    998
         AG_TITLE = 'AIR'                                                 998
         AG_EXNAME = 'MATERIAL'                                           998
         AG_PARLIST = 'NONE'                                              998
         CALL AXMATERIAL                                                  998
C    *                                                                    998
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=1 <===                       998
         CALL AGSRESET                                                    999
         AG_SEEN = 0                                                      999
         AG_COLO = 1                                                      999
         AG_TITLE = 'SFSM'                                                999
         AG_EXNAME = 'ATTRIBUTE'                                          999
         AG_PARLIST = 'SEEN_COLO'                                         999
         CALL AXATTRIBUTE                                                 999
C    *                                                                    999
C    Geant SHAPE : BOX ===> DX=SFPA_SMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  999
C    EN/2 <===                                                            999
         CALL AGSRESET                                                    1001
         AG_DX = SFPA_SMWID/2                                             1001
         AG_DY = SFPA_SMTHK/2                                             1001
         AG_DZ = SFPA_SMLEN/2                                             1001
         AG_TITLE = 'BOX'                                                 1001
         AG_EXNAME = 'SHAPE'                                              1001
         AG_PARLIST = 'DX_DY_DZ'                                          1001
         CALL AXSHAPE                                                     1001
C    *                                                                    1001
C    Create SFSS :                                                        1002
C    Call To Local Procedure SFSS                                         1002
         ASSIGN 6410 TO BLOCK_SFSS                                        1002
         GO TO  6400                                                      1002
6410     CONTINUE                                                         1002
C    *                                                                    1002
6331  CALL AGSPOPB                                                        1003
      GO TO BLOCK_SFSM, (6330 ,6340)                                      1003
C End of Local Procedure                                                  1003
C ----------------------------------------------------------------------  1004
C ------------ SFSS                                                       1004
C Begin Local Procedure SFSS                                              1005
C SFSS IS THE SUBVOLUME OF THE MOTHER STRUCT.                             1005
6400  CONTINUE                                                            1005
         AG_TITLE='SFSS'                                                  1005
         AG_MODULE=MODULE                                                 1005
         AG_MARK='V00'                                                    1005
         CALL AGSPUSH                                                     1005
         AG_MODULE=MODULE                                                 1005
C    *                                                                    1005
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1006
         CALL AGSRESET                                                    1006
         AG_TITLE = 'AIR'                                                 1006
         AG_EXNAME = 'MATERIAL'                                           1006
         AG_PARLIST = 'NONE'                                              1006
         CALL AXMATERIAL                                                  1006
C    *                                                                    1006
C    Geant ATTRIBUTE : SFSS ===> SEEN=0 COLO=1 <===                       1006
         CALL AGSRESET                                                    1007
         AG_SEEN = 0                                                      1007
         AG_COLO = 1                                                      1007
         AG_TITLE = 'SFSS'                                                1007
         AG_EXNAME = 'ATTRIBUTE'                                          1007
         AG_PARLIST = 'SEEN_COLO'                                         1007
         CALL AXATTRIBUTE                                                 1007
C    *                                                                    1007
C    Geant SHAPE : DIVISION ===> NDIV=20 IAXIS=3 <===                     1007
         CALL AGSRESET                                                    1008
         AG_NDIV = 20                                                     1008
         AG_IAXIS = 3                                                     1008
         AG_TITLE = 'DIVISION'                                            1008
         AG_EXNAME = 'SHAPE'                                              1008
         AG_PARLIST = 'NDIV_IAXIS'                                        1008
         CALL AXSHAPE                                                     1008
C    *                                                                    1008
C    Create SFCP :                                                        1009
C    Call To Local Procedure SFCP                                         1009
         ASSIGN 6430 TO BLOCK_SFCP                                        1009
         GO TO  6420                                                      1009
6430     CONTINUE                                                         1009
C    *                                                                    1009
C    Geant POSITION : SFCP ===> X=SFPA_SMWID/2-5.*SFPA_CPRAD Y=-SFPA_SMT  1010
C    HK/2+SFPA_CPRAD <===                                                 1010
         CALL AGSRESET                                                    1010
         AG_X = SFPA_SMWID/2-5.*SFPA_CPRAD                                1010
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  1010
         AG_TITLE = 'SFCP'                                                1010
         AG_EXNAME = 'POSITION'                                           1010
         AG_PARLIST = 'X_Y'                                               1010
         CALL AXPOSITION                                                  1010
C    *                                                                    1010
C    Geant POSITION : SFCP ===> X=-SFPA_SMWID/2+5.*SFPA_CPRAD Y=-SFPA_SM  1010
C    THK/2+SFPA_CPRAD <===                                                1010
         CALL AGSRESET                                                    1011
         AG_X = -SFPA_SMWID/2+5.*SFPA_CPRAD                               1011
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  1011
         AG_TITLE = 'SFCP'                                                1011
         AG_EXNAME = 'POSITION'                                           1011
         AG_PARLIST = 'X_Y'                                               1011
         CALL AXPOSITION                                                  1011
C    *                                                                    1011
C    Create SFCF :                                                        1012
C    Call To Local Procedure SFCF                                         1012
         ASSIGN 6450 TO BLOCK_SFCF                                        1012
         GO TO  6440                                                      1012
6450     CONTINUE                                                         1012
C    *                                                                    1012
C    Geant POSITION : SFCF ===> inherited arguments used <===             1013
         CALL AGSRESET                                                    1013
         AG_TITLE = 'SFCF'                                                1013
         AG_EXNAME = 'POSITION'                                           1013
         AG_PARLIST = 'NONE'                                              1013
         CALL AXPOSITION                                                  1013
C    *                                                                    1013
6401  CALL AGSPOPB                                                        1014
      GO TO BLOCK_SFSS, (6400 ,6410)                                      1014
C End of Local Procedure                                                  1014
C ----------------------------------------------------------------------  1015
C ------------ SFCP                                                       1015
C Begin Local Procedure SFCP                                              1016
C SFCP IS THE COOLING PIPE                                                1016
6420  CONTINUE                                                            1016
         AG_TITLE='SFCP'                                                  1016
         AG_MODULE=MODULE                                                 1016
         AG_MARK='V00'                                                    1016
         CALL AGSPUSH                                                     1016
         AG_MODULE=MODULE                                                 1016
C    *                                                                    1016
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1017
         CALL AGSRESET                                                    1017
         AG_TITLE = 'CARBON'                                              1017
         AG_EXNAME = 'MATERIAL'                                           1017
         AG_PARLIST = 'NONE'                                              1017
         CALL AXMATERIAL                                                  1017
C    *                                                                    1017
C    Geant ATTRIBUTE : SFCP ===> SEEN=1 COLO=6 <===                       1017
         CALL AGSRESET                                                    1018
         AG_SEEN = 1                                                      1018
         AG_COLO = 6                                                      1018
         AG_TITLE = 'SFCP'                                                1018
         AG_EXNAME = 'ATTRIBUTE'                                          1018
         AG_PARLIST = 'SEEN_COLO'                                         1018
         CALL AXATTRIBUTE                                                 1018
C    *                                                                    1018
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CPRAD DZ=SFPA_SSLEN/2 <===  1018
         CALL AGSRESET                                                    1019
         AG_RMIN = 0                                                      1019
         AG_RMAX = SFPA_CPRAD                                             1019
         AG_DZ = SFPA_SSLEN/2                                             1019
         AG_TITLE = 'TUBE'                                                1019
         AG_EXNAME = 'SHAPE'                                              1019
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1019
         CALL AXSHAPE                                                     1019
C    *                                                                    1019
C    Create SFCW :                                                        1020
C    Call To Local Procedure SFCW                                         1020
         ASSIGN 6470 TO BLOCK_SFCW                                        1020
         GO TO  6460                                                      1020
6470     CONTINUE                                                         1020
C    *                                                                    1020
C    Geant POSITION : SFCW ===> inherited arguments used <===             1021
         CALL AGSRESET                                                    1021
         AG_TITLE = 'SFCW'                                                1021
         AG_EXNAME = 'POSITION'                                           1021
         AG_PARLIST = 'NONE'                                              1021
         CALL AXPOSITION                                                  1021
C    *                                                                    1021
6421  CALL AGSPOPB                                                        1022
      GO TO BLOCK_SFCP, (6420 ,6430)                                      1022
C End of Local Procedure                                                  1022
C ----------------------------------------------------------------------  1023
C ------------ SFCW                                                       1023
C Begin Local Procedure SFCW                                              1024
C SFCW IS THE WATER CYLINDER IN THE COOLING PIPE                          1024
6460  CONTINUE                                                            1024
         AG_TITLE='SFCW'                                                  1024
         AG_MODULE=MODULE                                                 1024
         AG_MARK='V00'                                                    1024
         CALL AGSPUSH                                                     1024
         AG_MODULE=MODULE                                                 1024
C    *                                                                    1024
C    Geant ATTRIBUTE : SFCW ===> SEEN=1 COLO=6 <===                       1025
         CALL AGSRESET                                                    1025
         AG_SEEN = 1                                                      1025
         AG_COLO = 6                                                      1025
         AG_TITLE = 'SFCW'                                                1025
         AG_EXNAME = 'ATTRIBUTE'                                          1025
         AG_PARLIST = 'SEEN_COLO'                                         1025
         CALL AXATTRIBUTE                                                 1025
C    *                                                                    1025
C    Geant MATERIAL : WATER ===> inherited arguments used <===            1025
         CALL AGSRESET                                                    1026
         AG_TITLE = 'WATER'                                               1026
         AG_EXNAME = 'MATERIAL'                                           1026
         AG_PARLIST = 'NONE'                                              1026
         CALL AXMATERIAL                                                  1026
C    *                                                                    1026
C    Geant SHAPE : TUBE ===> RMAX=SFPA_CPRAL <===                         1026
         CALL AGSRESET                                                    1027
         AG_RMAX = SFPA_CPRAL                                             1027
         AG_TITLE = 'TUBE'                                                1027
         AG_EXNAME = 'SHAPE'                                              1027
         AG_PARLIST = 'RMAX'                                              1027
         CALL AXSHAPE                                                     1027
C    *                                                                    1027
6461  CALL AGSPOPB                                                        1028
      GO TO BLOCK_SFCW, (6460 ,6470)                                      1028
C End of Local Procedure                                                  1028
C ----------------------------------------------------------------------  1029
C ------------ SFCF                                                       1029
C Begin Local Procedure SFCF                                              1030
C SFCF IS THE CARBON FIBER STRUCTURE CONTAINER                            1030
6440  CONTINUE                                                            1030
         AG_TITLE='SFCF'                                                  1030
         AG_MODULE=MODULE                                                 1030
         AG_MARK='V00'                                                    1030
         CALL AGSPUSH                                                     1030
         AG_MODULE=MODULE                                                 1030
C    *                                                                    1030
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1031
         CALL AGSRESET                                                    1031
         AG_TITLE = 'AIR'                                                 1031
         AG_EXNAME = 'MATERIAL'                                           1031
         AG_PARLIST = 'NONE'                                              1031
         CALL AXMATERIAL                                                  1031
C    *                                                                    1031
C    Geant ATTRIBUTE : SFCF ===> SEEN=0 COLO=3 <===                       1031
         CALL AGSRESET                                                    1032
         AG_SEEN = 0                                                      1032
         AG_COLO = 3                                                      1032
         AG_TITLE = 'SFCF'                                                1032
         AG_EXNAME = 'ATTRIBUTE'                                          1032
         AG_PARLIST = 'SEEN_COLO'                                         1032
         CALL AXATTRIBUTE                                                 1032
C    *                                                                    1032
C    Geant SHAPE : BOX ===> DX=SFPA_SMTHK*TAN(PI/6.) DY=SFPA_SMTHK/2., D  1032
C    Z=SFPA_SSLEN/2 <===                                                  1032
         CALL AGSRESET                                                    1034
         AG_DX = SFPA_SMTHK*TAN(PI/6.)                                    1034
         AG_DY = SFPA_SMTHK/2.                                            1034
         AG_DZ = SFPA_SSLEN/2                                             1034
         AG_TITLE = 'BOX'                                                 1034
         AG_EXNAME = 'SHAPE'                                              1034
         AG_PARLIST = 'DX_DY_DZ'                                          1034
         CALL AXSHAPE                                                     1034
C    *                                                                    1034
C    Create SFCT :                                                        1035
C    Call To Local Procedure SFCT                                         1035
         ASSIGN 6490 TO BLOCK_SFCT                                        1035
         GO TO  6480                                                      1035
6490     CONTINUE                                                         1035
C    *                                                                    1035
C    Geant POSITION : SFCT ===> Y=SFPA_SMTHK/2.-SFPA_CFRAD <===           1036
         CALL AGSRESET                                                    1036
         AG_Y = SFPA_SMTHK/2.-SFPA_CFRAD                                  1036
         AG_TITLE = 'SFCT'                                                1036
         AG_EXNAME = 'POSITION'                                           1036
         AG_PARLIST = 'Y'                                                 1036
         CALL AXPOSITION                                                  1036
C    *                                                                    1036
C    Geant POSITION : SFCT ===> X=SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD Y=-SF  1036
C    PA_SMTHK/2.+SFPA_CFRAD <===                                          1036
         CALL AGSRESET                                                    1037
         AG_X = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                          1037
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1037
         AG_TITLE = 'SFCT'                                                1037
         AG_EXNAME = 'POSITION'                                           1037
         AG_PARLIST = 'X_Y'                                               1037
         CALL AXPOSITION                                                  1037
C    *                                                                    1037
C    Geant POSITION : SFCT ===> X=-SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD Y=-S  1037
C    FPA_SMTHK/2.+SFPA_CFRAD <===                                         1037
         CALL AGSRESET                                                    1038
         AG_X = -SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD                         1038
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1038
         AG_TITLE = 'SFCT'                                                1038
         AG_EXNAME = 'POSITION'                                           1038
         AG_PARLIST = 'X_Y'                                               1038
         CALL AXPOSITION                                                  1038
C    *                                                                    1038
C    Create SFCX :                                                        1039
C    Call To Local Procedure SFCX                                         1039
         ASSIGN 6510 TO BLOCK_SFCX                                        1039
         GO TO  6500                                                      1039
6510     CONTINUE                                                         1039
C    *                                                                    1039
C    Geant POSITION : SFCX ===> Y=-SFPA_SMTHK/2.+SFPA_CFRAD ORT=YZX <===  1040
         CALL AGSRESET                                                    1040
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 1040
         AG_ALPHAN=AG_ALPHAN+1                                            1040
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1040
         AG_TITLE = 'SFCX'                                                1040
         AG_EXNAME = 'POSITION'                                           1040
         AG_PARLIST = 'Y_ORT'                                             1040
         CALL AXPOSITION                                                  1040
C    *                                                                    1040
C    Geant POSITION : SFCX ===> X=SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.  1040
C     ORT=YZX ALPHAZ=-60 <===                                             1040
         CALL AGSRESET                                                    1041
         AG_X = SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.                    1041
         AG_ALPHAN=AG_ALPHAN+1                                            1041
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1041
         AG_ALPHAN=AG_ALPHAN+1                                            1041
         AG_ALPHAK(AG_ALPHAN)=3                                           1041
         AG_ALPHAL(AG_ALPHAN)=-60                                         1041
         AG_TITLE = 'SFCX'                                                1041
         AG_EXNAME = 'POSITION'                                           1041
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      1041
         CALL AXPOSITION                                                  1041
C    *                                                                    1041
C    Geant POSITION : SFCX ===> X=-SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3  1041
C    . ORT=YZX ALPHAZ=+60 <===                                            1041
         CALL AGSRESET                                                    1042
         AG_X = -SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3.                   1042
         AG_ALPHAN=AG_ALPHAN+1                                            1042
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1042
         AG_ALPHAN=AG_ALPHAN+1                                            1042
         AG_ALPHAK(AG_ALPHAN)=3                                           1042
         AG_ALPHAL(AG_ALPHAN)=+60                                         1042
         AG_TITLE = 'SFCX'                                                1042
         AG_EXNAME = 'POSITION'                                           1042
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      1042
         CALL AXPOSITION                                                  1042
C    *                                                                    1042
6441  CALL AGSPOPB                                                        1043
      GO TO BLOCK_SFCF, (6440 ,6450)                                      1043
C End of Local Procedure                                                  1043
C ----------------------------------------------------------------------  1044
C ------------ SFCT                                                       1044
C Begin Local Procedure SFCT                                              1045
C SFCT IS THE CARBON FIBER TUBE                                           1045
6480  CONTINUE                                                            1045
         AG_TITLE='SFCT'                                                  1045
         AG_MODULE=MODULE                                                 1045
         AG_MARK='V00'                                                    1045
         CALL AGSPUSH                                                     1045
         AG_MODULE=MODULE                                                 1045
C    *                                                                    1045
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1046
         CALL AGSRESET                                                    1046
         AG_TITLE = 'CARBON'                                              1046
         AG_EXNAME = 'MATERIAL'                                           1046
         AG_PARLIST = 'NONE'                                              1046
         CALL AXMATERIAL                                                  1046
C    *                                                                    1046
C    Geant ATTRIBUTE : SFCT ===> SEEN=1 COLO=6 <===                       1046
         CALL AGSRESET                                                    1047
         AG_SEEN = 1                                                      1047
         AG_COLO = 6                                                      1047
         AG_TITLE = 'SFCT'                                                1047
         AG_EXNAME = 'ATTRIBUTE'                                          1047
         AG_PARLIST = 'SEEN_COLO'                                         1047
         CALL AXATTRIBUTE                                                 1047
C    *                                                                    1047
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SSLEN/2 <===  1047
         CALL AGSRESET                                                    1048
         AG_RMIN = 0                                                      1048
         AG_RMAX = SFPA_CFRAD                                             1048
         AG_DZ = SFPA_SSLEN/2                                             1048
         AG_TITLE = 'TUBE'                                                1048
         AG_EXNAME = 'SHAPE'                                              1048
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1048
         CALL AXSHAPE                                                     1048
C    *                                                                    1048
6481  CALL AGSPOPB                                                        1049
      GO TO BLOCK_SFCT, (6480 ,6490)                                      1049
C End of Local Procedure                                                  1049
C ----------------------------------------------------------------------  1050
C ------------ SFCX                                                       1050
C Begin Local Procedure SFCX                                              1051
C SFCX IS THE CARBON FIBER TUBE                                           1051
6500  CONTINUE                                                            1051
         AG_TITLE='SFCX'                                                  1051
         AG_MODULE=MODULE                                                 1051
         AG_MARK='V00'                                                    1051
         CALL AGSPUSH                                                     1051
         AG_MODULE=MODULE                                                 1051
C    *                                                                    1051
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1052
         CALL AGSRESET                                                    1052
         AG_TITLE = 'CARBON'                                              1052
         AG_EXNAME = 'MATERIAL'                                           1052
         AG_PARLIST = 'NONE'                                              1052
         CALL AXMATERIAL                                                  1052
C    *                                                                    1052
C    Geant ATTRIBUTE : SFCX ===> SEEN=1 COLO=7 <===                       1052
         CALL AGSRESET                                                    1053
         AG_SEEN = 1                                                      1053
         AG_COLO = 7                                                      1053
         AG_TITLE = 'SFCX'                                                1053
         AG_EXNAME = 'ATTRIBUTE'                                          1053
         AG_PARLIST = 'SEEN_COLO'                                         1053
         CALL AXATTRIBUTE                                                 1053
C    *                                                                    1053
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SMTHK*TAN(PI  1053
C    /6.)-SFPA_CFRAD <===                                                 1053
         CALL AGSRESET                                                    1054
         AG_RMIN = 0                                                      1054
         AG_RMAX = SFPA_CFRAD                                             1054
         AG_DZ = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                         1054
         AG_TITLE = 'TUBE'                                                1054
         AG_EXNAME = 'SHAPE'                                              1054
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1054
         CALL AXSHAPE                                                     1054
C    *                                                                    1054
6501  CALL AGSPOPB                                                        1055
      GO TO BLOCK_SFCX, (6500 ,6510)                                      1055
C End of Local Procedure                                                  1055
C ----------------------------------------------------------------------  1056
C ------------ SBSP                                                       1056
C Begin Local Procedure SBSP                                              1057
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              1057
5210  CONTINUE                                                            1057
         AG_TITLE='SBSP'                                                  1057
         AG_MODULE=MODULE                                                 1057
         AG_MARK='V00'                                                    1057
         CALL AGSPUSH                                                     1057
         AG_MODULE=MODULE                                                 1057
C    *                                                                    1057
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1058
         CALL AGSRESET                                                    1058
         AG_TITLE = 'AIR'                                                 1058
         AG_EXNAME = 'MATERIAL'                                           1058
         AG_PARLIST = 'NONE'                                              1058
         CALL AXMATERIAL                                                  1058
C    *                                                                    1058
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       1058
         CALL AGSRESET                                                    1059
         AG_SEEN = 0                                                      1059
         AG_COLO = 1                                                      1059
         AG_TITLE = 'SBSP'                                                1059
         AG_EXNAME = 'ATTRIBUTE'                                          1059
         AG_PARLIST = 'SEEN_COLO'                                         1059
         CALL AXATTRIBUTE                                                 1059
C    *                                                                    1059
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  1059
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    1059
         CALL AGSRESET                                                    1061
         AG_RMIN = SVTG_RSIZEMIN                                          1061
         AG_RMAX = SSUB_KMOUNTOD/2                                        1061
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              1061
         AG_TITLE = 'TUBE'                                                1061
         AG_EXNAME = 'SHAPE'                                              1061
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1061
         CALL AXSHAPE                                                     1061
C    *                                                                    1061
C    Create SAKM :                                                        1062
C    Call To Local Procedure SAKM                                         1062
         ASSIGN 6530 TO BLOCK_SAKM                                        1062
         GO TO  6520                                                      1062
6530     CONTINUE                                                         1062
C    *                                                                    1062
C    Geant POSITION : SAKM ===> inherited arguments used <===             1063
         CALL AGSRESET                                                    1063
         AG_TITLE = 'SAKM'                                                1063
         AG_EXNAME = 'POSITION'                                           1063
         AG_PARLIST = 'NONE'                                              1063
         CALL AXPOSITION                                                  1063
C    *                                                                    1063
C    Create SBMM :                                                        1064
C    Call To Local Procedure SBMM                                         1064
         ASSIGN 6550 TO BLOCK_SBMM                                        1064
         GO TO  6540                                                      1064
6550     CONTINUE                                                         1064
C    *                                                                    1064
C    Create SBMM :                                                        1065
C    Call To Local Procedure SBMM                                         1065
         ASSIGN 6560 TO BLOCK_SBMM                                        1065
         GO TO  6540                                                      1065
6560     CONTINUE                                                         1065
C    *                                                                    1065
C    Create SBRL :                                                        1066
C    Call To Local Procedure SBRL                                         1066
         ASSIGN 6580 TO BLOCK_SBRL                                        1066
         GO TO  6570                                                      1066
6580     CONTINUE                                                         1066
C    *                                                                    1066
C    Create SBRX :                                                        1067
C    Call To Local Procedure SBRX                                         1067
         ASSIGN 6600 TO BLOCK_SBRX                                        1067
         GO TO  6590                                                      1067
6600     CONTINUE                                                         1067
C    *                                                                    1067
C    *                                                                    1068
         I=-1                                                             1068
            GOTO 6613                                                     1068
6611        I=I+(2)                                                       1068
6613        IF((2)*(I-(1)).GT.0) GO TO 6612                               1068
C       *                                                                 1069
            J=0                                                           1069
               GO TO 6623                                                 1069
6621           J=J+1                                                      1069
6623           IF(J-(1).GT.0) GO TO 6622                                  1069
               PHI=I*SSUB_SRCUTPHI+180*J                                  1070
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     1071
     *         2.0)                                                       1071
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  1073
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  1073
C          ==                                                             1073
               CALL AGSRESET                                              1073
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               1073
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              1073
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      1073
               AG_ALPHAN=AG_ALPHAN+1                                      1073
               AG_ALPHAK(AG_ALPHAN)=3                                     1073
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1073
               AG_TITLE = 'SBMM'                                          1073
               AG_EXNAME = 'POSITION'                                     1073
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                1073
               CALL AXPOSITION                                            1073
C          *                                                              1073
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         1073
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1076
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1076
C           ORT=ZXY <===                                                  1076
               CALL AGSRESET                                              1076
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1076
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1076
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1076
               AG_ALPHAN=AG_ALPHAN+1                                      1076
               AG_ALPHAK(AG_ALPHAN)=3                                     1076
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1076
               AG_ALPHAN=AG_ALPHAN+1                                      1076
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1076
               AG_TITLE = 'SBRL'                                          1076
               AG_EXNAME = 'POSITION'                                     1076
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1076
               CALL AXPOSITION                                            1076
C          *                                                              1076
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  1076
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  1076
C           ORT=ZXY <===                                                  1076
               CALL AGSRESET                                              1078
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               1078
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              1078
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      1078
               AG_ALPHAN=AG_ALPHAN+1                                      1078
               AG_ALPHAK(AG_ALPHAN)=3                                     1078
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                1078
               AG_ALPHAN=AG_ALPHAN+1                                      1078
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           1078
               AG_TITLE = 'SBRX'                                          1078
               AG_EXNAME = 'POSITION'                                     1078
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            1078
               CALL AXPOSITION                                            1078
C          *                                                              1078
            GO TO 6621                                                    1079
6622        CONTINUE                                                      1079
         GO TO 6611                                                       1080
6612     CONTINUE                                                         1080
C    Create SBSR :                                                        1081
C    Call To Local Procedure SBSR                                         1081
         ASSIGN 6640 TO BLOCK_SBSR                                        1081
         GO TO  6630                                                      1081
6640     CONTINUE                                                         1081
C    *                                                                    1081
C    Geant POSITION : SBSR ===> inherited arguments used <===             1082
         CALL AGSRESET                                                    1082
         AG_TITLE = 'SBSR'                                                1082
         AG_EXNAME = 'POSITION'                                           1082
         AG_PARLIST = 'NONE'                                              1082
         CALL AXPOSITION                                                  1082
C    *                                                                    1082
5211  CALL AGSPOPB                                                        1083
      GO TO BLOCK_SBSP, (5210 ,5220)                                      1083
C End of Local Procedure                                                  1083
C ----------------------------------------------------------------------  1084
C ------------ SAKM                                                       1084
C Begin Local Procedure SAKM                                              1085
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   1085
6520  CONTINUE                                                            1085
         AG_TITLE='SAKM'                                                  1085
         AG_MODULE=MODULE                                                 1085
         AG_MARK='V00'                                                    1085
         CALL AGSPUSH                                                     1085
         AG_MODULE=MODULE                                                 1085
C    *                                                                    1085
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1086
         CALL AGSRESET                                                    1086
         AG_TITLE = 'ALUMINIUM'                                           1086
         AG_EXNAME = 'MATERIAL'                                           1086
         AG_PARLIST = 'NONE'                                              1086
         CALL AXMATERIAL                                                  1086
C    *                                                                    1086
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       1086
         CALL AGSRESET                                                    1087
         AG_SEEN = 1                                                      1087
         AG_COLO = 2                                                      1087
         AG_TITLE = 'SAKM'                                                1087
         AG_EXNAME = 'ATTRIBUTE'                                          1087
         AG_PARLIST = 'SEEN_COLO'                                         1087
         CALL AXATTRIBUTE                                                 1087
C    *                                                                    1087
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   1087
C    DZ=SSUB_KMNTTHK/2 <===                                               1087
         CALL AGSRESET                                                    1089
         AG_RMIN = SSUB_KMOUNTID/2                                        1089
         AG_RMAX = SSUB_KMOUNTOD/2                                        1089
         AG_DZ = SSUB_KMNTTHK/2                                           1089
         AG_TITLE = 'TUBE'                                                1089
         AG_EXNAME = 'SHAPE'                                              1089
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1089
         CALL AXSHAPE                                                     1089
C    *                                                                    1089
C    Create SCKM :                                                        1090
C    Call To Local Procedure SCKM                                         1090
         ASSIGN 6660 TO BLOCK_SCKM                                        1090
         GO TO  6650                                                      1090
6660     CONTINUE                                                         1090
C    *                                                                    1090
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         1091
         CALL AGSRESET                                                    1091
         AG_Y = SSUB_KMCUTOFF                                             1091
         AG_KONLY = 'MANY'                                                1091
         AG_TITLE = 'SCKM'                                                1091
         AG_EXNAME = 'POSITION'                                           1091
         AG_PARLIST = 'Y_KONLY'                                           1091
         CALL AXPOSITION                                                  1091
C    *                                                                    1091
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  1091
C     <===                                                                1091
         CALL AGSRESET                                                    1092
         AG_Y = -SSUB_KMCUTOFF                                            1092
         AG_ALPHAN=AG_ALPHAN+1                                            1092
         AG_ALPHAK(AG_ALPHAN)=3                                           1092
         AG_ALPHAL(AG_ALPHAN)=180                                         1092
         AG_KONLY = 'MANY'                                                1092
         AG_TITLE = 'SCKM'                                                1092
         AG_EXNAME = 'POSITION'                                           1092
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    1092
         CALL AXPOSITION                                                  1092
C    *                                                                    1092
6521  CALL AGSPOPB                                                        1093
      GO TO BLOCK_SAKM, (6520 ,6530)                                      1093
C End of Local Procedure                                                  1093
C ----------------------------------------------------------------------  1094
C ------------ SCKM                                                       1094
C Begin Local Procedure SCKM                                              1095
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      1095
6650  CONTINUE                                                            1095
         AG_TITLE='SCKM'                                                  1095
         AG_MODULE=MODULE                                                 1095
         AG_MARK='V00'                                                    1095
         CALL AGSPUSH                                                     1095
         AG_MODULE=MODULE                                                 1095
C    *                                                                    1095
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1096
         CALL AGSRESET                                                    1096
         AG_TITLE = 'AIR'                                                 1096
         AG_EXNAME = 'MATERIAL'                                           1096
         AG_PARLIST = 'NONE'                                              1096
         CALL AXMATERIAL                                                  1096
C    *                                                                    1096
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       1096
         CALL AGSRESET                                                    1097
         AG_SEEN = 1                                                      1097
         AG_COLO = 1                                                      1097
         AG_TITLE = 'SCKM'                                                1097
         AG_EXNAME = 'ATTRIBUTE'                                          1097
         AG_PARLIST = 'SEEN_COLO'                                         1097
         CALL AXATTRIBUTE                                                 1097
C    *                                                                    1097
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  1097
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    1097
         CALL AGSRESET                                                    1100
         AG_RMIN = SSUB_KMCUTID/2                                         1100
         AG_RMAX = SSUB_KMCUTOD/2                                         1100
         AG_PHI1 = 270-SSUB_KMCUTOA                                       1100
         AG_PHI2 = 270+SSUB_KMCUTOA                                       1100
         AG_DZ = SSUB_KMNTTHK/2                                           1100
         AG_TITLE = 'TUBS'                                                1100
         AG_EXNAME = 'SHAPE'                                              1100
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1100
         CALL AXSHAPE                                                     1100
C    *                                                                    1100
6651  CALL AGSPOPB                                                        1101
      GO TO BLOCK_SCKM, (6650 ,6660)                                      1101
C End of Local Procedure                                                  1101
C ----------------------------------------------------------------------  1102
C ------------ SBSR                                                       1102
C Begin Local Procedure SBSR                                              1103
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   1103
6630  CONTINUE                                                            1103
         AG_TITLE='SBSR'                                                  1103
         AG_MODULE=MODULE                                                 1103
         AG_MARK='V00'                                                    1103
         CALL AGSPUSH                                                     1103
         AG_MODULE=MODULE                                                 1103
C    *                                                                    1103
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1104
         CALL AGSRESET                                                    1104
         AG_TITLE = 'G10'                                                 1104
         AG_EXNAME = 'MATERIAL'                                           1104
         AG_PARLIST = 'NONE'                                              1104
         CALL AXMATERIAL                                                  1104
C    *                                                                    1104
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       1104
         CALL AGSRESET                                                    1105
         AG_SEEN = 1                                                      1105
         AG_COLO = 3                                                      1105
         AG_TITLE = 'SBSR'                                                1105
         AG_EXNAME = 'ATTRIBUTE'                                          1105
         AG_PARLIST = 'SEEN_COLO'                                         1105
         CALL AXATTRIBUTE                                                 1105
C    *                                                                    1105
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  1105
C    =SSUB_SRINGTHK/2 <===                                                1105
         CALL AGSRESET                                                    1107
         AG_RMIN = SSUB_SRINGID/2                                         1107
         AG_RMAX = SSUB_SRINGOD/2                                         1107
         AG_DZ = SSUB_SRINGTHK/2                                          1107
         AG_TITLE = 'TUBE'                                                1107
         AG_EXNAME = 'SHAPE'                                              1107
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1107
         CALL AXSHAPE                                                     1107
C    *                                                                    1107
C    Create SBCR :                                                        1108
C    Call To Local Procedure SBCR                                         1108
         ASSIGN 6680 TO BLOCK_SBCR                                        1108
         GO TO  6670                                                      1108
6680     CONTINUE                                                         1108
C    *                                                                    1108
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             1109
C    *                                                                    1110
         I=-1                                                             1110
            GOTO 6693                                                     1110
6691        I=I+(2)                                                       1110
6693        IF((2)*(I-(1)).GT.0) GO TO 6692                               1110
C       *                                                                 1111
            J=0                                                           1111
               GO TO 6703                                                 1111
6701           J=J+1                                                      1111
6703           IF(J-(1).GT.0) GO TO 6702                                  1111
               PHI=I*SSUB_SRCUTPHI+180*J                                  1112
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  1114
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      1114
               CALL AGSRESET                                              1114
               AG_X = XBUF*SIN(DEGRAD*PHI)                                1114
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               1114
               AG_ALPHAN=AG_ALPHAN+1                                      1114
               AG_ALPHAK(AG_ALPHAN)=3                                     1114
               AG_ALPHAL(AG_ALPHAN)=PHI                                   1114
               AG_KONLY = 'MANY'                                          1114
               AG_TITLE = 'SBCR'                                          1114
               AG_EXNAME = 'POSITION'                                     1114
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            1114
               CALL AXPOSITION                                            1114
C          *                                                              1114
            GO TO 6701                                                    1115
6702        CONTINUE                                                      1115
         GO TO 6691                                                       1116
6692     CONTINUE                                                         1116
6631  CALL AGSPOPB                                                        1117
      GO TO BLOCK_SBSR, (6630 ,6640)                                      1117
C End of Local Procedure                                                  1117
C ----------------------------------------------------------------------  1118
C ------------ SBCR                                                       1118
C Begin Local Procedure SBCR                                              1119
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     1119
6670  CONTINUE                                                            1119
         AG_TITLE='SBCR'                                                  1119
         AG_MODULE=MODULE                                                 1119
         AG_MARK='V00'                                                    1119
         CALL AGSPUSH                                                     1119
         AG_MODULE=MODULE                                                 1119
C    *                                                                    1119
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1120
         CALL AGSRESET                                                    1120
         AG_TITLE = 'AIR'                                                 1120
         AG_EXNAME = 'MATERIAL'                                           1120
         AG_PARLIST = 'NONE'                                              1120
         CALL AXMATERIAL                                                  1120
C    *                                                                    1120
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       1120
         CALL AGSRESET                                                    1121
         AG_SEEN = 1                                                      1121
         AG_COLO = 1                                                      1121
         AG_TITLE = 'SBCR'                                                1121
         AG_EXNAME = 'ATTRIBUTE'                                          1121
         AG_PARLIST = 'SEEN_COLO'                                         1121
         CALL AXATTRIBUTE                                                 1121
C    *                                                                    1121
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  1121
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    1121
         CALL AGSRESET                                                    1123
         AG_DX = SSUB_SRCUTWID/2                                          1123
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           1123
         AG_DZ = SSUB_SRINGTHK/2                                          1123
         AG_TITLE = 'BOX'                                                 1123
         AG_EXNAME = 'SHAPE'                                              1123
         AG_PARLIST = 'DX_DY_DZ'                                          1123
         CALL AXSHAPE                                                     1123
C    *                                                                    1123
6671  CALL AGSPOPB                                                        1124
      GO TO BLOCK_SBCR, (6670 ,6680)                                      1124
C End of Local Procedure                                                  1124
C ----------------------------------------------------------------------  1125
C ------------ SBRL                                                       1125
C Begin Local Procedure SBRL                                              1126
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      1126
6570  CONTINUE                                                            1126
         AG_TITLE='SBRL'                                                  1126
         AG_MODULE=MODULE                                                 1126
         AG_MARK='V00'                                                    1126
         CALL AGSPUSH                                                     1126
         AG_MODULE=MODULE                                                 1126
C    *                                                                    1126
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  1127
C    ADL=12.6 <===                                                        1127
         CALL AGSRESET                                                    1127
         AG_A = 20.719                                                    1127
         AG_Z = 10.307                                                    1127
         AG_DENS = 2.23                                                   1127
         AG_ABSL = 50.7                                                   1127
         AG_RADL = 12.6                                                   1127
         AG_TITLE = 'PYREX'                                               1127
         AG_EXNAME = 'MATERIAL'                                           1127
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                1127
         CALL AXMATERIAL                                                  1127
C    *                                                                    1127
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       1127
         CALL AGSRESET                                                    1128
         AG_SEEN = 1                                                      1128
         AG_COLO = 6                                                      1128
         AG_TITLE = 'SBRL'                                                1128
         AG_EXNAME = 'ATTRIBUTE'                                          1128
         AG_PARLIST = 'SEEN_COLO'                                         1128
         CALL AXATTRIBUTE                                                 1128
C    *                                                                    1128
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  1128
C    =                                                                    1128
         CALL AGSRESET                                                    1129
         AG_RMIN = SSUB_SROLLID/2                                         1129
         AG_RMAX = SSUB_SROLLOD/2                                         1129
         AG_TITLE = 'TUBE'                                                1129
         AG_EXNAME = 'SHAPE'                                              1129
         AG_PARLIST = 'RMIN_RMAX'                                         1129
         CALL AXSHAPE                                                     1129
C    *                                                                    1129
6571  CALL AGSPOPB                                                        1130
      GO TO BLOCK_SBRL, (6570 ,6580)                                      1130
C End of Local Procedure                                                  1130
C ----------------------------------------------------------------------  1131
C ------------ SBRX                                                       1131
C Begin Local Procedure SBRX                                              1132
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 1132
6590  CONTINUE                                                            1132
         AG_TITLE='SBRX'                                                  1132
         AG_MODULE=MODULE                                                 1132
         AG_MARK='V00'                                                    1132
         CALL AGSPUSH                                                     1132
         AG_MODULE=MODULE                                                 1132
C    *                                                                    1132
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1133
         CALL AGSRESET                                                    1133
         AG_TITLE = 'IRON'                                                1133
         AG_EXNAME = 'MATERIAL'                                           1133
         AG_PARLIST = 'NONE'                                              1133
         CALL AXMATERIAL                                                  1133
C    *                                                                    1133
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       1133
         CALL AGSRESET                                                    1134
         AG_SEEN = 1                                                      1134
         AG_COLO = 2                                                      1134
         AG_TITLE = 'SBRX'                                                1134
         AG_EXNAME = 'ATTRIBUTE'                                          1134
         AG_PARLIST = 'SEEN_COLO'                                         1134
         CALL AXATTRIBUTE                                                 1134
C    *                                                                    1134
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  1134
C    LEN/2 <===                                                           1134
         CALL AGSRESET                                                    1136
         AG_RMIN = 0.0                                                    1136
         AG_RMAX = SSUB_SROLLID/2                                         1136
         AG_DZ = SSUB_SWIRELEN/2                                          1136
         AG_TITLE = 'TUBE'                                                1136
         AG_EXNAME = 'SHAPE'                                              1136
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1136
         CALL AXSHAPE                                                     1136
C    *                                                                    1136
6591  CALL AGSPOPB                                                        1137
      GO TO BLOCK_SBRX, (6590 ,6600)                                      1137
C End of Local Procedure                                                  1137
C ----------------------------------------------------------------------  1138
C ------------ SBMM                                                       1138
C Begin Local Procedure SBMM                                              1139
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     1139
6540  CONTINUE                                                            1139
         AG_TITLE='SBMM'                                                  1139
         AG_MODULE=MODULE                                                 1139
         AG_MARK='V00'                                                    1139
         CALL AGSPUSH                                                     1139
         AG_MODULE=MODULE                                                 1139
C    *                                                                    1139
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1140
         CALL AGSRESET                                                    1140
         AG_TITLE = 'AIR'                                                 1140
         AG_EXNAME = 'MATERIAL'                                           1140
         AG_PARLIST = 'NONE'                                              1140
         CALL AXMATERIAL                                                  1140
C    *                                                                    1140
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       1140
         CALL AGSRESET                                                    1141
         AG_SEEN = 0                                                      1141
         AG_COLO = 1                                                      1141
         AG_TITLE = 'SBMM'                                                1141
         AG_EXNAME = 'ATTRIBUTE'                                          1141
         AG_PARLIST = 'SEEN_COLO'                                         1141
         CALL AXATTRIBUTE                                                 1141
C    *                                                                    1141
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  1141
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     1141
         CALL AGSRESET                                                    1143
         AG_DX = SSUB_MBLKIWID/2                                          1143
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           1143
         AG_DZ = SSUB_MBLKHGH/2                                           1143
         AG_TITLE = 'BOX'                                                 1143
         AG_EXNAME = 'SHAPE'                                              1143
         AG_PARLIST = 'DX_DY_DZ'                                          1143
         CALL AXSHAPE                                                     1143
C    *                                                                    1143
C    Create SMRD :                                                        1144
C    Call To Local Procedure SMRD                                         1144
         ASSIGN 6720 TO BLOCK_SMRD                                        1144
         GO TO  6710                                                      1144
6720     CONTINUE                                                         1144
C    *                                                                    1144
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      1145
     *   SSUB_SROLLOD                                                     1145
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1146
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         1146
         CALL AGSRESET                                                    1146
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1146
         AG_ALPHAN=AG_ALPHAN+1                                            1146
         AG_ALPHAK(AG_ALPHAN)=1                                           1146
         AG_ALPHAL(AG_ALPHAN)=90                                          1146
         AG_TITLE = 'SMRD'                                                1146
         AG_EXNAME = 'POSITION'                                           1146
         AG_PARLIST = 'Y_ALPHAX'                                          1146
         CALL AXPOSITION                                                  1146
C    *                                                                    1146
C    Create SBMO :                                                        1147
C    Call To Local Procedure SBMO                                         1147
         ASSIGN 6740 TO BLOCK_SBMO                                        1147
         GO TO  6730                                                      1147
6740     CONTINUE                                                         1147
C    *                                                                    1147
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1148
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1149
C    UB_SROLLOD)/2 <===                                                   1149
         CALL AGSRESET                                                    1149
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1149
         AG_TITLE = 'SBMO'                                                1149
         AG_EXNAME = 'POSITION'                                           1149
         AG_PARLIST = 'Y'                                                 1149
         CALL AXPOSITION                                                  1149
C    *                                                                    1149
C    Create SBMI :                                                        1150
C    Call To Local Procedure SBMI                                         1150
         ASSIGN 6760 TO BLOCK_SBMI                                        1150
         GO TO  6750                                                      1150
6760     CONTINUE                                                         1150
C    *                                                                    1150
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             1151
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  1152
C    UB_SROLLOD)/2 <===                                                   1152
         CALL AGSRESET                                                    1152
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       1152
         AG_TITLE = 'SBMI'                                                1152
         AG_EXNAME = 'POSITION'                                           1152
         AG_PARLIST = 'Y'                                                 1152
         CALL AXPOSITION                                                  1152
C    *                                                                    1152
6541  CALL AGSPOPB                                                        1153
      GO TO BLOCK_SBMM, (6540 ,6550,6560)                                 1153
C End of Local Procedure                                                  1153
C ----------------------------------------------------------------------  1154
C ------------ SMRD                                                       1154
C Begin Local Procedure SMRD                                              1155
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  1155
6710  CONTINUE                                                            1155
         AG_TITLE='SMRD'                                                  1155
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
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       1156
         CALL AGSRESET                                                    1157
         AG_SEEN = 1                                                      1157
         AG_COLO = 2                                                      1157
         AG_TITLE = 'SMRD'                                                1157
         AG_EXNAME = 'ATTRIBUTE'                                          1157
         AG_PARLIST = 'SEEN_COLO'                                         1157
         CALL AXATTRIBUTE                                                 1157
C    *                                                                    1157
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  1157
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             1157
         CALL AGSRESET                                                    1159
         AG_RMIN = 0.0                                                    1159
         AG_RMAX = SSUB_MRODDIA/2                                         1159
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            1159
         AG_TITLE = 'TUBE'                                                1159
         AG_EXNAME = 'SHAPE'                                              1159
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1159
         CALL AXSHAPE                                                     1159
C    *                                                                    1159
6711  CALL AGSPOPB                                                        1160
      GO TO BLOCK_SMRD, (6710 ,6720)                                      1160
C End of Local Procedure                                                  1160
C ----------------------------------------------------------------------  1161
C ------------ SBMO                                                       1161
C Begin Local Procedure SBMO                                              1162
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       1162
6730  CONTINUE                                                            1162
         AG_TITLE='SBMO'                                                  1162
         AG_MODULE=MODULE                                                 1162
         AG_MARK='V00'                                                    1162
         CALL AGSPUSH                                                     1162
         AG_MODULE=MODULE                                                 1162
C    *                                                                    1162
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1163
         CALL AGSRESET                                                    1163
         AG_TITLE = 'G10'                                                 1163
         AG_EXNAME = 'MATERIAL'                                           1163
         AG_PARLIST = 'NONE'                                              1163
         CALL AXMATERIAL                                                  1163
C    *                                                                    1163
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1163
         CALL AGSRESET                                                    1164
         AG_SEEN = 1                                                      1164
         AG_COLO = 3                                                      1164
         AG_TITLE = 'SBMO'                                                1164
         AG_EXNAME = 'ATTRIBUTE'                                          1164
         AG_PARLIST = 'SEEN_COLO'                                         1164
         CALL AXATTRIBUTE                                                 1164
C    *                                                                    1164
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  1164
C    UB_MBLKHGH/2 <===                                                    1164
         CALL AGSRESET                                                    1166
         AG_DX = SSUB_MBLKOWID/2                                          1166
         AG_DY = SSUB_MBLKOLEN/2                                          1166
         AG_DZ = SSUB_MBLKHGH/2                                           1166
         AG_TITLE = 'BOX'                                                 1166
         AG_EXNAME = 'SHAPE'                                              1166
         AG_PARLIST = 'DX_DY_DZ'                                          1166
         CALL AXSHAPE                                                     1166
C    *                                                                    1166
6731  CALL AGSPOPB                                                        1167
      GO TO BLOCK_SBMO, (6730 ,6740)                                      1167
C End of Local Procedure                                                  1167
C ----------------------------------------------------------------------  1168
C ------------ SBMI                                                       1168
C Begin Local Procedure SBMI                                              1169
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       1169
6750  CONTINUE                                                            1169
         AG_TITLE='SBMI'                                                  1169
         AG_MODULE=MODULE                                                 1169
         AG_MARK='V00'                                                    1169
         CALL AGSPUSH                                                     1169
         AG_MODULE=MODULE                                                 1169
C    *                                                                    1169
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1170
         CALL AGSRESET                                                    1170
         AG_TITLE = 'G10'                                                 1170
         AG_EXNAME = 'MATERIAL'                                           1170
         AG_PARLIST = 'NONE'                                              1170
         CALL AXMATERIAL                                                  1170
C    *                                                                    1170
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       1170
         CALL AGSRESET                                                    1171
         AG_SEEN = 1                                                      1171
         AG_COLO = 3                                                      1171
         AG_TITLE = 'SBMO'                                                1171
         AG_EXNAME = 'ATTRIBUTE'                                          1171
         AG_PARLIST = 'SEEN_COLO'                                         1171
         CALL AXATTRIBUTE                                                 1171
C    *                                                                    1171
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  1171
C    UB_MBLKHGH/2 <===                                                    1171
         CALL AGSRESET                                                    1173
         AG_DX = SSUB_MBLKIWID/2                                          1173
         AG_DY = SSUB_MBLKILEN/2                                          1173
         AG_DZ = SSUB_MBLKHGH/2                                           1173
         AG_TITLE = 'BOX'                                                 1173
         AG_EXNAME = 'SHAPE'                                              1173
         AG_PARLIST = 'DX_DY_DZ'                                          1173
         CALL AXSHAPE                                                     1173
C    *                                                                    1173
6751  CALL AGSPOPB                                                        1174
      GO TO BLOCK_SBMI, (6750 ,6760)                                      1174
C End of Local Procedure                                                  1174
C ----------------------------------------------------------------------  1175
C ------------ SALM                                                       1175
C Begin Local Procedure SALM                                              1176
C SALM IS THE ALUMINUM SHIELD MESH                                        1176
5370  CONTINUE                                                            1176
         AG_TITLE='SALM'                                                  1176
         AG_MODULE=MODULE                                                 1176
         AG_MARK='V00'                                                    1176
         CALL AGSPUSH                                                     1176
         AG_MODULE=MODULE                                                 1176
C    *                                                                    1176
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1177
         CALL AGSRESET                                                    1177
         AG_TITLE = 'ALUMINIUM'                                           1177
         AG_EXNAME = 'MATERIAL'                                           1177
         AG_PARLIST = 'NONE'                                              1177
         CALL AXMATERIAL                                                  1177
C    *                                                                    1177
C    Geant ATTRIBUTE : SALM ===> SEEN=1 COLO=2 <===                       1177
         CALL AGSRESET                                                    1178
         AG_SEEN = 1                                                      1178
         AG_COLO = 2                                                      1178
         AG_TITLE = 'SALM'                                                1178
         AG_EXNAME = 'ATTRIBUTE'                                          1178
         AG_PARLIST = 'SEEN_COLO'                                         1178
         CALL AXATTRIBUTE                                                 1178
C    *                                                                    1178
C    Geant SHAPE : TUBE ===> RMIN=SSLD_ALMESHID/2 RMAX=SSLD_ALMESHOD/2,   1178
C    DZ=SSLD_ALMSHTHK/2 <===                                              1178
         CALL AGSRESET                                                    1180
         AG_RMIN = SSLD_ALMESHID/2                                        1180
         AG_RMAX = SSLD_ALMESHOD/2                                        1180
         AG_DZ = SSLD_ALMSHTHK/2                                          1180
         AG_TITLE = 'TUBE'                                                1180
         AG_EXNAME = 'SHAPE'                                              1180
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1180
         CALL AXSHAPE                                                     1180
C    *                                                                    1180
5371  CALL AGSPOPB                                                        1181
      GO TO BLOCK_SALM, (5370 ,5380)                                      1181
C End of Local Procedure                                                  1181
C ----------------------------------------------------------------------  1182
C ------------ SISH                                                       1182
C Begin Local Procedure SISH                                              1183
C SISH IS THE INNER SHIELD CYLINDER                                       1183
5310  CONTINUE                                                            1183
         AG_TITLE='SISH'                                                  1183
         AG_MODULE=MODULE                                                 1183
         AG_MARK='V00'                                                    1183
         CALL AGSPUSH                                                     1183
         AG_MODULE=MODULE                                                 1183
C    *                                                                    1183
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            1184
         CALL AGSRESET                                                    1184
         AG_TITLE = 'ALKAP'                                               1184
         AG_EXNAME = 'MATERIAL'                                           1184
         AG_PARLIST = 'NONE'                                              1184
         CALL AXMATERIAL                                                  1184
C    *                                                                    1184
C    Geant ATTRIBUTE : SISH ===> SEEN=1 COLO=3 <===                       1184
         CALL AGSRESET                                                    1185
         AG_SEEN = 1                                                      1185
         AG_COLO = 3                                                      1185
         AG_TITLE = 'SISH'                                                1185
         AG_EXNAME = 'ATTRIBUTE'                                          1185
         AG_PARLIST = 'SEEN_COLO'                                         1185
         CALL AXATTRIBUTE                                                 1185
C    *                                                                    1185
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SINRINN RMAX=SSLD_SINROUT, DZ=SSL  1185
C    D_SINLEN <===                                                        1185
         CALL AGSRESET                                                    1187
         AG_RMIN = SSLD_SINRINN                                           1187
         AG_RMAX = SSLD_SINROUT                                           1187
         AG_DZ = SSLD_SINLEN                                              1187
         AG_TITLE = 'TUBE'                                                1187
         AG_EXNAME = 'SHAPE'                                              1187
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1187
         CALL AXSHAPE                                                     1187
C    *                                                                    1187
5311  CALL AGSPOPB                                                        1188
      GO TO BLOCK_SISH, (5310 ,5320)                                      1188
C End of Local Procedure                                                  1188
C ----------------------------------------------------------------------  1189
C ------------ SSSH                                                       1189
C Begin Local Procedure SSSH                                              1190
C SSSH IS THE SEPARATION SHIELD CYLINDER                                  1190
5350  CONTINUE                                                            1190
         AG_TITLE='SSSH'                                                  1190
         AG_MODULE=MODULE                                                 1190
         AG_MARK='V00'                                                    1190
         CALL AGSPUSH                                                     1190
         AG_MODULE=MODULE                                                 1190
C    *                                                                    1190
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1191
         CALL AGSRESET                                                    1191
         AG_TITLE = 'SSDALMY'                                             1191
         AG_EXNAME = 'MATERIAL'                                           1191
         AG_PARLIST = 'NONE'                                              1191
         CALL AXMATERIAL                                                  1191
C    *                                                                    1191
C    Geant ATTRIBUTE : SSSH ===> SEEN=1 COLO=3 <===                       1191
         CALL AGSRESET                                                    1192
         AG_SEEN = 1                                                      1192
         AG_COLO = 3                                                      1192
         AG_TITLE = 'SSSH'                                                1192
         AG_EXNAME = 'ATTRIBUTE'                                          1192
         AG_PARLIST = 'SEEN_COLO'                                         1192
         CALL AXATTRIBUTE                                                 1192
C    *                                                                    1192
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SSEPRINN RMAX=SSLD_SSEPROUT, DZ=S  1192
C    SLD_SSEPLEN <===                                                     1192
         CALL AGSRESET                                                    1194
         AG_RMIN = SSLD_SSEPRINN                                          1194
         AG_RMAX = SSLD_SSEPROUT                                          1194
         AG_DZ = SSLD_SSEPLEN                                             1194
         AG_TITLE = 'TUBE'                                                1194
         AG_EXNAME = 'SHAPE'                                              1194
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1194
         CALL AXSHAPE                                                     1194
C    *                                                                    1194
5351  CALL AGSPOPB                                                        1195
      GO TO BLOCK_SSSH, (5350 ,5360)                                      1195
C End of Local Procedure                                                  1195
C ----------------------------------------------------------------------  1196
C ------------ SOSH                                                       1196
C Begin Local Procedure SOSH                                              1197
C SOSH IS THE SEPARATION SHIELD CYLINDER                                  1197
5330  CONTINUE                                                            1197
         AG_TITLE='SOSH'                                                  1197
         AG_MODULE=MODULE                                                 1197
         AG_MARK='V00'                                                    1197
         CALL AGSPUSH                                                     1197
         AG_MODULE=MODULE                                                 1197
C    *                                                                    1197
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1198
         CALL AGSRESET                                                    1198
         AG_TITLE = 'SSDALMY'                                             1198
         AG_EXNAME = 'MATERIAL'                                           1198
         AG_PARLIST = 'NONE'                                              1198
         CALL AXMATERIAL                                                  1198
C    *                                                                    1198
C    Geant ATTRIBUTE : SOSH ===> SEEN=1 COLO=3 <===                       1198
         CALL AGSRESET                                                    1199
         AG_SEEN = 1                                                      1199
         AG_COLO = 3                                                      1199
         AG_TITLE = 'SOSH'                                                1199
         AG_EXNAME = 'ATTRIBUTE'                                          1199
         AG_PARLIST = 'SEEN_COLO'                                         1199
         CALL AXATTRIBUTE                                                 1199
C    *                                                                    1199
C    Geant SHAPE : TUBE ===> RMIN=SSLD_SOUTRINN RMAX=SSLD_SOUTROUT, DZ=S  1199
C    SLD_SOUTLEN <===                                                     1199
         CALL AGSRESET                                                    1201
         AG_RMIN = SSLD_SOUTRINN                                          1201
         AG_RMAX = SSLD_SOUTROUT                                          1201
         AG_DZ = SSLD_SOUTLEN                                             1201
         AG_TITLE = 'TUBE'                                                1201
         AG_EXNAME = 'SHAPE'                                              1201
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1201
         CALL AXSHAPE                                                     1201
C    *                                                                    1201
5331  CALL AGSPOPB                                                        1202
      GO TO BLOCK_SOSH, (5330 ,5340)                                      1202
C End of Local Procedure                                                  1202
      WRITE(LOUT,6770)'===>>  [End AgML Module SVTTGEO1]  <<==='          1202
6770  FORMAT(A32,/,/)                                                     1202
C MODULE SVTTGEO1                                                         1204
      END                                                                 1204
