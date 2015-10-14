C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO9                                                 2
C SVTTGEO9 IS THE SVT-LESS GEOMETRY OF THE SUPPORT CONES OF STAR WITHOUT  2
C , THE SVT                                                               2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'SVTTGEO9'/, BANK_TITLE/'*'/                            2
      DATA MCOMMENT/'IS THE SVT-LESS GEOMETRY OF THE SUPPORT CONES OF ST  2
     *AR WITHOUT, THE SVT'/                                               2
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
C CREATED 12 JULY 2007                                                    3
      CHARACTER*40 CREATED /'12 JULY 2007'/                               4
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
      INTEGER BLOCK_SCON                                                  9
      INTEGER BLOCK_SGRA                                                  9
      INTEGER BLOCK_SBRG                                                  9
      INTEGER BLOCK_SOES                                                  9
      INTEGER BLOCK_SOSM                                                  9
      INTEGER BLOCK_SCRW                                                  9
      INTEGER BLOCK_SIES                                                  9
      INTEGER BLOCK_SISM                                                  9
      INTEGER BLOCK_SBSP                                                  9
      INTEGER BLOCK_SAKM                                                  9
      INTEGER BLOCK_SBMM                                                  9
      INTEGER BLOCK_SMRD                                                  9
      INTEGER BLOCK_SBMO                                                  9
      INTEGER BLOCK_SBMI                                                  9
      INTEGER BLOCK_SCKM                                                  9
      INTEGER BLOCK_SBRL                                                  9
      INTEGER BLOCK_SBRX                                                  9
      INTEGER BLOCK_SBSR                                                  9
      INTEGER BLOCK_SBCR                                                  9
      INTEGER BLOCK_SCMY                                                  9
C -                                                                       11
      REAL SVTG_VERSION                                                   11
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            11
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       11
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          11
     *MSVTG_VERSION)                                                      11
      CHARACTER CSVTG_VERSION*40/' '/                                     11
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             11
      INTEGER MSVTG_VERSION(2)/1,1/                                       11
      REAL RE_SVTG_VERSION                                                11
C -                                                                       11
      INTEGER SVTG_NLAYER                                                 11
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              11
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        11
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           11
     *MSVTG_NLAYER)                                                       11
      CHARACTER CSVTG_NLAYER*40/' '/                                      11
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               11
      INTEGER MSVTG_NLAYER(2)/1,1/                                        11
      REAL RE_SVTG_NLAYER                                                 11
C -                                                                       11
      REAL SVTG_RSIZEMIN                                                  11
      EQUIVALENCE (BANK_SVTG(5),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          11
     *(NAMES_SVTG(3),NSVTG_RSIZEMIN)                                      11
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_RSIZEMIN),(MAP_SVTG(1,3),         11
     *MSVTG_RSIZEMIN)                                                     11
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    11
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           11
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      11
      REAL RE_SVTG_RSIZEMIN                                               11
C -                                                                       11
      REAL SVTG_RSIZEMAX                                                  11
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          11
     *(NAMES_SVTG(4),NSVTG_RSIZEMAX)                                      11
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMAX),(MAP_SVTG(1,4),         11
     *MSVTG_RSIZEMAX)                                                     11
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    11
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           11
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      11
      REAL RE_SVTG_RSIZEMAX                                               11
C -                                                                       11
      REAL SVTG_ZSIZEMAX                                                  11
      EQUIVALENCE (BANK_SVTG(7),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          11
     *(NAMES_SVTG(5),NSVTG_ZSIZEMAX)                                      11
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_ZSIZEMAX),(MAP_SVTG(1,5),         11
     *MSVTG_ZSIZEMAX)                                                     11
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    11
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           11
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      11
      REAL RE_SVTG_ZSIZEMAX                                               11
C -                                                                       11
      REAL SVTG_ANGOFF                                                    11
      EQUIVALENCE (BANK_SVTG(8),SVTG_ANGOFF,RE_SVTG_ANGOFF),              11
     *(NAMES_SVTG(6),NSVTG_ANGOFF)                                        11
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ANGOFF),(MAP_SVTG(1,6),           11
     *MSVTG_ANGOFF)                                                       11
      CHARACTER CSVTG_ANGOFF*40/' '/                                      11
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               11
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        11
      REAL RE_SVTG_ANGOFF                                                 11
C -                                                                       11
      REAL SVTG_SUPPORTVER                                                11
      EQUIVALENCE (BANK_SVTG(9),SVTG_SUPPORTVER,RE_SVTG_SUPPORTVER),      11
     *(NAMES_SVTG(7),NSVTG_SUPPORTVER)                                    11
      EQUIVALENCE (COMMEN_SVTG(7),CSVTG_SUPPORTVER),(MAP_SVTG(1,7),       11
     *MSVTG_SUPPORTVER)                                                   11
      CHARACTER CSVTG_SUPPORTVER*40/' '/                                  11
      CHARACTER NSVTG_SUPPORTVER*12/'R.SUPPORTVER'/                       11
      INTEGER MSVTG_SUPPORTVER(2)/1,1/                                    11
      REAL RE_SVTG_SUPPORTVER                                             11
C -                                                                       11
      REAL SVTG_CONEVER                                                   11
      EQUIVALENCE (BANK_SVTG(10),SVTG_CONEVER,RE_SVTG_CONEVER),           11
     *(NAMES_SVTG(8),NSVTG_CONEVER)                                       11
      EQUIVALENCE (COMMEN_SVTG(8),CSVTG_CONEVER),(MAP_SVTG(1,8),          11
     *MSVTG_CONEVER)                                                      11
      CHARACTER CSVTG_CONEVER*40/' '/                                     11
      CHARACTER NSVTG_CONEVER*12/'R.CONEVER'/                             11
      INTEGER MSVTG_CONEVER(2)/1,1/                                       11
      REAL RE_SVTG_CONEVER                                                11
C -                                                                       11
      REAL SVTG_IFMANY                                                    11
      EQUIVALENCE (BANK_SVTG(11),SVTG_IFMANY,RE_SVTG_IFMANY),             11
     *(NAMES_SVTG(9),NSVTG_IFMANY)                                        11
      EQUIVALENCE (COMMEN_SVTG(9),CSVTG_IFMANY),(MAP_SVTG(1,9),           11
     *MSVTG_IFMANY)                                                       11
      CHARACTER CSVTG_IFMANY*40/' '/                                      11
      CHARACTER NSVTG_IFMANY*12/'R.IFMANY'/                               11
      INTEGER MSVTG_IFMANY(2)/1,1/                                        11
      REAL RE_SVTG_IFMANY                                                 11
C -                                                                       11
      REAL SVTG_NMIN                                                      11
      EQUIVALENCE (BANK_SVTG(12),SVTG_NMIN,RE_SVTG_NMIN),                 11
     *(NAMES_SVTG(10),NSVTG_NMIN)                                         11
      EQUIVALENCE (COMMEN_SVTG(10),CSVTG_NMIN),(MAP_SVTG(1,10),           11
     *MSVTG_NMIN)                                                         11
      CHARACTER CSVTG_NMIN*40/' '/                                        11
      CHARACTER NSVTG_NMIN*12/'R.NMIN'/                                   11
      INTEGER MSVTG_NMIN(2)/1,1/                                          11
      REAL RE_SVTG_NMIN                                                   11
C -                                                                       11
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                11
      PARAMETER (LEN_SVTG=12)                                             11
      PARAMETER (LLEN_SVTG=10)                                            11
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           11
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        11
      DATA IDNUM_SVTG/10*0/                                               11
      REAL BANK_SVTG(LEN_SVTG)                                            11
      CHARACTER*60 BPATH_SVTG/' '/                                        11
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          11
     *BPATH_SVTG                                                          11
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             11
C *                                                                       11
C -                                                                       13
      REAL SWCA_VERSION                                                   13
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            13
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       13
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          13
     *MSWCA_VERSION)                                                      13
      CHARACTER CSWCA_VERSION*40/' '/                                     13
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             13
      INTEGER MSWCA_VERSION(2)/1,1/                                       13
      REAL RE_SWCA_VERSION                                                13
C -                                                                       13
      REAL SWCA_LENGTH                                                    13
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              13
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        13
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           13
     *MSWCA_LENGTH)                                                       13
      CHARACTER CSWCA_LENGTH*40/' '/                                      13
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               13
      INTEGER MSWCA_LENGTH(2)/1,1/                                        13
      REAL RE_SWCA_LENGTH                                                 13
C -                                                                       13
      REAL SWCA_WAFERWID                                                  13
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          13
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      13
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         13
     *MSWCA_WAFERWID)                                                     13
      CHARACTER CSWCA_WAFERWID*40/' '/                                    13
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           13
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      13
      REAL RE_SWCA_WAFERWID                                               13
C -                                                                       13
      REAL SWCA_WAFERLEN                                                  13
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          13
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      13
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         13
     *MSWCA_WAFERLEN)                                                     13
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    13
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           13
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      13
      REAL RE_SWCA_WAFERLEN                                               13
C -                                                                       13
      REAL SWCA_WAFERTHK                                                  13
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          13
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      13
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         13
     *MSWCA_WAFERTHK)                                                     13
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    13
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           13
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      13
      REAL RE_SWCA_WAFERTHK                                               13
C -                                                                       13
      REAL SWCA_ROHATHK                                                   13
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            13
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       13
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          13
     *MSWCA_ROHATHK)                                                      13
      CHARACTER CSWCA_ROHATHK*40/' '/                                     13
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             13
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       13
      REAL RE_SWCA_ROHATHK                                                13
C -                                                                       13
      REAL SWCA_WAFCARWD                                                  13
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          13
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      13
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         13
     *MSWCA_WAFCARWD)                                                     13
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    13
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           13
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      13
      REAL RE_SWCA_WAFCARWD                                               13
C -                                                                       13
      REAL SWCA_WAFCARTH                                                  13
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         13
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      13
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         13
     *MSWCA_WAFCARTH)                                                     13
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    13
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           13
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      13
      REAL RE_SWCA_WAFCARTH                                               13
C -                                                                       13
      REAL SWCA_WAFERGAP                                                  13
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         13
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      13
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         13
     *MSWCA_WAFERGAP)                                                     13
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    13
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           13
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      13
      REAL RE_SWCA_WAFERGAP                                               13
C -                                                                       13
      REAL SWCA_DRIFT                                                     13
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               13
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        13
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          13
     *MSWCA_DRIFT)                                                        13
      CHARACTER CSWCA_DRIFT*40/' '/                                       13
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 13
      INTEGER MSWCA_DRIFT(2)/1,1/                                         13
      REAL RE_SWCA_DRIFT                                                  13
C -                                                                       13
      REAL SWCA_STRUTLEN                                                  13
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         13
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     13
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       13
     *MSWCA_STRUTLEN)                                                     13
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    13
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           13
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      13
      REAL RE_SWCA_STRUTLEN                                               13
C -                                                                       13
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                13
      PARAMETER (LEN_SWCA=13)                                             13
      PARAMETER (LLEN_SWCA=11)                                            13
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           13
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        13
      DATA IDNUM_SWCA/10*0/                                               13
      REAL BANK_SWCA(LEN_SWCA)                                            13
      CHARACTER*60 BPATH_SWCA/' '/                                        13
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          13
     *BPATH_SWCA                                                          13
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             13
C *                                                                       13
C -                                                                       16
      REAL SSUP_VERSION                                                   16
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            16
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       16
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          16
     *MSSUP_VERSION)                                                      16
      CHARACTER CSSUP_VERSION*40/' '/                                     16
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             16
      INTEGER MSSUP_VERSION(2)/1,1/                                       16
      REAL RE_SSUP_VERSION                                                16
C -                                                                       16
      REAL SSUP_CABTHK                                                    16
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              16
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        16
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           16
     *MSSUP_CABTHK)                                                       16
      CHARACTER CSSUP_CABTHK*40/' '/                                      16
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               16
      INTEGER MSSUP_CABTHK(2)/1,1/                                        16
      REAL RE_SSUP_CABTHK                                                 16
C -                                                                       16
      REAL SSUP_HOSRMN                                                    16
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              16
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        16
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           16
     *MSSUP_HOSRMN)                                                       16
      CHARACTER CSSUP_HOSRMN*40/' '/                                      16
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               16
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        16
      REAL RE_SSUP_HOSRMN                                                 16
C -                                                                       16
      REAL SSUP_HOSRMX                                                    16
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              16
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        16
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           16
     *MSSUP_HOSRMX)                                                       16
      CHARACTER CSSUP_HOSRMX*40/' '/                                      16
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               16
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        16
      REAL RE_SSUP_HOSRMX                                                 16
C -                                                                       16
      REAL SSUP_NHOSES                                                    16
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              16
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        16
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           16
     *MSSUP_NHOSES)                                                       16
      CHARACTER CSSUP_NHOSES*40/' '/                                      16
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               16
      INTEGER MSSUP_NHOSES(2)/1,1/                                        16
      REAL RE_SSUP_NHOSES                                                 16
C -                                                                       16
      REAL SSUP_WRPMYTHK                                                  16
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          16
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      16
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         16
     *MSSUP_WRPMYTHK)                                                     16
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    16
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           16
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      16
      REAL RE_SSUP_WRPMYTHK                                               16
C -                                                                       16
      REAL SSUP_WRPALTHK                                                  16
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          16
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      16
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         16
     *MSSUP_WRPALTHK)                                                     16
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    16
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           16
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      16
      REAL RE_SSUP_WRPALTHK                                               16
C -                                                                       16
      REAL SSUP_GRPHTHK                                                   16
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           16
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       16
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          16
     *MSSUP_GRPHTHK)                                                      16
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     16
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             16
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       16
      REAL RE_SSUP_GRPHTHK                                                16
C -                                                                       16
      REAL SSUP_CONE1ZMN                                                  16
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         16
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      16
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         16
     *MSSUP_CONE1ZMN)                                                     16
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    16
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           16
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      16
      REAL RE_SSUP_CONE1ZMN                                               16
C -                                                                       16
      REAL SSUP_RODLEN                                                    16
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             16
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       16
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         16
     *MSSUP_RODLEN)                                                       16
      CHARACTER CSSUP_RODLEN*40/' '/                                      16
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               16
      INTEGER MSSUP_RODLEN(2)/1,1/                                        16
      REAL RE_SSUP_RODLEN                                                 16
C -                                                                       16
      REAL SSUP_RODDIST                                                   16
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           16
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      16
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        16
     *MSSUP_RODDIST)                                                      16
      CHARACTER CSSUP_RODDIST*40/' '/                                     16
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             16
      INTEGER MSSUP_RODDIST(2)/1,1/                                       16
      REAL RE_SSUP_RODDIST                                                16
C -                                                                       16
      REAL SSUP_RODID                                                     16
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               16
     *(NAMES_SSUP(12),NSSUP_RODID)                                        16
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          16
     *MSSUP_RODID)                                                        16
      CHARACTER CSSUP_RODID*40/' '/                                       16
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 16
      INTEGER MSSUP_RODID(2)/1,1/                                         16
      REAL RE_SSUP_RODID                                                  16
C -                                                                       16
      REAL SSUP_RODOD                                                     16
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               16
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        16
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          16
     *MSSUP_RODOD)                                                        16
      CHARACTER CSSUP_RODOD*40/' '/                                       16
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 16
      INTEGER MSSUP_RODOD(2)/1,1/                                         16
      REAL RE_SSUP_RODOD                                                  16
C -                                                                       16
      REAL SSUP_CON1IDMN                                                  16
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         16
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     16
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       16
     *MSSUP_CON1IDMN)                                                     16
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    16
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           16
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      16
      REAL RE_SSUP_CON1IDMN                                               16
C -                                                                       16
      REAL SSUP_CON3IDMN                                                  16
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         16
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     16
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       16
     *MSSUP_CON3IDMN)                                                     16
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    16
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           16
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      16
      REAL RE_SSUP_CON3IDMN                                               16
C -                                                                       16
      REAL SSUP_CON4IDMN                                                  16
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         16
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     16
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       16
     *MSSUP_CON4IDMN)                                                     16
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    16
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           16
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      16
      REAL RE_SSUP_CON4IDMN                                               16
C -                                                                       16
      REAL SSUP_CON4IDMX                                                  16
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         16
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     16
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       16
     *MSSUP_CON4IDMX)                                                     16
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    16
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           16
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      16
      REAL RE_SSUP_CON4IDMX                                               16
C -                                                                       16
      REAL SSUP_CONE3ZMX                                                  16
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         16
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     16
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       16
     *MSSUP_CONE3ZMX)                                                     16
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    16
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           16
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      16
      REAL RE_SSUP_CONE3ZMX                                               16
C -                                                                       16
      REAL SSUP_CONE4ZMX                                                  16
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         16
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     16
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       16
     *MSSUP_CONE4ZMX)                                                     16
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    16
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           16
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      16
      REAL RE_SSUP_CONE4ZMX                                               16
C -                                                                       16
      REAL SSUP_BRATHK                                                    16
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             16
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       16
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         16
     *MSSUP_BRATHK)                                                       16
      CHARACTER CSSUP_BRATHK*40/' '/                                      16
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               16
      INTEGER MSSUP_BRATHK(2)/1,1/                                        16
      REAL RE_SSUP_BRATHK                                                 16
C -                                                                       16
      REAL SSUP_ERJTHK                                                    16
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             16
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       16
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         16
     *MSSUP_ERJTHK)                                                       16
      CHARACTER CSSUP_ERJTHK*40/' '/                                      16
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               16
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        16
      REAL RE_SSUP_ERJTHK                                                 16
C -                                                                       16
      REAL SSUP_ERJWID                                                    16
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             16
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       16
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         16
     *MSSUP_ERJWID)                                                       16
      CHARACTER CSSUP_ERJWID*40/' '/                                      16
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               16
      INTEGER MSSUP_ERJWID(2)/1,1/                                        16
      REAL RE_SSUP_ERJWID                                                 16
C -                                                                       16
      REAL SSUP_ERJLEN                                                    16
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             16
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       16
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         16
     *MSSUP_ERJLEN)                                                       16
      CHARACTER CSSUP_ERJLEN*40/' '/                                      16
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               16
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        16
      REAL RE_SSUP_ERJLEN                                                 16
C -                                                                       16
      REAL SSUP_ERJZDIS                                                   16
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           16
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      16
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        16
     *MSSUP_ERJZDIS)                                                      16
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     16
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             16
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       16
      REAL RE_SSUP_ERJZDIS                                                16
C -                                                                       16
      REAL SSUP_ERJ1X                                                     16
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               16
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        16
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          16
     *MSSUP_ERJ1X)                                                        16
      CHARACTER CSSUP_ERJ1X*40/' '/                                       16
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 16
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         16
      REAL RE_SSUP_ERJ1X                                                  16
C -                                                                       16
      REAL SSUP_ERJ2X                                                     16
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               16
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        16
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          16
     *MSSUP_ERJ2X)                                                        16
      CHARACTER CSSUP_ERJ2X*40/' '/                                       16
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 16
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         16
      REAL RE_SSUP_ERJ2X                                                  16
C -                                                                       16
      REAL SSUP_ERJ2Y                                                     16
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               16
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        16
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          16
     *MSSUP_ERJ2Y)                                                        16
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       16
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 16
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         16
      REAL RE_SSUP_ERJ2Y                                                  16
C -                                                                       16
      REAL SSUP_ERJRAD                                                    16
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             16
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       16
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         16
     *MSSUP_ERJRAD)                                                       16
      CHARACTER CSSUP_ERJRAD*40/' '/                                      16
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               16
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        16
      REAL RE_SSUP_ERJRAD                                                 16
C -                                                                       16
      REAL SSUP_ERJDIA                                                    16
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             16
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       16
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         16
     *MSSUP_ERJDIA)                                                       16
      CHARACTER CSSUP_ERJDIA*40/' '/                                      16
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               16
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        16
      REAL RE_SSUP_ERJDIA                                                 16
C -                                                                       16
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                16
      PARAMETER (LEN_SSUP=31)                                             16
      PARAMETER (LLEN_SSUP=29)                                            16
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           16
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        16
      DATA IDNUM_SSUP/10*0/                                               16
      REAL BANK_SSUP(LEN_SSUP)                                            16
      CHARACTER*60 BPATH_SSUP/' '/                                        16
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          16
     *BPATH_SSUP                                                          16
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             16
C *                                                                       16
C -                                                                       20
      REAL SSUB_VERSION                                                   20
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            20
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       20
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          20
     *MSSUB_VERSION)                                                      20
      CHARACTER CSSUB_VERSION*40/' '/                                     20
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             20
      INTEGER MSSUB_VERSION(2)/1,1/                                       20
      REAL RE_SSUB_VERSION                                                20
C -                                                                       20
      REAL SSUB_KMOUNTID                                                  20
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          20
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      20
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         20
     *MSSUB_KMOUNTID)                                                     20
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    20
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           20
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      20
      REAL RE_SSUB_KMOUNTID                                               20
C -                                                                       20
      REAL SSUB_KMOUNTOD                                                  20
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          20
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      20
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         20
     *MSSUB_KMOUNTOD)                                                     20
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    20
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           20
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      20
      REAL RE_SSUB_KMOUNTOD                                               20
C -                                                                       20
      REAL SSUB_KMNTTHK                                                   20
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            20
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       20
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          20
     *MSSUB_KMNTTHK)                                                      20
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     20
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             20
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       20
      REAL RE_SSUB_KMNTTHK                                                20
C -                                                                       20
      REAL SSUB_KMCUTOD                                                   20
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            20
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       20
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          20
     *MSSUB_KMCUTOD)                                                      20
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     20
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             20
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       20
      REAL RE_SSUB_KMCUTOD                                                20
C -                                                                       20
      REAL SSUB_KMCUTID                                                   20
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            20
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       20
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          20
     *MSSUB_KMCUTID)                                                      20
      CHARACTER CSSUB_KMCUTID*40/' '/                                     20
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             20
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       20
      REAL RE_SSUB_KMCUTID                                                20
C -                                                                       20
      REAL SSUB_KMCUTOA                                                   20
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            20
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       20
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          20
     *MSSUB_KMCUTOA)                                                      20
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     20
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             20
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       20
      REAL RE_SSUB_KMCUTOA                                                20
C -                                                                       20
      REAL SSUB_KMCUTOFF                                                  20
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         20
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      20
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         20
     *MSSUB_KMCUTOFF)                                                     20
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    20
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           20
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      20
      REAL RE_SSUB_KMCUTOFF                                               20
C -                                                                       20
      REAL SSUB_SRINGID                                                   20
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           20
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       20
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          20
     *MSSUB_SRINGID)                                                      20
      CHARACTER CSSUB_SRINGID*40/' '/                                     20
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             20
      INTEGER MSSUB_SRINGID(2)/1,1/                                       20
      REAL RE_SSUB_SRINGID                                                20
C -                                                                       20
      REAL SSUB_SRINGOD                                                   20
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           20
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      20
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        20
     *MSSUB_SRINGOD)                                                      20
      CHARACTER CSSUB_SRINGOD*40/' '/                                     20
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             20
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       20
      REAL RE_SSUB_SRINGOD                                                20
C -                                                                       20
      REAL SSUB_SRINGTHK                                                  20
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         20
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     20
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       20
     *MSSUB_SRINGTHK)                                                     20
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    20
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           20
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      20
      REAL RE_SSUB_SRINGTHK                                               20
C -                                                                       20
      REAL SSUB_SRCUTPHI                                                  20
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         20
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     20
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       20
     *MSSUB_SRCUTPHI)                                                     20
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    20
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           20
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      20
      REAL RE_SSUB_SRCUTPHI                                               20
C -                                                                       20
      REAL SSUB_SRCUTWID                                                  20
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         20
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     20
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       20
     *MSSUB_SRCUTWID)                                                     20
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    20
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           20
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      20
      REAL RE_SSUB_SRCUTWID                                               20
C -                                                                       20
      REAL SSUB_SRCUTOUT                                                  20
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         20
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     20
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       20
     *MSSUB_SRCUTOUT)                                                     20
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    20
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           20
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      20
      REAL RE_SSUB_SRCUTOUT                                               20
C -                                                                       20
      REAL SSUB_SRCUTIN                                                   20
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           20
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      20
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        20
     *MSSUB_SRCUTIN)                                                      20
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     20
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             20
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       20
      REAL RE_SSUB_SRCUTIN                                                20
C -                                                                       20
      REAL SSUB_SROLLID                                                   20
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           20
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      20
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        20
     *MSSUB_SROLLID)                                                      20
      CHARACTER CSSUB_SROLLID*40/' '/                                     20
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             20
      INTEGER MSSUB_SROLLID(2)/1,1/                                       20
      REAL RE_SSUB_SROLLID                                                20
C -                                                                       20
      REAL SSUB_SROLLOD                                                   20
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           20
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      20
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        20
     *MSSUB_SROLLOD)                                                      20
      CHARACTER CSSUB_SROLLOD*40/' '/                                     20
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             20
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       20
      REAL RE_SSUB_SROLLOD                                                20
C -                                                                       20
      REAL SSUB_SROLLLEN                                                  20
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         20
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     20
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       20
     *MSSUB_SROLLLEN)                                                     20
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    20
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           20
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      20
      REAL RE_SSUB_SROLLLEN                                               20
C -                                                                       20
      REAL SSUB_SWIRELEN                                                  20
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         20
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     20
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       20
     *MSSUB_SWIRELEN)                                                     20
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    20
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           20
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      20
      REAL RE_SSUB_SWIRELEN                                               20
C -                                                                       20
      REAL SSUB_MBLKHGH                                                   20
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           20
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      20
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        20
     *MSSUB_MBLKHGH)                                                      20
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     20
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             20
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       20
      REAL RE_SSUB_MBLKHGH                                                20
C -                                                                       20
      REAL SSUB_MBLKOWID                                                  20
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         20
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     20
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       20
     *MSSUB_MBLKOWID)                                                     20
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    20
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           20
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      20
      REAL RE_SSUB_MBLKOWID                                               20
C -                                                                       20
      REAL SSUB_MBLKOLEN                                                  20
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         20
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     20
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       20
     *MSSUB_MBLKOLEN)                                                     20
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    20
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           20
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      20
      REAL RE_SSUB_MBLKOLEN                                               20
C -                                                                       20
      REAL SSUB_MBLKIWID                                                  20
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         20
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     20
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       20
     *MSSUB_MBLKIWID)                                                     20
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    20
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           20
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      20
      REAL RE_SSUB_MBLKIWID                                               20
C -                                                                       20
      REAL SSUB_MBLKILEN                                                  20
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         20
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     20
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       20
     *MSSUB_MBLKILEN)                                                     20
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    20
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           20
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      20
      REAL RE_SSUB_MBLKILEN                                               20
C -                                                                       20
      REAL SSUB_MBLKORAD                                                  20
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         20
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     20
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       20
     *MSSUB_MBLKORAD)                                                     20
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    20
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           20
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      20
      REAL RE_SSUB_MBLKORAD                                               20
C -                                                                       20
      REAL SSUB_MBLKIRAD                                                  20
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         20
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     20
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       20
     *MSSUB_MBLKIRAD)                                                     20
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    20
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           20
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      20
      REAL RE_SSUB_MBLKIRAD                                               20
C -                                                                       20
      REAL SSUB_MRODDIA                                                   20
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           20
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      20
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        20
     *MSSUB_MRODDIA)                                                      20
      CHARACTER CSSUB_MRODDIA*40/' '/                                     20
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             20
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       20
      REAL RE_SSUB_MRODDIA                                                20
C -                                                                       20
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                20
      PARAMETER (LEN_SSUB=29)                                             20
      PARAMETER (LLEN_SSUB=27)                                            20
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           20
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        20
      DATA IDNUM_SSUB/10*0/                                               20
      REAL BANK_SSUB(LEN_SSUB)                                            20
      CHARACTER*60 BPATH_SSUB/' '/                                        20
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          20
     *BPATH_SSUB                                                          20
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             20
C *                                                                       20
C -                                                                       21
      REAL SWAM_VERSION                                                   21
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            21
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       21
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          21
     *MSWAM_VERSION)                                                      21
      CHARACTER CSWAM_VERSION*40/' '/                                     21
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             21
      INTEGER MSWAM_VERSION(2)/1,1/                                       21
      REAL RE_SWAM_VERSION                                                21
C -                                                                       21
      REAL SWAM_ZMIN                                                      21
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   21
     *NSWAM_ZMIN)                                                         21
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             21
     *MSWAM_ZMIN)                                                         21
      CHARACTER CSWAM_ZMIN*40/' '/                                        21
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   21
      INTEGER MSWAM_ZMIN(2)/1,1/                                          21
      REAL RE_SWAM_ZMIN                                                   21
C -                                                                       21
      REAL SWAM_LEN                                                       21
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     21
     *NSWAM_LEN)                                                          21
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    21
      CHARACTER CSWAM_LEN*40/' '/                                         21
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     21
      INTEGER MSWAM_LEN(2)/1,1/                                           21
      REAL RE_SWAM_LEN                                                    21
C -                                                                       21
      REAL SWAM_RMIN                                                      21
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   21
     *NSWAM_RMIN)                                                         21
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             21
     *MSWAM_RMIN)                                                         21
      CHARACTER CSWAM_RMIN*40/' '/                                        21
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   21
      INTEGER MSWAM_RMIN(2)/1,1/                                          21
      REAL RE_SWAM_RMIN                                                   21
C -                                                                       21
      REAL SWAM_RMAX                                                      21
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   21
     *NSWAM_RMAX)                                                         21
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             21
     *MSWAM_RMAX)                                                         21
      CHARACTER CSWAM_RMAX*40/' '/                                        21
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   21
      INTEGER MSWAM_RMAX(2)/1,1/                                          21
      REAL RE_SWAM_RMAX                                                   21
C -                                                                       21
      REAL SWAM_TBRDTHK                                                   21
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            21
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       21
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          21
     *MSWAM_TBRDTHK)                                                      21
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     21
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             21
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       21
      REAL RE_SWAM_TBRDTHK                                                21
C -                                                                       21
      REAL SWAM_WALLTHK                                                   21
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            21
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       21
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          21
     *MSWAM_WALLTHK)                                                      21
      CHARACTER CSWAM_WALLTHK*40/' '/                                     21
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             21
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       21
      REAL RE_SWAM_WALLTHK                                                21
C -                                                                       21
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                21
      PARAMETER (LEN_SWAM=9)                                              21
      PARAMETER (LLEN_SWAM=7)                                             21
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           21
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        21
      DATA IDNUM_SWAM/10*0/                                               21
      REAL BANK_SWAM(LEN_SWAM)                                            21
      CHARACTER*60 BPATH_SWAM/' '/                                        21
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          21
     *BPATH_SWAM                                                          21
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             21
C *                                                                       21
C -                                                                       22
      REAL SERG_VERSION                                                   22
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            22
     *(NAMES_SERG(1),NSERG_VERSION)                                       22
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          22
     *MSERG_VERSION)                                                      22
      CHARACTER CSERG_VERSION*40/' '/                                     22
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             22
      INTEGER MSERG_VERSION(2)/1,1/                                       22
      REAL RE_SERG_VERSION                                                22
C -                                                                       22
      REAL SERG_IRNGTRMX                                                  22
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          22
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      22
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         22
     *MSERG_IRNGTRMX)                                                     22
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    22
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           22
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      22
      REAL RE_SERG_IRNGTRMX                                               22
C -                                                                       22
      REAL SERG_IRNGPRMN                                                  22
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          22
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      22
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         22
     *MSERG_IRNGPRMN)                                                     22
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    22
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           22
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      22
      REAL RE_SERG_IRNGPRMN                                               22
C -                                                                       22
      REAL SERG_ORNGRMIN                                                  22
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          22
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      22
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         22
     *MSERG_ORNGRMIN)                                                     22
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    22
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           22
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      22
      REAL RE_SERG_ORNGRMIN                                               22
C -                                                                       22
      REAL SERG_ORNGRMAX                                                  22
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          22
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      22
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         22
     *MSERG_ORNGRMAX)                                                     22
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    22
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           22
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      22
      REAL RE_SERG_ORNGRMAX                                               22
C -                                                                       22
      REAL SERG_ENDRNGTH                                                  22
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          22
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      22
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         22
     *MSERG_ENDRNGTH)                                                     22
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    22
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           22
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      22
      REAL RE_SERG_ENDRNGTH                                               22
C -                                                                       22
      REAL SERG_ENDRNGZM                                                  22
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          22
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      22
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         22
     *MSERG_ENDRNGZM)                                                     22
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    22
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           22
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      22
      REAL RE_SERG_ENDRNGZM                                               22
C -                                                                       22
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                22
      PARAMETER (LEN_SERG=9)                                              22
      PARAMETER (LLEN_SERG=7)                                             22
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           22
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        22
      DATA IDNUM_SERG/10*0/                                               22
      REAL BANK_SERG(LEN_SERG)                                            22
      CHARACTER*60 BPATH_SERG/' '/                                        22
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          22
     *BPATH_SERG                                                          22
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             22
C *                                                                       22
C -                                                                       24
      REAL SELC_VERSION                                                   24
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            24
     *(NAMES_SELC(1),NSELC_VERSION)                                       24
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          24
     *MSELC_VERSION)                                                      24
      CHARACTER CSELC_VERSION*40/' '/                                     24
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             24
      INTEGER MSELC_VERSION(2)/1,1/                                       24
      REAL RE_SELC_VERSION                                                24
C -                                                                       24
      REAL SELC_BETHK                                                     24
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                24
     *(NAMES_SELC(2),NSELC_BETHK)                                         24
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            24
     *MSELC_BETHK)                                                        24
      CHARACTER CSELC_BETHK*40/' '/                                       24
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 24
      INTEGER MSELC_BETHK(2)/1,1/                                         24
      REAL RE_SELC_BETHK                                                  24
C -                                                                       24
      REAL SELC_WATTHK                                                    24
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              24
     *(NAMES_SELC(3),NSELC_WATTHK)                                        24
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           24
     *MSELC_WATTHK)                                                       24
      CHARACTER CSELC_WATTHK*40/' '/                                      24
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               24
      INTEGER MSELC_WATTHK(2)/1,1/                                        24
      REAL RE_SELC_WATTHK                                                 24
C -                                                                       24
      REAL SELC_BEOTHK                                                    24
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              24
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        24
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           24
     *MSELC_BEOTHK)                                                       24
      CHARACTER CSELC_BEOTHK*40/' '/                                      24
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               24
      INTEGER MSELC_BEOTHK(2)/1,1/                                        24
      REAL RE_SELC_BEOTHK                                                 24
C -                                                                       24
      REAL SELC_DYETHK                                                    24
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              24
     *(NAMES_SELC(5),NSELC_DYETHK)                                        24
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           24
     *MSELC_DYETHK)                                                       24
      CHARACTER CSELC_DYETHK*40/' '/                                      24
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               24
      INTEGER MSELC_DYETHK(2)/1,1/                                        24
      REAL RE_SELC_DYETHK                                                 24
C -                                                                       24
      REAL SELC_DYEWID                                                    24
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              24
     *(NAMES_SELC(6),NSELC_DYEWID)                                        24
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           24
     *MSELC_DYEWID)                                                       24
      CHARACTER CSELC_DYEWID*40/' '/                                      24
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               24
      INTEGER MSELC_DYEWID(2)/1,1/                                        24
      REAL RE_SELC_DYEWID                                                 24
C -                                                                       24
      REAL SELC_DYESPC                                                    24
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              24
     *(NAMES_SELC(7),NSELC_DYESPC)                                        24
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           24
     *MSELC_DYESPC)                                                       24
      CHARACTER CSELC_DYESPC*40/' '/                                      24
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               24
      INTEGER MSELC_DYESPC(2)/1,1/                                        24
      REAL RE_SELC_DYESPC                                                 24
C -                                                                       24
      REAL SELC_ELCAWID                                                   24
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           24
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       24
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          24
     *MSELC_ELCAWID)                                                      24
      CHARACTER CSELC_ELCAWID*40/' '/                                     24
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             24
      INTEGER MSELC_ELCAWID(2)/1,1/                                       24
      REAL RE_SELC_ELCAWID                                                24
C -                                                                       24
      REAL SELC_AGPDTHK                                                   24
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           24
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       24
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          24
     *MSELC_AGPDTHK)                                                      24
      CHARACTER CSELC_AGPDTHK*40/' '/                                     24
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             24
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       24
      REAL RE_SELC_AGPDTHK                                                24
C -                                                                       24
      REAL SELC_GLASSTHK                                                  24
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         24
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     24
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       24
     *MSELC_GLASSTHK)                                                     24
      CHARACTER CSELC_GLASSTHK*40/' '/                                    24
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           24
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      24
      REAL RE_SELC_GLASSTHK                                               24
C -                                                                       24
      REAL SELC_CABTHK                                                    24
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             24
     *(NAMES_SELC(11),NSELC_CABTHK)                                       24
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         24
     *MSELC_CABTHK)                                                       24
      CHARACTER CSELC_CABTHK*40/' '/                                      24
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               24
      INTEGER MSELC_CABTHK(2)/1,1/                                        24
      REAL RE_SELC_CABTHK                                                 24
C -                                                                       24
      REAL SELC_CABWID                                                    24
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             24
     *(NAMES_SELC(12),NSELC_CABWID)                                       24
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         24
     *MSELC_CABWID)                                                       24
      CHARACTER CSELC_CABWID*40/' '/                                      24
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               24
      INTEGER MSELC_CABWID(2)/1,1/                                        24
      REAL RE_SELC_CABWID                                                 24
C -                                                                       24
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                24
      PARAMETER (LEN_SELC=14)                                             24
      PARAMETER (LLEN_SELC=12)                                            24
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           24
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        24
      DATA IDNUM_SELC/10*0/                                               24
      REAL BANK_SELC(LEN_SELC)                                            24
      CHARACTER*60 BPATH_SELC/' '/                                        24
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          24
     *BPATH_SELC                                                          24
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             24
C *                                                                       24
C -                                                                       26
      INTEGER SVTL_LAYER                                                  26
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                26
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         26
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            26
     *MSVTL_LAYER)                                                        26
      CHARACTER CSVTL_LAYER*40/' '/                                       26
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 26
      INTEGER MSVTL_LAYER(2)/1,1/                                         26
      REAL RE_SVTL_LAYER                                                  26
C -                                                                       26
      REAL SVTL_NLADDER                                                   26
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            26
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       26
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          26
     *MSVTL_NLADDER)                                                      26
      CHARACTER CSVTL_NLADDER*40/' '/                                     26
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             26
      INTEGER MSVTL_NLADDER(2)/1,1/                                       26
      REAL RE_SVTL_NLADDER                                                26
C -                                                                       26
      REAL SVTL_NWAFER                                                    26
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              26
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        26
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           26
     *MSVTL_NWAFER)                                                       26
      CHARACTER CSVTL_NWAFER*40/' '/                                      26
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               26
      INTEGER MSVTL_NWAFER(2)/1,1/                                        26
      REAL RE_SVTL_NWAFER                                                 26
C -                                                                       26
      REAL SVTL_RADIUS                                                    26
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              26
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        26
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           26
     *MSVTL_RADIUS)                                                       26
      CHARACTER CSVTL_RADIUS*40/' '/                                      26
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               26
      INTEGER MSVTL_RADIUS(2)/1,1/                                        26
      REAL RE_SVTL_RADIUS                                                 26
C -                                                                       26
      REAL SVTL_BAREEDGE                                                  26
      EQUIVALENCE (BANK_SVTL(7),SVTL_BAREEDGE,RE_SVTL_BAREEDGE),          26
     *(NAMES_SVTL(5),NSVTL_BAREEDGE)                                      26
      EQUIVALENCE (COMMEN_SVTL(5),CSVTL_BAREEDGE),(MAP_SVTL(1,5),         26
     *MSVTL_BAREEDGE)                                                     26
      CHARACTER CSVTL_BAREEDGE*40/' '/                                    26
      CHARACTER NSVTL_BAREEDGE*12/'R.BAREEDGE'/                           26
      INTEGER MSVTL_BAREEDGE(2)/1,1/                                      26
      REAL RE_SVTL_BAREEDGE                                               26
C -                                                                       26
      REAL SVTL_PCBLEN                                                    26
      EQUIVALENCE (BANK_SVTL(8),SVTL_PCBLEN,RE_SVTL_PCBLEN),              26
     *(NAMES_SVTL(6),NSVTL_PCBLEN)                                        26
      EQUIVALENCE (COMMEN_SVTL(6),CSVTL_PCBLEN),(MAP_SVTL(1,6),           26
     *MSVTL_PCBLEN)                                                       26
      CHARACTER CSVTL_PCBLEN*40/' '/                                      26
      CHARACTER NSVTL_PCBLEN*12/'R.PCBLEN'/                               26
      INTEGER MSVTL_PCBLEN(2)/1,1/                                        26
      REAL RE_SVTL_PCBLEN                                                 26
C -                                                                       26
      REAL SVTL_PCBWIDTH                                                  26
      EQUIVALENCE (BANK_SVTL(9),SVTL_PCBWIDTH,RE_SVTL_PCBWIDTH),          26
     *(NAMES_SVTL(7),NSVTL_PCBWIDTH)                                      26
      EQUIVALENCE (COMMEN_SVTL(7),CSVTL_PCBWIDTH),(MAP_SVTL(1,7),         26
     *MSVTL_PCBWIDTH)                                                     26
      CHARACTER CSVTL_PCBWIDTH*40/' '/                                    26
      CHARACTER NSVTL_PCBWIDTH*12/'R.PCBWIDTH'/                           26
      INTEGER MSVTL_PCBWIDTH(2)/1,1/                                      26
      REAL RE_SVTL_PCBWIDTH                                               26
C -                                                                       26
      REAL SVTL_PCBTHK                                                    26
      EQUIVALENCE (BANK_SVTL(10),SVTL_PCBTHK,RE_SVTL_PCBTHK),             26
     *(NAMES_SVTL(8),NSVTL_PCBTHK)                                        26
      EQUIVALENCE (COMMEN_SVTL(8),CSVTL_PCBTHK),(MAP_SVTL(1,8),           26
     *MSVTL_PCBTHK)                                                       26
      CHARACTER CSVTL_PCBTHK*40/' '/                                      26
      CHARACTER NSVTL_PCBTHK*12/'R.PCBTHK'/                               26
      INTEGER MSVTL_PCBTHK(2)/1,1/                                        26
      REAL RE_SVTL_PCBTHK                                                 26
C -                                                                       26
      REAL SVTL_PCBGAP                                                    26
      EQUIVALENCE (BANK_SVTL(11),SVTL_PCBGAP,RE_SVTL_PCBGAP),             26
     *(NAMES_SVTL(9),NSVTL_PCBGAP)                                        26
      EQUIVALENCE (COMMEN_SVTL(9),CSVTL_PCBGAP),(MAP_SVTL(1,9),           26
     *MSVTL_PCBGAP)                                                       26
      CHARACTER CSVTL_PCBGAP*40/' '/                                      26
      CHARACTER NSVTL_PCBGAP*12/'R.PCBGAP'/                               26
      INTEGER MSVTL_PCBGAP(2)/1,1/                                        26
      REAL RE_SVTL_PCBGAP                                                 26
C -                                                                       26
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                26
      PARAMETER (LEN_SVTL=11)                                             26
      PARAMETER (LLEN_SVTL=9)                                             26
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           26
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        26
      DATA IDNUM_SVTL/10*0/                                               26
      REAL BANK_SVTL(LEN_SVTL)                                            26
      CHARACTER*60 BPATH_SVTL/' '/                                        26
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          26
     *BPATH_SVTL                                                          26
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             26
C *                                                                       26
C -                                                                       28
      REAL SSLD_VERSION                                                   28
      EQUIVALENCE (BANK_SSLD(3),SSLD_VERSION,RE_SSLD_VERSION),            28
     *(NAMES_SSLD(1),NSSLD_VERSION)                                       28
      EQUIVALENCE (COMMEN_SSLD(1),CSSLD_VERSION),(MAP_SSLD(1,1),          28
     *MSSLD_VERSION)                                                      28
      CHARACTER CSSLD_VERSION*40/' '/                                     28
      CHARACTER NSSLD_VERSION*12/'R.VERSION'/                             28
      INTEGER MSSLD_VERSION(2)/1,1/                                       28
      REAL RE_SSLD_VERSION                                                28
C -                                                                       28
      REAL SSLD_SINRINN                                                   28
      EQUIVALENCE (BANK_SSLD(4),SSLD_SINRINN,RE_SSLD_SINRINN),            28
     *(NAMES_SSLD(2),NSSLD_SINRINN)                                       28
      EQUIVALENCE (COMMEN_SSLD(2),CSSLD_SINRINN),(MAP_SSLD(1,2),          28
     *MSSLD_SINRINN)                                                      28
      CHARACTER CSSLD_SINRINN*40/' '/                                     28
      CHARACTER NSSLD_SINRINN*12/'R.SINRINN'/                             28
      INTEGER MSSLD_SINRINN(2)/1,1/                                       28
      REAL RE_SSLD_SINRINN                                                28
C -                                                                       28
      REAL SSLD_SINROUT                                                   28
      EQUIVALENCE (BANK_SSLD(5),SSLD_SINROUT,RE_SSLD_SINROUT),            28
     *(NAMES_SSLD(3),NSSLD_SINROUT)                                       28
      EQUIVALENCE (COMMEN_SSLD(3),CSSLD_SINROUT),(MAP_SSLD(1,3),          28
     *MSSLD_SINROUT)                                                      28
      CHARACTER CSSLD_SINROUT*40/' '/                                     28
      CHARACTER NSSLD_SINROUT*12/'R.SINROUT'/                             28
      INTEGER MSSLD_SINROUT(2)/1,1/                                       28
      REAL RE_SSLD_SINROUT                                                28
C -                                                                       28
      REAL SSLD_SINLEN                                                    28
      EQUIVALENCE (BANK_SSLD(6),SSLD_SINLEN,RE_SSLD_SINLEN),              28
     *(NAMES_SSLD(4),NSSLD_SINLEN)                                        28
      EQUIVALENCE (COMMEN_SSLD(4),CSSLD_SINLEN),(MAP_SSLD(1,4),           28
     *MSSLD_SINLEN)                                                       28
      CHARACTER CSSLD_SINLEN*40/' '/                                      28
      CHARACTER NSSLD_SINLEN*12/'R.SINLEN'/                               28
      INTEGER MSSLD_SINLEN(2)/1,1/                                        28
      REAL RE_SSLD_SINLEN                                                 28
C -                                                                       28
      REAL SSLD_SSEPRINN                                                  28
      EQUIVALENCE (BANK_SSLD(7),SSLD_SSEPRINN,RE_SSLD_SSEPRINN),          28
     *(NAMES_SSLD(5),NSSLD_SSEPRINN)                                      28
      EQUIVALENCE (COMMEN_SSLD(5),CSSLD_SSEPRINN),(MAP_SSLD(1,5),         28
     *MSSLD_SSEPRINN)                                                     28
      CHARACTER CSSLD_SSEPRINN*40/' '/                                    28
      CHARACTER NSSLD_SSEPRINN*12/'R.SSEPRINN'/                           28
      INTEGER MSSLD_SSEPRINN(2)/1,1/                                      28
      REAL RE_SSLD_SSEPRINN                                               28
C -                                                                       28
      REAL SSLD_SSEPROUT                                                  28
      EQUIVALENCE (BANK_SSLD(8),SSLD_SSEPROUT,RE_SSLD_SSEPROUT),          28
     *(NAMES_SSLD(6),NSSLD_SSEPROUT)                                      28
      EQUIVALENCE (COMMEN_SSLD(6),CSSLD_SSEPROUT),(MAP_SSLD(1,6),         28
     *MSSLD_SSEPROUT)                                                     28
      CHARACTER CSSLD_SSEPROUT*40/' '/                                    28
      CHARACTER NSSLD_SSEPROUT*12/'R.SSEPROUT'/                           28
      INTEGER MSSLD_SSEPROUT(2)/1,1/                                      28
      REAL RE_SSLD_SSEPROUT                                               28
C -                                                                       28
      REAL SSLD_SSEPLEN                                                   28
      EQUIVALENCE (BANK_SSLD(9),SSLD_SSEPLEN,RE_SSLD_SSEPLEN),            28
     *(NAMES_SSLD(7),NSSLD_SSEPLEN)                                       28
      EQUIVALENCE (COMMEN_SSLD(7),CSSLD_SSEPLEN),(MAP_SSLD(1,7),          28
     *MSSLD_SSEPLEN)                                                      28
      CHARACTER CSSLD_SSEPLEN*40/' '/                                     28
      CHARACTER NSSLD_SSEPLEN*12/'R.SSEPLEN'/                             28
      INTEGER MSSLD_SSEPLEN(2)/1,1/                                       28
      REAL RE_SSLD_SSEPLEN                                                28
C -                                                                       28
      REAL SSLD_SOUTRINN                                                  28
      EQUIVALENCE (BANK_SSLD(10),SSLD_SOUTRINN,RE_SSLD_SOUTRINN),         28
     *(NAMES_SSLD(8),NSSLD_SOUTRINN)                                      28
      EQUIVALENCE (COMMEN_SSLD(8),CSSLD_SOUTRINN),(MAP_SSLD(1,8),         28
     *MSSLD_SOUTRINN)                                                     28
      CHARACTER CSSLD_SOUTRINN*40/' '/                                    28
      CHARACTER NSSLD_SOUTRINN*12/'R.SOUTRINN'/                           28
      INTEGER MSSLD_SOUTRINN(2)/1,1/                                      28
      REAL RE_SSLD_SOUTRINN                                               28
C -                                                                       28
      REAL SSLD_SOUTROUT                                                  28
      EQUIVALENCE (BANK_SSLD(11),SSLD_SOUTROUT,RE_SSLD_SOUTROUT),         28
     *(NAMES_SSLD(9),NSSLD_SOUTROUT)                                      28
      EQUIVALENCE (COMMEN_SSLD(9),CSSLD_SOUTROUT),(MAP_SSLD(1,9),         28
     *MSSLD_SOUTROUT)                                                     28
      CHARACTER CSSLD_SOUTROUT*40/' '/                                    28
      CHARACTER NSSLD_SOUTROUT*12/'R.SOUTROUT'/                           28
      INTEGER MSSLD_SOUTROUT(2)/1,1/                                      28
      REAL RE_SSLD_SOUTROUT                                               28
C -                                                                       28
      REAL SSLD_SOUTLEN                                                   28
      EQUIVALENCE (BANK_SSLD(12),SSLD_SOUTLEN,RE_SSLD_SOUTLEN),           28
     *(NAMES_SSLD(10),NSSLD_SOUTLEN)                                      28
      EQUIVALENCE (COMMEN_SSLD(10),CSSLD_SOUTLEN),(MAP_SSLD(1,10),        28
     *MSSLD_SOUTLEN)                                                      28
      CHARACTER CSSLD_SOUTLEN*40/' '/                                     28
      CHARACTER NSSLD_SOUTLEN*12/'R.SOUTLEN'/                             28
      INTEGER MSSLD_SOUTLEN(2)/1,1/                                       28
      REAL RE_SSLD_SOUTLEN                                                28
C -                                                                       28
      REAL SSLD_ALMESHID                                                  28
      EQUIVALENCE (BANK_SSLD(13),SSLD_ALMESHID,RE_SSLD_ALMESHID),         28
     *(NAMES_SSLD(11),NSSLD_ALMESHID)                                     28
      EQUIVALENCE (COMMEN_SSLD(11),CSSLD_ALMESHID),(MAP_SSLD(1,11),       28
     *MSSLD_ALMESHID)                                                     28
      CHARACTER CSSLD_ALMESHID*40/' '/                                    28
      CHARACTER NSSLD_ALMESHID*12/'R.ALMESHID'/                           28
      INTEGER MSSLD_ALMESHID(2)/1,1/                                      28
      REAL RE_SSLD_ALMESHID                                               28
C -                                                                       28
      REAL SSLD_ALMESHOD                                                  28
      EQUIVALENCE (BANK_SSLD(14),SSLD_ALMESHOD,RE_SSLD_ALMESHOD),         28
     *(NAMES_SSLD(12),NSSLD_ALMESHOD)                                     28
      EQUIVALENCE (COMMEN_SSLD(12),CSSLD_ALMESHOD),(MAP_SSLD(1,12),       28
     *MSSLD_ALMESHOD)                                                     28
      CHARACTER CSSLD_ALMESHOD*40/' '/                                    28
      CHARACTER NSSLD_ALMESHOD*12/'R.ALMESHOD'/                           28
      INTEGER MSSLD_ALMESHOD(2)/1,1/                                      28
      REAL RE_SSLD_ALMESHOD                                               28
C -                                                                       28
      REAL SSLD_ALMSHTHK                                                  28
      EQUIVALENCE (BANK_SSLD(15),SSLD_ALMSHTHK,RE_SSLD_ALMSHTHK),         28
     *(NAMES_SSLD(13),NSSLD_ALMSHTHK)                                     28
      EQUIVALENCE (COMMEN_SSLD(13),CSSLD_ALMSHTHK),(MAP_SSLD(1,13),       28
     *MSSLD_ALMSHTHK)                                                     28
      CHARACTER CSSLD_ALMSHTHK*40/' '/                                    28
      CHARACTER NSSLD_ALMSHTHK*12/'R.ALMSHTHK'/                           28
      INTEGER MSSLD_ALMSHTHK(2)/1,1/                                      28
      REAL RE_SSLD_ALMSHTHK                                               28
C -                                                                       28
      REAL SSLD_ALMSHPOS                                                  28
      EQUIVALENCE (BANK_SSLD(16),SSLD_ALMSHPOS,RE_SSLD_ALMSHPOS),         28
     *(NAMES_SSLD(14),NSSLD_ALMSHPOS)                                     28
      EQUIVALENCE (COMMEN_SSLD(14),CSSLD_ALMSHPOS),(MAP_SSLD(1,14),       28
     *MSSLD_ALMSHPOS)                                                     28
      CHARACTER CSSLD_ALMSHPOS*40/' '/                                    28
      CHARACTER NSSLD_ALMSHPOS*12/'R.ALMSHPOS'/                           28
      INTEGER MSSLD_ALMSHPOS(2)/1,1/                                      28
      REAL RE_SSLD_ALMSHPOS                                               28
C -                                                                       28
      INTEGER LEN_SSLD,LLEN_SSLD,LINK_SSLD                                28
      PARAMETER (LEN_SSLD=16)                                             28
      PARAMETER (LLEN_SSLD=14)                                            28
      INTEGER FLAG_SSLD/0/,MAP_SSLD(2,LLEN_SSLD),IDNUM_SSLD(10)           28
      CHARACTER NAMES_SSLD(LLEN_SSLD)*12,COMMEN_SSLD(LLEN_SSLD)*40        28
      DATA IDNUM_SSLD/10*0/                                               28
      REAL BANK_SSLD(LEN_SSLD)                                            28
      CHARACTER*60 BPATH_SSLD/' '/                                        28
      SAVE FLAG_SSLD,MAP_SSLD,IDNUM_SSLD,NAMES_SSLD,COMMEN_SSLD,          28
     *BPATH_SSLD                                                          28
      DATA LINK_SSLD/0/,BANK_SSLD/LEN_SSLD*0/                             28
C *                                                                       28
C -                                                                       29
      INTEGER SCBP_LAYER                                                  29
      EQUIVALENCE (BANK_SCBP(3),SCBP_LAYER,RE_SCBP_LAYER),                29
     *(NAMES_SCBP(1),NSCBP_LAYER)                                         29
      EQUIVALENCE (COMMEN_SCBP(1),CSCBP_LAYER),(MAP_SCBP(1,1),            29
     *MSCBP_LAYER)                                                        29
      CHARACTER CSCBP_LAYER*40/' '/                                       29
      CHARACTER NSCBP_LAYER*12/'I.LAYER'/                                 29
      INTEGER MSCBP_LAYER(2)/1,1/                                         29
      REAL RE_SCBP_LAYER                                                  29
C -                                                                       29
      REAL SCBP_LEN                                                       29
      EQUIVALENCE (BANK_SCBP(4),SCBP_LEN,RE_SCBP_LEN),(NAMES_SCBP(2),     29
     *NSCBP_LEN)                                                          29
      EQUIVALENCE (COMMEN_SCBP(2),CSCBP_LEN),(MAP_SCBP(1,2),MSCBP_LEN)    29
      CHARACTER CSCBP_LEN*40/' '/                                         29
      CHARACTER NSCBP_LEN*12/'R.LEN'/                                     29
      INTEGER MSCBP_LEN(2)/1,1/                                           29
      REAL RE_SCBP_LEN                                                    29
C -                                                                       29
      REAL SCBP_RMIN1                                                     29
      EQUIVALENCE (BANK_SCBP(5),SCBP_RMIN1,RE_SCBP_RMIN1),                29
     *(NAMES_SCBP(3),NSCBP_RMIN1)                                         29
      EQUIVALENCE (COMMEN_SCBP(3),CSCBP_RMIN1),(MAP_SCBP(1,3),            29
     *MSCBP_RMIN1)                                                        29
      CHARACTER CSCBP_RMIN1*40/' '/                                       29
      CHARACTER NSCBP_RMIN1*12/'R.RMIN1'/                                 29
      INTEGER MSCBP_RMIN1(2)/1,1/                                         29
      REAL RE_SCBP_RMIN1                                                  29
C -                                                                       29
      REAL SCBP_RMAX1                                                     29
      EQUIVALENCE (BANK_SCBP(6),SCBP_RMAX1,RE_SCBP_RMAX1),                29
     *(NAMES_SCBP(4),NSCBP_RMAX1)                                         29
      EQUIVALENCE (COMMEN_SCBP(4),CSCBP_RMAX1),(MAP_SCBP(1,4),            29
     *MSCBP_RMAX1)                                                        29
      CHARACTER CSCBP_RMAX1*40/' '/                                       29
      CHARACTER NSCBP_RMAX1*12/'R.RMAX1'/                                 29
      INTEGER MSCBP_RMAX1(2)/1,1/                                         29
      REAL RE_SCBP_RMAX1                                                  29
C -                                                                       29
      REAL SCBP_RMIN2                                                     29
      EQUIVALENCE (BANK_SCBP(7),SCBP_RMIN2,RE_SCBP_RMIN2),                29
     *(NAMES_SCBP(5),NSCBP_RMIN2)                                         29
      EQUIVALENCE (COMMEN_SCBP(5),CSCBP_RMIN2),(MAP_SCBP(1,5),            29
     *MSCBP_RMIN2)                                                        29
      CHARACTER CSCBP_RMIN2*40/' '/                                       29
      CHARACTER NSCBP_RMIN2*12/'R.RMIN2'/                                 29
      INTEGER MSCBP_RMIN2(2)/1,1/                                         29
      REAL RE_SCBP_RMIN2                                                  29
C -                                                                       29
      REAL SCBP_RMAX2                                                     29
      EQUIVALENCE (BANK_SCBP(8),SCBP_RMAX2,RE_SCBP_RMAX2),                29
     *(NAMES_SCBP(6),NSCBP_RMAX2)                                         29
      EQUIVALENCE (COMMEN_SCBP(6),CSCBP_RMAX2),(MAP_SCBP(1,6),            29
     *MSCBP_RMAX2)                                                        29
      CHARACTER CSCBP_RMAX2*40/' '/                                       29
      CHARACTER NSCBP_RMAX2*12/'R.RMAX2'/                                 29
      INTEGER MSCBP_RMAX2(2)/1,1/                                         29
      REAL RE_SCBP_RMAX2                                                  29
C -                                                                       29
      REAL SCBP_VOL                                                       29
      EQUIVALENCE (BANK_SCBP(9),SCBP_VOL,RE_SCBP_VOL),(NAMES_SCBP(7),     29
     *NSCBP_VOL)                                                          29
      EQUIVALENCE (COMMEN_SCBP(7),CSCBP_VOL),(MAP_SCBP(1,7),MSCBP_VOL)    29
      CHARACTER CSCBP_VOL*40/' '/                                         29
      CHARACTER NSCBP_VOL*12/'R.VOL'/                                     29
      INTEGER MSCBP_VOL(2)/1,1/                                           29
      REAL RE_SCBP_VOL                                                    29
C -                                                                       29
      INTEGER LEN_SCBP,LLEN_SCBP,LINK_SCBP                                29
      PARAMETER (LEN_SCBP=9)                                              29
      PARAMETER (LLEN_SCBP=7)                                             29
      INTEGER FLAG_SCBP/0/,MAP_SCBP(2,LLEN_SCBP),IDNUM_SCBP(10)           29
      CHARACTER NAMES_SCBP(LLEN_SCBP)*12,COMMEN_SCBP(LLEN_SCBP)*40        29
      DATA IDNUM_SCBP/10*0/                                               29
      REAL BANK_SCBP(LEN_SCBP)                                            29
      CHARACTER*60 BPATH_SCBP/' '/                                        29
      SAVE FLAG_SCBP,MAP_SCBP,IDNUM_SCBP,NAMES_SCBP,COMMEN_SCBP,          29
     *BPATH_SCBP                                                          29
      DATA LINK_SCBP/0/,BANK_SCBP/LEN_SCBP*0/                             29
C *                                                                       29
C -                                                                       30
      INTEGER SFEP_LAYER                                                  30
      EQUIVALENCE (BANK_SFEP(3),SFEP_LAYER,RE_SFEP_LAYER),                30
     *(NAMES_SFEP(1),NSFEP_LAYER)                                         30
      EQUIVALENCE (COMMEN_SFEP(1),CSFEP_LAYER),(MAP_SFEP(1,1),            30
     *MSFEP_LAYER)                                                        30
      CHARACTER CSFEP_LAYER*40/' '/                                       30
      CHARACTER NSFEP_LAYER*12/'I.LAYER'/                                 30
      INTEGER MSFEP_LAYER(2)/1,1/                                         30
      REAL RE_SFEP_LAYER                                                  30
C -                                                                       30
      REAL SFEP_LEN                                                       30
      EQUIVALENCE (BANK_SFEP(4),SFEP_LEN,RE_SFEP_LEN),(NAMES_SFEP(2),     30
     *NSFEP_LEN)                                                          30
      EQUIVALENCE (COMMEN_SFEP(2),CSFEP_LEN),(MAP_SFEP(1,2),MSFEP_LEN)    30
      CHARACTER CSFEP_LEN*40/' '/                                         30
      CHARACTER NSFEP_LEN*12/'R.LEN'/                                     30
      INTEGER MSFEP_LEN(2)/1,1/                                           30
      REAL RE_SFEP_LEN                                                    30
C -                                                                       30
      REAL SFEP_RMIN1                                                     30
      EQUIVALENCE (BANK_SFEP(5),SFEP_RMIN1,RE_SFEP_RMIN1),                30
     *(NAMES_SFEP(3),NSFEP_RMIN1)                                         30
      EQUIVALENCE (COMMEN_SFEP(3),CSFEP_RMIN1),(MAP_SFEP(1,3),            30
     *MSFEP_RMIN1)                                                        30
      CHARACTER CSFEP_RMIN1*40/' '/                                       30
      CHARACTER NSFEP_RMIN1*12/'R.RMIN1'/                                 30
      INTEGER MSFEP_RMIN1(2)/1,1/                                         30
      REAL RE_SFEP_RMIN1                                                  30
C -                                                                       30
      REAL SFEP_RMAX1                                                     30
      EQUIVALENCE (BANK_SFEP(6),SFEP_RMAX1,RE_SFEP_RMAX1),                30
     *(NAMES_SFEP(4),NSFEP_RMAX1)                                         30
      EQUIVALENCE (COMMEN_SFEP(4),CSFEP_RMAX1),(MAP_SFEP(1,4),            30
     *MSFEP_RMAX1)                                                        30
      CHARACTER CSFEP_RMAX1*40/' '/                                       30
      CHARACTER NSFEP_RMAX1*12/'R.RMAX1'/                                 30
      INTEGER MSFEP_RMAX1(2)/1,1/                                         30
      REAL RE_SFEP_RMAX1                                                  30
C -                                                                       30
      REAL SFEP_RMIN2                                                     30
      EQUIVALENCE (BANK_SFEP(7),SFEP_RMIN2,RE_SFEP_RMIN2),                30
     *(NAMES_SFEP(5),NSFEP_RMIN2)                                         30
      EQUIVALENCE (COMMEN_SFEP(5),CSFEP_RMIN2),(MAP_SFEP(1,5),            30
     *MSFEP_RMIN2)                                                        30
      CHARACTER CSFEP_RMIN2*40/' '/                                       30
      CHARACTER NSFEP_RMIN2*12/'R.RMIN2'/                                 30
      INTEGER MSFEP_RMIN2(2)/1,1/                                         30
      REAL RE_SFEP_RMIN2                                                  30
C -                                                                       30
      REAL SFEP_RMAX2                                                     30
      EQUIVALENCE (BANK_SFEP(8),SFEP_RMAX2,RE_SFEP_RMAX2),                30
     *(NAMES_SFEP(6),NSFEP_RMAX2)                                         30
      EQUIVALENCE (COMMEN_SFEP(6),CSFEP_RMAX2),(MAP_SFEP(1,6),            30
     *MSFEP_RMAX2)                                                        30
      CHARACTER CSFEP_RMAX2*40/' '/                                       30
      CHARACTER NSFEP_RMAX2*12/'R.RMAX2'/                                 30
      INTEGER MSFEP_RMAX2(2)/1,1/                                         30
      REAL RE_SFEP_RMAX2                                                  30
C -                                                                       30
      REAL SFEP_VOL                                                       30
      EQUIVALENCE (BANK_SFEP(9),SFEP_VOL,RE_SFEP_VOL),(NAMES_SFEP(7),     30
     *NSFEP_VOL)                                                          30
      EQUIVALENCE (COMMEN_SFEP(7),CSFEP_VOL),(MAP_SFEP(1,7),MSFEP_VOL)    30
      CHARACTER CSFEP_VOL*40/' '/                                         30
      CHARACTER NSFEP_VOL*12/'R.VOL'/                                     30
      INTEGER MSFEP_VOL(2)/1,1/                                           30
      REAL RE_SFEP_VOL                                                    30
C -                                                                       30
      REAL SFEP_VOLPLAST                                                  30
      EQUIVALENCE (BANK_SFEP(10),SFEP_VOLPLAST,RE_SFEP_VOLPLAST),         30
     *(NAMES_SFEP(8),NSFEP_VOLPLAST)                                      30
      EQUIVALENCE (COMMEN_SFEP(8),CSFEP_VOLPLAST),(MAP_SFEP(1,8),         30
     *MSFEP_VOLPLAST)                                                     30
      CHARACTER CSFEP_VOLPLAST*40/' '/                                    30
      CHARACTER NSFEP_VOLPLAST*12/'R.VOLPLAST'/                           30
      INTEGER MSFEP_VOLPLAST(2)/1,1/                                      30
      REAL RE_SFEP_VOLPLAST                                               30
C -                                                                       30
      INTEGER LEN_SFEP,LLEN_SFEP,LINK_SFEP                                30
      PARAMETER (LEN_SFEP=10)                                             30
      PARAMETER (LLEN_SFEP=8)                                             30
      INTEGER FLAG_SFEP/0/,MAP_SFEP(2,LLEN_SFEP),IDNUM_SFEP(10)           30
      CHARACTER NAMES_SFEP(LLEN_SFEP)*12,COMMEN_SFEP(LLEN_SFEP)*40        30
      DATA IDNUM_SFEP/10*0/                                               30
      REAL BANK_SFEP(LEN_SFEP)                                            30
      CHARACTER*60 BPATH_SFEP/' '/                                        30
      SAVE FLAG_SFEP,MAP_SFEP,IDNUM_SFEP,NAMES_SFEP,COMMEN_SFEP,          30
     *BPATH_SFEP                                                          30
      DATA LINK_SFEP/0/,BANK_SFEP/LEN_SFEP*0/                             30
C *                                                                       30
C -                                                                       31
      INTEGER SWCX_LAYER                                                  31
      EQUIVALENCE (BANK_SWCX(3),SWCX_LAYER,RE_SWCX_LAYER),                31
     *(NAMES_SWCX(1),NSWCX_LAYER)                                         31
      EQUIVALENCE (COMMEN_SWCX(1),CSWCX_LAYER),(MAP_SWCX(1,1),            31
     *MSWCX_LAYER)                                                        31
      CHARACTER CSWCX_LAYER*40/' '/                                       31
      CHARACTER NSWCX_LAYER*12/'I.LAYER'/                                 31
      INTEGER MSWCX_LAYER(2)/1,1/                                         31
      REAL RE_SWCX_LAYER                                                  31
C -                                                                       31
      REAL SWCX_LENGTH                                                    31
      EQUIVALENCE (BANK_SWCX(4),SWCX_LENGTH,RE_SWCX_LENGTH),              31
     *(NAMES_SWCX(2),NSWCX_LENGTH)                                        31
      EQUIVALENCE (COMMEN_SWCX(2),CSWCX_LENGTH),(MAP_SWCX(1,2),           31
     *MSWCX_LENGTH)                                                       31
      CHARACTER CSWCX_LENGTH*40/' '/                                      31
      CHARACTER NSWCX_LENGTH*12/'R.LENGTH'/                               31
      INTEGER MSWCX_LENGTH(2)/1,1/                                        31
      REAL RE_SWCX_LENGTH                                                 31
C -                                                                       31
      REAL SWCX_DR                                                        31
      EQUIVALENCE (BANK_SWCX(5),SWCX_DR,RE_SWCX_DR),(NAMES_SWCX(3),       31
     *NSWCX_DR)                                                           31
      EQUIVALENCE (COMMEN_SWCX(3),CSWCX_DR),(MAP_SWCX(1,3),MSWCX_DR)      31
      CHARACTER CSWCX_DR*40/' '/                                          31
      CHARACTER NSWCX_DR*12/'R.DR'/                                       31
      INTEGER MSWCX_DR(2)/1,1/                                            31
      REAL RE_SWCX_DR                                                     31
C -                                                                       31
      REAL SWCX_OFFSET                                                    31
      EQUIVALENCE (BANK_SWCX(6),SWCX_OFFSET,RE_SWCX_OFFSET),              31
     *(NAMES_SWCX(4),NSWCX_OFFSET)                                        31
      EQUIVALENCE (COMMEN_SWCX(4),CSWCX_OFFSET),(MAP_SWCX(1,4),           31
     *MSWCX_OFFSET)                                                       31
      CHARACTER CSWCX_OFFSET*40/' '/                                      31
      CHARACTER NSWCX_OFFSET*12/'R.OFFSET'/                               31
      INTEGER MSWCX_OFFSET(2)/1,1/                                        31
      REAL RE_SWCX_OFFSET                                                 31
C -                                                                       31
      REAL SWCX_RAD                                                       31
      EQUIVALENCE (BANK_SWCX(7),SWCX_RAD,RE_SWCX_RAD),(NAMES_SWCX(5),     31
     *NSWCX_RAD)                                                          31
      EQUIVALENCE (COMMEN_SWCX(5),CSWCX_RAD),(MAP_SWCX(1,5),MSWCX_RAD)    31
      CHARACTER CSWCX_RAD*40/' '/                                         31
      CHARACTER NSWCX_RAD*12/'R.RAD'/                                     31
      INTEGER MSWCX_RAD(2)/1,1/                                           31
      REAL RE_SWCX_RAD                                                    31
C -                                                                       31
      REAL SWCX_WALL                                                      31
      EQUIVALENCE (BANK_SWCX(8),SWCX_WALL,RE_SWCX_WALL),(NAMES_SWCX(6),   31
     *NSWCX_WALL)                                                         31
      EQUIVALENCE (COMMEN_SWCX(6),CSWCX_WALL),(MAP_SWCX(1,6),             31
     *MSWCX_WALL)                                                         31
      CHARACTER CSWCX_WALL*40/' '/                                        31
      CHARACTER NSWCX_WALL*12/'R.WALL'/                                   31
      INTEGER MSWCX_WALL(2)/1,1/                                          31
      REAL RE_SWCX_WALL                                                   31
C -                                                                       31
      REAL SWCX_ROFFSET                                                   31
      EQUIVALENCE (BANK_SWCX(9),SWCX_ROFFSET,RE_SWCX_ROFFSET),            31
     *(NAMES_SWCX(7),NSWCX_ROFFSET)                                       31
      EQUIVALENCE (COMMEN_SWCX(7),CSWCX_ROFFSET),(MAP_SWCX(1,7),          31
     *MSWCX_ROFFSET)                                                      31
      CHARACTER CSWCX_ROFFSET*40/' '/                                     31
      CHARACTER NSWCX_ROFFSET*12/'R.ROFFSET'/                             31
      INTEGER MSWCX_ROFFSET(2)/1,1/                                       31
      REAL RE_SWCX_ROFFSET                                                31
C -                                                                       31
      INTEGER LEN_SWCX,LLEN_SWCX,LINK_SWCX                                31
      PARAMETER (LEN_SWCX=9)                                              31
      PARAMETER (LLEN_SWCX=7)                                             31
      INTEGER FLAG_SWCX/0/,MAP_SWCX(2,LLEN_SWCX),IDNUM_SWCX(10)           31
      CHARACTER NAMES_SWCX(LLEN_SWCX)*12,COMMEN_SWCX(LLEN_SWCX)*40        31
      DATA IDNUM_SWCX/10*0/                                               31
      REAL BANK_SWCX(LEN_SWCX)                                            31
      CHARACTER*60 BPATH_SWCX/' '/                                        31
      SAVE FLAG_SWCX,MAP_SWCX,IDNUM_SWCX,NAMES_SWCX,COMMEN_SWCX,          31
     *BPATH_SWCX                                                          31
      DATA LINK_SWCX/0/,BANK_SWCX/LEN_SWCX*0/                             31
C *                                                                       31
C -                                                                       32
      REAL SOUP_VERSION                                                   32
      EQUIVALENCE (BANK_SOUP(3),SOUP_VERSION,RE_SOUP_VERSION),            32
     *(NAMES_SOUP(1),NSOUP_VERSION)                                       32
      EQUIVALENCE (COMMEN_SOUP(1),CSOUP_VERSION),(MAP_SOUP(1,1),          32
     *MSOUP_VERSION)                                                      32
      CHARACTER CSOUP_VERSION*40/' '/                                     32
      CHARACTER NSOUP_VERSION*12/'R.VERSION'/                             32
      INTEGER MSOUP_VERSION(2)/1,1/                                       32
      REAL RE_SOUP_VERSION                                                32
C -                                                                       32
      REAL SOUP_LENGTH                                                    32
      EQUIVALENCE (BANK_SOUP(4),SOUP_LENGTH,RE_SOUP_LENGTH),              32
     *(NAMES_SOUP(2),NSOUP_LENGTH)                                        32
      EQUIVALENCE (COMMEN_SOUP(2),CSOUP_LENGTH),(MAP_SOUP(1,2),           32
     *MSOUP_LENGTH)                                                       32
      CHARACTER CSOUP_LENGTH*40/' '/                                      32
      CHARACTER NSOUP_LENGTH*12/'R.LENGTH'/                               32
      INTEGER MSOUP_LENGTH(2)/1,1/                                        32
      REAL RE_SOUP_LENGTH                                                 32
C -                                                                       32
      REAL SOUP_ROUT                                                      32
      EQUIVALENCE (BANK_SOUP(5),SOUP_ROUT,RE_SOUP_ROUT),(NAMES_SOUP(3),   32
     *NSOUP_ROUT)                                                         32
      EQUIVALENCE (COMMEN_SOUP(3),CSOUP_ROUT),(MAP_SOUP(1,3),             32
     *MSOUP_ROUT)                                                         32
      CHARACTER CSOUP_ROUT*40/' '/                                        32
      CHARACTER NSOUP_ROUT*12/'R.ROUT'/                                   32
      INTEGER MSOUP_ROUT(2)/1,1/                                          32
      REAL RE_SOUP_ROUT                                                   32
C -                                                                       32
      REAL SOUP_DR                                                        32
      EQUIVALENCE (BANK_SOUP(6),SOUP_DR,RE_SOUP_DR),(NAMES_SOUP(4),       32
     *NSOUP_DR)                                                           32
      EQUIVALENCE (COMMEN_SOUP(4),CSOUP_DR),(MAP_SOUP(1,4),MSOUP_DR)      32
      CHARACTER CSOUP_DR*40/' '/                                          32
      CHARACTER NSOUP_DR*12/'R.DR'/                                       32
      INTEGER MSOUP_DR(2)/1,1/                                            32
      REAL RE_SOUP_DR                                                     32
C -                                                                       32
      REAL SOUP_PHI1                                                      32
      EQUIVALENCE (BANK_SOUP(7),SOUP_PHI1,RE_SOUP_PHI1),(NAMES_SOUP(5),   32
     *NSOUP_PHI1)                                                         32
      EQUIVALENCE (COMMEN_SOUP(5),CSOUP_PHI1),(MAP_SOUP(1,5),             32
     *MSOUP_PHI1)                                                         32
      CHARACTER CSOUP_PHI1*40/' '/                                        32
      CHARACTER NSOUP_PHI1*12/'R.PHI1'/                                   32
      INTEGER MSOUP_PHI1(2)/1,1/                                          32
      REAL RE_SOUP_PHI1                                                   32
C -                                                                       32
      REAL SOUP_PHI2                                                      32
      EQUIVALENCE (BANK_SOUP(8),SOUP_PHI2,RE_SOUP_PHI2),(NAMES_SOUP(6),   32
     *NSOUP_PHI2)                                                         32
      EQUIVALENCE (COMMEN_SOUP(6),CSOUP_PHI2),(MAP_SOUP(1,6),             32
     *MSOUP_PHI2)                                                         32
      CHARACTER CSOUP_PHI2*40/' '/                                        32
      CHARACTER NSOUP_PHI2*12/'R.PHI2'/                                   32
      INTEGER MSOUP_PHI2(2)/1,1/                                          32
      REAL RE_SOUP_PHI2                                                   32
C -                                                                       32
      REAL SOUP_DIAMOUT                                                   32
      EQUIVALENCE (BANK_SOUP(9),SOUP_DIAMOUT,RE_SOUP_DIAMOUT),            32
     *(NAMES_SOUP(7),NSOUP_DIAMOUT)                                       32
      EQUIVALENCE (COMMEN_SOUP(7),CSOUP_DIAMOUT),(MAP_SOUP(1,7),          32
     *MSOUP_DIAMOUT)                                                      32
      CHARACTER CSOUP_DIAMOUT*40/' '/                                     32
      CHARACTER NSOUP_DIAMOUT*12/'R.DIAMOUT'/                             32
      INTEGER MSOUP_DIAMOUT(2)/1,1/                                       32
      REAL RE_SOUP_DIAMOUT                                                32
C -                                                                       32
      REAL SOUP_DIAMIN                                                    32
      EQUIVALENCE (BANK_SOUP(10),SOUP_DIAMIN,RE_SOUP_DIAMIN),             32
     *(NAMES_SOUP(8),NSOUP_DIAMIN)                                        32
      EQUIVALENCE (COMMEN_SOUP(8),CSOUP_DIAMIN),(MAP_SOUP(1,8),           32
     *MSOUP_DIAMIN)                                                       32
      CHARACTER CSOUP_DIAMIN*40/' '/                                      32
      CHARACTER NSOUP_DIAMIN*12/'R.DIAMIN'/                               32
      INTEGER MSOUP_DIAMIN(2)/1,1/                                        32
      REAL RE_SOUP_DIAMIN                                                 32
C -                                                                       32
      INTEGER LEN_SOUP,LLEN_SOUP,LINK_SOUP                                32
      PARAMETER (LEN_SOUP=10)                                             32
      PARAMETER (LLEN_SOUP=8)                                             32
      INTEGER FLAG_SOUP/0/,MAP_SOUP(2,LLEN_SOUP),IDNUM_SOUP(10)           32
      CHARACTER NAMES_SOUP(LLEN_SOUP)*12,COMMEN_SOUP(LLEN_SOUP)*40        32
      DATA IDNUM_SOUP/10*0/                                               32
      REAL BANK_SOUP(LEN_SOUP)                                            32
      CHARACTER*60 BPATH_SOUP/' '/                                        32
      SAVE FLAG_SOUP,MAP_SOUP,IDNUM_SOUP,NAMES_SOUP,COMMEN_SOUP,          32
     *BPATH_SOUP                                                          32
      DATA LINK_SOUP/0/,BANK_SOUP/LEN_SOUP*0/                             32
C *                                                                       32
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 33
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         35
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           37
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          39
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    41
      REAL XBUF,PHI,XBUF1,XBUF2                                           43
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     45
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          47
     *SHIELD_PHI(4)                                                       47
      INTEGER I_PHI                                                       49
      RADII(1)= 6.37                                                      51
      RADII(2)= 7.38                                                      52
      RADII(3)=10.38                                                      53
      RADII(4)=11.27                                                      54
      RADII(5)=14.19                                                      55
      RADII(6)=15.13                                                      56
      RAD_OFFSET=0.015                                                    57
      SHIELD_PHI(1)=9.0                                                   58
      SHIELD_PHI(2)=27.0                                                  59
      SHIELD_PHI(3)=45.0                                                  60
      SHIELD_PHI(4)=63.0                                                  61
C *                                                                       62
      CALL AGSBEGM(MODULE,IPRIN)                                          62
      IF (IPRIN.LT.0) RETURN                                              62
C Filling bank SVTG - BASIC SVT DIMENSIONS                                62
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   62
      BPATH_SVTG='SVTG*'                                                  62
      IDNUM_SVTG(1)=0                                                     63
C GEOMETRY VERSION                                                        63
      SVTG_VERSION=2                                                      63
      CSVTG_VERSION='GEOMETRY VERSION'                                    63
C NUMBER OF SVT LAYERS (WAS 7)                                            64
      SVTG_NLAYER=7                                                       64
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         64
C STV INNERMOST RADIUS                                                    65
      SVTG_RSIZEMIN=4.100                                                 65
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               65
C STV OUTERMOST RADIUS                                                    66
      SVTG_RSIZEMAX=46.107                                                66
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               66
C SVT+FTPC LENGTH                                                         67
      SVTG_ZSIZEMAX=270                                                   67
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    67
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          68
      SVTG_ANGOFF=0                                                       68
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       68
C VERSIONING OF THE SHIELD                                                69
      SVTG_SUPPORTVER=1                                                   69
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         69
C VERSIONING OF THE SUPPORT CONE                                          70
      SVTG_CONEVER=1                                                      70
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      70
C WHETHER WE USE THE GEANT MANY OPTION                                    71
      SVTG_IFMANY=0                                                       71
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 71
C THE INDEX OF THE INNERMOST LAYER                                        72
      SVTG_NMIN=1                                                         72
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       72
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    73
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           73
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   73
C *                                                                       73
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    73
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           73
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   73
C *                                                                       74
      CALL AGSBEGM(MODULE,IPRIN)                                          74
      IF (IPRIN.LT.0) RETURN                                              74
C Filling bank SWCA - WAFER CARRIER                                       74
      BANK_TITLE='WAFER CARRIER'                                          74
      BPATH_SWCA='SWCA*'                                                  74
      IDNUM_SWCA(1)=0                                                     75
C GEOMETRY VERSION                                                        75
      SWCA_VERSION=1                                                      75
      CSWCA_VERSION='GEOMETRY VERSION'                                    75
C LADDER LENGTH                                                           76
      SWCA_LENGTH=56.04                                                   76
      CSWCA_LENGTH='LADDER LENGTH'                                        76
C WAFER WIDTH                                                             77
      SWCA_WAFERWID=6.305                                                 77
      CSWCA_WAFERWID='WAFER WIDTH'                                        77
C WAFER LENGTH                                                            78
      SWCA_WAFERLEN=6.305                                                 78
      CSWCA_WAFERLEN='WAFER LENGTH'                                       78
C WAFER THICKNESS                                                         79
      SWCA_WAFERTHK=0.0300                                                79
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    79
C ROHA CELL PLUS GLUE THICKNESS                                           80
      SWCA_ROHATHK=0.0381                                                 80
      CSWCA_ROHATHK='ROHA CELL PLUS GLUE THICKNESS'                       80
C WAFER CARRIER RAILS WIDTH                                               81
      SWCA_WAFCARWD=1.5                                                   81
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          81
C WAFER CARRIER THICKNESS                                                 82
      SWCA_WAFCARTH=0.0300                                                82
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            82
C WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIONS)                   83
      SWCA_WAFERGAP=0.05                                                  83
      CSWCA_WAFERGAP='WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIO  83
     *NS)'                                                                83
C DRIFT DIRECTION                                                         84
      SWCA_DRIFT=1                                                        84
      CSWCA_DRIFT='DRIFT DIRECTION'                                       84
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        85
      SWCA_STRUTLEN=1.0                                                   85
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   85
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    86
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           86
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   86
C *                                                                       86
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    86
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           86
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   86
C *                                                                       87
      CALL AGSBEGM(MODULE,IPRIN)                                          87
      IF (IPRIN.LT.0) RETURN                                              87
C Filling bank SSUP - SUPPORT STRUCTURES                                  87
      BANK_TITLE='SUPPORT STRUCTURES'                                     87
      BPATH_SSUP='SSUP*'                                                  87
      IDNUM_SSUP(1)=0                                                     88
C GEOMETRY VERSION                                                        88
      SSUP_VERSION=1                                                      88
      CSSUP_VERSION='GEOMETRY VERSION'                                    88
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            89
      SSUP_CABTHK=0.05                                                    89
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         89
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             90
      SSUP_HOSRMN=0.75                                                    90
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          90
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             91
      SSUP_HOSRMX=0.95                                                    91
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          91
C NUMBER OF WATER HOSES                                                   92
      SSUP_NHOSES=10                                                      92
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                92
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             93
      SSUP_WRPMYTHK=0.10                                                  93
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        93
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   94
      SSUP_WRPALTHK=0.01                                                  94
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              94
C SUPPORT CONE THICKNESS                                                  95
      SSUP_GRPHTHK=0.16                                                   95
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              95
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              96
      SSUP_CONE1ZMN=52.23                                                 96
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         96
C LENGTH OF SUPPORT RODS                                                  97
      SSUP_RODLEN=110.8                                                   97
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               97
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               98
      SSUP_RODDIST=17.5                                                   98
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           98
C ID OF CARBON SUPPORT RODS (APPROX)                                      99
      SSUP_RODID=2.5                                                      99
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    99
C OD OF CARBON SUPPORT RODS (APPROX)                                      100
      SSUP_RODOD=3.05                                                     100
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    100
C MINIMUM ID OF CONE 1                                                    101
      SSUP_CON1IDMN=15.67                                                 101
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               101
C MINIMUM ID OF CONE 3 (TBD)                                              102
      SSUP_CON3IDMN=21.67                                                 102
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         102
C MINIMUM ID OF CONE 4 (TBD)                                              103
      SSUP_CON4IDMN=37.4                                                  103
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         103
C MAXIMUM ID OF CONE 4 (TBD)                                              104
      SSUP_CON4IDMX=37.4                                                  104
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         104
C MAXIMUM Z OF CONE 3 (TBD)                                               105
      SSUP_CONE3ZMX=150.0                                                 105
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          105
C MAXIMUM Z OF CONE 4 (TBD)                                               106
      SSUP_CONE4ZMX=229.36                                                106
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          106
C THICKNESS OF AL BRACKETS                                                107
      SSUP_BRATHK=.2                                                      107
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             107
C (Z) THICKNESS OF END RING JOINING BRACKETS                              108
      SSUP_ERJTHK=.1                                                      108
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           108
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          109
      SSUP_ERJWID=2.07                                                    109
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       109
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            110
      SSUP_ERJLEN=5.19                                                    110
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         110
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    111
      SSUP_ERJZDIS=2.0                                                    111
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                111
C ERJ SCREW 1 X POSITION (RADIAL)                                         112
      SSUP_ERJ1X=0.31                                                     112
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       112
C ERJ SCREW 2 X POSITION                                                  113
      SSUP_ERJ2X=1.15                                                     113
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                113
C ERJ SCREW 2 Y POSITION                                                  114
      SSUP_ERJ2Y=0.72                                                     114
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                114
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   115
      SSUP_ERJRAD=10.80                                                   115
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                115
C ERJ SCREW DIAMETER                                                      116
      SSUP_ERJDIA=0.17                                                    116
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   116
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    117
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           117
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   117
C *                                                                       117
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    117
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           117
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   117
C *                                                                       118
      CALL AGSBEGM(MODULE,IPRIN)                                          118
      IF (IPRIN.LT.0) RETURN                                              118
C Filling bank SSUP - SUPPORT STRUCTURES                                  118
      BANK_TITLE='SUPPORT STRUCTURES'                                     118
      BPATH_SSUP='SSUP*'                                                  118
      IDNUM_SSUP(1)=0                                                     119
C GEOMETRY VERSION                                                        119
      SSUP_VERSION=2                                                      119
      CSSUP_VERSION='GEOMETRY VERSION'                                    119
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            120
      SSUP_CABTHK=0.21                                                    120
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         120
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             121
      SSUP_HOSRMN=0.75                                                    121
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          121
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             122
      SSUP_HOSRMX=0.95                                                    122
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          122
C NUMBER OF WATER HOSES                                                   123
      SSUP_NHOSES=10                                                      123
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                123
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             124
      SSUP_WRPMYTHK=0.10                                                  124
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        124
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   125
      SSUP_WRPALTHK=0.01                                                  125
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              125
C SUPPORT CONE THICKNESS                                                  126
      SSUP_GRPHTHK=0.16                                                   126
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              126
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              127
      SSUP_CONE1ZMN=52.23                                                 127
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         127
C LENGTH OF SUPPORT RODS                                                  128
      SSUP_RODLEN=110.8                                                   128
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               128
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               129
      SSUP_RODDIST=17.5                                                   129
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           129
C ID OF CARBON SUPPORT RODS (APPROX)                                      130
      SSUP_RODID=2.5                                                      130
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    130
C OD OF CARBON SUPPORT RODS (APPROX)                                      131
      SSUP_RODOD=3.05                                                     131
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    131
C MINIMUM ID OF CONE 1                                                    132
      SSUP_CON1IDMN=15.67                                                 132
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               132
C MINIMUM ID OF CONE 3 (TBD)                                              133
      SSUP_CON3IDMN=21.67                                                 133
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         133
C MINIMUM ID OF CONE 4 (TBD)                                              134
      SSUP_CON4IDMN=37.4                                                  134
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         134
C MAXIMUM ID OF CONE 4 (TBD)                                              135
      SSUP_CON4IDMX=37.4                                                  135
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         135
C MAXIMUM Z OF CONE 3 (TBD)                                               136
      SSUP_CONE3ZMX=150.0                                                 136
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          136
C MAXIMUM Z OF CONE 4 (TBD)                                               137
      SSUP_CONE4ZMX=229.36                                                137
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          137
C THICKNESS OF AL BRACKETS                                                138
      SSUP_BRATHK=.2                                                      138
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             138
C (Z) THICKNESS OF END RING JOINING BRACKETS                              139
      SSUP_ERJTHK=.1                                                      139
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           139
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          140
      SSUP_ERJWID=2.07                                                    140
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       140
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            141
      SSUP_ERJLEN=5.19                                                    141
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         141
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    142
      SSUP_ERJZDIS=2.0                                                    142
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                142
C ERJ SCREW 1 X POSITION (RADIAL)                                         143
      SSUP_ERJ1X=0.31                                                     143
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       143
C ERJ SCREW 2 X POSITION                                                  144
      SSUP_ERJ2X=1.15                                                     144
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                144
C ERJ SCREW 2 Y POSITION                                                  145
      SSUP_ERJ2Y=0.72                                                     145
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                145
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   146
      SSUP_ERJRAD=10.80                                                   146
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                146
C ERJ SCREW DIAMETER                                                      147
      SSUP_ERJDIA=0.17                                                    147
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   147
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    148
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           148
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   148
C *                                                                       148
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    148
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           148
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   148
C *                                                                       149
      CALL AGSBEGM(MODULE,IPRIN)                                          149
      IF (IPRIN.LT.0) RETURN                                              149
C Filling bank SSUB - BEAMPIPE SUPPORT                                    149
      BANK_TITLE='BEAMPIPE SUPPORT'                                       149
      BPATH_SSUB='SSUB*'                                                  149
      IDNUM_SSUB(1)=0                                                     150
C GEOMETRY VERSION                                                        150
      SSUB_VERSION=1                                                      150
      CSSUB_VERSION='GEOMETRY VERSION'                                    150
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  151
      SSUB_KMOUNTID=31.34                                                 151
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             151
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  152
      SSUB_KMOUNTOD=38.96                                                 152
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             152
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    153
      SSUB_KMNTTHK=0.64                                                   153
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                153
C OD OF CUTOUT IN KINEMATIC MOUNT                                         154
      SSUB_KMCUTOD=18.31                                                  154
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     154
C ID OF CUTOUT IN KINEMATIC MOUNT                                         155
      SSUB_KMCUTID=14                                                     155
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     155
C OPENING ANGLE OF CUTOUT                                                 156
      SSUB_KMCUTOA=38                                                     156
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             156
C OFFSET OF CUTOUT CENTER FROM AXIS                                       157
      SSUB_KMCUTOFF=26.58                                                 157
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  157
C ID OF BEAMPIPE SUPPORT RING                                             158
      SSUB_SRINGID=8.47                                                   158
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         158
C OD OF BEAMPIPE SUPPORT RING                                             159
      SSUB_SRINGOD=12.78                                                  159
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         159
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     160
      SSUB_SRINGTHK=0.64                                                  160
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                160
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     161
      SSUB_SRCUTPHI=38                                                    161
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                161
C SUPPORT RING CUTOUT WIDTH                                               162
      SSUB_SRCUTWID=3.63                                                  162
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          162
C SUPPORT RING CUTOUT DEPTH                                               163
      SSUB_SRCUTOUT=5.08                                                  163
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          163
C SUPPORT RING CUTOUT START                                               164
      SSUB_SRCUTIN=3.5                                                    164
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           164
C SUPPORT ROLLER ID                                                       165
      SSUB_SROLLID=0.2                                                    165
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   165
C SUPPORT ROLLER OD                                                       166
      SSUB_SROLLOD=0.62                                                   166
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   166
C SUPPORT ROLLER LENGTH                                                   167
      SSUB_SROLLLEN=2.54                                                  167
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              167
C SUPPORT ROLLER AXIS LENGTH                                              168
      SSUB_SWIRELEN=5.08                                                  168
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         168
C MOUNTING BLOCK HEIGHT                                                   169
      SSUB_MBLKHGH=0.97                                                   169
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               169
C OUTER MOUNTING BLOCK WIDTH                                              170
      SSUB_MBLKOWID=2.54                                                  170
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         170
C OUTER MOUNTING BLOCK LENGTH                                             171
      SSUB_MBLKOLEN=1.27                                                  171
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        171
C INNER MOUNTING BLOCK WIDTH                                              172
      SSUB_MBLKIWID=3.175                                                 172
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         172
C INNER MOUNTING BLOCK LENGTH                                             173
      SSUB_MBLKILEN=1.27                                                  173
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        173
C OUTER MOUNTING BLOCK AT RADIUS                                          174
      SSUB_MBLKORAD=17.4                                                  174
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     174
C INNER MOUNTING BLOCK AT RADIUS                                          175
      SSUB_MBLKIRAD=5.42                                                  175
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     175
C MOUNTING SUPPORT ROD DIAMETER                                           176
      SSUB_MRODDIA=0.32                                                   176
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       176
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    177
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           177
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   177
C *                                                                       177
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    177
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           177
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   177
C *                                                                       178
      CALL AGSBEGM(MODULE,IPRIN)                                          178
      IF (IPRIN.LT.0) RETURN                                              178
C Filling bank SERG - END RINGS                                           178
      BANK_TITLE='END RINGS'                                              178
      BPATH_SERG='SERG*'                                                  178
      IDNUM_SERG(1)=0                                                     179
C GEOMETRY VERSION                                                        179
      SERG_VERSION=1                                                      179
      CSERG_VERSION='GEOMETRY VERSION'                                    179
C INNER END RING TUBE MAXIMUM RADIUS                                      180
      SERG_IRNGTRMX=9.703                                                 180
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 180
C INNER END RING POLYGON MINIMUM RADIUS                                   181
      SERG_IRNGPRMN=7.671                                                 181
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              181
C OUTER END RING MINIMUM RADIUS                                           182
      SERG_ORNGRMIN=11.900                                                182
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      182
C OUTER END RING MAXIMUM RADIUS                                           183
      SERG_ORNGRMAX=13.805                                                183
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      183
C END RING THICKNESS                                                      184
      SERG_ENDRNGTH=0.2                                                   184
      CSERG_ENDRNGTH='END RING THICKNESS'                                 184
C MINIMUM Z FOR END RINGS                                                 185
      SERG_ENDRNGZM=23.01                                                 185
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            185
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    186
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           186
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   186
C *                                                                       186
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    186
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           186
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   186
C *                                                                       187
      CALL AGSBEGM(MODULE,IPRIN)                                          187
      IF (IPRIN.LT.0) RETURN                                              187
C Filling bank SWAM - WATER MANIFOLD                                      187
      BANK_TITLE='WATER MANIFOLD'                                         187
      BPATH_SWAM='SWAM*'                                                  187
      IDNUM_SWAM(1)=0                                                     188
C GEOMETRY VERSION                                                        188
      SWAM_VERSION=1                                                      188
      CSWAM_VERSION='GEOMETRY VERSION'                                    188
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                189
      SWAM_ZMIN=33.9                                                      189
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               189
C WATER MANIFOLD FULL LENGTH                                              190
      SWAM_LEN=16.0                                                       190
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              190
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         191
      SWAM_RMIN=15.24                                                     191
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        191
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         192
      SWAM_RMAX=16.83                                                     192
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        192
C TRANSITION BOARD THICKNESS                                              193
      SWAM_TBRDTHK=0.1                                                    193
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          193
C WATER MANIFOLD WALL THICKNESS                                           194
      SWAM_WALLTHK=0.1                                                    194
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       194
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    195
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           195
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   195
C *                                                                       195
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    195
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           195
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   195
C *                                                                       196
      CALL AGSBEGM(MODULE,IPRIN)                                          196
      IF (IPRIN.LT.0) RETURN                                              196
C Filling bank SELC - ELECTRONICS CARRIER DATA                            196
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               196
      BPATH_SELC='SELC*'                                                  196
      IDNUM_SELC(1)=0                                                     197
C GEOMETRY VERSION                                                        197
      SELC_VERSION=1                                                      197
      CSELC_VERSION='GEOMETRY VERSION'                                    197
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              198
      SELC_BETHK=0.0500                                                   198
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            198
C THICKNESS OF WATER CHANNEL                                              199
      SELC_WATTHK=0.0750                                                  199
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           199
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    200
      SELC_BEOTHK=0.0500                                                  200
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 200
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                201
      SELC_DYETHK=0.0340                                                  201
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             201
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                202
      SELC_DYEWID=0.3                                                     202
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             202
C SPACE IC CHIPS                                                          203
      SELC_DYESPC=0.1                                                     203
      CSELC_DYESPC='SPACE IC CHIPS'                                       203
C ELECTRONICS CARRIER WIDTH                                               204
      SELC_ELCAWID=2.0                                                    204
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           204
C THICKNESS OF AG-PD CONDUCTOR                                            205
      SELC_AGPDTHK=0.0030                                                 205
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        205
C THICKNESS OF GLASS INSULATOR                                            206
      SELC_GLASSTHK=0.0150                                                206
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       206
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        207
      SELC_CABTHK=0.0033                                                  207
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     207
C CABLE WIDTH                                                             208
      SELC_CABWID=0.6                                                     208
      CSELC_CABWID='CABLE WIDTH'                                          208
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    209
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           209
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   209
C *                                                                       209
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    209
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           209
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   209
C *                                                                       210
      CALL AGSBEGM(MODULE,IPRIN)                                          210
      IF (IPRIN.LT.0) RETURN                                              210
C Filling bank SVTL - SINGLE LAYER DATA                                   210
      BANK_TITLE='SINGLE LAYER DATA'                                      210
      BPATH_SVTL='SVTL*'                                                  210
      IDNUM_SVTL(1)=0                                                     211
C LAYER NUMBER                                                            211
      SVTL_LAYER=1                                                        211
      CSVTL_LAYER='LAYER NUMBER'                                          211
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 212
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     212
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              212
C NUMBER OF LADDER                                                        213
      SVTL_NLADDER=4                                                      213
      CSVTL_NLADDER='NUMBER OF LADDER'                                    213
C NUMBER OF WAFERS                                                        214
      SVTL_NWAFER=4                                                       214
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     214
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        215
      SVTL_BAREEDGE=1.0                                                   215
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   215
C PCB LENGTH                                                              216
      SVTL_PCBLEN=14.9                                                    216
      CSVTL_PCBLEN='PCB LENGTH'                                           216
C PCB WIDTH                                                               217
      SVTL_PCBWIDTH=6.3                                                   217
      CSVTL_PCBWIDTH='PCB WIDTH'                                          217
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            218
      SVTL_PCBTHK=0.1                                                     218
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  218
     *R COPPER'                                                           218
C GAP BETWEEN THE PCB AND THE SI WAFER                                    219
      SVTL_PCBGAP=0.2                                                     219
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 219
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    220
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           220
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   220
C *                                                                       220
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    220
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           220
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   220
C *                                                                       221
      CALL AGSBEGM(MODULE,IPRIN)                                          221
      IF (IPRIN.LT.0) RETURN                                              221
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             221
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                221
      BPATH_SVTL='SVTL*'                                                  221
      IDNUM_SVTL(1)=0                                                     222
C LAYER NUMBER                                                            222
      SVTL_LAYER=2                                                        222
      CSVTL_LAYER='LAYER NUMBER'                                          222
C LAYER RADIUS                                                            223
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     223
      CSVTL_RADIUS='LAYER RADIUS'                                         223
C PCB LENGTH                                                              224
      SVTL_PCBLEN=14.9                                                    224
      CSVTL_PCBLEN='PCB LENGTH'                                           224
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    225
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           225
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   225
C *                                                                       225
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    225
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           225
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   225
C *                                                                       226
      CALL AGSBEGM(MODULE,IPRIN)                                          226
      IF (IPRIN.LT.0) RETURN                                              226
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             226
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                226
      BPATH_SVTL='SVTL*'                                                  226
      IDNUM_SVTL(1)=0                                                     227
C LAYER NUMBER                                                            227
      SVTL_LAYER=3                                                        227
      CSVTL_LAYER='LAYER NUMBER'                                          227
C LAYER RADIUS                                                            228
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     228
      CSVTL_RADIUS='LAYER RADIUS'                                         228
C NUMBER OF LADDER                                                        229
      SVTL_NLADDER=6                                                      229
      CSVTL_NLADDER='NUMBER OF LADDER'                                    229
C NUMBER OF WAFERS                                                        230
      SVTL_NWAFER=6                                                       230
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     230
C PCB LENGTH                                                              231
      SVTL_PCBLEN=7.5                                                     231
      CSVTL_PCBLEN='PCB LENGTH'                                           231
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    232
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           232
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   232
C *                                                                       232
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    232
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           232
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   232
C *                                                                       233
      CALL AGSBEGM(MODULE,IPRIN)                                          233
      IF (IPRIN.LT.0) RETURN                                              233
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             233
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                233
      BPATH_SVTL='SVTL*'                                                  233
      IDNUM_SVTL(1)=0                                                     234
C LAYER NUMBER                                                            234
      SVTL_LAYER=4                                                        234
      CSVTL_LAYER='LAYER NUMBER'                                          234
C LAYER RADIUS                                                            235
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     235
      CSVTL_RADIUS='LAYER RADIUS'                                         235
C PCB LENGTH                                                              236
      SVTL_PCBLEN=7.5                                                     236
      CSVTL_PCBLEN='PCB LENGTH'                                           236
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    237
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           237
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   237
C *                                                                       237
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    237
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           237
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   237
C *                                                                       238
      CALL AGSBEGM(MODULE,IPRIN)                                          238
      IF (IPRIN.LT.0) RETURN                                              238
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             238
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                238
      BPATH_SVTL='SVTL*'                                                  238
      IDNUM_SVTL(1)=0                                                     239
C LAYER NUMBER                                                            239
      SVTL_LAYER=5                                                        239
      CSVTL_LAYER='LAYER NUMBER'                                          239
C LAYER RADIUS                                                            240
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     240
      CSVTL_RADIUS='LAYER RADIUS'                                         240
C NUMBER OF LADDER                                                        241
      SVTL_NLADDER=8                                                      241
      CSVTL_NLADDER='NUMBER OF LADDER'                                    241
C NUMBER OF WAFERS                                                        242
      SVTL_NWAFER=7                                                       242
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     242
C PCB LENGTH                                                              243
      SVTL_PCBLEN=4.4                                                     243
      CSVTL_PCBLEN='PCB LENGTH'                                           243
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    244
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           244
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   244
C *                                                                       244
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    244
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           244
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   244
C *                                                                       245
      CALL AGSBEGM(MODULE,IPRIN)                                          245
      IF (IPRIN.LT.0) RETURN                                              245
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             245
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                245
      BPATH_SVTL='SVTL*'                                                  245
      IDNUM_SVTL(1)=0                                                     246
C LAYER NUMBER                                                            246
      SVTL_LAYER=6                                                        246
      CSVTL_LAYER='LAYER NUMBER'                                          246
C LAYER RADIUS                                                            247
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     247
      CSVTL_RADIUS='LAYER RADIUS'                                         247
C PCB LENGTH                                                              248
      SVTL_PCBLEN=4.4                                                     248
      CSVTL_PCBLEN='PCB LENGTH'                                           248
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    249
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           249
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   249
C *                                                                       249
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    249
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           249
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   249
C *                                                                       250
      CALL AGSBEGM(MODULE,IPRIN)                                          250
      IF (IPRIN.LT.0) RETURN                                              250
C Filling bank SSLD - SHIELDING PARAMETERS                                250
      BANK_TITLE='SHIELDING PARAMETERS'                                   250
      BPATH_SSLD='SSLD*'                                                  250
      IDNUM_SSLD(1)=0                                                     251
C GEOMETRY VERSION                                                        251
      SSLD_VERSION=1                                                      251
      CSSLD_VERSION='GEOMETRY VERSION'                                    251
C INNER SHIELD CYLINDER, INNER RADIUS                                     252
      SSLD_SINRINN=5                                                      252
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 252
C INNER SHIELD CYLINDER, OUTER RADIUS                                     253
      SSLD_SINROUT=5.008                                                  253
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 253
C INNER SHIELD CYLINDER, HALF LENGTH                                      254
      SSLD_SINLEN=53.5                                                    254
      CSSLD_SINLEN='INNER SHIELD CYLINDER, HALF LENGTH'                   254
C SEPARATION SHIELD CYLINDER, INNER RADIUS                                255
      SSLD_SSEPRINN=22                                                    255
      CSSLD_SSEPRINN='SEPARATION SHIELD CYLINDER, INNER RADIUS'           255
C SEPARATION SHIELD CYLINDER, OUTER RADIUS                                256
      SSLD_SSEPROUT=22.018                                                256
      CSSLD_SSEPROUT='SEPARATION SHIELD CYLINDER, OUTER RADIUS'           256
C SEPARATION SHIELD CYLINDER, HALF LENGTH                                 257
      SSLD_SSEPLEN=55.4                                                   257
      CSSLD_SSEPLEN='SEPARATION SHIELD CYLINDER, HALF LENGTH'             257
C OUTER SHIELD CYLINDER, INNER RADIUS                                     258
      SSLD_SOUTRINN=29.5                                                  258
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                258
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     259
      SSLD_SOUTROUT=29.52                                                 259
      CSSLD_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                259
C OUTER SHIELD CYLINDER, HALF LENGTH                                      260
      SSLD_SOUTLEN=65.4                                                   260
      CSSLD_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  260
C ALUMINUM SHIELD MESH INNER DIAMETER                                     261
      SSLD_ALMESHID=9.7                                                   261
      CSSLD_ALMESHID='ALUMINUM SHIELD MESH INNER DIAMETER'                261
C ALUMINUM SHIELD MESH OUTER DIAMETER                                     262
      SSLD_ALMESHOD=44                                                    262
      CSSLD_ALMESHOD='ALUMINUM SHIELD MESH OUTER DIAMETER'                262
C ALUMINUM SHIELD MESH EFFECTIVE THICKNESS                                263
      SSLD_ALMSHTHK=0.03                                                  263
      CSSLD_ALMSHTHK='ALUMINUM SHIELD MESH EFFECTIVE THICKNESS'           263
C ALUMINUM SHIELD MESH Z POSITION                                         264
      SSLD_ALMSHPOS=53.5                                                  264
      CSSLD_ALMSHPOS='ALUMINUM SHIELD MESH Z POSITION'                    264
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    265
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           265
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   265
C *                                                                       265
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    265
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           265
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   265
C *                                                                       266
      CALL AGSBEGM(MODULE,IPRIN)                                          266
      IF (IPRIN.LT.0) RETURN                                              266
C Filling bank SSLD - SHIELDING PARAMETERS                                266
      BANK_TITLE='SHIELDING PARAMETERS'                                   266
      BPATH_SSLD='SSLD*'                                                  266
      IDNUM_SSLD(1)=0                                                     267
C GEOMETRY VERSION                                                        267
      SSLD_VERSION=2                                                      267
      CSSLD_VERSION='GEOMETRY VERSION'                                    267
C INNER SHIELD CYLINDER, INNER RADIUS                                     268
      SSLD_SINRINN=5.9                                                    268
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 268
C INNER SHIELD CYLINDER, OUTER RADIUS                                     269
      SSLD_SINROUT=5.908                                                  269
      CSSLD_SINROUT='INNER SHIELD CYLINDER, OUTER RADIUS'                 269
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    270
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           270
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   270
C *                                                                       270
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLD',    270
     *BPATH_SSLD,IDNUM_SSLD,1, MAP_SSLD,NAMES_SSLD,COMMEN_SSLD,           270
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   270
C *                                                                       271
      DO 5011 I=1,4                                                       271
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 272
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 273
5011  CONTINUE                                                            274
5012  CONTINUE                                                            274
C *                                                                       275
      CALL AGSBEGM(MODULE,IPRIN)                                          275
      IF (IPRIN.LT.0) RETURN                                              275
C Filling bank SCBP - CABLING                                             275
      BANK_TITLE='CABLING'                                                275
      BPATH_SCBP='SCBP*'                                                  275
      IDNUM_SCBP(1)=0                                                     276
C LAYER                                                                   276
      SCBP_LAYER=1                                                        276
      CSCBP_LAYER='LAYER'                                                 276
C LENGTH                                                                  277
      SCBP_LEN=1.85                                                       277
      CSCBP_LEN='LENGTH'                                                  277
C MIN RADIUS CLOSER TO WAFERS                                             278
      SCBP_RMIN1=RAD_CONES_IN(1)                                          278
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           278
C MIN RADIUS FURTHER FROM WAFERS                                          279
      SCBP_RMIN2=RAD_CONES_OUT(1)                                         279
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        279
C VOLUME OF COPPER, LV+HV CABLES                                          280
      SCBP_VOL=7.24+3.21                                                  280
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          280
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    281
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           281
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   281
C *                                                                       281
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    281
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           281
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   281
C *                                                                       282
      CALL AGSBEGM(MODULE,IPRIN)                                          282
      IF (IPRIN.LT.0) RETURN                                              282
C Filling bank SCBP - CABLING                                             282
      BANK_TITLE='CABLING'                                                282
      BPATH_SCBP='SCBP*'                                                  282
      IDNUM_SCBP(1)=0                                                     283
C LAYER                                                                   283
      SCBP_LAYER=2                                                        283
      CSCBP_LAYER='LAYER'                                                 283
C MIN RADIUS CLOSER TO WAFERS                                             284
      SCBP_RMIN1=RAD_CONES_IN(2)                                          284
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           284
C MIN RADIUS FURTHER FROM WAFERS                                          285
      SCBP_RMIN2=RAD_CONES_OUT(2)                                         285
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        285
C VOLUME OF COPPER, LV+HV CABLES                                          286
      SCBP_VOL=15.54+5.7                                                  286
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          286
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    287
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           287
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   287
C *                                                                       287
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    287
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           287
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   287
C *                                                                       288
      CALL AGSBEGM(MODULE,IPRIN)                                          288
      IF (IPRIN.LT.0) RETURN                                              288
C Filling bank SCBP - CABLING                                             288
      BANK_TITLE='CABLING'                                                288
      BPATH_SCBP='SCBP*'                                                  288
      IDNUM_SCBP(1)=0                                                     289
C LAYER                                                                   289
      SCBP_LAYER=3                                                        289
      CSCBP_LAYER='LAYER'                                                 289
C MIN RADIUS CLOSER TO WAFERS                                             290
      SCBP_RMIN1=RAD_CONES_IN(3)                                          290
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           290
C MIN RADIUS FURTHER FROM WAFERS                                          291
      SCBP_RMIN2=RAD_CONES_OUT(3)                                         291
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        291
C VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE                   292
      SCBP_VOL=4.05+2.02+3.67+1.69                                        292
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES -- 3+4 LAYERS COALESCE'   292
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    293
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           293
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   293
C *                                                                       293
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    293
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           293
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   293
C *                                                                       294
      CALL AGSBEGM(MODULE,IPRIN)                                          294
      IF (IPRIN.LT.0) RETURN                                              294
C Filling bank SCBP - CABLING                                             294
      BANK_TITLE='CABLING'                                                294
      BPATH_SCBP='SCBP*'                                                  294
      IDNUM_SCBP(1)=0                                                     295
C LAYER (FORMER 5TH)                                                      295
      SCBP_LAYER=4                                                        295
      CSCBP_LAYER='LAYER (FORMER 5TH)'                                    295
C MIN RADIUS CLOSER TO WAFERS                                             296
      SCBP_RMIN1=RAD_CONES_IN(4)                                          296
      CSCBP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           296
C MIN RADIUS FURTHER FROM WAFERS                                          297
      SCBP_RMIN2=RAD_CONES_OUT(4)                                         297
      CSCBP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        297
C VOLUME OF COPPER, LV+HV CABLES                                          298
      SCBP_VOL=6.95+2.43                                                  298
      CSCBP_VOL='VOLUME OF COPPER, LV+HV CABLES'                          298
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    299
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           299
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   299
C *                                                                       299
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SCBP',    299
     *BPATH_SCBP,IDNUM_SCBP,1, MAP_SCBP,NAMES_SCBP,COMMEN_SCBP,           299
     *BANK_SCBP,LEN_SCBP,LLEN_SCBP,LINK_SCBP,FLAG_SCBP)                   299
C *                                                                       300
      CALL AGSBEGM(MODULE,IPRIN)                                          300
      IF (IPRIN.LT.0) RETURN                                              300
C Filling bank SFEP - WATER FEED                                          300
      BANK_TITLE='WATER FEED'                                             300
      BPATH_SFEP='SFEP*'                                                  300
      IDNUM_SFEP(1)=0                                                     301
C LAYER                                                                   301
      SFEP_LAYER=1                                                        301
      CSFEP_LAYER='LAYER'                                                 301
C LENGTH                                                                  302
      SFEP_LEN=1.85                                                       302
      CSFEP_LEN='LENGTH'                                                  302
C MIN RADIUS CLOSER TO WAFERS                                             303
      SFEP_RMIN1=0.5*(RAD_CONES_IN(1) +RAD_CONES_IN(2))                   303
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           303
C MIN RADIUS FURTHER FROM WAFERS                                          304
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(1)+RAD_CONES_OUT(2))                  304
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        304
C VOLUME OF WATER                                                         305
      SFEP_VOL=16.0                                                       305
      CSFEP_VOL='VOLUME OF WATER'                                         305
C VOLUME OF PLASTIC                                                       306
      SFEP_VOLPLAST=38.4                                                  306
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  306
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    307
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           307
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   307
C *                                                                       307
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    307
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           307
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   307
C *                                                                       308
      CALL AGSBEGM(MODULE,IPRIN)                                          308
      IF (IPRIN.LT.0) RETURN                                              308
C Filling bank SFEP - WATER FEED                                          308
      BANK_TITLE='WATER FEED'                                             308
      BPATH_SFEP='SFEP*'                                                  308
      IDNUM_SFEP(1)=0                                                     309
C LAYER                                                                   309
      SFEP_LAYER=2                                                        309
      CSFEP_LAYER='LAYER'                                                 309
C MIN RADIUS CLOSER TO WAFERS                                             310
      SFEP_RMIN1=0.5*(RAD_CONES_IN(2) +RAD_CONES_IN(3))                   310
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           310
C MIN RADIUS FURTHER FROM WAFERS                                          311
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(2)+RAD_CONES_OUT(3))                  311
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        311
C VOLUME OF WATER                                                         312
      SFEP_VOL=24.0                                                       312
      CSFEP_VOL='VOLUME OF WATER'                                         312
C VOLUME OF PLASTIC                                                       313
      SFEP_VOLPLAST=57.6                                                  313
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  313
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    314
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           314
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   314
C *                                                                       314
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    314
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           314
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   314
C *                                                                       315
      CALL AGSBEGM(MODULE,IPRIN)                                          315
      IF (IPRIN.LT.0) RETURN                                              315
C Filling bank SFEP - WATER FEED                                          315
      BANK_TITLE='WATER FEED'                                             315
      BPATH_SFEP='SFEP*'                                                  315
      IDNUM_SFEP(1)=0                                                     316
C LAYER                                                                   316
      SFEP_LAYER=3                                                        316
      CSFEP_LAYER='LAYER'                                                 316
C MIN RADIUS CLOSER TO WAFERS                                             317
      SFEP_RMIN1=0.5*(RAD_CONES_IN(3) +RAD_CONES_IN(4))                   317
      CSFEP_RMIN1='MIN RADIUS CLOSER TO WAFERS'                           317
C MIN RADIUS FURTHER FROM WAFERS                                          318
      SFEP_RMIN2=0.5*(RAD_CONES_OUT(3)+RAD_CONES_OUT(4))                  318
      CSFEP_RMIN2='MIN RADIUS FURTHER FROM WAFERS'                        318
C VOLUME OF WATER                                                         319
      SFEP_VOL=32                                                         319
      CSFEP_VOL='VOLUME OF WATER'                                         319
C VOLUME OF PLASTIC                                                       320
      SFEP_VOLPLAST=76.8                                                  320
      CSFEP_VOLPLAST='VOLUME OF PLASTIC'                                  320
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    321
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           321
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   321
C *                                                                       321
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFEP',    321
     *BPATH_SFEP,IDNUM_SFEP,1, MAP_SFEP,NAMES_SFEP,COMMEN_SFEP,           321
     *BANK_SFEP,LEN_SFEP,LLEN_SFEP,LINK_SFEP,FLAG_SFEP)                   321
C *                                                                       322
      CALL AGSBEGM(MODULE,IPRIN)                                          322
      IF (IPRIN.LT.0) RETURN                                              322
C Filling bank SWCX - SEGMENTS OF THE WATER DISTRIBUTION PIPES            322
      BANK_TITLE='SEGMENTS OF THE WATER DISTRIBUTION PIPES'               322
      BPATH_SWCX='SWCX*'                                                  322
      IDNUM_SWCX(1)=0                                                     323
C VERSION                                                                 323
      SWCX_LAYER=1                                                        323
      CSWCX_LAYER='VERSION'                                               323
C OF THE RING IN THE Z DIRECTION                                          324
      SWCX_LENGTH=2.8                                                     324
      CSWCX_LENGTH='OF THE RING IN THE Z DIRECTION'                       324
C THICKNESS OF THE MOTHER LAYER                                           325
      SWCX_DR=0.72                                                        325
      CSWCX_DR='THICKNESS OF THE MOTHER LAYER'                            325
C INNER PLASTIC TUBE RADIUS                                               326
      SWCX_RAD=0.2                                                        326
      CSWCX_RAD='INNER PLASTIC TUBE RADIUS'                               326
C FROM THE EDGE OF THE LADDER SUPPORT, INWARD                             327
      SWCX_OFFSET=-2.0                                                    327
      CSWCX_OFFSET='FROM THE EDGE OF THE LADDER SUPPORT, INWARD'          327
C RADIAL OFFSET                                                           328
      SWCX_ROFFSET=1.0                                                    328
      CSWCX_ROFFSET='RADIAL OFFSET'                                       328
C THICKNESS OF THE PLASTIC PIPE WALL                                      329
      SWCX_WALL=0.16                                                      329
      CSWCX_WALL='THICKNESS OF THE PLASTIC PIPE WALL'                     329
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    330
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           330
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   330
C *                                                                       330
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCX',    330
     *BPATH_SWCX,IDNUM_SWCX,1, MAP_SWCX,NAMES_SWCX,COMMEN_SWCX,           330
     *BANK_SWCX,LEN_SWCX,LLEN_SWCX,LINK_SWCX,FLAG_SWCX)                   330
C *                                                                       331
      CALL AGSBEGM(MODULE,IPRIN)                                          331
      IF (IPRIN.LT.0) RETURN                                              331
C Filling bank SOUP - MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS      331
      BANK_TITLE='MOTHER OF THE OUTER SHIELDING CAGE, PARAMETERS'         331
      BPATH_SOUP='SOUP*'                                                  331
      IDNUM_SOUP(1)=0                                                     332
C VERSION                                                                 332
      SOUP_VERSION=1                                                      332
      CSOUP_VERSION='VERSION'                                             332
C LENGTH                                                                  333
      SOUP_LENGTH=82.5                                                    333
      CSOUP_LENGTH='LENGTH'                                               333
C OUTER RADIUS OF THE SHIELD                                              334
      SOUP_ROUT=19.41                                                     334
      CSOUP_ROUT='OUTER RADIUS OF THE SHIELD'                             334
C DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF THE MOTHER)     335
      SOUP_DR=0.711                                                       335
      CSOUP_DR='DIAMETER OF THE TUBES CONSTITUTING THE CAGE (ALSO DR OF   335
     *THE MOTHER)'                                                        335
C STARTING ANGLE OF THE MOTHER                                            336
      SOUP_PHI1=0.0                                                       336
      CSOUP_PHI1='STARTING ANGLE OF THE MOTHER'                           336
C FINISHING ANGLE OF THE MOTHER                                           337
      SOUP_PHI2=70.0                                                      337
      CSOUP_PHI2='FINISHING ANGLE OF THE MOTHER'                          337
C OUTER DIAM OF THE CARBON TUBE                                           338
      SOUP_DIAMOUT=0.711                                                  338
      CSOUP_DIAMOUT='OUTER DIAM OF THE CARBON TUBE'                       338
C INNER DIAM OF THE CARBON TUBE                                           339
      SOUP_DIAMIN=0.620                                                   339
      CSOUP_DIAMIN='INNER DIAM OF THE CARBON TUBE'                        339
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    340
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           340
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   340
C *                                                                       340
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SOUP',    340
     *BPATH_SOUP,IDNUM_SOUP,1, MAP_SOUP,NAMES_SOUP,COMMEN_SOUP,           340
     *BANK_SOUP,LEN_SOUP,LLEN_SOUP,LINK_SOUP,FLAG_SOUP)                   340
C use default SVTG bank                                                   341
      BPATH_SVTG='SVTG*'                                                  341
      IDNUM_SVTG(1)=0                                                     341
      CALL AGDGETP('SVTTGEO9','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,  341
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      341
     *LINK_SVTG,FLAG_SVTG)                                                341
C use default SWCA bank                                                   342
      BPATH_SWCA='SWCA*'                                                  342
      IDNUM_SWCA(1)=0                                                     342
      CALL AGDGETP('SVTTGEO9','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,  342
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      342
     *LINK_SWCA,FLAG_SWCA)                                                342
C use default SELC bank                                                   343
      BPATH_SELC='SELC*'                                                  343
      IDNUM_SELC(1)=0                                                     343
      CALL AGDGETP('SVTTGEO9','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  343
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      343
     *LINK_SELC,FLAG_SELC)                                                343
C use SSUP bank                                                           344
      BPATH_SSUP='SSUP*'                                                  344
      IDNUM_SSUP(1)=0                                                     344
      SSUP_VERSION=SVTG_CONEVER                                           344
      CALL AGDGETP('SVTTGEO9','*',-999,'SSUP',NSSUP_VERSION,              344
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        344
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   344
C use default SSUB bank                                                   345
      BPATH_SSUB='SSUB*'                                                  345
      IDNUM_SSUB(1)=0                                                     345
      CALL AGDGETP('SVTTGEO9','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,  345
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      345
     *LINK_SSUB,FLAG_SSUB)                                                345
C use default SERG bank                                                   346
      BPATH_SERG='SERG*'                                                  346
      IDNUM_SERG(1)=0                                                     346
      CALL AGDGETP('SVTTGEO9','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,  346
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      346
     *LINK_SERG,FLAG_SERG)                                                346
C use default SWAM bank                                                   347
      BPATH_SWAM='SWAM*'                                                  347
      IDNUM_SWAM(1)=0                                                     347
      CALL AGDGETP('SVTTGEO9','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,  347
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      347
     *LINK_SWAM,FLAG_SWAM)                                                347
C use default SELC bank                                                   348
      BPATH_SELC='SELC*'                                                  348
      IDNUM_SELC(1)=0                                                     348
      CALL AGDGETP('SVTTGEO9','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  348
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      348
     *LINK_SELC,FLAG_SELC)                                                348
C use SSLD bank                                                           349
      BPATH_SSLD='SSLD*'                                                  349
      IDNUM_SSLD(1)=0                                                     349
      SSLD_VERSION=SVTG_SUPPORTVER                                        349
      CALL AGDGETP('SVTTGEO9','*',-999,'SSLD',NSSLD_VERSION,              349
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        349
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   349
C use default SCBP bank                                                   350
      BPATH_SCBP='SCBP*'                                                  350
      IDNUM_SCBP(1)=0                                                     350
      CALL AGDGETP('SVTTGEO9','*',-999,'SCBP',' ',BANK_SCBP, BPATH_SCBP,  350
     *IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,LLEN_SCBP,      350
     *LINK_SCBP,FLAG_SCBP)                                                350
C use default SWCX bank                                                   351
      BPATH_SWCX='SWCX*'                                                  351
      IDNUM_SWCX(1)=0                                                     351
      CALL AGDGETP('SVTTGEO9','*',-999,'SWCX',' ',BANK_SWCX, BPATH_SWCX,  351
     *IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,LLEN_SWCX,      351
     *LINK_SWCX,FLAG_SWCX)                                                351
C use default SOUP bank                                                   352
      BPATH_SOUP='SOUP*'                                                  352
      IDNUM_SOUP(1)=0                                                     352
      CALL AGDGETP('SVTTGEO9','*',-999,'SOUP',' ',BANK_SOUP, BPATH_SOUP,  352
     *IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,LLEN_SOUP,      352
     *LINK_SOUP,FLAG_SOUP)                                                352
C Check SVTG_SUPPORTVER==2                                                353
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      353
      SVTG_RSIZEMIN = 6.0                                                 354
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
C Check SWAM_LEN>0                                                        365
      IF (SWAM_LEN.GT.0) THEN                                             366
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              367
      CALL AGSRESET                                                       367
      AG_A = 1                                                            367
      AG_Z = 1                                                            367
      AG_W = 2                                                            367
      AG_TITLE = 'H2'                                                     367
      AG_EXNAME = 'COMPONENT'                                             367
      AG_PARLIST = 'A_Z_W'                                                367
      CALL AXCOMPONENT                                                    367
C *                                                                       367
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              367
      CALL AGSRESET                                                       368
      AG_A = 16                                                           368
      AG_Z = 8                                                            368
      AG_W = 1                                                            368
      AG_TITLE = 'O'                                                      368
      AG_EXNAME = 'COMPONENT'                                             368
      AG_PARLIST = 'A_Z_W'                                                368
      CALL AXCOMPONENT                                                    368
C *                                                                       368
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                368
      CALL AGSRESET                                                       369
      AG_DENS = 1.0                                                       369
      AG_TITLE = 'WATER'                                                  369
      AG_EXNAME = 'MIXTURE'                                               369
      AG_PARLIST = 'DENS'                                                 369
      CALL AXMIXTURE                                                      369
C *                                                                       369
      ELSE                                                                369
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             371
      CALL AGSRESET                                                       371
      AG_DENS = 0.0009                                                    371
      AG_TITLE = 'WATER'                                                  371
      AG_EXNAME = 'MIXTURE'                                               371
      AG_PARLIST = 'DENS'                                                 371
      CALL AXMIXTURE                                                      371
C *                                                                       371
      ENDIF                                                               371
C Check SVTG_IFMANY>0                                                     373
      IF (SVTG_IFMANY.GT.0) THEN                                          373
      CALL AGSBEGM(MODULE,IPRIN)                                          374
C Check IPRIN<0                                                           374
      IF (IPRIN.LT.0) RETURN                                              374
C Create SVTT :                                                           374
C Call To Local Procedure SVTT                                            374
      ASSIGN 5030 TO BLOCK_SVTT                                           374
      GO TO  5020                                                         374
5030  CONTINUE                                                            374
C *                                                                       374
C Geant POSITION : SVTT ===> MOTHER='CAVE' KONLY='MANY' <===              375
      CALL AGSRESET                                                       375
      AG_MOTHER = 'CAVE'                                                  375
      AG_KONLY = 'MANY'                                                   375
      AG_TITLE = 'SVTT'                                                   375
      AG_EXNAME = 'POSITION'                                              375
      AG_PARLIST = 'MOTHER_KONLY'                                         375
      CALL AXPOSITION                                                     375
C *                                                                       375
      ELSE                                                                375
      CALL AGSBEGM(MODULE,IPRIN)                                          377
C Check IPRIN<0                                                           377
      IF (IPRIN.LT.0) RETURN                                              377
C Create SVTT :                                                           377
C Call To Local Procedure SVTT                                            377
      ASSIGN 5040 TO BLOCK_SVTT                                           377
      GO TO  5020                                                         377
5040  CONTINUE                                                            377
C *                                                                       377
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           378
      CALL AGSRESET                                                       378
      AG_MOTHER = 'CAVE'                                                  378
      AG_TITLE = 'SVTT'                                                   378
      AG_EXNAME = 'POSITION'                                              378
      AG_PARLIST = 'MOTHER'                                               378
      CALL AXPOSITION                                                     378
C *                                                                       378
      ENDIF                                                               378
C ----------------------------------------------------------------------  380
C ------------ SVTT                                                       380
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
C    Create SBRG :                                                        390
C    Call To Local Procedure SBRG                                         390
         ASSIGN 5060 TO BLOCK_SBRG                                        390
         GO TO  5050                                                      390
5060     CONTINUE                                                         390
C    *                                                                    390
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  391
C    /2 <===                                                              391
         CALL AGSRESET                                                    391
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  391
         AG_TITLE = 'SBRG'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'Z'                                                 391
         CALL AXPOSITION                                                  391
C    *                                                                    391
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  391
C    K/2 <===                                                             391
         CALL AGSRESET                                                    392
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 392
         AG_TITLE = 'SBRG'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'Z'                                                 392
         CALL AXPOSITION                                                  392
C    *                                                                    392
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           392
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             394
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      395
C    Create SOES :                                                        396
C    Call To Local Procedure SOES                                         396
         ASSIGN 5080 TO BLOCK_SOES                                        396
         GO TO  5070                                                      396
5080     CONTINUE                                                         396
C    *                                                                    396
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               397
         CALL AGSRESET                                                    397
         AG_Z = SCREW_Z                                                   397
         AG_KONLY = 'MANY'                                                397
         AG_TITLE = 'SOES'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'Z_KONLY'                                           397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              397
         CALL AGSRESET                                                    398
         AG_Z = -SCREW_Z                                                  398
         AG_KONLY = 'MANY'                                                398
         AG_TITLE = 'SOES'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'Z_KONLY'                                           398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Create SIES :                                                        399
C    Call To Local Procedure SIES                                         399
         ASSIGN 5100 TO BLOCK_SIES                                        399
         GO TO  5090                                                      399
5100     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               400
         CALL AGSRESET                                                    400
         AG_Z = SCREW_Z                                                   400
         AG_KONLY = 'MANY'                                                400
         AG_TITLE = 'SIES'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'Z_KONLY'                                           400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              400
         CALL AGSRESET                                                    401
         AG_Z = -SCREW_Z                                                  401
         AG_KONLY = 'MANY'                                                401
         AG_TITLE = 'SIES'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'Z_KONLY'                                           401
         CALL AXPOSITION                                                  401
C    *                                                                    401
C    Create SCON :                                                        402
C    Call To Local Procedure SCON                                         402
         ASSIGN 5120 TO BLOCK_SCON                                        402
         GO TO  5110                                                      402
5120     CONTINUE                                                         402
C    *                                                                    402
C    Geant POSITION : SCON ===> inherited arguments used <===             403
         CALL AGSRESET                                                    403
         AG_TITLE = 'SCON'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'NONE'                                              403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Geant POSITION : SCON ===> THETAZ=180 <===                           403
         CALL AGSRESET                                                    404
         AG_THETAZ = 180                                                  404
         AG_TITLE = 'SCON'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'THETAZ'                                            404
         CALL AXPOSITION                                                  404
C    *                                                                    404
C    Create SBSP :                                                        405
C    Call To Local Procedure SBSP                                         405
         ASSIGN 5140 TO BLOCK_SBSP                                        405
         GO TO  5130                                                      405
5140     CONTINUE                                                         405
C    *                                                                    405
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     406
         CALL AGSRESET                                                    406
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            406
         AG_TITLE = 'SBSP'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'Z'                                                 406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  406
C    =180 <===                                                            406
         CALL AGSRESET                                                    407
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           407
         AG_THETAZ = 180                                                  407
         AG_TITLE = 'SBSP'                                                407
         AG_EXNAME = 'POSITION'                                           407
         AG_PARLIST = 'Z_THETAZ'                                          407
         CALL AXPOSITION                                                  407
C    *                                                                    407
5021  CALL AGSPOPB                                                        408
      GO TO BLOCK_SVTT, (5020 ,5030,5040)                                 408
C End of Local Procedure                                                  408
C ----------------------------------------------------------------------  409
C ------------ SCON                                                       409
C Begin Local Procedure SCON                                              410
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               410
5110  CONTINUE                                                            410
         AG_TITLE='SCON'                                                  410
         AG_MODULE=MODULE                                                 410
         AG_MARK='V00'                                                    410
         CALL AGSPUSH                                                     410
         AG_MODULE=MODULE                                                 410
C    *                                                                    410
         CONE_THK1=SSUP_GRPHTHK                                           411
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                412
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              413
C    Geant MATERIAL : AIR ===> inherited arguments used <===              414
         CALL AGSRESET                                                    414
         AG_TITLE = 'AIR'                                                 414
         AG_EXNAME = 'MATERIAL'                                           414
         AG_PARLIST = 'NONE'                                              414
         CALL AXMATERIAL                                                  414
C    *                                                                    414
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       414
         CALL AGSRESET                                                    415
         AG_SEEN = 0                                                      415
         AG_COLO = 1                                                      415
         AG_TITLE = 'SCON'                                                415
         AG_EXNAME = 'ATTRIBUTE'                                          415
         AG_PARLIST = 'SEEN_COLO'                                         415
         CALL AXATTRIBUTE                                                 415
C    *                                                                    415
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  415
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  415
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  415
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  415
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  415
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  415
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      415
         CALL AGSRESET                                                    425
         AG_PHI1 = 0                                                      425
         AG_DPHI = 360                                                    425
         AG_NZ = 7                                                        425
         AG_ZI(1)=SSUP_CONE1ZMN                                           425
         AG_ZI(2)=SSUP_RODLEN/2                                           425
         AG_ZI(3)=SSUP_RODLEN/2                                           425
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              425
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              425
         AG_ZI(6)=SSUP_CONE3ZMX                                           425
         AG_ZI(7)=SSUP_CONE4ZMX                                           425
         AG_RMN(1)=SSUP_CON1IDMN                                          425
         AG_RMN(2)=SSUP_CON1IDMN                                          425
         AG_RMN(3)=SSUP_CON1IDMN                                          425
         AG_RMN(4)=SSUP_CON1IDMN                                          425
         AG_RMN(5)=SSUP_CON3IDMN                                          425
         AG_RMN(6)=SSUP_CON4IDMN                                          425
         AG_RMN(7)=SSUP_CON4IDMX                                          425
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                425
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                425
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                425
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                425
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                425
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                425
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                425
         AG_TITLE = 'PCON'                                                425
         AG_EXNAME = 'SHAPE'                                              425
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           425
         CALL AXSHAPE                                                     425
C    *                                                                    425
C    Create SGRA :                                                        426
C    Call To Local Procedure SGRA                                         426
         ASSIGN 5160 TO BLOCK_SGRA                                        426
         GO TO  5150                                                      426
5160     CONTINUE                                                         426
C    *                                                                    426
C    Geant POSITION : SGRA ===> inherited arguments used <===             427
         CALL AGSRESET                                                    427
         AG_TITLE = 'SGRA'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'NONE'                                              427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Create SCMY :                                                        428
C    Call To Local Procedure SCMY                                         428
         ASSIGN 5180 TO BLOCK_SCMY                                        428
         GO TO  5170                                                      428
5180     CONTINUE                                                         428
C    *                                                                    428
C    Geant POSITION : SCMY ===> inherited arguments used <===             429
         CALL AGSRESET                                                    429
         AG_TITLE = 'SCMY'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'NONE'                                              429
         CALL AXPOSITION                                                  429
C    *                                                                    429
5111  CALL AGSPOPB                                                        430
      GO TO BLOCK_SCON, (5110 ,5120)                                      430
C End of Local Procedure                                                  430
C ----------------------------------------------------------------------  431
C ------------ SGRA                                                       431
C Begin Local Procedure SGRA                                              432
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 432
5150  CONTINUE                                                            432
         AG_TITLE='SGRA'                                                  432
         AG_MODULE=MODULE                                                 432
         AG_MARK='V00'                                                    432
         CALL AGSPUSH                                                     432
         AG_MODULE=MODULE                                                 432
C    *                                                                    432
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           433
         CALL AGSRESET                                                    433
         AG_TITLE = 'CARBON'                                              433
         AG_EXNAME = 'MATERIAL'                                           433
         AG_PARLIST = 'NONE'                                              433
         CALL AXMATERIAL                                                  433
C    *                                                                    433
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       433
         CALL AGSRESET                                                    434
         AG_SEEN = 1                                                      434
         AG_COLO = 6                                                      434
         AG_TITLE = 'SGRA'                                                434
         AG_EXNAME = 'ATTRIBUTE'                                          434
         AG_PARLIST = 'SEEN_COLO'                                         434
         CALL AXATTRIBUTE                                                 434
C    *                                                                    434
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  434
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  434
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  434
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  434
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  434
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              434
         CALL AGSRESET                                                    442
         AG_PHI1 = 0                                                      442
         AG_DPHI = 360                                                    442
         AG_NZ = 5                                                        442
         AG_ZI(1)=SSUP_RODLEN/2                                           442
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              442
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              442
         AG_ZI(4)=SSUP_CONE3ZMX                                           442
         AG_ZI(5)=SSUP_CONE4ZMX                                           442
         AG_RMN(1)=SSUP_CON1IDMN                                          442
         AG_RMN(2)=SSUP_CON1IDMN                                          442
         AG_RMN(3)=SSUP_CON3IDMN                                          442
         AG_RMN(4)=SSUP_CON4IDMN                                          442
         AG_RMN(5)=SSUP_CON4IDMX                                          442
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             442
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             442
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             442
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             442
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             442
         AG_TITLE = 'PCON'                                                442
         AG_EXNAME = 'SHAPE'                                              442
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           442
         CALL AXSHAPE                                                     442
C    *                                                                    442
5151  CALL AGSPOPB                                                        443
      GO TO BLOCK_SGRA, (5150 ,5160)                                      443
C End of Local Procedure                                                  443
C ----------------------------------------------------------------------  444
C ------------ SBRG                                                       444
C Begin Local Procedure SBRG                                              445
C SBRG IS THE BRACKET JOINING THE END RINGS                               445
5050  CONTINUE                                                            445
         AG_TITLE='SBRG'                                                  445
         AG_MODULE=MODULE                                                 445
         AG_MARK='V00'                                                    445
         CALL AGSPUSH                                                     445
         AG_MODULE=MODULE                                                 445
C    *                                                                    445
         RIN=SERG_IRNGPRMN                                                446
         ROU=SERG_ORNGRMAX                                                447
C    Geant MATERIAL : AIR ===> inherited arguments used <===              448
         CALL AGSRESET                                                    448
         AG_TITLE = 'AIR'                                                 448
         AG_EXNAME = 'MATERIAL'                                           448
         AG_PARLIST = 'NONE'                                              448
         CALL AXMATERIAL                                                  448
C    *                                                                    448
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       448
         CALL AGSRESET                                                    449
         AG_SEEN = 0                                                      449
         AG_COLO = 1                                                      449
         AG_TITLE = 'SBRG'                                                449
         AG_EXNAME = 'ATTRIBUTE'                                          449
         AG_PARLIST = 'SEEN_COLO'                                         449
         CALL AXATTRIBUTE                                                 449
C    *                                                                    449
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      449
         CALL AGSRESET                                                    450
         AG_RMIN = RIN                                                    450
         AG_RMAX = ROU                                                    450
         AG_DZ = SSUP_ERJTHK/2                                            450
         AG_TITLE = 'TUBE'                                                450
         AG_EXNAME = 'SHAPE'                                              450
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      450
         CALL AXSHAPE                                                     450
C    *                                                                    450
5051  CALL AGSPOPB                                                        451
      GO TO BLOCK_SBRG, (5050 ,5060)                                      451
C End of Local Procedure                                                  451
C ----------------------------------------------------------------------  452
C ------------ SOES                                                       452
C Begin Local Procedure SOES                                              453
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         453
5070  CONTINUE                                                            453
         AG_TITLE='SOES'                                                  453
         AG_MODULE=MODULE                                                 453
         AG_MARK='V00'                                                    453
         CALL AGSPUSH                                                     453
         AG_MODULE=MODULE                                                 453
C    *                                                                    453
C    Geant MATERIAL : AIR ===> inherited arguments used <===              454
         CALL AGSRESET                                                    454
         AG_TITLE = 'AIR'                                                 454
         AG_EXNAME = 'MATERIAL'                                           454
         AG_PARLIST = 'NONE'                                              454
         CALL AXMATERIAL                                                  454
C    *                                                                    454
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       454
         CALL AGSRESET                                                    455
         AG_SEEN = 0                                                      455
         AG_COLO = 1                                                      455
         AG_TITLE = 'SOES'                                                455
         AG_EXNAME = 'ATTRIBUTE'                                          455
         AG_PARLIST = 'SEEN_COLO'                                         455
         CALL AXATTRIBUTE                                                 455
C    *                                                                    455
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  455
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           455
         CALL AGSRESET                                                    457
         AG_RMIN = SERG_ORNGRMIN                                          457
         AG_RMAX = SERG_ORNGRMAX                                          457
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   457
         AG_TITLE = 'TUBE'                                                457
         AG_EXNAME = 'SHAPE'                                              457
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      457
         CALL AXSHAPE                                                     457
C    *                                                                    457
C    Create SOSM :                                                        458
C    Call To Local Procedure SOSM                                         458
         ASSIGN 5200 TO BLOCK_SOSM                                        458
         GO TO  5190                                                      458
5200     CONTINUE                                                         458
C    *                                                                    458
5071  CALL AGSPOPB                                                        459
      GO TO BLOCK_SOES, (5070 ,5080)                                      459
C End of Local Procedure                                                  459
C ----------------------------------------------------------------------  460
C ------------ SIES                                                       460
C Begin Local Procedure SIES                                              461
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         461
5090  CONTINUE                                                            461
         AG_TITLE='SIES'                                                  461
         AG_MODULE=MODULE                                                 461
         AG_MARK='V00'                                                    461
         CALL AGSPUSH                                                     461
         AG_MODULE=MODULE                                                 461
C    *                                                                    461
C    Geant MATERIAL : AIR ===> inherited arguments used <===              462
         CALL AGSRESET                                                    462
         AG_TITLE = 'AIR'                                                 462
         AG_EXNAME = 'MATERIAL'                                           462
         AG_PARLIST = 'NONE'                                              462
         CALL AXMATERIAL                                                  462
C    *                                                                    462
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       462
         CALL AGSRESET                                                    463
         AG_SEEN = 0                                                      463
         AG_COLO = 1                                                      463
         AG_TITLE = 'SIES'                                                463
         AG_EXNAME = 'ATTRIBUTE'                                          463
         AG_PARLIST = 'SEEN_COLO'                                         463
         CALL AXATTRIBUTE                                                 463
C    *                                                                    463
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  463
C    ACK_Z-ENDRNG_Z) <===                                                 463
         CALL AGSRESET                                                    465
         AG_RMIN = IR_RMIN                                                465
         AG_RMAX = SERG_IRNGTRMX                                          465
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   465
         AG_TITLE = 'TUBE'                                                465
         AG_EXNAME = 'SHAPE'                                              465
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      465
         CALL AXSHAPE                                                     465
C    *                                                                    465
C    Create SISM :                                                        466
C    Call To Local Procedure SISM                                         466
         ASSIGN 5220 TO BLOCK_SISM                                        466
         GO TO  5210                                                      466
5220     CONTINUE                                                         466
C    *                                                                    466
5091  CALL AGSPOPB                                                        467
      GO TO BLOCK_SIES, (5090 ,5100)                                      467
C End of Local Procedure                                                  467
C ----------------------------------------------------------------------  468
C ------------ SISM                                                       468
C Begin Local Procedure SISM                                              469
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        469
5210  CONTINUE                                                            469
         AG_TITLE='SISM'                                                  469
         AG_MODULE=MODULE                                                 469
         AG_MARK='V00'                                                    469
         CALL AGSPUSH                                                     469
         AG_MODULE=MODULE                                                 469
C    *                                                                    469
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       470
         CALL AGSRESET                                                    470
         AG_SEEN = 0                                                      470
         AG_COLO = 1                                                      470
         AG_TITLE = 'SISM'                                                470
         AG_EXNAME = 'ATTRIBUTE'                                          470
         AG_PARLIST = 'SEEN_COLO'                                         470
         CALL AXATTRIBUTE                                                 470
C    *                                                                    470
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                470
         CALL AGSRESET                                                    471
         AG_NDIV = 4                                                      471
         AG_IAXIS = 2                                                     471
         AG_C0 = 45                                                       471
         AG_TITLE = 'DIVISION'                                            471
         AG_EXNAME = 'SHAPE'                                              471
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     471
         CALL AXSHAPE                                                     471
C    *                                                                    471
C    Create SCRW :                                                        472
C    Call To Local Procedure SCRW                                         472
         ASSIGN 5240 TO BLOCK_SCRW                                        472
         GO TO  5230                                                      472
5240     CONTINUE                                                         472
C    *                                                                    472
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  473
C    ===                                                                  473
         CALL AGSRESET                                                    473
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      473
         AG_TITLE = 'SCRW'                                                473
         AG_EXNAME = 'POSITION'                                           473
         AG_PARLIST = 'X'                                                 473
         CALL AXPOSITION                                                  473
C    *                                                                    473
5211  CALL AGSPOPB                                                        474
      GO TO BLOCK_SISM, (5210 ,5220)                                      474
C End of Local Procedure                                                  474
C ----------------------------------------------------------------------  475
C ------------ SOSM                                                       475
C Begin Local Procedure SOSM                                              476
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        476
5190  CONTINUE                                                            476
         AG_TITLE='SOSM'                                                  476
         AG_MODULE=MODULE                                                 476
         AG_MARK='V00'                                                    476
         CALL AGSPUSH                                                     476
         AG_MODULE=MODULE                                                 476
C    *                                                                    476
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       477
         CALL AGSRESET                                                    477
         AG_SEEN = 0                                                      477
         AG_COLO = 1                                                      477
         AG_TITLE = 'SOSM'                                                477
         AG_EXNAME = 'ATTRIBUTE'                                          477
         AG_PARLIST = 'SEEN_COLO'                                         477
         CALL AXATTRIBUTE                                                 477
C    *                                                                    477
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                477
         CALL AGSRESET                                                    478
         AG_NDIV = 4                                                      478
         AG_IAXIS = 2                                                     478
         AG_C0 = 45                                                       478
         AG_TITLE = 'DIVISION'                                            478
         AG_EXNAME = 'SHAPE'                                              478
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     478
         CALL AXSHAPE                                                     478
C    *                                                                    478
C    Create SCRW :                                                        479
C    Call To Local Procedure SCRW                                         479
         ASSIGN 5250 TO BLOCK_SCRW                                        479
         GO TO  5230                                                      479
5250     CONTINUE                                                         479
C    *                                                                    479
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  480
C    ===                                                                  480
         CALL AGSRESET                                                    480
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      480
         AG_TITLE = 'SCRW'                                                480
         AG_EXNAME = 'POSITION'                                           480
         AG_PARLIST = 'X'                                                 480
         CALL AXPOSITION                                                  480
C    *                                                                    480
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  480
C    ===                                                                  480
         CALL AGSRESET                                                    481
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      481
         AG_TITLE = 'SCRW'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'X'                                                 481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  481
C    =SSUP_ERJ2Y <===                                                     481
         CALL AGSRESET                                                    482
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      482
         AG_Y = SSUP_ERJ2Y                                                482
         AG_TITLE = 'SCRW'                                                482
         AG_EXNAME = 'POSITION'                                           482
         AG_PARLIST = 'X_Y'                                               482
         CALL AXPOSITION                                                  482
C    *                                                                    482
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  482
C    =-SSUP_ERJ2Y <===                                                    482
         CALL AGSRESET                                                    483
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      483
         AG_Y = -SSUP_ERJ2Y                                               483
         AG_TITLE = 'SCRW'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'X_Y'                                               483
         CALL AXPOSITION                                                  483
C    *                                                                    483
5191  CALL AGSPOPB                                                        484
      GO TO BLOCK_SOSM, (5190 ,5200)                                      484
C End of Local Procedure                                                  484
C ----------------------------------------------------------------------  485
C ------------ SCRW                                                       485
C Begin Local Procedure SCRW                                              486
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   486
5230  CONTINUE                                                            486
         AG_TITLE='SCRW'                                                  486
         AG_MODULE=MODULE                                                 486
         AG_MARK='V00'                                                    486
         CALL AGSPUSH                                                     486
         AG_MODULE=MODULE                                                 486
C    *                                                                    486
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        487
         CALL AGSRESET                                                    487
         AG_TITLE = 'BERILLIUM'                                           487
         AG_EXNAME = 'MATERIAL'                                           487
         AG_PARLIST = 'NONE'                                              487
         CALL AXMATERIAL                                                  487
C    *                                                                    487
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       487
         CALL AGSRESET                                                    488
         AG_SEEN = 1                                                      488
         AG_COLO = 2                                                      488
         AG_TITLE = 'SCRW'                                                488
         AG_EXNAME = 'ATTRIBUTE'                                          488
         AG_PARLIST = 'SEEN_COLO'                                         488
         CALL AXATTRIBUTE                                                 488
C    *                                                                    488
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  488
C    NDRNG_Z) <===                                                        488
         CALL AGSRESET                                                    489
         AG_RMIN = 0                                                      489
         AG_RMAX = SSUP_ERJDIA/2                                          489
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   489
         AG_TITLE = 'TUBE'                                                489
         AG_EXNAME = 'SHAPE'                                              489
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      489
         CALL AXSHAPE                                                     489
C    *                                                                    489
5231  CALL AGSPOPB                                                        490
      GO TO BLOCK_SCRW, (5230 ,5240,5250)                                 490
C End of Local Procedure                                                  490
C ----------------------------------------------------------------------  491
C ------------ SBSP                                                       491
C Begin Local Procedure SBSP                                              492
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              492
5130  CONTINUE                                                            492
         AG_TITLE='SBSP'                                                  492
         AG_MODULE=MODULE                                                 492
         AG_MARK='V00'                                                    492
         CALL AGSPUSH                                                     492
         AG_MODULE=MODULE                                                 492
C    *                                                                    492
C    Geant MATERIAL : AIR ===> inherited arguments used <===              493
         CALL AGSRESET                                                    493
         AG_TITLE = 'AIR'                                                 493
         AG_EXNAME = 'MATERIAL'                                           493
         AG_PARLIST = 'NONE'                                              493
         CALL AXMATERIAL                                                  493
C    *                                                                    493
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       493
         CALL AGSRESET                                                    494
         AG_SEEN = 0                                                      494
         AG_COLO = 1                                                      494
         AG_TITLE = 'SBSP'                                                494
         AG_EXNAME = 'ATTRIBUTE'                                          494
         AG_PARLIST = 'SEEN_COLO'                                         494
         CALL AXATTRIBUTE                                                 494
C    *                                                                    494
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  494
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    494
         CALL AGSRESET                                                    496
         AG_RMIN = SVTG_RSIZEMIN                                          496
         AG_RMAX = SSUB_KMOUNTOD/2                                        496
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              496
         AG_TITLE = 'TUBE'                                                496
         AG_EXNAME = 'SHAPE'                                              496
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      496
         CALL AXSHAPE                                                     496
C    *                                                                    496
C    Create SAKM :                                                        497
C    Call To Local Procedure SAKM                                         497
         ASSIGN 5270 TO BLOCK_SAKM                                        497
         GO TO  5260                                                      497
5270     CONTINUE                                                         497
C    *                                                                    497
C    Geant POSITION : SAKM ===> inherited arguments used <===             498
         CALL AGSRESET                                                    498
         AG_TITLE = 'SAKM'                                                498
         AG_EXNAME = 'POSITION'                                           498
         AG_PARLIST = 'NONE'                                              498
         CALL AXPOSITION                                                  498
C    *                                                                    498
C    Create SBMM :                                                        499
C    Call To Local Procedure SBMM                                         499
         ASSIGN 5290 TO BLOCK_SBMM                                        499
         GO TO  5280                                                      499
5290     CONTINUE                                                         499
C    *                                                                    499
C    Create SBRL :                                                        500
C    Call To Local Procedure SBRL                                         500
         ASSIGN 5310 TO BLOCK_SBRL                                        500
         GO TO  5300                                                      500
5310     CONTINUE                                                         500
C    *                                                                    500
C    Create SBRX :                                                        501
C    Call To Local Procedure SBRX                                         501
         ASSIGN 5330 TO BLOCK_SBRX                                        501
         GO TO  5320                                                      501
5330     CONTINUE                                                         501
C    *                                                                    501
C    *                                                                    502
         I=-1                                                             502
            GOTO 5343                                                     502
5341        I=I+(2)                                                       502
5343        IF((2)*(I-(1)).GT.0) GO TO 5342                               502
C       *                                                                 503
            J=0                                                           503
               GO TO 5353                                                 503
5351           J=J+1                                                      503
5353           IF(J-(1).GT.0) GO TO 5352                                  503
               PHI=I*SSUB_SRCUTPHI+180*J                                  504
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     505
     *         2.0)                                                       505
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  507
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  507
C          ==                                                             507
               CALL AGSRESET                                              507
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               507
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              507
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      507
               AG_ALPHAN=AG_ALPHAN+1                                      507
               AG_ALPHAK(AG_ALPHAN)=3                                     507
               AG_ALPHAL(AG_ALPHAN)=PHI                                   507
               AG_TITLE = 'SBMM'                                          507
               AG_EXNAME = 'POSITION'                                     507
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                507
               CALL AXPOSITION                                            507
C          *                                                              507
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         507
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  510
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  510
C           ORT=ZXY <===                                                  510
               CALL AGSRESET                                              510
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               510
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              510
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      510
               AG_ALPHAN=AG_ALPHAN+1                                      510
               AG_ALPHAK(AG_ALPHAN)=3                                     510
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                510
               AG_ALPHAN=AG_ALPHAN+1                                      510
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           510
               AG_TITLE = 'SBRL'                                          510
               AG_EXNAME = 'POSITION'                                     510
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            510
               CALL AXPOSITION                                            510
C          *                                                              510
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  510
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  510
C           ORT=ZXY <===                                                  510
               CALL AGSRESET                                              512
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               512
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              512
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      512
               AG_ALPHAN=AG_ALPHAN+1                                      512
               AG_ALPHAK(AG_ALPHAN)=3                                     512
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                512
               AG_ALPHAN=AG_ALPHAN+1                                      512
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           512
               AG_TITLE = 'SBRX'                                          512
               AG_EXNAME = 'POSITION'                                     512
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            512
               CALL AXPOSITION                                            512
C          *                                                              512
            GO TO 5351                                                    513
5352        CONTINUE                                                      513
         GO TO 5341                                                       514
5342     CONTINUE                                                         514
C    Create SBSR :                                                        515
C    Call To Local Procedure SBSR                                         515
         ASSIGN 5370 TO BLOCK_SBSR                                        515
         GO TO  5360                                                      515
5370     CONTINUE                                                         515
C    *                                                                    515
C    Geant POSITION : SBSR ===> inherited arguments used <===             516
         CALL AGSRESET                                                    516
         AG_TITLE = 'SBSR'                                                516
         AG_EXNAME = 'POSITION'                                           516
         AG_PARLIST = 'NONE'                                              516
         CALL AXPOSITION                                                  516
C    *                                                                    516
5131  CALL AGSPOPB                                                        517
      GO TO BLOCK_SBSP, (5130 ,5140)                                      517
C End of Local Procedure                                                  517
C ----------------------------------------------------------------------  518
C ------------ SAKM                                                       518
C Begin Local Procedure SAKM                                              519
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   519
5260  CONTINUE                                                            519
         AG_TITLE='SAKM'                                                  519
         AG_MODULE=MODULE                                                 519
         AG_MARK='V00'                                                    519
         CALL AGSPUSH                                                     519
         AG_MODULE=MODULE                                                 519
C    *                                                                    519
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        520
         CALL AGSRESET                                                    520
         AG_TITLE = 'ALUMINIUM'                                           520
         AG_EXNAME = 'MATERIAL'                                           520
         AG_PARLIST = 'NONE'                                              520
         CALL AXMATERIAL                                                  520
C    *                                                                    520
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       520
         CALL AGSRESET                                                    521
         AG_SEEN = 1                                                      521
         AG_COLO = 2                                                      521
         AG_TITLE = 'SAKM'                                                521
         AG_EXNAME = 'ATTRIBUTE'                                          521
         AG_PARLIST = 'SEEN_COLO'                                         521
         CALL AXATTRIBUTE                                                 521
C    *                                                                    521
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   521
C    DZ=SSUB_KMNTTHK/2 <===                                               521
         CALL AGSRESET                                                    523
         AG_RMIN = SSUB_KMOUNTID/2                                        523
         AG_RMAX = SSUB_KMOUNTOD/2                                        523
         AG_DZ = SSUB_KMNTTHK/2                                           523
         AG_TITLE = 'TUBE'                                                523
         AG_EXNAME = 'SHAPE'                                              523
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      523
         CALL AXSHAPE                                                     523
C    *                                                                    523
C    Create SCKM :                                                        524
C    Call To Local Procedure SCKM                                         524
         ASSIGN 5390 TO BLOCK_SCKM                                        524
         GO TO  5380                                                      524
5390     CONTINUE                                                         524
C    *                                                                    524
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         525
         CALL AGSRESET                                                    525
         AG_Y = SSUB_KMCUTOFF                                             525
         AG_KONLY = 'MANY'                                                525
         AG_TITLE = 'SCKM'                                                525
         AG_EXNAME = 'POSITION'                                           525
         AG_PARLIST = 'Y_KONLY'                                           525
         CALL AXPOSITION                                                  525
C    *                                                                    525
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  525
C     <===                                                                525
         CALL AGSRESET                                                    526
         AG_Y = -SSUB_KMCUTOFF                                            526
         AG_ALPHAN=AG_ALPHAN+1                                            526
         AG_ALPHAK(AG_ALPHAN)=3                                           526
         AG_ALPHAL(AG_ALPHAN)=180                                         526
         AG_KONLY = 'MANY'                                                526
         AG_TITLE = 'SCKM'                                                526
         AG_EXNAME = 'POSITION'                                           526
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    526
         CALL AXPOSITION                                                  526
C    *                                                                    526
5261  CALL AGSPOPB                                                        527
      GO TO BLOCK_SAKM, (5260 ,5270)                                      527
C End of Local Procedure                                                  527
C ----------------------------------------------------------------------  528
C ------------ SBMM                                                       528
C Begin Local Procedure SBMM                                              529
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     529
5280  CONTINUE                                                            529
         AG_TITLE='SBMM'                                                  529
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
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       530
         CALL AGSRESET                                                    531
         AG_SEEN = 0                                                      531
         AG_COLO = 1                                                      531
         AG_TITLE = 'SBMM'                                                531
         AG_EXNAME = 'ATTRIBUTE'                                          531
         AG_PARLIST = 'SEEN_COLO'                                         531
         CALL AXATTRIBUTE                                                 531
C    *                                                                    531
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  531
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     531
         CALL AGSRESET                                                    533
         AG_DX = SSUB_MBLKIWID/2                                          533
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           533
         AG_DZ = SSUB_MBLKHGH/2                                           533
         AG_TITLE = 'BOX'                                                 533
         AG_EXNAME = 'SHAPE'                                              533
         AG_PARLIST = 'DX_DY_DZ'                                          533
         CALL AXSHAPE                                                     533
C    *                                                                    533
C    Create SMRD :                                                        534
C    Call To Local Procedure SMRD                                         534
         ASSIGN 5410 TO BLOCK_SMRD                                        534
         GO TO  5400                                                      534
5410     CONTINUE                                                         534
C    *                                                                    534
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      535
     *   SSUB_SROLLOD                                                     535
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  536
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         536
         CALL AGSRESET                                                    536
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       536
         AG_ALPHAN=AG_ALPHAN+1                                            536
         AG_ALPHAK(AG_ALPHAN)=1                                           536
         AG_ALPHAL(AG_ALPHAN)=90                                          536
         AG_TITLE = 'SMRD'                                                536
         AG_EXNAME = 'POSITION'                                           536
         AG_PARLIST = 'Y_ALPHAX'                                          536
         CALL AXPOSITION                                                  536
C    *                                                                    536
C    Create SBMO :                                                        537
C    Call To Local Procedure SBMO                                         537
         ASSIGN 5430 TO BLOCK_SBMO                                        537
         GO TO  5420                                                      537
5430     CONTINUE                                                         537
C    *                                                                    537
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             538
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  539
C    UB_SROLLOD)/2 <===                                                   539
         CALL AGSRESET                                                    539
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       539
         AG_TITLE = 'SBMO'                                                539
         AG_EXNAME = 'POSITION'                                           539
         AG_PARLIST = 'Y'                                                 539
         CALL AXPOSITION                                                  539
C    *                                                                    539
C    Create SBMI :                                                        540
C    Call To Local Procedure SBMI                                         540
         ASSIGN 5450 TO BLOCK_SBMI                                        540
         GO TO  5440                                                      540
5450     CONTINUE                                                         540
C    *                                                                    540
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             541
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  542
C    UB_SROLLOD)/2 <===                                                   542
         CALL AGSRESET                                                    542
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       542
         AG_TITLE = 'SBMI'                                                542
         AG_EXNAME = 'POSITION'                                           542
         AG_PARLIST = 'Y'                                                 542
         CALL AXPOSITION                                                  542
C    *                                                                    542
5281  CALL AGSPOPB                                                        543
      GO TO BLOCK_SBMM, (5280 ,5290)                                      543
C End of Local Procedure                                                  543
C ----------------------------------------------------------------------  544
C ------------ SMRD                                                       544
C Begin Local Procedure SMRD                                              545
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  545
5400  CONTINUE                                                            545
         AG_TITLE='SMRD'                                                  545
         AG_MODULE=MODULE                                                 545
         AG_MARK='V00'                                                    545
         CALL AGSPUSH                                                     545
         AG_MODULE=MODULE                                                 545
C    *                                                                    545
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        546
         CALL AGSRESET                                                    546
         AG_TITLE = 'ALUMINIUM'                                           546
         AG_EXNAME = 'MATERIAL'                                           546
         AG_PARLIST = 'NONE'                                              546
         CALL AXMATERIAL                                                  546
C    *                                                                    546
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       546
         CALL AGSRESET                                                    547
         AG_SEEN = 1                                                      547
         AG_COLO = 2                                                      547
         AG_TITLE = 'SMRD'                                                547
         AG_EXNAME = 'ATTRIBUTE'                                          547
         AG_PARLIST = 'SEEN_COLO'                                         547
         CALL AXATTRIBUTE                                                 547
C    *                                                                    547
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  547
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             547
         CALL AGSRESET                                                    549
         AG_RMIN = 0.0                                                    549
         AG_RMAX = SSUB_MRODDIA/2                                         549
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            549
         AG_TITLE = 'TUBE'                                                549
         AG_EXNAME = 'SHAPE'                                              549
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      549
         CALL AXSHAPE                                                     549
C    *                                                                    549
5401  CALL AGSPOPB                                                        550
      GO TO BLOCK_SMRD, (5400 ,5410)                                      550
C End of Local Procedure                                                  550
C ----------------------------------------------------------------------  551
C ------------ SBMO                                                       551
C Begin Local Procedure SBMO                                              552
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       552
5420  CONTINUE                                                            552
         AG_TITLE='SBMO'                                                  552
         AG_MODULE=MODULE                                                 552
         AG_MARK='V00'                                                    552
         CALL AGSPUSH                                                     552
         AG_MODULE=MODULE                                                 552
C    *                                                                    552
C    Geant MATERIAL : G10 ===> inherited arguments used <===              553
         CALL AGSRESET                                                    553
         AG_TITLE = 'G10'                                                 553
         AG_EXNAME = 'MATERIAL'                                           553
         AG_PARLIST = 'NONE'                                              553
         CALL AXMATERIAL                                                  553
C    *                                                                    553
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       553
         CALL AGSRESET                                                    554
         AG_SEEN = 1                                                      554
         AG_COLO = 3                                                      554
         AG_TITLE = 'SBMO'                                                554
         AG_EXNAME = 'ATTRIBUTE'                                          554
         AG_PARLIST = 'SEEN_COLO'                                         554
         CALL AXATTRIBUTE                                                 554
C    *                                                                    554
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  554
C    UB_MBLKHGH/2 <===                                                    554
         CALL AGSRESET                                                    556
         AG_DX = SSUB_MBLKOWID/2                                          556
         AG_DY = SSUB_MBLKOLEN/2                                          556
         AG_DZ = SSUB_MBLKHGH/2                                           556
         AG_TITLE = 'BOX'                                                 556
         AG_EXNAME = 'SHAPE'                                              556
         AG_PARLIST = 'DX_DY_DZ'                                          556
         CALL AXSHAPE                                                     556
C    *                                                                    556
5421  CALL AGSPOPB                                                        557
      GO TO BLOCK_SBMO, (5420 ,5430)                                      557
C End of Local Procedure                                                  557
C ----------------------------------------------------------------------  558
C ------------ SBMI                                                       558
C Begin Local Procedure SBMI                                              559
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       559
5440  CONTINUE                                                            559
         AG_TITLE='SBMI'                                                  559
         AG_MODULE=MODULE                                                 559
         AG_MARK='V00'                                                    559
         CALL AGSPUSH                                                     559
         AG_MODULE=MODULE                                                 559
C    *                                                                    559
C    Geant MATERIAL : G10 ===> inherited arguments used <===              560
         CALL AGSRESET                                                    560
         AG_TITLE = 'G10'                                                 560
         AG_EXNAME = 'MATERIAL'                                           560
         AG_PARLIST = 'NONE'                                              560
         CALL AXMATERIAL                                                  560
C    *                                                                    560
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       560
         CALL AGSRESET                                                    561
         AG_SEEN = 1                                                      561
         AG_COLO = 3                                                      561
         AG_TITLE = 'SBMO'                                                561
         AG_EXNAME = 'ATTRIBUTE'                                          561
         AG_PARLIST = 'SEEN_COLO'                                         561
         CALL AXATTRIBUTE                                                 561
C    *                                                                    561
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  561
C    UB_MBLKHGH/2 <===                                                    561
         CALL AGSRESET                                                    563
         AG_DX = SSUB_MBLKIWID/2                                          563
         AG_DY = SSUB_MBLKILEN/2                                          563
         AG_DZ = SSUB_MBLKHGH/2                                           563
         AG_TITLE = 'BOX'                                                 563
         AG_EXNAME = 'SHAPE'                                              563
         AG_PARLIST = 'DX_DY_DZ'                                          563
         CALL AXSHAPE                                                     563
C    *                                                                    563
5441  CALL AGSPOPB                                                        564
      GO TO BLOCK_SBMI, (5440 ,5450)                                      564
C End of Local Procedure                                                  564
C ----------------------------------------------------------------------  565
C ------------ SCKM                                                       565
C Begin Local Procedure SCKM                                              566
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      566
5380  CONTINUE                                                            566
         AG_TITLE='SCKM'                                                  566
         AG_MODULE=MODULE                                                 566
         AG_MARK='V00'                                                    566
         CALL AGSPUSH                                                     566
         AG_MODULE=MODULE                                                 566
C    *                                                                    566
C    Geant MATERIAL : AIR ===> inherited arguments used <===              567
         CALL AGSRESET                                                    567
         AG_TITLE = 'AIR'                                                 567
         AG_EXNAME = 'MATERIAL'                                           567
         AG_PARLIST = 'NONE'                                              567
         CALL AXMATERIAL                                                  567
C    *                                                                    567
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       567
         CALL AGSRESET                                                    568
         AG_SEEN = 1                                                      568
         AG_COLO = 1                                                      568
         AG_TITLE = 'SCKM'                                                568
         AG_EXNAME = 'ATTRIBUTE'                                          568
         AG_PARLIST = 'SEEN_COLO'                                         568
         CALL AXATTRIBUTE                                                 568
C    *                                                                    568
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  568
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    568
         CALL AGSRESET                                                    571
         AG_RMIN = SSUB_KMCUTID/2                                         571
         AG_RMAX = SSUB_KMCUTOD/2                                         571
         AG_PHI1 = 270-SSUB_KMCUTOA                                       571
         AG_PHI2 = 270+SSUB_KMCUTOA                                       571
         AG_DZ = SSUB_KMNTTHK/2                                           571
         AG_TITLE = 'TUBS'                                                571
         AG_EXNAME = 'SHAPE'                                              571
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            571
         CALL AXSHAPE                                                     571
C    *                                                                    571
5381  CALL AGSPOPB                                                        572
      GO TO BLOCK_SCKM, (5380 ,5390)                                      572
C End of Local Procedure                                                  572
C ----------------------------------------------------------------------  573
C ------------ SBRL                                                       573
C Begin Local Procedure SBRL                                              574
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      574
5300  CONTINUE                                                            574
         AG_TITLE='SBRL'                                                  574
         AG_MODULE=MODULE                                                 574
         AG_MARK='V00'                                                    574
         CALL AGSPUSH                                                     574
         AG_MODULE=MODULE                                                 574
C    *                                                                    574
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  575
C    ADL=12.6 <===                                                        575
         CALL AGSRESET                                                    575
         AG_A = 20.719                                                    575
         AG_Z = 10.307                                                    575
         AG_DENS = 2.23                                                   575
         AG_ABSL = 50.7                                                   575
         AG_RADL = 12.6                                                   575
         AG_TITLE = 'PYREX'                                               575
         AG_EXNAME = 'MATERIAL'                                           575
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                575
         CALL AXMATERIAL                                                  575
C    *                                                                    575
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       575
         CALL AGSRESET                                                    576
         AG_SEEN = 1                                                      576
         AG_COLO = 6                                                      576
         AG_TITLE = 'SBRL'                                                576
         AG_EXNAME = 'ATTRIBUTE'                                          576
         AG_PARLIST = 'SEEN_COLO'                                         576
         CALL AXATTRIBUTE                                                 576
C    *                                                                    576
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  576
C    =                                                                    576
         CALL AGSRESET                                                    577
         AG_RMIN = SSUB_SROLLID/2                                         577
         AG_RMAX = SSUB_SROLLOD/2                                         577
         AG_TITLE = 'TUBE'                                                577
         AG_EXNAME = 'SHAPE'                                              577
         AG_PARLIST = 'RMIN_RMAX'                                         577
         CALL AXSHAPE                                                     577
C    *                                                                    577
5301  CALL AGSPOPB                                                        578
      GO TO BLOCK_SBRL, (5300 ,5310)                                      578
C End of Local Procedure                                                  578
C ----------------------------------------------------------------------  579
C ------------ SBRX                                                       579
C Begin Local Procedure SBRX                                              580
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 580
5320  CONTINUE                                                            580
         AG_TITLE='SBRX'                                                  580
         AG_MODULE=MODULE                                                 580
         AG_MARK='V00'                                                    580
         CALL AGSPUSH                                                     580
         AG_MODULE=MODULE                                                 580
C    *                                                                    580
C    Geant MATERIAL : IRON ===> inherited arguments used <===             581
         CALL AGSRESET                                                    581
         AG_TITLE = 'IRON'                                                581
         AG_EXNAME = 'MATERIAL'                                           581
         AG_PARLIST = 'NONE'                                              581
         CALL AXMATERIAL                                                  581
C    *                                                                    581
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       581
         CALL AGSRESET                                                    582
         AG_SEEN = 1                                                      582
         AG_COLO = 2                                                      582
         AG_TITLE = 'SBRX'                                                582
         AG_EXNAME = 'ATTRIBUTE'                                          582
         AG_PARLIST = 'SEEN_COLO'                                         582
         CALL AXATTRIBUTE                                                 582
C    *                                                                    582
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  582
C    LEN/2 <===                                                           582
         CALL AGSRESET                                                    584
         AG_RMIN = 0.0                                                    584
         AG_RMAX = SSUB_SROLLID/2                                         584
         AG_DZ = SSUB_SWIRELEN/2                                          584
         AG_TITLE = 'TUBE'                                                584
         AG_EXNAME = 'SHAPE'                                              584
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      584
         CALL AXSHAPE                                                     584
C    *                                                                    584
5321  CALL AGSPOPB                                                        585
      GO TO BLOCK_SBRX, (5320 ,5330)                                      585
C End of Local Procedure                                                  585
C ----------------------------------------------------------------------  586
C ------------ SBSR                                                       586
C Begin Local Procedure SBSR                                              587
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   587
5360  CONTINUE                                                            587
         AG_TITLE='SBSR'                                                  587
         AG_MODULE=MODULE                                                 587
         AG_MARK='V00'                                                    587
         CALL AGSPUSH                                                     587
         AG_MODULE=MODULE                                                 587
C    *                                                                    587
C    Geant MATERIAL : G10 ===> inherited arguments used <===              588
         CALL AGSRESET                                                    588
         AG_TITLE = 'G10'                                                 588
         AG_EXNAME = 'MATERIAL'                                           588
         AG_PARLIST = 'NONE'                                              588
         CALL AXMATERIAL                                                  588
C    *                                                                    588
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       588
         CALL AGSRESET                                                    589
         AG_SEEN = 1                                                      589
         AG_COLO = 3                                                      589
         AG_TITLE = 'SBSR'                                                589
         AG_EXNAME = 'ATTRIBUTE'                                          589
         AG_PARLIST = 'SEEN_COLO'                                         589
         CALL AXATTRIBUTE                                                 589
C    *                                                                    589
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  589
C    =SSUB_SRINGTHK/2 <===                                                589
         CALL AGSRESET                                                    591
         AG_RMIN = SSUB_SRINGID/2                                         591
         AG_RMAX = SSUB_SRINGOD/2                                         591
         AG_DZ = SSUB_SRINGTHK/2                                          591
         AG_TITLE = 'TUBE'                                                591
         AG_EXNAME = 'SHAPE'                                              591
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      591
         CALL AXSHAPE                                                     591
C    *                                                                    591
C    Create SBCR :                                                        592
C    Call To Local Procedure SBCR                                         592
         ASSIGN 5470 TO BLOCK_SBCR                                        592
         GO TO  5460                                                      592
5470     CONTINUE                                                         592
C    *                                                                    592
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             593
C    *                                                                    594
         I=-1                                                             594
            GOTO 5483                                                     594
5481        I=I+(2)                                                       594
5483        IF((2)*(I-(1)).GT.0) GO TO 5482                               594
C       *                                                                 595
            J=0                                                           595
               GO TO 5493                                                 595
5491           J=J+1                                                      595
5493           IF(J-(1).GT.0) GO TO 5492                                  595
               PHI=I*SSUB_SRCUTPHI+180*J                                  596
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  598
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      598
               CALL AGSRESET                                              598
               AG_X = XBUF*SIN(DEGRAD*PHI)                                598
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               598
               AG_ALPHAN=AG_ALPHAN+1                                      598
               AG_ALPHAK(AG_ALPHAN)=3                                     598
               AG_ALPHAL(AG_ALPHAN)=PHI                                   598
               AG_KONLY = 'MANY'                                          598
               AG_TITLE = 'SBCR'                                          598
               AG_EXNAME = 'POSITION'                                     598
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            598
               CALL AXPOSITION                                            598
C          *                                                              598
            GO TO 5491                                                    599
5492        CONTINUE                                                      599
         GO TO 5481                                                       600
5482     CONTINUE                                                         600
5361  CALL AGSPOPB                                                        601
      GO TO BLOCK_SBSR, (5360 ,5370)                                      601
C End of Local Procedure                                                  601
C ----------------------------------------------------------------------  602
C ------------ SBCR                                                       602
C Begin Local Procedure SBCR                                              603
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     603
5460  CONTINUE                                                            603
         AG_TITLE='SBCR'                                                  603
         AG_MODULE=MODULE                                                 603
         AG_MARK='V00'                                                    603
         CALL AGSPUSH                                                     603
         AG_MODULE=MODULE                                                 603
C    *                                                                    603
C    Geant MATERIAL : AIR ===> inherited arguments used <===              604
         CALL AGSRESET                                                    604
         AG_TITLE = 'AIR'                                                 604
         AG_EXNAME = 'MATERIAL'                                           604
         AG_PARLIST = 'NONE'                                              604
         CALL AXMATERIAL                                                  604
C    *                                                                    604
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       604
         CALL AGSRESET                                                    605
         AG_SEEN = 1                                                      605
         AG_COLO = 1                                                      605
         AG_TITLE = 'SBCR'                                                605
         AG_EXNAME = 'ATTRIBUTE'                                          605
         AG_PARLIST = 'SEEN_COLO'                                         605
         CALL AXATTRIBUTE                                                 605
C    *                                                                    605
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  605
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    605
         CALL AGSRESET                                                    607
         AG_DX = SSUB_SRCUTWID/2                                          607
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           607
         AG_DZ = SSUB_SRINGTHK/2                                          607
         AG_TITLE = 'BOX'                                                 607
         AG_EXNAME = 'SHAPE'                                              607
         AG_PARLIST = 'DX_DY_DZ'                                          607
         CALL AXSHAPE                                                     607
C    *                                                                    607
5461  CALL AGSPOPB                                                        608
      GO TO BLOCK_SBCR, (5460 ,5470)                                      608
C End of Local Procedure                                                  608
C ----------------------------------------------------------------------  609
C ------------ SCMY                                                       609
C Begin Local Procedure SCMY                                              610
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            610
5170  CONTINUE                                                            610
         AG_TITLE='SCMY'                                                  610
         AG_MODULE=MODULE                                                 610
         AG_MARK='V00'                                                    610
         CALL AGSPUSH                                                     610
         AG_MODULE=MODULE                                                 610
C    *                                                                    610
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             611
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          612
         CALL AGSRESET                                                    612
         AG_A = 12                                                        612
         AG_Z = 6                                                         612
         AG_W = 5                                                         612
         AG_TITLE = 'C5'                                                  612
         AG_EXNAME = 'COMPONENT'                                          612
         AG_PARLIST = 'A_Z_W'                                             612
         CALL AXCOMPONENT                                                 612
C    *                                                                    612
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           612
         CALL AGSRESET                                                    613
         AG_A = 1                                                         613
         AG_Z = 1                                                         613
         AG_W = 4                                                         613
         AG_TITLE = 'H4'                                                  613
         AG_EXNAME = 'COMPONENT'                                          613
         AG_PARLIST = 'A_Z_W'                                             613
         CALL AXCOMPONENT                                                 613
C    *                                                                    613
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          613
         CALL AGSRESET                                                    614
         AG_A = 16                                                        614
         AG_Z = 8                                                         614
         AG_W = 2                                                         614
         AG_TITLE = 'O2'                                                  614
         AG_EXNAME = 'COMPONENT'                                          614
         AG_PARLIST = 'A_Z_W'                                             614
         CALL AXCOMPONENT                                                 614
C    *                                                                    614
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            614
         CALL AGSRESET                                                    615
         AG_DENS = 1.39                                                   615
         AG_TITLE = 'MYLAR'                                               615
         AG_EXNAME = 'MIXTURE'                                            615
         AG_PARLIST = 'DENS'                                              615
         CALL AXMIXTURE                                                   615
C    *                                                                    615
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       615
         CALL AGSRESET                                                    616
         AG_SEEN = 1                                                      616
         AG_COLO = 3                                                      616
         AG_TITLE = 'SCMY'                                                616
         AG_EXNAME = 'ATTRIBUTE'                                          616
         AG_PARLIST = 'SEEN_COLO'                                         616
         CALL AXATTRIBUTE                                                 616
C    *                                                                    616
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  616
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  616
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  616
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  616
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     616
         CALL AGSRESET                                                    622
         AG_PHI1 = 0                                                      622
         AG_DPHI = 360                                                    622
         AG_NZ = 3                                                        622
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              622
         AG_ZI(2)=SSUP_CONE3ZMX                                           622
         AG_ZI(3)=SSUP_CONE4ZMX                                           622
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  622
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  622
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  622
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    622
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    622
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    622
         AG_TITLE = 'PCON'                                                622
         AG_EXNAME = 'SHAPE'                                              622
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           622
         CALL AXSHAPE                                                     622
C    *                                                                    622
5171  CALL AGSPOPB                                                        623
      GO TO BLOCK_SCMY, (5170 ,5180)                                      623
C End of Local Procedure                                                  623
      WRITE(LOUT,5500)'===>>  [End AgML Module SVTTGEO9]  <<==='          623
5500  FORMAT(A32,/,/)                                                     623
C MODULE SVTTGEO9                                                         625
      END                                                                 625
