C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SVTTGEO5                                                 1
C SVTTGEO5 IS THE SVT GEOMETRY FOR STAR: WITHOUT THE CENTRAL PART         1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SVTTGEO5'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE SVT GEOMETRY FOR STAR: WITHOUT THE CENTRAL P  1
     *ART'/                                                               1
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
C CREATED 04 OCT 2005                                                     2
      CHARACTER*40 CREATED /'04 OCT 2005'/                                3
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
      INTEGER BLOCK_SCON                                                  8
      INTEGER BLOCK_SGRA                                                  8
      INTEGER BLOCK_STAP                                                  8
      INTEGER BLOCK_STAC                                                  8
      INTEGER BLOCK_SHLA                                                  8
      INTEGER BLOCK_SHLB                                                  8
      INTEGER BLOCK_SBRG                                                  8
      INTEGER BLOCK_SOES                                                  8
      INTEGER BLOCK_SOSM                                                  8
      INTEGER BLOCK_SCRW                                                  8
      INTEGER BLOCK_SIES                                                  8
      INTEGER BLOCK_SISM                                                  8
      INTEGER BLOCK_SBSP                                                  8
      INTEGER BLOCK_SAKM                                                  8
      INTEGER BLOCK_SBMM                                                  8
      INTEGER BLOCK_SMRD                                                  8
      INTEGER BLOCK_SBMO                                                  8
      INTEGER BLOCK_SBMI                                                  8
      INTEGER BLOCK_SCKM                                                  8
      INTEGER BLOCK_SBRL                                                  8
      INTEGER BLOCK_SBRX                                                  8
      INTEGER BLOCK_SBSR                                                  8
      INTEGER BLOCK_SBCR                                                  8
      INTEGER BLOCK_SCMY                                                  8
C -                                                                       10
      REAL SVTG_VERSION                                                   10
      EQUIVALENCE (BANK_SVTG(3),SVTG_VERSION,RE_SVTG_VERSION),            10
     *(NAMES_SVTG(1),NSVTG_VERSION)                                       10
      EQUIVALENCE (COMMEN_SVTG(1),CSVTG_VERSION),(MAP_SVTG(1,1),          10
     *MSVTG_VERSION)                                                      10
      CHARACTER CSVTG_VERSION*40/' '/                                     10
      CHARACTER NSVTG_VERSION*12/'R.VERSION'/                             10
      INTEGER MSVTG_VERSION(2)/1,1/                                       10
      REAL RE_SVTG_VERSION                                                10
C -                                                                       10
      INTEGER SVTG_NLAYER                                                 10
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              10
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        10
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           10
     *MSVTG_NLAYER)                                                       10
      CHARACTER CSVTG_NLAYER*40/' '/                                      10
      CHARACTER NSVTG_NLAYER*12/'I.NLAYER'/                               10
      INTEGER MSVTG_NLAYER(2)/1,1/                                        10
      REAL RE_SVTG_NLAYER                                                 10
C -                                                                       10
      REAL SVTG_RSIZEMIN                                                  10
      EQUIVALENCE (BANK_SVTG(5),SVTG_RSIZEMIN,RE_SVTG_RSIZEMIN),          10
     *(NAMES_SVTG(3),NSVTG_RSIZEMIN)                                      10
      EQUIVALENCE (COMMEN_SVTG(3),CSVTG_RSIZEMIN),(MAP_SVTG(1,3),         10
     *MSVTG_RSIZEMIN)                                                     10
      CHARACTER CSVTG_RSIZEMIN*40/' '/                                    10
      CHARACTER NSVTG_RSIZEMIN*12/'R.RSIZEMIN'/                           10
      INTEGER MSVTG_RSIZEMIN(2)/1,1/                                      10
      REAL RE_SVTG_RSIZEMIN                                               10
C -                                                                       10
      REAL SVTG_RSIZEMAX                                                  10
      EQUIVALENCE (BANK_SVTG(6),SVTG_RSIZEMAX,RE_SVTG_RSIZEMAX),          10
     *(NAMES_SVTG(4),NSVTG_RSIZEMAX)                                      10
      EQUIVALENCE (COMMEN_SVTG(4),CSVTG_RSIZEMAX),(MAP_SVTG(1,4),         10
     *MSVTG_RSIZEMAX)                                                     10
      CHARACTER CSVTG_RSIZEMAX*40/' '/                                    10
      CHARACTER NSVTG_RSIZEMAX*12/'R.RSIZEMAX'/                           10
      INTEGER MSVTG_RSIZEMAX(2)/1,1/                                      10
      REAL RE_SVTG_RSIZEMAX                                               10
C -                                                                       10
      REAL SVTG_ZSIZEMAX                                                  10
      EQUIVALENCE (BANK_SVTG(7),SVTG_ZSIZEMAX,RE_SVTG_ZSIZEMAX),          10
     *(NAMES_SVTG(5),NSVTG_ZSIZEMAX)                                      10
      EQUIVALENCE (COMMEN_SVTG(5),CSVTG_ZSIZEMAX),(MAP_SVTG(1,5),         10
     *MSVTG_ZSIZEMAX)                                                     10
      CHARACTER CSVTG_ZSIZEMAX*40/' '/                                    10
      CHARACTER NSVTG_ZSIZEMAX*12/'R.ZSIZEMAX'/                           10
      INTEGER MSVTG_ZSIZEMAX(2)/1,1/                                      10
      REAL RE_SVTG_ZSIZEMAX                                               10
C -                                                                       10
      REAL SVTG_ANGOFF                                                    10
      EQUIVALENCE (BANK_SVTG(8),SVTG_ANGOFF,RE_SVTG_ANGOFF),              10
     *(NAMES_SVTG(6),NSVTG_ANGOFF)                                        10
      EQUIVALENCE (COMMEN_SVTG(6),CSVTG_ANGOFF),(MAP_SVTG(1,6),           10
     *MSVTG_ANGOFF)                                                       10
      CHARACTER CSVTG_ANGOFF*40/' '/                                      10
      CHARACTER NSVTG_ANGOFF*12/'R.ANGOFF'/                               10
      INTEGER MSVTG_ANGOFF(2)/1,1/                                        10
      REAL RE_SVTG_ANGOFF                                                 10
C -                                                                       10
      REAL SVTG_SUPPORTVER                                                10
      EQUIVALENCE (BANK_SVTG(9),SVTG_SUPPORTVER,RE_SVTG_SUPPORTVER),      10
     *(NAMES_SVTG(7),NSVTG_SUPPORTVER)                                    10
      EQUIVALENCE (COMMEN_SVTG(7),CSVTG_SUPPORTVER),(MAP_SVTG(1,7),       10
     *MSVTG_SUPPORTVER)                                                   10
      CHARACTER CSVTG_SUPPORTVER*40/' '/                                  10
      CHARACTER NSVTG_SUPPORTVER*12/'R.SUPPORTVER'/                       10
      INTEGER MSVTG_SUPPORTVER(2)/1,1/                                    10
      REAL RE_SVTG_SUPPORTVER                                             10
C -                                                                       10
      REAL SVTG_CONEVER                                                   10
      EQUIVALENCE (BANK_SVTG(10),SVTG_CONEVER,RE_SVTG_CONEVER),           10
     *(NAMES_SVTG(8),NSVTG_CONEVER)                                       10
      EQUIVALENCE (COMMEN_SVTG(8),CSVTG_CONEVER),(MAP_SVTG(1,8),          10
     *MSVTG_CONEVER)                                                      10
      CHARACTER CSVTG_CONEVER*40/' '/                                     10
      CHARACTER NSVTG_CONEVER*12/'R.CONEVER'/                             10
      INTEGER MSVTG_CONEVER(2)/1,1/                                       10
      REAL RE_SVTG_CONEVER                                                10
C -                                                                       10
      REAL SVTG_IFMANY                                                    10
      EQUIVALENCE (BANK_SVTG(11),SVTG_IFMANY,RE_SVTG_IFMANY),             10
     *(NAMES_SVTG(9),NSVTG_IFMANY)                                        10
      EQUIVALENCE (COMMEN_SVTG(9),CSVTG_IFMANY),(MAP_SVTG(1,9),           10
     *MSVTG_IFMANY)                                                       10
      CHARACTER CSVTG_IFMANY*40/' '/                                      10
      CHARACTER NSVTG_IFMANY*12/'R.IFMANY'/                               10
      INTEGER MSVTG_IFMANY(2)/1,1/                                        10
      REAL RE_SVTG_IFMANY                                                 10
C -                                                                       10
      REAL SVTG_NMIN                                                      10
      EQUIVALENCE (BANK_SVTG(12),SVTG_NMIN,RE_SVTG_NMIN),                 10
     *(NAMES_SVTG(10),NSVTG_NMIN)                                         10
      EQUIVALENCE (COMMEN_SVTG(10),CSVTG_NMIN),(MAP_SVTG(1,10),           10
     *MSVTG_NMIN)                                                         10
      CHARACTER CSVTG_NMIN*40/' '/                                        10
      CHARACTER NSVTG_NMIN*12/'R.NMIN'/                                   10
      INTEGER MSVTG_NMIN(2)/1,1/                                          10
      REAL RE_SVTG_NMIN                                                   10
C -                                                                       10
      INTEGER LEN_SVTG,LLEN_SVTG,LINK_SVTG                                10
      PARAMETER (LEN_SVTG=12)                                             10
      PARAMETER (LLEN_SVTG=10)                                            10
      INTEGER FLAG_SVTG/0/,MAP_SVTG(2,LLEN_SVTG),IDNUM_SVTG(10)           10
      CHARACTER NAMES_SVTG(LLEN_SVTG)*12,COMMEN_SVTG(LLEN_SVTG)*40        10
      DATA IDNUM_SVTG/10*0/                                               10
      REAL BANK_SVTG(LEN_SVTG)                                            10
      CHARACTER*60 BPATH_SVTG/' '/                                        10
      SAVE FLAG_SVTG,MAP_SVTG,IDNUM_SVTG,NAMES_SVTG,COMMEN_SVTG,          10
     *BPATH_SVTG                                                          10
      DATA LINK_SVTG/0/,BANK_SVTG/LEN_SVTG*0/                             10
C *                                                                       10
C -                                                                       12
      REAL SWCA_VERSION                                                   12
      EQUIVALENCE (BANK_SWCA(3),SWCA_VERSION,RE_SWCA_VERSION),            12
     *(NAMES_SWCA(1),NSWCA_VERSION)                                       12
      EQUIVALENCE (COMMEN_SWCA(1),CSWCA_VERSION),(MAP_SWCA(1,1),          12
     *MSWCA_VERSION)                                                      12
      CHARACTER CSWCA_VERSION*40/' '/                                     12
      CHARACTER NSWCA_VERSION*12/'R.VERSION'/                             12
      INTEGER MSWCA_VERSION(2)/1,1/                                       12
      REAL RE_SWCA_VERSION                                                12
C -                                                                       12
      REAL SWCA_LENGTH                                                    12
      EQUIVALENCE (BANK_SWCA(4),SWCA_LENGTH,RE_SWCA_LENGTH),              12
     *(NAMES_SWCA(2),NSWCA_LENGTH)                                        12
      EQUIVALENCE (COMMEN_SWCA(2),CSWCA_LENGTH),(MAP_SWCA(1,2),           12
     *MSWCA_LENGTH)                                                       12
      CHARACTER CSWCA_LENGTH*40/' '/                                      12
      CHARACTER NSWCA_LENGTH*12/'R.LENGTH'/                               12
      INTEGER MSWCA_LENGTH(2)/1,1/                                        12
      REAL RE_SWCA_LENGTH                                                 12
C -                                                                       12
      REAL SWCA_WAFERWID                                                  12
      EQUIVALENCE (BANK_SWCA(5),SWCA_WAFERWID,RE_SWCA_WAFERWID),          12
     *(NAMES_SWCA(3),NSWCA_WAFERWID)                                      12
      EQUIVALENCE (COMMEN_SWCA(3),CSWCA_WAFERWID),(MAP_SWCA(1,3),         12
     *MSWCA_WAFERWID)                                                     12
      CHARACTER CSWCA_WAFERWID*40/' '/                                    12
      CHARACTER NSWCA_WAFERWID*12/'R.WAFERWID'/                           12
      INTEGER MSWCA_WAFERWID(2)/1,1/                                      12
      REAL RE_SWCA_WAFERWID                                               12
C -                                                                       12
      REAL SWCA_WAFERLEN                                                  12
      EQUIVALENCE (BANK_SWCA(6),SWCA_WAFERLEN,RE_SWCA_WAFERLEN),          12
     *(NAMES_SWCA(4),NSWCA_WAFERLEN)                                      12
      EQUIVALENCE (COMMEN_SWCA(4),CSWCA_WAFERLEN),(MAP_SWCA(1,4),         12
     *MSWCA_WAFERLEN)                                                     12
      CHARACTER CSWCA_WAFERLEN*40/' '/                                    12
      CHARACTER NSWCA_WAFERLEN*12/'R.WAFERLEN'/                           12
      INTEGER MSWCA_WAFERLEN(2)/1,1/                                      12
      REAL RE_SWCA_WAFERLEN                                               12
C -                                                                       12
      REAL SWCA_WAFERTHK                                                  12
      EQUIVALENCE (BANK_SWCA(7),SWCA_WAFERTHK,RE_SWCA_WAFERTHK),          12
     *(NAMES_SWCA(5),NSWCA_WAFERTHK)                                      12
      EQUIVALENCE (COMMEN_SWCA(5),CSWCA_WAFERTHK),(MAP_SWCA(1,5),         12
     *MSWCA_WAFERTHK)                                                     12
      CHARACTER CSWCA_WAFERTHK*40/' '/                                    12
      CHARACTER NSWCA_WAFERTHK*12/'R.WAFERTHK'/                           12
      INTEGER MSWCA_WAFERTHK(2)/1,1/                                      12
      REAL RE_SWCA_WAFERTHK                                               12
C -                                                                       12
      REAL SWCA_ROHATHK                                                   12
      EQUIVALENCE (BANK_SWCA(8),SWCA_ROHATHK,RE_SWCA_ROHATHK),            12
     *(NAMES_SWCA(6),NSWCA_ROHATHK)                                       12
      EQUIVALENCE (COMMEN_SWCA(6),CSWCA_ROHATHK),(MAP_SWCA(1,6),          12
     *MSWCA_ROHATHK)                                                      12
      CHARACTER CSWCA_ROHATHK*40/' '/                                     12
      CHARACTER NSWCA_ROHATHK*12/'R.ROHATHK'/                             12
      INTEGER MSWCA_ROHATHK(2)/1,1/                                       12
      REAL RE_SWCA_ROHATHK                                                12
C -                                                                       12
      REAL SWCA_WAFCARWD                                                  12
      EQUIVALENCE (BANK_SWCA(9),SWCA_WAFCARWD,RE_SWCA_WAFCARWD),          12
     *(NAMES_SWCA(7),NSWCA_WAFCARWD)                                      12
      EQUIVALENCE (COMMEN_SWCA(7),CSWCA_WAFCARWD),(MAP_SWCA(1,7),         12
     *MSWCA_WAFCARWD)                                                     12
      CHARACTER CSWCA_WAFCARWD*40/' '/                                    12
      CHARACTER NSWCA_WAFCARWD*12/'R.WAFCARWD'/                           12
      INTEGER MSWCA_WAFCARWD(2)/1,1/                                      12
      REAL RE_SWCA_WAFCARWD                                               12
C -                                                                       12
      REAL SWCA_WAFCARTH                                                  12
      EQUIVALENCE (BANK_SWCA(10),SWCA_WAFCARTH,RE_SWCA_WAFCARTH),         12
     *(NAMES_SWCA(8),NSWCA_WAFCARTH)                                      12
      EQUIVALENCE (COMMEN_SWCA(8),CSWCA_WAFCARTH),(MAP_SWCA(1,8),         12
     *MSWCA_WAFCARTH)                                                     12
      CHARACTER CSWCA_WAFCARTH*40/' '/                                    12
      CHARACTER NSWCA_WAFCARTH*12/'R.WAFCARTH'/                           12
      INTEGER MSWCA_WAFCARTH(2)/1,1/                                      12
      REAL RE_SWCA_WAFCARTH                                               12
C -                                                                       12
      REAL SWCA_WAFERGAP                                                  12
      EQUIVALENCE (BANK_SWCA(11),SWCA_WAFERGAP,RE_SWCA_WAFERGAP),         12
     *(NAMES_SWCA(9),NSWCA_WAFERGAP)                                      12
      EQUIVALENCE (COMMEN_SWCA(9),CSWCA_WAFERGAP),(MAP_SWCA(1,9),         12
     *MSWCA_WAFERGAP)                                                     12
      CHARACTER CSWCA_WAFERGAP*40/' '/                                    12
      CHARACTER NSWCA_WAFERGAP*12/'R.WAFERGAP'/                           12
      INTEGER MSWCA_WAFERGAP(2)/1,1/                                      12
      REAL RE_SWCA_WAFERGAP                                               12
C -                                                                       12
      REAL SWCA_DRIFT                                                     12
      EQUIVALENCE (BANK_SWCA(12),SWCA_DRIFT,RE_SWCA_DRIFT),               12
     *(NAMES_SWCA(10),NSWCA_DRIFT)                                        12
      EQUIVALENCE (COMMEN_SWCA(10),CSWCA_DRIFT),(MAP_SWCA(1,10),          12
     *MSWCA_DRIFT)                                                        12
      CHARACTER CSWCA_DRIFT*40/' '/                                       12
      CHARACTER NSWCA_DRIFT*12/'R.DRIFT'/                                 12
      INTEGER MSWCA_DRIFT(2)/1,1/                                         12
      REAL RE_SWCA_DRIFT                                                  12
C -                                                                       12
      REAL SWCA_STRUTLEN                                                  12
      EQUIVALENCE (BANK_SWCA(13),SWCA_STRUTLEN,RE_SWCA_STRUTLEN),         12
     *(NAMES_SWCA(11),NSWCA_STRUTLEN)                                     12
      EQUIVALENCE (COMMEN_SWCA(11),CSWCA_STRUTLEN),(MAP_SWCA(1,11),       12
     *MSWCA_STRUTLEN)                                                     12
      CHARACTER CSWCA_STRUTLEN*40/' '/                                    12
      CHARACTER NSWCA_STRUTLEN*12/'R.STRUTLEN'/                           12
      INTEGER MSWCA_STRUTLEN(2)/1,1/                                      12
      REAL RE_SWCA_STRUTLEN                                               12
C -                                                                       12
      INTEGER LEN_SWCA,LLEN_SWCA,LINK_SWCA                                12
      PARAMETER (LEN_SWCA=13)                                             12
      PARAMETER (LLEN_SWCA=11)                                            12
      INTEGER FLAG_SWCA/0/,MAP_SWCA(2,LLEN_SWCA),IDNUM_SWCA(10)           12
      CHARACTER NAMES_SWCA(LLEN_SWCA)*12,COMMEN_SWCA(LLEN_SWCA)*40        12
      DATA IDNUM_SWCA/10*0/                                               12
      REAL BANK_SWCA(LEN_SWCA)                                            12
      CHARACTER*60 BPATH_SWCA/' '/                                        12
      SAVE FLAG_SWCA,MAP_SWCA,IDNUM_SWCA,NAMES_SWCA,COMMEN_SWCA,          12
     *BPATH_SWCA                                                          12
      DATA LINK_SWCA/0/,BANK_SWCA/LEN_SWCA*0/                             12
C *                                                                       12
C -                                                                       15
      REAL SSUP_VERSION                                                   15
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            15
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       15
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          15
     *MSSUP_VERSION)                                                      15
      CHARACTER CSSUP_VERSION*40/' '/                                     15
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             15
      INTEGER MSSUP_VERSION(2)/1,1/                                       15
      REAL RE_SSUP_VERSION                                                15
C -                                                                       15
      REAL SSUP_CABTHK                                                    15
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              15
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        15
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           15
     *MSSUP_CABTHK)                                                       15
      CHARACTER CSSUP_CABTHK*40/' '/                                      15
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               15
      INTEGER MSSUP_CABTHK(2)/1,1/                                        15
      REAL RE_SSUP_CABTHK                                                 15
C -                                                                       15
      REAL SSUP_HOSRMN                                                    15
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              15
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        15
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           15
     *MSSUP_HOSRMN)                                                       15
      CHARACTER CSSUP_HOSRMN*40/' '/                                      15
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               15
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        15
      REAL RE_SSUP_HOSRMN                                                 15
C -                                                                       15
      REAL SSUP_HOSRMX                                                    15
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              15
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        15
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           15
     *MSSUP_HOSRMX)                                                       15
      CHARACTER CSSUP_HOSRMX*40/' '/                                      15
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               15
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        15
      REAL RE_SSUP_HOSRMX                                                 15
C -                                                                       15
      REAL SSUP_NHOSES                                                    15
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              15
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        15
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           15
     *MSSUP_NHOSES)                                                       15
      CHARACTER CSSUP_NHOSES*40/' '/                                      15
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               15
      INTEGER MSSUP_NHOSES(2)/1,1/                                        15
      REAL RE_SSUP_NHOSES                                                 15
C -                                                                       15
      REAL SSUP_WRPMYTHK                                                  15
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          15
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      15
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         15
     *MSSUP_WRPMYTHK)                                                     15
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    15
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           15
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      15
      REAL RE_SSUP_WRPMYTHK                                               15
C -                                                                       15
      REAL SSUP_WRPALTHK                                                  15
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          15
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      15
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         15
     *MSSUP_WRPALTHK)                                                     15
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    15
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           15
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      15
      REAL RE_SSUP_WRPALTHK                                               15
C -                                                                       15
      REAL SSUP_GRPHTHK                                                   15
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           15
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       15
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          15
     *MSSUP_GRPHTHK)                                                      15
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     15
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             15
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       15
      REAL RE_SSUP_GRPHTHK                                                15
C -                                                                       15
      REAL SSUP_CONE1ZMN                                                  15
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         15
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      15
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         15
     *MSSUP_CONE1ZMN)                                                     15
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    15
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           15
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      15
      REAL RE_SSUP_CONE1ZMN                                               15
C -                                                                       15
      REAL SSUP_RODLEN                                                    15
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             15
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       15
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         15
     *MSSUP_RODLEN)                                                       15
      CHARACTER CSSUP_RODLEN*40/' '/                                      15
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               15
      INTEGER MSSUP_RODLEN(2)/1,1/                                        15
      REAL RE_SSUP_RODLEN                                                 15
C -                                                                       15
      REAL SSUP_RODDIST                                                   15
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           15
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      15
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        15
     *MSSUP_RODDIST)                                                      15
      CHARACTER CSSUP_RODDIST*40/' '/                                     15
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             15
      INTEGER MSSUP_RODDIST(2)/1,1/                                       15
      REAL RE_SSUP_RODDIST                                                15
C -                                                                       15
      REAL SSUP_RODID                                                     15
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               15
     *(NAMES_SSUP(12),NSSUP_RODID)                                        15
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          15
     *MSSUP_RODID)                                                        15
      CHARACTER CSSUP_RODID*40/' '/                                       15
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 15
      INTEGER MSSUP_RODID(2)/1,1/                                         15
      REAL RE_SSUP_RODID                                                  15
C -                                                                       15
      REAL SSUP_RODOD                                                     15
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               15
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        15
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          15
     *MSSUP_RODOD)                                                        15
      CHARACTER CSSUP_RODOD*40/' '/                                       15
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 15
      INTEGER MSSUP_RODOD(2)/1,1/                                         15
      REAL RE_SSUP_RODOD                                                  15
C -                                                                       15
      REAL SSUP_CON1IDMN                                                  15
      EQUIVALENCE (BANK_SSUP(16),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         15
     *(NAMES_SSUP(14),NSSUP_CON1IDMN)                                     15
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CON1IDMN),(MAP_SSUP(1,14),       15
     *MSSUP_CON1IDMN)                                                     15
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    15
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           15
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      15
      REAL RE_SSUP_CON1IDMN                                               15
C -                                                                       15
      REAL SSUP_CON3IDMN                                                  15
      EQUIVALENCE (BANK_SSUP(17),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         15
     *(NAMES_SSUP(15),NSSUP_CON3IDMN)                                     15
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_CON3IDMN),(MAP_SSUP(1,15),       15
     *MSSUP_CON3IDMN)                                                     15
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    15
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           15
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      15
      REAL RE_SSUP_CON3IDMN                                               15
C -                                                                       15
      REAL SSUP_CON4IDMN                                                  15
      EQUIVALENCE (BANK_SSUP(18),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         15
     *(NAMES_SSUP(16),NSSUP_CON4IDMN)                                     15
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CON4IDMN),(MAP_SSUP(1,16),       15
     *MSSUP_CON4IDMN)                                                     15
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    15
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           15
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      15
      REAL RE_SSUP_CON4IDMN                                               15
C -                                                                       15
      REAL SSUP_CON4IDMX                                                  15
      EQUIVALENCE (BANK_SSUP(19),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         15
     *(NAMES_SSUP(17),NSSUP_CON4IDMX)                                     15
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CON4IDMX),(MAP_SSUP(1,17),       15
     *MSSUP_CON4IDMX)                                                     15
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    15
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           15
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      15
      REAL RE_SSUP_CON4IDMX                                               15
C -                                                                       15
      REAL SSUP_CONE3ZMX                                                  15
      EQUIVALENCE (BANK_SSUP(20),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         15
     *(NAMES_SSUP(18),NSSUP_CONE3ZMX)                                     15
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_CONE3ZMX),(MAP_SSUP(1,18),       15
     *MSSUP_CONE3ZMX)                                                     15
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    15
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           15
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      15
      REAL RE_SSUP_CONE3ZMX                                               15
C -                                                                       15
      REAL SSUP_CONE4ZMX                                                  15
      EQUIVALENCE (BANK_SSUP(21),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         15
     *(NAMES_SSUP(19),NSSUP_CONE4ZMX)                                     15
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CONE4ZMX),(MAP_SSUP(1,19),       15
     *MSSUP_CONE4ZMX)                                                     15
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    15
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           15
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      15
      REAL RE_SSUP_CONE4ZMX                                               15
C -                                                                       15
      REAL SSUP_BRATHK                                                    15
      EQUIVALENCE (BANK_SSUP(22),SSUP_BRATHK,RE_SSUP_BRATHK),             15
     *(NAMES_SSUP(20),NSSUP_BRATHK)                                       15
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_BRATHK),(MAP_SSUP(1,20),         15
     *MSSUP_BRATHK)                                                       15
      CHARACTER CSSUP_BRATHK*40/' '/                                      15
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               15
      INTEGER MSSUP_BRATHK(2)/1,1/                                        15
      REAL RE_SSUP_BRATHK                                                 15
C -                                                                       15
      REAL SSUP_ERJTHK                                                    15
      EQUIVALENCE (BANK_SSUP(23),SSUP_ERJTHK,RE_SSUP_ERJTHK),             15
     *(NAMES_SSUP(21),NSSUP_ERJTHK)                                       15
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_ERJTHK),(MAP_SSUP(1,21),         15
     *MSSUP_ERJTHK)                                                       15
      CHARACTER CSSUP_ERJTHK*40/' '/                                      15
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               15
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        15
      REAL RE_SSUP_ERJTHK                                                 15
C -                                                                       15
      REAL SSUP_ERJWID                                                    15
      EQUIVALENCE (BANK_SSUP(24),SSUP_ERJWID,RE_SSUP_ERJWID),             15
     *(NAMES_SSUP(22),NSSUP_ERJWID)                                       15
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_ERJWID),(MAP_SSUP(1,22),         15
     *MSSUP_ERJWID)                                                       15
      CHARACTER CSSUP_ERJWID*40/' '/                                      15
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               15
      INTEGER MSSUP_ERJWID(2)/1,1/                                        15
      REAL RE_SSUP_ERJWID                                                 15
C -                                                                       15
      REAL SSUP_ERJLEN                                                    15
      EQUIVALENCE (BANK_SSUP(25),SSUP_ERJLEN,RE_SSUP_ERJLEN),             15
     *(NAMES_SSUP(23),NSSUP_ERJLEN)                                       15
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_ERJLEN),(MAP_SSUP(1,23),         15
     *MSSUP_ERJLEN)                                                       15
      CHARACTER CSSUP_ERJLEN*40/' '/                                      15
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               15
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        15
      REAL RE_SSUP_ERJLEN                                                 15
C -                                                                       15
      REAL SSUP_ERJZDIS                                                   15
      EQUIVALENCE (BANK_SSUP(26),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           15
     *(NAMES_SSUP(24),NSSUP_ERJZDIS)                                      15
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_ERJZDIS),(MAP_SSUP(1,24),        15
     *MSSUP_ERJZDIS)                                                      15
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     15
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             15
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       15
      REAL RE_SSUP_ERJZDIS                                                15
C -                                                                       15
      REAL SSUP_ERJ1X                                                     15
      EQUIVALENCE (BANK_SSUP(27),SSUP_ERJ1X,RE_SSUP_ERJ1X),               15
     *(NAMES_SSUP(25),NSSUP_ERJ1X)                                        15
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_ERJ1X),(MAP_SSUP(1,25),          15
     *MSSUP_ERJ1X)                                                        15
      CHARACTER CSSUP_ERJ1X*40/' '/                                       15
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 15
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         15
      REAL RE_SSUP_ERJ1X                                                  15
C -                                                                       15
      REAL SSUP_ERJ2X                                                     15
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJ2X,RE_SSUP_ERJ2X),               15
     *(NAMES_SSUP(26),NSSUP_ERJ2X)                                        15
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJ2X),(MAP_SSUP(1,26),          15
     *MSSUP_ERJ2X)                                                        15
      CHARACTER CSSUP_ERJ2X*40/' '/                                       15
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 15
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         15
      REAL RE_SSUP_ERJ2X                                                  15
C -                                                                       15
      REAL SSUP_ERJ2Y                                                     15
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               15
     *(NAMES_SSUP(27),NSSUP_ERJ2Y)                                        15
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJ2Y),(MAP_SSUP(1,27),          15
     *MSSUP_ERJ2Y)                                                        15
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       15
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 15
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         15
      REAL RE_SSUP_ERJ2Y                                                  15
C -                                                                       15
      REAL SSUP_ERJRAD                                                    15
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJRAD,RE_SSUP_ERJRAD),             15
     *(NAMES_SSUP(28),NSSUP_ERJRAD)                                       15
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJRAD),(MAP_SSUP(1,28),         15
     *MSSUP_ERJRAD)                                                       15
      CHARACTER CSSUP_ERJRAD*40/' '/                                      15
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               15
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        15
      REAL RE_SSUP_ERJRAD                                                 15
C -                                                                       15
      REAL SSUP_ERJDIA                                                    15
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJDIA,RE_SSUP_ERJDIA),             15
     *(NAMES_SSUP(29),NSSUP_ERJDIA)                                       15
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJDIA),(MAP_SSUP(1,29),         15
     *MSSUP_ERJDIA)                                                       15
      CHARACTER CSSUP_ERJDIA*40/' '/                                      15
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               15
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        15
      REAL RE_SSUP_ERJDIA                                                 15
C -                                                                       15
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                15
      PARAMETER (LEN_SSUP=31)                                             15
      PARAMETER (LLEN_SSUP=29)                                            15
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           15
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        15
      DATA IDNUM_SSUP/10*0/                                               15
      REAL BANK_SSUP(LEN_SSUP)                                            15
      CHARACTER*60 BPATH_SSUP/' '/                                        15
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          15
     *BPATH_SSUP                                                          15
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             15
C *                                                                       15
C -                                                                       19
      REAL SSUB_VERSION                                                   19
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            19
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       19
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          19
     *MSSUB_VERSION)                                                      19
      CHARACTER CSSUB_VERSION*40/' '/                                     19
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             19
      INTEGER MSSUB_VERSION(2)/1,1/                                       19
      REAL RE_SSUB_VERSION                                                19
C -                                                                       19
      REAL SSUB_KMOUNTID                                                  19
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          19
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      19
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         19
     *MSSUB_KMOUNTID)                                                     19
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    19
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           19
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      19
      REAL RE_SSUB_KMOUNTID                                               19
C -                                                                       19
      REAL SSUB_KMOUNTOD                                                  19
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          19
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      19
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         19
     *MSSUB_KMOUNTOD)                                                     19
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    19
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           19
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      19
      REAL RE_SSUB_KMOUNTOD                                               19
C -                                                                       19
      REAL SSUB_KMNTTHK                                                   19
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            19
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       19
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          19
     *MSSUB_KMNTTHK)                                                      19
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     19
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             19
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       19
      REAL RE_SSUB_KMNTTHK                                                19
C -                                                                       19
      REAL SSUB_KMCUTOD                                                   19
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            19
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       19
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          19
     *MSSUB_KMCUTOD)                                                      19
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     19
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             19
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       19
      REAL RE_SSUB_KMCUTOD                                                19
C -                                                                       19
      REAL SSUB_KMCUTID                                                   19
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            19
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       19
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          19
     *MSSUB_KMCUTID)                                                      19
      CHARACTER CSSUB_KMCUTID*40/' '/                                     19
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             19
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       19
      REAL RE_SSUB_KMCUTID                                                19
C -                                                                       19
      REAL SSUB_KMCUTOA                                                   19
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            19
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       19
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          19
     *MSSUB_KMCUTOA)                                                      19
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     19
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             19
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       19
      REAL RE_SSUB_KMCUTOA                                                19
C -                                                                       19
      REAL SSUB_KMCUTOFF                                                  19
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         19
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      19
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         19
     *MSSUB_KMCUTOFF)                                                     19
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    19
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           19
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      19
      REAL RE_SSUB_KMCUTOFF                                               19
C -                                                                       19
      REAL SSUB_SRINGID                                                   19
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           19
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       19
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          19
     *MSSUB_SRINGID)                                                      19
      CHARACTER CSSUB_SRINGID*40/' '/                                     19
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             19
      INTEGER MSSUB_SRINGID(2)/1,1/                                       19
      REAL RE_SSUB_SRINGID                                                19
C -                                                                       19
      REAL SSUB_SRINGOD                                                   19
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           19
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      19
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        19
     *MSSUB_SRINGOD)                                                      19
      CHARACTER CSSUB_SRINGOD*40/' '/                                     19
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             19
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       19
      REAL RE_SSUB_SRINGOD                                                19
C -                                                                       19
      REAL SSUB_SRINGTHK                                                  19
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         19
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     19
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       19
     *MSSUB_SRINGTHK)                                                     19
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    19
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           19
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      19
      REAL RE_SSUB_SRINGTHK                                               19
C -                                                                       19
      REAL SSUB_SRCUTPHI                                                  19
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         19
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     19
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       19
     *MSSUB_SRCUTPHI)                                                     19
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    19
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           19
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      19
      REAL RE_SSUB_SRCUTPHI                                               19
C -                                                                       19
      REAL SSUB_SRCUTWID                                                  19
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         19
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     19
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       19
     *MSSUB_SRCUTWID)                                                     19
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    19
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           19
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      19
      REAL RE_SSUB_SRCUTWID                                               19
C -                                                                       19
      REAL SSUB_SRCUTOUT                                                  19
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         19
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     19
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       19
     *MSSUB_SRCUTOUT)                                                     19
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    19
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           19
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      19
      REAL RE_SSUB_SRCUTOUT                                               19
C -                                                                       19
      REAL SSUB_SRCUTIN                                                   19
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           19
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      19
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        19
     *MSSUB_SRCUTIN)                                                      19
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     19
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             19
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       19
      REAL RE_SSUB_SRCUTIN                                                19
C -                                                                       19
      REAL SSUB_SROLLID                                                   19
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           19
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      19
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        19
     *MSSUB_SROLLID)                                                      19
      CHARACTER CSSUB_SROLLID*40/' '/                                     19
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             19
      INTEGER MSSUB_SROLLID(2)/1,1/                                       19
      REAL RE_SSUB_SROLLID                                                19
C -                                                                       19
      REAL SSUB_SROLLOD                                                   19
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           19
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      19
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        19
     *MSSUB_SROLLOD)                                                      19
      CHARACTER CSSUB_SROLLOD*40/' '/                                     19
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             19
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       19
      REAL RE_SSUB_SROLLOD                                                19
C -                                                                       19
      REAL SSUB_SROLLLEN                                                  19
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         19
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     19
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       19
     *MSSUB_SROLLLEN)                                                     19
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    19
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           19
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      19
      REAL RE_SSUB_SROLLLEN                                               19
C -                                                                       19
      REAL SSUB_SWIRELEN                                                  19
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         19
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     19
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       19
     *MSSUB_SWIRELEN)                                                     19
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    19
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           19
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      19
      REAL RE_SSUB_SWIRELEN                                               19
C -                                                                       19
      REAL SSUB_MBLKHGH                                                   19
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           19
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      19
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        19
     *MSSUB_MBLKHGH)                                                      19
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     19
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             19
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       19
      REAL RE_SSUB_MBLKHGH                                                19
C -                                                                       19
      REAL SSUB_MBLKOWID                                                  19
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         19
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     19
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       19
     *MSSUB_MBLKOWID)                                                     19
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    19
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           19
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      19
      REAL RE_SSUB_MBLKOWID                                               19
C -                                                                       19
      REAL SSUB_MBLKOLEN                                                  19
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         19
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     19
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       19
     *MSSUB_MBLKOLEN)                                                     19
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    19
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           19
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      19
      REAL RE_SSUB_MBLKOLEN                                               19
C -                                                                       19
      REAL SSUB_MBLKIWID                                                  19
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         19
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     19
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       19
     *MSSUB_MBLKIWID)                                                     19
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    19
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           19
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      19
      REAL RE_SSUB_MBLKIWID                                               19
C -                                                                       19
      REAL SSUB_MBLKILEN                                                  19
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         19
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     19
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       19
     *MSSUB_MBLKILEN)                                                     19
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    19
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           19
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      19
      REAL RE_SSUB_MBLKILEN                                               19
C -                                                                       19
      REAL SSUB_MBLKORAD                                                  19
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         19
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     19
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       19
     *MSSUB_MBLKORAD)                                                     19
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    19
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           19
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      19
      REAL RE_SSUB_MBLKORAD                                               19
C -                                                                       19
      REAL SSUB_MBLKIRAD                                                  19
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         19
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     19
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       19
     *MSSUB_MBLKIRAD)                                                     19
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    19
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           19
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      19
      REAL RE_SSUB_MBLKIRAD                                               19
C -                                                                       19
      REAL SSUB_MRODDIA                                                   19
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           19
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      19
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        19
     *MSSUB_MRODDIA)                                                      19
      CHARACTER CSSUB_MRODDIA*40/' '/                                     19
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             19
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       19
      REAL RE_SSUB_MRODDIA                                                19
C -                                                                       19
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                19
      PARAMETER (LEN_SSUB=29)                                             19
      PARAMETER (LLEN_SSUB=27)                                            19
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           19
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        19
      DATA IDNUM_SSUB/10*0/                                               19
      REAL BANK_SSUB(LEN_SSUB)                                            19
      CHARACTER*60 BPATH_SSUB/' '/                                        19
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          19
     *BPATH_SSUB                                                          19
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             19
C *                                                                       19
C -                                                                       20
      REAL SWAM_VERSION                                                   20
      EQUIVALENCE (BANK_SWAM(3),SWAM_VERSION,RE_SWAM_VERSION),            20
     *(NAMES_SWAM(1),NSWAM_VERSION)                                       20
      EQUIVALENCE (COMMEN_SWAM(1),CSWAM_VERSION),(MAP_SWAM(1,1),          20
     *MSWAM_VERSION)                                                      20
      CHARACTER CSWAM_VERSION*40/' '/                                     20
      CHARACTER NSWAM_VERSION*12/'R.VERSION'/                             20
      INTEGER MSWAM_VERSION(2)/1,1/                                       20
      REAL RE_SWAM_VERSION                                                20
C -                                                                       20
      REAL SWAM_ZMIN                                                      20
      EQUIVALENCE (BANK_SWAM(4),SWAM_ZMIN,RE_SWAM_ZMIN),(NAMES_SWAM(2),   20
     *NSWAM_ZMIN)                                                         20
      EQUIVALENCE (COMMEN_SWAM(2),CSWAM_ZMIN),(MAP_SWAM(1,2),             20
     *MSWAM_ZMIN)                                                         20
      CHARACTER CSWAM_ZMIN*40/' '/                                        20
      CHARACTER NSWAM_ZMIN*12/'R.ZMIN'/                                   20
      INTEGER MSWAM_ZMIN(2)/1,1/                                          20
      REAL RE_SWAM_ZMIN                                                   20
C -                                                                       20
      REAL SWAM_LEN                                                       20
      EQUIVALENCE (BANK_SWAM(5),SWAM_LEN,RE_SWAM_LEN),(NAMES_SWAM(3),     20
     *NSWAM_LEN)                                                          20
      EQUIVALENCE (COMMEN_SWAM(3),CSWAM_LEN),(MAP_SWAM(1,3),MSWAM_LEN)    20
      CHARACTER CSWAM_LEN*40/' '/                                         20
      CHARACTER NSWAM_LEN*12/'R.LEN'/                                     20
      INTEGER MSWAM_LEN(2)/1,1/                                           20
      REAL RE_SWAM_LEN                                                    20
C -                                                                       20
      REAL SWAM_RMIN                                                      20
      EQUIVALENCE (BANK_SWAM(6),SWAM_RMIN,RE_SWAM_RMIN),(NAMES_SWAM(4),   20
     *NSWAM_RMIN)                                                         20
      EQUIVALENCE (COMMEN_SWAM(4),CSWAM_RMIN),(MAP_SWAM(1,4),             20
     *MSWAM_RMIN)                                                         20
      CHARACTER CSWAM_RMIN*40/' '/                                        20
      CHARACTER NSWAM_RMIN*12/'R.RMIN'/                                   20
      INTEGER MSWAM_RMIN(2)/1,1/                                          20
      REAL RE_SWAM_RMIN                                                   20
C -                                                                       20
      REAL SWAM_RMAX                                                      20
      EQUIVALENCE (BANK_SWAM(7),SWAM_RMAX,RE_SWAM_RMAX),(NAMES_SWAM(5),   20
     *NSWAM_RMAX)                                                         20
      EQUIVALENCE (COMMEN_SWAM(5),CSWAM_RMAX),(MAP_SWAM(1,5),             20
     *MSWAM_RMAX)                                                         20
      CHARACTER CSWAM_RMAX*40/' '/                                        20
      CHARACTER NSWAM_RMAX*12/'R.RMAX'/                                   20
      INTEGER MSWAM_RMAX(2)/1,1/                                          20
      REAL RE_SWAM_RMAX                                                   20
C -                                                                       20
      REAL SWAM_TBRDTHK                                                   20
      EQUIVALENCE (BANK_SWAM(8),SWAM_TBRDTHK,RE_SWAM_TBRDTHK),            20
     *(NAMES_SWAM(6),NSWAM_TBRDTHK)                                       20
      EQUIVALENCE (COMMEN_SWAM(6),CSWAM_TBRDTHK),(MAP_SWAM(1,6),          20
     *MSWAM_TBRDTHK)                                                      20
      CHARACTER CSWAM_TBRDTHK*40/' '/                                     20
      CHARACTER NSWAM_TBRDTHK*12/'R.TBRDTHK'/                             20
      INTEGER MSWAM_TBRDTHK(2)/1,1/                                       20
      REAL RE_SWAM_TBRDTHK                                                20
C -                                                                       20
      REAL SWAM_WALLTHK                                                   20
      EQUIVALENCE (BANK_SWAM(9),SWAM_WALLTHK,RE_SWAM_WALLTHK),            20
     *(NAMES_SWAM(7),NSWAM_WALLTHK)                                       20
      EQUIVALENCE (COMMEN_SWAM(7),CSWAM_WALLTHK),(MAP_SWAM(1,7),          20
     *MSWAM_WALLTHK)                                                      20
      CHARACTER CSWAM_WALLTHK*40/' '/                                     20
      CHARACTER NSWAM_WALLTHK*12/'R.WALLTHK'/                             20
      INTEGER MSWAM_WALLTHK(2)/1,1/                                       20
      REAL RE_SWAM_WALLTHK                                                20
C -                                                                       20
      INTEGER LEN_SWAM,LLEN_SWAM,LINK_SWAM                                20
      PARAMETER (LEN_SWAM=9)                                              20
      PARAMETER (LLEN_SWAM=7)                                             20
      INTEGER FLAG_SWAM/0/,MAP_SWAM(2,LLEN_SWAM),IDNUM_SWAM(10)           20
      CHARACTER NAMES_SWAM(LLEN_SWAM)*12,COMMEN_SWAM(LLEN_SWAM)*40        20
      DATA IDNUM_SWAM/10*0/                                               20
      REAL BANK_SWAM(LEN_SWAM)                                            20
      CHARACTER*60 BPATH_SWAM/' '/                                        20
      SAVE FLAG_SWAM,MAP_SWAM,IDNUM_SWAM,NAMES_SWAM,COMMEN_SWAM,          20
     *BPATH_SWAM                                                          20
      DATA LINK_SWAM/0/,BANK_SWAM/LEN_SWAM*0/                             20
C *                                                                       20
C -                                                                       21
      REAL SERG_VERSION                                                   21
      EQUIVALENCE (BANK_SERG(3),SERG_VERSION,RE_SERG_VERSION),            21
     *(NAMES_SERG(1),NSERG_VERSION)                                       21
      EQUIVALENCE (COMMEN_SERG(1),CSERG_VERSION),(MAP_SERG(1,1),          21
     *MSERG_VERSION)                                                      21
      CHARACTER CSERG_VERSION*40/' '/                                     21
      CHARACTER NSERG_VERSION*12/'R.VERSION'/                             21
      INTEGER MSERG_VERSION(2)/1,1/                                       21
      REAL RE_SERG_VERSION                                                21
C -                                                                       21
      REAL SERG_IRNGTRMX                                                  21
      EQUIVALENCE (BANK_SERG(4),SERG_IRNGTRMX,RE_SERG_IRNGTRMX),          21
     *(NAMES_SERG(2),NSERG_IRNGTRMX)                                      21
      EQUIVALENCE (COMMEN_SERG(2),CSERG_IRNGTRMX),(MAP_SERG(1,2),         21
     *MSERG_IRNGTRMX)                                                     21
      CHARACTER CSERG_IRNGTRMX*40/' '/                                    21
      CHARACTER NSERG_IRNGTRMX*12/'R.IRNGTRMX'/                           21
      INTEGER MSERG_IRNGTRMX(2)/1,1/                                      21
      REAL RE_SERG_IRNGTRMX                                               21
C -                                                                       21
      REAL SERG_IRNGPRMN                                                  21
      EQUIVALENCE (BANK_SERG(5),SERG_IRNGPRMN,RE_SERG_IRNGPRMN),          21
     *(NAMES_SERG(3),NSERG_IRNGPRMN)                                      21
      EQUIVALENCE (COMMEN_SERG(3),CSERG_IRNGPRMN),(MAP_SERG(1,3),         21
     *MSERG_IRNGPRMN)                                                     21
      CHARACTER CSERG_IRNGPRMN*40/' '/                                    21
      CHARACTER NSERG_IRNGPRMN*12/'R.IRNGPRMN'/                           21
      INTEGER MSERG_IRNGPRMN(2)/1,1/                                      21
      REAL RE_SERG_IRNGPRMN                                               21
C -                                                                       21
      REAL SERG_ORNGRMIN                                                  21
      EQUIVALENCE (BANK_SERG(6),SERG_ORNGRMIN,RE_SERG_ORNGRMIN),          21
     *(NAMES_SERG(4),NSERG_ORNGRMIN)                                      21
      EQUIVALENCE (COMMEN_SERG(4),CSERG_ORNGRMIN),(MAP_SERG(1,4),         21
     *MSERG_ORNGRMIN)                                                     21
      CHARACTER CSERG_ORNGRMIN*40/' '/                                    21
      CHARACTER NSERG_ORNGRMIN*12/'R.ORNGRMIN'/                           21
      INTEGER MSERG_ORNGRMIN(2)/1,1/                                      21
      REAL RE_SERG_ORNGRMIN                                               21
C -                                                                       21
      REAL SERG_ORNGRMAX                                                  21
      EQUIVALENCE (BANK_SERG(7),SERG_ORNGRMAX,RE_SERG_ORNGRMAX),          21
     *(NAMES_SERG(5),NSERG_ORNGRMAX)                                      21
      EQUIVALENCE (COMMEN_SERG(5),CSERG_ORNGRMAX),(MAP_SERG(1,5),         21
     *MSERG_ORNGRMAX)                                                     21
      CHARACTER CSERG_ORNGRMAX*40/' '/                                    21
      CHARACTER NSERG_ORNGRMAX*12/'R.ORNGRMAX'/                           21
      INTEGER MSERG_ORNGRMAX(2)/1,1/                                      21
      REAL RE_SERG_ORNGRMAX                                               21
C -                                                                       21
      REAL SERG_ENDRNGTH                                                  21
      EQUIVALENCE (BANK_SERG(8),SERG_ENDRNGTH,RE_SERG_ENDRNGTH),          21
     *(NAMES_SERG(6),NSERG_ENDRNGTH)                                      21
      EQUIVALENCE (COMMEN_SERG(6),CSERG_ENDRNGTH),(MAP_SERG(1,6),         21
     *MSERG_ENDRNGTH)                                                     21
      CHARACTER CSERG_ENDRNGTH*40/' '/                                    21
      CHARACTER NSERG_ENDRNGTH*12/'R.ENDRNGTH'/                           21
      INTEGER MSERG_ENDRNGTH(2)/1,1/                                      21
      REAL RE_SERG_ENDRNGTH                                               21
C -                                                                       21
      REAL SERG_ENDRNGZM                                                  21
      EQUIVALENCE (BANK_SERG(9),SERG_ENDRNGZM,RE_SERG_ENDRNGZM),          21
     *(NAMES_SERG(7),NSERG_ENDRNGZM)                                      21
      EQUIVALENCE (COMMEN_SERG(7),CSERG_ENDRNGZM),(MAP_SERG(1,7),         21
     *MSERG_ENDRNGZM)                                                     21
      CHARACTER CSERG_ENDRNGZM*40/' '/                                    21
      CHARACTER NSERG_ENDRNGZM*12/'R.ENDRNGZM'/                           21
      INTEGER MSERG_ENDRNGZM(2)/1,1/                                      21
      REAL RE_SERG_ENDRNGZM                                               21
C -                                                                       21
      INTEGER LEN_SERG,LLEN_SERG,LINK_SERG                                21
      PARAMETER (LEN_SERG=9)                                              21
      PARAMETER (LLEN_SERG=7)                                             21
      INTEGER FLAG_SERG/0/,MAP_SERG(2,LLEN_SERG),IDNUM_SERG(10)           21
      CHARACTER NAMES_SERG(LLEN_SERG)*12,COMMEN_SERG(LLEN_SERG)*40        21
      DATA IDNUM_SERG/10*0/                                               21
      REAL BANK_SERG(LEN_SERG)                                            21
      CHARACTER*60 BPATH_SERG/' '/                                        21
      SAVE FLAG_SERG,MAP_SERG,IDNUM_SERG,NAMES_SERG,COMMEN_SERG,          21
     *BPATH_SERG                                                          21
      DATA LINK_SERG/0/,BANK_SERG/LEN_SERG*0/                             21
C *                                                                       21
C -                                                                       23
      REAL SELC_VERSION                                                   23
      EQUIVALENCE (BANK_SELC(3),SELC_VERSION,RE_SELC_VERSION),            23
     *(NAMES_SELC(1),NSELC_VERSION)                                       23
      EQUIVALENCE (COMMEN_SELC(1),CSELC_VERSION),(MAP_SELC(1,1),          23
     *MSELC_VERSION)                                                      23
      CHARACTER CSELC_VERSION*40/' '/                                     23
      CHARACTER NSELC_VERSION*12/'R.VERSION'/                             23
      INTEGER MSELC_VERSION(2)/1,1/                                       23
      REAL RE_SELC_VERSION                                                23
C -                                                                       23
      REAL SELC_BETHK                                                     23
      EQUIVALENCE (BANK_SELC(4),SELC_BETHK,RE_SELC_BETHK),                23
     *(NAMES_SELC(2),NSELC_BETHK)                                         23
      EQUIVALENCE (COMMEN_SELC(2),CSELC_BETHK),(MAP_SELC(1,2),            23
     *MSELC_BETHK)                                                        23
      CHARACTER CSELC_BETHK*40/' '/                                       23
      CHARACTER NSELC_BETHK*12/'R.BETHK'/                                 23
      INTEGER MSELC_BETHK(2)/1,1/                                         23
      REAL RE_SELC_BETHK                                                  23
C -                                                                       23
      REAL SELC_WATTHK                                                    23
      EQUIVALENCE (BANK_SELC(5),SELC_WATTHK,RE_SELC_WATTHK),              23
     *(NAMES_SELC(3),NSELC_WATTHK)                                        23
      EQUIVALENCE (COMMEN_SELC(3),CSELC_WATTHK),(MAP_SELC(1,3),           23
     *MSELC_WATTHK)                                                       23
      CHARACTER CSELC_WATTHK*40/' '/                                      23
      CHARACTER NSELC_WATTHK*12/'R.WATTHK'/                               23
      INTEGER MSELC_WATTHK(2)/1,1/                                        23
      REAL RE_SELC_WATTHK                                                 23
C -                                                                       23
      REAL SELC_BEOTHK                                                    23
      EQUIVALENCE (BANK_SELC(6),SELC_BEOTHK,RE_SELC_BEOTHK),              23
     *(NAMES_SELC(4),NSELC_BEOTHK)                                        23
      EQUIVALENCE (COMMEN_SELC(4),CSELC_BEOTHK),(MAP_SELC(1,4),           23
     *MSELC_BEOTHK)                                                       23
      CHARACTER CSELC_BEOTHK*40/' '/                                      23
      CHARACTER NSELC_BEOTHK*12/'R.BEOTHK'/                               23
      INTEGER MSELC_BEOTHK(2)/1,1/                                        23
      REAL RE_SELC_BEOTHK                                                 23
C -                                                                       23
      REAL SELC_DYETHK                                                    23
      EQUIVALENCE (BANK_SELC(7),SELC_DYETHK,RE_SELC_DYETHK),              23
     *(NAMES_SELC(5),NSELC_DYETHK)                                        23
      EQUIVALENCE (COMMEN_SELC(5),CSELC_DYETHK),(MAP_SELC(1,5),           23
     *MSELC_DYETHK)                                                       23
      CHARACTER CSELC_DYETHK*40/' '/                                      23
      CHARACTER NSELC_DYETHK*12/'R.DYETHK'/                               23
      INTEGER MSELC_DYETHK(2)/1,1/                                        23
      REAL RE_SELC_DYETHK                                                 23
C -                                                                       23
      REAL SELC_DYEWID                                                    23
      EQUIVALENCE (BANK_SELC(8),SELC_DYEWID,RE_SELC_DYEWID),              23
     *(NAMES_SELC(6),NSELC_DYEWID)                                        23
      EQUIVALENCE (COMMEN_SELC(6),CSELC_DYEWID),(MAP_SELC(1,6),           23
     *MSELC_DYEWID)                                                       23
      CHARACTER CSELC_DYEWID*40/' '/                                      23
      CHARACTER NSELC_DYEWID*12/'R.DYEWID'/                               23
      INTEGER MSELC_DYEWID(2)/1,1/                                        23
      REAL RE_SELC_DYEWID                                                 23
C -                                                                       23
      REAL SELC_DYESPC                                                    23
      EQUIVALENCE (BANK_SELC(9),SELC_DYESPC,RE_SELC_DYESPC),              23
     *(NAMES_SELC(7),NSELC_DYESPC)                                        23
      EQUIVALENCE (COMMEN_SELC(7),CSELC_DYESPC),(MAP_SELC(1,7),           23
     *MSELC_DYESPC)                                                       23
      CHARACTER CSELC_DYESPC*40/' '/                                      23
      CHARACTER NSELC_DYESPC*12/'R.DYESPC'/                               23
      INTEGER MSELC_DYESPC(2)/1,1/                                        23
      REAL RE_SELC_DYESPC                                                 23
C -                                                                       23
      REAL SELC_ELCAWID                                                   23
      EQUIVALENCE (BANK_SELC(10),SELC_ELCAWID,RE_SELC_ELCAWID),           23
     *(NAMES_SELC(8),NSELC_ELCAWID)                                       23
      EQUIVALENCE (COMMEN_SELC(8),CSELC_ELCAWID),(MAP_SELC(1,8),          23
     *MSELC_ELCAWID)                                                      23
      CHARACTER CSELC_ELCAWID*40/' '/                                     23
      CHARACTER NSELC_ELCAWID*12/'R.ELCAWID'/                             23
      INTEGER MSELC_ELCAWID(2)/1,1/                                       23
      REAL RE_SELC_ELCAWID                                                23
C -                                                                       23
      REAL SELC_AGPDTHK                                                   23
      EQUIVALENCE (BANK_SELC(11),SELC_AGPDTHK,RE_SELC_AGPDTHK),           23
     *(NAMES_SELC(9),NSELC_AGPDTHK)                                       23
      EQUIVALENCE (COMMEN_SELC(9),CSELC_AGPDTHK),(MAP_SELC(1,9),          23
     *MSELC_AGPDTHK)                                                      23
      CHARACTER CSELC_AGPDTHK*40/' '/                                     23
      CHARACTER NSELC_AGPDTHK*12/'R.AGPDTHK'/                             23
      INTEGER MSELC_AGPDTHK(2)/1,1/                                       23
      REAL RE_SELC_AGPDTHK                                                23
C -                                                                       23
      REAL SELC_GLASSTHK                                                  23
      EQUIVALENCE (BANK_SELC(12),SELC_GLASSTHK,RE_SELC_GLASSTHK),         23
     *(NAMES_SELC(10),NSELC_GLASSTHK)                                     23
      EQUIVALENCE (COMMEN_SELC(10),CSELC_GLASSTHK),(MAP_SELC(1,10),       23
     *MSELC_GLASSTHK)                                                     23
      CHARACTER CSELC_GLASSTHK*40/' '/                                    23
      CHARACTER NSELC_GLASSTHK*12/'R.GLASSTHK'/                           23
      INTEGER MSELC_GLASSTHK(2)/1,1/                                      23
      REAL RE_SELC_GLASSTHK                                               23
C -                                                                       23
      REAL SELC_CABTHK                                                    23
      EQUIVALENCE (BANK_SELC(13),SELC_CABTHK,RE_SELC_CABTHK),             23
     *(NAMES_SELC(11),NSELC_CABTHK)                                       23
      EQUIVALENCE (COMMEN_SELC(11),CSELC_CABTHK),(MAP_SELC(1,11),         23
     *MSELC_CABTHK)                                                       23
      CHARACTER CSELC_CABTHK*40/' '/                                      23
      CHARACTER NSELC_CABTHK*12/'R.CABTHK'/                               23
      INTEGER MSELC_CABTHK(2)/1,1/                                        23
      REAL RE_SELC_CABTHK                                                 23
C -                                                                       23
      REAL SELC_CABWID                                                    23
      EQUIVALENCE (BANK_SELC(14),SELC_CABWID,RE_SELC_CABWID),             23
     *(NAMES_SELC(12),NSELC_CABWID)                                       23
      EQUIVALENCE (COMMEN_SELC(12),CSELC_CABWID),(MAP_SELC(1,12),         23
     *MSELC_CABWID)                                                       23
      CHARACTER CSELC_CABWID*40/' '/                                      23
      CHARACTER NSELC_CABWID*12/'R.CABWID'/                               23
      INTEGER MSELC_CABWID(2)/1,1/                                        23
      REAL RE_SELC_CABWID                                                 23
C -                                                                       23
      INTEGER LEN_SELC,LLEN_SELC,LINK_SELC                                23
      PARAMETER (LEN_SELC=14)                                             23
      PARAMETER (LLEN_SELC=12)                                            23
      INTEGER FLAG_SELC/0/,MAP_SELC(2,LLEN_SELC),IDNUM_SELC(10)           23
      CHARACTER NAMES_SELC(LLEN_SELC)*12,COMMEN_SELC(LLEN_SELC)*40        23
      DATA IDNUM_SELC/10*0/                                               23
      REAL BANK_SELC(LEN_SELC)                                            23
      CHARACTER*60 BPATH_SELC/' '/                                        23
      SAVE FLAG_SELC,MAP_SELC,IDNUM_SELC,NAMES_SELC,COMMEN_SELC,          23
     *BPATH_SELC                                                          23
      DATA LINK_SELC/0/,BANK_SELC/LEN_SELC*0/                             23
C *                                                                       23
C -                                                                       25
      INTEGER SVTL_LAYER                                                  25
      EQUIVALENCE (BANK_SVTL(3),SVTL_LAYER,RE_SVTL_LAYER),                25
     *(NAMES_SVTL(1),NSVTL_LAYER)                                         25
      EQUIVALENCE (COMMEN_SVTL(1),CSVTL_LAYER),(MAP_SVTL(1,1),            25
     *MSVTL_LAYER)                                                        25
      CHARACTER CSVTL_LAYER*40/' '/                                       25
      CHARACTER NSVTL_LAYER*12/'I.LAYER'/                                 25
      INTEGER MSVTL_LAYER(2)/1,1/                                         25
      REAL RE_SVTL_LAYER                                                  25
C -                                                                       25
      REAL SVTL_NLADDER                                                   25
      EQUIVALENCE (BANK_SVTL(4),SVTL_NLADDER,RE_SVTL_NLADDER),            25
     *(NAMES_SVTL(2),NSVTL_NLADDER)                                       25
      EQUIVALENCE (COMMEN_SVTL(2),CSVTL_NLADDER),(MAP_SVTL(1,2),          25
     *MSVTL_NLADDER)                                                      25
      CHARACTER CSVTL_NLADDER*40/' '/                                     25
      CHARACTER NSVTL_NLADDER*12/'R.NLADDER'/                             25
      INTEGER MSVTL_NLADDER(2)/1,1/                                       25
      REAL RE_SVTL_NLADDER                                                25
C -                                                                       25
      REAL SVTL_NWAFER                                                    25
      EQUIVALENCE (BANK_SVTL(5),SVTL_NWAFER,RE_SVTL_NWAFER),              25
     *(NAMES_SVTL(3),NSVTL_NWAFER)                                        25
      EQUIVALENCE (COMMEN_SVTL(3),CSVTL_NWAFER),(MAP_SVTL(1,3),           25
     *MSVTL_NWAFER)                                                       25
      CHARACTER CSVTL_NWAFER*40/' '/                                      25
      CHARACTER NSVTL_NWAFER*12/'R.NWAFER'/                               25
      INTEGER MSVTL_NWAFER(2)/1,1/                                        25
      REAL RE_SVTL_NWAFER                                                 25
C -                                                                       25
      REAL SVTL_RADIUS                                                    25
      EQUIVALENCE (BANK_SVTL(6),SVTL_RADIUS,RE_SVTL_RADIUS),              25
     *(NAMES_SVTL(4),NSVTL_RADIUS)                                        25
      EQUIVALENCE (COMMEN_SVTL(4),CSVTL_RADIUS),(MAP_SVTL(1,4),           25
     *MSVTL_RADIUS)                                                       25
      CHARACTER CSVTL_RADIUS*40/' '/                                      25
      CHARACTER NSVTL_RADIUS*12/'R.RADIUS'/                               25
      INTEGER MSVTL_RADIUS(2)/1,1/                                        25
      REAL RE_SVTL_RADIUS                                                 25
C -                                                                       25
      REAL SVTL_BAREEDGE                                                  25
      EQUIVALENCE (BANK_SVTL(7),SVTL_BAREEDGE,RE_SVTL_BAREEDGE),          25
     *(NAMES_SVTL(5),NSVTL_BAREEDGE)                                      25
      EQUIVALENCE (COMMEN_SVTL(5),CSVTL_BAREEDGE),(MAP_SVTL(1,5),         25
     *MSVTL_BAREEDGE)                                                     25
      CHARACTER CSVTL_BAREEDGE*40/' '/                                    25
      CHARACTER NSVTL_BAREEDGE*12/'R.BAREEDGE'/                           25
      INTEGER MSVTL_BAREEDGE(2)/1,1/                                      25
      REAL RE_SVTL_BAREEDGE                                               25
C -                                                                       25
      REAL SVTL_PCBLEN                                                    25
      EQUIVALENCE (BANK_SVTL(8),SVTL_PCBLEN,RE_SVTL_PCBLEN),              25
     *(NAMES_SVTL(6),NSVTL_PCBLEN)                                        25
      EQUIVALENCE (COMMEN_SVTL(6),CSVTL_PCBLEN),(MAP_SVTL(1,6),           25
     *MSVTL_PCBLEN)                                                       25
      CHARACTER CSVTL_PCBLEN*40/' '/                                      25
      CHARACTER NSVTL_PCBLEN*12/'R.PCBLEN'/                               25
      INTEGER MSVTL_PCBLEN(2)/1,1/                                        25
      REAL RE_SVTL_PCBLEN                                                 25
C -                                                                       25
      REAL SVTL_PCBWIDTH                                                  25
      EQUIVALENCE (BANK_SVTL(9),SVTL_PCBWIDTH,RE_SVTL_PCBWIDTH),          25
     *(NAMES_SVTL(7),NSVTL_PCBWIDTH)                                      25
      EQUIVALENCE (COMMEN_SVTL(7),CSVTL_PCBWIDTH),(MAP_SVTL(1,7),         25
     *MSVTL_PCBWIDTH)                                                     25
      CHARACTER CSVTL_PCBWIDTH*40/' '/                                    25
      CHARACTER NSVTL_PCBWIDTH*12/'R.PCBWIDTH'/                           25
      INTEGER MSVTL_PCBWIDTH(2)/1,1/                                      25
      REAL RE_SVTL_PCBWIDTH                                               25
C -                                                                       25
      REAL SVTL_PCBTHK                                                    25
      EQUIVALENCE (BANK_SVTL(10),SVTL_PCBTHK,RE_SVTL_PCBTHK),             25
     *(NAMES_SVTL(8),NSVTL_PCBTHK)                                        25
      EQUIVALENCE (COMMEN_SVTL(8),CSVTL_PCBTHK),(MAP_SVTL(1,8),           25
     *MSVTL_PCBTHK)                                                       25
      CHARACTER CSVTL_PCBTHK*40/' '/                                      25
      CHARACTER NSVTL_PCBTHK*12/'R.PCBTHK'/                               25
      INTEGER MSVTL_PCBTHK(2)/1,1/                                        25
      REAL RE_SVTL_PCBTHK                                                 25
C -                                                                       25
      REAL SVTL_PCBGAP                                                    25
      EQUIVALENCE (BANK_SVTL(11),SVTL_PCBGAP,RE_SVTL_PCBGAP),             25
     *(NAMES_SVTL(9),NSVTL_PCBGAP)                                        25
      EQUIVALENCE (COMMEN_SVTL(9),CSVTL_PCBGAP),(MAP_SVTL(1,9),           25
     *MSVTL_PCBGAP)                                                       25
      CHARACTER CSVTL_PCBGAP*40/' '/                                      25
      CHARACTER NSVTL_PCBGAP*12/'R.PCBGAP'/                               25
      INTEGER MSVTL_PCBGAP(2)/1,1/                                        25
      REAL RE_SVTL_PCBGAP                                                 25
C -                                                                       25
      INTEGER LEN_SVTL,LLEN_SVTL,LINK_SVTL                                25
      PARAMETER (LEN_SVTL=11)                                             25
      PARAMETER (LLEN_SVTL=9)                                             25
      INTEGER FLAG_SVTL/0/,MAP_SVTL(2,LLEN_SVTL),IDNUM_SVTL(10)           25
      CHARACTER NAMES_SVTL(LLEN_SVTL)*12,COMMEN_SVTL(LLEN_SVTL)*40        25
      DATA IDNUM_SVTL/10*0/                                               25
      REAL BANK_SVTL(LEN_SVTL)                                            25
      CHARACTER*60 BPATH_SVTL/' '/                                        25
      SAVE FLAG_SVTL,MAP_SVTL,IDNUM_SVTL,NAMES_SVTL,COMMEN_SVTL,          25
     *BPATH_SVTL                                                          25
      DATA LINK_SVTL/0/,BANK_SVTL/LEN_SVTL*0/                             25
C *                                                                       25
C -                                                                       27
      REAL SSLD_VERSION                                                   27
      EQUIVALENCE (BANK_SSLD(3),SSLD_VERSION,RE_SSLD_VERSION),            27
     *(NAMES_SSLD(1),NSSLD_VERSION)                                       27
      EQUIVALENCE (COMMEN_SSLD(1),CSSLD_VERSION),(MAP_SSLD(1,1),          27
     *MSSLD_VERSION)                                                      27
      CHARACTER CSSLD_VERSION*40/' '/                                     27
      CHARACTER NSSLD_VERSION*12/'R.VERSION'/                             27
      INTEGER MSSLD_VERSION(2)/1,1/                                       27
      REAL RE_SSLD_VERSION                                                27
C -                                                                       27
      REAL SSLD_SINRINN                                                   27
      EQUIVALENCE (BANK_SSLD(4),SSLD_SINRINN,RE_SSLD_SINRINN),            27
     *(NAMES_SSLD(2),NSSLD_SINRINN)                                       27
      EQUIVALENCE (COMMEN_SSLD(2),CSSLD_SINRINN),(MAP_SSLD(1,2),          27
     *MSSLD_SINRINN)                                                      27
      CHARACTER CSSLD_SINRINN*40/' '/                                     27
      CHARACTER NSSLD_SINRINN*12/'R.SINRINN'/                             27
      INTEGER MSSLD_SINRINN(2)/1,1/                                       27
      REAL RE_SSLD_SINRINN                                                27
C -                                                                       27
      REAL SSLD_SINROUT                                                   27
      EQUIVALENCE (BANK_SSLD(5),SSLD_SINROUT,RE_SSLD_SINROUT),            27
     *(NAMES_SSLD(3),NSSLD_SINROUT)                                       27
      EQUIVALENCE (COMMEN_SSLD(3),CSSLD_SINROUT),(MAP_SSLD(1,3),          27
     *MSSLD_SINROUT)                                                      27
      CHARACTER CSSLD_SINROUT*40/' '/                                     27
      CHARACTER NSSLD_SINROUT*12/'R.SINROUT'/                             27
      INTEGER MSSLD_SINROUT(2)/1,1/                                       27
      REAL RE_SSLD_SINROUT                                                27
C -                                                                       27
      REAL SSLD_SINLEN                                                    27
      EQUIVALENCE (BANK_SSLD(6),SSLD_SINLEN,RE_SSLD_SINLEN),              27
     *(NAMES_SSLD(4),NSSLD_SINLEN)                                        27
      EQUIVALENCE (COMMEN_SSLD(4),CSSLD_SINLEN),(MAP_SSLD(1,4),           27
     *MSSLD_SINLEN)                                                       27
      CHARACTER CSSLD_SINLEN*40/' '/                                      27
      CHARACTER NSSLD_SINLEN*12/'R.SINLEN'/                               27
      INTEGER MSSLD_SINLEN(2)/1,1/                                        27
      REAL RE_SSLD_SINLEN                                                 27
C -                                                                       27
      REAL SSLD_SSEPRINN                                                  27
      EQUIVALENCE (BANK_SSLD(7),SSLD_SSEPRINN,RE_SSLD_SSEPRINN),          27
     *(NAMES_SSLD(5),NSSLD_SSEPRINN)                                      27
      EQUIVALENCE (COMMEN_SSLD(5),CSSLD_SSEPRINN),(MAP_SSLD(1,5),         27
     *MSSLD_SSEPRINN)                                                     27
      CHARACTER CSSLD_SSEPRINN*40/' '/                                    27
      CHARACTER NSSLD_SSEPRINN*12/'R.SSEPRINN'/                           27
      INTEGER MSSLD_SSEPRINN(2)/1,1/                                      27
      REAL RE_SSLD_SSEPRINN                                               27
C -                                                                       27
      REAL SSLD_SSEPROUT                                                  27
      EQUIVALENCE (BANK_SSLD(8),SSLD_SSEPROUT,RE_SSLD_SSEPROUT),          27
     *(NAMES_SSLD(6),NSSLD_SSEPROUT)                                      27
      EQUIVALENCE (COMMEN_SSLD(6),CSSLD_SSEPROUT),(MAP_SSLD(1,6),         27
     *MSSLD_SSEPROUT)                                                     27
      CHARACTER CSSLD_SSEPROUT*40/' '/                                    27
      CHARACTER NSSLD_SSEPROUT*12/'R.SSEPROUT'/                           27
      INTEGER MSSLD_SSEPROUT(2)/1,1/                                      27
      REAL RE_SSLD_SSEPROUT                                               27
C -                                                                       27
      REAL SSLD_SSEPLEN                                                   27
      EQUIVALENCE (BANK_SSLD(9),SSLD_SSEPLEN,RE_SSLD_SSEPLEN),            27
     *(NAMES_SSLD(7),NSSLD_SSEPLEN)                                       27
      EQUIVALENCE (COMMEN_SSLD(7),CSSLD_SSEPLEN),(MAP_SSLD(1,7),          27
     *MSSLD_SSEPLEN)                                                      27
      CHARACTER CSSLD_SSEPLEN*40/' '/                                     27
      CHARACTER NSSLD_SSEPLEN*12/'R.SSEPLEN'/                             27
      INTEGER MSSLD_SSEPLEN(2)/1,1/                                       27
      REAL RE_SSLD_SSEPLEN                                                27
C -                                                                       27
      REAL SSLD_SOUTRINN                                                  27
      EQUIVALENCE (BANK_SSLD(10),SSLD_SOUTRINN,RE_SSLD_SOUTRINN),         27
     *(NAMES_SSLD(8),NSSLD_SOUTRINN)                                      27
      EQUIVALENCE (COMMEN_SSLD(8),CSSLD_SOUTRINN),(MAP_SSLD(1,8),         27
     *MSSLD_SOUTRINN)                                                     27
      CHARACTER CSSLD_SOUTRINN*40/' '/                                    27
      CHARACTER NSSLD_SOUTRINN*12/'R.SOUTRINN'/                           27
      INTEGER MSSLD_SOUTRINN(2)/1,1/                                      27
      REAL RE_SSLD_SOUTRINN                                               27
C -                                                                       27
      REAL SSLD_SOUTROUT                                                  27
      EQUIVALENCE (BANK_SSLD(11),SSLD_SOUTROUT,RE_SSLD_SOUTROUT),         27
     *(NAMES_SSLD(9),NSSLD_SOUTROUT)                                      27
      EQUIVALENCE (COMMEN_SSLD(9),CSSLD_SOUTROUT),(MAP_SSLD(1,9),         27
     *MSSLD_SOUTROUT)                                                     27
      CHARACTER CSSLD_SOUTROUT*40/' '/                                    27
      CHARACTER NSSLD_SOUTROUT*12/'R.SOUTROUT'/                           27
      INTEGER MSSLD_SOUTROUT(2)/1,1/                                      27
      REAL RE_SSLD_SOUTROUT                                               27
C -                                                                       27
      REAL SSLD_SOUTLEN                                                   27
      EQUIVALENCE (BANK_SSLD(12),SSLD_SOUTLEN,RE_SSLD_SOUTLEN),           27
     *(NAMES_SSLD(10),NSSLD_SOUTLEN)                                      27
      EQUIVALENCE (COMMEN_SSLD(10),CSSLD_SOUTLEN),(MAP_SSLD(1,10),        27
     *MSSLD_SOUTLEN)                                                      27
      CHARACTER CSSLD_SOUTLEN*40/' '/                                     27
      CHARACTER NSSLD_SOUTLEN*12/'R.SOUTLEN'/                             27
      INTEGER MSSLD_SOUTLEN(2)/1,1/                                       27
      REAL RE_SSLD_SOUTLEN                                                27
C -                                                                       27
      REAL SSLD_ALMESHID                                                  27
      EQUIVALENCE (BANK_SSLD(13),SSLD_ALMESHID,RE_SSLD_ALMESHID),         27
     *(NAMES_SSLD(11),NSSLD_ALMESHID)                                     27
      EQUIVALENCE (COMMEN_SSLD(11),CSSLD_ALMESHID),(MAP_SSLD(1,11),       27
     *MSSLD_ALMESHID)                                                     27
      CHARACTER CSSLD_ALMESHID*40/' '/                                    27
      CHARACTER NSSLD_ALMESHID*12/'R.ALMESHID'/                           27
      INTEGER MSSLD_ALMESHID(2)/1,1/                                      27
      REAL RE_SSLD_ALMESHID                                               27
C -                                                                       27
      REAL SSLD_ALMESHOD                                                  27
      EQUIVALENCE (BANK_SSLD(14),SSLD_ALMESHOD,RE_SSLD_ALMESHOD),         27
     *(NAMES_SSLD(12),NSSLD_ALMESHOD)                                     27
      EQUIVALENCE (COMMEN_SSLD(12),CSSLD_ALMESHOD),(MAP_SSLD(1,12),       27
     *MSSLD_ALMESHOD)                                                     27
      CHARACTER CSSLD_ALMESHOD*40/' '/                                    27
      CHARACTER NSSLD_ALMESHOD*12/'R.ALMESHOD'/                           27
      INTEGER MSSLD_ALMESHOD(2)/1,1/                                      27
      REAL RE_SSLD_ALMESHOD                                               27
C -                                                                       27
      REAL SSLD_ALMSHTHK                                                  27
      EQUIVALENCE (BANK_SSLD(15),SSLD_ALMSHTHK,RE_SSLD_ALMSHTHK),         27
     *(NAMES_SSLD(13),NSSLD_ALMSHTHK)                                     27
      EQUIVALENCE (COMMEN_SSLD(13),CSSLD_ALMSHTHK),(MAP_SSLD(1,13),       27
     *MSSLD_ALMSHTHK)                                                     27
      CHARACTER CSSLD_ALMSHTHK*40/' '/                                    27
      CHARACTER NSSLD_ALMSHTHK*12/'R.ALMSHTHK'/                           27
      INTEGER MSSLD_ALMSHTHK(2)/1,1/                                      27
      REAL RE_SSLD_ALMSHTHK                                               27
C -                                                                       27
      REAL SSLD_ALMSHPOS                                                  27
      EQUIVALENCE (BANK_SSLD(16),SSLD_ALMSHPOS,RE_SSLD_ALMSHPOS),         27
     *(NAMES_SSLD(14),NSSLD_ALMSHPOS)                                     27
      EQUIVALENCE (COMMEN_SSLD(14),CSSLD_ALMSHPOS),(MAP_SSLD(1,14),       27
     *MSSLD_ALMSHPOS)                                                     27
      CHARACTER CSSLD_ALMSHPOS*40/' '/                                    27
      CHARACTER NSSLD_ALMSHPOS*12/'R.ALMSHPOS'/                           27
      INTEGER MSSLD_ALMSHPOS(2)/1,1/                                      27
      REAL RE_SSLD_ALMSHPOS                                               27
C -                                                                       27
      INTEGER LEN_SSLD,LLEN_SSLD,LINK_SSLD                                27
      PARAMETER (LEN_SSLD=16)                                             27
      PARAMETER (LLEN_SSLD=14)                                            27
      INTEGER FLAG_SSLD/0/,MAP_SSLD(2,LLEN_SSLD),IDNUM_SSLD(10)           27
      CHARACTER NAMES_SSLD(LLEN_SSLD)*12,COMMEN_SSLD(LLEN_SSLD)*40        27
      DATA IDNUM_SSLD/10*0/                                               27
      REAL BANK_SSLD(LEN_SSLD)                                            27
      CHARACTER*60 BPATH_SSLD/' '/                                        27
      SAVE FLAG_SSLD,MAP_SSLD,IDNUM_SSLD,NAMES_SSLD,COMMEN_SSLD,          27
     *BPATH_SSLD                                                          27
      DATA LINK_SSLD/0/,BANK_SSLD/LEN_SSLD*0/                             27
C *                                                                       27
C -                                                                       28
      INTEGER SCBP_LAYER                                                  28
      EQUIVALENCE (BANK_SCBP(3),SCBP_LAYER,RE_SCBP_LAYER),                28
     *(NAMES_SCBP(1),NSCBP_LAYER)                                         28
      EQUIVALENCE (COMMEN_SCBP(1),CSCBP_LAYER),(MAP_SCBP(1,1),            28
     *MSCBP_LAYER)                                                        28
      CHARACTER CSCBP_LAYER*40/' '/                                       28
      CHARACTER NSCBP_LAYER*12/'I.LAYER'/                                 28
      INTEGER MSCBP_LAYER(2)/1,1/                                         28
      REAL RE_SCBP_LAYER                                                  28
C -                                                                       28
      REAL SCBP_LEN                                                       28
      EQUIVALENCE (BANK_SCBP(4),SCBP_LEN,RE_SCBP_LEN),(NAMES_SCBP(2),     28
     *NSCBP_LEN)                                                          28
      EQUIVALENCE (COMMEN_SCBP(2),CSCBP_LEN),(MAP_SCBP(1,2),MSCBP_LEN)    28
      CHARACTER CSCBP_LEN*40/' '/                                         28
      CHARACTER NSCBP_LEN*12/'R.LEN'/                                     28
      INTEGER MSCBP_LEN(2)/1,1/                                           28
      REAL RE_SCBP_LEN                                                    28
C -                                                                       28
      REAL SCBP_RMIN1                                                     28
      EQUIVALENCE (BANK_SCBP(5),SCBP_RMIN1,RE_SCBP_RMIN1),                28
     *(NAMES_SCBP(3),NSCBP_RMIN1)                                         28
      EQUIVALENCE (COMMEN_SCBP(3),CSCBP_RMIN1),(MAP_SCBP(1,3),            28
     *MSCBP_RMIN1)                                                        28
      CHARACTER CSCBP_RMIN1*40/' '/                                       28
      CHARACTER NSCBP_RMIN1*12/'R.RMIN1'/                                 28
      INTEGER MSCBP_RMIN1(2)/1,1/                                         28
      REAL RE_SCBP_RMIN1                                                  28
C -                                                                       28
      REAL SCBP_RMAX1                                                     28
      EQUIVALENCE (BANK_SCBP(6),SCBP_RMAX1,RE_SCBP_RMAX1),                28
     *(NAMES_SCBP(4),NSCBP_RMAX1)                                         28
      EQUIVALENCE (COMMEN_SCBP(4),CSCBP_RMAX1),(MAP_SCBP(1,4),            28
     *MSCBP_RMAX1)                                                        28
      CHARACTER CSCBP_RMAX1*40/' '/                                       28
      CHARACTER NSCBP_RMAX1*12/'R.RMAX1'/                                 28
      INTEGER MSCBP_RMAX1(2)/1,1/                                         28
      REAL RE_SCBP_RMAX1                                                  28
C -                                                                       28
      REAL SCBP_RMIN2                                                     28
      EQUIVALENCE (BANK_SCBP(7),SCBP_RMIN2,RE_SCBP_RMIN2),                28
     *(NAMES_SCBP(5),NSCBP_RMIN2)                                         28
      EQUIVALENCE (COMMEN_SCBP(5),CSCBP_RMIN2),(MAP_SCBP(1,5),            28
     *MSCBP_RMIN2)                                                        28
      CHARACTER CSCBP_RMIN2*40/' '/                                       28
      CHARACTER NSCBP_RMIN2*12/'R.RMIN2'/                                 28
      INTEGER MSCBP_RMIN2(2)/1,1/                                         28
      REAL RE_SCBP_RMIN2                                                  28
C -                                                                       28
      REAL SCBP_RMAX2                                                     28
      EQUIVALENCE (BANK_SCBP(8),SCBP_RMAX2,RE_SCBP_RMAX2),                28
     *(NAMES_SCBP(6),NSCBP_RMAX2)                                         28
      EQUIVALENCE (COMMEN_SCBP(6),CSCBP_RMAX2),(MAP_SCBP(1,6),            28
     *MSCBP_RMAX2)                                                        28
      CHARACTER CSCBP_RMAX2*40/' '/                                       28
      CHARACTER NSCBP_RMAX2*12/'R.RMAX2'/                                 28
      INTEGER MSCBP_RMAX2(2)/1,1/                                         28
      REAL RE_SCBP_RMAX2                                                  28
C -                                                                       28
      REAL SCBP_VOL                                                       28
      EQUIVALENCE (BANK_SCBP(9),SCBP_VOL,RE_SCBP_VOL),(NAMES_SCBP(7),     28
     *NSCBP_VOL)                                                          28
      EQUIVALENCE (COMMEN_SCBP(7),CSCBP_VOL),(MAP_SCBP(1,7),MSCBP_VOL)    28
      CHARACTER CSCBP_VOL*40/' '/                                         28
      CHARACTER NSCBP_VOL*12/'R.VOL'/                                     28
      INTEGER MSCBP_VOL(2)/1,1/                                           28
      REAL RE_SCBP_VOL                                                    28
C -                                                                       28
      INTEGER LEN_SCBP,LLEN_SCBP,LINK_SCBP                                28
      PARAMETER (LEN_SCBP=9)                                              28
      PARAMETER (LLEN_SCBP=7)                                             28
      INTEGER FLAG_SCBP/0/,MAP_SCBP(2,LLEN_SCBP),IDNUM_SCBP(10)           28
      CHARACTER NAMES_SCBP(LLEN_SCBP)*12,COMMEN_SCBP(LLEN_SCBP)*40        28
      DATA IDNUM_SCBP/10*0/                                               28
      REAL BANK_SCBP(LEN_SCBP)                                            28
      CHARACTER*60 BPATH_SCBP/' '/                                        28
      SAVE FLAG_SCBP,MAP_SCBP,IDNUM_SCBP,NAMES_SCBP,COMMEN_SCBP,          28
     *BPATH_SCBP                                                          28
      DATA LINK_SCBP/0/,BANK_SCBP/LEN_SCBP*0/                             28
C *                                                                       28
C -                                                                       29
      INTEGER SFEP_LAYER                                                  29
      EQUIVALENCE (BANK_SFEP(3),SFEP_LAYER,RE_SFEP_LAYER),                29
     *(NAMES_SFEP(1),NSFEP_LAYER)                                         29
      EQUIVALENCE (COMMEN_SFEP(1),CSFEP_LAYER),(MAP_SFEP(1,1),            29
     *MSFEP_LAYER)                                                        29
      CHARACTER CSFEP_LAYER*40/' '/                                       29
      CHARACTER NSFEP_LAYER*12/'I.LAYER'/                                 29
      INTEGER MSFEP_LAYER(2)/1,1/                                         29
      REAL RE_SFEP_LAYER                                                  29
C -                                                                       29
      REAL SFEP_LEN                                                       29
      EQUIVALENCE (BANK_SFEP(4),SFEP_LEN,RE_SFEP_LEN),(NAMES_SFEP(2),     29
     *NSFEP_LEN)                                                          29
      EQUIVALENCE (COMMEN_SFEP(2),CSFEP_LEN),(MAP_SFEP(1,2),MSFEP_LEN)    29
      CHARACTER CSFEP_LEN*40/' '/                                         29
      CHARACTER NSFEP_LEN*12/'R.LEN'/                                     29
      INTEGER MSFEP_LEN(2)/1,1/                                           29
      REAL RE_SFEP_LEN                                                    29
C -                                                                       29
      REAL SFEP_RMIN1                                                     29
      EQUIVALENCE (BANK_SFEP(5),SFEP_RMIN1,RE_SFEP_RMIN1),                29
     *(NAMES_SFEP(3),NSFEP_RMIN1)                                         29
      EQUIVALENCE (COMMEN_SFEP(3),CSFEP_RMIN1),(MAP_SFEP(1,3),            29
     *MSFEP_RMIN1)                                                        29
      CHARACTER CSFEP_RMIN1*40/' '/                                       29
      CHARACTER NSFEP_RMIN1*12/'R.RMIN1'/                                 29
      INTEGER MSFEP_RMIN1(2)/1,1/                                         29
      REAL RE_SFEP_RMIN1                                                  29
C -                                                                       29
      REAL SFEP_RMAX1                                                     29
      EQUIVALENCE (BANK_SFEP(6),SFEP_RMAX1,RE_SFEP_RMAX1),                29
     *(NAMES_SFEP(4),NSFEP_RMAX1)                                         29
      EQUIVALENCE (COMMEN_SFEP(4),CSFEP_RMAX1),(MAP_SFEP(1,4),            29
     *MSFEP_RMAX1)                                                        29
      CHARACTER CSFEP_RMAX1*40/' '/                                       29
      CHARACTER NSFEP_RMAX1*12/'R.RMAX1'/                                 29
      INTEGER MSFEP_RMAX1(2)/1,1/                                         29
      REAL RE_SFEP_RMAX1                                                  29
C -                                                                       29
      REAL SFEP_RMIN2                                                     29
      EQUIVALENCE (BANK_SFEP(7),SFEP_RMIN2,RE_SFEP_RMIN2),                29
     *(NAMES_SFEP(5),NSFEP_RMIN2)                                         29
      EQUIVALENCE (COMMEN_SFEP(5),CSFEP_RMIN2),(MAP_SFEP(1,5),            29
     *MSFEP_RMIN2)                                                        29
      CHARACTER CSFEP_RMIN2*40/' '/                                       29
      CHARACTER NSFEP_RMIN2*12/'R.RMIN2'/                                 29
      INTEGER MSFEP_RMIN2(2)/1,1/                                         29
      REAL RE_SFEP_RMIN2                                                  29
C -                                                                       29
      REAL SFEP_RMAX2                                                     29
      EQUIVALENCE (BANK_SFEP(8),SFEP_RMAX2,RE_SFEP_RMAX2),                29
     *(NAMES_SFEP(6),NSFEP_RMAX2)                                         29
      EQUIVALENCE (COMMEN_SFEP(6),CSFEP_RMAX2),(MAP_SFEP(1,6),            29
     *MSFEP_RMAX2)                                                        29
      CHARACTER CSFEP_RMAX2*40/' '/                                       29
      CHARACTER NSFEP_RMAX2*12/'R.RMAX2'/                                 29
      INTEGER MSFEP_RMAX2(2)/1,1/                                         29
      REAL RE_SFEP_RMAX2                                                  29
C -                                                                       29
      REAL SFEP_VOL                                                       29
      EQUIVALENCE (BANK_SFEP(9),SFEP_VOL,RE_SFEP_VOL),(NAMES_SFEP(7),     29
     *NSFEP_VOL)                                                          29
      EQUIVALENCE (COMMEN_SFEP(7),CSFEP_VOL),(MAP_SFEP(1,7),MSFEP_VOL)    29
      CHARACTER CSFEP_VOL*40/' '/                                         29
      CHARACTER NSFEP_VOL*12/'R.VOL'/                                     29
      INTEGER MSFEP_VOL(2)/1,1/                                           29
      REAL RE_SFEP_VOL                                                    29
C -                                                                       29
      REAL SFEP_VOLPLAST                                                  29
      EQUIVALENCE (BANK_SFEP(10),SFEP_VOLPLAST,RE_SFEP_VOLPLAST),         29
     *(NAMES_SFEP(8),NSFEP_VOLPLAST)                                      29
      EQUIVALENCE (COMMEN_SFEP(8),CSFEP_VOLPLAST),(MAP_SFEP(1,8),         29
     *MSFEP_VOLPLAST)                                                     29
      CHARACTER CSFEP_VOLPLAST*40/' '/                                    29
      CHARACTER NSFEP_VOLPLAST*12/'R.VOLPLAST'/                           29
      INTEGER MSFEP_VOLPLAST(2)/1,1/                                      29
      REAL RE_SFEP_VOLPLAST                                               29
C -                                                                       29
      INTEGER LEN_SFEP,LLEN_SFEP,LINK_SFEP                                29
      PARAMETER (LEN_SFEP=10)                                             29
      PARAMETER (LLEN_SFEP=8)                                             29
      INTEGER FLAG_SFEP/0/,MAP_SFEP(2,LLEN_SFEP),IDNUM_SFEP(10)           29
      CHARACTER NAMES_SFEP(LLEN_SFEP)*12,COMMEN_SFEP(LLEN_SFEP)*40        29
      DATA IDNUM_SFEP/10*0/                                               29
      REAL BANK_SFEP(LEN_SFEP)                                            29
      CHARACTER*60 BPATH_SFEP/' '/                                        29
      SAVE FLAG_SFEP,MAP_SFEP,IDNUM_SFEP,NAMES_SFEP,COMMEN_SFEP,          29
     *BPATH_SFEP                                                          29
      DATA LINK_SFEP/0/,BANK_SFEP/LEN_SFEP*0/                             29
C *                                                                       29
C -                                                                       30
      INTEGER SWCX_LAYER                                                  30
      EQUIVALENCE (BANK_SWCX(3),SWCX_LAYER,RE_SWCX_LAYER),                30
     *(NAMES_SWCX(1),NSWCX_LAYER)                                         30
      EQUIVALENCE (COMMEN_SWCX(1),CSWCX_LAYER),(MAP_SWCX(1,1),            30
     *MSWCX_LAYER)                                                        30
      CHARACTER CSWCX_LAYER*40/' '/                                       30
      CHARACTER NSWCX_LAYER*12/'I.LAYER'/                                 30
      INTEGER MSWCX_LAYER(2)/1,1/                                         30
      REAL RE_SWCX_LAYER                                                  30
C -                                                                       30
      REAL SWCX_LENGTH                                                    30
      EQUIVALENCE (BANK_SWCX(4),SWCX_LENGTH,RE_SWCX_LENGTH),              30
     *(NAMES_SWCX(2),NSWCX_LENGTH)                                        30
      EQUIVALENCE (COMMEN_SWCX(2),CSWCX_LENGTH),(MAP_SWCX(1,2),           30
     *MSWCX_LENGTH)                                                       30
      CHARACTER CSWCX_LENGTH*40/' '/                                      30
      CHARACTER NSWCX_LENGTH*12/'R.LENGTH'/                               30
      INTEGER MSWCX_LENGTH(2)/1,1/                                        30
      REAL RE_SWCX_LENGTH                                                 30
C -                                                                       30
      REAL SWCX_DR                                                        30
      EQUIVALENCE (BANK_SWCX(5),SWCX_DR,RE_SWCX_DR),(NAMES_SWCX(3),       30
     *NSWCX_DR)                                                           30
      EQUIVALENCE (COMMEN_SWCX(3),CSWCX_DR),(MAP_SWCX(1,3),MSWCX_DR)      30
      CHARACTER CSWCX_DR*40/' '/                                          30
      CHARACTER NSWCX_DR*12/'R.DR'/                                       30
      INTEGER MSWCX_DR(2)/1,1/                                            30
      REAL RE_SWCX_DR                                                     30
C -                                                                       30
      REAL SWCX_OFFSET                                                    30
      EQUIVALENCE (BANK_SWCX(6),SWCX_OFFSET,RE_SWCX_OFFSET),              30
     *(NAMES_SWCX(4),NSWCX_OFFSET)                                        30
      EQUIVALENCE (COMMEN_SWCX(4),CSWCX_OFFSET),(MAP_SWCX(1,4),           30
     *MSWCX_OFFSET)                                                       30
      CHARACTER CSWCX_OFFSET*40/' '/                                      30
      CHARACTER NSWCX_OFFSET*12/'R.OFFSET'/                               30
      INTEGER MSWCX_OFFSET(2)/1,1/                                        30
      REAL RE_SWCX_OFFSET                                                 30
C -                                                                       30
      REAL SWCX_RAD                                                       30
      EQUIVALENCE (BANK_SWCX(7),SWCX_RAD,RE_SWCX_RAD),(NAMES_SWCX(5),     30
     *NSWCX_RAD)                                                          30
      EQUIVALENCE (COMMEN_SWCX(5),CSWCX_RAD),(MAP_SWCX(1,5),MSWCX_RAD)    30
      CHARACTER CSWCX_RAD*40/' '/                                         30
      CHARACTER NSWCX_RAD*12/'R.RAD'/                                     30
      INTEGER MSWCX_RAD(2)/1,1/                                           30
      REAL RE_SWCX_RAD                                                    30
C -                                                                       30
      REAL SWCX_WALL                                                      30
      EQUIVALENCE (BANK_SWCX(8),SWCX_WALL,RE_SWCX_WALL),(NAMES_SWCX(6),   30
     *NSWCX_WALL)                                                         30
      EQUIVALENCE (COMMEN_SWCX(6),CSWCX_WALL),(MAP_SWCX(1,6),             30
     *MSWCX_WALL)                                                         30
      CHARACTER CSWCX_WALL*40/' '/                                        30
      CHARACTER NSWCX_WALL*12/'R.WALL'/                                   30
      INTEGER MSWCX_WALL(2)/1,1/                                          30
      REAL RE_SWCX_WALL                                                   30
C -                                                                       30
      REAL SWCX_ROFFSET                                                   30
      EQUIVALENCE (BANK_SWCX(9),SWCX_ROFFSET,RE_SWCX_ROFFSET),            30
     *(NAMES_SWCX(7),NSWCX_ROFFSET)                                       30
      EQUIVALENCE (COMMEN_SWCX(7),CSWCX_ROFFSET),(MAP_SWCX(1,7),          30
     *MSWCX_ROFFSET)                                                      30
      CHARACTER CSWCX_ROFFSET*40/' '/                                     30
      CHARACTER NSWCX_ROFFSET*12/'R.ROFFSET'/                             30
      INTEGER MSWCX_ROFFSET(2)/1,1/                                       30
      REAL RE_SWCX_ROFFSET                                                30
C -                                                                       30
      INTEGER LEN_SWCX,LLEN_SWCX,LINK_SWCX                                30
      PARAMETER (LEN_SWCX=9)                                              30
      PARAMETER (LLEN_SWCX=7)                                             30
      INTEGER FLAG_SWCX/0/,MAP_SWCX(2,LLEN_SWCX),IDNUM_SWCX(10)           30
      CHARACTER NAMES_SWCX(LLEN_SWCX)*12,COMMEN_SWCX(LLEN_SWCX)*40        30
      DATA IDNUM_SWCX/10*0/                                               30
      REAL BANK_SWCX(LEN_SWCX)                                            30
      CHARACTER*60 BPATH_SWCX/' '/                                        30
      SAVE FLAG_SWCX,MAP_SWCX,IDNUM_SWCX,NAMES_SWCX,COMMEN_SWCX,          30
     *BPATH_SWCX                                                          30
      DATA LINK_SWCX/0/,BANK_SWCX/LEN_SWCX*0/                             30
C *                                                                       30
C -                                                                       31
      REAL SOUP_VERSION                                                   31
      EQUIVALENCE (BANK_SOUP(3),SOUP_VERSION,RE_SOUP_VERSION),            31
     *(NAMES_SOUP(1),NSOUP_VERSION)                                       31
      EQUIVALENCE (COMMEN_SOUP(1),CSOUP_VERSION),(MAP_SOUP(1,1),          31
     *MSOUP_VERSION)                                                      31
      CHARACTER CSOUP_VERSION*40/' '/                                     31
      CHARACTER NSOUP_VERSION*12/'R.VERSION'/                             31
      INTEGER MSOUP_VERSION(2)/1,1/                                       31
      REAL RE_SOUP_VERSION                                                31
C -                                                                       31
      REAL SOUP_LENGTH                                                    31
      EQUIVALENCE (BANK_SOUP(4),SOUP_LENGTH,RE_SOUP_LENGTH),              31
     *(NAMES_SOUP(2),NSOUP_LENGTH)                                        31
      EQUIVALENCE (COMMEN_SOUP(2),CSOUP_LENGTH),(MAP_SOUP(1,2),           31
     *MSOUP_LENGTH)                                                       31
      CHARACTER CSOUP_LENGTH*40/' '/                                      31
      CHARACTER NSOUP_LENGTH*12/'R.LENGTH'/                               31
      INTEGER MSOUP_LENGTH(2)/1,1/                                        31
      REAL RE_SOUP_LENGTH                                                 31
C -                                                                       31
      REAL SOUP_ROUT                                                      31
      EQUIVALENCE (BANK_SOUP(5),SOUP_ROUT,RE_SOUP_ROUT),(NAMES_SOUP(3),   31
     *NSOUP_ROUT)                                                         31
      EQUIVALENCE (COMMEN_SOUP(3),CSOUP_ROUT),(MAP_SOUP(1,3),             31
     *MSOUP_ROUT)                                                         31
      CHARACTER CSOUP_ROUT*40/' '/                                        31
      CHARACTER NSOUP_ROUT*12/'R.ROUT'/                                   31
      INTEGER MSOUP_ROUT(2)/1,1/                                          31
      REAL RE_SOUP_ROUT                                                   31
C -                                                                       31
      REAL SOUP_DR                                                        31
      EQUIVALENCE (BANK_SOUP(6),SOUP_DR,RE_SOUP_DR),(NAMES_SOUP(4),       31
     *NSOUP_DR)                                                           31
      EQUIVALENCE (COMMEN_SOUP(4),CSOUP_DR),(MAP_SOUP(1,4),MSOUP_DR)      31
      CHARACTER CSOUP_DR*40/' '/                                          31
      CHARACTER NSOUP_DR*12/'R.DR'/                                       31
      INTEGER MSOUP_DR(2)/1,1/                                            31
      REAL RE_SOUP_DR                                                     31
C -                                                                       31
      REAL SOUP_PHI1                                                      31
      EQUIVALENCE (BANK_SOUP(7),SOUP_PHI1,RE_SOUP_PHI1),(NAMES_SOUP(5),   31
     *NSOUP_PHI1)                                                         31
      EQUIVALENCE (COMMEN_SOUP(5),CSOUP_PHI1),(MAP_SOUP(1,5),             31
     *MSOUP_PHI1)                                                         31
      CHARACTER CSOUP_PHI1*40/' '/                                        31
      CHARACTER NSOUP_PHI1*12/'R.PHI1'/                                   31
      INTEGER MSOUP_PHI1(2)/1,1/                                          31
      REAL RE_SOUP_PHI1                                                   31
C -                                                                       31
      REAL SOUP_PHI2                                                      31
      EQUIVALENCE (BANK_SOUP(8),SOUP_PHI2,RE_SOUP_PHI2),(NAMES_SOUP(6),   31
     *NSOUP_PHI2)                                                         31
      EQUIVALENCE (COMMEN_SOUP(6),CSOUP_PHI2),(MAP_SOUP(1,6),             31
     *MSOUP_PHI2)                                                         31
      CHARACTER CSOUP_PHI2*40/' '/                                        31
      CHARACTER NSOUP_PHI2*12/'R.PHI2'/                                   31
      INTEGER MSOUP_PHI2(2)/1,1/                                          31
      REAL RE_SOUP_PHI2                                                   31
C -                                                                       31
      REAL SOUP_DIAMOUT                                                   31
      EQUIVALENCE (BANK_SOUP(9),SOUP_DIAMOUT,RE_SOUP_DIAMOUT),            31
     *(NAMES_SOUP(7),NSOUP_DIAMOUT)                                       31
      EQUIVALENCE (COMMEN_SOUP(7),CSOUP_DIAMOUT),(MAP_SOUP(1,7),          31
     *MSOUP_DIAMOUT)                                                      31
      CHARACTER CSOUP_DIAMOUT*40/' '/                                     31
      CHARACTER NSOUP_DIAMOUT*12/'R.DIAMOUT'/                             31
      INTEGER MSOUP_DIAMOUT(2)/1,1/                                       31
      REAL RE_SOUP_DIAMOUT                                                31
C -                                                                       31
      REAL SOUP_DIAMIN                                                    31
      EQUIVALENCE (BANK_SOUP(10),SOUP_DIAMIN,RE_SOUP_DIAMIN),             31
     *(NAMES_SOUP(8),NSOUP_DIAMIN)                                        31
      EQUIVALENCE (COMMEN_SOUP(8),CSOUP_DIAMIN),(MAP_SOUP(1,8),           31
     *MSOUP_DIAMIN)                                                       31
      CHARACTER CSOUP_DIAMIN*40/' '/                                      31
      CHARACTER NSOUP_DIAMIN*12/'R.DIAMIN'/                               31
      INTEGER MSOUP_DIAMIN(2)/1,1/                                        31
      REAL RE_SOUP_DIAMIN                                                 31
C -                                                                       31
      INTEGER LEN_SOUP,LLEN_SOUP,LINK_SOUP                                31
      PARAMETER (LEN_SOUP=10)                                             31
      PARAMETER (LLEN_SOUP=8)                                             31
      INTEGER FLAG_SOUP/0/,MAP_SOUP(2,LLEN_SOUP),IDNUM_SOUP(10)           31
      CHARACTER NAMES_SOUP(LLEN_SOUP)*12,COMMEN_SOUP(LLEN_SOUP)*40        31
      DATA IDNUM_SOUP/10*0/                                               31
      REAL BANK_SOUP(LEN_SOUP)                                            31
      CHARACTER*60 BPATH_SOUP/' '/                                        31
      SAVE FLAG_SOUP,MAP_SOUP,IDNUM_SOUP,NAMES_SOUP,COMMEN_SOUP,          31
     *BPATH_SOUP                                                          31
      DATA LINK_SOUP/0/,BANK_SOUP/LEN_SOUP*0/                             31
C *                                                                       31
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 32
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         34
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           36
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          38
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    40
      REAL XBUF,PHI,XBUF1,XBUF2                                           42
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     44
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          46
     *SHIELD_PHI(4)                                                       46
      INTEGER I_PHI                                                       48
      RADII(1)= 6.37                                                      50
      RADII(2)= 7.38                                                      51
      RADII(3)=10.38                                                      52
      RADII(4)=11.27                                                      53
      RADII(5)=14.19                                                      54
      RADII(6)=15.13                                                      55
      RAD_OFFSET=0.015                                                    56
      SHIELD_PHI(1)=9.0                                                   57
      SHIELD_PHI(2)=27.0                                                  58
      SHIELD_PHI(3)=45.0                                                  59
      SHIELD_PHI(4)=63.0                                                  60
C *                                                                       61
      CALL AGSBEGM(MODULE,IPRIN)                                          61
      IF (IPRIN.LT.0) RETURN                                              61
C Filling bank SVTG - BASIC SVT DIMENSIONS                                61
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   61
      BPATH_SVTG='SVTG*'                                                  61
      IDNUM_SVTG(1)=0                                                     62
C GEOMETRY VERSION                                                        62
      SVTG_VERSION=2                                                      62
      CSVTG_VERSION='GEOMETRY VERSION'                                    62
C NUMBER OF SVT LAYERS (WAS 7)                                            63
      SVTG_NLAYER=7                                                       63
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         63
C STV INNERMOST RADIUS                                                    64
      SVTG_RSIZEMIN=4.100                                                 64
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               64
C STV OUTERMOST RADIUS                                                    65
      SVTG_RSIZEMAX=46.107                                                65
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               65
C SVT+FTPC LENGTH                                                         66
      SVTG_ZSIZEMAX=270                                                   66
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    66
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          67
      SVTG_ANGOFF=0                                                       67
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       67
C VERSIONING OF THE SHIELD                                                68
      SVTG_SUPPORTVER=1                                                   68
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         68
C VERSIONING OF THE SUPPORT CONE                                          69
      SVTG_CONEVER=1                                                      69
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      69
C WHETHER WE USE THE GEANT MANY OPTION                                    70
      SVTG_IFMANY=0                                                       70
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 70
C THE INDEX OF THE INNERMOST LAYER                                        71
      SVTG_NMIN=1                                                         71
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       71
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    72
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           72
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   72
C *                                                                       72
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    72
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           72
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   72
C *                                                                       73
      CALL AGSBEGM(MODULE,IPRIN)                                          73
      IF (IPRIN.LT.0) RETURN                                              73
C Filling bank SWCA - WAFER CARRIER                                       73
      BANK_TITLE='WAFER CARRIER'                                          73
      BPATH_SWCA='SWCA*'                                                  73
      IDNUM_SWCA(1)=0                                                     74
C GEOMETRY VERSION                                                        74
      SWCA_VERSION=1                                                      74
      CSWCA_VERSION='GEOMETRY VERSION'                                    74
C LADDER LENGTH                                                           75
      SWCA_LENGTH=56.04                                                   75
      CSWCA_LENGTH='LADDER LENGTH'                                        75
C WAFER WIDTH                                                             76
      SWCA_WAFERWID=6.305                                                 76
      CSWCA_WAFERWID='WAFER WIDTH'                                        76
C WAFER LENGTH                                                            77
      SWCA_WAFERLEN=6.305                                                 77
      CSWCA_WAFERLEN='WAFER LENGTH'                                       77
C WAFER THICKNESS                                                         78
      SWCA_WAFERTHK=0.0300                                                78
      CSWCA_WAFERTHK='WAFER THICKNESS'                                    78
C ROHA CELL PLUS GLUE THICKNESS                                           79
      SWCA_ROHATHK=0.0381                                                 79
      CSWCA_ROHATHK='ROHA CELL PLUS GLUE THICKNESS'                       79
C WAFER CARRIER RAILS WIDTH                                               80
      SWCA_WAFCARWD=1.5                                                   80
      CSWCA_WAFCARWD='WAFER CARRIER RAILS WIDTH'                          80
C WAFER CARRIER THICKNESS                                                 81
      SWCA_WAFCARTH=0.0300                                                81
      CSWCA_WAFCARTH='WAFER CARRIER THICKNESS'                            81
C WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIONS)                   82
      SWCA_WAFERGAP=0.05                                                  82
      CSWCA_WAFERGAP='WIDTH OF THE INTER-WAFER GAP (WAS 0 IN PREV VERSIO  82
     *NS)'                                                                82
C DRIFT DIRECTION                                                         83
      SWCA_DRIFT=1                                                        83
      CSWCA_DRIFT='DRIFT DIRECTION'                                       83
C LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)                        84
      SWCA_STRUTLEN=1.0                                                   84
      CSWCA_STRUTLEN='LEN (Z) OF STRUT BETWEEN WAF CAR. RAILS (APPROX)'   84
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    85
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           85
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   85
C *                                                                       85
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWCA',    85
     *BPATH_SWCA,IDNUM_SWCA,1, MAP_SWCA,NAMES_SWCA,COMMEN_SWCA,           85
     *BANK_SWCA,LEN_SWCA,LLEN_SWCA,LINK_SWCA,FLAG_SWCA)                   85
C *                                                                       86
      CALL AGSBEGM(MODULE,IPRIN)                                          86
      IF (IPRIN.LT.0) RETURN                                              86
C Filling bank SSUP - SUPPORT STRUCTURES                                  86
      BANK_TITLE='SUPPORT STRUCTURES'                                     86
      BPATH_SSUP='SSUP*'                                                  86
      IDNUM_SSUP(1)=0                                                     87
C GEOMETRY VERSION                                                        87
      SSUP_VERSION=1                                                      87
      CSSUP_VERSION='GEOMETRY VERSION'                                    87
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            88
      SSUP_CABTHK=0.05                                                    88
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         88
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             89
      SSUP_HOSRMN=0.75                                                    89
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          89
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             90
      SSUP_HOSRMX=0.95                                                    90
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          90
C NUMBER OF WATER HOSES                                                   91
      SSUP_NHOSES=10                                                      91
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                91
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             92
      SSUP_WRPMYTHK=0.10                                                  92
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        92
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   93
      SSUP_WRPALTHK=0.01                                                  93
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              93
C SUPPORT CONE THICKNESS                                                  94
      SSUP_GRPHTHK=0.16                                                   94
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              94
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              95
      SSUP_CONE1ZMN=52.23                                                 95
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         95
C LENGTH OF SUPPORT RODS                                                  96
      SSUP_RODLEN=110.8                                                   96
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               96
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               97
      SSUP_RODDIST=17.5                                                   97
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           97
C ID OF CARBON SUPPORT RODS (APPROX)                                      98
      SSUP_RODID=2.5                                                      98
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    98
C OD OF CARBON SUPPORT RODS (APPROX)                                      99
      SSUP_RODOD=3.05                                                     99
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    99
C MINIMUM ID OF CONE 1                                                    100
      SSUP_CON1IDMN=15.67                                                 100
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               100
C MINIMUM ID OF CONE 3 (TBD)                                              101
      SSUP_CON3IDMN=21.67                                                 101
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         101
C MINIMUM ID OF CONE 4 (TBD)                                              102
      SSUP_CON4IDMN=37.4                                                  102
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         102
C MAXIMUM ID OF CONE 4 (TBD)                                              103
      SSUP_CON4IDMX=37.4                                                  103
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         103
C MAXIMUM Z OF CONE 3 (TBD)                                               104
      SSUP_CONE3ZMX=150.0                                                 104
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          104
C MAXIMUM Z OF CONE 4 (TBD)                                               105
      SSUP_CONE4ZMX=229.36                                                105
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          105
C THICKNESS OF AL BRACKETS                                                106
      SSUP_BRATHK=.2                                                      106
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             106
C (Z) THICKNESS OF END RING JOINING BRACKETS                              107
      SSUP_ERJTHK=.1                                                      107
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           107
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          108
      SSUP_ERJWID=2.07                                                    108
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       108
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            109
      SSUP_ERJLEN=5.19                                                    109
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         109
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    110
      SSUP_ERJZDIS=2.0                                                    110
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                110
C ERJ SCREW 1 X POSITION (RADIAL)                                         111
      SSUP_ERJ1X=0.31                                                     111
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       111
C ERJ SCREW 2 X POSITION                                                  112
      SSUP_ERJ2X=1.15                                                     112
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                112
C ERJ SCREW 2 Y POSITION                                                  113
      SSUP_ERJ2Y=0.72                                                     113
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                113
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   114
      SSUP_ERJRAD=10.80                                                   114
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                114
C ERJ SCREW DIAMETER                                                      115
      SSUP_ERJDIA=0.17                                                    115
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   115
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    116
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           116
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   116
C *                                                                       116
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    116
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           116
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   116
C *                                                                       117
      CALL AGSBEGM(MODULE,IPRIN)                                          117
      IF (IPRIN.LT.0) RETURN                                              117
C Filling bank SSUP - SUPPORT STRUCTURES                                  117
      BANK_TITLE='SUPPORT STRUCTURES'                                     117
      BPATH_SSUP='SSUP*'                                                  117
      IDNUM_SSUP(1)=0                                                     118
C GEOMETRY VERSION                                                        118
      SSUP_VERSION=2                                                      118
      CSSUP_VERSION='GEOMETRY VERSION'                                    118
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            119
      SSUP_CABTHK=0.21                                                    119
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         119
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             120
      SSUP_HOSRMN=0.75                                                    120
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          120
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             121
      SSUP_HOSRMX=0.95                                                    121
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          121
C NUMBER OF WATER HOSES                                                   122
      SSUP_NHOSES=10                                                      122
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                122
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             123
      SSUP_WRPMYTHK=0.10                                                  123
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        123
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   124
      SSUP_WRPALTHK=0.01                                                  124
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              124
C SUPPORT CONE THICKNESS                                                  125
      SSUP_GRPHTHK=0.16                                                   125
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              125
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              126
      SSUP_CONE1ZMN=52.23                                                 126
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         126
C LENGTH OF SUPPORT RODS                                                  127
      SSUP_RODLEN=110.8                                                   127
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               127
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               128
      SSUP_RODDIST=17.5                                                   128
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           128
C ID OF CARBON SUPPORT RODS (APPROX)                                      129
      SSUP_RODID=2.5                                                      129
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    129
C OD OF CARBON SUPPORT RODS (APPROX)                                      130
      SSUP_RODOD=3.05                                                     130
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    130
C MINIMUM ID OF CONE 1                                                    131
      SSUP_CON1IDMN=15.67                                                 131
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               131
C MINIMUM ID OF CONE 3 (TBD)                                              132
      SSUP_CON3IDMN=21.67                                                 132
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         132
C MINIMUM ID OF CONE 4 (TBD)                                              133
      SSUP_CON4IDMN=37.4                                                  133
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         133
C MAXIMUM ID OF CONE 4 (TBD)                                              134
      SSUP_CON4IDMX=37.4                                                  134
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         134
C MAXIMUM Z OF CONE 3 (TBD)                                               135
      SSUP_CONE3ZMX=150.0                                                 135
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          135
C MAXIMUM Z OF CONE 4 (TBD)                                               136
      SSUP_CONE4ZMX=229.36                                                136
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          136
C THICKNESS OF AL BRACKETS                                                137
      SSUP_BRATHK=.2                                                      137
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             137
C (Z) THICKNESS OF END RING JOINING BRACKETS                              138
      SSUP_ERJTHK=.1                                                      138
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           138
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          139
      SSUP_ERJWID=2.07                                                    139
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       139
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            140
      SSUP_ERJLEN=5.19                                                    140
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         140
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    141
      SSUP_ERJZDIS=2.0                                                    141
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                141
C ERJ SCREW 1 X POSITION (RADIAL)                                         142
      SSUP_ERJ1X=0.31                                                     142
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       142
C ERJ SCREW 2 X POSITION                                                  143
      SSUP_ERJ2X=1.15                                                     143
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                143
C ERJ SCREW 2 Y POSITION                                                  144
      SSUP_ERJ2Y=0.72                                                     144
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                144
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   145
      SSUP_ERJRAD=10.80                                                   145
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                145
C ERJ SCREW DIAMETER                                                      146
      SSUP_ERJDIA=0.17                                                    146
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   146
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    147
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           147
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   147
C *                                                                       147
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    147
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           147
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   147
C *                                                                       148
      CALL AGSBEGM(MODULE,IPRIN)                                          148
      IF (IPRIN.LT.0) RETURN                                              148
C Filling bank SSUB - BEAMPIPE SUPPORT                                    148
      BANK_TITLE='BEAMPIPE SUPPORT'                                       148
      BPATH_SSUB='SSUB*'                                                  148
      IDNUM_SSUB(1)=0                                                     149
C GEOMETRY VERSION                                                        149
      SSUB_VERSION=1                                                      149
      CSSUB_VERSION='GEOMETRY VERSION'                                    149
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  150
      SSUB_KMOUNTID=31.34                                                 150
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             150
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  151
      SSUB_KMOUNTOD=38.96                                                 151
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             151
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    152
      SSUB_KMNTTHK=0.64                                                   152
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                152
C OD OF CUTOUT IN KINEMATIC MOUNT                                         153
      SSUB_KMCUTOD=18.31                                                  153
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     153
C ID OF CUTOUT IN KINEMATIC MOUNT                                         154
      SSUB_KMCUTID=14                                                     154
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     154
C OPENING ANGLE OF CUTOUT                                                 155
      SSUB_KMCUTOA=38                                                     155
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             155
C OFFSET OF CUTOUT CENTER FROM AXIS                                       156
      SSUB_KMCUTOFF=26.58                                                 156
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  156
C ID OF BEAMPIPE SUPPORT RING                                             157
      SSUB_SRINGID=8.47                                                   157
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         157
C OD OF BEAMPIPE SUPPORT RING                                             158
      SSUB_SRINGOD=12.78                                                  158
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         158
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     159
      SSUB_SRINGTHK=0.64                                                  159
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                159
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     160
      SSUB_SRCUTPHI=38                                                    160
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                160
C SUPPORT RING CUTOUT WIDTH                                               161
      SSUB_SRCUTWID=3.63                                                  161
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          161
C SUPPORT RING CUTOUT DEPTH                                               162
      SSUB_SRCUTOUT=5.08                                                  162
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          162
C SUPPORT RING CUTOUT START                                               163
      SSUB_SRCUTIN=3.5                                                    163
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           163
C SUPPORT ROLLER ID                                                       164
      SSUB_SROLLID=0.2                                                    164
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   164
C SUPPORT ROLLER OD                                                       165
      SSUB_SROLLOD=0.62                                                   165
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   165
C SUPPORT ROLLER LENGTH                                                   166
      SSUB_SROLLLEN=2.54                                                  166
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              166
C SUPPORT ROLLER AXIS LENGTH                                              167
      SSUB_SWIRELEN=5.08                                                  167
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         167
C MOUNTING BLOCK HEIGHT                                                   168
      SSUB_MBLKHGH=0.97                                                   168
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               168
C OUTER MOUNTING BLOCK WIDTH                                              169
      SSUB_MBLKOWID=2.54                                                  169
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         169
C OUTER MOUNTING BLOCK LENGTH                                             170
      SSUB_MBLKOLEN=1.27                                                  170
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        170
C INNER MOUNTING BLOCK WIDTH                                              171
      SSUB_MBLKIWID=3.175                                                 171
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         171
C INNER MOUNTING BLOCK LENGTH                                             172
      SSUB_MBLKILEN=1.27                                                  172
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        172
C OUTER MOUNTING BLOCK AT RADIUS                                          173
      SSUB_MBLKORAD=17.4                                                  173
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     173
C INNER MOUNTING BLOCK AT RADIUS                                          174
      SSUB_MBLKIRAD=5.42                                                  174
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     174
C MOUNTING SUPPORT ROD DIAMETER                                           175
      SSUB_MRODDIA=0.32                                                   175
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       175
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    176
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           176
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   176
C *                                                                       176
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    176
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           176
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   176
C *                                                                       177
      CALL AGSBEGM(MODULE,IPRIN)                                          177
      IF (IPRIN.LT.0) RETURN                                              177
C Filling bank SERG - END RINGS                                           177
      BANK_TITLE='END RINGS'                                              177
      BPATH_SERG='SERG*'                                                  177
      IDNUM_SERG(1)=0                                                     178
C GEOMETRY VERSION                                                        178
      SERG_VERSION=1                                                      178
      CSERG_VERSION='GEOMETRY VERSION'                                    178
C INNER END RING TUBE MAXIMUM RADIUS                                      179
      SERG_IRNGTRMX=9.703                                                 179
      CSERG_IRNGTRMX='INNER END RING TUBE MAXIMUM RADIUS'                 179
C INNER END RING POLYGON MINIMUM RADIUS                                   180
      SERG_IRNGPRMN=7.671                                                 180
      CSERG_IRNGPRMN='INNER END RING POLYGON MINIMUM RADIUS'              180
C OUTER END RING MINIMUM RADIUS                                           181
      SERG_ORNGRMIN=11.900                                                181
      CSERG_ORNGRMIN='OUTER END RING MINIMUM RADIUS'                      181
C OUTER END RING MAXIMUM RADIUS                                           182
      SERG_ORNGRMAX=13.805                                                182
      CSERG_ORNGRMAX='OUTER END RING MAXIMUM RADIUS'                      182
C END RING THICKNESS                                                      183
      SERG_ENDRNGTH=0.2                                                   183
      CSERG_ENDRNGTH='END RING THICKNESS'                                 183
C MINIMUM Z FOR END RINGS                                                 184
      SERG_ENDRNGZM=23.01                                                 184
      CSERG_ENDRNGZM='MINIMUM Z FOR END RINGS'                            184
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    185
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           185
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   185
C *                                                                       185
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SERG',    185
     *BPATH_SERG,IDNUM_SERG,1, MAP_SERG,NAMES_SERG,COMMEN_SERG,           185
     *BANK_SERG,LEN_SERG,LLEN_SERG,LINK_SERG,FLAG_SERG)                   185
C *                                                                       186
      CALL AGSBEGM(MODULE,IPRIN)                                          186
      IF (IPRIN.LT.0) RETURN                                              186
C Filling bank SWAM - WATER MANIFOLD                                      186
      BANK_TITLE='WATER MANIFOLD'                                         186
      BPATH_SWAM='SWAM*'                                                  186
      IDNUM_SWAM(1)=0                                                     187
C GEOMETRY VERSION                                                        187
      SWAM_VERSION=1                                                      187
      CSWAM_VERSION='GEOMETRY VERSION'                                    187
C MINIMUM Z FOR POSITIONING WATER MANIFOLD                                188
      SWAM_ZMIN=33.9                                                      188
      CSWAM_ZMIN='MINIMUM Z FOR POSITIONING WATER MANIFOLD'               188
C WATER MANIFOLD FULL LENGTH                                              189
      SWAM_LEN=16.0                                                       189
      CSWAM_LEN='WATER MANIFOLD FULL LENGTH'                              189
C WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)                         190
      SWAM_RMIN=15.24                                                     190
      CSWAM_RMIN='WATER MANIFOLD RMIN (NOT INCLUDING TRANS. BRDS)'        190
C WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)                         191
      SWAM_RMAX=16.83                                                     191
      CSWAM_RMAX='WATER MANIFOLD RMAX (NOT INCLUDING TRANS. BRDS)'        191
C TRANSITION BOARD THICKNESS                                              192
      SWAM_TBRDTHK=0.1                                                    192
      CSWAM_TBRDTHK='TRANSITION BOARD THICKNESS'                          192
C WATER MANIFOLD WALL THICKNESS                                           193
      SWAM_WALLTHK=0.1                                                    193
      CSWAM_WALLTHK='WATER MANIFOLD WALL THICKNESS'                       193
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    194
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           194
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   194
C *                                                                       194
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SWAM',    194
     *BPATH_SWAM,IDNUM_SWAM,1, MAP_SWAM,NAMES_SWAM,COMMEN_SWAM,           194
     *BANK_SWAM,LEN_SWAM,LLEN_SWAM,LINK_SWAM,FLAG_SWAM)                   194
C *                                                                       195
      CALL AGSBEGM(MODULE,IPRIN)                                          195
      IF (IPRIN.LT.0) RETURN                                              195
C Filling bank SELC - ELECTRONICS CARRIER DATA                            195
      BANK_TITLE='ELECTRONICS CARRIER DATA'                               195
      BPATH_SELC='SELC*'                                                  195
      IDNUM_SELC(1)=0                                                     196
C GEOMETRY VERSION                                                        196
      SELC_VERSION=1                                                      196
      CSELC_VERSION='GEOMETRY VERSION'                                    196
C THICKNESS OF BERILLIUM WATER CHANNEL WALLS                              197
      SELC_BETHK=0.0500                                                   197
      CSELC_BETHK='THICKNESS OF BERILLIUM WATER CHANNEL WALLS'            197
C THICKNESS OF WATER CHANNEL                                              198
      SELC_WATTHK=0.0750                                                  198
      CSELC_WATTHK='THICKNESS OF WATER CHANNEL'                           198
C THICKNESS OF BERRILLIA (BEO) SUBSTRA                                    199
      SELC_BEOTHK=0.0500                                                  199
      CSELC_BEOTHK='THICKNESS OF BERRILLIA (BEO) SUBSTRA'                 199
C TO GIVE .11% OF A RADIATION LENGTH OF SI                                200
      SELC_DYETHK=0.0340                                                  200
      CSELC_DYETHK='TO GIVE .11% OF A RADIATION LENGTH OF SI'             200
C WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)                                201
      SELC_DYEWID=0.3                                                     201
      CSELC_DYEWID='WIDTH OF IC CHIPS (2 COVERS 0.3 OF AREA)'             201
C SPACE IC CHIPS                                                          202
      SELC_DYESPC=0.1                                                     202
      CSELC_DYESPC='SPACE IC CHIPS'                                       202
C ELECTRONICS CARRIER WIDTH                                               203
      SELC_ELCAWID=2.0                                                    203
      CSELC_ELCAWID='ELECTRONICS CARRIER WIDTH'                           203
C THICKNESS OF AG-PD CONDUCTOR                                            204
      SELC_AGPDTHK=0.0030                                                 204
      CSELC_AGPDTHK='THICKNESS OF AG-PD CONDUCTOR'                        204
C THICKNESS OF GLASS INSULATOR                                            205
      SELC_GLASSTHK=0.0150                                                205
      CSELC_GLASSTHK='THICKNESS OF GLASS INSULATOR'                       205
C CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)                        206
      SELC_CABTHK=0.0033                                                  206
      CSELC_CABTHK='CU FOR CABLES OF .23% RAD LEN (WEIGHTED AVERAGE)'     206
C CABLE WIDTH                                                             207
      SELC_CABWID=0.6                                                     207
      CSELC_CABWID='CABLE WIDTH'                                          207
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    208
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           208
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   208
C *                                                                       208
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SELC',    208
     *BPATH_SELC,IDNUM_SELC,1, MAP_SELC,NAMES_SELC,COMMEN_SELC,           208
     *BANK_SELC,LEN_SELC,LLEN_SELC,LINK_SELC,FLAG_SELC)                   208
C *                                                                       209
      CALL AGSBEGM(MODULE,IPRIN)                                          209
      IF (IPRIN.LT.0) RETURN                                              209
C Filling bank SVTL - SINGLE LAYER DATA                                   209
      BANK_TITLE='SINGLE LAYER DATA'                                      209
      BPATH_SVTL='SVTL*'                                                  209
      IDNUM_SVTL(1)=0                                                     210
C LAYER NUMBER                                                            210
      SVTL_LAYER=1                                                        210
      CSVTL_LAYER='LAYER NUMBER'                                          210
C LAYER RADIUS (CENTER OF WAFER POSITION)                                 211
      SVTL_RADIUS=RADII(1)+RAD_OFFSET                                     211
      CSVTL_RADIUS='LAYER RADIUS (CENTER OF WAFER POSITION)'              211
C NUMBER OF LADDER                                                        212
      SVTL_NLADDER=4                                                      212
      CSVTL_NLADDER='NUMBER OF LADDER'                                    212
C NUMBER OF WAFERS                                                        213
      SVTL_NWAFER=4                                                       213
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     213
C THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER                        214
      SVTL_BAREEDGE=1.0                                                   214
      CSVTL_BAREEDGE='THE STRIP OF BARE BE WHICH MAKES THE PCB SHORTER'   214
C PCB LENGTH                                                              215
      SVTL_PCBLEN=14.9                                                    215
      CSVTL_PCBLEN='PCB LENGTH'                                           215
C PCB WIDTH                                                               216
      SVTL_PCBWIDTH=6.3                                                   216
      CSVTL_PCBWIDTH='PCB WIDTH'                                          216
C PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FOR COPPER            217
      SVTL_PCBTHK=0.1                                                     217
      CSVTL_PCBTHK='PCB THICKNESS -- SHOULD BE 0.09, BUT WE ADD EXTRA FO  217
     *R COPPER'                                                           217
C GAP BETWEEN THE PCB AND THE SI WAFER                                    218
      SVTL_PCBGAP=0.2                                                     218
      CSVTL_PCBGAP='GAP BETWEEN THE PCB AND THE SI WAFER'                 218
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    219
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           219
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   219
C *                                                                       219
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    219
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           219
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   219
C *                                                                       220
      CALL AGSBEGM(MODULE,IPRIN)                                          220
      IF (IPRIN.LT.0) RETURN                                              220
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             220
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                220
      BPATH_SVTL='SVTL*'                                                  220
      IDNUM_SVTL(1)=0                                                     221
C LAYER NUMBER                                                            221
      SVTL_LAYER=2                                                        221
      CSVTL_LAYER='LAYER NUMBER'                                          221
C LAYER RADIUS                                                            222
      SVTL_RADIUS=RADII(2)+RAD_OFFSET                                     222
      CSVTL_RADIUS='LAYER RADIUS'                                         222
C PCB LENGTH                                                              223
      SVTL_PCBLEN=14.9                                                    223
      CSVTL_PCBLEN='PCB LENGTH'                                           223
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    224
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           224
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   224
C *                                                                       224
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTL',    224
     *BPATH_SVTL,IDNUM_SVTL,1, MAP_SVTL,NAMES_SVTL,COMMEN_SVTL,           224
     *BANK_SVTL,LEN_SVTL,LLEN_SVTL,LINK_SVTL,FLAG_SVTL)                   224
C *                                                                       225
      CALL AGSBEGM(MODULE,IPRIN)                                          225
      IF (IPRIN.LT.0) RETURN                                              225
C Filling bank SVTL - SINGLE LAYER PARAMETERS                             225
      BANK_TITLE='SINGLE LAYER PARAMETERS'                                225
      BPATH_SVTL='SVTL*'                                                  225
      IDNUM_SVTL(1)=0                                                     226
C LAYER NUMBER                                                            226
      SVTL_LAYER=3                                                        226
      CSVTL_LAYER='LAYER NUMBER'                                          226
C LAYER RADIUS                                                            227
      SVTL_RADIUS=RADII(3)+RAD_OFFSET                                     227
      CSVTL_RADIUS='LAYER RADIUS'                                         227
C NUMBER OF LADDER                                                        228
      SVTL_NLADDER=6                                                      228
      CSVTL_NLADDER='NUMBER OF LADDER'                                    228
C NUMBER OF WAFERS                                                        229
      SVTL_NWAFER=6                                                       229
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     229
C PCB LENGTH                                                              230
      SVTL_PCBLEN=7.5                                                     230
      CSVTL_PCBLEN='PCB LENGTH'                                           230
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
      SVTL_LAYER=4                                                        233
      CSVTL_LAYER='LAYER NUMBER'                                          233
C LAYER RADIUS                                                            234
      SVTL_RADIUS=RADII(4)+RAD_OFFSET                                     234
      CSVTL_RADIUS='LAYER RADIUS'                                         234
C PCB LENGTH                                                              235
      SVTL_PCBLEN=7.5                                                     235
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
      SVTL_LAYER=5                                                        238
      CSVTL_LAYER='LAYER NUMBER'                                          238
C LAYER RADIUS                                                            239
      SVTL_RADIUS=RADII(5)+RAD_OFFSET                                     239
      CSVTL_RADIUS='LAYER RADIUS'                                         239
C NUMBER OF LADDER                                                        240
      SVTL_NLADDER=8                                                      240
      CSVTL_NLADDER='NUMBER OF LADDER'                                    240
C NUMBER OF WAFERS                                                        241
      SVTL_NWAFER=7                                                       241
      CSVTL_NWAFER='NUMBER OF WAFERS'                                     241
C PCB LENGTH                                                              242
      SVTL_PCBLEN=4.4                                                     242
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
      SVTL_LAYER=6                                                        245
      CSVTL_LAYER='LAYER NUMBER'                                          245
C LAYER RADIUS                                                            246
      SVTL_RADIUS=RADII(6)+RAD_OFFSET                                     246
      CSVTL_RADIUS='LAYER RADIUS'                                         246
C PCB LENGTH                                                              247
      SVTL_PCBLEN=4.4                                                     247
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
      SSLD_SOUTRINN=29.5                                                  257
      CSSLD_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                257
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     258
      SSLD_SOUTROUT=29.52                                                 258
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
      SSLD_SINRINN=5.9                                                    267
      CSSLD_SINRINN='INNER SHIELD CYLINDER, INNER RADIUS'                 267
C INNER SHIELD CYLINDER, OUTER RADIUS                                     268
      SSLD_SINROUT=5.908                                                  268
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
      CALL AGDGETP('SVTTGEO5','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,  340
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      340
     *LINK_SVTG,FLAG_SVTG)                                                340
C use default SWCA bank                                                   341
      BPATH_SWCA='SWCA*'                                                  341
      IDNUM_SWCA(1)=0                                                     341
      CALL AGDGETP('SVTTGEO5','*',-999,'SWCA',' ',BANK_SWCA, BPATH_SWCA,  341
     *IDNUM_SWCA,1,MAP_SWCA,NAMES_SWCA,BANK_SWCA,LEN_SWCA,LLEN_SWCA,      341
     *LINK_SWCA,FLAG_SWCA)                                                341
C use default SELC bank                                                   342
      BPATH_SELC='SELC*'                                                  342
      IDNUM_SELC(1)=0                                                     342
      CALL AGDGETP('SVTTGEO5','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  342
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      342
     *LINK_SELC,FLAG_SELC)                                                342
C use SSUP bank                                                           343
      BPATH_SSUP='SSUP*'                                                  343
      IDNUM_SSUP(1)=0                                                     343
      SSUP_VERSION=SVTG_CONEVER                                           343
      CALL AGDGETP('SVTTGEO5','*',-999,'SSUP',NSSUP_VERSION,              343
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        343
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   343
C use default SSUB bank                                                   344
      BPATH_SSUB='SSUB*'                                                  344
      IDNUM_SSUB(1)=0                                                     344
      CALL AGDGETP('SVTTGEO5','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,  344
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      344
     *LINK_SSUB,FLAG_SSUB)                                                344
C use default SERG bank                                                   345
      BPATH_SERG='SERG*'                                                  345
      IDNUM_SERG(1)=0                                                     345
      CALL AGDGETP('SVTTGEO5','*',-999,'SERG',' ',BANK_SERG, BPATH_SERG,  345
     *IDNUM_SERG,1,MAP_SERG,NAMES_SERG,BANK_SERG,LEN_SERG,LLEN_SERG,      345
     *LINK_SERG,FLAG_SERG)                                                345
C use default SWAM bank                                                   346
      BPATH_SWAM='SWAM*'                                                  346
      IDNUM_SWAM(1)=0                                                     346
      CALL AGDGETP('SVTTGEO5','*',-999,'SWAM',' ',BANK_SWAM, BPATH_SWAM,  346
     *IDNUM_SWAM,1,MAP_SWAM,NAMES_SWAM,BANK_SWAM,LEN_SWAM,LLEN_SWAM,      346
     *LINK_SWAM,FLAG_SWAM)                                                346
C use default SELC bank                                                   347
      BPATH_SELC='SELC*'                                                  347
      IDNUM_SELC(1)=0                                                     347
      CALL AGDGETP('SVTTGEO5','*',-999,'SELC',' ',BANK_SELC, BPATH_SELC,  347
     *IDNUM_SELC,1,MAP_SELC,NAMES_SELC,BANK_SELC,LEN_SELC,LLEN_SELC,      347
     *LINK_SELC,FLAG_SELC)                                                347
C use SSLD bank                                                           348
      BPATH_SSLD='SSLD*'                                                  348
      IDNUM_SSLD(1)=0                                                     348
      SSLD_VERSION=SVTG_SUPPORTVER                                        348
      CALL AGDGETP('SVTTGEO5','*',-999,'SSLD',NSSLD_VERSION,              348
     *RE_SSLD_VERSION,BPATH_SSLD,IDNUM_SSLD,1,MAP_SSLD,NAMES_SSLD,        348
     *BANK_SSLD,LEN_SSLD,LLEN_SSLD,LINK_SSLD,FLAG_SSLD)                   348
C use default SCBP bank                                                   349
      BPATH_SCBP='SCBP*'                                                  349
      IDNUM_SCBP(1)=0                                                     349
      CALL AGDGETP('SVTTGEO5','*',-999,'SCBP',' ',BANK_SCBP, BPATH_SCBP,  349
     *IDNUM_SCBP,1,MAP_SCBP,NAMES_SCBP,BANK_SCBP,LEN_SCBP,LLEN_SCBP,      349
     *LINK_SCBP,FLAG_SCBP)                                                349
C use default SWCX bank                                                   350
      BPATH_SWCX='SWCX*'                                                  350
      IDNUM_SWCX(1)=0                                                     350
      CALL AGDGETP('SVTTGEO5','*',-999,'SWCX',' ',BANK_SWCX, BPATH_SWCX,  350
     *IDNUM_SWCX,1,MAP_SWCX,NAMES_SWCX,BANK_SWCX,LEN_SWCX,LLEN_SWCX,      350
     *LINK_SWCX,FLAG_SWCX)                                                350
C use default SOUP bank                                                   351
      BPATH_SOUP='SOUP*'                                                  351
      IDNUM_SOUP(1)=0                                                     351
      CALL AGDGETP('SVTTGEO5','*',-999,'SOUP',' ',BANK_SOUP, BPATH_SOUP,  351
     *IDNUM_SOUP,1,MAP_SOUP,NAMES_SOUP,BANK_SOUP,LEN_SOUP,LLEN_SOUP,      351
     *LINK_SOUP,FLAG_SOUP)                                                351
C Check SVTG_SUPPORTVER==2                                                352
      IF (SVTG_SUPPORTVER.EQ.2) THEN                                      352
      SVTG_RSIZEMIN = 6.0                                                 353
      ENDIF                                                               354
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             355
      CALL AGSRESET                                                       355
      AG_A = 28.08                                                        355
      AG_Z = 14                                                           355
      AG_W = 0.6*1*28./60.                                                355
      AG_TITLE = 'SI'                                                     355
      AG_EXNAME = 'COMPONENT'                                             355
      AG_PARLIST = 'A_Z_W'                                                355
      CALL AXCOMPONENT                                                    355
C *                                                                       355
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  355
C =                                                                       355
      CALL AGSRESET                                                       356
      AG_A = 16                                                           356
      AG_Z = 8                                                            356
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               356
      AG_TITLE = 'O'                                                      356
      AG_EXNAME = 'COMPONENT'                                             356
      AG_PARLIST = 'A_Z_W'                                                356
      CALL AXCOMPONENT                                                    356
C *                                                                       356
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 356
      CALL AGSRESET                                                       357
      AG_A = 12                                                           357
      AG_Z = 6                                                            357
      AG_W = 0.4*8*12./174.                                               357
      AG_TITLE = 'C'                                                      357
      AG_EXNAME = 'COMPONENT'                                             357
      AG_PARLIST = 'A_Z_W'                                                357
      CALL AXCOMPONENT                                                    357
C *                                                                       357
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  357
      CALL AGSRESET                                                       358
      AG_A = 1                                                            358
      AG_Z = 1                                                            358
      AG_W = 0.4*14*1./174.                                               358
      AG_TITLE = 'H'                                                      358
      AG_EXNAME = 'COMPONENT'                                             358
      AG_PARLIST = 'A_Z_W'                                                358
      CALL AXCOMPONENT                                                    358
C *                                                                       358
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  358
      CALL AGSRESET                                                       359
      AG_DENS = 1.7                                                       359
      AG_TITLE = 'G10'                                                    359
      AG_EXNAME = 'MIXTURE'                                               359
      AG_PARLIST = 'DENS'                                                 359
      CALL AXMIXTURE                                                      359
C *                                                                       359
C Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                       359
      CALL AGSRESET                                                       360
      AG_A = 12                                                           360
      AG_Z = 6                                                            360
      AG_W = 10000*5                                                      360
      AG_TITLE = 'C5'                                                     360
      AG_EXNAME = 'COMPONENT'                                             360
      AG_PARLIST = 'A_Z_W'                                                360
      CALL AXCOMPONENT                                                    360
C *                                                                       360
C Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                        360
      CALL AGSRESET                                                       361
      AG_A = 1                                                            361
      AG_Z = 1                                                            361
      AG_W = 10000*4                                                      361
      AG_TITLE = 'H4'                                                     361
      AG_EXNAME = 'COMPONENT'                                             361
      AG_PARLIST = 'A_Z_W'                                                361
      CALL AXCOMPONENT                                                    361
C *                                                                       361
C Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                       361
      CALL AGSRESET                                                       362
      AG_A = 16                                                           362
      AG_Z = 8                                                            362
      AG_W = 10000*2                                                      362
      AG_TITLE = 'O2'                                                     362
      AG_EXNAME = 'COMPONENT'                                             362
      AG_PARLIST = 'A_Z_W'                                                362
      CALL AXCOMPONENT                                                    362
C *                                                                       362
C Geant COMPONENT : AL ===> A=27 Z=13 W=10000*0.2302 <===                 362
      CALL AGSRESET                                                       363
      AG_A = 27                                                           363
      AG_Z = 13                                                           363
      AG_W = 10000*0.2302                                                 363
      AG_TITLE = 'AL'                                                     363
      AG_EXNAME = 'COMPONENT'                                             363
      AG_PARLIST = 'A_Z_W'                                                363
      CALL AXCOMPONENT                                                    363
C *                                                                       363
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              363
      CALL AGSRESET                                                       364
      AG_DENS = 1.432                                                     364
      AG_TITLE = 'ALKAP'                                                  364
      AG_EXNAME = 'MIXTURE'                                               364
      AG_PARLIST = 'DENS'                                                 364
      CALL AXMIXTURE                                                      364
C *                                                                       364
C Check SWAM_LEN>0                                                        364
      IF (SWAM_LEN.GT.0) THEN                                             365
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              366
      CALL AGSRESET                                                       366
      AG_A = 1                                                            366
      AG_Z = 1                                                            366
      AG_W = 2                                                            366
      AG_TITLE = 'H2'                                                     366
      AG_EXNAME = 'COMPONENT'                                             366
      AG_PARLIST = 'A_Z_W'                                                366
      CALL AXCOMPONENT                                                    366
C *                                                                       366
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              366
      CALL AGSRESET                                                       367
      AG_A = 16                                                           367
      AG_Z = 8                                                            367
      AG_W = 1                                                            367
      AG_TITLE = 'O'                                                      367
      AG_EXNAME = 'COMPONENT'                                             367
      AG_PARLIST = 'A_Z_W'                                                367
      CALL AXCOMPONENT                                                    367
C *                                                                       367
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                367
      CALL AGSRESET                                                       368
      AG_DENS = 1.0                                                       368
      AG_TITLE = 'WATER'                                                  368
      AG_EXNAME = 'MIXTURE'                                               368
      AG_PARLIST = 'DENS'                                                 368
      CALL AXMIXTURE                                                      368
C *                                                                       368
      ELSE                                                                368
C Geant MIXTURE : WATER ===> DENS=0.0009 <===                             370
      CALL AGSRESET                                                       370
      AG_DENS = 0.0009                                                    370
      AG_TITLE = 'WATER'                                                  370
      AG_EXNAME = 'MIXTURE'                                               370
      AG_PARLIST = 'DENS'                                                 370
      CALL AXMIXTURE                                                      370
C *                                                                       370
      ENDIF                                                               370
C Check SVTG_IFMANY>0                                                     372
      IF (SVTG_IFMANY.GT.0) THEN                                          372
      CALL AGSBEGM(MODULE,IPRIN)                                          373
C Check IPRIN<0                                                           373
      IF (IPRIN.LT.0) RETURN                                              373
C Create SVTT :                                                           373
C Call To Local Procedure SVTT                                            373
      ASSIGN 5030 TO BLOCK_SVTT                                           373
      GO TO  5020                                                         373
5030  CONTINUE                                                            373
C *                                                                       373
C Geant POSITION : SVTT ===> MOTHER='CAVE' KONLY='MANY' <===              374
      CALL AGSRESET                                                       374
      AG_MOTHER = 'CAVE'                                                  374
      AG_KONLY = 'MANY'                                                   374
      AG_TITLE = 'SVTT'                                                   374
      AG_EXNAME = 'POSITION'                                              374
      AG_PARLIST = 'MOTHER_KONLY'                                         374
      CALL AXPOSITION                                                     374
C *                                                                       374
      ELSE                                                                374
      CALL AGSBEGM(MODULE,IPRIN)                                          376
C Check IPRIN<0                                                           376
      IF (IPRIN.LT.0) RETURN                                              376
C Create SVTT :                                                           376
C Call To Local Procedure SVTT                                            376
      ASSIGN 5040 TO BLOCK_SVTT                                           376
      GO TO  5020                                                         376
5040  CONTINUE                                                            376
C *                                                                       376
C Geant POSITION : SVTT ===> MOTHER='CAVE' <===                           377
      CALL AGSRESET                                                       377
      AG_MOTHER = 'CAVE'                                                  377
      AG_TITLE = 'SVTT'                                                   377
      AG_EXNAME = 'POSITION'                                              377
      AG_PARLIST = 'MOTHER'                                               377
      CALL AXPOSITION                                                     377
C *                                                                       377
      ENDIF                                                               377
C ----------------------------------------------------------------------  379
C ------------ SVTT                                                       379
      CALL AGSENDM                                                        380
      RETURN                                                              380
C Begin Local Procedure SVTT                                              380
C SVTT IS THE MOTHER OF ALL SVT VOLUMES                                   380
5020  CONTINUE                                                            380
         AG_TITLE='SVTT'                                                  380
         AG_MODULE=MODULE                                                 380
         AG_MARK='V00'                                                    380
         CALL AGSPUSH                                                     380
         AG_MODULE=MODULE                                                 380
C    *                                                                    380
         RSIZEMAX=SSUP_CON4IDMX                                           381
         RSIZEMAX=RSIZEMAX + SSUP_GRPHTHK + SSUP_CABTHK +                 382
     *   2.0*SSUP_HOSRMX                                                  382
         RSIZEMAX=RSIZEMAX + SSUP_WRPMYTHK + SSUP_WRPALTHK                383
         CLEARANCE=SVTG_RSIZEMAX - RSIZEMAX                               384
C    Geant MATERIAL : AIR ===> inherited arguments used <===              385
         CALL AGSRESET                                                    385
         AG_TITLE = 'AIR'                                                 385
         AG_EXNAME = 'MATERIAL'                                           385
         AG_PARLIST = 'NONE'                                              385
         CALL AXMATERIAL                                                  385
C    *                                                                    385
C    Geant ATTRIBUTE : SVTT ===> SEEN=0 COLO=1 <===                       385
         CALL AGSRESET                                                    386
         AG_SEEN = 0                                                      386
         AG_COLO = 1                                                      386
         AG_TITLE = 'SVTT'                                                386
         AG_EXNAME = 'ATTRIBUTE'                                          386
         AG_PARLIST = 'SEEN_COLO'                                         386
         CALL AXATTRIBUTE                                                 386
C    *                                                                    386
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SVTG_RSIZEMAX, DZ=S  386
C    VTG_ZSIZEMAX <===                                                    386
         CALL AGSRESET                                                    388
         AG_RMIN = SVTG_RSIZEMIN                                          388
         AG_RMAX = SVTG_RSIZEMAX                                          388
         AG_DZ = SVTG_ZSIZEMAX                                            388
         AG_TITLE = 'TUBE'                                                388
         AG_EXNAME = 'SHAPE'                                              388
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      388
         CALL AXSHAPE                                                     388
C    *                                                                    388
C    Create SBRG :                                                        389
C    Call To Local Procedure SBRG                                         389
         ASSIGN 5060 TO BLOCK_SBRG                                        389
         GO TO  5050                                                      389
5060     CONTINUE                                                         389
C    *                                                                    389
C    Geant POSITION : SBRG ===> Z=SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK  390
C    /2 <===                                                              390
         CALL AGSRESET                                                    390
         AG_Z = SWCA_LENGTH/2+SSUP_ERJZDIS+SSUP_ERJTHK/2                  390
         AG_TITLE = 'SBRG'                                                390
         AG_EXNAME = 'POSITION'                                           390
         AG_PARLIST = 'Z'                                                 390
         CALL AXPOSITION                                                  390
C    *                                                                    390
C    Geant POSITION : SBRG ===> Z=-SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTH  390
C    K/2 <===                                                             390
         CALL AGSRESET                                                    391
         AG_Z = -SWCA_LENGTH/2-SSUP_ERJZDIS-SSUP_ERJTHK/2                 391
         AG_TITLE = 'SBRG'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'Z'                                                 391
         CALL AXPOSITION                                                  391
C    *                                                                    391
         ENDRNG_Z=SERG_ENDRNGZM + SERG_ENDRNGTH                           391
         BRACK_Z=SWCA_LENGTH/2 + SSUP_ERJZDIS                             393
         SCREW_Z=ENDRNG_Z + 0.5*(BRACK_Z - ENDRNG_Z)                      394
C    Create SOES :                                                        395
C    Call To Local Procedure SOES                                         395
         ASSIGN 5080 TO BLOCK_SOES                                        395
         GO TO  5070                                                      395
5080     CONTINUE                                                         395
C    *                                                                    395
C    Geant POSITION : SOES ===> Z=SCREW_Z KONLY='MANY' <===               396
         CALL AGSRESET                                                    396
         AG_Z = SCREW_Z                                                   396
         AG_KONLY = 'MANY'                                                396
         AG_TITLE = 'SOES'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'Z_KONLY'                                           396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Geant POSITION : SOES ===> Z=-SCREW_Z KONLY='MANY' <===              396
         CALL AGSRESET                                                    397
         AG_Z = -SCREW_Z                                                  397
         AG_KONLY = 'MANY'                                                397
         AG_TITLE = 'SOES'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'Z_KONLY'                                           397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Create SIES :                                                        398
C    Call To Local Procedure SIES                                         398
         ASSIGN 5100 TO BLOCK_SIES                                        398
         GO TO  5090                                                      398
5100     CONTINUE                                                         398
C    *                                                                    398
C    Geant POSITION : SIES ===> Z=SCREW_Z KONLY='MANY' <===               399
         CALL AGSRESET                                                    399
         AG_Z = SCREW_Z                                                   399
         AG_KONLY = 'MANY'                                                399
         AG_TITLE = 'SIES'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'Z_KONLY'                                           399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Geant POSITION : SIES ===> Z=-SCREW_Z KONLY='MANY' <===              399
         CALL AGSRESET                                                    400
         AG_Z = -SCREW_Z                                                  400
         AG_KONLY = 'MANY'                                                400
         AG_TITLE = 'SIES'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'Z_KONLY'                                           400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Create SCON :                                                        401
C    Call To Local Procedure SCON                                         401
         ASSIGN 5120 TO BLOCK_SCON                                        401
         GO TO  5110                                                      401
5120     CONTINUE                                                         401
C    *                                                                    401
C    Geant POSITION : SCON ===> inherited arguments used <===             402
         CALL AGSRESET                                                    402
         AG_TITLE = 'SCON'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'NONE'                                              402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Geant POSITION : SCON ===> THETAZ=180 <===                           402
         CALL AGSRESET                                                    403
         AG_THETAZ = 180                                                  403
         AG_TITLE = 'SCON'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'THETAZ'                                            403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Create SBSP :                                                        404
C    Call To Local Procedure SBSP                                         404
         ASSIGN 5140 TO BLOCK_SBSP                                        404
         GO TO  5130                                                      404
5140     CONTINUE                                                         404
C    *                                                                    404
C    Geant POSITION : SBSP ===> Z=(SSUP_RODLEN/2-SSUB_KMNTTHK/2) <===     405
         CALL AGSRESET                                                    405
         AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                            405
         AG_TITLE = 'SBSP'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'Z'                                                 405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Geant POSITION : SBSP ===> Z=-(SSUP_RODLEN/2-SSUB_KMNTTHK/2) THETAZ  405
C    =180 <===                                                            405
         CALL AGSRESET                                                    406
         AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                           406
         AG_THETAZ = 180                                                  406
         AG_TITLE = 'SBSP'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'Z_THETAZ'                                          406
         CALL AXPOSITION                                                  406
C    *                                                                    406
5021  CALL AGSPOPB                                                        407
      GO TO BLOCK_SVTT, (5020 ,5030,5040)                                 407
C End of Local Procedure                                                  407
C ----------------------------------------------------------------------  408
C ------------ SCON                                                       408
C Begin Local Procedure SCON                                              409
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               409
5110  CONTINUE                                                            409
         AG_TITLE='SCON'                                                  409
         AG_MODULE=MODULE                                                 409
         AG_MARK='V00'                                                    409
         CALL AGSPUSH                                                     409
         AG_MODULE=MODULE                                                 409
C    *                                                                    409
         CONE_THK1=SSUP_GRPHTHK                                           410
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                411
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              412
C    Geant MATERIAL : AIR ===> inherited arguments used <===              413
         CALL AGSRESET                                                    413
         AG_TITLE = 'AIR'                                                 413
         AG_EXNAME = 'MATERIAL'                                           413
         AG_PARLIST = 'NONE'                                              413
         CALL AXMATERIAL                                                  413
C    *                                                                    413
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       413
         CALL AGSRESET                                                    414
         AG_SEEN = 0                                                      414
         AG_COLO = 1                                                      414
         AG_TITLE = 'SCON'                                                414
         AG_EXNAME = 'ATTRIBUTE'                                          414
         AG_PARLIST = 'SEEN_COLO'                                         414
         CALL AXATTRIBUTE                                                 414
C    *                                                                    414
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  414
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  414
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  414
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  414
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  414
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  414
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      414
         CALL AGSRESET                                                    424
         AG_PHI1 = 0                                                      424
         AG_DPHI = 360                                                    424
         AG_NZ = 7                                                        424
         AG_ZI(1)=SSUP_CONE1ZMN                                           424
         AG_ZI(2)=SSUP_RODLEN/2                                           424
         AG_ZI(3)=SSUP_RODLEN/2                                           424
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              424
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              424
         AG_ZI(6)=SSUP_CONE3ZMX                                           424
         AG_ZI(7)=SSUP_CONE4ZMX                                           424
         AG_RMN(1)=SSUP_CON1IDMN                                          424
         AG_RMN(2)=SSUP_CON1IDMN                                          424
         AG_RMN(3)=SSUP_CON1IDMN                                          424
         AG_RMN(4)=SSUP_CON1IDMN                                          424
         AG_RMN(5)=SSUP_CON3IDMN                                          424
         AG_RMN(6)=SSUP_CON4IDMN                                          424
         AG_RMN(7)=SSUP_CON4IDMX                                          424
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                424
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                424
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                424
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                424
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                424
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                424
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                424
         AG_TITLE = 'PCON'                                                424
         AG_EXNAME = 'SHAPE'                                              424
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           424
         CALL AXSHAPE                                                     424
C    *                                                                    424
C    Create SGRA :                                                        425
C    Call To Local Procedure SGRA                                         425
         ASSIGN 5160 TO BLOCK_SGRA                                        425
         GO TO  5150                                                      425
5160     CONTINUE                                                         425
C    *                                                                    425
C    Geant POSITION : SGRA ===> inherited arguments used <===             426
         CALL AGSRESET                                                    426
         AG_TITLE = 'SGRA'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'NONE'                                              426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Create STAP :                                                        427
C    Call To Local Procedure STAP                                         427
         ASSIGN 5180 TO BLOCK_STAP                                        427
         GO TO  5170                                                      427
5180     CONTINUE                                                         427
C    *                                                                    427
C    Geant POSITION : STAP ===> inherited arguments used <===             428
         CALL AGSRESET                                                    428
         AG_TITLE = 'STAP'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'NONE'                                              428
         CALL AXPOSITION                                                  428
C    *                                                                    428
C    Create STAC :                                                        429
C    Call To Local Procedure STAC                                         429
         ASSIGN 5200 TO BLOCK_STAC                                        429
         GO TO  5190                                                      429
5200     CONTINUE                                                         429
C    *                                                                    429
C    Geant POSITION : STAC ===> inherited arguments used <===             430
         CALL AGSRESET                                                    430
         AG_TITLE = 'STAC'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'NONE'                                              430
         CALL AXPOSITION                                                  430
C    *                                                                    430
C    Create SHLA :                                                        431
C    Call To Local Procedure SHLA                                         431
         ASSIGN 5220 TO BLOCK_SHLA                                        431
         GO TO  5210                                                      431
5220     CONTINUE                                                         431
C    *                                                                    431
C    Geant POSITION : SHLA ===> Z=SSUP_RODLEN/2+SSUP_GRPHTHK+0.5*(SSUP_C  433
C    ONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK) <===                             433
         CALL AGSRESET                                                    433
         AG_Z = SSUP_RODLEN/                                              433
     *   2+SSUP_GRPHTHK+0.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)    433
         AG_TITLE = 'SHLA'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'Z'                                                 433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Create SHLB :                                                        434
C    Call To Local Procedure SHLB                                         434
         ASSIGN 5240 TO BLOCK_SHLB                                        434
         GO TO  5230                                                      434
5240     CONTINUE                                                         434
C    *                                                                    434
C    Geant POSITION : SHLB ===> Z=SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_  435
C    CONE3ZMX) <===                                                       435
         CALL AGSRESET                                                    435
         AG_Z = SSUP_CONE3ZMX+0.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)           435
         AG_TITLE = 'SHLB'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'Z'                                                 435
         CALL AXPOSITION                                                  435
C    *                                                                    435
C    Create SCMY :                                                        436
C    Call To Local Procedure SCMY                                         436
         ASSIGN 5260 TO BLOCK_SCMY                                        436
         GO TO  5250                                                      436
5260     CONTINUE                                                         436
C    *                                                                    436
C    Geant POSITION : SCMY ===> inherited arguments used <===             437
         CALL AGSRESET                                                    437
         AG_TITLE = 'SCMY'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'NONE'                                              437
         CALL AXPOSITION                                                  437
C    *                                                                    437
5111  CALL AGSPOPB                                                        438
      GO TO BLOCK_SCON, (5110 ,5120)                                      438
C End of Local Procedure                                                  438
C ----------------------------------------------------------------------  439
C ------------ SGRA                                                       439
C Begin Local Procedure SGRA                                              440
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 440
5150  CONTINUE                                                            440
         AG_TITLE='SGRA'                                                  440
         AG_MODULE=MODULE                                                 440
         AG_MARK='V00'                                                    440
         CALL AGSPUSH                                                     440
         AG_MODULE=MODULE                                                 440
C    *                                                                    440
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           441
         CALL AGSRESET                                                    441
         AG_TITLE = 'CARBON'                                              441
         AG_EXNAME = 'MATERIAL'                                           441
         AG_PARLIST = 'NONE'                                              441
         CALL AXMATERIAL                                                  441
C    *                                                                    441
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       441
         CALL AGSRESET                                                    442
         AG_SEEN = 1                                                      442
         AG_COLO = 6                                                      442
         AG_TITLE = 'SGRA'                                                442
         AG_EXNAME = 'ATTRIBUTE'                                          442
         AG_PARLIST = 'SEEN_COLO'                                         442
         CALL AXATTRIBUTE                                                 442
C    *                                                                    442
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  442
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  442
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  442
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  442
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  442
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              442
         CALL AGSRESET                                                    450
         AG_PHI1 = 0                                                      450
         AG_DPHI = 360                                                    450
         AG_NZ = 5                                                        450
         AG_ZI(1)=SSUP_RODLEN/2                                           450
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              450
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              450
         AG_ZI(4)=SSUP_CONE3ZMX                                           450
         AG_ZI(5)=SSUP_CONE4ZMX                                           450
         AG_RMN(1)=SSUP_CON1IDMN                                          450
         AG_RMN(2)=SSUP_CON1IDMN                                          450
         AG_RMN(3)=SSUP_CON3IDMN                                          450
         AG_RMN(4)=SSUP_CON4IDMN                                          450
         AG_RMN(5)=SSUP_CON4IDMX                                          450
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             450
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             450
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             450
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             450
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             450
         AG_TITLE = 'PCON'                                                450
         AG_EXNAME = 'SHAPE'                                              450
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           450
         CALL AXSHAPE                                                     450
C    *                                                                    450
5151  CALL AGSPOPB                                                        451
      GO TO BLOCK_SGRA, (5150 ,5160)                                      451
C End of Local Procedure                                                  451
C ----------------------------------------------------------------------  452
C ------------ STAP                                                       452
C Begin Local Procedure STAP                                              453
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  453
C E)                                                                      453
5170  CONTINUE                                                            453
         AG_TITLE='STAP'                                                  453
         AG_MODULE=MODULE                                                 453
         AG_MARK='V00'                                                    453
         CALL AGSPUSH                                                     453
         AG_MODULE=MODULE                                                 453
C    *                                                                    453
         ROFFSET=SSUP_GRPHTHK                                             454
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           455
         CALL AGSRESET                                                    455
         AG_A = 12                                                        455
         AG_Z = 6                                                         455
         AG_W = 1                                                         455
         AG_TITLE = 'C'                                                   455
         AG_EXNAME = 'COMPONENT'                                          455
         AG_PARLIST = 'A_Z_W'                                             455
         CALL AXCOMPONENT                                                 455
C    *                                                                    455
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           455
         CALL AGSRESET                                                    456
         AG_A = 1                                                         456
         AG_Z = 1                                                         456
         AG_W = 2                                                         456
         AG_TITLE = 'H2'                                                  456
         AG_EXNAME = 'COMPONENT'                                          456
         AG_PARLIST = 'A_Z_W'                                             456
         CALL AXCOMPONENT                                                 456
C    *                                                                    456
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             456
         CALL AGSRESET                                                    457
         AG_DENS = 0.935                                                  457
         AG_TITLE = 'CH2'                                                 457
         AG_EXNAME = 'MIXTURE'                                            457
         AG_PARLIST = 'DENS'                                              457
         CALL AXMIXTURE                                                   457
C    *                                                                    457
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       457
         CALL AGSRESET                                                    458
         AG_SEEN = 1                                                      458
         AG_COLO = 3                                                      458
         AG_TITLE = 'STAP'                                                458
         AG_EXNAME = 'ATTRIBUTE'                                          458
         AG_PARLIST = 'SEEN_COLO'                                         458
         CALL AXATTRIBUTE                                                 458
C    *                                                                    458
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  458
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  458
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  458
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  458
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     458
         CALL AGSRESET                                                    464
         AG_PHI1 = 0                                                      464
         AG_DPHI = 360                                                    464
         AG_NZ = 3                                                        464
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              464
         AG_ZI(2)=SSUP_CONE3ZMX                                           464
         AG_ZI(3)=SSUP_CONE4ZMX                                           464
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  464
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  464
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  464
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    464
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    464
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    464
         AG_TITLE = 'PCON'                                                464
         AG_EXNAME = 'SHAPE'                                              464
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           464
         CALL AXSHAPE                                                     464
C    *                                                                    464
5171  CALL AGSPOPB                                                        465
      GO TO BLOCK_STAP, (5170 ,5180)                                      465
C End of Local Procedure                                                  465
C ----------------------------------------------------------------------  466
C ------------ STAC                                                       466
C Begin Local Procedure STAC                                              467
C STAC IS THE COPPER PART OF THE TWIN-AX CABLE LAYER                      467
5190  CONTINUE                                                            467
         AG_TITLE='STAC'                                                  467
         AG_MODULE=MODULE                                                 467
         AG_MARK='V00'                                                    467
         CALL AGSPUSH                                                     467
         AG_MODULE=MODULE                                                 467
C    *                                                                    467
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK/2                             468
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           469
         CALL AGSRESET                                                    469
         AG_TITLE = 'COPPER'                                              469
         AG_EXNAME = 'MATERIAL'                                           469
         AG_PARLIST = 'NONE'                                              469
         CALL AXMATERIAL                                                  469
C    *                                                                    469
C    Geant ATTRIBUTE : STAC ===> SEEN=1 COLO=2 <===                       469
         CALL AGSRESET                                                    470
         AG_SEEN = 1                                                      470
         AG_COLO = 2                                                      470
         AG_TITLE = 'STAC'                                                470
         AG_EXNAME = 'ATTRIBUTE'                                          470
         AG_PARLIST = 'SEEN_COLO'                                         470
         CALL AXATTRIBUTE                                                 470
C    *                                                                    470
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  470
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  470
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  470
C    FFSET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4  470
C    IDMX+ROFFSET+SSUP_CABTHK/2} <===                                     470
         CALL AGSRESET                                                    476
         AG_PHI1 = 0                                                      476
         AG_DPHI = 360                                                    476
         AG_NZ = 3                                                        476
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              476
         AG_ZI(2)=SSUP_CONE3ZMX                                           476
         AG_ZI(3)=SSUP_CONE4ZMX                                           476
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  476
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  476
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  476
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    476
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    476
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    476
         AG_TITLE = 'PCON'                                                476
         AG_EXNAME = 'SHAPE'                                              476
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           476
         CALL AXSHAPE                                                     476
C    *                                                                    476
5191  CALL AGSPOPB                                                        477
      GO TO BLOCK_STAC, (5190 ,5200)                                      477
C End of Local Procedure                                                  477
C ----------------------------------------------------------------------  478
C ------------ SHLA                                                       478
C Begin Local Procedure SHLA                                              479
C SHLA IS THE WATER HOSE LAYER FOR CONE 3 (CLOSER TO VERTEX)              479
5210  CONTINUE                                                            479
         AG_TITLE='SHLA'                                                  479
         AG_MODULE=MODULE                                                 479
         AG_MARK='V00'                                                    479
         CALL AGSPUSH                                                     479
         AG_MODULE=MODULE                                                 479
C    *                                                                    479
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               480
C    Geant MATERIAL : AIR ===> inherited arguments used <===              481
         CALL AGSRESET                                                    481
         AG_TITLE = 'AIR'                                                 481
         AG_EXNAME = 'MATERIAL'                                           481
         AG_PARLIST = 'NONE'                                              481
         CALL AXMATERIAL                                                  481
C    *                                                                    481
C    Geant ATTRIBUTE : SHLA ===> SEEN=0 COLO=1 <===                       481
         CALL AGSRESET                                                    482
         AG_SEEN = 0                                                      482
         AG_COLO = 1                                                      482
         AG_TITLE = 'SHLA'                                                482
         AG_EXNAME = 'ATTRIBUTE'                                          482
         AG_PARLIST = 'SEEN_COLO'                                         482
         CALL AXATTRIBUTE                                                 482
C    *                                                                    482
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRP  482
C    HTHK), RMN1=SSUP_CON3IDMN+ROFFSET, RMX1=SSUP_CON3IDMN+ROFFSET+2.0*S  482
C    SUP_HOSRMX, RMN2=SSUP_CON4IDMN+ROFFSET RMX2=SSUP_CON4IDMN+ROFFSET+2  482
C    .0*SSUP_HOSRMX <===                                                  482
         CALL AGSRESET                                                    486
         AG_DZ = .5*(SSUP_CONE3ZMX-SSUP_RODLEN/2-SSUP_GRPHTHK)            486
         AG_RMN1 = SSUP_CON3IDMN+ROFFSET                                  486
         AG_RMX1 = SSUP_CON3IDMN+ROFFSET+2.0*SSUP_HOSRMX                  486
         AG_RMN2 = SSUP_CON4IDMN+ROFFSET                                  486
         AG_RMX2 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  486
         AG_TITLE = 'CONE'                                                486
         AG_EXNAME = 'SHAPE'                                              486
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            486
         CALL AXSHAPE                                                     486
C    *                                                                    486
5211  CALL AGSPOPB                                                        487
      GO TO BLOCK_SHLA, (5210 ,5220)                                      487
C End of Local Procedure                                                  487
C ----------------------------------------------------------------------  488
C ------------ SHLB                                                       488
C Begin Local Procedure SHLB                                              489
C SHLB IS THE WATER HOSE LAYER CONE 4 (FURTHER FROM VERTEX)               489
5230  CONTINUE                                                            489
         AG_TITLE='SHLB'                                                  489
         AG_MODULE=MODULE                                                 489
         AG_MARK='V00'                                                    489
         CALL AGSPUSH                                                     489
         AG_MODULE=MODULE                                                 489
C    *                                                                    489
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK                               490
C    Geant MATERIAL : AIR ===> inherited arguments used <===              491
         CALL AGSRESET                                                    491
         AG_TITLE = 'AIR'                                                 491
         AG_EXNAME = 'MATERIAL'                                           491
         AG_PARLIST = 'NONE'                                              491
         CALL AXMATERIAL                                                  491
C    *                                                                    491
C    Geant ATTRIBUTE : SHLB ===> SEEN=0 COLO=1 <===                       491
         CALL AGSRESET                                                    492
         AG_SEEN = 0                                                      492
         AG_COLO = 1                                                      492
         AG_TITLE = 'SHLB'                                                492
         AG_EXNAME = 'ATTRIBUTE'                                          492
         AG_PARLIST = 'SEEN_COLO'                                         492
         CALL AXATTRIBUTE                                                 492
C    *                                                                    492
C    Geant SHAPE : CONE ===> DZ=.5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX), RMN1=S  492
C    SUP_CON4IDMN+ROFFSET RMX1=SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX, RM  492
C    N2=SSUP_CON4IDMX+ROFFSET, RMX2=SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRM  492
C    X <===                                                               492
         CALL AGSRESET                                                    496
         AG_DZ = .5*(SSUP_CONE4ZMX-SSUP_CONE3ZMX)                         496
         AG_RMN1 = SSUP_CON4IDMN+ROFFSET                                  496
         AG_RMX1 = SSUP_CON4IDMN+ROFFSET+2.0*SSUP_HOSRMX                  496
         AG_RMN2 = SSUP_CON4IDMX+ROFFSET                                  496
         AG_RMX2 = SSUP_CON4IDMX+ROFFSET+2.0*SSUP_HOSRMX                  496
         AG_TITLE = 'CONE'                                                496
         AG_EXNAME = 'SHAPE'                                              496
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            496
         CALL AXSHAPE                                                     496
C    *                                                                    496
5231  CALL AGSPOPB                                                        497
      GO TO BLOCK_SHLB, (5230 ,5240)                                      497
C End of Local Procedure                                                  497
C ----------------------------------------------------------------------  498
C ------------ SBRG                                                       498
C Begin Local Procedure SBRG                                              499
C SBRG IS THE BRACKET JOINING THE END RINGS                               499
5050  CONTINUE                                                            499
         AG_TITLE='SBRG'                                                  499
         AG_MODULE=MODULE                                                 499
         AG_MARK='V00'                                                    499
         CALL AGSPUSH                                                     499
         AG_MODULE=MODULE                                                 499
C    *                                                                    499
         RIN=SERG_IRNGPRMN                                                500
         ROU=SERG_ORNGRMAX                                                501
C    Geant MATERIAL : AIR ===> inherited arguments used <===              502
         CALL AGSRESET                                                    502
         AG_TITLE = 'AIR'                                                 502
         AG_EXNAME = 'MATERIAL'                                           502
         AG_PARLIST = 'NONE'                                              502
         CALL AXMATERIAL                                                  502
C    *                                                                    502
C    Geant ATTRIBUTE : SBRG ===> SEEN=0 COLO=1 <===                       502
         CALL AGSRESET                                                    503
         AG_SEEN = 0                                                      503
         AG_COLO = 1                                                      503
         AG_TITLE = 'SBRG'                                                503
         AG_EXNAME = 'ATTRIBUTE'                                          503
         AG_PARLIST = 'SEEN_COLO'                                         503
         CALL AXATTRIBUTE                                                 503
C    *                                                                    503
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROU DZ=SSUP_ERJTHK/2 <===      503
         CALL AGSRESET                                                    504
         AG_RMIN = RIN                                                    504
         AG_RMAX = ROU                                                    504
         AG_DZ = SSUP_ERJTHK/2                                            504
         AG_TITLE = 'TUBE'                                                504
         AG_EXNAME = 'SHAPE'                                              504
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      504
         CALL AXSHAPE                                                     504
C    *                                                                    504
5051  CALL AGSPOPB                                                        505
      GO TO BLOCK_SBRG, (5050 ,5060)                                      505
C End of Local Procedure                                                  505
C ----------------------------------------------------------------------  506
C ------------ SOES                                                       506
C Begin Local Procedure SOES                                              507
C SOES IS THE VOLUME TO HOLD OUTER ENDRING SCREWS                         507
5070  CONTINUE                                                            507
         AG_TITLE='SOES'                                                  507
         AG_MODULE=MODULE                                                 507
         AG_MARK='V00'                                                    507
         CALL AGSPUSH                                                     507
         AG_MODULE=MODULE                                                 507
C    *                                                                    507
C    Geant MATERIAL : AIR ===> inherited arguments used <===              508
         CALL AGSRESET                                                    508
         AG_TITLE = 'AIR'                                                 508
         AG_EXNAME = 'MATERIAL'                                           508
         AG_PARLIST = 'NONE'                                              508
         CALL AXMATERIAL                                                  508
C    *                                                                    508
C    Geant ATTRIBUTE : SOES ===> SEEN=0 COLO=1 <===                       508
         CALL AGSRESET                                                    509
         AG_SEEN = 0                                                      509
         AG_COLO = 1                                                      509
         AG_TITLE = 'SOES'                                                509
         AG_EXNAME = 'ATTRIBUTE'                                          509
         AG_PARLIST = 'SEEN_COLO'                                         509
         CALL AXATTRIBUTE                                                 509
C    *                                                                    509
C    Geant SHAPE : TUBE ===> RMIN=SERG_ORNGRMIN RMAX=SERG_ORNGRMAX, DZ=0  509
C    .5*(BRACK_Z-ENDRNG_Z) <===                                           509
         CALL AGSRESET                                                    511
         AG_RMIN = SERG_ORNGRMIN                                          511
         AG_RMAX = SERG_ORNGRMAX                                          511
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   511
         AG_TITLE = 'TUBE'                                                511
         AG_EXNAME = 'SHAPE'                                              511
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      511
         CALL AXSHAPE                                                     511
C    *                                                                    511
C    Create SOSM :                                                        512
C    Call To Local Procedure SOSM                                         512
         ASSIGN 5280 TO BLOCK_SOSM                                        512
         GO TO  5270                                                      512
5280     CONTINUE                                                         512
C    *                                                                    512
5071  CALL AGSPOPB                                                        513
      GO TO BLOCK_SOES, (5070 ,5080)                                      513
C End of Local Procedure                                                  513
C ----------------------------------------------------------------------  514
C ------------ SIES                                                       514
C Begin Local Procedure SIES                                              515
C SIES IS THE VOLUME TO HOLD INNER ENDRING SCREWS                         515
5090  CONTINUE                                                            515
         AG_TITLE='SIES'                                                  515
         AG_MODULE=MODULE                                                 515
         AG_MARK='V00'                                                    515
         CALL AGSPUSH                                                     515
         AG_MODULE=MODULE                                                 515
C    *                                                                    515
C    Geant MATERIAL : AIR ===> inherited arguments used <===              516
         CALL AGSRESET                                                    516
         AG_TITLE = 'AIR'                                                 516
         AG_EXNAME = 'MATERIAL'                                           516
         AG_PARLIST = 'NONE'                                              516
         CALL AXMATERIAL                                                  516
C    *                                                                    516
C    Geant ATTRIBUTE : SIES ===> SEEN=0 COLO=1 <===                       516
         CALL AGSRESET                                                    517
         AG_SEEN = 0                                                      517
         AG_COLO = 1                                                      517
         AG_TITLE = 'SIES'                                                517
         AG_EXNAME = 'ATTRIBUTE'                                          517
         AG_PARLIST = 'SEEN_COLO'                                         517
         CALL AXATTRIBUTE                                                 517
C    *                                                                    517
C    Geant SHAPE : TUBE ===> RMIN=IR_RMIN RMAX=SERG_IRNGTRMX, DZ=0.5*(BR  517
C    ACK_Z-ENDRNG_Z) <===                                                 517
         CALL AGSRESET                                                    519
         AG_RMIN = IR_RMIN                                                519
         AG_RMAX = SERG_IRNGTRMX                                          519
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   519
         AG_TITLE = 'TUBE'                                                519
         AG_EXNAME = 'SHAPE'                                              519
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      519
         CALL AXSHAPE                                                     519
C    *                                                                    519
C    Create SISM :                                                        520
C    Call To Local Procedure SISM                                         520
         ASSIGN 5300 TO BLOCK_SISM                                        520
         GO TO  5290                                                      520
5300     CONTINUE                                                         520
C    *                                                                    520
5091  CALL AGSPOPB                                                        521
      GO TO BLOCK_SIES, (5090 ,5100)                                      521
C End of Local Procedure                                                  521
C ----------------------------------------------------------------------  522
C ------------ SISM                                                       522
C Begin Local Procedure SISM                                              523
C SISM IS THE MOTHER VOLUME DIVISION FOR THE INNER END RING SCREWS        523
5290  CONTINUE                                                            523
         AG_TITLE='SISM'                                                  523
         AG_MODULE=MODULE                                                 523
         AG_MARK='V00'                                                    523
         CALL AGSPUSH                                                     523
         AG_MODULE=MODULE                                                 523
C    *                                                                    523
C    Geant ATTRIBUTE : SISM ===> SEEN=0 COLO=1 <===                       524
         CALL AGSRESET                                                    524
         AG_SEEN = 0                                                      524
         AG_COLO = 1                                                      524
         AG_TITLE = 'SISM'                                                524
         AG_EXNAME = 'ATTRIBUTE'                                          524
         AG_PARLIST = 'SEEN_COLO'                                         524
         CALL AXATTRIBUTE                                                 524
C    *                                                                    524
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                524
         CALL AGSRESET                                                    525
         AG_NDIV = 4                                                      525
         AG_IAXIS = 2                                                     525
         AG_C0 = 45                                                       525
         AG_TITLE = 'DIVISION'                                            525
         AG_EXNAME = 'SHAPE'                                              525
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     525
         CALL AXSHAPE                                                     525
C    *                                                                    525
C    Create SCRW :                                                        526
C    Call To Local Procedure SCRW                                         526
         ASSIGN 5320 TO BLOCK_SCRW                                        526
         GO TO  5310                                                      526
5320     CONTINUE                                                         526
C    *                                                                    526
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X <  527
C    ===                                                                  527
         CALL AGSRESET                                                    527
         AG_X = SSUP_ERJRAD-SSUP_ERJLEN/2+SSUP_ERJ1X                      527
         AG_TITLE = 'SCRW'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'X'                                                 527
         CALL AXPOSITION                                                  527
C    *                                                                    527
5291  CALL AGSPOPB                                                        528
      GO TO BLOCK_SISM, (5290 ,5300)                                      528
C End of Local Procedure                                                  528
C ----------------------------------------------------------------------  529
C ------------ SOSM                                                       529
C Begin Local Procedure SOSM                                              530
C SOSM IS THE MOTHER VOLUME DIVISION FOR THE OUTER END RING SCREWS        530
5270  CONTINUE                                                            530
         AG_TITLE='SOSM'                                                  530
         AG_MODULE=MODULE                                                 530
         AG_MARK='V00'                                                    530
         CALL AGSPUSH                                                     530
         AG_MODULE=MODULE                                                 530
C    *                                                                    530
C    Geant ATTRIBUTE : SOSM ===> SEEN=0 COLO=1 <===                       531
         CALL AGSRESET                                                    531
         AG_SEEN = 0                                                      531
         AG_COLO = 1                                                      531
         AG_TITLE = 'SOSM'                                                531
         AG_EXNAME = 'ATTRIBUTE'                                          531
         AG_PARLIST = 'SEEN_COLO'                                         531
         CALL AXATTRIBUTE                                                 531
C    *                                                                    531
C    Geant SHAPE : DIVISION ===> NDIV=4 IAXIS=2 C0=45 <===                531
         CALL AGSRESET                                                    532
         AG_NDIV = 4                                                      532
         AG_IAXIS = 2                                                     532
         AG_C0 = 45                                                       532
         AG_TITLE = 'DIVISION'                                            532
         AG_EXNAME = 'SHAPE'                                              532
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     532
         CALL AXSHAPE                                                     532
C    *                                                                    532
C    Create SCRW :                                                        533
C    Call To Local Procedure SCRW                                         533
         ASSIGN 5330 TO BLOCK_SCRW                                        533
         GO TO  5310                                                      533
5330     CONTINUE                                                         533
C    *                                                                    533
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X <  534
C    ===                                                                  534
         CALL AGSRESET                                                    534
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ1X                      534
         AG_TITLE = 'SCRW'                                                534
         AG_EXNAME = 'POSITION'                                           534
         AG_PARLIST = 'X'                                                 534
         CALL AXPOSITION                                                  534
C    *                                                                    534
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X <  534
C    ===                                                                  534
         CALL AGSRESET                                                    535
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      535
         AG_TITLE = 'SCRW'                                                535
         AG_EXNAME = 'POSITION'                                           535
         AG_PARLIST = 'X'                                                 535
         CALL AXPOSITION                                                  535
C    *                                                                    535
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  535
C    =SSUP_ERJ2Y <===                                                     535
         CALL AGSRESET                                                    536
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      536
         AG_Y = SSUP_ERJ2Y                                                536
         AG_TITLE = 'SCRW'                                                536
         AG_EXNAME = 'POSITION'                                           536
         AG_PARLIST = 'X_Y'                                               536
         CALL AXPOSITION                                                  536
C    *                                                                    536
C    Geant POSITION : SCRW ===> X=SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X Y  536
C    =-SSUP_ERJ2Y <===                                                    536
         CALL AGSRESET                                                    537
         AG_X = SSUP_ERJRAD+SSUP_ERJLEN/2-SSUP_ERJ2X                      537
         AG_Y = -SSUP_ERJ2Y                                               537
         AG_TITLE = 'SCRW'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'X_Y'                                               537
         CALL AXPOSITION                                                  537
C    *                                                                    537
5271  CALL AGSPOPB                                                        538
      GO TO BLOCK_SOSM, (5270 ,5280)                                      538
C End of Local Procedure                                                  538
C ----------------------------------------------------------------------  539
C ------------ SCRW                                                       539
C Begin Local Procedure SCRW                                              540
C SCRW IS THE SCREW WHICH ATTACHES THE END RING TO THE END RING BRACKET   540
5310  CONTINUE                                                            540
         AG_TITLE='SCRW'                                                  540
         AG_MODULE=MODULE                                                 540
         AG_MARK='V00'                                                    540
         CALL AGSPUSH                                                     540
         AG_MODULE=MODULE                                                 540
C    *                                                                    540
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        541
         CALL AGSRESET                                                    541
         AG_TITLE = 'BERILLIUM'                                           541
         AG_EXNAME = 'MATERIAL'                                           541
         AG_PARLIST = 'NONE'                                              541
         CALL AXMATERIAL                                                  541
C    *                                                                    541
C    Geant ATTRIBUTE : SCRW ===> SEEN=1 COLO=2 <===                       541
         CALL AGSRESET                                                    542
         AG_SEEN = 1                                                      542
         AG_COLO = 2                                                      542
         AG_TITLE = 'SCRW'                                                542
         AG_EXNAME = 'ATTRIBUTE'                                          542
         AG_PARLIST = 'SEEN_COLO'                                         542
         CALL AXATTRIBUTE                                                 542
C    *                                                                    542
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSUP_ERJDIA/2 DZ=0.5*(BRACK_Z-E  542
C    NDRNG_Z) <===                                                        542
         CALL AGSRESET                                                    543
         AG_RMIN = 0                                                      543
         AG_RMAX = SSUP_ERJDIA/2                                          543
         AG_DZ = 0.5*(BRACK_Z-ENDRNG_Z)                                   543
         AG_TITLE = 'TUBE'                                                543
         AG_EXNAME = 'SHAPE'                                              543
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      543
         CALL AXSHAPE                                                     543
C    *                                                                    543
5311  CALL AGSPOPB                                                        544
      GO TO BLOCK_SCRW, (5310 ,5320,5330)                                 544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ SBSP                                                       545
C Begin Local Procedure SBSP                                              546
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              546
5130  CONTINUE                                                            546
         AG_TITLE='SBSP'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant MATERIAL : AIR ===> inherited arguments used <===              547
         CALL AGSRESET                                                    547
         AG_TITLE = 'AIR'                                                 547
         AG_EXNAME = 'MATERIAL'                                           547
         AG_PARLIST = 'NONE'                                              547
         CALL AXMATERIAL                                                  547
C    *                                                                    547
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       547
         CALL AGSRESET                                                    548
         AG_SEEN = 0                                                      548
         AG_COLO = 1                                                      548
         AG_TITLE = 'SBSP'                                                548
         AG_EXNAME = 'ATTRIBUTE'                                          548
         AG_PARLIST = 'SEEN_COLO'                                         548
         CALL AXATTRIBUTE                                                 548
C    *                                                                    548
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  548
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    548
         CALL AGSRESET                                                    550
         AG_RMIN = SVTG_RSIZEMIN                                          550
         AG_RMAX = SSUB_KMOUNTOD/2                                        550
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              550
         AG_TITLE = 'TUBE'                                                550
         AG_EXNAME = 'SHAPE'                                              550
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      550
         CALL AXSHAPE                                                     550
C    *                                                                    550
C    Create SAKM :                                                        551
C    Call To Local Procedure SAKM                                         551
         ASSIGN 5350 TO BLOCK_SAKM                                        551
         GO TO  5340                                                      551
5350     CONTINUE                                                         551
C    *                                                                    551
C    Geant POSITION : SAKM ===> inherited arguments used <===             552
         CALL AGSRESET                                                    552
         AG_TITLE = 'SAKM'                                                552
         AG_EXNAME = 'POSITION'                                           552
         AG_PARLIST = 'NONE'                                              552
         CALL AXPOSITION                                                  552
C    *                                                                    552
C    Create SBMM :                                                        553
C    Call To Local Procedure SBMM                                         553
         ASSIGN 5370 TO BLOCK_SBMM                                        553
         GO TO  5360                                                      553
5370     CONTINUE                                                         553
C    *                                                                    553
C    Create SBRL :                                                        554
C    Call To Local Procedure SBRL                                         554
         ASSIGN 5390 TO BLOCK_SBRL                                        554
         GO TO  5380                                                      554
5390     CONTINUE                                                         554
C    *                                                                    554
C    Create SBRX :                                                        555
C    Call To Local Procedure SBRX                                         555
         ASSIGN 5410 TO BLOCK_SBRX                                        555
         GO TO  5400                                                      555
5410     CONTINUE                                                         555
C    *                                                                    555
C    *                                                                    556
         I=-1                                                             556
            GOTO 5423                                                     556
5421        I=I+(2)                                                       556
5423        IF((2)*(I-(1)).GT.0) GO TO 5422                               556
C       *                                                                 557
            J=0                                                           557
               GO TO 5433                                                 557
5431           J=J+1                                                      557
5433           IF(J-(1).GT.0) GO TO 5432                                  557
               PHI=I*SSUB_SRCUTPHI+180*J                                  558
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     559
     *         2.0)                                                       559
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  561
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  561
C          ==                                                             561
               CALL AGSRESET                                              561
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               561
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              561
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      561
               AG_ALPHAN=AG_ALPHAN+1                                      561
               AG_ALPHAK(AG_ALPHAN)=3                                     561
               AG_ALPHAL(AG_ALPHAN)=PHI                                   561
               AG_TITLE = 'SBMM'                                          561
               AG_EXNAME = 'POSITION'                                     561
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                561
               CALL AXPOSITION                                            561
C          *                                                              561
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         561
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  564
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  564
C           ORT=ZXY <===                                                  564
               CALL AGSRESET                                              564
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               564
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              564
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      564
               AG_ALPHAN=AG_ALPHAN+1                                      564
               AG_ALPHAK(AG_ALPHAN)=3                                     564
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                564
               AG_ALPHAN=AG_ALPHAN+1                                      564
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           564
               AG_TITLE = 'SBRL'                                          564
               AG_EXNAME = 'POSITION'                                     564
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            564
               CALL AXPOSITION                                            564
C          *                                                              564
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  564
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  564
C           ORT=ZXY <===                                                  564
               CALL AGSRESET                                              566
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               566
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              566
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      566
               AG_ALPHAN=AG_ALPHAN+1                                      566
               AG_ALPHAK(AG_ALPHAN)=3                                     566
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                566
               AG_ALPHAN=AG_ALPHAN+1                                      566
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           566
               AG_TITLE = 'SBRX'                                          566
               AG_EXNAME = 'POSITION'                                     566
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            566
               CALL AXPOSITION                                            566
C          *                                                              566
            GO TO 5431                                                    567
5432        CONTINUE                                                      567
         GO TO 5421                                                       568
5422     CONTINUE                                                         568
C    Create SBSR :                                                        569
C    Call To Local Procedure SBSR                                         569
         ASSIGN 5450 TO BLOCK_SBSR                                        569
         GO TO  5440                                                      569
5450     CONTINUE                                                         569
C    *                                                                    569
C    Geant POSITION : SBSR ===> inherited arguments used <===             570
         CALL AGSRESET                                                    570
         AG_TITLE = 'SBSR'                                                570
         AG_EXNAME = 'POSITION'                                           570
         AG_PARLIST = 'NONE'                                              570
         CALL AXPOSITION                                                  570
C    *                                                                    570
5131  CALL AGSPOPB                                                        571
      GO TO BLOCK_SBSP, (5130 ,5140)                                      571
C End of Local Procedure                                                  571
C ----------------------------------------------------------------------  572
C ------------ SAKM                                                       572
C Begin Local Procedure SAKM                                              573
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   573
5340  CONTINUE                                                            573
         AG_TITLE='SAKM'                                                  573
         AG_MODULE=MODULE                                                 573
         AG_MARK='V00'                                                    573
         CALL AGSPUSH                                                     573
         AG_MODULE=MODULE                                                 573
C    *                                                                    573
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        574
         CALL AGSRESET                                                    574
         AG_TITLE = 'ALUMINIUM'                                           574
         AG_EXNAME = 'MATERIAL'                                           574
         AG_PARLIST = 'NONE'                                              574
         CALL AXMATERIAL                                                  574
C    *                                                                    574
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       574
         CALL AGSRESET                                                    575
         AG_SEEN = 1                                                      575
         AG_COLO = 2                                                      575
         AG_TITLE = 'SAKM'                                                575
         AG_EXNAME = 'ATTRIBUTE'                                          575
         AG_PARLIST = 'SEEN_COLO'                                         575
         CALL AXATTRIBUTE                                                 575
C    *                                                                    575
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   575
C    DZ=SSUB_KMNTTHK/2 <===                                               575
         CALL AGSRESET                                                    577
         AG_RMIN = SSUB_KMOUNTID/2                                        577
         AG_RMAX = SSUB_KMOUNTOD/2                                        577
         AG_DZ = SSUB_KMNTTHK/2                                           577
         AG_TITLE = 'TUBE'                                                577
         AG_EXNAME = 'SHAPE'                                              577
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      577
         CALL AXSHAPE                                                     577
C    *                                                                    577
C    Create SCKM :                                                        578
C    Call To Local Procedure SCKM                                         578
         ASSIGN 5470 TO BLOCK_SCKM                                        578
         GO TO  5460                                                      578
5470     CONTINUE                                                         578
C    *                                                                    578
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         579
         CALL AGSRESET                                                    579
         AG_Y = SSUB_KMCUTOFF                                             579
         AG_KONLY = 'MANY'                                                579
         AG_TITLE = 'SCKM'                                                579
         AG_EXNAME = 'POSITION'                                           579
         AG_PARLIST = 'Y_KONLY'                                           579
         CALL AXPOSITION                                                  579
C    *                                                                    579
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  579
C     <===                                                                579
         CALL AGSRESET                                                    580
         AG_Y = -SSUB_KMCUTOFF                                            580
         AG_ALPHAN=AG_ALPHAN+1                                            580
         AG_ALPHAK(AG_ALPHAN)=3                                           580
         AG_ALPHAL(AG_ALPHAN)=180                                         580
         AG_KONLY = 'MANY'                                                580
         AG_TITLE = 'SCKM'                                                580
         AG_EXNAME = 'POSITION'                                           580
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    580
         CALL AXPOSITION                                                  580
C    *                                                                    580
5341  CALL AGSPOPB                                                        581
      GO TO BLOCK_SAKM, (5340 ,5350)                                      581
C End of Local Procedure                                                  581
C ----------------------------------------------------------------------  582
C ------------ SBMM                                                       582
C Begin Local Procedure SBMM                                              583
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     583
5360  CONTINUE                                                            583
         AG_TITLE='SBMM'                                                  583
         AG_MODULE=MODULE                                                 583
         AG_MARK='V00'                                                    583
         CALL AGSPUSH                                                     583
         AG_MODULE=MODULE                                                 583
C    *                                                                    583
C    Geant MATERIAL : AIR ===> inherited arguments used <===              584
         CALL AGSRESET                                                    584
         AG_TITLE = 'AIR'                                                 584
         AG_EXNAME = 'MATERIAL'                                           584
         AG_PARLIST = 'NONE'                                              584
         CALL AXMATERIAL                                                  584
C    *                                                                    584
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       584
         CALL AGSRESET                                                    585
         AG_SEEN = 0                                                      585
         AG_COLO = 1                                                      585
         AG_TITLE = 'SBMM'                                                585
         AG_EXNAME = 'ATTRIBUTE'                                          585
         AG_PARLIST = 'SEEN_COLO'                                         585
         CALL AXATTRIBUTE                                                 585
C    *                                                                    585
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  585
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     585
         CALL AGSRESET                                                    587
         AG_DX = SSUB_MBLKIWID/2                                          587
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           587
         AG_DZ = SSUB_MBLKHGH/2                                           587
         AG_TITLE = 'BOX'                                                 587
         AG_EXNAME = 'SHAPE'                                              587
         AG_PARLIST = 'DX_DY_DZ'                                          587
         CALL AXSHAPE                                                     587
C    *                                                                    587
C    Create SMRD :                                                        588
C    Call To Local Procedure SMRD                                         588
         ASSIGN 5490 TO BLOCK_SMRD                                        588
         GO TO  5480                                                      588
5490     CONTINUE                                                         588
C    *                                                                    588
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      589
     *   SSUB_SROLLOD                                                     589
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  590
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         590
         CALL AGSRESET                                                    590
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       590
         AG_ALPHAN=AG_ALPHAN+1                                            590
         AG_ALPHAK(AG_ALPHAN)=1                                           590
         AG_ALPHAL(AG_ALPHAN)=90                                          590
         AG_TITLE = 'SMRD'                                                590
         AG_EXNAME = 'POSITION'                                           590
         AG_PARLIST = 'Y_ALPHAX'                                          590
         CALL AXPOSITION                                                  590
C    *                                                                    590
C    Create SBMO :                                                        591
C    Call To Local Procedure SBMO                                         591
         ASSIGN 5510 TO BLOCK_SBMO                                        591
         GO TO  5500                                                      591
5510     CONTINUE                                                         591
C    *                                                                    591
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             592
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  593
C    UB_SROLLOD)/2 <===                                                   593
         CALL AGSRESET                                                    593
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       593
         AG_TITLE = 'SBMO'                                                593
         AG_EXNAME = 'POSITION'                                           593
         AG_PARLIST = 'Y'                                                 593
         CALL AXPOSITION                                                  593
C    *                                                                    593
C    Create SBMI :                                                        594
C    Call To Local Procedure SBMI                                         594
         ASSIGN 5530 TO BLOCK_SBMI                                        594
         GO TO  5520                                                      594
5530     CONTINUE                                                         594
C    *                                                                    594
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             595
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  596
C    UB_SROLLOD)/2 <===                                                   596
         CALL AGSRESET                                                    596
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       596
         AG_TITLE = 'SBMI'                                                596
         AG_EXNAME = 'POSITION'                                           596
         AG_PARLIST = 'Y'                                                 596
         CALL AXPOSITION                                                  596
C    *                                                                    596
5361  CALL AGSPOPB                                                        597
      GO TO BLOCK_SBMM, (5360 ,5370)                                      597
C End of Local Procedure                                                  597
C ----------------------------------------------------------------------  598
C ------------ SMRD                                                       598
C Begin Local Procedure SMRD                                              599
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  599
5480  CONTINUE                                                            599
         AG_TITLE='SMRD'                                                  599
         AG_MODULE=MODULE                                                 599
         AG_MARK='V00'                                                    599
         CALL AGSPUSH                                                     599
         AG_MODULE=MODULE                                                 599
C    *                                                                    599
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        600
         CALL AGSRESET                                                    600
         AG_TITLE = 'ALUMINIUM'                                           600
         AG_EXNAME = 'MATERIAL'                                           600
         AG_PARLIST = 'NONE'                                              600
         CALL AXMATERIAL                                                  600
C    *                                                                    600
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       600
         CALL AGSRESET                                                    601
         AG_SEEN = 1                                                      601
         AG_COLO = 2                                                      601
         AG_TITLE = 'SMRD'                                                601
         AG_EXNAME = 'ATTRIBUTE'                                          601
         AG_PARLIST = 'SEEN_COLO'                                         601
         CALL AXATTRIBUTE                                                 601
C    *                                                                    601
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  601
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             601
         CALL AGSRESET                                                    603
         AG_RMIN = 0.0                                                    603
         AG_RMAX = SSUB_MRODDIA/2                                         603
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            603
         AG_TITLE = 'TUBE'                                                603
         AG_EXNAME = 'SHAPE'                                              603
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      603
         CALL AXSHAPE                                                     603
C    *                                                                    603
5481  CALL AGSPOPB                                                        604
      GO TO BLOCK_SMRD, (5480 ,5490)                                      604
C End of Local Procedure                                                  604
C ----------------------------------------------------------------------  605
C ------------ SBMO                                                       605
C Begin Local Procedure SBMO                                              606
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       606
5500  CONTINUE                                                            606
         AG_TITLE='SBMO'                                                  606
         AG_MODULE=MODULE                                                 606
         AG_MARK='V00'                                                    606
         CALL AGSPUSH                                                     606
         AG_MODULE=MODULE                                                 606
C    *                                                                    606
C    Geant MATERIAL : G10 ===> inherited arguments used <===              607
         CALL AGSRESET                                                    607
         AG_TITLE = 'G10'                                                 607
         AG_EXNAME = 'MATERIAL'                                           607
         AG_PARLIST = 'NONE'                                              607
         CALL AXMATERIAL                                                  607
C    *                                                                    607
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       607
         CALL AGSRESET                                                    608
         AG_SEEN = 1                                                      608
         AG_COLO = 3                                                      608
         AG_TITLE = 'SBMO'                                                608
         AG_EXNAME = 'ATTRIBUTE'                                          608
         AG_PARLIST = 'SEEN_COLO'                                         608
         CALL AXATTRIBUTE                                                 608
C    *                                                                    608
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  608
C    UB_MBLKHGH/2 <===                                                    608
         CALL AGSRESET                                                    610
         AG_DX = SSUB_MBLKOWID/2                                          610
         AG_DY = SSUB_MBLKOLEN/2                                          610
         AG_DZ = SSUB_MBLKHGH/2                                           610
         AG_TITLE = 'BOX'                                                 610
         AG_EXNAME = 'SHAPE'                                              610
         AG_PARLIST = 'DX_DY_DZ'                                          610
         CALL AXSHAPE                                                     610
C    *                                                                    610
5501  CALL AGSPOPB                                                        611
      GO TO BLOCK_SBMO, (5500 ,5510)                                      611
C End of Local Procedure                                                  611
C ----------------------------------------------------------------------  612
C ------------ SBMI                                                       612
C Begin Local Procedure SBMI                                              613
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       613
5520  CONTINUE                                                            613
         AG_TITLE='SBMI'                                                  613
         AG_MODULE=MODULE                                                 613
         AG_MARK='V00'                                                    613
         CALL AGSPUSH                                                     613
         AG_MODULE=MODULE                                                 613
C    *                                                                    613
C    Geant MATERIAL : G10 ===> inherited arguments used <===              614
         CALL AGSRESET                                                    614
         AG_TITLE = 'G10'                                                 614
         AG_EXNAME = 'MATERIAL'                                           614
         AG_PARLIST = 'NONE'                                              614
         CALL AXMATERIAL                                                  614
C    *                                                                    614
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       614
         CALL AGSRESET                                                    615
         AG_SEEN = 1                                                      615
         AG_COLO = 3                                                      615
         AG_TITLE = 'SBMO'                                                615
         AG_EXNAME = 'ATTRIBUTE'                                          615
         AG_PARLIST = 'SEEN_COLO'                                         615
         CALL AXATTRIBUTE                                                 615
C    *                                                                    615
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  615
C    UB_MBLKHGH/2 <===                                                    615
         CALL AGSRESET                                                    617
         AG_DX = SSUB_MBLKIWID/2                                          617
         AG_DY = SSUB_MBLKILEN/2                                          617
         AG_DZ = SSUB_MBLKHGH/2                                           617
         AG_TITLE = 'BOX'                                                 617
         AG_EXNAME = 'SHAPE'                                              617
         AG_PARLIST = 'DX_DY_DZ'                                          617
         CALL AXSHAPE                                                     617
C    *                                                                    617
5521  CALL AGSPOPB                                                        618
      GO TO BLOCK_SBMI, (5520 ,5530)                                      618
C End of Local Procedure                                                  618
C ----------------------------------------------------------------------  619
C ------------ SCKM                                                       619
C Begin Local Procedure SCKM                                              620
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      620
5460  CONTINUE                                                            620
         AG_TITLE='SCKM'                                                  620
         AG_MODULE=MODULE                                                 620
         AG_MARK='V00'                                                    620
         CALL AGSPUSH                                                     620
         AG_MODULE=MODULE                                                 620
C    *                                                                    620
C    Geant MATERIAL : AIR ===> inherited arguments used <===              621
         CALL AGSRESET                                                    621
         AG_TITLE = 'AIR'                                                 621
         AG_EXNAME = 'MATERIAL'                                           621
         AG_PARLIST = 'NONE'                                              621
         CALL AXMATERIAL                                                  621
C    *                                                                    621
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       621
         CALL AGSRESET                                                    622
         AG_SEEN = 1                                                      622
         AG_COLO = 1                                                      622
         AG_TITLE = 'SCKM'                                                622
         AG_EXNAME = 'ATTRIBUTE'                                          622
         AG_PARLIST = 'SEEN_COLO'                                         622
         CALL AXATTRIBUTE                                                 622
C    *                                                                    622
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  622
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    622
         CALL AGSRESET                                                    625
         AG_RMIN = SSUB_KMCUTID/2                                         625
         AG_RMAX = SSUB_KMCUTOD/2                                         625
         AG_PHI1 = 270-SSUB_KMCUTOA                                       625
         AG_PHI2 = 270+SSUB_KMCUTOA                                       625
         AG_DZ = SSUB_KMNTTHK/2                                           625
         AG_TITLE = 'TUBS'                                                625
         AG_EXNAME = 'SHAPE'                                              625
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            625
         CALL AXSHAPE                                                     625
C    *                                                                    625
5461  CALL AGSPOPB                                                        626
      GO TO BLOCK_SCKM, (5460 ,5470)                                      626
C End of Local Procedure                                                  626
C ----------------------------------------------------------------------  627
C ------------ SBRL                                                       627
C Begin Local Procedure SBRL                                              628
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      628
5380  CONTINUE                                                            628
         AG_TITLE='SBRL'                                                  628
         AG_MODULE=MODULE                                                 628
         AG_MARK='V00'                                                    628
         CALL AGSPUSH                                                     628
         AG_MODULE=MODULE                                                 628
C    *                                                                    628
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  629
C    ADL=12.6 <===                                                        629
         CALL AGSRESET                                                    629
         AG_A = 20.719                                                    629
         AG_Z = 10.307                                                    629
         AG_DENS = 2.23                                                   629
         AG_ABSL = 50.7                                                   629
         AG_RADL = 12.6                                                   629
         AG_TITLE = 'PYREX'                                               629
         AG_EXNAME = 'MATERIAL'                                           629
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                629
         CALL AXMATERIAL                                                  629
C    *                                                                    629
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       629
         CALL AGSRESET                                                    630
         AG_SEEN = 1                                                      630
         AG_COLO = 6                                                      630
         AG_TITLE = 'SBRL'                                                630
         AG_EXNAME = 'ATTRIBUTE'                                          630
         AG_PARLIST = 'SEEN_COLO'                                         630
         CALL AXATTRIBUTE                                                 630
C    *                                                                    630
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  630
C    =                                                                    630
         CALL AGSRESET                                                    631
         AG_RMIN = SSUB_SROLLID/2                                         631
         AG_RMAX = SSUB_SROLLOD/2                                         631
         AG_TITLE = 'TUBE'                                                631
         AG_EXNAME = 'SHAPE'                                              631
         AG_PARLIST = 'RMIN_RMAX'                                         631
         CALL AXSHAPE                                                     631
C    *                                                                    631
5381  CALL AGSPOPB                                                        632
      GO TO BLOCK_SBRL, (5380 ,5390)                                      632
C End of Local Procedure                                                  632
C ----------------------------------------------------------------------  633
C ------------ SBRX                                                       633
C Begin Local Procedure SBRX                                              634
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 634
5400  CONTINUE                                                            634
         AG_TITLE='SBRX'                                                  634
         AG_MODULE=MODULE                                                 634
         AG_MARK='V00'                                                    634
         CALL AGSPUSH                                                     634
         AG_MODULE=MODULE                                                 634
C    *                                                                    634
C    Geant MATERIAL : IRON ===> inherited arguments used <===             635
         CALL AGSRESET                                                    635
         AG_TITLE = 'IRON'                                                635
         AG_EXNAME = 'MATERIAL'                                           635
         AG_PARLIST = 'NONE'                                              635
         CALL AXMATERIAL                                                  635
C    *                                                                    635
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       635
         CALL AGSRESET                                                    636
         AG_SEEN = 1                                                      636
         AG_COLO = 2                                                      636
         AG_TITLE = 'SBRX'                                                636
         AG_EXNAME = 'ATTRIBUTE'                                          636
         AG_PARLIST = 'SEEN_COLO'                                         636
         CALL AXATTRIBUTE                                                 636
C    *                                                                    636
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  636
C    LEN/2 <===                                                           636
         CALL AGSRESET                                                    638
         AG_RMIN = 0.0                                                    638
         AG_RMAX = SSUB_SROLLID/2                                         638
         AG_DZ = SSUB_SWIRELEN/2                                          638
         AG_TITLE = 'TUBE'                                                638
         AG_EXNAME = 'SHAPE'                                              638
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      638
         CALL AXSHAPE                                                     638
C    *                                                                    638
5401  CALL AGSPOPB                                                        639
      GO TO BLOCK_SBRX, (5400 ,5410)                                      639
C End of Local Procedure                                                  639
C ----------------------------------------------------------------------  640
C ------------ SBSR                                                       640
C Begin Local Procedure SBSR                                              641
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   641
5440  CONTINUE                                                            641
         AG_TITLE='SBSR'                                                  641
         AG_MODULE=MODULE                                                 641
         AG_MARK='V00'                                                    641
         CALL AGSPUSH                                                     641
         AG_MODULE=MODULE                                                 641
C    *                                                                    641
C    Geant MATERIAL : G10 ===> inherited arguments used <===              642
         CALL AGSRESET                                                    642
         AG_TITLE = 'G10'                                                 642
         AG_EXNAME = 'MATERIAL'                                           642
         AG_PARLIST = 'NONE'                                              642
         CALL AXMATERIAL                                                  642
C    *                                                                    642
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       642
         CALL AGSRESET                                                    643
         AG_SEEN = 1                                                      643
         AG_COLO = 3                                                      643
         AG_TITLE = 'SBSR'                                                643
         AG_EXNAME = 'ATTRIBUTE'                                          643
         AG_PARLIST = 'SEEN_COLO'                                         643
         CALL AXATTRIBUTE                                                 643
C    *                                                                    643
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  643
C    =SSUB_SRINGTHK/2 <===                                                643
         CALL AGSRESET                                                    645
         AG_RMIN = SSUB_SRINGID/2                                         645
         AG_RMAX = SSUB_SRINGOD/2                                         645
         AG_DZ = SSUB_SRINGTHK/2                                          645
         AG_TITLE = 'TUBE'                                                645
         AG_EXNAME = 'SHAPE'                                              645
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      645
         CALL AXSHAPE                                                     645
C    *                                                                    645
C    Create SBCR :                                                        646
C    Call To Local Procedure SBCR                                         646
         ASSIGN 5550 TO BLOCK_SBCR                                        646
         GO TO  5540                                                      646
5550     CONTINUE                                                         646
C    *                                                                    646
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             647
C    *                                                                    648
         I=-1                                                             648
            GOTO 5563                                                     648
5561        I=I+(2)                                                       648
5563        IF((2)*(I-(1)).GT.0) GO TO 5562                               648
C       *                                                                 649
            J=0                                                           649
               GO TO 5573                                                 649
5571           J=J+1                                                      649
5573           IF(J-(1).GT.0) GO TO 5572                                  649
               PHI=I*SSUB_SRCUTPHI+180*J                                  650
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  652
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      652
               CALL AGSRESET                                              652
               AG_X = XBUF*SIN(DEGRAD*PHI)                                652
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               652
               AG_ALPHAN=AG_ALPHAN+1                                      652
               AG_ALPHAK(AG_ALPHAN)=3                                     652
               AG_ALPHAL(AG_ALPHAN)=PHI                                   652
               AG_KONLY = 'MANY'                                          652
               AG_TITLE = 'SBCR'                                          652
               AG_EXNAME = 'POSITION'                                     652
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            652
               CALL AXPOSITION                                            652
C          *                                                              652
            GO TO 5571                                                    653
5572        CONTINUE                                                      653
         GO TO 5561                                                       654
5562     CONTINUE                                                         654
5441  CALL AGSPOPB                                                        655
      GO TO BLOCK_SBSR, (5440 ,5450)                                      655
C End of Local Procedure                                                  655
C ----------------------------------------------------------------------  656
C ------------ SBCR                                                       656
C Begin Local Procedure SBCR                                              657
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     657
5540  CONTINUE                                                            657
         AG_TITLE='SBCR'                                                  657
         AG_MODULE=MODULE                                                 657
         AG_MARK='V00'                                                    657
         CALL AGSPUSH                                                     657
         AG_MODULE=MODULE                                                 657
C    *                                                                    657
C    Geant MATERIAL : AIR ===> inherited arguments used <===              658
         CALL AGSRESET                                                    658
         AG_TITLE = 'AIR'                                                 658
         AG_EXNAME = 'MATERIAL'                                           658
         AG_PARLIST = 'NONE'                                              658
         CALL AXMATERIAL                                                  658
C    *                                                                    658
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       658
         CALL AGSRESET                                                    659
         AG_SEEN = 1                                                      659
         AG_COLO = 1                                                      659
         AG_TITLE = 'SBCR'                                                659
         AG_EXNAME = 'ATTRIBUTE'                                          659
         AG_PARLIST = 'SEEN_COLO'                                         659
         CALL AXATTRIBUTE                                                 659
C    *                                                                    659
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  659
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    659
         CALL AGSRESET                                                    661
         AG_DX = SSUB_SRCUTWID/2                                          661
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           661
         AG_DZ = SSUB_SRINGTHK/2                                          661
         AG_TITLE = 'BOX'                                                 661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'DX_DY_DZ'                                          661
         CALL AXSHAPE                                                     661
C    *                                                                    661
5541  CALL AGSPOPB                                                        662
      GO TO BLOCK_SBCR, (5540 ,5550)                                      662
C End of Local Procedure                                                  662
C ----------------------------------------------------------------------  663
C ------------ SCMY                                                       663
C Begin Local Procedure SCMY                                              664
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            664
5250  CONTINUE                                                            664
         AG_TITLE='SCMY'                                                  664
         AG_MODULE=MODULE                                                 664
         AG_MARK='V00'                                                    664
         CALL AGSPUSH                                                     664
         AG_MODULE=MODULE                                                 664
C    *                                                                    664
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             665
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          666
         CALL AGSRESET                                                    666
         AG_A = 12                                                        666
         AG_Z = 6                                                         666
         AG_W = 5                                                         666
         AG_TITLE = 'C5'                                                  666
         AG_EXNAME = 'COMPONENT'                                          666
         AG_PARLIST = 'A_Z_W'                                             666
         CALL AXCOMPONENT                                                 666
C    *                                                                    666
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           666
         CALL AGSRESET                                                    667
         AG_A = 1                                                         667
         AG_Z = 1                                                         667
         AG_W = 4                                                         667
         AG_TITLE = 'H4'                                                  667
         AG_EXNAME = 'COMPONENT'                                          667
         AG_PARLIST = 'A_Z_W'                                             667
         CALL AXCOMPONENT                                                 667
C    *                                                                    667
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          667
         CALL AGSRESET                                                    668
         AG_A = 16                                                        668
         AG_Z = 8                                                         668
         AG_W = 2                                                         668
         AG_TITLE = 'O2'                                                  668
         AG_EXNAME = 'COMPONENT'                                          668
         AG_PARLIST = 'A_Z_W'                                             668
         CALL AXCOMPONENT                                                 668
C    *                                                                    668
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            668
         CALL AGSRESET                                                    669
         AG_DENS = 1.39                                                   669
         AG_TITLE = 'MYLAR'                                               669
         AG_EXNAME = 'MIXTURE'                                            669
         AG_PARLIST = 'DENS'                                              669
         CALL AXMIXTURE                                                   669
C    *                                                                    669
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       669
         CALL AGSRESET                                                    670
         AG_SEEN = 1                                                      670
         AG_COLO = 3                                                      670
         AG_TITLE = 'SCMY'                                                670
         AG_EXNAME = 'ATTRIBUTE'                                          670
         AG_PARLIST = 'SEEN_COLO'                                         670
         CALL AXATTRIBUTE                                                 670
C    *                                                                    670
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  670
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  670
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  670
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  670
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     670
         CALL AGSRESET                                                    676
         AG_PHI1 = 0                                                      676
         AG_DPHI = 360                                                    676
         AG_NZ = 3                                                        676
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              676
         AG_ZI(2)=SSUP_CONE3ZMX                                           676
         AG_ZI(3)=SSUP_CONE4ZMX                                           676
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  676
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  676
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  676
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    676
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    676
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    676
         AG_TITLE = 'PCON'                                                676
         AG_EXNAME = 'SHAPE'                                              676
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           676
         CALL AXSHAPE                                                     676
C    *                                                                    676
5251  CALL AGSPOPB                                                        677
      GO TO BLOCK_SCMY, (5250 ,5260)                                      677
C End of Local Procedure                                                  677
      WRITE(LOUT,5580)'===>>  [End AgML Module SVTTGEO5]  <<==='          677
5580  FORMAT(A32,/,/)                                                     677
C MODULE SVTTGEO5                                                         679
      END                                                                 679
