C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SCONGEO                                                  1
C SCONGEO IS SUPPORT STRUCTURES FROM SVTT MOVED INTO CAVE:                1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SCONGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS SUPPORT STRUCTURES FROM SVTT MOVED INTO CAVE:'/   1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR VICTOR PEREV                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'VICTOR PEREV'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 31 OCT 2007                                                     2
      CHARACTER*40 CREATED /'31 OCT 2007'/                                3
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
      INTEGER BLOCK_SCON                                                  6
      INTEGER BLOCK_SCMY                                                  8
      INTEGER BLOCK_SGRA                                                  8
      INTEGER BLOCK_SBSP                                                  8
      INTEGER BLOCK_SAKM                                                  8
      INTEGER BLOCK_SCKM                                                  8
      INTEGER BLOCK_SBMM                                                  8
      INTEGER BLOCK_SBMI                                                  8
      INTEGER BLOCK_SBMO                                                  8
      INTEGER BLOCK_SMRD                                                  8
      INTEGER BLOCK_SBRL                                                  8
      INTEGER BLOCK_SBRX                                                  8
      INTEGER BLOCK_SBSR                                                  8
      INTEGER BLOCK_SBCR                                                  8
      INTEGER BLOCK_SROD                                                  8
      INTEGER BLOCK_SROH                                                  8
      INTEGER BLOCK_SRON                                                  8
      INTEGER BLOCK_SROI                                                  8
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
      REAL SVTG_NLAYER                                                    10
      EQUIVALENCE (BANK_SVTG(4),SVTG_NLAYER,RE_SVTG_NLAYER),              10
     *(NAMES_SVTG(2),NSVTG_NLAYER)                                        10
      EQUIVALENCE (COMMEN_SVTG(2),CSVTG_NLAYER),(MAP_SVTG(1,2),           10
     *MSVTG_NLAYER)                                                       10
      CHARACTER CSVTG_NLAYER*40/' '/                                      10
      CHARACTER NSVTG_NLAYER*12/'R.NLAYER'/                               10
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
C -                                                                       14
      REAL SSUP_VERSION                                                   14
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            14
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       14
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          14
     *MSSUP_VERSION)                                                      14
      CHARACTER CSSUP_VERSION*40/' '/                                     14
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             14
      INTEGER MSSUP_VERSION(2)/1,1/                                       14
      REAL RE_SSUP_VERSION                                                14
C -                                                                       14
      REAL SSUP_CABTHK                                                    14
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              14
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        14
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           14
     *MSSUP_CABTHK)                                                       14
      CHARACTER CSSUP_CABTHK*40/' '/                                      14
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               14
      INTEGER MSSUP_CABTHK(2)/1,1/                                        14
      REAL RE_SSUP_CABTHK                                                 14
C -                                                                       14
      REAL SSUP_HOSRMN                                                    14
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMN,RE_SSUP_HOSRMN),              14
     *(NAMES_SSUP(3),NSSUP_HOSRMN)                                        14
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMN),(MAP_SSUP(1,3),           14
     *MSSUP_HOSRMN)                                                       14
      CHARACTER CSSUP_HOSRMN*40/' '/                                      14
      CHARACTER NSSUP_HOSRMN*12/'R.HOSRMN'/                               14
      INTEGER MSSUP_HOSRMN(2)/1,1/                                        14
      REAL RE_SSUP_HOSRMN                                                 14
C -                                                                       14
      REAL SSUP_HOSRMX                                                    14
      EQUIVALENCE (BANK_SSUP(6),SSUP_HOSRMX,RE_SSUP_HOSRMX),              14
     *(NAMES_SSUP(4),NSSUP_HOSRMX)                                        14
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_HOSRMX),(MAP_SSUP(1,4),           14
     *MSSUP_HOSRMX)                                                       14
      CHARACTER CSSUP_HOSRMX*40/' '/                                      14
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               14
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        14
      REAL RE_SSUP_HOSRMX                                                 14
C -                                                                       14
      REAL SSUP_NHOSES                                                    14
      EQUIVALENCE (BANK_SSUP(7),SSUP_NHOSES,RE_SSUP_NHOSES),              14
     *(NAMES_SSUP(5),NSSUP_NHOSES)                                        14
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_NHOSES),(MAP_SSUP(1,5),           14
     *MSSUP_NHOSES)                                                       14
      CHARACTER CSSUP_NHOSES*40/' '/                                      14
      CHARACTER NSSUP_NHOSES*12/'R.NHOSES'/                               14
      INTEGER MSSUP_NHOSES(2)/1,1/                                        14
      REAL RE_SSUP_NHOSES                                                 14
C -                                                                       14
      REAL SSUP_WRPMYTHK                                                  14
      EQUIVALENCE (BANK_SSUP(8),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          14
     *(NAMES_SSUP(6),NSSUP_WRPMYTHK)                                      14
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_WRPMYTHK),(MAP_SSUP(1,6),         14
     *MSSUP_WRPMYTHK)                                                     14
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    14
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           14
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      14
      REAL RE_SSUP_WRPMYTHK                                               14
C -                                                                       14
      REAL SSUP_WRPALTHK                                                  14
      EQUIVALENCE (BANK_SSUP(9),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          14
     *(NAMES_SSUP(7),NSSUP_WRPALTHK)                                      14
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_WRPALTHK),(MAP_SSUP(1,7),         14
     *MSSUP_WRPALTHK)                                                     14
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    14
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           14
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      14
      REAL RE_SSUP_WRPALTHK                                               14
C -                                                                       14
      REAL SSUP_GRPHTHK                                                   14
      EQUIVALENCE (BANK_SSUP(10),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),           14
     *(NAMES_SSUP(8),NSSUP_GRPHTHK)                                       14
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_GRPHTHK),(MAP_SSUP(1,8),          14
     *MSSUP_GRPHTHK)                                                      14
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     14
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             14
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       14
      REAL RE_SSUP_GRPHTHK                                                14
C -                                                                       14
      REAL SSUP_CONE1ZMN                                                  14
      EQUIVALENCE (BANK_SSUP(11),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         14
     *(NAMES_SSUP(9),NSSUP_CONE1ZMN)                                      14
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CONE1ZMN),(MAP_SSUP(1,9),         14
     *MSSUP_CONE1ZMN)                                                     14
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    14
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           14
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      14
      REAL RE_SSUP_CONE1ZMN                                               14
C -                                                                       14
      REAL SSUP_RODLEN                                                    14
      EQUIVALENCE (BANK_SSUP(12),SSUP_RODLEN,RE_SSUP_RODLEN),             14
     *(NAMES_SSUP(10),NSSUP_RODLEN)                                       14
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_RODLEN),(MAP_SSUP(1,10),         14
     *MSSUP_RODLEN)                                                       14
      CHARACTER CSSUP_RODLEN*40/' '/                                      14
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               14
      INTEGER MSSUP_RODLEN(2)/1,1/                                        14
      REAL RE_SSUP_RODLEN                                                 14
C -                                                                       14
      REAL SSUP_RODDIST                                                   14
      EQUIVALENCE (BANK_SSUP(13),SSUP_RODDIST,RE_SSUP_RODDIST),           14
     *(NAMES_SSUP(11),NSSUP_RODDIST)                                      14
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_RODDIST),(MAP_SSUP(1,11),        14
     *MSSUP_RODDIST)                                                      14
      CHARACTER CSSUP_RODDIST*40/' '/                                     14
      CHARACTER NSSUP_RODDIST*12/'R.RODDIST'/                             14
      INTEGER MSSUP_RODDIST(2)/1,1/                                       14
      REAL RE_SSUP_RODDIST                                                14
C -                                                                       14
      REAL SSUP_RODID                                                     14
      EQUIVALENCE (BANK_SSUP(14),SSUP_RODID,RE_SSUP_RODID),               14
     *(NAMES_SSUP(12),NSSUP_RODID)                                        14
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_RODID),(MAP_SSUP(1,12),          14
     *MSSUP_RODID)                                                        14
      CHARACTER CSSUP_RODID*40/' '/                                       14
      CHARACTER NSSUP_RODID*12/'R.RODID'/                                 14
      INTEGER MSSUP_RODID(2)/1,1/                                         14
      REAL RE_SSUP_RODID                                                  14
C -                                                                       14
      REAL SSUP_RODOD                                                     14
      EQUIVALENCE (BANK_SSUP(15),SSUP_RODOD,RE_SSUP_RODOD),               14
     *(NAMES_SSUP(13),NSSUP_RODOD)                                        14
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_RODOD),(MAP_SSUP(1,13),          14
     *MSSUP_RODOD)                                                        14
      CHARACTER CSSUP_RODOD*40/' '/                                       14
      CHARACTER NSSUP_RODOD*12/'R.RODOD'/                                 14
      INTEGER MSSUP_RODOD(2)/1,1/                                         14
      REAL RE_SSUP_RODOD                                                  14
C -                                                                       14
      REAL SSUP_RODIDX                                                    14
      EQUIVALENCE (BANK_SSUP(16),SSUP_RODIDX,RE_SSUP_RODIDX),             14
     *(NAMES_SSUP(14),NSSUP_RODIDX)                                       14
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_RODIDX),(MAP_SSUP(1,14),         14
     *MSSUP_RODIDX)                                                       14
      CHARACTER CSSUP_RODIDX*40/' '/                                      14
      CHARACTER NSSUP_RODIDX*12/'R.RODIDX'/                               14
      INTEGER MSSUP_RODIDX(2)/1,1/                                        14
      REAL RE_SSUP_RODIDX                                                 14
C -                                                                       14
      REAL SSUP_RODODX                                                    14
      EQUIVALENCE (BANK_SSUP(17),SSUP_RODODX,RE_SSUP_RODODX),             14
     *(NAMES_SSUP(15),NSSUP_RODODX)                                       14
      EQUIVALENCE (COMMEN_SSUP(15),CSSUP_RODODX),(MAP_SSUP(1,15),         14
     *MSSUP_RODODX)                                                       14
      CHARACTER CSSUP_RODODX*40/' '/                                      14
      CHARACTER NSSUP_RODODX*12/'R.RODODX'/                               14
      INTEGER MSSUP_RODODX(2)/1,1/                                        14
      REAL RE_SSUP_RODODX                                                 14
C -                                                                       14
      REAL SSUP_CARBONSHELL                                               14
      EQUIVALENCE (BANK_SSUP(18),SSUP_CARBONSHELL,RE_SSUP_CARBONSHELL),   14
     *(NAMES_SSUP(16),NSSUP_CARBONSHELL)                                  14
      EQUIVALENCE (COMMEN_SSUP(16),CSSUP_CARBONSHELL),(MAP_SSUP(1,16),    14
     *MSSUP_CARBONSHELL)                                                  14
      CHARACTER CSSUP_CARBONSHELL*40/' '/                                 14
      CHARACTER NSSUP_CARBONSHELL*12/'R.CARBONSHELL'/                     14
      INTEGER MSSUP_CARBONSHELL(2)/1,1/                                   14
      REAL RE_SSUP_CARBONSHELL                                            14
C -                                                                       14
      REAL SSUP_CARBONDENS                                                14
      EQUIVALENCE (BANK_SSUP(19),SSUP_CARBONDENS,RE_SSUP_CARBONDENS),     14
     *(NAMES_SSUP(17),NSSUP_CARBONDENS)                                   14
      EQUIVALENCE (COMMEN_SSUP(17),CSSUP_CARBONDENS),(MAP_SSUP(1,17),     14
     *MSSUP_CARBONDENS)                                                   14
      CHARACTER CSSUP_CARBONDENS*40/' '/                                  14
      CHARACTER NSSUP_CARBONDENS*12/'R.CARBONDENS'/                       14
      INTEGER MSSUP_CARBONDENS(2)/1,1/                                    14
      REAL RE_SSUP_CARBONDENS                                             14
C -                                                                       14
      REAL SSUP_NOMEXDENS                                                 14
      EQUIVALENCE (BANK_SSUP(20),SSUP_NOMEXDENS,RE_SSUP_NOMEXDENS),       14
     *(NAMES_SSUP(18),NSSUP_NOMEXDENS)                                    14
      EQUIVALENCE (COMMEN_SSUP(18),CSSUP_NOMEXDENS),(MAP_SSUP(1,18),      14
     *MSSUP_NOMEXDENS)                                                    14
      CHARACTER CSSUP_NOMEXDENS*40/' '/                                   14
      CHARACTER NSSUP_NOMEXDENS*12/'R.NOMEXDENS'/                         14
      INTEGER MSSUP_NOMEXDENS(2)/1,1/                                     14
      REAL RE_SSUP_NOMEXDENS                                              14
C -                                                                       14
      REAL SSUP_CON1IDMN                                                  14
      EQUIVALENCE (BANK_SSUP(21),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         14
     *(NAMES_SSUP(19),NSSUP_CON1IDMN)                                     14
      EQUIVALENCE (COMMEN_SSUP(19),CSSUP_CON1IDMN),(MAP_SSUP(1,19),       14
     *MSSUP_CON1IDMN)                                                     14
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    14
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           14
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      14
      REAL RE_SSUP_CON1IDMN                                               14
C -                                                                       14
      REAL SSUP_CON3IDMN                                                  14
      EQUIVALENCE (BANK_SSUP(22),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         14
     *(NAMES_SSUP(20),NSSUP_CON3IDMN)                                     14
      EQUIVALENCE (COMMEN_SSUP(20),CSSUP_CON3IDMN),(MAP_SSUP(1,20),       14
     *MSSUP_CON3IDMN)                                                     14
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    14
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           14
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      14
      REAL RE_SSUP_CON3IDMN                                               14
C -                                                                       14
      REAL SSUP_CON4IDMN                                                  14
      EQUIVALENCE (BANK_SSUP(23),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         14
     *(NAMES_SSUP(21),NSSUP_CON4IDMN)                                     14
      EQUIVALENCE (COMMEN_SSUP(21),CSSUP_CON4IDMN),(MAP_SSUP(1,21),       14
     *MSSUP_CON4IDMN)                                                     14
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    14
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           14
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      14
      REAL RE_SSUP_CON4IDMN                                               14
C -                                                                       14
      REAL SSUP_CON4IDMX                                                  14
      EQUIVALENCE (BANK_SSUP(24),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         14
     *(NAMES_SSUP(22),NSSUP_CON4IDMX)                                     14
      EQUIVALENCE (COMMEN_SSUP(22),CSSUP_CON4IDMX),(MAP_SSUP(1,22),       14
     *MSSUP_CON4IDMX)                                                     14
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    14
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           14
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      14
      REAL RE_SSUP_CON4IDMX                                               14
C -                                                                       14
      REAL SSUP_CONE3ZMX                                                  14
      EQUIVALENCE (BANK_SSUP(25),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         14
     *(NAMES_SSUP(23),NSSUP_CONE3ZMX)                                     14
      EQUIVALENCE (COMMEN_SSUP(23),CSSUP_CONE3ZMX),(MAP_SSUP(1,23),       14
     *MSSUP_CONE3ZMX)                                                     14
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    14
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           14
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      14
      REAL RE_SSUP_CONE3ZMX                                               14
C -                                                                       14
      REAL SSUP_CONE4ZMX                                                  14
      EQUIVALENCE (BANK_SSUP(26),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         14
     *(NAMES_SSUP(24),NSSUP_CONE4ZMX)                                     14
      EQUIVALENCE (COMMEN_SSUP(24),CSSUP_CONE4ZMX),(MAP_SSUP(1,24),       14
     *MSSUP_CONE4ZMX)                                                     14
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    14
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           14
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      14
      REAL RE_SSUP_CONE4ZMX                                               14
C -                                                                       14
      REAL SSUP_BRATHK                                                    14
      EQUIVALENCE (BANK_SSUP(27),SSUP_BRATHK,RE_SSUP_BRATHK),             14
     *(NAMES_SSUP(25),NSSUP_BRATHK)                                       14
      EQUIVALENCE (COMMEN_SSUP(25),CSSUP_BRATHK),(MAP_SSUP(1,25),         14
     *MSSUP_BRATHK)                                                       14
      CHARACTER CSSUP_BRATHK*40/' '/                                      14
      CHARACTER NSSUP_BRATHK*12/'R.BRATHK'/                               14
      INTEGER MSSUP_BRATHK(2)/1,1/                                        14
      REAL RE_SSUP_BRATHK                                                 14
C -                                                                       14
      REAL SSUP_ERJTHK                                                    14
      EQUIVALENCE (BANK_SSUP(28),SSUP_ERJTHK,RE_SSUP_ERJTHK),             14
     *(NAMES_SSUP(26),NSSUP_ERJTHK)                                       14
      EQUIVALENCE (COMMEN_SSUP(26),CSSUP_ERJTHK),(MAP_SSUP(1,26),         14
     *MSSUP_ERJTHK)                                                       14
      CHARACTER CSSUP_ERJTHK*40/' '/                                      14
      CHARACTER NSSUP_ERJTHK*12/'R.ERJTHK'/                               14
      INTEGER MSSUP_ERJTHK(2)/1,1/                                        14
      REAL RE_SSUP_ERJTHK                                                 14
C -                                                                       14
      REAL SSUP_ERJWID                                                    14
      EQUIVALENCE (BANK_SSUP(29),SSUP_ERJWID,RE_SSUP_ERJWID),             14
     *(NAMES_SSUP(27),NSSUP_ERJWID)                                       14
      EQUIVALENCE (COMMEN_SSUP(27),CSSUP_ERJWID),(MAP_SSUP(1,27),         14
     *MSSUP_ERJWID)                                                       14
      CHARACTER CSSUP_ERJWID*40/' '/                                      14
      CHARACTER NSSUP_ERJWID*12/'R.ERJWID'/                               14
      INTEGER MSSUP_ERJWID(2)/1,1/                                        14
      REAL RE_SSUP_ERJWID                                                 14
C -                                                                       14
      REAL SSUP_ERJLEN                                                    14
      EQUIVALENCE (BANK_SSUP(30),SSUP_ERJLEN,RE_SSUP_ERJLEN),             14
     *(NAMES_SSUP(28),NSSUP_ERJLEN)                                       14
      EQUIVALENCE (COMMEN_SSUP(28),CSSUP_ERJLEN),(MAP_SSUP(1,28),         14
     *MSSUP_ERJLEN)                                                       14
      CHARACTER CSSUP_ERJLEN*40/' '/                                      14
      CHARACTER NSSUP_ERJLEN*12/'R.ERJLEN'/                               14
      INTEGER MSSUP_ERJLEN(2)/1,1/                                        14
      REAL RE_SSUP_ERJLEN                                                 14
C -                                                                       14
      REAL SSUP_ERJZDIS                                                   14
      EQUIVALENCE (BANK_SSUP(31),SSUP_ERJZDIS,RE_SSUP_ERJZDIS),           14
     *(NAMES_SSUP(29),NSSUP_ERJZDIS)                                      14
      EQUIVALENCE (COMMEN_SSUP(29),CSSUP_ERJZDIS),(MAP_SSUP(1,29),        14
     *MSSUP_ERJZDIS)                                                      14
      CHARACTER CSSUP_ERJZDIS*40/' '/                                     14
      CHARACTER NSSUP_ERJZDIS*12/'R.ERJZDIS'/                             14
      INTEGER MSSUP_ERJZDIS(2)/1,1/                                       14
      REAL RE_SSUP_ERJZDIS                                                14
C -                                                                       14
      REAL SSUP_ERJ1X                                                     14
      EQUIVALENCE (BANK_SSUP(32),SSUP_ERJ1X,RE_SSUP_ERJ1X),               14
     *(NAMES_SSUP(30),NSSUP_ERJ1X)                                        14
      EQUIVALENCE (COMMEN_SSUP(30),CSSUP_ERJ1X),(MAP_SSUP(1,30),          14
     *MSSUP_ERJ1X)                                                        14
      CHARACTER CSSUP_ERJ1X*40/' '/                                       14
      CHARACTER NSSUP_ERJ1X*12/'R.ERJ1X'/                                 14
      INTEGER MSSUP_ERJ1X(2)/1,1/                                         14
      REAL RE_SSUP_ERJ1X                                                  14
C -                                                                       14
      REAL SSUP_ERJ2X                                                     14
      EQUIVALENCE (BANK_SSUP(33),SSUP_ERJ2X,RE_SSUP_ERJ2X),               14
     *(NAMES_SSUP(31),NSSUP_ERJ2X)                                        14
      EQUIVALENCE (COMMEN_SSUP(31),CSSUP_ERJ2X),(MAP_SSUP(1,31),          14
     *MSSUP_ERJ2X)                                                        14
      CHARACTER CSSUP_ERJ2X*40/' '/                                       14
      CHARACTER NSSUP_ERJ2X*12/'R.ERJ2X'/                                 14
      INTEGER MSSUP_ERJ2X(2)/1,1/                                         14
      REAL RE_SSUP_ERJ2X                                                  14
C -                                                                       14
      REAL SSUP_ERJ2Y                                                     14
      EQUIVALENCE (BANK_SSUP(34),SSUP_ERJ2Y,RE_SSUP_ERJ2Y),               14
     *(NAMES_SSUP(32),NSSUP_ERJ2Y)                                        14
      EQUIVALENCE (COMMEN_SSUP(32),CSSUP_ERJ2Y),(MAP_SSUP(1,32),          14
     *MSSUP_ERJ2Y)                                                        14
      CHARACTER CSSUP_ERJ2Y*40/' '/                                       14
      CHARACTER NSSUP_ERJ2Y*12/'R.ERJ2Y'/                                 14
      INTEGER MSSUP_ERJ2Y(2)/1,1/                                         14
      REAL RE_SSUP_ERJ2Y                                                  14
C -                                                                       14
      REAL SSUP_ERJRAD                                                    14
      EQUIVALENCE (BANK_SSUP(35),SSUP_ERJRAD,RE_SSUP_ERJRAD),             14
     *(NAMES_SSUP(33),NSSUP_ERJRAD)                                       14
      EQUIVALENCE (COMMEN_SSUP(33),CSSUP_ERJRAD),(MAP_SSUP(1,33),         14
     *MSSUP_ERJRAD)                                                       14
      CHARACTER CSSUP_ERJRAD*40/' '/                                      14
      CHARACTER NSSUP_ERJRAD*12/'R.ERJRAD'/                               14
      INTEGER MSSUP_ERJRAD(2)/1,1/                                        14
      REAL RE_SSUP_ERJRAD                                                 14
C -                                                                       14
      REAL SSUP_ERJDIA                                                    14
      EQUIVALENCE (BANK_SSUP(36),SSUP_ERJDIA,RE_SSUP_ERJDIA),             14
     *(NAMES_SSUP(34),NSSUP_ERJDIA)                                       14
      EQUIVALENCE (COMMEN_SSUP(34),CSSUP_ERJDIA),(MAP_SSUP(1,34),         14
     *MSSUP_ERJDIA)                                                       14
      CHARACTER CSSUP_ERJDIA*40/' '/                                      14
      CHARACTER NSSUP_ERJDIA*12/'R.ERJDIA'/                               14
      INTEGER MSSUP_ERJDIA(2)/1,1/                                        14
      REAL RE_SSUP_ERJDIA                                                 14
C -                                                                       14
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                14
      PARAMETER (LEN_SSUP=36)                                             14
      PARAMETER (LLEN_SSUP=34)                                            14
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           14
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        14
      DATA IDNUM_SSUP/10*0/                                               14
      REAL BANK_SSUP(LEN_SSUP)                                            14
      CHARACTER*60 BPATH_SSUP/' '/                                        14
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          14
     *BPATH_SSUP                                                          14
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             14
C *                                                                       14
C -                                                                       18
      REAL SSUB_VERSION                                                   18
      EQUIVALENCE (BANK_SSUB(3),SSUB_VERSION,RE_SSUB_VERSION),            18
     *(NAMES_SSUB(1),NSSUB_VERSION)                                       18
      EQUIVALENCE (COMMEN_SSUB(1),CSSUB_VERSION),(MAP_SSUB(1,1),          18
     *MSSUB_VERSION)                                                      18
      CHARACTER CSSUB_VERSION*40/' '/                                     18
      CHARACTER NSSUB_VERSION*12/'R.VERSION'/                             18
      INTEGER MSSUB_VERSION(2)/1,1/                                       18
      REAL RE_SSUB_VERSION                                                18
C -                                                                       18
      REAL SSUB_KMOUNTID                                                  18
      EQUIVALENCE (BANK_SSUB(4),SSUB_KMOUNTID,RE_SSUB_KMOUNTID),          18
     *(NAMES_SSUB(2),NSSUB_KMOUNTID)                                      18
      EQUIVALENCE (COMMEN_SSUB(2),CSSUB_KMOUNTID),(MAP_SSUB(1,2),         18
     *MSSUB_KMOUNTID)                                                     18
      CHARACTER CSSUB_KMOUNTID*40/' '/                                    18
      CHARACTER NSSUB_KMOUNTID*12/'R.KMOUNTID'/                           18
      INTEGER MSSUB_KMOUNTID(2)/1,1/                                      18
      REAL RE_SSUB_KMOUNTID                                               18
C -                                                                       18
      REAL SSUB_KMOUNTOD                                                  18
      EQUIVALENCE (BANK_SSUB(5),SSUB_KMOUNTOD,RE_SSUB_KMOUNTOD),          18
     *(NAMES_SSUB(3),NSSUB_KMOUNTOD)                                      18
      EQUIVALENCE (COMMEN_SSUB(3),CSSUB_KMOUNTOD),(MAP_SSUB(1,3),         18
     *MSSUB_KMOUNTOD)                                                     18
      CHARACTER CSSUB_KMOUNTOD*40/' '/                                    18
      CHARACTER NSSUB_KMOUNTOD*12/'R.KMOUNTOD'/                           18
      INTEGER MSSUB_KMOUNTOD(2)/1,1/                                      18
      REAL RE_SSUB_KMOUNTOD                                               18
C -                                                                       18
      REAL SSUB_KMNTTHK                                                   18
      EQUIVALENCE (BANK_SSUB(6),SSUB_KMNTTHK,RE_SSUB_KMNTTHK),            18
     *(NAMES_SSUB(4),NSSUB_KMNTTHK)                                       18
      EQUIVALENCE (COMMEN_SSUB(4),CSSUB_KMNTTHK),(MAP_SSUB(1,4),          18
     *MSSUB_KMNTTHK)                                                      18
      CHARACTER CSSUB_KMNTTHK*40/' '/                                     18
      CHARACTER NSSUB_KMNTTHK*12/'R.KMNTTHK'/                             18
      INTEGER MSSUB_KMNTTHK(2)/1,1/                                       18
      REAL RE_SSUB_KMNTTHK                                                18
C -                                                                       18
      REAL SSUB_KMCUTOD                                                   18
      EQUIVALENCE (BANK_SSUB(7),SSUB_KMCUTOD,RE_SSUB_KMCUTOD),            18
     *(NAMES_SSUB(5),NSSUB_KMCUTOD)                                       18
      EQUIVALENCE (COMMEN_SSUB(5),CSSUB_KMCUTOD),(MAP_SSUB(1,5),          18
     *MSSUB_KMCUTOD)                                                      18
      CHARACTER CSSUB_KMCUTOD*40/' '/                                     18
      CHARACTER NSSUB_KMCUTOD*12/'R.KMCUTOD'/                             18
      INTEGER MSSUB_KMCUTOD(2)/1,1/                                       18
      REAL RE_SSUB_KMCUTOD                                                18
C -                                                                       18
      REAL SSUB_KMCUTID                                                   18
      EQUIVALENCE (BANK_SSUB(8),SSUB_KMCUTID,RE_SSUB_KMCUTID),            18
     *(NAMES_SSUB(6),NSSUB_KMCUTID)                                       18
      EQUIVALENCE (COMMEN_SSUB(6),CSSUB_KMCUTID),(MAP_SSUB(1,6),          18
     *MSSUB_KMCUTID)                                                      18
      CHARACTER CSSUB_KMCUTID*40/' '/                                     18
      CHARACTER NSSUB_KMCUTID*12/'R.KMCUTID'/                             18
      INTEGER MSSUB_KMCUTID(2)/1,1/                                       18
      REAL RE_SSUB_KMCUTID                                                18
C -                                                                       18
      REAL SSUB_KMCUTOA                                                   18
      EQUIVALENCE (BANK_SSUB(9),SSUB_KMCUTOA,RE_SSUB_KMCUTOA),            18
     *(NAMES_SSUB(7),NSSUB_KMCUTOA)                                       18
      EQUIVALENCE (COMMEN_SSUB(7),CSSUB_KMCUTOA),(MAP_SSUB(1,7),          18
     *MSSUB_KMCUTOA)                                                      18
      CHARACTER CSSUB_KMCUTOA*40/' '/                                     18
      CHARACTER NSSUB_KMCUTOA*12/'R.KMCUTOA'/                             18
      INTEGER MSSUB_KMCUTOA(2)/1,1/                                       18
      REAL RE_SSUB_KMCUTOA                                                18
C -                                                                       18
      REAL SSUB_KMCUTOFF                                                  18
      EQUIVALENCE (BANK_SSUB(10),SSUB_KMCUTOFF,RE_SSUB_KMCUTOFF),         18
     *(NAMES_SSUB(8),NSSUB_KMCUTOFF)                                      18
      EQUIVALENCE (COMMEN_SSUB(8),CSSUB_KMCUTOFF),(MAP_SSUB(1,8),         18
     *MSSUB_KMCUTOFF)                                                     18
      CHARACTER CSSUB_KMCUTOFF*40/' '/                                    18
      CHARACTER NSSUB_KMCUTOFF*12/'R.KMCUTOFF'/                           18
      INTEGER MSSUB_KMCUTOFF(2)/1,1/                                      18
      REAL RE_SSUB_KMCUTOFF                                               18
C -                                                                       18
      REAL SSUB_SRINGID                                                   18
      EQUIVALENCE (BANK_SSUB(11),SSUB_SRINGID,RE_SSUB_SRINGID),           18
     *(NAMES_SSUB(9),NSSUB_SRINGID)                                       18
      EQUIVALENCE (COMMEN_SSUB(9),CSSUB_SRINGID),(MAP_SSUB(1,9),          18
     *MSSUB_SRINGID)                                                      18
      CHARACTER CSSUB_SRINGID*40/' '/                                     18
      CHARACTER NSSUB_SRINGID*12/'R.SRINGID'/                             18
      INTEGER MSSUB_SRINGID(2)/1,1/                                       18
      REAL RE_SSUB_SRINGID                                                18
C -                                                                       18
      REAL SSUB_SRINGOD                                                   18
      EQUIVALENCE (BANK_SSUB(12),SSUB_SRINGOD,RE_SSUB_SRINGOD),           18
     *(NAMES_SSUB(10),NSSUB_SRINGOD)                                      18
      EQUIVALENCE (COMMEN_SSUB(10),CSSUB_SRINGOD),(MAP_SSUB(1,10),        18
     *MSSUB_SRINGOD)                                                      18
      CHARACTER CSSUB_SRINGOD*40/' '/                                     18
      CHARACTER NSSUB_SRINGOD*12/'R.SRINGOD'/                             18
      INTEGER MSSUB_SRINGOD(2)/1,1/                                       18
      REAL RE_SSUB_SRINGOD                                                18
C -                                                                       18
      REAL SSUB_SRINGTHK                                                  18
      EQUIVALENCE (BANK_SSUB(13),SSUB_SRINGTHK,RE_SSUB_SRINGTHK),         18
     *(NAMES_SSUB(11),NSSUB_SRINGTHK)                                     18
      EQUIVALENCE (COMMEN_SSUB(11),CSSUB_SRINGTHK),(MAP_SSUB(1,11),       18
     *MSSUB_SRINGTHK)                                                     18
      CHARACTER CSSUB_SRINGTHK*40/' '/                                    18
      CHARACTER NSSUB_SRINGTHK*12/'R.SRINGTHK'/                           18
      INTEGER MSSUB_SRINGTHK(2)/1,1/                                      18
      REAL RE_SSUB_SRINGTHK                                               18
C -                                                                       18
      REAL SSUB_SRCUTPHI                                                  18
      EQUIVALENCE (BANK_SSUB(14),SSUB_SRCUTPHI,RE_SSUB_SRCUTPHI),         18
     *(NAMES_SSUB(12),NSSUB_SRCUTPHI)                                     18
      EQUIVALENCE (COMMEN_SSUB(12),CSSUB_SRCUTPHI),(MAP_SSUB(1,12),       18
     *MSSUB_SRCUTPHI)                                                     18
      CHARACTER CSSUB_SRCUTPHI*40/' '/                                    18
      CHARACTER NSSUB_SRCUTPHI*12/'R.SRCUTPHI'/                           18
      INTEGER MSSUB_SRCUTPHI(2)/1,1/                                      18
      REAL RE_SSUB_SRCUTPHI                                               18
C -                                                                       18
      REAL SSUB_SRCUTWID                                                  18
      EQUIVALENCE (BANK_SSUB(15),SSUB_SRCUTWID,RE_SSUB_SRCUTWID),         18
     *(NAMES_SSUB(13),NSSUB_SRCUTWID)                                     18
      EQUIVALENCE (COMMEN_SSUB(13),CSSUB_SRCUTWID),(MAP_SSUB(1,13),       18
     *MSSUB_SRCUTWID)                                                     18
      CHARACTER CSSUB_SRCUTWID*40/' '/                                    18
      CHARACTER NSSUB_SRCUTWID*12/'R.SRCUTWID'/                           18
      INTEGER MSSUB_SRCUTWID(2)/1,1/                                      18
      REAL RE_SSUB_SRCUTWID                                               18
C -                                                                       18
      REAL SSUB_SRCUTOUT                                                  18
      EQUIVALENCE (BANK_SSUB(16),SSUB_SRCUTOUT,RE_SSUB_SRCUTOUT),         18
     *(NAMES_SSUB(14),NSSUB_SRCUTOUT)                                     18
      EQUIVALENCE (COMMEN_SSUB(14),CSSUB_SRCUTOUT),(MAP_SSUB(1,14),       18
     *MSSUB_SRCUTOUT)                                                     18
      CHARACTER CSSUB_SRCUTOUT*40/' '/                                    18
      CHARACTER NSSUB_SRCUTOUT*12/'R.SRCUTOUT'/                           18
      INTEGER MSSUB_SRCUTOUT(2)/1,1/                                      18
      REAL RE_SSUB_SRCUTOUT                                               18
C -                                                                       18
      REAL SSUB_SRCUTIN                                                   18
      EQUIVALENCE (BANK_SSUB(17),SSUB_SRCUTIN,RE_SSUB_SRCUTIN),           18
     *(NAMES_SSUB(15),NSSUB_SRCUTIN)                                      18
      EQUIVALENCE (COMMEN_SSUB(15),CSSUB_SRCUTIN),(MAP_SSUB(1,15),        18
     *MSSUB_SRCUTIN)                                                      18
      CHARACTER CSSUB_SRCUTIN*40/' '/                                     18
      CHARACTER NSSUB_SRCUTIN*12/'R.SRCUTIN'/                             18
      INTEGER MSSUB_SRCUTIN(2)/1,1/                                       18
      REAL RE_SSUB_SRCUTIN                                                18
C -                                                                       18
      REAL SSUB_SROLLID                                                   18
      EQUIVALENCE (BANK_SSUB(18),SSUB_SROLLID,RE_SSUB_SROLLID),           18
     *(NAMES_SSUB(16),NSSUB_SROLLID)                                      18
      EQUIVALENCE (COMMEN_SSUB(16),CSSUB_SROLLID),(MAP_SSUB(1,16),        18
     *MSSUB_SROLLID)                                                      18
      CHARACTER CSSUB_SROLLID*40/' '/                                     18
      CHARACTER NSSUB_SROLLID*12/'R.SROLLID'/                             18
      INTEGER MSSUB_SROLLID(2)/1,1/                                       18
      REAL RE_SSUB_SROLLID                                                18
C -                                                                       18
      REAL SSUB_SROLLOD                                                   18
      EQUIVALENCE (BANK_SSUB(19),SSUB_SROLLOD,RE_SSUB_SROLLOD),           18
     *(NAMES_SSUB(17),NSSUB_SROLLOD)                                      18
      EQUIVALENCE (COMMEN_SSUB(17),CSSUB_SROLLOD),(MAP_SSUB(1,17),        18
     *MSSUB_SROLLOD)                                                      18
      CHARACTER CSSUB_SROLLOD*40/' '/                                     18
      CHARACTER NSSUB_SROLLOD*12/'R.SROLLOD'/                             18
      INTEGER MSSUB_SROLLOD(2)/1,1/                                       18
      REAL RE_SSUB_SROLLOD                                                18
C -                                                                       18
      REAL SSUB_SROLLLEN                                                  18
      EQUIVALENCE (BANK_SSUB(20),SSUB_SROLLLEN,RE_SSUB_SROLLLEN),         18
     *(NAMES_SSUB(18),NSSUB_SROLLLEN)                                     18
      EQUIVALENCE (COMMEN_SSUB(18),CSSUB_SROLLLEN),(MAP_SSUB(1,18),       18
     *MSSUB_SROLLLEN)                                                     18
      CHARACTER CSSUB_SROLLLEN*40/' '/                                    18
      CHARACTER NSSUB_SROLLLEN*12/'R.SROLLLEN'/                           18
      INTEGER MSSUB_SROLLLEN(2)/1,1/                                      18
      REAL RE_SSUB_SROLLLEN                                               18
C -                                                                       18
      REAL SSUB_SWIRELEN                                                  18
      EQUIVALENCE (BANK_SSUB(21),SSUB_SWIRELEN,RE_SSUB_SWIRELEN),         18
     *(NAMES_SSUB(19),NSSUB_SWIRELEN)                                     18
      EQUIVALENCE (COMMEN_SSUB(19),CSSUB_SWIRELEN),(MAP_SSUB(1,19),       18
     *MSSUB_SWIRELEN)                                                     18
      CHARACTER CSSUB_SWIRELEN*40/' '/                                    18
      CHARACTER NSSUB_SWIRELEN*12/'R.SWIRELEN'/                           18
      INTEGER MSSUB_SWIRELEN(2)/1,1/                                      18
      REAL RE_SSUB_SWIRELEN                                               18
C -                                                                       18
      REAL SSUB_MBLKHGH                                                   18
      EQUIVALENCE (BANK_SSUB(22),SSUB_MBLKHGH,RE_SSUB_MBLKHGH),           18
     *(NAMES_SSUB(20),NSSUB_MBLKHGH)                                      18
      EQUIVALENCE (COMMEN_SSUB(20),CSSUB_MBLKHGH),(MAP_SSUB(1,20),        18
     *MSSUB_MBLKHGH)                                                      18
      CHARACTER CSSUB_MBLKHGH*40/' '/                                     18
      CHARACTER NSSUB_MBLKHGH*12/'R.MBLKHGH'/                             18
      INTEGER MSSUB_MBLKHGH(2)/1,1/                                       18
      REAL RE_SSUB_MBLKHGH                                                18
C -                                                                       18
      REAL SSUB_MBLKOWID                                                  18
      EQUIVALENCE (BANK_SSUB(23),SSUB_MBLKOWID,RE_SSUB_MBLKOWID),         18
     *(NAMES_SSUB(21),NSSUB_MBLKOWID)                                     18
      EQUIVALENCE (COMMEN_SSUB(21),CSSUB_MBLKOWID),(MAP_SSUB(1,21),       18
     *MSSUB_MBLKOWID)                                                     18
      CHARACTER CSSUB_MBLKOWID*40/' '/                                    18
      CHARACTER NSSUB_MBLKOWID*12/'R.MBLKOWID'/                           18
      INTEGER MSSUB_MBLKOWID(2)/1,1/                                      18
      REAL RE_SSUB_MBLKOWID                                               18
C -                                                                       18
      REAL SSUB_MBLKOLEN                                                  18
      EQUIVALENCE (BANK_SSUB(24),SSUB_MBLKOLEN,RE_SSUB_MBLKOLEN),         18
     *(NAMES_SSUB(22),NSSUB_MBLKOLEN)                                     18
      EQUIVALENCE (COMMEN_SSUB(22),CSSUB_MBLKOLEN),(MAP_SSUB(1,22),       18
     *MSSUB_MBLKOLEN)                                                     18
      CHARACTER CSSUB_MBLKOLEN*40/' '/                                    18
      CHARACTER NSSUB_MBLKOLEN*12/'R.MBLKOLEN'/                           18
      INTEGER MSSUB_MBLKOLEN(2)/1,1/                                      18
      REAL RE_SSUB_MBLKOLEN                                               18
C -                                                                       18
      REAL SSUB_MBLKIWID                                                  18
      EQUIVALENCE (BANK_SSUB(25),SSUB_MBLKIWID,RE_SSUB_MBLKIWID),         18
     *(NAMES_SSUB(23),NSSUB_MBLKIWID)                                     18
      EQUIVALENCE (COMMEN_SSUB(23),CSSUB_MBLKIWID),(MAP_SSUB(1,23),       18
     *MSSUB_MBLKIWID)                                                     18
      CHARACTER CSSUB_MBLKIWID*40/' '/                                    18
      CHARACTER NSSUB_MBLKIWID*12/'R.MBLKIWID'/                           18
      INTEGER MSSUB_MBLKIWID(2)/1,1/                                      18
      REAL RE_SSUB_MBLKIWID                                               18
C -                                                                       18
      REAL SSUB_MBLKILEN                                                  18
      EQUIVALENCE (BANK_SSUB(26),SSUB_MBLKILEN,RE_SSUB_MBLKILEN),         18
     *(NAMES_SSUB(24),NSSUB_MBLKILEN)                                     18
      EQUIVALENCE (COMMEN_SSUB(24),CSSUB_MBLKILEN),(MAP_SSUB(1,24),       18
     *MSSUB_MBLKILEN)                                                     18
      CHARACTER CSSUB_MBLKILEN*40/' '/                                    18
      CHARACTER NSSUB_MBLKILEN*12/'R.MBLKILEN'/                           18
      INTEGER MSSUB_MBLKILEN(2)/1,1/                                      18
      REAL RE_SSUB_MBLKILEN                                               18
C -                                                                       18
      REAL SSUB_MBLKORAD                                                  18
      EQUIVALENCE (BANK_SSUB(27),SSUB_MBLKORAD,RE_SSUB_MBLKORAD),         18
     *(NAMES_SSUB(25),NSSUB_MBLKORAD)                                     18
      EQUIVALENCE (COMMEN_SSUB(25),CSSUB_MBLKORAD),(MAP_SSUB(1,25),       18
     *MSSUB_MBLKORAD)                                                     18
      CHARACTER CSSUB_MBLKORAD*40/' '/                                    18
      CHARACTER NSSUB_MBLKORAD*12/'R.MBLKORAD'/                           18
      INTEGER MSSUB_MBLKORAD(2)/1,1/                                      18
      REAL RE_SSUB_MBLKORAD                                               18
C -                                                                       18
      REAL SSUB_MBLKIRAD                                                  18
      EQUIVALENCE (BANK_SSUB(28),SSUB_MBLKIRAD,RE_SSUB_MBLKIRAD),         18
     *(NAMES_SSUB(26),NSSUB_MBLKIRAD)                                     18
      EQUIVALENCE (COMMEN_SSUB(26),CSSUB_MBLKIRAD),(MAP_SSUB(1,26),       18
     *MSSUB_MBLKIRAD)                                                     18
      CHARACTER CSSUB_MBLKIRAD*40/' '/                                    18
      CHARACTER NSSUB_MBLKIRAD*12/'R.MBLKIRAD'/                           18
      INTEGER MSSUB_MBLKIRAD(2)/1,1/                                      18
      REAL RE_SSUB_MBLKIRAD                                               18
C -                                                                       18
      REAL SSUB_MRODDIA                                                   18
      EQUIVALENCE (BANK_SSUB(29),SSUB_MRODDIA,RE_SSUB_MRODDIA),           18
     *(NAMES_SSUB(27),NSSUB_MRODDIA)                                      18
      EQUIVALENCE (COMMEN_SSUB(27),CSSUB_MRODDIA),(MAP_SSUB(1,27),        18
     *MSSUB_MRODDIA)                                                      18
      CHARACTER CSSUB_MRODDIA*40/' '/                                     18
      CHARACTER NSSUB_MRODDIA*12/'R.MRODDIA'/                             18
      INTEGER MSSUB_MRODDIA(2)/1,1/                                       18
      REAL RE_SSUB_MRODDIA                                                18
C -                                                                       18
      INTEGER LEN_SSUB,LLEN_SSUB,LINK_SSUB                                18
      PARAMETER (LEN_SSUB=29)                                             18
      PARAMETER (LLEN_SSUB=27)                                            18
      INTEGER FLAG_SSUB/0/,MAP_SSUB(2,LLEN_SSUB),IDNUM_SSUB(10)           18
      CHARACTER NAMES_SSUB(LLEN_SSUB)*12,COMMEN_SSUB(LLEN_SSUB)*40        18
      DATA IDNUM_SSUB/10*0/                                               18
      REAL BANK_SSUB(LEN_SSUB)                                            18
      CHARACTER*60 BPATH_SSUB/' '/                                        18
      SAVE FLAG_SSUB,MAP_SSUB,IDNUM_SSUB,NAMES_SSUB,COMMEN_SSUB,          18
     *BPATH_SSUB                                                          18
      DATA LINK_SSUB/0/,BANK_SSUB/LEN_SSUB*0/                             18
C *                                                                       18
      INTEGER ILAYER,S,SIDE,ILAD,IWAF,I,J                                 19
      REAL LADTHK,CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0         21
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           23
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          25
      REAL ENDRNG_Z,BRACK_Z,SCREW_Z,IR_RMIN,ANG,WAFPCKLEN,DTHK,RADTILT    27
      REAL XBUF,PHI,XBUF1,XBUF2                                           29
      REAL YPCB,A,CUTHK,SQ,TUBE_ANGLE                                     31
      REAL RADII(6),RAD_CONES_IN(5),RAD_CONES_OUT(5),RAD_OFFSET,          33
     *SHIELD_PHI(4)                                                       33
      REAL TRAPY,SSIDX,SSIRY                                              35
      INTEGER I_PHI                                                       37
      SHIELD_PHI(1)=9.0                                                   39
      SHIELD_PHI(2)=27.0                                                  40
      SHIELD_PHI(3)=45.0                                                  41
      SHIELD_PHI(4)=63.0                                                  42
C *                                                                       43
      CALL AGSBEGM(MODULE,IPRIN)                                          43
      IF (IPRIN.LT.0) RETURN                                              43
C Filling bank SVTG - BASIC SVT DIMENSIONS                                43
      BANK_TITLE='BASIC SVT DIMENSIONS'                                   43
      BPATH_SVTG='SVTG*'                                                  43
      IDNUM_SVTG(1)=0                                                     44
C GEOMETRY VERSION                                                        44
      SVTG_VERSION=2                                                      44
      CSVTG_VERSION='GEOMETRY VERSION'                                    44
C NUMBER OF SVT LAYERS (WAS 7)                                            45
      SVTG_NLAYER=7                                                       45
      CSVTG_NLAYER='NUMBER OF SVT LAYERS (WAS 7)'                         45
C STV INNERMOST RADIUS                                                    46
      SVTG_RSIZEMIN=4.100                                                 46
      CSVTG_RSIZEMIN='STV INNERMOST RADIUS'                               46
C STV OUTERMOST RADIUS                                                    47
      SVTG_RSIZEMAX=46.107                                                47
      CSVTG_RSIZEMAX='STV OUTERMOST RADIUS'                               47
C SVT+FTPC LENGTH                                                         48
      SVTG_ZSIZEMAX=270                                                   48
      CSVTG_ZSIZEMAX='SVT+FTPC LENGTH'                                    48
C ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3                          49
      SVTG_ANGOFF=0                                                       49
      CSVTG_ANGOFF='ANGULAR OFFSET X1 FOR SLAYER 2 X2 FOR SLAYER 3'       49
C VERSIONING OF THE SHIELD                                                50
      SVTG_SUPPORTVER=1                                                   50
      CSVTG_SUPPORTVER='VERSIONING OF THE SHIELD'                         50
C VERSIONING OF THE SUPPORT CONE                                          51
      SVTG_CONEVER=1                                                      51
      CSVTG_CONEVER='VERSIONING OF THE SUPPORT CONE'                      51
C WHETHER WE USE THE GEANT MANY OPTION                                    52
      SVTG_IFMANY=0                                                       52
      CSVTG_IFMANY='WHETHER WE USE THE GEANT MANY OPTION'                 52
C THE INDEX OF THE INNERMOST LAYER                                        53
      SVTG_NMIN=1                                                         53
      CSVTG_NMIN='THE INDEX OF THE INNERMOST LAYER'                       53
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    54
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           54
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   54
C *                                                                       54
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SVTG',    54
     *BPATH_SVTG,IDNUM_SVTG,1, MAP_SVTG,NAMES_SVTG,COMMEN_SVTG,           54
     *BANK_SVTG,LEN_SVTG,LLEN_SVTG,LINK_SVTG,FLAG_SVTG)                   54
C *                                                                       55
      CALL AGSBEGM(MODULE,IPRIN)                                          55
      IF (IPRIN.LT.0) RETURN                                              55
C Filling bank SSUP - SUPPORT STRUCTURES                                  55
      BANK_TITLE='SUPPORT STRUCTURES'                                     55
      BPATH_SSUP='SSUP*'                                                  55
      IDNUM_SSUP(1)=0                                                     56
C GEOMETRY VERSION                                                        56
      SSUP_VERSION=1                                                      56
      CSSUP_VERSION='GEOMETRY VERSION'                                    56
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            57
      SSUP_CABTHK=0.05                                                    57
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         57
C INNER RADIUS OF WATER HOSES ON SUPPORT CONE                             58
      SSUP_HOSRMN=0.75                                                    58
      CSSUP_HOSRMN='INNER RADIUS OF WATER HOSES ON SUPPORT CONE'          58
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             59
      SSUP_HOSRMX=0.95                                                    59
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          59
C NUMBER OF WATER HOSES                                                   60
      SSUP_NHOSES=10                                                      60
      CSSUP_NHOSES='NUMBER OF WATER HOSES'                                60
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             61
      SSUP_WRPMYTHK=0.10                                                  61
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        61
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   62
      SSUP_WRPALTHK=0.01                                                  62
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              62
C SUPPORT CONE THICKNESS                                                  63
      SSUP_GRPHTHK=0.16                                                   63
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              63
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              64
      SSUP_CONE1ZMN=52.23                                                 64
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         64
C LENGTH OF SUPPORT RODS                                                  65
      SSUP_RODLEN=110.8                                                   65
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               65
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               66
      SSUP_RODDIST=17.5                                                   66
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           66
C ID OF CARBON SUPPORT RODS (APPROX)                                      67
      SSUP_RODID=2.5                                                      67
      CSSUP_RODID='ID OF CARBON SUPPORT RODS (APPROX)'                    67
C OD OF CARBON SUPPORT RODS (APPROX)                                      68
      SSUP_RODOD=3.05                                                     68
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS (APPROX)'                    68
C MINIMUM ID OF CONE 1                                                    69
      SSUP_CON1IDMN=15.67                                                 69
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               69
C MINIMUM ID OF CONE 3 (TBD)                                              70
      SSUP_CON3IDMN=21.67                                                 70
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         70
C MINIMUM ID OF CONE 4 (TBD)                                              71
      SSUP_CON4IDMN=37.4                                                  71
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         71
C MAXIMUM ID OF CONE 4 (TBD)                                              72
      SSUP_CON4IDMX=37.4                                                  72
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         72
C MAXIMUM Z OF CONE 3 (TBD)                                               73
      SSUP_CONE3ZMX=150.0                                                 73
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          73
C MAXIMUM Z OF CONE 4 (TBD)                                               74
      SSUP_CONE4ZMX=229.36                                                74
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          74
C THICKNESS OF AL BRACKETS                                                75
      SSUP_BRATHK=.2                                                      75
      CSSUP_BRATHK='THICKNESS OF AL BRACKETS'                             75
C (Z) THICKNESS OF END RING JOINING BRACKETS                              76
      SSUP_ERJTHK=.1                                                      76
      CSSUP_ERJTHK='(Z) THICKNESS OF END RING JOINING BRACKETS'           76
C (AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS                          77
      SSUP_ERJWID=2.07                                                    77
      CSSUP_ERJWID='(AZIMUTHAL) WIDTH OF END RING JOINING BRACKETS'       77
C (RADIAL) LENGTH OF END RING JOINING BRACKETS                            78
      SSUP_ERJLEN=5.19                                                    78
      CSSUP_ERJLEN='(RADIAL) LENGTH OF END RING JOINING BRACKETS'         78
C DIST FROM LADDER ENDS TO ERJ (GUESS)                                    79
      SSUP_ERJZDIS=2.0                                                    79
      CSSUP_ERJZDIS='DIST FROM LADDER ENDS TO ERJ (GUESS)'                79
C ERJ SCREW 1 X POSITION (RADIAL)                                         80
      SSUP_ERJ1X=0.31                                                     80
      CSSUP_ERJ1X='ERJ SCREW 1 X POSITION (RADIAL)'                       80
C ERJ SCREW 2 X POSITION                                                  81
      SSUP_ERJ2X=1.15                                                     81
      CSSUP_ERJ2X='ERJ SCREW 2 X POSITION'                                81
C ERJ SCREW 2 Y POSITION                                                  82
      SSUP_ERJ2Y=0.72                                                     82
      CSSUP_ERJ2Y='ERJ SCREW 2 Y POSITION'                                82
C DISTANCE OF ERJ CENTER FROM BEAM AXIS                                   83
      SSUP_ERJRAD=10.80                                                   83
      CSSUP_ERJRAD='DISTANCE OF ERJ CENTER FROM BEAM AXIS'                83
C ERJ SCREW DIAMETER                                                      84
      SSUP_ERJDIA=0.17                                                    84
      CSSUP_ERJDIA='ERJ SCREW DIAMETER'                                   84
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    85
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           85
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   85
C *                                                                       85
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    85
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           85
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   85
C *                                                                       86
      CALL AGSBEGM(MODULE,IPRIN)                                          86
      IF (IPRIN.LT.0) RETURN                                              86
C Filling bank SSUP - SUPPORT STRUCTURES                                  86
      BANK_TITLE='SUPPORT STRUCTURES'                                     86
      BPATH_SSUP='SSUP*'                                                  86
      IDNUM_SSUP(1)=0                                                     87
C GEOMETRY VERSION                                                        87
      SSUP_VERSION=2                                                      87
      CSSUP_VERSION='GEOMETRY VERSION'                                    87
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            88
      SSUP_CABTHK=0.21                                                    88
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
      SSUP_VERSION=3                                                      118
      CSSUP_VERSION='GEOMETRY VERSION'                                    118
C LENGTH OF SUPPORT RODS                                                  119
      SSUP_RODLEN=110.8                                                   119
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               119
C DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS                               120
      SSUP_RODDIST=17.5                                                   120
      CSSUP_RODDIST='DISTANCE OF SUPPORT ROD OD FROM BEAM AXIS'           120
C ID OF CARBON SUPPORT RODS                                               121
      SSUP_RODID=3.64                                                     121
      CSSUP_RODID='ID OF CARBON SUPPORT RODS'                             121
C OD OF CARBON SUPPORT RODS                                               122
      SSUP_RODOD=4.50                                                     122
      CSSUP_RODOD='OD OF CARBON SUPPORT RODS'                             122
C ID OF CARBON SUPPORT RODS                                               123
      SSUP_RODIDX=8.72                                                    123
      CSSUP_RODIDX='ID OF CARBON SUPPORT RODS'                            123
C OD OF CARBON SUPPORT RODS                                               124
      SSUP_RODODX=9.58                                                    124
      CSSUP_RODODX='OD OF CARBON SUPPORT RODS'                            124
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
C Filling bank SSUP - SUPPORT STRUCTURES                                  126
      BANK_TITLE='SUPPORT STRUCTURES'                                     126
      BPATH_SSUP='SSUP*'                                                  126
      IDNUM_SSUP(1)=0                                                     127
C GEOMETRY VERSION                                                        127
      SSUP_VERSION=4                                                      127
      CSSUP_VERSION='GEOMETRY VERSION'                                    127
C 0.4MM CARBON FIBER SHELL                                                128
      SSUP_CARBONSHELL=0.04                                               128
      CSSUP_CARBONSHELL='0.4MM CARBON FIBER SHELL'                        128
C 1.78 G/CM^3 IS A TYPICAL CARBON COMPOSITE DENSITY                       129
      SSUP_CARBONDENS=1.78                                                129
      CSSUP_CARBONDENS='1.78 G/CM^3 IS A TYPICAL CARBON COMPOSITE DENSIT  129
     *Y'                                                                  129
C BALLPARK FIGURE                                                         130
      SSUP_NOMEXDENS=0.048                                                130
      CSSUP_NOMEXDENS='BALLPARK FIGURE'                                   130
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    131
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           131
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   131
C *                                                                       131
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    131
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           131
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   131
C *                                                                       132
      CALL AGSBEGM(MODULE,IPRIN)                                          132
      IF (IPRIN.LT.0) RETURN                                              132
C Filling bank SSUB - BEAMPIPE SUPPORT                                    132
      BANK_TITLE='BEAMPIPE SUPPORT'                                       132
      BPATH_SSUB='SSUB*'                                                  132
      IDNUM_SSUB(1)=0                                                     133
C GEOMETRY VERSION                                                        133
      SSUB_VERSION=1                                                      133
      CSSUB_VERSION='GEOMETRY VERSION'                                    133
C ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  134
      SSUB_KMOUNTID=31.34                                                 134
      CSSUB_KMOUNTID='ID OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             134
C OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT                                  135
      SSUB_KMOUNTOD=38.96                                                 135
      CSSUB_KMOUNTOD='OD OF BEAMPIPE SUPPORT KINEMATIC MOUNT'             135
C THICKNESS OF SUPPORT KINEMATIC MOUNT                                    136
      SSUB_KMNTTHK=0.64                                                   136
      CSSUB_KMNTTHK='THICKNESS OF SUPPORT KINEMATIC MOUNT'                136
C OD OF CUTOUT IN KINEMATIC MOUNT                                         137
      SSUB_KMCUTOD=18.31                                                  137
      CSSUB_KMCUTOD='OD OF CUTOUT IN KINEMATIC MOUNT'                     137
C ID OF CUTOUT IN KINEMATIC MOUNT                                         138
      SSUB_KMCUTID=14                                                     138
      CSSUB_KMCUTID='ID OF CUTOUT IN KINEMATIC MOUNT'                     138
C OPENING ANGLE OF CUTOUT                                                 139
      SSUB_KMCUTOA=38                                                     139
      CSSUB_KMCUTOA='OPENING ANGLE OF CUTOUT'                             139
C OFFSET OF CUTOUT CENTER FROM AXIS                                       140
      SSUB_KMCUTOFF=26.58                                                 140
      CSSUB_KMCUTOFF='OFFSET OF CUTOUT CENTER FROM AXIS'                  140
C ID OF BEAMPIPE SUPPORT RING                                             141
      SSUB_SRINGID=8.47                                                   141
      CSSUB_SRINGID='ID OF BEAMPIPE SUPPORT RING'                         141
C OD OF BEAMPIPE SUPPORT RING                                             142
      SSUB_SRINGOD=12.78                                                  142
      CSSUB_SRINGOD='OD OF BEAMPIPE SUPPORT RING'                         142
C THICHKNESS OF BEAMPIPE SUPPORT RING                                     143
      SSUB_SRINGTHK=0.64                                                  143
      CSSUB_SRINGTHK='THICHKNESS OF BEAMPIPE SUPPORT RING'                143
C SUPPORT RING CUTOUT ANGLE TO Z-AXIS                                     144
      SSUB_SRCUTPHI=38                                                    144
      CSSUB_SRCUTPHI='SUPPORT RING CUTOUT ANGLE TO Z-AXIS'                144
C SUPPORT RING CUTOUT WIDTH                                               145
      SSUB_SRCUTWID=3.63                                                  145
      CSSUB_SRCUTWID='SUPPORT RING CUTOUT WIDTH'                          145
C SUPPORT RING CUTOUT DEPTH                                               146
      SSUB_SRCUTOUT=5.08                                                  146
      CSSUB_SRCUTOUT='SUPPORT RING CUTOUT DEPTH'                          146
C SUPPORT RING CUTOUT START                                               147
      SSUB_SRCUTIN=3.5                                                    147
      CSSUB_SRCUTIN='SUPPORT RING CUTOUT START'                           147
C SUPPORT ROLLER ID                                                       148
      SSUB_SROLLID=0.2                                                    148
      CSSUB_SROLLID='SUPPORT ROLLER ID'                                   148
C SUPPORT ROLLER OD                                                       149
      SSUB_SROLLOD=0.62                                                   149
      CSSUB_SROLLOD='SUPPORT ROLLER OD'                                   149
C SUPPORT ROLLER LENGTH                                                   150
      SSUB_SROLLLEN=2.54                                                  150
      CSSUB_SROLLLEN='SUPPORT ROLLER LENGTH'                              150
C SUPPORT ROLLER AXIS LENGTH                                              151
      SSUB_SWIRELEN=5.08                                                  151
      CSSUB_SWIRELEN='SUPPORT ROLLER AXIS LENGTH'                         151
C MOUNTING BLOCK HEIGHT                                                   152
      SSUB_MBLKHGH=0.97                                                   152
      CSSUB_MBLKHGH='MOUNTING BLOCK HEIGHT'                               152
C OUTER MOUNTING BLOCK WIDTH                                              153
      SSUB_MBLKOWID=2.54                                                  153
      CSSUB_MBLKOWID='OUTER MOUNTING BLOCK WIDTH'                         153
C OUTER MOUNTING BLOCK LENGTH                                             154
      SSUB_MBLKOLEN=1.27                                                  154
      CSSUB_MBLKOLEN='OUTER MOUNTING BLOCK LENGTH'                        154
C INNER MOUNTING BLOCK WIDTH                                              155
      SSUB_MBLKIWID=3.175                                                 155
      CSSUB_MBLKIWID='INNER MOUNTING BLOCK WIDTH'                         155
C INNER MOUNTING BLOCK LENGTH                                             156
      SSUB_MBLKILEN=1.27                                                  156
      CSSUB_MBLKILEN='INNER MOUNTING BLOCK LENGTH'                        156
C OUTER MOUNTING BLOCK AT RADIUS                                          157
      SSUB_MBLKORAD=17.4                                                  157
      CSSUB_MBLKORAD='OUTER MOUNTING BLOCK AT RADIUS'                     157
C INNER MOUNTING BLOCK AT RADIUS                                          158
      SSUB_MBLKIRAD=5.42                                                  158
      CSSUB_MBLKIRAD='INNER MOUNTING BLOCK AT RADIUS'                     158
C MOUNTING SUPPORT ROD DIAMETER                                           159
      SSUB_MRODDIA=0.32                                                   159
      CSSUB_MRODDIA='MOUNTING SUPPORT ROD DIAMETER'                       159
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    160
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           160
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   160
C *                                                                       160
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUB',    160
     *BPATH_SSUB,IDNUM_SSUB,1, MAP_SSUB,NAMES_SSUB,COMMEN_SSUB,           160
     *BANK_SSUB,LEN_SSUB,LLEN_SSUB,LINK_SSUB,FLAG_SSUB)                   160
C *                                                                       161
      DO 5011 I=1,4                                                       161
         RAD_CONES_IN(I) = 8.5+2.60*(I-1)                                 162
         RAD_CONES_OUT(I)=15.0+0.85*(I-1)                                 163
5011  CONTINUE                                                            164
5012  CONTINUE                                                            164
C use default SVTG bank                                                   165
      BPATH_SVTG='SVTG*'                                                  165
      IDNUM_SVTG(1)=0                                                     165
      CALL AGDGETP('SCONGEO','*',-999,'SVTG',' ',BANK_SVTG, BPATH_SVTG,   165
     *IDNUM_SVTG,1,MAP_SVTG,NAMES_SVTG,BANK_SVTG,LEN_SVTG,LLEN_SVTG,      165
     *LINK_SVTG,FLAG_SVTG)                                                165
C use SSUP bank                                                           166
      BPATH_SSUP='SSUP*'                                                  166
      IDNUM_SSUP(1)=0                                                     166
      SSUP_VERSION=SVTG_CONEVER                                           166
      CALL AGDGETP('SCONGEO','*',-999,'SSUP',NSSUP_VERSION,               166
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        166
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   166
C use default SSUB bank                                                   167
      BPATH_SSUB='SSUB*'                                                  167
      IDNUM_SSUB(1)=0                                                     167
      CALL AGDGETP('SCONGEO','*',-999,'SSUB',' ',BANK_SSUB, BPATH_SSUB,   167
     *IDNUM_SSUB,1,MAP_SSUB,NAMES_SSUB,BANK_SSUB,LEN_SSUB,LLEN_SSUB,      167
     *LINK_SSUB,FLAG_SSUB)                                                167
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             168
      CALL AGSRESET                                                       168
      AG_A = 28.08                                                        168
      AG_Z = 14                                                           168
      AG_W = 0.6*1*28./60.                                                168
      AG_TITLE = 'SI'                                                     168
      AG_EXNAME = 'COMPONENT'                                             168
      AG_PARLIST = 'A_Z_W'                                                168
      CALL AXCOMPONENT                                                    168
C *                                                                       168
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  168
C =                                                                       168
      CALL AGSRESET                                                       169
      AG_A = 16                                                           169
      AG_Z = 8                                                            169
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               169
      AG_TITLE = 'O'                                                      169
      AG_EXNAME = 'COMPONENT'                                             169
      AG_PARLIST = 'A_Z_W'                                                169
      CALL AXCOMPONENT                                                    169
C *                                                                       169
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 169
      CALL AGSRESET                                                       170
      AG_A = 12                                                           170
      AG_Z = 6                                                            170
      AG_W = 0.4*8*12./174.                                               170
      AG_TITLE = 'C'                                                      170
      AG_EXNAME = 'COMPONENT'                                             170
      AG_PARLIST = 'A_Z_W'                                                170
      CALL AXCOMPONENT                                                    170
C *                                                                       170
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  170
      CALL AGSRESET                                                       171
      AG_A = 1                                                            171
      AG_Z = 1                                                            171
      AG_W = 0.4*14*1./174.                                               171
      AG_TITLE = 'H'                                                      171
      AG_EXNAME = 'COMPONENT'                                             171
      AG_PARLIST = 'A_Z_W'                                                171
      CALL AXCOMPONENT                                                    171
C *                                                                       171
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  171
      CALL AGSRESET                                                       172
      AG_DENS = 1.7                                                       172
      AG_TITLE = 'G10'                                                    172
      AG_EXNAME = 'MIXTURE'                                               172
      AG_PARLIST = 'DENS'                                                 172
      CALL AXMIXTURE                                                      172
C *                                                                       172
      CALL AGSBEGM(MODULE,IPRIN)                                          172
C Check IPRIN<0                                                           173
      IF (IPRIN.LT.0) RETURN                                              173
C Create SCON :                                                           173
C Call To Local Procedure SCON                                            173
      ASSIGN 5030 TO BLOCK_SCON                                           173
      GO TO  5020                                                         173
5030  CONTINUE                                                            173
C *                                                                       173
C Geant POSITION : SCON ===> MOTHER='CAVE' <===                           174
      CALL AGSRESET                                                       174
      AG_MOTHER = 'CAVE'                                                  174
      AG_TITLE = 'SCON'                                                   174
      AG_EXNAME = 'POSITION'                                              174
      AG_PARLIST = 'MOTHER'                                               174
      CALL AXPOSITION                                                     174
C *                                                                       174
      CALL AGSBEGM(MODULE,IPRIN)                                          174
C Check IPRIN<0                                                           175
      IF (IPRIN.LT.0) RETURN                                              175
C Create SCON :                                                           175
C Call To Local Procedure SCON                                            175
      ASSIGN 5040 TO BLOCK_SCON                                           175
      GO TO  5020                                                         175
5040  CONTINUE                                                            175
C *                                                                       175
C Geant POSITION : SCON ===> MOTHER='CAVE' THETAZ=180 <===                176
      CALL AGSRESET                                                       176
      AG_MOTHER = 'CAVE'                                                  176
      AG_THETAZ = 180                                                     176
      AG_TITLE = 'SCON'                                                   176
      AG_EXNAME = 'POSITION'                                              176
      AG_PARLIST = 'MOTHER_THETAZ'                                        176
      CALL AXPOSITION                                                     176
C *                                                                       176
      CALL AGSBEGM(MODULE,IPRIN)                                          176
C Check IPRIN<0                                                           177
      IF (IPRIN.LT.0) RETURN                                              177
C Create SROD :                                                           177
C Call To Local Procedure SROD                                            177
      ASSIGN 5060 TO BLOCK_SROD                                           177
      GO TO  5050                                                         177
5060  CONTINUE                                                            177
C *                                                                       177
C Geant POSITION : SROD ===> Y=SSUP_RODDIST+SSUP_RODOD/2 <===             178
      CALL AGSRESET                                                       178
      AG_Y = SSUP_RODDIST+SSUP_RODOD/2                                    178
      AG_TITLE = 'SROD'                                                   178
      AG_EXNAME = 'POSITION'                                              178
      AG_PARLIST = 'Y'                                                    178
      CALL AXPOSITION                                                     178
C *                                                                       178
C Geant POSITION : SROD ===> Y=-SSUP_RODDIST-SSUP_RODOD/2 <===            178
      CALL AGSRESET                                                       179
      AG_Y = -SSUP_RODDIST-SSUP_RODOD/2                                   179
      AG_TITLE = 'SROD'                                                   179
      AG_EXNAME = 'POSITION'                                              179
      AG_PARLIST = 'Y'                                                    179
      CALL AXPOSITION                                                     179
C *                                                                       179
      CALL AGSBEGM(MODULE,IPRIN)                                          179
C Check IPRIN<0                                                           180
      IF (IPRIN.LT.0) RETURN                                              180
C Create SBSP :                                                           180
C Call To Local Procedure SBSP                                            180
      ASSIGN 5080 TO BLOCK_SBSP                                           180
      GO TO  5070                                                         180
5080  CONTINUE                                                            180
C *                                                                       180
C Geant POSITION : SBSP ===> MOTHER='CAVE' Z=(SSUP_RODLEN/2-SSUB_KMNTTHK  181
C /2) <===                                                                181
      CALL AGSRESET                                                       181
      AG_MOTHER = 'CAVE'                                                  181
      AG_Z = (SSUP_RODLEN/2-SSUB_KMNTTHK/2)                               181
      AG_TITLE = 'SBSP'                                                   181
      AG_EXNAME = 'POSITION'                                              181
      AG_PARLIST = 'MOTHER_Z'                                             181
      CALL AXPOSITION                                                     181
C *                                                                       181
      CALL AGSBEGM(MODULE,IPRIN)                                          181
C Check IPRIN<0                                                           182
      IF (IPRIN.LT.0) RETURN                                              182
C Create SBSP :                                                           182
C Call To Local Procedure SBSP                                            182
      ASSIGN 5090 TO BLOCK_SBSP                                           182
      GO TO  5070                                                         182
5090  CONTINUE                                                            182
C *                                                                       182
C Geant POSITION : SBSP ===> MOTHER='CAVE' Z=-(SSUP_RODLEN/2-SSUB_KMNTTH  183
C K/2) THETAZ=180 <===                                                    183
      CALL AGSRESET                                                       183
      AG_MOTHER = 'CAVE'                                                  183
      AG_Z = -(SSUP_RODLEN/2-SSUB_KMNTTHK/2)                              183
      AG_THETAZ = 180                                                     183
      AG_TITLE = 'SBSP'                                                   183
      AG_EXNAME = 'POSITION'                                              183
      AG_PARLIST = 'MOTHER_Z_THETAZ'                                      183
      CALL AXPOSITION                                                     183
C *                                                                       183
C ----------------------------------------------------------------------  183
C ------------ SCON                                                       183
      CALL AGSENDM                                                        185
      RETURN                                                              185
C Begin Local Procedure SCON                                              185
C SCON IS THE SILICON TRACKER SUPPORTING CONE MOTHER VOLUME               185
5020  CONTINUE                                                            185
         AG_TITLE='SCON'                                                  185
         AG_MODULE=MODULE                                                 185
         AG_MARK='V00'                                                    185
         CALL AGSPUSH                                                     185
         AG_MODULE=MODULE                                                 185
C    *                                                                    185
         CONE_THK1=SSUP_GRPHTHK                                           186
         CONE_THK2=CONE_THK1 + SSUP_CABTHK + 2*SSUP_HOSRMX                187
         CONE_THK2=CONE_THK2 + SSUP_WRPMYTHK + SSUP_WRPALTHK              188
C    Geant MATERIAL : AIR ===> inherited arguments used <===              189
         CALL AGSRESET                                                    189
         AG_TITLE = 'AIR'                                                 189
         AG_EXNAME = 'MATERIAL'                                           189
         AG_PARLIST = 'NONE'                                              189
         CALL AXMATERIAL                                                  189
C    *                                                                    189
C    Geant ATTRIBUTE : SCON ===> SEEN=0 COLO=1 <===                       189
         CALL AGSRESET                                                    190
         AG_SEEN = 0                                                      190
         AG_COLO = 1                                                      190
         AG_TITLE = 'SCON'                                                190
         AG_EXNAME = 'ATTRIBUTE'                                          190
         AG_PARLIST = 'SEEN_COLO'                                         190
         CALL AXATTRIBUTE                                                 190
C    *                                                                    190
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7 ZI={SSUP_CONE1ZMN, SSU  190
C    P_RODLEN/2,SSUP_RODLEN/2,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+  190
C    SSUP_GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX} RMN={SSUP_CON1IDMN, SSUP  190
C    _CON1IDMN,SSUP_CON1IDMN,SSUP_CON1IDMN, SSUP_CON3IDMN,SSUP_CON4IDMN,  190
C    SSUP_CON4IDMX}, RMX={SSUP_CON1IDMN+CONE_THK1,SSUP_CON1IDMN+CONE_THK  190
C    1, SSUP_CON3IDMN+CONE_THK1,SSUP_CON3IDMN+CONE_THK1, SSUP_CON3IDMN+C  190
C    ONE_THK2,SSUP_CON4IDMN+CONE_THK2, SSUP_CON4IDMX+CONE_THK2} <===      190
         CALL AGSRESET                                                    200
         AG_PHI1 = 0                                                      200
         AG_DPHI = 360                                                    200
         AG_NZ = 7                                                        200
         AG_ZI(1)=SSUP_CONE1ZMN                                           200
         AG_ZI(2)=SSUP_RODLEN/2                                           200
         AG_ZI(3)=SSUP_RODLEN/2                                           200
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              200
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              200
         AG_ZI(6)=SSUP_CONE3ZMX                                           200
         AG_ZI(7)=SSUP_CONE4ZMX                                           200
         AG_RMN(1)=SSUP_CON1IDMN                                          200
         AG_RMN(2)=SSUP_CON1IDMN                                          200
         AG_RMN(3)=SSUP_CON1IDMN                                          200
         AG_RMN(4)=SSUP_CON1IDMN                                          200
         AG_RMN(5)=SSUP_CON3IDMN                                          200
         AG_RMN(6)=SSUP_CON4IDMN                                          200
         AG_RMN(7)=SSUP_CON4IDMX                                          200
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                200
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                200
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                200
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                200
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                200
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                200
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                200
         AG_TITLE = 'PCON'                                                200
         AG_EXNAME = 'SHAPE'                                              200
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           200
         CALL AXSHAPE                                                     200
C    *                                                                    200
C    Create SGRA :                                                        201
C    Call To Local Procedure SGRA                                         201
         ASSIGN 5110 TO BLOCK_SGRA                                        201
         GO TO  5100                                                      201
5110     CONTINUE                                                         201
C    *                                                                    201
C    Geant POSITION : SGRA ===> inherited arguments used <===             202
         CALL AGSRESET                                                    202
         AG_TITLE = 'SGRA'                                                202
         AG_EXNAME = 'POSITION'                                           202
         AG_PARLIST = 'NONE'                                              202
         CALL AXPOSITION                                                  202
C    *                                                                    202
C    Create SCMY :                                                        203
C    Call To Local Procedure SCMY                                         203
         ASSIGN 5130 TO BLOCK_SCMY                                        203
         GO TO  5120                                                      203
5130     CONTINUE                                                         203
C    *                                                                    203
C    Geant POSITION : SCMY ===> inherited arguments used <===             204
         CALL AGSRESET                                                    204
         AG_TITLE = 'SCMY'                                                204
         AG_EXNAME = 'POSITION'                                           204
         AG_PARLIST = 'NONE'                                              204
         CALL AXPOSITION                                                  204
C    *                                                                    204
5021  CALL AGSPOPB                                                        205
      GO TO BLOCK_SCON, (5020 ,5030,5040)                                 205
C End of Local Procedure                                                  205
C ----------------------------------------------------------------------  206
C ------------ SGRA                                                       206
C Begin Local Procedure SGRA                                              207
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 207
5100  CONTINUE                                                            207
         AG_TITLE='SGRA'                                                  207
         AG_MODULE=MODULE                                                 207
         AG_MARK='V00'                                                    207
         CALL AGSPUSH                                                     207
         AG_MODULE=MODULE                                                 207
C    *                                                                    207
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           208
         CALL AGSRESET                                                    208
         AG_TITLE = 'CARBON'                                              208
         AG_EXNAME = 'MATERIAL'                                           208
         AG_PARLIST = 'NONE'                                              208
         CALL AXMATERIAL                                                  208
C    *                                                                    208
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       208
         CALL AGSRESET                                                    209
         AG_SEEN = 1                                                      209
         AG_COLO = 6                                                      209
         AG_TITLE = 'SGRA'                                                209
         AG_EXNAME = 'ATTRIBUTE'                                          209
         AG_PARLIST = 'SEEN_COLO'                                         209
         CALL AXATTRIBUTE                                                 209
C    *                                                                    209
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={SSUP_RODLEN/2, SSU  209
C    P_RODLEN/2+SSUP_GRPHTHK,SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_CONE3ZMX,S  209
C    SUP_CONE4ZMX}, RMN={SSUP_CON1IDMN,SSUP_CON1IDMN,SSUP_CON3IDMN, SSUP  209
C    _CON4IDMN,SSUP_CON4IDMX} RMX={SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3  209
C    IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK,SSUP_CON4IDMN+SSUP_GR  209
C    PHTHK, SSUP_CON4IDMX+SSUP_GRPHTHK} <===                              209
         CALL AGSRESET                                                    217
         AG_PHI1 = 0                                                      217
         AG_DPHI = 360                                                    217
         AG_NZ = 5                                                        217
         AG_ZI(1)=SSUP_RODLEN/2                                           217
         AG_ZI(2)=SSUP_RODLEN/2+SSUP_GRPHTHK                              217
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              217
         AG_ZI(4)=SSUP_CONE3ZMX                                           217
         AG_ZI(5)=SSUP_CONE4ZMX                                           217
         AG_RMN(1)=SSUP_CON1IDMN                                          217
         AG_RMN(2)=SSUP_CON1IDMN                                          217
         AG_RMN(3)=SSUP_CON3IDMN                                          217
         AG_RMN(4)=SSUP_CON4IDMN                                          217
         AG_RMN(5)=SSUP_CON4IDMX                                          217
         AG_RMX(1)=SSUP_CON3IDMN+SSUP_GRPHTHK                             217
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             217
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             217
         AG_RMX(4)=SSUP_CON4IDMN+SSUP_GRPHTHK                             217
         AG_RMX(5)=SSUP_CON4IDMX+SSUP_GRPHTHK                             217
         AG_TITLE = 'PCON'                                                217
         AG_EXNAME = 'SHAPE'                                              217
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           217
         CALL AXSHAPE                                                     217
C    *                                                                    217
5101  CALL AGSPOPB                                                        218
      GO TO BLOCK_SGRA, (5100 ,5110)                                      218
C End of Local Procedure                                                  218
C ----------------------------------------------------------------------  219
C ------------ SBSP                                                       219
C Begin Local Procedure SBSP                                              220
C SBSP IS THE BEAMPIPE SUPPORT MOTHER VOLUME                              220
5070  CONTINUE                                                            220
         AG_TITLE='SBSP'                                                  220
         AG_MODULE=MODULE                                                 220
         AG_MARK='V00'                                                    220
         CALL AGSPUSH                                                     220
         AG_MODULE=MODULE                                                 220
C    *                                                                    220
C    Geant MATERIAL : AIR ===> inherited arguments used <===              221
         CALL AGSRESET                                                    221
         AG_TITLE = 'AIR'                                                 221
         AG_EXNAME = 'MATERIAL'                                           221
         AG_PARLIST = 'NONE'                                              221
         CALL AXMATERIAL                                                  221
C    *                                                                    221
C    Geant ATTRIBUTE : SBSP ===> SEEN=0 COLO=1 <===                       221
         CALL AGSRESET                                                    222
         AG_SEEN = 0                                                      222
         AG_COLO = 1                                                      222
         AG_TITLE = 'SBSP'                                                222
         AG_EXNAME = 'ATTRIBUTE'                                          222
         AG_PARLIST = 'SEEN_COLO'                                         222
         CALL AXATTRIBUTE                                                 222
C    *                                                                    222
C    Geant SHAPE : TUBE ===> RMIN=SVTG_RSIZEMIN RMAX=SSUB_KMOUNTOD/2, DZ  222
C    =SSUB_KMNTTHK/2+SSUB_MBLKHGH <===                                    222
         CALL AGSRESET                                                    224
         AG_RMIN = SVTG_RSIZEMIN                                          224
         AG_RMAX = SSUB_KMOUNTOD/2                                        224
         AG_DZ = SSUB_KMNTTHK/2+SSUB_MBLKHGH                              224
         AG_TITLE = 'TUBE'                                                224
         AG_EXNAME = 'SHAPE'                                              224
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      224
         CALL AXSHAPE                                                     224
C    *                                                                    224
C    Create SAKM :                                                        225
C    Call To Local Procedure SAKM                                         225
         ASSIGN 5150 TO BLOCK_SAKM                                        225
         GO TO  5140                                                      225
5150     CONTINUE                                                         225
C    *                                                                    225
C    Geant POSITION : SAKM ===> inherited arguments used <===             226
         CALL AGSRESET                                                    226
         AG_TITLE = 'SAKM'                                                226
         AG_EXNAME = 'POSITION'                                           226
         AG_PARLIST = 'NONE'                                              226
         CALL AXPOSITION                                                  226
C    *                                                                    226
C    Create SBMM :                                                        227
C    Call To Local Procedure SBMM                                         227
         ASSIGN 5170 TO BLOCK_SBMM                                        227
         GO TO  5160                                                      227
5170     CONTINUE                                                         227
C    *                                                                    227
C    Create SBRL :                                                        228
C    Call To Local Procedure SBRL                                         228
         ASSIGN 5190 TO BLOCK_SBRL                                        228
         GO TO  5180                                                      228
5190     CONTINUE                                                         228
C    *                                                                    228
C    Create SBRX :                                                        229
C    Call To Local Procedure SBRX                                         229
         ASSIGN 5210 TO BLOCK_SBRX                                        229
         GO TO  5200                                                      229
5210     CONTINUE                                                         229
C    *                                                                    229
C    *                                                                    230
         I=-1                                                             230
            GOTO 5223                                                     230
5221        I=I+(2)                                                       230
5223        IF((2)*(I-(1)).GT.0) GO TO 5222                               230
C       *                                                                 231
            J=0                                                           231
               GO TO 5233                                                 231
5231           J=J+1                                                      231
5233           IF(J-(1).GT.0) GO TO 5232                                  231
               PHI=I*SSUB_SRCUTPHI+180*J                                  232
               XBUF1=(SSUB_KMOUNTOD/4.0+(SVTG_RSIZEMIN+SSUB_SROLLOD)/     233
     *         2.0)                                                       233
C          Geant POSITION : SBMM ===> X=XBUF1*SIN(DEGRAD*PHI) Y=-XBUF1*C  235
C          OS(DEGRAD*PHI) Z=-SSUB_KMNTTHK/2-SSUB_MBLKHGH/2 ALPHAZ=PHI <=  235
C          ==                                                             235
               CALL AGSRESET                                              235
               AG_X = XBUF1*SIN(DEGRAD*PHI)                               235
               AG_Y = -XBUF1*COS(DEGRAD*PHI)                              235
               AG_Z = -SSUB_KMNTTHK/2-SSUB_MBLKHGH/2                      235
               AG_ALPHAN=AG_ALPHAN+1                                      235
               AG_ALPHAK(AG_ALPHAN)=3                                     235
               AG_ALPHAL(AG_ALPHAN)=PHI                                   235
               AG_TITLE = 'SBMM'                                          235
               AG_EXNAME = 'POSITION'                                     235
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                235
               CALL AXPOSITION                                            235
C          *                                                              235
               XBUF2=SVTG_RSIZEMIN+SSUB_SROLLOD/2                         235
C          Geant POSITION : SBRL ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  238
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  238
C           ORT=ZXY <===                                                  238
               CALL AGSRESET                                              238
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               238
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              238
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      238
               AG_ALPHAN=AG_ALPHAN+1                                      238
               AG_ALPHAK(AG_ALPHAN)=3                                     238
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                238
               AG_ALPHAN=AG_ALPHAN+1                                      238
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           238
               AG_TITLE = 'SBRL'                                          238
               AG_EXNAME = 'POSITION'                                     238
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            238
               CALL AXPOSITION                                            238
C          *                                                              238
C          Geant POSITION : SBRX ===> X=XBUF2*SIN(DEGRAD*PHI) Y=-XBUF2*C  238
C          OS(DEGRAD*PHI) Z=SSUB_SRINGTHK/2+SSUB_SROLLID/2 ALPHAZ=PHI-90  238
C           ORT=ZXY <===                                                  238
               CALL AGSRESET                                              240
               AG_X = XBUF2*SIN(DEGRAD*PHI)                               240
               AG_Y = -XBUF2*COS(DEGRAD*PHI)                              240
               AG_Z = SSUB_SRINGTHK/2+SSUB_SROLLID/2                      240
               AG_ALPHAN=AG_ALPHAN+1                                      240
               AG_ALPHAK(AG_ALPHAN)=3                                     240
               AG_ALPHAL(AG_ALPHAN)=PHI-90                                240
               AG_ALPHAN=AG_ALPHAN+1                                      240
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           240
               AG_TITLE = 'SBRX'                                          240
               AG_EXNAME = 'POSITION'                                     240
               AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                            240
               CALL AXPOSITION                                            240
C          *                                                              240
            GO TO 5231                                                    241
5232        CONTINUE                                                      241
         GO TO 5221                                                       242
5222     CONTINUE                                                         242
C    Create SBSR :                                                        243
C    Call To Local Procedure SBSR                                         243
         ASSIGN 5250 TO BLOCK_SBSR                                        243
         GO TO  5240                                                      243
5250     CONTINUE                                                         243
C    *                                                                    243
C    Geant POSITION : SBSR ===> inherited arguments used <===             244
         CALL AGSRESET                                                    244
         AG_TITLE = 'SBSR'                                                244
         AG_EXNAME = 'POSITION'                                           244
         AG_PARLIST = 'NONE'                                              244
         CALL AXPOSITION                                                  244
C    *                                                                    244
5071  CALL AGSPOPB                                                        245
      GO TO BLOCK_SBSP, (5070 ,5080,5090)                                 245
C End of Local Procedure                                                  245
C ----------------------------------------------------------------------  246
C ------------ SBMM                                                       246
C Begin Local Procedure SBMM                                              247
C SBMM IS THE BEAMPIPE SUPPORT MOUNTING MOTHER VOLUME                     247
5160  CONTINUE                                                            247
         AG_TITLE='SBMM'                                                  247
         AG_MODULE=MODULE                                                 247
         AG_MARK='V00'                                                    247
         CALL AGSPUSH                                                     247
         AG_MODULE=MODULE                                                 247
C    *                                                                    247
C    Geant MATERIAL : AIR ===> inherited arguments used <===              248
         CALL AGSRESET                                                    248
         AG_TITLE = 'AIR'                                                 248
         AG_EXNAME = 'MATERIAL'                                           248
         AG_PARLIST = 'NONE'                                              248
         CALL AXMATERIAL                                                  248
C    *                                                                    248
C    Geant ATTRIBUTE : SBMM ===> SEEN=0 COLO=1 <===                       248
         CALL AGSRESET                                                    249
         AG_SEEN = 0                                                      249
         AG_COLO = 1                                                      249
         AG_TITLE = 'SBMM'                                                249
         AG_EXNAME = 'ATTRIBUTE'                                          249
         AG_PARLIST = 'SEEN_COLO'                                         249
         CALL AXATTRIBUTE                                                 249
C    *                                                                    249
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=(SSUB_KMOUNTOD/2-SVTG_  249
C    RSIZEMIN-SSUB_SROLLOD)/2, DZ=SSUB_MBLKHGH/2 <===                     249
         CALL AGSRESET                                                    251
         AG_DX = SSUB_MBLKIWID/2                                          251
         AG_DY = (SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2           251
         AG_DZ = SSUB_MBLKHGH/2                                           251
         AG_TITLE = 'BOX'                                                 251
         AG_EXNAME = 'SHAPE'                                              251
         AG_PARLIST = 'DX_DY_DZ'                                          251
         CALL AXSHAPE                                                     251
C    *                                                                    251
C    Create SMRD :                                                        252
C    Call To Local Procedure SMRD                                         252
         ASSIGN 5270 TO BLOCK_SMRD                                        252
         GO TO  5260                                                      252
5270     CONTINUE                                                         252
C    *                                                                    252
         XBUF= - (SSUB_MBLKORAD + SSUB_MBLKIRAD)/2 + SVTG_RSIZEMIN +      253
     *   SSUB_SROLLOD                                                     253
C    Geant POSITION : SMRD ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  254
C    UB_SROLLOD)/2 ALPHAX=90 <===                                         254
         CALL AGSRESET                                                    254
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       254
         AG_ALPHAN=AG_ALPHAN+1                                            254
         AG_ALPHAK(AG_ALPHAN)=1                                           254
         AG_ALPHAL(AG_ALPHAN)=90                                          254
         AG_TITLE = 'SMRD'                                                254
         AG_EXNAME = 'POSITION'                                           254
         AG_PARLIST = 'Y_ALPHAX'                                          254
         CALL AXPOSITION                                                  254
C    *                                                                    254
C    Create SBMO :                                                        255
C    Call To Local Procedure SBMO                                         255
         ASSIGN 5290 TO BLOCK_SBMO                                        255
         GO TO  5280                                                      255
5290     CONTINUE                                                         255
C    *                                                                    255
         XBUF= - SSUB_MBLKORAD + SVTG_RSIZEMIN + SSUB_SROLLOD             256
C    Geant POSITION : SBMO ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  257
C    UB_SROLLOD)/2 <===                                                   257
         CALL AGSRESET                                                    257
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       257
         AG_TITLE = 'SBMO'                                                257
         AG_EXNAME = 'POSITION'                                           257
         AG_PARLIST = 'Y'                                                 257
         CALL AXPOSITION                                                  257
C    *                                                                    257
C    Create SBMI :                                                        258
C    Call To Local Procedure SBMI                                         258
         ASSIGN 5310 TO BLOCK_SBMI                                        258
         GO TO  5300                                                      258
5310     CONTINUE                                                         258
C    *                                                                    258
         XBUF= - SSUB_MBLKIRAD + SVTG_RSIZEMIN + SSUB_SROLLOD             259
C    Geant POSITION : SBMI ===> Y=XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SS  260
C    UB_SROLLOD)/2 <===                                                   260
         CALL AGSRESET                                                    260
         AG_Y = XBUF+(SSUB_KMOUNTOD/2-SVTG_RSIZEMIN-SSUB_SROLLOD)/2       260
         AG_TITLE = 'SBMI'                                                260
         AG_EXNAME = 'POSITION'                                           260
         AG_PARLIST = 'Y'                                                 260
         CALL AXPOSITION                                                  260
C    *                                                                    260
5161  CALL AGSPOPB                                                        261
      GO TO BLOCK_SBMM, (5160 ,5170)                                      261
C End of Local Procedure                                                  261
C ----------------------------------------------------------------------  262
C ------------ SMRD                                                       262
C Begin Local Procedure SMRD                                              263
C SMRD IS THE ALUMINUM ROD CARRYING THE BEAMPIPE SUPPORT                  263
5260  CONTINUE                                                            263
         AG_TITLE='SMRD'                                                  263
         AG_MODULE=MODULE                                                 263
         AG_MARK='V00'                                                    263
         CALL AGSPUSH                                                     263
         AG_MODULE=MODULE                                                 263
C    *                                                                    263
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        264
         CALL AGSRESET                                                    264
         AG_TITLE = 'ALUMINIUM'                                           264
         AG_EXNAME = 'MATERIAL'                                           264
         AG_PARLIST = 'NONE'                                              264
         CALL AXMATERIAL                                                  264
C    *                                                                    264
C    Geant ATTRIBUTE : SMRD ===> SEEN=1 COLO=2 <===                       264
         CALL AGSRESET                                                    265
         AG_SEEN = 1                                                      265
         AG_COLO = 2                                                      265
         AG_TITLE = 'SMRD'                                                265
         AG_EXNAME = 'ATTRIBUTE'                                          265
         AG_PARLIST = 'SEEN_COLO'                                         265
         CALL AXATTRIBUTE                                                 265
C    *                                                                    265
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_MRODDIA/2, DZ=(SSUB_MBLK  265
C    ORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2 <===                             265
         CALL AGSRESET                                                    267
         AG_RMIN = 0.0                                                    267
         AG_RMAX = SSUB_MRODDIA/2                                         267
         AG_DZ = (SSUB_MBLKORAD-SSUB_MBLKIRAD+SSUB_MBLKOLEN)/2            267
         AG_TITLE = 'TUBE'                                                267
         AG_EXNAME = 'SHAPE'                                              267
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      267
         CALL AXSHAPE                                                     267
C    *                                                                    267
5261  CALL AGSPOPB                                                        268
      GO TO BLOCK_SMRD, (5260 ,5270)                                      268
C End of Local Procedure                                                  268
C ----------------------------------------------------------------------  269
C ------------ SBMO                                                       269
C Begin Local Procedure SBMO                                              270
C SBMO IS THE OUTER BEAMPIPE SUPPORT MOUNTING BLOCK                       270
5280  CONTINUE                                                            270
         AG_TITLE='SBMO'                                                  270
         AG_MODULE=MODULE                                                 270
         AG_MARK='V00'                                                    270
         CALL AGSPUSH                                                     270
         AG_MODULE=MODULE                                                 270
C    *                                                                    270
C    Geant MATERIAL : G10 ===> inherited arguments used <===              271
         CALL AGSRESET                                                    271
         AG_TITLE = 'G10'                                                 271
         AG_EXNAME = 'MATERIAL'                                           271
         AG_PARLIST = 'NONE'                                              271
         CALL AXMATERIAL                                                  271
C    *                                                                    271
C    Geant ATTRIBUTE : SBMO ===> SEEN=1 COLO=3 <===                       271
         CALL AGSRESET                                                    272
         AG_SEEN = 1                                                      272
         AG_COLO = 3                                                      272
         AG_TITLE = 'SBMO'                                                272
         AG_EXNAME = 'ATTRIBUTE'                                          272
         AG_PARLIST = 'SEEN_COLO'                                         272
         CALL AXATTRIBUTE                                                 272
C    *                                                                    272
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKOWID/2 DY=SSUB_MBLKOLEN/2, DZ=SS  272
C    UB_MBLKHGH/2 <===                                                    272
         CALL AGSRESET                                                    274
         AG_DX = SSUB_MBLKOWID/2                                          274
         AG_DY = SSUB_MBLKOLEN/2                                          274
         AG_DZ = SSUB_MBLKHGH/2                                           274
         AG_TITLE = 'BOX'                                                 274
         AG_EXNAME = 'SHAPE'                                              274
         AG_PARLIST = 'DX_DY_DZ'                                          274
         CALL AXSHAPE                                                     274
C    *                                                                    274
5281  CALL AGSPOPB                                                        275
      GO TO BLOCK_SBMO, (5280 ,5290)                                      275
C End of Local Procedure                                                  275
C ----------------------------------------------------------------------  276
C ------------ SBMI                                                       276
C Begin Local Procedure SBMI                                              277
C SBMI IS THE INNER BEAMPIPE SUPPORT MOUNTING BLOCK                       277
5300  CONTINUE                                                            277
         AG_TITLE='SBMI'                                                  277
         AG_MODULE=MODULE                                                 277
         AG_MARK='V00'                                                    277
         CALL AGSPUSH                                                     277
         AG_MODULE=MODULE                                                 277
C    *                                                                    277
C    Geant MATERIAL : G10 ===> inherited arguments used <===              278
         CALL AGSRESET                                                    278
         AG_TITLE = 'G10'                                                 278
         AG_EXNAME = 'MATERIAL'                                           278
         AG_PARLIST = 'NONE'                                              278
         CALL AXMATERIAL                                                  278
C    *                                                                    278
C    Geant ATTRIBUTE : SBMI ===> SEEN=1 COLO=3 <===                       278
         CALL AGSRESET                                                    279
         AG_SEEN = 1                                                      279
         AG_COLO = 3                                                      279
         AG_TITLE = 'SBMI'                                                279
         AG_EXNAME = 'ATTRIBUTE'                                          279
         AG_PARLIST = 'SEEN_COLO'                                         279
         CALL AXATTRIBUTE                                                 279
C    *                                                                    279
C    Geant SHAPE : BOX ===> DX=SSUB_MBLKIWID/2 DY=SSUB_MBLKILEN/2, DZ=SS  279
C    UB_MBLKHGH/2 <===                                                    279
         CALL AGSRESET                                                    281
         AG_DX = SSUB_MBLKIWID/2                                          281
         AG_DY = SSUB_MBLKILEN/2                                          281
         AG_DZ = SSUB_MBLKHGH/2                                           281
         AG_TITLE = 'BOX'                                                 281
         AG_EXNAME = 'SHAPE'                                              281
         AG_PARLIST = 'DX_DY_DZ'                                          281
         CALL AXSHAPE                                                     281
C    *                                                                    281
5301  CALL AGSPOPB                                                        282
      GO TO BLOCK_SBMI, (5300 ,5310)                                      282
C End of Local Procedure                                                  282
C ----------------------------------------------------------------------  283
C ------------ SAKM                                                       283
C Begin Local Procedure SAKM                                              284
C SAKM IS THE BEAMPIPE SUPPORT ALUMINUM KINEMATIC MOUNT                   284
5140  CONTINUE                                                            284
         AG_TITLE='SAKM'                                                  284
         AG_MODULE=MODULE                                                 284
         AG_MARK='V00'                                                    284
         CALL AGSPUSH                                                     284
         AG_MODULE=MODULE                                                 284
C    *                                                                    284
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        285
         CALL AGSRESET                                                    285
         AG_TITLE = 'ALUMINIUM'                                           285
         AG_EXNAME = 'MATERIAL'                                           285
         AG_PARLIST = 'NONE'                                              285
         CALL AXMATERIAL                                                  285
C    *                                                                    285
C    Geant ATTRIBUTE : SAKM ===> SEEN=1 COLO=2 <===                       285
         CALL AGSRESET                                                    286
         AG_SEEN = 1                                                      286
         AG_COLO = 2                                                      286
         AG_TITLE = 'SAKM'                                                286
         AG_EXNAME = 'ATTRIBUTE'                                          286
         AG_PARLIST = 'SEEN_COLO'                                         286
         CALL AXATTRIBUTE                                                 286
C    *                                                                    286
C    Geant SHAPE : TUBE ===> RMIN=SSUB_KMOUNTID/2 RMAX=SSUB_KMOUNTOD/2,   286
C    DZ=SSUB_KMNTTHK/2 <===                                               286
         CALL AGSRESET                                                    288
         AG_RMIN = SSUB_KMOUNTID/2                                        288
         AG_RMAX = SSUB_KMOUNTOD/2                                        288
         AG_DZ = SSUB_KMNTTHK/2                                           288
         AG_TITLE = 'TUBE'                                                288
         AG_EXNAME = 'SHAPE'                                              288
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      288
         CALL AXSHAPE                                                     288
C    *                                                                    288
C    Create SCKM :                                                        289
C    Call To Local Procedure SCKM                                         289
         ASSIGN 5330 TO BLOCK_SCKM                                        289
         GO TO  5320                                                      289
5330     CONTINUE                                                         289
C    *                                                                    289
C    Geant POSITION : SCKM ===> Y=SSUB_KMCUTOFF KONLY='MANY' <===         290
         CALL AGSRESET                                                    290
         AG_Y = SSUB_KMCUTOFF                                             290
         AG_KONLY = 'MANY'                                                290
         AG_TITLE = 'SCKM'                                                290
         AG_EXNAME = 'POSITION'                                           290
         AG_PARLIST = 'Y_KONLY'                                           290
         CALL AXPOSITION                                                  290
C    *                                                                    290
C    Geant POSITION : SCKM ===> Y=-SSUB_KMCUTOFF ALPHAZ=180 KONLY='MANY'  290
C     <===                                                                290
         CALL AGSRESET                                                    291
         AG_Y = -SSUB_KMCUTOFF                                            291
         AG_ALPHAN=AG_ALPHAN+1                                            291
         AG_ALPHAK(AG_ALPHAN)=3                                           291
         AG_ALPHAL(AG_ALPHAN)=180                                         291
         AG_KONLY = 'MANY'                                                291
         AG_TITLE = 'SCKM'                                                291
         AG_EXNAME = 'POSITION'                                           291
         AG_PARLIST = 'Y_ALPHAZ_KONLY'                                    291
         CALL AXPOSITION                                                  291
C    *                                                                    291
5141  CALL AGSPOPB                                                        292
      GO TO BLOCK_SAKM, (5140 ,5150)                                      292
C End of Local Procedure                                                  292
C ----------------------------------------------------------------------  293
C ------------ SCKM                                                       293
C Begin Local Procedure SCKM                                              294
C SCKM IS THE CUTOUT IN THE ALUMINUM KINEMATIC MOUNT                      294
5320  CONTINUE                                                            294
         AG_TITLE='SCKM'                                                  294
         AG_MODULE=MODULE                                                 294
         AG_MARK='V00'                                                    294
         CALL AGSPUSH                                                     294
         AG_MODULE=MODULE                                                 294
C    *                                                                    294
C    Geant MATERIAL : AIR ===> inherited arguments used <===              295
         CALL AGSRESET                                                    295
         AG_TITLE = 'AIR'                                                 295
         AG_EXNAME = 'MATERIAL'                                           295
         AG_PARLIST = 'NONE'                                              295
         CALL AXMATERIAL                                                  295
C    *                                                                    295
C    Geant ATTRIBUTE : SCKM ===> SEEN=1 COLO=1 <===                       295
         CALL AGSRESET                                                    296
         AG_SEEN = 1                                                      296
         AG_COLO = 1                                                      296
         AG_TITLE = 'SCKM'                                                296
         AG_EXNAME = 'ATTRIBUTE'                                          296
         AG_PARLIST = 'SEEN_COLO'                                         296
         CALL AXATTRIBUTE                                                 296
C    *                                                                    296
C    Geant SHAPE : TUBS ===> RMIN=SSUB_KMCUTID/2 RMAX=SSUB_KMCUTOD/2, PH  296
C    I1=270-SSUB_KMCUTOA PHI2=270+SSUB_KMCUTOA, DZ=SSUB_KMNTTHK/2 <===    296
         CALL AGSRESET                                                    299
         AG_RMIN = SSUB_KMCUTID/2                                         299
         AG_RMAX = SSUB_KMCUTOD/2                                         299
         AG_PHI1 = 270-SSUB_KMCUTOA                                       299
         AG_PHI2 = 270+SSUB_KMCUTOA                                       299
         AG_DZ = SSUB_KMNTTHK/2                                           299
         AG_TITLE = 'TUBS'                                                299
         AG_EXNAME = 'SHAPE'                                              299
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            299
         CALL AXSHAPE                                                     299
C    *                                                                    299
5321  CALL AGSPOPB                                                        300
      GO TO BLOCK_SCKM, (5320 ,5330)                                      300
C End of Local Procedure                                                  300
C ----------------------------------------------------------------------  301
C ------------ SBRL                                                       301
C Begin Local Procedure SBRL                                              302
C SBRL IS THE CERAMIC ROLLER SUPPORTING THE BEAMPIPE                      302
5180  CONTINUE                                                            302
         AG_TITLE='SBRL'                                                  302
         AG_MODULE=MODULE                                                 302
         AG_MARK='V00'                                                    302
         CALL AGSPUSH                                                     302
         AG_MODULE=MODULE                                                 302
C    *                                                                    302
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  303
C    ADL=12.6 <===                                                        303
         CALL AGSRESET                                                    303
         AG_A = 20.719                                                    303
         AG_Z = 10.307                                                    303
         AG_DENS = 2.23                                                   303
         AG_ABSL = 50.7                                                   303
         AG_RADL = 12.6                                                   303
         AG_TITLE = 'PYREX'                                               303
         AG_EXNAME = 'MATERIAL'                                           303
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                303
         CALL AXMATERIAL                                                  303
C    *                                                                    303
C    Geant ATTRIBUTE : SBRL ===> SEEN=1 COLO=6 <===                       303
         CALL AGSRESET                                                    304
         AG_SEEN = 1                                                      304
         AG_COLO = 6                                                      304
         AG_TITLE = 'SBRL'                                                304
         AG_EXNAME = 'ATTRIBUTE'                                          304
         AG_PARLIST = 'SEEN_COLO'                                         304
         CALL AXATTRIBUTE                                                 304
C    *                                                                    304
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SROLLID/2 RMAX=SSUB_SROLLOD/2 <==  304
C    =                                                                    304
         CALL AGSRESET                                                    305
         AG_RMIN = SSUB_SROLLID/2                                         305
         AG_RMAX = SSUB_SROLLOD/2                                         305
         AG_TITLE = 'TUBE'                                                305
         AG_EXNAME = 'SHAPE'                                              305
         AG_PARLIST = 'RMIN_RMAX'                                         305
         CALL AXSHAPE                                                     305
C    *                                                                    305
5181  CALL AGSPOPB                                                        306
      GO TO BLOCK_SBRL, (5180 ,5190)                                      306
C End of Local Procedure                                                  306
C ----------------------------------------------------------------------  307
C ------------ SBRX                                                       307
C Begin Local Procedure SBRX                                              308
C SBRX IS THE STAINLESS STEEL ROLLER AXIS                                 308
5200  CONTINUE                                                            308
         AG_TITLE='SBRX'                                                  308
         AG_MODULE=MODULE                                                 308
         AG_MARK='V00'                                                    308
         CALL AGSPUSH                                                     308
         AG_MODULE=MODULE                                                 308
C    *                                                                    308
C    Geant MATERIAL : IRON ===> inherited arguments used <===             309
         CALL AGSRESET                                                    309
         AG_TITLE = 'IRON'                                                309
         AG_EXNAME = 'MATERIAL'                                           309
         AG_PARLIST = 'NONE'                                              309
         CALL AXMATERIAL                                                  309
C    *                                                                    309
C    Geant ATTRIBUTE : SBRX ===> SEEN=1 COLO=2 <===                       309
         CALL AGSRESET                                                    310
         AG_SEEN = 1                                                      310
         AG_COLO = 2                                                      310
         AG_TITLE = 'SBRX'                                                310
         AG_EXNAME = 'ATTRIBUTE'                                          310
         AG_PARLIST = 'SEEN_COLO'                                         310
         CALL AXATTRIBUTE                                                 310
C    *                                                                    310
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SSUB_SROLLID/2, DZ=SSUB_SWIRE  310
C    LEN/2 <===                                                           310
         CALL AGSRESET                                                    312
         AG_RMIN = 0.0                                                    312
         AG_RMAX = SSUB_SROLLID/2                                         312
         AG_DZ = SSUB_SWIRELEN/2                                          312
         AG_TITLE = 'TUBE'                                                312
         AG_EXNAME = 'SHAPE'                                              312
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      312
         CALL AXSHAPE                                                     312
C    *                                                                    312
5201  CALL AGSPOPB                                                        313
      GO TO BLOCK_SBRX, (5200 ,5210)                                      313
C End of Local Procedure                                                  313
C ----------------------------------------------------------------------  314
C ------------ SBSR                                                       314
C Begin Local Procedure SBSR                                              315
C SBSR IS THE BEAMPIPE SUPPORT G10 RING                                   315
5240  CONTINUE                                                            315
         AG_TITLE='SBSR'                                                  315
         AG_MODULE=MODULE                                                 315
         AG_MARK='V00'                                                    315
         CALL AGSPUSH                                                     315
         AG_MODULE=MODULE                                                 315
C    *                                                                    315
C    Geant MATERIAL : G10 ===> inherited arguments used <===              316
         CALL AGSRESET                                                    316
         AG_TITLE = 'G10'                                                 316
         AG_EXNAME = 'MATERIAL'                                           316
         AG_PARLIST = 'NONE'                                              316
         CALL AXMATERIAL                                                  316
C    *                                                                    316
C    Geant ATTRIBUTE : SBSR ===> SEEN=1 COLO=3 <===                       316
         CALL AGSRESET                                                    317
         AG_SEEN = 1                                                      317
         AG_COLO = 3                                                      317
         AG_TITLE = 'SBSR'                                                317
         AG_EXNAME = 'ATTRIBUTE'                                          317
         AG_PARLIST = 'SEEN_COLO'                                         317
         CALL AXATTRIBUTE                                                 317
C    *                                                                    317
C    Geant SHAPE : TUBE ===> RMIN=SSUB_SRINGID/2 RMAX=SSUB_SRINGOD/2, DZ  317
C    =SSUB_SRINGTHK/2 <===                                                317
         CALL AGSRESET                                                    319
         AG_RMIN = SSUB_SRINGID/2                                         319
         AG_RMAX = SSUB_SRINGOD/2                                         319
         AG_DZ = SSUB_SRINGTHK/2                                          319
         AG_TITLE = 'TUBE'                                                319
         AG_EXNAME = 'SHAPE'                                              319
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      319
         CALL AXSHAPE                                                     319
C    *                                                                    319
C    Create SBCR :                                                        320
C    Call To Local Procedure SBCR                                         320
         ASSIGN 5350 TO BLOCK_SBCR                                        320
         GO TO  5340                                                      320
5350     CONTINUE                                                         320
C    *                                                                    320
         XBUF=SSUB_SRCUTIN + (SSUB_SRCUTOUT - SSUB_SRCUTIN)/2             321
C    *                                                                    322
         I=-1                                                             322
            GOTO 5363                                                     322
5361        I=I+(2)                                                       322
5363        IF((2)*(I-(1)).GT.0) GO TO 5362                               322
C       *                                                                 323
            J=0                                                           323
               GO TO 5373                                                 323
5371           J=J+1                                                      323
5373           IF(J-(1).GT.0) GO TO 5372                                  323
               PHI=I*SSUB_SRCUTPHI+180*J                                  324
C          Geant POSITION : SBCR ===> X=XBUF*SIN(DEGRAD*PHI) Y=-XBUF*COS  326
C          (DEGRAD*PHI) ALPHAZ=PHI KONLY='MANY' <===                      326
               CALL AGSRESET                                              326
               AG_X = XBUF*SIN(DEGRAD*PHI)                                326
               AG_Y = -XBUF*COS(DEGRAD*PHI)                               326
               AG_ALPHAN=AG_ALPHAN+1                                      326
               AG_ALPHAK(AG_ALPHAN)=3                                     326
               AG_ALPHAL(AG_ALPHAN)=PHI                                   326
               AG_KONLY = 'MANY'                                          326
               AG_TITLE = 'SBCR'                                          326
               AG_EXNAME = 'POSITION'                                     326
               AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                            326
               CALL AXPOSITION                                            326
C          *                                                              326
            GO TO 5371                                                    327
5372        CONTINUE                                                      327
         GO TO 5361                                                       328
5362     CONTINUE                                                         328
5241  CALL AGSPOPB                                                        329
      GO TO BLOCK_SBSR, (5240 ,5250)                                      329
C End of Local Procedure                                                  329
C ----------------------------------------------------------------------  330
C ------------ SBCR                                                       330
C Begin Local Procedure SBCR                                              331
C SBCR IS THE CUTOUT IN THE BEAMPIPE SUPPORT G10 RING                     331
5340  CONTINUE                                                            331
         AG_TITLE='SBCR'                                                  331
         AG_MODULE=MODULE                                                 331
         AG_MARK='V00'                                                    331
         CALL AGSPUSH                                                     331
         AG_MODULE=MODULE                                                 331
C    *                                                                    331
C    Geant MATERIAL : AIR ===> inherited arguments used <===              332
         CALL AGSRESET                                                    332
         AG_TITLE = 'AIR'                                                 332
         AG_EXNAME = 'MATERIAL'                                           332
         AG_PARLIST = 'NONE'                                              332
         CALL AXMATERIAL                                                  332
C    *                                                                    332
C    Geant ATTRIBUTE : SBCR ===> SEEN=1 COLO=1 <===                       332
         CALL AGSRESET                                                    333
         AG_SEEN = 1                                                      333
         AG_COLO = 1                                                      333
         AG_TITLE = 'SBCR'                                                333
         AG_EXNAME = 'ATTRIBUTE'                                          333
         AG_PARLIST = 'SEEN_COLO'                                         333
         CALL AXATTRIBUTE                                                 333
C    *                                                                    333
C    Geant SHAPE : BOX ===> DX=SSUB_SRCUTWID/2 DY=(SSUB_SRCUTOUT-SSUB_SR  333
C    CUTIN)/2, DZ=SSUB_SRINGTHK/2 <===                                    333
         CALL AGSRESET                                                    335
         AG_DX = SSUB_SRCUTWID/2                                          335
         AG_DY = (SSUB_SRCUTOUT-SSUB_SRCUTIN)/2                           335
         AG_DZ = SSUB_SRINGTHK/2                                          335
         AG_TITLE = 'BOX'                                                 335
         AG_EXNAME = 'SHAPE'                                              335
         AG_PARLIST = 'DX_DY_DZ'                                          335
         CALL AXSHAPE                                                     335
C    *                                                                    335
5341  CALL AGSPOPB                                                        336
      GO TO BLOCK_SBCR, (5340 ,5350)                                      336
C End of Local Procedure                                                  336
C ----------------------------------------------------------------------  337
C ------------ SCMY                                                       337
C Begin Local Procedure SCMY                                              338
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            338
5120  CONTINUE                                                            338
         AG_TITLE='SCMY'                                                  338
         AG_MODULE=MODULE                                                 338
         AG_MARK='V00'                                                    338
         CALL AGSPUSH                                                     338
         AG_MODULE=MODULE                                                 338
C    *                                                                    338
         ROFFSET=SSUP_GRPHTHK + SSUP_CABTHK + 2.0*SSUP_HOSRMX             339
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          340
         CALL AGSRESET                                                    340
         AG_A = 12                                                        340
         AG_Z = 6                                                         340
         AG_W = 5                                                         340
         AG_TITLE = 'C5'                                                  340
         AG_EXNAME = 'COMPONENT'                                          340
         AG_PARLIST = 'A_Z_W'                                             340
         CALL AXCOMPONENT                                                 340
C    *                                                                    340
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           340
         CALL AGSRESET                                                    341
         AG_A = 1                                                         341
         AG_Z = 1                                                         341
         AG_W = 4                                                         341
         AG_TITLE = 'H4'                                                  341
         AG_EXNAME = 'COMPONENT'                                          341
         AG_PARLIST = 'A_Z_W'                                             341
         CALL AXCOMPONENT                                                 341
C    *                                                                    341
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          341
         CALL AGSRESET                                                    342
         AG_A = 16                                                        342
         AG_Z = 8                                                         342
         AG_W = 2                                                         342
         AG_TITLE = 'O2'                                                  342
         AG_EXNAME = 'COMPONENT'                                          342
         AG_PARLIST = 'A_Z_W'                                             342
         CALL AXCOMPONENT                                                 342
C    *                                                                    342
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            342
         CALL AGSRESET                                                    343
         AG_DENS = 1.39                                                   343
         AG_TITLE = 'MYLAR'                                               343
         AG_EXNAME = 'MIXTURE'                                            343
         AG_PARLIST = 'DENS'                                              343
         CALL AXMIXTURE                                                   343
C    *                                                                    343
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       343
         CALL AGSRESET                                                    344
         AG_SEEN = 1                                                      344
         AG_COLO = 3                                                      344
         AG_TITLE = 'SCMY'                                                344
         AG_EXNAME = 'ATTRIBUTE'                                          344
         AG_PARLIST = 'SEEN_COLO'                                         344
         CALL AXATTRIBUTE                                                 344
C    *                                                                    344
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={SSUP_RODLEN/2+SSUP  344
C    _GRPHTHK, SSUP_CONE3ZMX,SSUP_CONE4ZMX}, RMN={SSUP_CON3IDMN+ROFFSET,  344
C    SSUP_CON4IDMN+ROFFSET, SSUP_CON4IDMX+ROFFSET} RMX={SSUP_CON3IDMN+RO  344
C    FFSET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4  344
C    IDMX+ROFFSET+SSUP_WRPMYTHK} <===                                     344
         CALL AGSRESET                                                    350
         AG_PHI1 = 0                                                      350
         AG_DPHI = 360                                                    350
         AG_NZ = 3                                                        350
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              350
         AG_ZI(2)=SSUP_CONE3ZMX                                           350
         AG_ZI(3)=SSUP_CONE4ZMX                                           350
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  350
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  350
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  350
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    350
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    350
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    350
         AG_TITLE = 'PCON'                                                350
         AG_EXNAME = 'SHAPE'                                              350
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           350
         CALL AXSHAPE                                                     350
C    *                                                                    350
5121  CALL AGSPOPB                                                        351
      GO TO BLOCK_SCMY, (5120 ,5130)                                      351
C End of Local Procedure                                                  351
C ----------------------------------------------------------------------  352
C ------------ SROD                                                       352
C Begin Local Procedure SROD                                              353
C SROD IS THE SVT CARBON COMPOSITE SUPPORT ROD                            353
5050  CONTINUE                                                            353
         AG_TITLE='SROD'                                                  353
         AG_MODULE=MODULE                                                 353
         AG_MARK='V00'                                                    353
         CALL AGSPUSH                                                     353
         AG_MODULE=MODULE                                                 353
C    *                                                                    353
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           354
         CALL AGSRESET                                                    354
         AG_TITLE = 'CARBON'                                              354
         AG_EXNAME = 'MATERIAL'                                           354
         AG_PARLIST = 'NONE'                                              354
         CALL AXMATERIAL                                                  354
C    *                                                                    354
C    Check SSUP_VERSION>=4.00                                             354
         IF (SSUP_VERSION.GE.4.00) THEN                                   355
C    Geant MATERIAL : CARBONFIBER ===> DENS=SSUP_CARBONDENS <===          356
         CALL AGSRESET                                                    356
         AG_DENS = SSUP_CARBONDENS                                        356
         AG_TITLE = 'CARBONFIBER'                                         356
         AG_EXNAME = 'MATERIAL'                                           356
         AG_PARLIST = 'DENS'                                              356
         CALL AXMATERIAL                                                  356
C    *                                                                    356
         ENDIF                                                            356
C    Geant ATTRIBUTE : SROD ===> SEEN=1 COLO=1 <===                       358
         CALL AGSRESET                                                    358
         AG_SEEN = 1                                                      358
         AG_COLO = 1                                                      358
         AG_TITLE = 'SROD'                                                358
         AG_EXNAME = 'ATTRIBUTE'                                          358
         AG_PARLIST = 'SEEN_COLO'                                         358
         CALL AXATTRIBUTE                                                 358
C    *                                                                    358
C    Geant SHAPE : ELTU ===> P1=SSUP_RODODX/2 P2=SSUP_RODOD/2, DZ=SSUP_R  358
C    ODLEN/2 <===                                                         358
         CALL AGSRESET                                                    360
         AG_P1 = SSUP_RODODX/2                                            360
         AG_P2 = SSUP_RODOD/2                                             360
         AG_DZ = SSUP_RODLEN/2                                            360
         AG_TITLE = 'ELTU'                                                360
         AG_EXNAME = 'SHAPE'                                              360
         AG_PARLIST = 'P1_P2_DZ'                                          360
         CALL AXSHAPE                                                     360
C    *                                                                    360
C    Check SSUP_VERSION>=4.00                                             360
         IF (SSUP_VERSION.GE.4.00) THEN                                   361
C    Create SRON :                                                        362
C    Call To Local Procedure SRON                                         362
         ASSIGN 5390 TO BLOCK_SRON                                        362
         GO TO  5380                                                      362
5390     CONTINUE                                                         362
C    *                                                                    362
C    Geant POSITION : SRON ===> inherited arguments used <===             363
         CALL AGSRESET                                                    363
         AG_TITLE = 'SRON'                                                363
         AG_EXNAME = 'POSITION'                                           363
         AG_PARLIST = 'NONE'                                              363
         CALL AXPOSITION                                                  363
C    *                                                                    363
         ELSE                                                             363
C    Create SROH :                                                        365
C    Call To Local Procedure SROH                                         365
         ASSIGN 5410 TO BLOCK_SROH                                        365
         GO TO  5400                                                      365
5410     CONTINUE                                                         365
C    *                                                                    365
C    Geant POSITION : SROH ===> inherited arguments used <===             366
         CALL AGSRESET                                                    366
         AG_TITLE = 'SROH'                                                366
         AG_EXNAME = 'POSITION'                                           366
         AG_PARLIST = 'NONE'                                              366
         CALL AXPOSITION                                                  366
C    *                                                                    366
         ENDIF                                                            366
5051  CALL AGSPOPB                                                        368
      GO TO BLOCK_SROD, (5050 ,5060)                                      368
C End of Local Procedure                                                  368
C ----------------------------------------------------------------------  369
C ------------ SRON                                                       369
C Begin Local Procedure SRON                                              370
C SRON IS THE CREAMY NOMEX FILLING                                        370
5380  CONTINUE                                                            370
         AG_TITLE='SRON'                                                  370
         AG_MODULE=MODULE                                                 370
         AG_MARK='V00'                                                    370
         CALL AGSPUSH                                                     370
         AG_MODULE=MODULE                                                 370
C    *                                                                    370
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           371
         CALL AGSRESET                                                    371
         AG_A = 12                                                        371
         AG_Z = 6                                                         371
         AG_W = 5                                                         371
         AG_TITLE = 'C'                                                   371
         AG_EXNAME = 'COMPONENT'                                          371
         AG_PARLIST = 'A_Z_W'                                             371
         CALL AXCOMPONENT                                                 371
C    *                                                                    371
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            371
         CALL AGSRESET                                                    372
         AG_A = 1                                                         372
         AG_Z = 1                                                         372
         AG_W = 8                                                         372
         AG_TITLE = 'H'                                                   372
         AG_EXNAME = 'COMPONENT'                                          372
         AG_PARLIST = 'A_Z_W'                                             372
         CALL AXCOMPONENT                                                 372
C    *                                                                    372
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           372
         CALL AGSRESET                                                    373
         AG_A = 16                                                        373
         AG_Z = 8                                                         373
         AG_W = 2                                                         373
         AG_TITLE = 'O'                                                   373
         AG_EXNAME = 'COMPONENT'                                          373
         AG_PARLIST = 'A_Z_W'                                             373
         CALL AXCOMPONENT                                                 373
C    *                                                                    373
C    Geant MIXTURE : NOMEX ===> DENS=SSUP_NOMEXDENS <===                  373
         CALL AGSRESET                                                    374
         AG_DENS = SSUP_NOMEXDENS                                         374
         AG_TITLE = 'NOMEX'                                               374
         AG_EXNAME = 'MIXTURE'                                            374
         AG_PARLIST = 'DENS'                                              374
         CALL AXMIXTURE                                                   374
C    *                                                                    374
C    Geant ATTRIBUTE : SRON ===> SEEN=1 COLO=5 <===                       374
         CALL AGSRESET                                                    375
         AG_SEEN = 1                                                      375
         AG_COLO = 5                                                      375
         AG_TITLE = 'SRON'                                                375
         AG_EXNAME = 'ATTRIBUTE'                                          375
         AG_PARLIST = 'SEEN_COLO'                                         375
         CALL AXATTRIBUTE                                                 375
C    *                                                                    375
C    Geant SHAPE : ELTU ===> P1=SSUP_RODODX/2-SSUP_CARBONSHELL, P2=SSUP_  375
C    RODOD/2-SSUP_CARBONSHELL <===                                        375
         CALL AGSRESET                                                    377
         AG_P1 = SSUP_RODODX/2-SSUP_CARBONSHELL                           377
         AG_P2 = SSUP_RODOD/2-SSUP_CARBONSHELL                            377
         AG_TITLE = 'ELTU'                                                377
         AG_EXNAME = 'SHAPE'                                              377
         AG_PARLIST = 'P1_P2'                                             377
         CALL AXSHAPE                                                     377
C    *                                                                    377
C    Create SROI :                                                        378
C    Call To Local Procedure SROI                                         378
         ASSIGN 5430 TO BLOCK_SROI                                        378
         GO TO  5420                                                      378
5430     CONTINUE                                                         378
C    *                                                                    378
C    Geant POSITION : SROI ===> inherited arguments used <===             379
         CALL AGSRESET                                                    379
         AG_TITLE = 'SROI'                                                379
         AG_EXNAME = 'POSITION'                                           379
         AG_PARLIST = 'NONE'                                              379
         CALL AXPOSITION                                                  379
C    *                                                                    379
5381  CALL AGSPOPB                                                        380
      GO TO BLOCK_SRON, (5380 ,5390)                                      380
C End of Local Procedure                                                  380
C ----------------------------------------------------------------------  381
C ------------ SROI                                                       381
C Begin Local Procedure SROI                                              382
C SROI IS THE INNER CARBON FIBER SHELL                                    382
5420  CONTINUE                                                            382
         AG_TITLE='SROI'                                                  382
         AG_MODULE=MODULE                                                 382
         AG_MARK='V00'                                                    382
         CALL AGSPUSH                                                     382
         AG_MODULE=MODULE                                                 382
C    *                                                                    382
C    Geant MATERIAL : CARBONFIBER ===> inherited arguments used <===      383
         CALL AGSRESET                                                    383
         AG_TITLE = 'CARBONFIBER'                                         383
         AG_EXNAME = 'MATERIAL'                                           383
         AG_PARLIST = 'NONE'                                              383
         CALL AXMATERIAL                                                  383
C    *                                                                    383
C    Geant ATTRIBUTE : SROI ===> SEEN=1 COLO=1 <===                       383
         CALL AGSRESET                                                    384
         AG_SEEN = 1                                                      384
         AG_COLO = 1                                                      384
         AG_TITLE = 'SROI'                                                384
         AG_EXNAME = 'ATTRIBUTE'                                          384
         AG_PARLIST = 'SEEN_COLO'                                         384
         CALL AXATTRIBUTE                                                 384
C    *                                                                    384
C    Geant SHAPE : ELTU ===> P1=SSUP_RODIDX/2+SSUP_CARBONSHELL, P2=SSUP_  384
C    RODID/2+SSUP_CARBONSHELL <===                                        384
         CALL AGSRESET                                                    386
         AG_P1 = SSUP_RODIDX/2+SSUP_CARBONSHELL                           386
         AG_P2 = SSUP_RODID/2+SSUP_CARBONSHELL                            386
         AG_TITLE = 'ELTU'                                                386
         AG_EXNAME = 'SHAPE'                                              386
         AG_PARLIST = 'P1_P2'                                             386
         CALL AXSHAPE                                                     386
C    *                                                                    386
C    Create SROH :                                                        387
C    Call To Local Procedure SROH                                         387
         ASSIGN 5440 TO BLOCK_SROH                                        387
         GO TO  5400                                                      387
5440     CONTINUE                                                         387
C    *                                                                    387
C    Geant POSITION : SROH ===> inherited arguments used <===             388
         CALL AGSRESET                                                    388
         AG_TITLE = 'SROH'                                                388
         AG_EXNAME = 'POSITION'                                           388
         AG_PARLIST = 'NONE'                                              388
         CALL AXPOSITION                                                  388
C    *                                                                    388
5421  CALL AGSPOPB                                                        389
      GO TO BLOCK_SROI, (5420 ,5430)                                      389
C End of Local Procedure                                                  389
C ----------------------------------------------------------------------  390
C ------------ SROH                                                       390
C Begin Local Procedure SROH                                              391
C SROH IS THE HOLE IN SROD                                                391
5400  CONTINUE                                                            391
         AG_TITLE='SROH'                                                  391
         AG_MODULE=MODULE                                                 391
         AG_MARK='V00'                                                    391
         CALL AGSPUSH                                                     391
         AG_MODULE=MODULE                                                 391
C    *                                                                    391
C    Geant MATERIAL : AIR ===> inherited arguments used <===              392
         CALL AGSRESET                                                    392
         AG_TITLE = 'AIR'                                                 392
         AG_EXNAME = 'MATERIAL'                                           392
         AG_PARLIST = 'NONE'                                              392
         CALL AXMATERIAL                                                  392
C    *                                                                    392
C    Geant ATTRIBUTE : SROH ===> SEEN=1 COLO=3 <===                       392
         CALL AGSRESET                                                    393
         AG_SEEN = 1                                                      393
         AG_COLO = 3                                                      393
         AG_TITLE = 'SROH'                                                393
         AG_EXNAME = 'ATTRIBUTE'                                          393
         AG_PARLIST = 'SEEN_COLO'                                         393
         CALL AXATTRIBUTE                                                 393
C    *                                                                    393
C    Geant SHAPE : ELTU ===> P1=SSUP_RODIDX/2 P2=SSUP_RODID/2, DZ=SSUP_R  393
C    ODLEN/2 <===                                                         393
         CALL AGSRESET                                                    395
         AG_P1 = SSUP_RODIDX/2                                            395
         AG_P2 = SSUP_RODID/2                                             395
         AG_DZ = SSUP_RODLEN/2                                            395
         AG_TITLE = 'ELTU'                                                395
         AG_EXNAME = 'SHAPE'                                              395
         AG_PARLIST = 'P1_P2_DZ'                                          395
         CALL AXSHAPE                                                     395
C    *                                                                    395
5401  CALL AGSPOPB                                                        396
      GO TO BLOCK_SROH, (5400 ,5410,5440)                                 396
C End of Local Procedure                                                  396
      WRITE(LOUT,5450)'===>>  [End AgML Module SCONGEO]  <<==='           396
5450  FORMAT(A32,/,/)                                                     396
C MODULE SCONGEO                                                          398
      END                                                                 398
