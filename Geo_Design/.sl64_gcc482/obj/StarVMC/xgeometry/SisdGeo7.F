C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SISDGEO7                                                 1
C SISDGEO7 IS THE SILICON STRIP DETECTOR WITH TUP MODIFICATIONS           1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SISDGEO7'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE SILICON STRIP DETECTOR WITH TUP MODIFICATION  1
     *S'/                                                                 1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR BOUCHET AND POTEKHIN                                             1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'BOUCHET AND POTEKHIN'/                        2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 1 FEB 07                                                        2
      CHARACTER*40 CREATED /'1 FEB 07'/                                   3
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
      INTEGER BLOCK_SFMO                                                  6
      INTEGER BLOCK_SFLM                                                  14
      INTEGER BLOCK_SFSW                                                  14
      INTEGER BLOCK_SFSD                                                  14
      INTEGER BLOCK_SFSL                                                  14
      INTEGER BLOCK_SFLT                                                  14
      INTEGER BLOCK_SFLU                                                  14
      INTEGER BLOCK_SFFK                                                  14
      INTEGER BLOCK_SFFL                                                  14
      INTEGER BLOCK_SFKK                                                  14
      INTEGER BLOCK_SFKL                                                  14
      INTEGER BLOCK_SFRA                                                  14
      INTEGER BLOCK_SFRS                                                  14
      INTEGER BLOCK_SFFX                                                  14
      INTEGER BLOCK_SFPI                                                  14
      INTEGER BLOCK_SFPJ                                                  14
      INTEGER BLOCK_SFAA                                                  14
      INTEGER BLOCK_SFAM                                                  14
      INTEGER BLOCK_SFAB                                                  14
      INTEGER BLOCK_SFAS                                                  14
      INTEGER BLOCK_SAPP                                                  14
      INTEGER BLOCK_SAPC                                                  14
      INTEGER BLOCK_SAPS                                                  14
      INTEGER BLOCK_SAPT                                                  14
      INTEGER BLOCK_SFLA                                                  14
      INTEGER BLOCK_SFLB                                                  14
      INTEGER BLOCK_SFLC                                                  14
      INTEGER BLOCK_SFES                                                  14
      INTEGER BLOCK_SFEB                                                  14
      INTEGER BLOCK_SFCO                                                  14
      INTEGER BLOCK_SFCM                                                  14
      INTEGER BLOCK_SFCB                                                  14
      INTEGER BLOCK_SFCS                                                  14
      INTEGER BLOCK_SFKF                                                  14
      INTEGER BLOCK_SFKS                                                  14
      INTEGER BLOCK_SSBS                                                  14
      INTEGER BLOCK_SSBB                                                  14
      INTEGER BLOCK_SFPR                                                  14
      INTEGER BLOCK_SFPB                                                  14
      INTEGER BLOCK_SSST                                                  14
      INTEGER BLOCK_SSSS                                                  14
      INTEGER BLOCK_SSRS                                                  14
      INTEGER BLOCK_SSRT                                                  14
      INTEGER BLOCK_SSLB                                                  14
      INTEGER BLOCK_SBCH                                                  14
      INTEGER BLOCK_SBCV                                                  14
      INTEGER BLOCK_SBFH                                                  14
      INTEGER BLOCK_SBFV                                                  14
      INTEGER BLOCK_SSLT                                                  14
      INTEGER BLOCK_SCMP                                                  14
      INTEGER BLOCK_SCVM                                                  14
      INTEGER BLOCK_SCVB                                                  14
      INTEGER BLOCK_SCVS                                                  14
      INTEGER BLOCK_SOSK                                                  14
      INTEGER BLOCK_SFPK                                                  14
C -                                                                       15
      REAL SSDP_VERSION                                                   15
      EQUIVALENCE (BANK_SSDP(3),SSDP_VERSION,RE_SSDP_VERSION),            15
     *(NAMES_SSDP(1),NSSDP_VERSION)                                       15
      EQUIVALENCE (COMMEN_SSDP(1),CSSDP_VERSION),(MAP_SSDP(1,1),          15
     *MSSDP_VERSION)                                                      15
      CHARACTER CSSDP_VERSION*40/' '/                                     15
      CHARACTER NSSDP_VERSION*12/'R.VERSION'/                             15
      INTEGER MSSDP_VERSION(2)/1,1/                                       15
      REAL RE_SSDP_VERSION                                                15
C -                                                                       15
      INTEGER SSDP_CONFIG                                                 15
      EQUIVALENCE (BANK_SSDP(4),SSDP_CONFIG,RE_SSDP_CONFIG),              15
     *(NAMES_SSDP(2),NSSDP_CONFIG)                                        15
      EQUIVALENCE (COMMEN_SSDP(2),CSSDP_CONFIG),(MAP_SSDP(1,2),           15
     *MSSDP_CONFIG)                                                       15
      CHARACTER CSSDP_CONFIG*40/' '/                                      15
      CHARACTER NSSDP_CONFIG*12/'I.CONFIG'/                               15
      INTEGER MSSDP_CONFIG(2)/1,1/                                        15
      REAL RE_SSDP_CONFIG                                                 15
C -                                                                       15
      INTEGER SSDP_PLACEMENT                                              15
      EQUIVALENCE (BANK_SSDP(5),SSDP_PLACEMENT,RE_SSDP_PLACEMENT),        15
     *(NAMES_SSDP(3),NSSDP_PLACEMENT)                                     15
      EQUIVALENCE (COMMEN_SSDP(3),CSSDP_PLACEMENT),(MAP_SSDP(1,3),        15
     *MSSDP_PLACEMENT)                                                    15
      CHARACTER CSSDP_PLACEMENT*40/' '/                                   15
      CHARACTER NSSDP_PLACEMENT*12/'I.PLACEMENT'/                         15
      INTEGER MSSDP_PLACEMENT(2)/1,1/                                     15
      REAL RE_SSDP_PLACEMENT                                              15
C -                                                                       15
      INTEGER LEN_SSDP,LLEN_SSDP,LINK_SSDP                                15
      PARAMETER (LEN_SSDP=5)                                              15
      PARAMETER (LLEN_SSDP=3)                                             15
      INTEGER FLAG_SSDP/0/,MAP_SSDP(2,LLEN_SSDP),IDNUM_SSDP(10)           15
      CHARACTER NAMES_SSDP(LLEN_SSDP)*12,COMMEN_SSDP(LLEN_SSDP)*40        15
      DATA IDNUM_SSDP/10*0/                                               15
      REAL BANK_SSDP(LEN_SSDP)                                            15
      CHARACTER*60 BPATH_SSDP/' '/                                        15
      SAVE FLAG_SSDP,MAP_SSDP,IDNUM_SSDP,NAMES_SSDP,COMMEN_SSDP,          15
     *BPATH_SSDP                                                          15
      DATA LINK_SSDP/0/,BANK_SSDP/LEN_SSDP*0/                             15
C *                                                                       15
      INTEGER CONFIGA,CONFIGB                                             16
C -                                                                       33
      INTEGER SFJP_VERSION                                                33
      EQUIVALENCE (BANK_SFJP(3),SFJP_VERSION,RE_SFJP_VERSION),            33
     *(NAMES_SFJP(1),NSFJP_VERSION)                                       33
      EQUIVALENCE (COMMEN_SFJP(1),CSFJP_VERSION),(MAP_SFJP(1,1),          33
     *MSFJP_VERSION)                                                      33
      CHARACTER CSFJP_VERSION*40/' '/                                     33
      CHARACTER NSFJP_VERSION*12/'I.VERSION'/                             33
      INTEGER MSFJP_VERSION(2)/1,1/                                       33
      REAL RE_SFJP_VERSION                                                33
C -                                                                       33
      REAL SFJP_ALPHAZ                                                    33
      EQUIVALENCE (BANK_SFJP(4),SFJP_ALPHAZ,RE_SFJP_ALPHAZ),              33
     *(NAMES_SFJP(2),NSFJP_ALPHAZ)                                        33
      EQUIVALENCE (COMMEN_SFJP(2),CSFJP_ALPHAZ),(MAP_SFJP(1,2),           33
     *MSFJP_ALPHAZ)                                                       33
      CHARACTER CSFJP_ALPHAZ*40/' '/                                      33
      CHARACTER NSFJP_ALPHAZ*12/'R.ALPHAZ'/                               33
      INTEGER MSFJP_ALPHAZ(2)/1,1/                                        33
      REAL RE_SFJP_ALPHAZ                                                 33
C -                                                                       33
      REAL SFJP_ALPHAZH                                                   33
      EQUIVALENCE (BANK_SFJP(5),SFJP_ALPHAZH,RE_SFJP_ALPHAZH),            33
     *(NAMES_SFJP(3),NSFJP_ALPHAZH)                                       33
      EQUIVALENCE (COMMEN_SFJP(3),CSFJP_ALPHAZH),(MAP_SFJP(1,3),          33
     *MSFJP_ALPHAZH)                                                      33
      CHARACTER CSFJP_ALPHAZH*40/' '/                                     33
      CHARACTER NSFJP_ALPHAZH*12/'R.ALPHAZH'/                             33
      INTEGER MSFJP_ALPHAZH(2)/1,1/                                       33
      REAL RE_SFJP_ALPHAZH                                                33
C -                                                                       33
      REAL SFJP_SSST_RMIN                                                 33
      EQUIVALENCE (BANK_SFJP(6),SFJP_SSST_RMIN,RE_SFJP_SSST_RMIN),        33
     *(NAMES_SFJP(4),NSFJP_SSST_RMIN)                                     33
      EQUIVALENCE (COMMEN_SFJP(4),CSFJP_SSST_RMIN),(MAP_SFJP(1,4),        33
     *MSFJP_SSST_RMIN)                                                    33
      CHARACTER CSFJP_SSST_RMIN*40/' '/                                   33
      CHARACTER NSFJP_SSST_RMIN*12/'R.SSST_RMIN'/                         33
      INTEGER MSFJP_SSST_RMIN(2)/1,1/                                     33
      REAL RE_SFJP_SSST_RMIN                                              33
C -                                                                       33
      REAL SFJP_SSST_RMAX                                                 33
      EQUIVALENCE (BANK_SFJP(7),SFJP_SSST_RMAX,RE_SFJP_SSST_RMAX),        33
     *(NAMES_SFJP(5),NSFJP_SSST_RMAX)                                     33
      EQUIVALENCE (COMMEN_SFJP(5),CSFJP_SSST_RMAX),(MAP_SFJP(1,5),        33
     *MSFJP_SSST_RMAX)                                                    33
      CHARACTER CSFJP_SSST_RMAX*40/' '/                                   33
      CHARACTER NSFJP_SSST_RMAX*12/'R.SSST_RMAX'/                         33
      INTEGER MSFJP_SSST_RMAX(2)/1,1/                                     33
      REAL RE_SFJP_SSST_RMAX                                              33
C -                                                                       33
      REAL SFJP_SSST_WIDTH                                                33
      EQUIVALENCE (BANK_SFJP(8),SFJP_SSST_WIDTH,RE_SFJP_SSST_WIDTH),      33
     *(NAMES_SFJP(6),NSFJP_SSST_WIDTH)                                    33
      EQUIVALENCE (COMMEN_SFJP(6),CSFJP_SSST_WIDTH),(MAP_SFJP(1,6),       33
     *MSFJP_SSST_WIDTH)                                                   33
      CHARACTER CSFJP_SSST_WIDTH*40/' '/                                  33
      CHARACTER NSFJP_SSST_WIDTH*12/'R.SSST_WIDTH'/                       33
      INTEGER MSFJP_SSST_WIDTH(2)/1,1/                                    33
      REAL RE_SFJP_SSST_WIDTH                                             33
C -                                                                       33
      REAL SFJP_SSST_PZ                                                   33
      EQUIVALENCE (BANK_SFJP(9),SFJP_SSST_PZ,RE_SFJP_SSST_PZ),            33
     *(NAMES_SFJP(7),NSFJP_SSST_PZ)                                       33
      EQUIVALENCE (COMMEN_SFJP(7),CSFJP_SSST_PZ),(MAP_SFJP(1,7),          33
     *MSFJP_SSST_PZ)                                                      33
      CHARACTER CSFJP_SSST_PZ*40/' '/                                     33
      CHARACTER NSFJP_SSST_PZ*12/'R.SSST_PZ'/                             33
      INTEGER MSFJP_SSST_PZ(2)/1,1/                                       33
      REAL RE_SFJP_SSST_PZ                                                33
C -                                                                       33
      REAL SFJP_SSSS_RMIN                                                 33
      EQUIVALENCE (BANK_SFJP(10),SFJP_SSSS_RMIN,RE_SFJP_SSSS_RMIN),       33
     *(NAMES_SFJP(8),NSFJP_SSSS_RMIN)                                     33
      EQUIVALENCE (COMMEN_SFJP(8),CSFJP_SSSS_RMIN),(MAP_SFJP(1,8),        33
     *MSFJP_SSSS_RMIN)                                                    33
      CHARACTER CSFJP_SSSS_RMIN*40/' '/                                   33
      CHARACTER NSFJP_SSSS_RMIN*12/'R.SSSS_RMIN'/                         33
      INTEGER MSFJP_SSSS_RMIN(2)/1,1/                                     33
      REAL RE_SFJP_SSSS_RMIN                                              33
C -                                                                       33
      REAL SFJP_SSSS_WIDTH                                                33
      EQUIVALENCE (BANK_SFJP(11),SFJP_SSSS_WIDTH,RE_SFJP_SSSS_WIDTH),     33
     *(NAMES_SFJP(9),NSFJP_SSSS_WIDTH)                                    33
      EQUIVALENCE (COMMEN_SFJP(9),CSFJP_SSSS_WIDTH),(MAP_SFJP(1,9),       33
     *MSFJP_SSSS_WIDTH)                                                   33
      CHARACTER CSFJP_SSSS_WIDTH*40/' '/                                  33
      CHARACTER NSFJP_SSSS_WIDTH*12/'R.SSSS_WIDTH'/                       33
      INTEGER MSFJP_SSSS_WIDTH(2)/1,1/                                    33
      REAL RE_SFJP_SSSS_WIDTH                                             33
C -                                                                       33
      REAL SFJP_SSRS_RMIN                                                 33
      EQUIVALENCE (BANK_SFJP(12),SFJP_SSRS_RMIN,RE_SFJP_SSRS_RMIN),       33
     *(NAMES_SFJP(10),NSFJP_SSRS_RMIN)                                    33
      EQUIVALENCE (COMMEN_SFJP(10),CSFJP_SSRS_RMIN),(MAP_SFJP(1,10),      33
     *MSFJP_SSRS_RMIN)                                                    33
      CHARACTER CSFJP_SSRS_RMIN*40/' '/                                   33
      CHARACTER NSFJP_SSRS_RMIN*12/'R.SSRS_RMIN'/                         33
      INTEGER MSFJP_SSRS_RMIN(2)/1,1/                                     33
      REAL RE_SFJP_SSRS_RMIN                                              33
C -                                                                       33
      REAL SFJP_SSLB_DX                                                   33
      EQUIVALENCE (BANK_SFJP(13),SFJP_SSLB_DX,RE_SFJP_SSLB_DX),           33
     *(NAMES_SFJP(11),NSFJP_SSLB_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(11),CSFJP_SSLB_DX),(MAP_SFJP(1,11),        33
     *MSFJP_SSLB_DX)                                                      33
      CHARACTER CSFJP_SSLB_DX*40/' '/                                     33
      CHARACTER NSFJP_SSLB_DX*12/'R.SSLB_DX'/                             33
      INTEGER MSFJP_SSLB_DX(2)/1,1/                                       33
      REAL RE_SFJP_SSLB_DX                                                33
C -                                                                       33
      REAL SFJP_SSLB_PX                                                   33
      EQUIVALENCE (BANK_SFJP(14),SFJP_SSLB_PX,RE_SFJP_SSLB_PX),           33
     *(NAMES_SFJP(12),NSFJP_SSLB_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(12),CSFJP_SSLB_PX),(MAP_SFJP(1,12),        33
     *MSFJP_SSLB_PX)                                                      33
      CHARACTER CSFJP_SSLB_PX*40/' '/                                     33
      CHARACTER NSFJP_SSLB_PX*12/'R.SSLB_PX'/                             33
      INTEGER MSFJP_SSLB_PX(2)/1,1/                                       33
      REAL RE_SFJP_SSLB_PX                                                33
C -                                                                       33
      REAL SFJP_SSLB_DY                                                   33
      EQUIVALENCE (BANK_SFJP(15),SFJP_SSLB_DY,RE_SFJP_SSLB_DY),           33
     *(NAMES_SFJP(13),NSFJP_SSLB_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(13),CSFJP_SSLB_DY),(MAP_SFJP(1,13),        33
     *MSFJP_SSLB_DY)                                                      33
      CHARACTER CSFJP_SSLB_DY*40/' '/                                     33
      CHARACTER NSFJP_SSLB_DY*12/'R.SSLB_DY'/                             33
      INTEGER MSFJP_SSLB_DY(2)/1,1/                                       33
      REAL RE_SFJP_SSLB_DY                                                33
C -                                                                       33
      REAL SFJP_SSLB_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(16),SFJP_SSLB_DZ,RE_SFJP_SSLB_DZ),           33
     *(NAMES_SFJP(14),NSFJP_SSLB_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(14),CSFJP_SSLB_DZ),(MAP_SFJP(1,14),        33
     *MSFJP_SSLB_DZ)                                                      33
      CHARACTER CSFJP_SSLB_DZ*40/' '/                                     33
      CHARACTER NSFJP_SSLB_DZ*12/'R.SSLB_DZ'/                             33
      INTEGER MSFJP_SSLB_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SSLB_DZ                                                33
C -                                                                       33
      REAL SFJP_SSBQ_DX                                                   33
      EQUIVALENCE (BANK_SFJP(17),SFJP_SSBQ_DX,RE_SFJP_SSBQ_DX),           33
     *(NAMES_SFJP(15),NSFJP_SSBQ_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(15),CSFJP_SSBQ_DX),(MAP_SFJP(1,15),        33
     *MSFJP_SSBQ_DX)                                                      33
      CHARACTER CSFJP_SSBQ_DX*40/' '/                                     33
      CHARACTER NSFJP_SSBQ_DX*12/'R.SSBQ_DX'/                             33
      INTEGER MSFJP_SSBQ_DX(2)/1,1/                                       33
      REAL RE_SFJP_SSBQ_DX                                                33
C -                                                                       33
      REAL SFJP_SSBQ_DY                                                   33
      EQUIVALENCE (BANK_SFJP(18),SFJP_SSBQ_DY,RE_SFJP_SSBQ_DY),           33
     *(NAMES_SFJP(16),NSFJP_SSBQ_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(16),CSFJP_SSBQ_DY),(MAP_SFJP(1,16),        33
     *MSFJP_SSBQ_DY)                                                      33
      CHARACTER CSFJP_SSBQ_DY*40/' '/                                     33
      CHARACTER NSFJP_SSBQ_DY*12/'R.SSBQ_DY'/                             33
      INTEGER MSFJP_SSBQ_DY(2)/1,1/                                       33
      REAL RE_SFJP_SSBQ_DY                                                33
C -                                                                       33
      REAL SFJP_SSBQ_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(19),SFJP_SSBQ_DZ,RE_SFJP_SSBQ_DZ),           33
     *(NAMES_SFJP(17),NSFJP_SSBQ_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(17),CSFJP_SSBQ_DZ),(MAP_SFJP(1,17),        33
     *MSFJP_SSBQ_DZ)                                                      33
      CHARACTER CSFJP_SSBQ_DZ*40/' '/                                     33
      CHARACTER NSFJP_SSBQ_DZ*12/'R.SSBQ_DZ'/                             33
      INTEGER MSFJP_SSBQ_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SSBQ_DZ                                                33
C -                                                                       33
      REAL SFJP_SSCR_THA                                                  33
      EQUIVALENCE (BANK_SFJP(20),SFJP_SSCR_THA,RE_SFJP_SSCR_THA),         33
     *(NAMES_SFJP(18),NSFJP_SSCR_THA)                                     33
      EQUIVALENCE (COMMEN_SFJP(18),CSFJP_SSCR_THA),(MAP_SFJP(1,18),       33
     *MSFJP_SSCR_THA)                                                     33
      CHARACTER CSFJP_SSCR_THA*40/' '/                                    33
      CHARACTER NSFJP_SSCR_THA*12/'R.SSCR_THA'/                           33
      INTEGER MSFJP_SSCR_THA(2)/1,1/                                      33
      REAL RE_SFJP_SSCR_THA                                               33
C -                                                                       33
      REAL SFJP_SSCR_THD                                                  33
      EQUIVALENCE (BANK_SFJP(21),SFJP_SSCR_THD,RE_SFJP_SSCR_THD),         33
     *(NAMES_SFJP(19),NSFJP_SSCR_THD)                                     33
      EQUIVALENCE (COMMEN_SFJP(19),CSFJP_SSCR_THD),(MAP_SFJP(1,19),       33
     *MSFJP_SSCR_THD)                                                     33
      CHARACTER CSFJP_SSCR_THD*40/' '/                                    33
      CHARACTER NSFJP_SSCR_THD*12/'R.SSCR_THD'/                           33
      INTEGER MSFJP_SSCR_THD(2)/1,1/                                      33
      REAL RE_SFJP_SSCR_THD                                               33
C -                                                                       33
      REAL SFJP_SSCR_WD                                                   33
      EQUIVALENCE (BANK_SFJP(22),SFJP_SSCR_WD,RE_SFJP_SSCR_WD),           33
     *(NAMES_SFJP(20),NSFJP_SSCR_WD)                                      33
      EQUIVALENCE (COMMEN_SFJP(20),CSFJP_SSCR_WD),(MAP_SFJP(1,20),        33
     *MSFJP_SSCR_WD)                                                      33
      CHARACTER CSFJP_SSCR_WD*40/' '/                                     33
      CHARACTER NSFJP_SSCR_WD*12/'R.SSCR_WD'/                             33
      INTEGER MSFJP_SSCR_WD(2)/1,1/                                       33
      REAL RE_SFJP_SSCR_WD                                                33
C -                                                                       33
      REAL SFJP_SSCR_HL                                                   33
      EQUIVALENCE (BANK_SFJP(23),SFJP_SSCR_HL,RE_SFJP_SSCR_HL),           33
     *(NAMES_SFJP(21),NSFJP_SSCR_HL)                                      33
      EQUIVALENCE (COMMEN_SFJP(21),CSFJP_SSCR_HL),(MAP_SFJP(1,21),        33
     *MSFJP_SSCR_HL)                                                      33
      CHARACTER CSFJP_SSCR_HL*40/' '/                                     33
      CHARACTER NSFJP_SSCR_HL*12/'R.SSCR_HL'/                             33
      INTEGER MSFJP_SSCR_HL(2)/1,1/                                       33
      REAL RE_SFJP_SSCR_HL                                                33
C -                                                                       33
      REAL SFJP_SSCR_VL                                                   33
      EQUIVALENCE (BANK_SFJP(24),SFJP_SSCR_VL,RE_SFJP_SSCR_VL),           33
     *(NAMES_SFJP(22),NSFJP_SSCR_VL)                                      33
      EQUIVALENCE (COMMEN_SFJP(22),CSFJP_SSCR_VL),(MAP_SFJP(1,22),        33
     *MSFJP_SSCR_VL)                                                      33
      CHARACTER CSFJP_SSCR_VL*40/' '/                                     33
      CHARACTER NSFJP_SSCR_VL*12/'R.SSCR_VL'/                             33
      INTEGER MSFJP_SSCR_VL(2)/1,1/                                       33
      REAL RE_SFJP_SSCR_VL                                                33
C -                                                                       33
      REAL SFJP_SSLT_PX                                                   33
      EQUIVALENCE (BANK_SFJP(25),SFJP_SSLT_PX,RE_SFJP_SSLT_PX),           33
     *(NAMES_SFJP(23),NSFJP_SSLT_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(23),CSFJP_SSLT_PX),(MAP_SFJP(1,23),        33
     *MSFJP_SSLT_PX)                                                      33
      CHARACTER CSFJP_SSLT_PX*40/' '/                                     33
      CHARACTER NSFJP_SSLT_PX*12/'R.SSLT_PX'/                             33
      INTEGER MSFJP_SSLT_PX(2)/1,1/                                       33
      REAL RE_SFJP_SSLT_PX                                                33
C -                                                                       33
      REAL SFJP_SSLT_RMAX                                                 33
      EQUIVALENCE (BANK_SFJP(26),SFJP_SSLT_RMAX,RE_SFJP_SSLT_RMAX),       33
     *(NAMES_SFJP(24),NSFJP_SSLT_RMAX)                                    33
      EQUIVALENCE (COMMEN_SFJP(24),CSFJP_SSLT_RMAX),(MAP_SFJP(1,24),      33
     *MSFJP_SSLT_RMAX)                                                    33
      CHARACTER CSFJP_SSLT_RMAX*40/' '/                                   33
      CHARACTER NSFJP_SSLT_RMAX*12/'R.SSLT_RMAX'/                         33
      INTEGER MSFJP_SSLT_RMAX(2)/1,1/                                     33
      REAL RE_SFJP_SSLT_RMAX                                              33
C -                                                                       33
      REAL SFJP_SSLT_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(27),SFJP_SSLT_DZ,RE_SFJP_SSLT_DZ),           33
     *(NAMES_SFJP(25),NSFJP_SSLT_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(25),CSFJP_SSLT_DZ),(MAP_SFJP(1,25),        33
     *MSFJP_SSLT_DZ)                                                      33
      CHARACTER CSFJP_SSLT_DZ*40/' '/                                     33
      CHARACTER NSFJP_SSLT_DZ*12/'R.SSLT_DZ'/                             33
      INTEGER MSFJP_SSLT_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SSLT_DZ                                                33
C -                                                                       33
      REAL SFJP_SCMP_DX                                                   33
      EQUIVALENCE (BANK_SFJP(28),SFJP_SCMP_DX,RE_SFJP_SCMP_DX),           33
     *(NAMES_SFJP(26),NSFJP_SCMP_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(26),CSFJP_SCMP_DX),(MAP_SFJP(1,26),        33
     *MSFJP_SCMP_DX)                                                      33
      CHARACTER CSFJP_SCMP_DX*40/' '/                                     33
      CHARACTER NSFJP_SCMP_DX*12/'R.SCMP_DX'/                             33
      INTEGER MSFJP_SCMP_DX(2)/1,1/                                       33
      REAL RE_SFJP_SCMP_DX                                                33
C -                                                                       33
      REAL SFJP_SCMP_DY                                                   33
      EQUIVALENCE (BANK_SFJP(29),SFJP_SCMP_DY,RE_SFJP_SCMP_DY),           33
     *(NAMES_SFJP(27),NSFJP_SCMP_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(27),CSFJP_SCMP_DY),(MAP_SFJP(1,27),        33
     *MSFJP_SCMP_DY)                                                      33
      CHARACTER CSFJP_SCMP_DY*40/' '/                                     33
      CHARACTER NSFJP_SCMP_DY*12/'R.SCMP_DY'/                             33
      INTEGER MSFJP_SCMP_DY(2)/1,1/                                       33
      REAL RE_SFJP_SCMP_DY                                                33
C -                                                                       33
      REAL SFJP_SCMP_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(30),SFJP_SCMP_DZ,RE_SFJP_SCMP_DZ),           33
     *(NAMES_SFJP(28),NSFJP_SCMP_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(28),CSFJP_SCMP_DZ),(MAP_SFJP(1,28),        33
     *MSFJP_SCMP_DZ)                                                      33
      CHARACTER CSFJP_SCMP_DZ*40/' '/                                     33
      CHARACTER NSFJP_SCMP_DZ*12/'R.SCMP_DZ'/                             33
      INTEGER MSFJP_SCMP_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SCMP_DZ                                                33
C -                                                                       33
      REAL SFJP_SCMP_PX                                                   33
      EQUIVALENCE (BANK_SFJP(31),SFJP_SCMP_PX,RE_SFJP_SCMP_PX),           33
     *(NAMES_SFJP(29),NSFJP_SCMP_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(29),CSFJP_SCMP_PX),(MAP_SFJP(1,29),        33
     *MSFJP_SCMP_PX)                                                      33
      CHARACTER CSFJP_SCMP_PX*40/' '/                                     33
      CHARACTER NSFJP_SCMP_PX*12/'R.SCMP_PX'/                             33
      INTEGER MSFJP_SCMP_PX(2)/1,1/                                       33
      REAL RE_SFJP_SCMP_PX                                                33
C -                                                                       33
      REAL SFJP_SCVM_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(32),SFJP_SCVM_DZ,RE_SFJP_SCVM_DZ),           33
     *(NAMES_SFJP(30),NSFJP_SCVM_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(30),CSFJP_SCVM_DZ),(MAP_SFJP(1,30),        33
     *MSFJP_SCVM_DZ)                                                      33
      CHARACTER CSFJP_SCVM_DZ*40/' '/                                     33
      CHARACTER NSFJP_SCVM_DZ*12/'R.SCVM_DZ'/                             33
      INTEGER MSFJP_SCVM_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SCVM_DZ                                                33
C -                                                                       33
      REAL SFJP_SCVM_DX                                                   33
      EQUIVALENCE (BANK_SFJP(33),SFJP_SCVM_DX,RE_SFJP_SCVM_DX),           33
     *(NAMES_SFJP(31),NSFJP_SCVM_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(31),CSFJP_SCVM_DX),(MAP_SFJP(1,31),        33
     *MSFJP_SCVM_DX)                                                      33
      CHARACTER CSFJP_SCVM_DX*40/' '/                                     33
      CHARACTER NSFJP_SCVM_DX*12/'R.SCVM_DX'/                             33
      INTEGER MSFJP_SCVM_DX(2)/1,1/                                       33
      REAL RE_SFJP_SCVM_DX                                                33
C -                                                                       33
      REAL SFJP_SCVM_PX                                                   33
      EQUIVALENCE (BANK_SFJP(34),SFJP_SCVM_PX,RE_SFJP_SCVM_PX),           33
     *(NAMES_SFJP(32),NSFJP_SCVM_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(32),CSFJP_SCVM_PX),(MAP_SFJP(1,32),        33
     *MSFJP_SCVM_PX)                                                      33
      CHARACTER CSFJP_SCVM_PX*40/' '/                                     33
      CHARACTER NSFJP_SCVM_PX*12/'R.SCVM_PX'/                             33
      INTEGER MSFJP_SCVM_PX(2)/1,1/                                       33
      REAL RE_SFJP_SCVM_PX                                                33
C -                                                                       33
      REAL SFJP_SCVM_DY                                                   33
      EQUIVALENCE (BANK_SFJP(35),SFJP_SCVM_DY,RE_SFJP_SCVM_DY),           33
     *(NAMES_SFJP(33),NSFJP_SCVM_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(33),CSFJP_SCVM_DY),(MAP_SFJP(1,33),        33
     *MSFJP_SCVM_DY)                                                      33
      CHARACTER CSFJP_SCVM_DY*40/' '/                                     33
      CHARACTER NSFJP_SCVM_DY*12/'R.SCVM_DY'/                             33
      INTEGER MSFJP_SCVM_DY(2)/1,1/                                       33
      REAL RE_SFJP_SCVM_DY                                                33
C -                                                                       33
      REAL SFJP_SCVM_PZ                                                   33
      EQUIVALENCE (BANK_SFJP(36),SFJP_SCVM_PZ,RE_SFJP_SCVM_PZ),           33
     *(NAMES_SFJP(34),NSFJP_SCVM_PZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(34),CSFJP_SCVM_PZ),(MAP_SFJP(1,34),        33
     *MSFJP_SCVM_PZ)                                                      33
      CHARACTER CSFJP_SCVM_PZ*40/' '/                                     33
      CHARACTER NSFJP_SCVM_PZ*12/'R.SCVM_PZ'/                             33
      INTEGER MSFJP_SCVM_PZ(2)/1,1/                                       33
      REAL RE_SFJP_SCVM_PZ                                                33
C -                                                                       33
      REAL SFJP_SCVB_DY                                                   33
      EQUIVALENCE (BANK_SFJP(37),SFJP_SCVB_DY,RE_SFJP_SCVB_DY),           33
     *(NAMES_SFJP(35),NSFJP_SCVB_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(35),CSFJP_SCVB_DY),(MAP_SFJP(1,35),        33
     *MSFJP_SCVB_DY)                                                      33
      CHARACTER CSFJP_SCVB_DY*40/' '/                                     33
      CHARACTER NSFJP_SCVB_DY*12/'R.SCVB_DY'/                             33
      INTEGER MSFJP_SCVB_DY(2)/1,1/                                       33
      REAL RE_SFJP_SCVB_DY                                                33
C -                                                                       33
      REAL SFJP_SCVS_DX                                                   33
      EQUIVALENCE (BANK_SFJP(38),SFJP_SCVS_DX,RE_SFJP_SCVS_DX),           33
     *(NAMES_SFJP(36),NSFJP_SCVS_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(36),CSFJP_SCVS_DX),(MAP_SFJP(1,36),        33
     *MSFJP_SCVS_DX)                                                      33
      CHARACTER CSFJP_SCVS_DX*40/' '/                                     33
      CHARACTER NSFJP_SCVS_DX*12/'R.SCVS_DX'/                             33
      INTEGER MSFJP_SCVS_DX(2)/1,1/                                       33
      REAL RE_SFJP_SCVS_DX                                                33
C -                                                                       33
      REAL SFJP_SCVS_DY                                                   33
      EQUIVALENCE (BANK_SFJP(39),SFJP_SCVS_DY,RE_SFJP_SCVS_DY),           33
     *(NAMES_SFJP(37),NSFJP_SCVS_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(37),CSFJP_SCVS_DY),(MAP_SFJP(1,37),        33
     *MSFJP_SCVS_DY)                                                      33
      CHARACTER CSFJP_SCVS_DY*40/' '/                                     33
      CHARACTER NSFJP_SCVS_DY*12/'R.SCVS_DY'/                             33
      INTEGER MSFJP_SCVS_DY(2)/1,1/                                       33
      REAL RE_SFJP_SCVS_DY                                                33
C -                                                                       33
      REAL SFJP_SCVS_PX                                                   33
      EQUIVALENCE (BANK_SFJP(40),SFJP_SCVS_PX,RE_SFJP_SCVS_PX),           33
     *(NAMES_SFJP(38),NSFJP_SCVS_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(38),CSFJP_SCVS_PX),(MAP_SFJP(1,38),        33
     *MSFJP_SCVS_PX)                                                      33
      CHARACTER CSFJP_SCVS_PX*40/' '/                                     33
      CHARACTER NSFJP_SCVS_PX*12/'R.SCVS_PX'/                             33
      INTEGER MSFJP_SCVS_PX(2)/1,1/                                       33
      REAL RE_SFJP_SCVS_PX                                                33
C -                                                                       33
      REAL SFJP_SCVS_PY                                                   33
      EQUIVALENCE (BANK_SFJP(41),SFJP_SCVS_PY,RE_SFJP_SCVS_PY),           33
     *(NAMES_SFJP(39),NSFJP_SCVS_PY)                                      33
      EQUIVALENCE (COMMEN_SFJP(39),CSFJP_SCVS_PY),(MAP_SFJP(1,39),        33
     *MSFJP_SCVS_PY)                                                      33
      CHARACTER CSFJP_SCVS_PY*40/' '/                                     33
      CHARACTER NSFJP_SCVS_PY*12/'R.SCVS_PY'/                             33
      INTEGER MSFJP_SCVS_PY(2)/1,1/                                       33
      REAL RE_SFJP_SCVS_PY                                                33
C -                                                                       33
      REAL SFJP_SFCO_DX                                                   33
      EQUIVALENCE (BANK_SFJP(42),SFJP_SFCO_DX,RE_SFJP_SFCO_DX),           33
     *(NAMES_SFJP(40),NSFJP_SFCO_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(40),CSFJP_SFCO_DX),(MAP_SFJP(1,40),        33
     *MSFJP_SFCO_DX)                                                      33
      CHARACTER CSFJP_SFCO_DX*40/' '/                                     33
      CHARACTER NSFJP_SFCO_DX*12/'R.SFCO_DX'/                             33
      INTEGER MSFJP_SFCO_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFCO_DX                                                33
C -                                                                       33
      REAL SFJP_SFCO_DY                                                   33
      EQUIVALENCE (BANK_SFJP(43),SFJP_SFCO_DY,RE_SFJP_SFCO_DY),           33
     *(NAMES_SFJP(41),NSFJP_SFCO_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(41),CSFJP_SFCO_DY),(MAP_SFJP(1,41),        33
     *MSFJP_SFCO_DY)                                                      33
      CHARACTER CSFJP_SFCO_DY*40/' '/                                     33
      CHARACTER NSFJP_SFCO_DY*12/'R.SFCO_DY'/                             33
      INTEGER MSFJP_SFCO_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFCO_DY                                                33
C -                                                                       33
      REAL SFJP_SFCO_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(44),SFJP_SFCO_DZ,RE_SFJP_SFCO_DZ),           33
     *(NAMES_SFJP(42),NSFJP_SFCO_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(42),CSFJP_SFCO_DZ),(MAP_SFJP(1,42),        33
     *MSFJP_SFCO_DZ)                                                      33
      CHARACTER CSFJP_SFCO_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFCO_DZ*12/'R.SFCO_DZ'/                             33
      INTEGER MSFJP_SFCO_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFCO_DZ                                                33
C -                                                                       33
      REAL SFJP_SFCO_PX                                                   33
      EQUIVALENCE (BANK_SFJP(45),SFJP_SFCO_PX,RE_SFJP_SFCO_PX),           33
     *(NAMES_SFJP(43),NSFJP_SFCO_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(43),CSFJP_SFCO_PX),(MAP_SFJP(1,43),        33
     *MSFJP_SFCO_PX)                                                      33
      CHARACTER CSFJP_SFCO_PX*40/' '/                                     33
      CHARACTER NSFJP_SFCO_PX*12/'R.SFCO_PX'/                             33
      INTEGER MSFJP_SFCO_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFCO_PX                                                33
C -                                                                       33
      REAL SFJP_SFCO_PY                                                   33
      EQUIVALENCE (BANK_SFJP(46),SFJP_SFCO_PY,RE_SFJP_SFCO_PY),           33
     *(NAMES_SFJP(44),NSFJP_SFCO_PY)                                      33
      EQUIVALENCE (COMMEN_SFJP(44),CSFJP_SFCO_PY),(MAP_SFJP(1,44),        33
     *MSFJP_SFCO_PY)                                                      33
      CHARACTER CSFJP_SFCO_PY*40/' '/                                     33
      CHARACTER NSFJP_SFCO_PY*12/'R.SFCO_PY'/                             33
      INTEGER MSFJP_SFCO_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFCO_PY                                                33
C -                                                                       33
      REAL SFJP_SFCM_DX                                                   33
      EQUIVALENCE (BANK_SFJP(47),SFJP_SFCM_DX,RE_SFJP_SFCM_DX),           33
     *(NAMES_SFJP(45),NSFJP_SFCM_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(45),CSFJP_SFCM_DX),(MAP_SFJP(1,45),        33
     *MSFJP_SFCM_DX)                                                      33
      CHARACTER CSFJP_SFCM_DX*40/' '/                                     33
      CHARACTER NSFJP_SFCM_DX*12/'R.SFCM_DX'/                             33
      INTEGER MSFJP_SFCM_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFCM_DX                                                33
C -                                                                       33
      REAL SFJP_SFCS_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(48),SFJP_SFCS_DZ,RE_SFJP_SFCS_DZ),           33
     *(NAMES_SFJP(46),NSFJP_SFCS_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(46),CSFJP_SFCS_DZ),(MAP_SFJP(1,46),        33
     *MSFJP_SFCS_DZ)                                                      33
      CHARACTER CSFJP_SFCS_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFCS_DZ*12/'R.SFCS_DZ'/                             33
      INTEGER MSFJP_SFCS_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFCS_DZ                                                33
C -                                                                       33
      REAL SFJP_SFKF_DY                                                   33
      EQUIVALENCE (BANK_SFJP(49),SFJP_SFKF_DY,RE_SFJP_SFKF_DY),           33
     *(NAMES_SFJP(47),NSFJP_SFKF_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(47),CSFJP_SFKF_DY),(MAP_SFJP(1,47),        33
     *MSFJP_SFKF_DY)                                                      33
      CHARACTER CSFJP_SFKF_DY*40/' '/                                     33
      CHARACTER NSFJP_SFKF_DY*12/'R.SFKF_DY'/                             33
      INTEGER MSFJP_SFKF_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFKF_DY                                                33
C -                                                                       33
      REAL SFJP_SFKF_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(50),SFJP_SFKF_DZ,RE_SFJP_SFKF_DZ),           33
     *(NAMES_SFJP(48),NSFJP_SFKF_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(48),CSFJP_SFKF_DZ),(MAP_SFJP(1,48),        33
     *MSFJP_SFKF_DZ)                                                      33
      CHARACTER CSFJP_SFKF_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFKF_DZ*12/'R.SFKF_DZ'/                             33
      INTEGER MSFJP_SFKF_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFKF_DZ                                                33
C -                                                                       33
      REAL SFJP_SFKF_DX                                                   33
      EQUIVALENCE (BANK_SFJP(51),SFJP_SFKF_DX,RE_SFJP_SFKF_DX),           33
     *(NAMES_SFJP(49),NSFJP_SFKF_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(49),CSFJP_SFKF_DX),(MAP_SFJP(1,49),        33
     *MSFJP_SFKF_DX)                                                      33
      CHARACTER CSFJP_SFKF_DX*40/' '/                                     33
      CHARACTER NSFJP_SFKF_DX*12/'R.SFKF_DX'/                             33
      INTEGER MSFJP_SFKF_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFKF_DX                                                33
C -                                                                       33
      REAL SFJP_SFKF_PX                                                   33
      EQUIVALENCE (BANK_SFJP(52),SFJP_SFKF_PX,RE_SFJP_SFKF_PX),           33
     *(NAMES_SFJP(50),NSFJP_SFKF_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(50),CSFJP_SFKF_PX),(MAP_SFJP(1,50),        33
     *MSFJP_SFKF_PX)                                                      33
      CHARACTER CSFJP_SFKF_PX*40/' '/                                     33
      CHARACTER NSFJP_SFKF_PX*12/'R.SFKF_PX'/                             33
      INTEGER MSFJP_SFKF_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFKF_PX                                                33
C -                                                                       33
      REAL SFJP_SFKF_PY                                                   33
      EQUIVALENCE (BANK_SFJP(53),SFJP_SFKF_PY,RE_SFJP_SFKF_PY),           33
     *(NAMES_SFJP(51),NSFJP_SFKF_PY)                                      33
      EQUIVALENCE (COMMEN_SFJP(51),CSFJP_SFKF_PY),(MAP_SFJP(1,51),        33
     *MSFJP_SFKF_PY)                                                      33
      CHARACTER CSFJP_SFKF_PY*40/' '/                                     33
      CHARACTER NSFJP_SFKF_PY*12/'R.SFKF_PY'/                             33
      INTEGER MSFJP_SFKF_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFKF_PY                                                33
C -                                                                       33
      REAL SFJP_SFKS_DX                                                   33
      EQUIVALENCE (BANK_SFJP(54),SFJP_SFKS_DX,RE_SFJP_SFKS_DX),           33
     *(NAMES_SFJP(52),NSFJP_SFKS_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(52),CSFJP_SFKS_DX),(MAP_SFJP(1,52),        33
     *MSFJP_SFKS_DX)                                                      33
      CHARACTER CSFJP_SFKS_DX*40/' '/                                     33
      CHARACTER NSFJP_SFKS_DX*12/'R.SFKS_DX'/                             33
      INTEGER MSFJP_SFKS_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFKS_DX                                                33
C -                                                                       33
      REAL SFJP_SFKS_PX                                                   33
      EQUIVALENCE (BANK_SFJP(55),SFJP_SFKS_PX,RE_SFJP_SFKS_PX),           33
     *(NAMES_SFJP(53),NSFJP_SFKS_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(53),CSFJP_SFKS_PX),(MAP_SFJP(1,53),        33
     *MSFJP_SFKS_PX)                                                      33
      CHARACTER CSFJP_SFKS_PX*40/' '/                                     33
      CHARACTER NSFJP_SFKS_PX*12/'R.SFKS_PX'/                             33
      INTEGER MSFJP_SFKS_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFKS_PX                                                33
C -                                                                       33
      REAL SFJP_SFKS_PY                                                   33
      EQUIVALENCE (BANK_SFJP(56),SFJP_SFKS_PY,RE_SFJP_SFKS_PY),           33
     *(NAMES_SFJP(54),NSFJP_SFKS_PY)                                      33
      EQUIVALENCE (COMMEN_SFJP(54),CSFJP_SFKS_PY),(MAP_SFJP(1,54),        33
     *MSFJP_SFKS_PY)                                                      33
      CHARACTER CSFJP_SFKS_PY*40/' '/                                     33
      CHARACTER NSFJP_SFKS_PY*12/'R.SFKS_PY'/                             33
      INTEGER MSFJP_SFKS_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFKS_PY                                                33
C -                                                                       33
      REAL SFJP_SFPR_PY                                                   33
      EQUIVALENCE (BANK_SFJP(57),SFJP_SFPR_PY,RE_SFJP_SFPR_PY),           33
     *(NAMES_SFJP(55),NSFJP_SFPR_PY)                                      33
      EQUIVALENCE (COMMEN_SFJP(55),CSFJP_SFPR_PY),(MAP_SFJP(1,55),        33
     *MSFJP_SFPR_PY)                                                      33
      CHARACTER CSFJP_SFPR_PY*40/' '/                                     33
      CHARACTER NSFJP_SFPR_PY*12/'R.SFPR_PY'/                             33
      INTEGER MSFJP_SFPR_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFPR_PY                                                33
C -                                                                       33
      REAL SFJP_SFPB_PY                                                   33
      EQUIVALENCE (BANK_SFJP(58),SFJP_SFPB_PY,RE_SFJP_SFPB_PY),           33
     *(NAMES_SFJP(56),NSFJP_SFPB_PY)                                      33
      EQUIVALENCE (COMMEN_SFJP(56),CSFJP_SFPB_PY),(MAP_SFJP(1,56),        33
     *MSFJP_SFPB_PY)                                                      33
      CHARACTER CSFJP_SFPB_PY*40/' '/                                     33
      CHARACTER NSFJP_SFPB_PY*12/'R.SFPB_PY'/                             33
      INTEGER MSFJP_SFPB_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFPB_PY                                                33
C -                                                                       33
      REAL SFJP_SFPB_PY2                                                  33
      EQUIVALENCE (BANK_SFJP(59),SFJP_SFPB_PY2,RE_SFJP_SFPB_PY2),         33
     *(NAMES_SFJP(57),NSFJP_SFPB_PY2)                                     33
      EQUIVALENCE (COMMEN_SFJP(57),CSFJP_SFPB_PY2),(MAP_SFJP(1,57),       33
     *MSFJP_SFPB_PY2)                                                     33
      CHARACTER CSFJP_SFPB_PY2*40/' '/                                    33
      CHARACTER NSFJP_SFPB_PY2*12/'R.SFPB_PY2'/                           33
      INTEGER MSFJP_SFPB_PY2(2)/1,1/                                      33
      REAL RE_SFJP_SFPB_PY2                                               33
C -                                                                       33
      REAL SFJP_SFCB_DX                                                   33
      EQUIVALENCE (BANK_SFJP(60),SFJP_SFCB_DX,RE_SFJP_SFCB_DX),           33
     *(NAMES_SFJP(58),NSFJP_SFCB_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(58),CSFJP_SFCB_DX),(MAP_SFJP(1,58),        33
     *MSFJP_SFCB_DX)                                                      33
      CHARACTER CSFJP_SFCB_DX*40/' '/                                     33
      CHARACTER NSFJP_SFCB_DX*12/'R.SFCB_DX'/                             33
      INTEGER MSFJP_SFCB_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFCB_DX                                                33
C -                                                                       33
      REAL SFJP_SSBS_DY                                                   33
      EQUIVALENCE (BANK_SFJP(61),SFJP_SSBS_DY,RE_SFJP_SSBS_DY),           33
     *(NAMES_SFJP(59),NSFJP_SSBS_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(59),CSFJP_SSBS_DY),(MAP_SFJP(1,59),        33
     *MSFJP_SSBS_DY)                                                      33
      CHARACTER CSFJP_SSBS_DY*40/' '/                                     33
      CHARACTER NSFJP_SSBS_DY*12/'R.SSBS_DY'/                             33
      INTEGER MSFJP_SSBS_DY(2)/1,1/                                       33
      REAL RE_SFJP_SSBS_DY                                                33
C -                                                                       33
      REAL SFJP_SSBS_DX                                                   33
      EQUIVALENCE (BANK_SFJP(62),SFJP_SSBS_DX,RE_SFJP_SSBS_DX),           33
     *(NAMES_SFJP(60),NSFJP_SSBS_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(60),CSFJP_SSBS_DX),(MAP_SFJP(1,60),        33
     *MSFJP_SSBS_DX)                                                      33
      CHARACTER CSFJP_SSBS_DX*40/' '/                                     33
      CHARACTER NSFJP_SSBS_DX*12/'R.SSBS_DX'/                             33
      INTEGER MSFJP_SSBS_DX(2)/1,1/                                       33
      REAL RE_SFJP_SSBS_DX                                                33
C -                                                                       33
      REAL SFJP_SSBB_DX                                                   33
      EQUIVALENCE (BANK_SFJP(63),SFJP_SSBB_DX,RE_SFJP_SSBB_DX),           33
     *(NAMES_SFJP(61),NSFJP_SSBB_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(61),CSFJP_SSBB_DX),(MAP_SFJP(1,61),        33
     *MSFJP_SSBB_DX)                                                      33
      CHARACTER CSFJP_SSBB_DX*40/' '/                                     33
      CHARACTER NSFJP_SSBB_DX*12/'R.SSBB_DX'/                             33
      INTEGER MSFJP_SSBB_DX(2)/1,1/                                       33
      REAL RE_SFJP_SSBB_DX                                                33
C -                                                                       33
      REAL SFJP_SSBB_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(64),SFJP_SSBB_DZ,RE_SFJP_SSBB_DZ),           33
     *(NAMES_SFJP(62),NSFJP_SSBB_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(62),CSFJP_SSBB_DZ),(MAP_SFJP(1,62),        33
     *MSFJP_SSBB_DZ)                                                      33
      CHARACTER CSFJP_SSBB_DZ*40/' '/                                     33
      CHARACTER NSFJP_SSBB_DZ*12/'R.SSBB_DZ'/                             33
      INTEGER MSFJP_SSBB_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SSBB_DZ                                                33
C -                                                                       33
      REAL SFJP_FLEX_DI                                                   33
      EQUIVALENCE (BANK_SFJP(65),SFJP_FLEX_DI,RE_SFJP_FLEX_DI),           33
     *(NAMES_SFJP(63),NSFJP_FLEX_DI)                                      33
      EQUIVALENCE (COMMEN_SFJP(63),CSFJP_FLEX_DI),(MAP_SFJP(1,63),        33
     *MSFJP_FLEX_DI)                                                      33
      CHARACTER CSFJP_FLEX_DI*40/' '/                                     33
      CHARACTER NSFJP_FLEX_DI*12/'R.FLEX_DI'/                             33
      INTEGER MSFJP_FLEX_DI(2)/1,1/                                       33
      REAL RE_SFJP_FLEX_DI                                                33
C -                                                                       33
      REAL SFJP_SFPB_DX                                                   33
      EQUIVALENCE (BANK_SFJP(66),SFJP_SFPB_DX,RE_SFJP_SFPB_DX),           33
     *(NAMES_SFJP(64),NSFJP_SFPB_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(64),CSFJP_SFPB_DX),(MAP_SFJP(1,64),        33
     *MSFJP_SFPB_DX)                                                      33
      CHARACTER CSFJP_SFPB_DX*40/' '/                                     33
      CHARACTER NSFJP_SFPB_DX*12/'R.SFPB_DX'/                             33
      INTEGER MSFJP_SFPB_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFPB_DX                                                33
C -                                                                       33
      REAL SFJP_SFPB_DY                                                   33
      EQUIVALENCE (BANK_SFJP(67),SFJP_SFPB_DY,RE_SFJP_SFPB_DY),           33
     *(NAMES_SFJP(65),NSFJP_SFPB_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(65),CSFJP_SFPB_DY),(MAP_SFJP(1,65),        33
     *MSFJP_SFPB_DY)                                                      33
      CHARACTER CSFJP_SFPB_DY*40/' '/                                     33
      CHARACTER NSFJP_SFPB_DY*12/'R.SFPB_DY'/                             33
      INTEGER MSFJP_SFPB_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFPB_DY                                                33
C -                                                                       33
      REAL SFJP_SFPBDZ                                                    33
      EQUIVALENCE (BANK_SFJP(68),SFJP_SFPBDZ,RE_SFJP_SFPBDZ),             33
     *(NAMES_SFJP(66),NSFJP_SFPBDZ)                                       33
      EQUIVALENCE (COMMEN_SFJP(66),CSFJP_SFPBDZ),(MAP_SFJP(1,66),         33
     *MSFJP_SFPBDZ)                                                       33
      CHARACTER CSFJP_SFPBDZ*40/' '/                                      33
      CHARACTER NSFJP_SFPBDZ*12/'R.SFPBDZ'/                               33
      INTEGER MSFJP_SFPBDZ(2)/1,1/                                        33
      REAL RE_SFJP_SFPBDZ                                                 33
C -                                                                       33
      REAL SFJP_SAPP_DXE                                                  33
      EQUIVALENCE (BANK_SFJP(69),SFJP_SAPP_DXE,RE_SFJP_SAPP_DXE),         33
     *(NAMES_SFJP(67),NSFJP_SAPP_DXE)                                     33
      EQUIVALENCE (COMMEN_SFJP(67),CSFJP_SAPP_DXE),(MAP_SFJP(1,67),       33
     *MSFJP_SAPP_DXE)                                                     33
      CHARACTER CSFJP_SAPP_DXE*40/' '/                                    33
      CHARACTER NSFJP_SAPP_DXE*12/'R.SAPP_DXE'/                           33
      INTEGER MSFJP_SAPP_DXE(2)/1,1/                                      33
      REAL RE_SFJP_SAPP_DXE                                               33
C -                                                                       33
      REAL SFJP_SAPP_DXZ                                                  33
      EQUIVALENCE (BANK_SFJP(70),SFJP_SAPP_DXZ,RE_SFJP_SAPP_DXZ),         33
     *(NAMES_SFJP(68),NSFJP_SAPP_DXZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(68),CSFJP_SAPP_DXZ),(MAP_SFJP(1,68),       33
     *MSFJP_SAPP_DXZ)                                                     33
      CHARACTER CSFJP_SAPP_DXZ*40/' '/                                    33
      CHARACTER NSFJP_SAPP_DXZ*12/'R.SAPP_DXZ'/                           33
      INTEGER MSFJP_SAPP_DXZ(2)/1,1/                                      33
      REAL RE_SFJP_SAPP_DXZ                                               33
C -                                                                       33
      REAL SFJP_SAPP_DY                                                   33
      EQUIVALENCE (BANK_SFJP(71),SFJP_SAPP_DY,RE_SFJP_SAPP_DY),           33
     *(NAMES_SFJP(69),NSFJP_SAPP_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(69),CSFJP_SAPP_DY),(MAP_SFJP(1,69),        33
     *MSFJP_SAPP_DY)                                                      33
      CHARACTER CSFJP_SAPP_DY*40/' '/                                     33
      CHARACTER NSFJP_SAPP_DY*12/'R.SAPP_DY'/                             33
      INTEGER MSFJP_SAPP_DY(2)/1,1/                                       33
      REAL RE_SFJP_SAPP_DY                                                33
C -                                                                       33
      REAL SFJP_SAPP_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(72),SFJP_SAPP_DZ,RE_SFJP_SAPP_DZ),           33
     *(NAMES_SFJP(70),NSFJP_SAPP_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(70),CSFJP_SAPP_DZ),(MAP_SFJP(1,70),        33
     *MSFJP_SAPP_DZ)                                                      33
      CHARACTER CSFJP_SAPP_DZ*40/' '/                                     33
      CHARACTER NSFJP_SAPP_DZ*12/'R.SAPP_DZ'/                             33
      INTEGER MSFJP_SAPP_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SAPP_DZ                                                33
C -                                                                       33
      REAL SFJP_SAPP_PY1                                                  33
      EQUIVALENCE (BANK_SFJP(73),SFJP_SAPP_PY1,RE_SFJP_SAPP_PY1),         33
     *(NAMES_SFJP(71),NSFJP_SAPP_PY1)                                     33
      EQUIVALENCE (COMMEN_SFJP(71),CSFJP_SAPP_PY1),(MAP_SFJP(1,71),       33
     *MSFJP_SAPP_PY1)                                                     33
      CHARACTER CSFJP_SAPP_PY1*40/' '/                                    33
      CHARACTER NSFJP_SAPP_PY1*12/'R.SAPP_PY1'/                           33
      INTEGER MSFJP_SAPP_PY1(2)/1,1/                                      33
      REAL RE_SFJP_SAPP_PY1                                               33
C -                                                                       33
      REAL SFJP_SAPP_PY2                                                  33
      EQUIVALENCE (BANK_SFJP(74),SFJP_SAPP_PY2,RE_SFJP_SAPP_PY2),         33
     *(NAMES_SFJP(72),NSFJP_SAPP_PY2)                                     33
      EQUIVALENCE (COMMEN_SFJP(72),CSFJP_SAPP_PY2),(MAP_SFJP(1,72),       33
     *MSFJP_SAPP_PY2)                                                     33
      CHARACTER CSFJP_SAPP_PY2*40/' '/                                    33
      CHARACTER NSFJP_SAPP_PY2*12/'R.SAPP_PY2'/                           33
      INTEGER MSFJP_SAPP_PY2(2)/1,1/                                      33
      REAL RE_SFJP_SAPP_PY2                                               33
C -                                                                       33
      REAL SFJP_SAPP_PY3                                                  33
      EQUIVALENCE (BANK_SFJP(75),SFJP_SAPP_PY3,RE_SFJP_SAPP_PY3),         33
     *(NAMES_SFJP(73),NSFJP_SAPP_PY3)                                     33
      EQUIVALENCE (COMMEN_SFJP(73),CSFJP_SAPP_PY3),(MAP_SFJP(1,73),       33
     *MSFJP_SAPP_PY3)                                                     33
      CHARACTER CSFJP_SAPP_PY3*40/' '/                                    33
      CHARACTER NSFJP_SAPP_PY3*12/'R.SAPP_PY3'/                           33
      INTEGER MSFJP_SAPP_PY3(2)/1,1/                                      33
      REAL RE_SFJP_SAPP_PY3                                               33
C -                                                                       33
      REAL SFJP_SFAM_DXE                                                  33
      EQUIVALENCE (BANK_SFJP(76),SFJP_SFAM_DXE,RE_SFJP_SFAM_DXE),         33
     *(NAMES_SFJP(74),NSFJP_SFAM_DXE)                                     33
      EQUIVALENCE (COMMEN_SFJP(74),CSFJP_SFAM_DXE),(MAP_SFJP(1,74),       33
     *MSFJP_SFAM_DXE)                                                     33
      CHARACTER CSFJP_SFAM_DXE*40/' '/                                    33
      CHARACTER NSFJP_SFAM_DXE*12/'R.SFAM_DXE'/                           33
      INTEGER MSFJP_SFAM_DXE(2)/1,1/                                      33
      REAL RE_SFJP_SFAM_DXE                                               33
C -                                                                       33
      REAL SFJP_SFAM_DXZ                                                  33
      EQUIVALENCE (BANK_SFJP(77),SFJP_SFAM_DXZ,RE_SFJP_SFAM_DXZ),         33
     *(NAMES_SFJP(75),NSFJP_SFAM_DXZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(75),CSFJP_SFAM_DXZ),(MAP_SFJP(1,75),       33
     *MSFJP_SFAM_DXZ)                                                     33
      CHARACTER CSFJP_SFAM_DXZ*40/' '/                                    33
      CHARACTER NSFJP_SFAM_DXZ*12/'R.SFAM_DXZ'/                           33
      INTEGER MSFJP_SFAM_DXZ(2)/1,1/                                      33
      REAL RE_SFJP_SFAM_DXZ                                               33
C -                                                                       33
      REAL SFJP_SFAM_DY                                                   33
      EQUIVALENCE (BANK_SFJP(78),SFJP_SFAM_DY,RE_SFJP_SFAM_DY),           33
     *(NAMES_SFJP(76),NSFJP_SFAM_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(76),CSFJP_SFAM_DY),(MAP_SFJP(1,76),        33
     *MSFJP_SFAM_DY)                                                      33
      CHARACTER CSFJP_SFAM_DY*40/' '/                                     33
      CHARACTER NSFJP_SFAM_DY*12/'R.SFAM_DY'/                             33
      INTEGER MSFJP_SFAM_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFAM_DY                                                33
C -                                                                       33
      REAL SFJP_SFAM_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(79),SFJP_SFAM_DZ,RE_SFJP_SFAM_DZ),           33
     *(NAMES_SFJP(77),NSFJP_SFAM_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(77),CSFJP_SFAM_DZ),(MAP_SFJP(1,77),        33
     *MSFJP_SFAM_DZ)                                                      33
      CHARACTER CSFJP_SFAM_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFAM_DZ*12/'R.SFAM_DZ'/                             33
      INTEGER MSFJP_SFAM_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFAM_DZ                                                33
C -                                                                       33
      REAL SFJP_SFAM_DZS                                                  33
      EQUIVALENCE (BANK_SFJP(80),SFJP_SFAM_DZS,RE_SFJP_SFAM_DZS),         33
     *(NAMES_SFJP(78),NSFJP_SFAM_DZS)                                     33
      EQUIVALENCE (COMMEN_SFJP(78),CSFJP_SFAM_DZS),(MAP_SFJP(1,78),       33
     *MSFJP_SFAM_DZS)                                                     33
      CHARACTER CSFJP_SFAM_DZS*40/' '/                                    33
      CHARACTER NSFJP_SFAM_DZS*12/'R.SFAM_DZS'/                           33
      INTEGER MSFJP_SFAM_DZS(2)/1,1/                                      33
      REAL RE_SFJP_SFAM_DZS                                               33
C -                                                                       33
      REAL SFJP_SFLA_DX                                                   33
      EQUIVALENCE (BANK_SFJP(81),SFJP_SFLA_DX,RE_SFJP_SFLA_DX),           33
     *(NAMES_SFJP(79),NSFJP_SFLA_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(79),CSFJP_SFLA_DX),(MAP_SFJP(1,79),        33
     *MSFJP_SFLA_DX)                                                      33
      CHARACTER CSFJP_SFLA_DX*40/' '/                                     33
      CHARACTER NSFJP_SFLA_DX*12/'R.SFLA_DX'/                             33
      INTEGER MSFJP_SFLA_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFLA_DX                                                33
C -                                                                       33
      REAL SFJP_SFLA_DY                                                   33
      EQUIVALENCE (BANK_SFJP(82),SFJP_SFLA_DY,RE_SFJP_SFLA_DY),           33
     *(NAMES_SFJP(80),NSFJP_SFLA_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(80),CSFJP_SFLA_DY),(MAP_SFJP(1,80),        33
     *MSFJP_SFLA_DY)                                                      33
      CHARACTER CSFJP_SFLA_DY*40/' '/                                     33
      CHARACTER NSFJP_SFLA_DY*12/'R.SFLA_DY'/                             33
      INTEGER MSFJP_SFLA_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFLA_DY                                                33
C -                                                                       33
      REAL SFJP_SFLA_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(83),SFJP_SFLA_DZ,RE_SFJP_SFLA_DZ),           33
     *(NAMES_SFJP(81),NSFJP_SFLA_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(81),CSFJP_SFLA_DZ),(MAP_SFJP(1,81),        33
     *MSFJP_SFLA_DZ)                                                      33
      CHARACTER CSFJP_SFLA_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFLA_DZ*12/'R.SFLA_DZ'/                             33
      INTEGER MSFJP_SFLA_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFLA_DZ                                                33
C -                                                                       33
      REAL SFJP_SFLB_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(84),SFJP_SFLB_DZ,RE_SFJP_SFLB_DZ),           33
     *(NAMES_SFJP(82),NSFJP_SFLB_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(82),CSFJP_SFLB_DZ),(MAP_SFJP(1,82),        33
     *MSFJP_SFLB_DZ)                                                      33
      CHARACTER CSFJP_SFLB_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFLB_DZ*12/'R.SFLB_DZ'/                             33
      INTEGER MSFJP_SFLB_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFLB_DZ                                                33
C -                                                                       33
      REAL SFJP_SFLC_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(85),SFJP_SFLC_DZ,RE_SFJP_SFLC_DZ),           33
     *(NAMES_SFJP(83),NSFJP_SFLC_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(83),CSFJP_SFLC_DZ),(MAP_SFJP(1,83),        33
     *MSFJP_SFLC_DZ)                                                      33
      CHARACTER CSFJP_SFLC_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFLC_DZ*12/'R.SFLC_DZ'/                             33
      INTEGER MSFJP_SFLC_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFLC_DZ                                                33
C -                                                                       33
      REAL SFJP_SFEB_DX                                                   33
      EQUIVALENCE (BANK_SFJP(86),SFJP_SFEB_DX,RE_SFJP_SFEB_DX),           33
     *(NAMES_SFJP(84),NSFJP_SFEB_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(84),CSFJP_SFEB_DX),(MAP_SFJP(1,84),        33
     *MSFJP_SFEB_DX)                                                      33
      CHARACTER CSFJP_SFEB_DX*40/' '/                                     33
      CHARACTER NSFJP_SFEB_DX*12/'R.SFEB_DX'/                             33
      INTEGER MSFJP_SFEB_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFEB_DX                                                33
C -                                                                       33
      REAL SFJP_SFEB_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(87),SFJP_SFEB_DZ,RE_SFJP_SFEB_DZ),           33
     *(NAMES_SFJP(85),NSFJP_SFEB_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(85),CSFJP_SFEB_DZ),(MAP_SFJP(1,85),        33
     *MSFJP_SFEB_DZ)                                                      33
      CHARACTER CSFJP_SFEB_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFEB_DZ*12/'R.SFEB_DZ'/                             33
      INTEGER MSFJP_SFEB_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFEB_DZ                                                33
C -                                                                       33
      REAL SFJP_SFES_DX                                                   33
      EQUIVALENCE (BANK_SFJP(88),SFJP_SFES_DX,RE_SFJP_SFES_DX),           33
     *(NAMES_SFJP(86),NSFJP_SFES_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(86),CSFJP_SFES_DX),(MAP_SFJP(1,86),        33
     *MSFJP_SFES_DX)                                                      33
      CHARACTER CSFJP_SFES_DX*40/' '/                                     33
      CHARACTER NSFJP_SFES_DX*12/'R.SFES_DX'/                             33
      INTEGER MSFJP_SFES_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFES_DX                                                33
C -                                                                       33
      REAL SFJP_SFFK_DXE                                                  33
      EQUIVALENCE (BANK_SFJP(89),SFJP_SFFK_DXE,RE_SFJP_SFFK_DXE),         33
     *(NAMES_SFJP(87),NSFJP_SFFK_DXE)                                     33
      EQUIVALENCE (COMMEN_SFJP(87),CSFJP_SFFK_DXE),(MAP_SFJP(1,87),       33
     *MSFJP_SFFK_DXE)                                                     33
      CHARACTER CSFJP_SFFK_DXE*40/' '/                                    33
      CHARACTER NSFJP_SFFK_DXE*12/'R.SFFK_DXE'/                           33
      INTEGER MSFJP_SFFK_DXE(2)/1,1/                                      33
      REAL RE_SFJP_SFFK_DXE                                               33
C -                                                                       33
      REAL SFJP_SFFK_DY                                                   33
      EQUIVALENCE (BANK_SFJP(90),SFJP_SFFK_DY,RE_SFJP_SFFK_DY),           33
     *(NAMES_SFJP(88),NSFJP_SFFK_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(88),CSFJP_SFFK_DY),(MAP_SFJP(1,88),        33
     *MSFJP_SFFK_DY)                                                      33
      CHARACTER CSFJP_SFFK_DY*40/' '/                                     33
      CHARACTER NSFJP_SFFK_DY*12/'R.SFFK_DY'/                             33
      INTEGER MSFJP_SFFK_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFFK_DY                                                33
C -                                                                       33
      REAL SFJP_SFFX_DYE                                                  33
      EQUIVALENCE (BANK_SFJP(91),SFJP_SFFX_DYE,RE_SFJP_SFFX_DYE),         33
     *(NAMES_SFJP(89),NSFJP_SFFX_DYE)                                     33
      EQUIVALENCE (COMMEN_SFJP(89),CSFJP_SFFX_DYE),(MAP_SFJP(1,89),       33
     *MSFJP_SFFX_DYE)                                                     33
      CHARACTER CSFJP_SFFX_DYE*40/' '/                                    33
      CHARACTER NSFJP_SFFX_DYE*12/'R.SFFX_DYE'/                           33
      INTEGER MSFJP_SFFX_DYE(2)/1,1/                                      33
      REAL RE_SFJP_SFFX_DYE                                               33
C -                                                                       33
      REAL SFJP_SFFK_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(92),SFJP_SFFK_DZ,RE_SFJP_SFFK_DZ),           33
     *(NAMES_SFJP(90),NSFJP_SFFK_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(90),CSFJP_SFFK_DZ),(MAP_SFJP(1,90),        33
     *MSFJP_SFFK_DZ)                                                      33
      CHARACTER CSFJP_SFFK_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFFK_DZ*12/'R.SFFK_DZ'/                             33
      INTEGER MSFJP_SFFK_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFFK_DZ                                                33
C -                                                                       33
      REAL SFJP_SFFL_DX                                                   33
      EQUIVALENCE (BANK_SFJP(93),SFJP_SFFL_DX,RE_SFJP_SFFL_DX),           33
     *(NAMES_SFJP(91),NSFJP_SFFL_DX)                                      33
      EQUIVALENCE (COMMEN_SFJP(91),CSFJP_SFFL_DX),(MAP_SFJP(1,91),        33
     *MSFJP_SFFL_DX)                                                      33
      CHARACTER CSFJP_SFFL_DX*40/' '/                                     33
      CHARACTER NSFJP_SFFL_DX*12/'R.SFFL_DX'/                             33
      INTEGER MSFJP_SFFL_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFFL_DX                                                33
C -                                                                       33
      REAL SFJP_SFFK_DXZ                                                  33
      EQUIVALENCE (BANK_SFJP(94),SFJP_SFFK_DXZ,RE_SFJP_SFFK_DXZ),         33
     *(NAMES_SFJP(92),NSFJP_SFFK_DXZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(92),CSFJP_SFFK_DXZ),(MAP_SFJP(1,92),       33
     *MSFJP_SFFK_DXZ)                                                     33
      CHARACTER CSFJP_SFFK_DXZ*40/' '/                                    33
      CHARACTER NSFJP_SFFK_DXZ*12/'R.SFFK_DXZ'/                           33
      INTEGER MSFJP_SFFK_DXZ(2)/1,1/                                      33
      REAL RE_SFJP_SFFK_DXZ                                               33
C -                                                                       33
      REAL SFJP_SFFK_PX                                                   33
      EQUIVALENCE (BANK_SFJP(95),SFJP_SFFK_PX,RE_SFJP_SFFK_PX),           33
     *(NAMES_SFJP(93),NSFJP_SFFK_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(93),CSFJP_SFFK_PX),(MAP_SFJP(1,93),        33
     *MSFJP_SFFK_PX)                                                      33
      CHARACTER CSFJP_SFFK_PX*40/' '/                                     33
      CHARACTER NSFJP_SFFK_PX*12/'R.SFFK_PX'/                             33
      INTEGER MSFJP_SFFK_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFFK_PX                                                33
C -                                                                       33
      REAL SFJP_SFFK_PY1                                                  33
      EQUIVALENCE (BANK_SFJP(96),SFJP_SFFK_PY1,RE_SFJP_SFFK_PY1),         33
     *(NAMES_SFJP(94),NSFJP_SFFK_PY1)                                     33
      EQUIVALENCE (COMMEN_SFJP(94),CSFJP_SFFK_PY1),(MAP_SFJP(1,94),       33
     *MSFJP_SFFK_PY1)                                                     33
      CHARACTER CSFJP_SFFK_PY1*40/' '/                                    33
      CHARACTER NSFJP_SFFK_PY1*12/'R.SFFK_PY1'/                           33
      INTEGER MSFJP_SFFK_PY1(2)/1,1/                                      33
      REAL RE_SFJP_SFFK_PY1                                               33
C -                                                                       33
      REAL SFJP_SFFK_PY2                                                  33
      EQUIVALENCE (BANK_SFJP(97),SFJP_SFFK_PY2,RE_SFJP_SFFK_PY2),         33
     *(NAMES_SFJP(95),NSFJP_SFFK_PY2)                                     33
      EQUIVALENCE (COMMEN_SFJP(95),CSFJP_SFFK_PY2),(MAP_SFJP(1,95),       33
     *MSFJP_SFFK_PY2)                                                     33
      CHARACTER CSFJP_SFFK_PY2*40/' '/                                    33
      CHARACTER NSFJP_SFFK_PY2*12/'R.SFFK_PY2'/                           33
      INTEGER MSFJP_SFFK_PY2(2)/1,1/                                      33
      REAL RE_SFJP_SFFK_PY2                                               33
C -                                                                       33
      REAL SFJP_SFKL_PX                                                   33
      EQUIVALENCE (BANK_SFJP(98),SFJP_SFKL_PX,RE_SFJP_SFKL_PX),           33
     *(NAMES_SFJP(96),NSFJP_SFKL_PX)                                      33
      EQUIVALENCE (COMMEN_SFJP(96),CSFJP_SFKL_PX),(MAP_SFJP(1,96),        33
     *MSFJP_SFKL_PX)                                                      33
      CHARACTER CSFJP_SFKL_PX*40/' '/                                     33
      CHARACTER NSFJP_SFKL_PX*12/'R.SFKL_PX'/                             33
      INTEGER MSFJP_SFKL_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFKL_PX                                                33
C -                                                                       33
      REAL SFJP_SFKK_DY                                                   33
      EQUIVALENCE (BANK_SFJP(99),SFJP_SFKK_DY,RE_SFJP_SFKK_DY),           33
     *(NAMES_SFJP(97),NSFJP_SFKK_DY)                                      33
      EQUIVALENCE (COMMEN_SFJP(97),CSFJP_SFKK_DY),(MAP_SFJP(1,97),        33
     *MSFJP_SFKK_DY)                                                      33
      CHARACTER CSFJP_SFKK_DY*40/' '/                                     33
      CHARACTER NSFJP_SFKK_DY*12/'R.SFKK_DY'/                             33
      INTEGER MSFJP_SFKK_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFKK_DY                                                33
C -                                                                       33
      REAL SFJP_SFLU_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(100),SFJP_SFLU_DZ,RE_SFJP_SFLU_DZ),          33
     *(NAMES_SFJP(98),NSFJP_SFLU_DZ)                                      33
      EQUIVALENCE (COMMEN_SFJP(98),CSFJP_SFLU_DZ),(MAP_SFJP(1,98),        33
     *MSFJP_SFLU_DZ)                                                      33
      CHARACTER CSFJP_SFLU_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFLU_DZ*12/'R.SFLU_DZ'/                             33
      INTEGER MSFJP_SFLU_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFLU_DZ                                                33
C -                                                                       33
      REAL SFJP_SFLU_H1                                                   33
      EQUIVALENCE (BANK_SFJP(101),SFJP_SFLU_H1,RE_SFJP_SFLU_H1),          33
     *(NAMES_SFJP(99),NSFJP_SFLU_H1)                                      33
      EQUIVALENCE (COMMEN_SFJP(99),CSFJP_SFLU_H1),(MAP_SFJP(1,99),        33
     *MSFJP_SFLU_H1)                                                      33
      CHARACTER CSFJP_SFLU_H1*40/' '/                                     33
      CHARACTER NSFJP_SFLU_H1*12/'R.SFLU_H1'/                             33
      INTEGER MSFJP_SFLU_H1(2)/1,1/                                       33
      REAL RE_SFJP_SFLU_H1                                                33
C -                                                                       33
      REAL SFJP_SFLU_BL1                                                  33
      EQUIVALENCE (BANK_SFJP(102),SFJP_SFLU_BL1,RE_SFJP_SFLU_BL1),        33
     *(NAMES_SFJP(100),NSFJP_SFLU_BL1)                                    33
      EQUIVALENCE (COMMEN_SFJP(100),CSFJP_SFLU_BL1),(MAP_SFJP(1,100),     33
     *MSFJP_SFLU_BL1)                                                     33
      CHARACTER CSFJP_SFLU_BL1*40/' '/                                    33
      CHARACTER NSFJP_SFLU_BL1*12/'R.SFLU_BL1'/                           33
      INTEGER MSFJP_SFLU_BL1(2)/1,1/                                      33
      REAL RE_SFJP_SFLU_BL1                                               33
C -                                                                       33
      REAL SFJP_SFRA_DX                                                   33
      EQUIVALENCE (BANK_SFJP(103),SFJP_SFRA_DX,RE_SFJP_SFRA_DX),          33
     *(NAMES_SFJP(101),NSFJP_SFRA_DX)                                     33
      EQUIVALENCE (COMMEN_SFJP(101),CSFJP_SFRA_DX),(MAP_SFJP(1,101),      33
     *MSFJP_SFRA_DX)                                                      33
      CHARACTER CSFJP_SFRA_DX*40/' '/                                     33
      CHARACTER NSFJP_SFRA_DX*12/'R.SFRA_DX'/                             33
      INTEGER MSFJP_SFRA_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFRA_DX                                                33
C -                                                                       33
      REAL SFJP_SFRA_DY                                                   33
      EQUIVALENCE (BANK_SFJP(104),SFJP_SFRA_DY,RE_SFJP_SFRA_DY),          33
     *(NAMES_SFJP(102),NSFJP_SFRA_DY)                                     33
      EQUIVALENCE (COMMEN_SFJP(102),CSFJP_SFRA_DY),(MAP_SFJP(1,102),      33
     *MSFJP_SFRA_DY)                                                      33
      CHARACTER CSFJP_SFRA_DY*40/' '/                                     33
      CHARACTER NSFJP_SFRA_DY*12/'R.SFRA_DY'/                             33
      INTEGER MSFJP_SFRA_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFRA_DY                                                33
C -                                                                       33
      REAL SFJP_SFRA_PY                                                   33
      EQUIVALENCE (BANK_SFJP(105),SFJP_SFRA_PY,RE_SFJP_SFRA_PY),          33
     *(NAMES_SFJP(103),NSFJP_SFRA_PY)                                     33
      EQUIVALENCE (COMMEN_SFJP(103),CSFJP_SFRA_PY),(MAP_SFJP(1,103),      33
     *MSFJP_SFRA_PY)                                                      33
      CHARACTER CSFJP_SFRA_PY*40/' '/                                     33
      CHARACTER NSFJP_SFRA_PY*12/'R.SFRA_PY'/                             33
      INTEGER MSFJP_SFRA_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFRA_PY                                                33
C -                                                                       33
      REAL SFJP_SFRA_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(106),SFJP_SFRA_DZ,RE_SFJP_SFRA_DZ),          33
     *(NAMES_SFJP(104),NSFJP_SFRA_DZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(104),CSFJP_SFRA_DZ),(MAP_SFJP(1,104),      33
     *MSFJP_SFRA_DZ)                                                      33
      CHARACTER CSFJP_SFRA_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFRA_DZ*12/'R.SFRA_DZ'/                             33
      INTEGER MSFJP_SFRA_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFRA_DZ                                                33
C -                                                                       33
      REAL SFJP_SFRA_PZ                                                   33
      EQUIVALENCE (BANK_SFJP(107),SFJP_SFRA_PZ,RE_SFJP_SFRA_PZ),          33
     *(NAMES_SFJP(105),NSFJP_SFRA_PZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(105),CSFJP_SFRA_PZ),(MAP_SFJP(1,105),      33
     *MSFJP_SFRA_PZ)                                                      33
      CHARACTER CSFJP_SFRA_PZ*40/' '/                                     33
      CHARACTER NSFJP_SFRA_PZ*12/'R.SFRA_PZ'/                             33
      INTEGER MSFJP_SFRA_PZ(2)/1,1/                                       33
      REAL RE_SFJP_SFRA_PZ                                                33
C -                                                                       33
      REAL SFJP_SFSW_DY                                                   33
      EQUIVALENCE (BANK_SFJP(108),SFJP_SFSW_DY,RE_SFJP_SFSW_DY),          33
     *(NAMES_SFJP(106),NSFJP_SFSW_DY)                                     33
      EQUIVALENCE (COMMEN_SFJP(106),CSFJP_SFSW_DY),(MAP_SFJP(1,106),      33
     *MSFJP_SFSW_DY)                                                      33
      CHARACTER CSFJP_SFSW_DY*40/' '/                                     33
      CHARACTER NSFJP_SFSW_DY*12/'R.SFSW_DY'/                             33
      INTEGER MSFJP_SFSW_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFSW_DY                                                33
C -                                                                       33
      REAL SFJP_SFSW_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(109),SFJP_SFSW_DZ,RE_SFJP_SFSW_DZ),          33
     *(NAMES_SFJP(107),NSFJP_SFSW_DZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(107),CSFJP_SFSW_DZ),(MAP_SFJP(1,107),      33
     *MSFJP_SFSW_DZ)                                                      33
      CHARACTER CSFJP_SFSW_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFSW_DZ*12/'R.SFSW_DZ'/                             33
      INTEGER MSFJP_SFSW_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFSW_DZ                                                33
C -                                                                       33
      REAL SFJP_SFRS_DX                                                   33
      EQUIVALENCE (BANK_SFJP(110),SFJP_SFRS_DX,RE_SFJP_SFRS_DX),          33
     *(NAMES_SFJP(108),NSFJP_SFRS_DX)                                     33
      EQUIVALENCE (COMMEN_SFJP(108),CSFJP_SFRS_DX),(MAP_SFJP(1,108),      33
     *MSFJP_SFRS_DX)                                                      33
      CHARACTER CSFJP_SFRS_DX*40/' '/                                     33
      CHARACTER NSFJP_SFRS_DX*12/'R.SFRS_DX'/                             33
      INTEGER MSFJP_SFRS_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFRS_DX                                                33
C -                                                                       33
      REAL SFJP_SFRS_DY                                                   33
      EQUIVALENCE (BANK_SFJP(111),SFJP_SFRS_DY,RE_SFJP_SFRS_DY),          33
     *(NAMES_SFJP(109),NSFJP_SFRS_DY)                                     33
      EQUIVALENCE (COMMEN_SFJP(109),CSFJP_SFRS_DY),(MAP_SFJP(1,109),      33
     *MSFJP_SFRS_DY)                                                      33
      CHARACTER CSFJP_SFRS_DY*40/' '/                                     33
      CHARACTER NSFJP_SFRS_DY*12/'R.SFRS_DY'/                             33
      INTEGER MSFJP_SFRS_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFRS_DY                                                33
C -                                                                       33
      REAL SFJP_SFRS_PX                                                   33
      EQUIVALENCE (BANK_SFJP(112),SFJP_SFRS_PX,RE_SFJP_SFRS_PX),          33
     *(NAMES_SFJP(110),NSFJP_SFRS_PX)                                     33
      EQUIVALENCE (COMMEN_SFJP(110),CSFJP_SFRS_PX),(MAP_SFJP(1,110),      33
     *MSFJP_SFRS_PX)                                                      33
      CHARACTER CSFJP_SFRS_PX*40/' '/                                     33
      CHARACTER NSFJP_SFRS_PX*12/'R.SFRS_PX'/                             33
      INTEGER MSFJP_SFRS_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFRS_PX                                                33
C -                                                                       33
      REAL SFJP_SFRS_PY                                                   33
      EQUIVALENCE (BANK_SFJP(113),SFJP_SFRS_PY,RE_SFJP_SFRS_PY),          33
     *(NAMES_SFJP(111),NSFJP_SFRS_PY)                                     33
      EQUIVALENCE (COMMEN_SFJP(111),CSFJP_SFRS_PY),(MAP_SFJP(1,111),      33
     *MSFJP_SFRS_PY)                                                      33
      CHARACTER CSFJP_SFRS_PY*40/' '/                                     33
      CHARACTER NSFJP_SFRS_PY*12/'R.SFRS_PY'/                             33
      INTEGER MSFJP_SFRS_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFRS_PY                                                33
C -                                                                       33
      REAL SFJP_SFSM_LL                                                   33
      EQUIVALENCE (BANK_SFJP(114),SFJP_SFSM_LL,RE_SFJP_SFSM_LL),          33
     *(NAMES_SFJP(112),NSFJP_SFSM_LL)                                     33
      EQUIVALENCE (COMMEN_SFJP(112),CSFJP_SFSM_LL),(MAP_SFJP(1,112),      33
     *MSFJP_SFSM_LL)                                                      33
      CHARACTER CSFJP_SFSM_LL*40/' '/                                     33
      CHARACTER NSFJP_SFSM_LL*12/'R.SFSM_LL'/                             33
      INTEGER MSFJP_SFSM_LL(2)/1,1/                                       33
      REAL RE_SFJP_SFSM_LL                                                33
C -                                                                       33
      REAL SFJP_SFFX_DX                                                   33
      EQUIVALENCE (BANK_SFJP(115),SFJP_SFFX_DX,RE_SFJP_SFFX_DX),          33
     *(NAMES_SFJP(113),NSFJP_SFFX_DX)                                     33
      EQUIVALENCE (COMMEN_SFJP(113),CSFJP_SFFX_DX),(MAP_SFJP(1,113),      33
     *MSFJP_SFFX_DX)                                                      33
      CHARACTER CSFJP_SFFX_DX*40/' '/                                     33
      CHARACTER NSFJP_SFFX_DX*12/'R.SFFX_DX'/                             33
      INTEGER MSFJP_SFFX_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFFX_DX                                                33
C -                                                                       33
      REAL SFJP_SFFX_DYZ                                                  33
      EQUIVALENCE (BANK_SFJP(116),SFJP_SFFX_DYZ,RE_SFJP_SFFX_DYZ),        33
     *(NAMES_SFJP(114),NSFJP_SFFX_DYZ)                                    33
      EQUIVALENCE (COMMEN_SFJP(114),CSFJP_SFFX_DYZ),(MAP_SFJP(1,114),     33
     *MSFJP_SFFX_DYZ)                                                     33
      CHARACTER CSFJP_SFFX_DYZ*40/' '/                                    33
      CHARACTER NSFJP_SFFX_DYZ*12/'R.SFFX_DYZ'/                           33
      INTEGER MSFJP_SFFX_DYZ(2)/1,1/                                      33
      REAL RE_SFJP_SFFX_DYZ                                               33
C -                                                                       33
      REAL SFJP_SFPI_RMIN                                                 33
      EQUIVALENCE (BANK_SFJP(117),SFJP_SFPI_RMIN,RE_SFJP_SFPI_RMIN),      33
     *(NAMES_SFJP(115),NSFJP_SFPI_RMIN)                                   33
      EQUIVALENCE (COMMEN_SFJP(115),CSFJP_SFPI_RMIN),(MAP_SFJP(1,115),    33
     *MSFJP_SFPI_RMIN)                                                    33
      CHARACTER CSFJP_SFPI_RMIN*40/' '/                                   33
      CHARACTER NSFJP_SFPI_RMIN*12/'R.SFPI_RMIN'/                         33
      INTEGER MSFJP_SFPI_RMIN(2)/1,1/                                     33
      REAL RE_SFJP_SFPI_RMIN                                              33
C -                                                                       33
      REAL SFJP_SFPI_RMAX                                                 33
      EQUIVALENCE (BANK_SFJP(118),SFJP_SFPI_RMAX,RE_SFJP_SFPI_RMAX),      33
     *(NAMES_SFJP(116),NSFJP_SFPI_RMAX)                                   33
      EQUIVALENCE (COMMEN_SFJP(116),CSFJP_SFPI_RMAX),(MAP_SFJP(1,116),    33
     *MSFJP_SFPI_RMAX)                                                    33
      CHARACTER CSFJP_SFPI_RMAX*40/' '/                                   33
      CHARACTER NSFJP_SFPI_RMAX*12/'R.SFPI_RMAX'/                         33
      INTEGER MSFJP_SFPI_RMAX(2)/1,1/                                     33
      REAL RE_SFJP_SFPI_RMAX                                              33
C -                                                                       33
      REAL SFJP_SFPI_PX                                                   33
      EQUIVALENCE (BANK_SFJP(119),SFJP_SFPI_PX,RE_SFJP_SFPI_PX),          33
     *(NAMES_SFJP(117),NSFJP_SFPI_PX)                                     33
      EQUIVALENCE (COMMEN_SFJP(117),CSFJP_SFPI_PX),(MAP_SFJP(1,117),      33
     *MSFJP_SFPI_PX)                                                      33
      CHARACTER CSFJP_SFPI_PX*40/' '/                                     33
      CHARACTER NSFJP_SFPI_PX*12/'R.SFPI_PX'/                             33
      INTEGER MSFJP_SFPI_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFPI_PX                                                33
C -                                                                       33
      REAL SFJP_SFPI_PY1                                                  33
      EQUIVALENCE (BANK_SFJP(120),SFJP_SFPI_PY1,RE_SFJP_SFPI_PY1),        33
     *(NAMES_SFJP(118),NSFJP_SFPI_PY1)                                    33
      EQUIVALENCE (COMMEN_SFJP(118),CSFJP_SFPI_PY1),(MAP_SFJP(1,118),     33
     *MSFJP_SFPI_PY1)                                                     33
      CHARACTER CSFJP_SFPI_PY1*40/' '/                                    33
      CHARACTER NSFJP_SFPI_PY1*12/'R.SFPI_PY1'/                           33
      INTEGER MSFJP_SFPI_PY1(2)/1,1/                                      33
      REAL RE_SFJP_SFPI_PY1                                               33
C -                                                                       33
      REAL SFJP_SFPI_PY2                                                  33
      EQUIVALENCE (BANK_SFJP(121),SFJP_SFPI_PY2,RE_SFJP_SFPI_PY2),        33
     *(NAMES_SFJP(119),NSFJP_SFPI_PY2)                                    33
      EQUIVALENCE (COMMEN_SFJP(119),CSFJP_SFPI_PY2),(MAP_SFJP(1,119),     33
     *MSFJP_SFPI_PY2)                                                     33
      CHARACTER CSFJP_SFPI_PY2*40/' '/                                    33
      CHARACTER NSFJP_SFPI_PY2*12/'R.SFPI_PY2'/                           33
      INTEGER MSFJP_SFPI_PY2(2)/1,1/                                      33
      REAL RE_SFJP_SFPI_PY2                                               33
C -                                                                       33
      REAL SFJP_SFPI_PZ                                                   33
      EQUIVALENCE (BANK_SFJP(122),SFJP_SFPI_PZ,RE_SFJP_SFPI_PZ),          33
     *(NAMES_SFJP(120),NSFJP_SFPI_PZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(120),CSFJP_SFPI_PZ),(MAP_SFJP(1,120),      33
     *MSFJP_SFPI_PZ)                                                      33
      CHARACTER CSFJP_SFPI_PZ*40/' '/                                     33
      CHARACTER NSFJP_SFPI_PZ*12/'R.SFPI_PZ'/                             33
      INTEGER MSFJP_SFPI_PZ(2)/1,1/                                       33
      REAL RE_SFJP_SFPI_PZ                                                33
C -                                                                       33
      REAL SFJP_SFPJ_DX1                                                  33
      EQUIVALENCE (BANK_SFJP(123),SFJP_SFPJ_DX1,RE_SFJP_SFPJ_DX1),        33
     *(NAMES_SFJP(121),NSFJP_SFPJ_DX1)                                    33
      EQUIVALENCE (COMMEN_SFJP(121),CSFJP_SFPJ_DX1),(MAP_SFJP(1,121),     33
     *MSFJP_SFPJ_DX1)                                                     33
      CHARACTER CSFJP_SFPJ_DX1*40/' '/                                    33
      CHARACTER NSFJP_SFPJ_DX1*12/'R.SFPJ_DX1'/                           33
      INTEGER MSFJP_SFPJ_DX1(2)/1,1/                                      33
      REAL RE_SFJP_SFPJ_DX1                                               33
C -                                                                       33
      REAL SFJP_SFPJ_DX2                                                  33
      EQUIVALENCE (BANK_SFJP(124),SFJP_SFPJ_DX2,RE_SFJP_SFPJ_DX2),        33
     *(NAMES_SFJP(122),NSFJP_SFPJ_DX2)                                    33
      EQUIVALENCE (COMMEN_SFJP(122),CSFJP_SFPJ_DX2),(MAP_SFJP(1,122),     33
     *MSFJP_SFPJ_DX2)                                                     33
      CHARACTER CSFJP_SFPJ_DX2*40/' '/                                    33
      CHARACTER NSFJP_SFPJ_DX2*12/'R.SFPJ_DX2'/                           33
      INTEGER MSFJP_SFPJ_DX2(2)/1,1/                                      33
      REAL RE_SFJP_SFPJ_DX2                                               33
C -                                                                       33
      REAL SFJP_SFPJ_DY                                                   33
      EQUIVALENCE (BANK_SFJP(125),SFJP_SFPJ_DY,RE_SFJP_SFPJ_DY),          33
     *(NAMES_SFJP(123),NSFJP_SFPJ_DY)                                     33
      EQUIVALENCE (COMMEN_SFJP(123),CSFJP_SFPJ_DY),(MAP_SFJP(1,123),      33
     *MSFJP_SFPJ_DY)                                                      33
      CHARACTER CSFJP_SFPJ_DY*40/' '/                                     33
      CHARACTER NSFJP_SFPJ_DY*12/'R.SFPJ_DY'/                             33
      INTEGER MSFJP_SFPJ_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFPJ_DY                                                33
C -                                                                       33
      REAL SFJP_SFPJ_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(126),SFJP_SFPJ_DZ,RE_SFJP_SFPJ_DZ),          33
     *(NAMES_SFJP(124),NSFJP_SFPJ_DZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(124),CSFJP_SFPJ_DZ),(MAP_SFJP(1,124),      33
     *MSFJP_SFPJ_DZ)                                                      33
      CHARACTER CSFJP_SFPJ_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFPJ_DZ*12/'R.SFPJ_DZ'/                             33
      INTEGER MSFJP_SFPJ_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFPJ_DZ                                                33
C -                                                                       33
      REAL SFJP_SFAA_DX                                                   33
      EQUIVALENCE (BANK_SFJP(127),SFJP_SFAA_DX,RE_SFJP_SFAA_DX),          33
     *(NAMES_SFJP(125),NSFJP_SFAA_DX)                                     33
      EQUIVALENCE (COMMEN_SFJP(125),CSFJP_SFAA_DX),(MAP_SFJP(1,125),      33
     *MSFJP_SFAA_DX)                                                      33
      CHARACTER CSFJP_SFAA_DX*40/' '/                                     33
      CHARACTER NSFJP_SFAA_DX*12/'R.SFAA_DX'/                             33
      INTEGER MSFJP_SFAA_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFAA_DX                                                33
C -                                                                       33
      REAL SFJP_SFAA_DY                                                   33
      EQUIVALENCE (BANK_SFJP(128),SFJP_SFAA_DY,RE_SFJP_SFAA_DY),          33
     *(NAMES_SFJP(126),NSFJP_SFAA_DY)                                     33
      EQUIVALENCE (COMMEN_SFJP(126),CSFJP_SFAA_DY),(MAP_SFJP(1,126),      33
     *MSFJP_SFAA_DY)                                                      33
      CHARACTER CSFJP_SFAA_DY*40/' '/                                     33
      CHARACTER NSFJP_SFAA_DY*12/'R.SFAA_DY'/                             33
      INTEGER MSFJP_SFAA_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFAA_DY                                                33
C -                                                                       33
      REAL SFJP_SFAA_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(129),SFJP_SFAA_DZ,RE_SFJP_SFAA_DZ),          33
     *(NAMES_SFJP(127),NSFJP_SFAA_DZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(127),CSFJP_SFAA_DZ),(MAP_SFJP(1,127),      33
     *MSFJP_SFAA_DZ)                                                      33
      CHARACTER CSFJP_SFAA_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFAA_DZ*12/'R.SFAA_DZ'/                             33
      INTEGER MSFJP_SFAA_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFAA_DZ                                                33
C -                                                                       33
      REAL SFJP_SFAA_PX1                                                  33
      EQUIVALENCE (BANK_SFJP(130),SFJP_SFAA_PX1,RE_SFJP_SFAA_PX1),        33
     *(NAMES_SFJP(128),NSFJP_SFAA_PX1)                                    33
      EQUIVALENCE (COMMEN_SFJP(128),CSFJP_SFAA_PX1),(MAP_SFJP(1,128),     33
     *MSFJP_SFAA_PX1)                                                     33
      CHARACTER CSFJP_SFAA_PX1*40/' '/                                    33
      CHARACTER NSFJP_SFAA_PX1*12/'R.SFAA_PX1'/                           33
      INTEGER MSFJP_SFAA_PX1(2)/1,1/                                      33
      REAL RE_SFJP_SFAA_PX1                                               33
C -                                                                       33
      REAL SFJP_SFAA_PX2                                                  33
      EQUIVALENCE (BANK_SFJP(131),SFJP_SFAA_PX2,RE_SFJP_SFAA_PX2),        33
     *(NAMES_SFJP(129),NSFJP_SFAA_PX2)                                    33
      EQUIVALENCE (COMMEN_SFJP(129),CSFJP_SFAA_PX2),(MAP_SFJP(1,129),     33
     *MSFJP_SFAA_PX2)                                                     33
      CHARACTER CSFJP_SFAA_PX2*40/' '/                                    33
      CHARACTER NSFJP_SFAA_PX2*12/'R.SFAA_PX2'/                           33
      INTEGER MSFJP_SFAA_PX2(2)/1,1/                                      33
      REAL RE_SFJP_SFAA_PX2                                               33
C -                                                                       33
      REAL SFJP_SFAA_PX3                                                  33
      EQUIVALENCE (BANK_SFJP(132),SFJP_SFAA_PX3,RE_SFJP_SFAA_PX3),        33
     *(NAMES_SFJP(130),NSFJP_SFAA_PX3)                                    33
      EQUIVALENCE (COMMEN_SFJP(130),CSFJP_SFAA_PX3),(MAP_SFJP(1,130),     33
     *MSFJP_SFAA_PX3)                                                     33
      CHARACTER CSFJP_SFAA_PX3*40/' '/                                    33
      CHARACTER NSFJP_SFAA_PX3*12/'R.SFAA_PX3'/                           33
      INTEGER MSFJP_SFAA_PX3(2)/1,1/                                      33
      REAL RE_SFJP_SFAA_PX3                                               33
C -                                                                       33
      REAL SFJP_SFAA_PZ1                                                  33
      EQUIVALENCE (BANK_SFJP(133),SFJP_SFAA_PZ1,RE_SFJP_SFAA_PZ1),        33
     *(NAMES_SFJP(131),NSFJP_SFAA_PZ1)                                    33
      EQUIVALENCE (COMMEN_SFJP(131),CSFJP_SFAA_PZ1),(MAP_SFJP(1,131),     33
     *MSFJP_SFAA_PZ1)                                                     33
      CHARACTER CSFJP_SFAA_PZ1*40/' '/                                    33
      CHARACTER NSFJP_SFAA_PZ1*12/'R.SFAA_PZ1'/                           33
      INTEGER MSFJP_SFAA_PZ1(2)/1,1/                                      33
      REAL RE_SFJP_SFAA_PZ1                                               33
C -                                                                       33
      REAL SFJP_SFAA_PZ2                                                  33
      EQUIVALENCE (BANK_SFJP(134),SFJP_SFAA_PZ2,RE_SFJP_SFAA_PZ2),        33
     *(NAMES_SFJP(132),NSFJP_SFAA_PZ2)                                    33
      EQUIVALENCE (COMMEN_SFJP(132),CSFJP_SFAA_PZ2),(MAP_SFJP(1,132),     33
     *MSFJP_SFAA_PZ2)                                                     33
      CHARACTER CSFJP_SFAA_PZ2*40/' '/                                    33
      CHARACTER NSFJP_SFAA_PZ2*12/'R.SFAA_PZ2'/                           33
      INTEGER MSFJP_SFAA_PZ2(2)/1,1/                                      33
      REAL RE_SFJP_SFAA_PZ2                                               33
C -                                                                       33
      REAL SFJP_SFAA_PZ3                                                  33
      EQUIVALENCE (BANK_SFJP(135),SFJP_SFAA_PZ3,RE_SFJP_SFAA_PZ3),        33
     *(NAMES_SFJP(133),NSFJP_SFAA_PZ3)                                    33
      EQUIVALENCE (COMMEN_SFJP(133),CSFJP_SFAA_PZ3),(MAP_SFJP(1,133),     33
     *MSFJP_SFAA_PZ3)                                                     33
      CHARACTER CSFJP_SFAA_PZ3*40/' '/                                    33
      CHARACTER NSFJP_SFAA_PZ3*12/'R.SFAA_PZ3'/                           33
      INTEGER MSFJP_SFAA_PZ3(2)/1,1/                                      33
      REAL RE_SFJP_SFAA_PZ3                                               33
C -                                                                       33
      REAL SFJP_SFSL_DX                                                   33
      EQUIVALENCE (BANK_SFJP(136),SFJP_SFSL_DX,RE_SFJP_SFSL_DX),          33
     *(NAMES_SFJP(134),NSFJP_SFSL_DX)                                     33
      EQUIVALENCE (COMMEN_SFJP(134),CSFJP_SFSL_DX),(MAP_SFJP(1,134),      33
     *MSFJP_SFSL_DX)                                                      33
      CHARACTER CSFJP_SFSL_DX*40/' '/                                     33
      CHARACTER NSFJP_SFSL_DX*12/'R.SFSL_DX'/                             33
      INTEGER MSFJP_SFSL_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFSL_DX                                                33
C -                                                                       33
      REAL SFJP_SFSL_DY                                                   33
      EQUIVALENCE (BANK_SFJP(137),SFJP_SFSL_DY,RE_SFJP_SFSL_DY),          33
     *(NAMES_SFJP(135),NSFJP_SFSL_DY)                                     33
      EQUIVALENCE (COMMEN_SFJP(135),CSFJP_SFSL_DY),(MAP_SFJP(1,135),      33
     *MSFJP_SFSL_DY)                                                      33
      CHARACTER CSFJP_SFSL_DY*40/' '/                                     33
      CHARACTER NSFJP_SFSL_DY*12/'R.SFSL_DY'/                             33
      INTEGER MSFJP_SFSL_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFSL_DY                                                33
C -                                                                       33
      REAL SFJP_SFSL_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(138),SFJP_SFSL_DZ,RE_SFJP_SFSL_DZ),          33
     *(NAMES_SFJP(136),NSFJP_SFSL_DZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(136),CSFJP_SFSL_DZ),(MAP_SFJP(1,136),      33
     *MSFJP_SFSL_DZ)                                                      33
      CHARACTER CSFJP_SFSL_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFSL_DZ*12/'R.SFSL_DZ'/                             33
      INTEGER MSFJP_SFSL_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFSL_DZ                                                33
C -                                                                       33
      REAL SFJP_SFSD_DX                                                   33
      EQUIVALENCE (BANK_SFJP(139),SFJP_SFSD_DX,RE_SFJP_SFSD_DX),          33
     *(NAMES_SFJP(137),NSFJP_SFSD_DX)                                     33
      EQUIVALENCE (COMMEN_SFJP(137),CSFJP_SFSD_DX),(MAP_SFJP(1,137),      33
     *MSFJP_SFSD_DX)                                                      33
      CHARACTER CSFJP_SFSD_DX*40/' '/                                     33
      CHARACTER NSFJP_SFSD_DX*12/'R.SFSD_DX'/                             33
      INTEGER MSFJP_SFSD_DX(2)/1,1/                                       33
      REAL RE_SFJP_SFSD_DX                                                33
C -                                                                       33
      REAL SFJP_SFSD_DY                                                   33
      EQUIVALENCE (BANK_SFJP(140),SFJP_SFSD_DY,RE_SFJP_SFSD_DY),          33
     *(NAMES_SFJP(138),NSFJP_SFSD_DY)                                     33
      EQUIVALENCE (COMMEN_SFJP(138),CSFJP_SFSD_DY),(MAP_SFJP(1,138),      33
     *MSFJP_SFSD_DY)                                                      33
      CHARACTER CSFJP_SFSD_DY*40/' '/                                     33
      CHARACTER NSFJP_SFSD_DY*12/'R.SFSD_DY'/                             33
      INTEGER MSFJP_SFSD_DY(2)/1,1/                                       33
      REAL RE_SFJP_SFSD_DY                                                33
C -                                                                       33
      REAL SFJP_SFSD_DZ                                                   33
      EQUIVALENCE (BANK_SFJP(141),SFJP_SFSD_DZ,RE_SFJP_SFSD_DZ),          33
     *(NAMES_SFJP(139),NSFJP_SFSD_DZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(139),CSFJP_SFSD_DZ),(MAP_SFJP(1,139),      33
     *MSFJP_SFSD_DZ)                                                      33
      CHARACTER CSFJP_SFSD_DZ*40/' '/                                     33
      CHARACTER NSFJP_SFSD_DZ*12/'R.SFSD_DZ'/                             33
      INTEGER MSFJP_SFSD_DZ(2)/1,1/                                       33
      REAL RE_SFJP_SFSD_DZ                                                33
C -                                                                       33
      REAL SFJP_SFLA_PX                                                   33
      EQUIVALENCE (BANK_SFJP(142),SFJP_SFLA_PX,RE_SFJP_SFLA_PX),          33
     *(NAMES_SFJP(140),NSFJP_SFLA_PX)                                     33
      EQUIVALENCE (COMMEN_SFJP(140),CSFJP_SFLA_PX),(MAP_SFJP(1,140),      33
     *MSFJP_SFLA_PX)                                                      33
      CHARACTER CSFJP_SFLA_PX*40/' '/                                     33
      CHARACTER NSFJP_SFLA_PX*12/'R.SFLA_PX'/                             33
      INTEGER MSFJP_SFLA_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFLA_PX                                                33
C -                                                                       33
      REAL SFJP_SFLA_PY                                                   33
      EQUIVALENCE (BANK_SFJP(143),SFJP_SFLA_PY,RE_SFJP_SFLA_PY),          33
     *(NAMES_SFJP(141),NSFJP_SFLA_PY)                                     33
      EQUIVALENCE (COMMEN_SFJP(141),CSFJP_SFLA_PY),(MAP_SFJP(1,141),      33
     *MSFJP_SFLA_PY)                                                      33
      CHARACTER CSFJP_SFLA_PY*40/' '/                                     33
      CHARACTER NSFJP_SFLA_PY*12/'R.SFLA_PY'/                             33
      INTEGER MSFJP_SFLA_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFLA_PY                                                33
C -                                                                       33
      REAL SFJP_SFLC_PX                                                   33
      EQUIVALENCE (BANK_SFJP(144),SFJP_SFLC_PX,RE_SFJP_SFLC_PX),          33
     *(NAMES_SFJP(142),NSFJP_SFLC_PX)                                     33
      EQUIVALENCE (COMMEN_SFJP(142),CSFJP_SFLC_PX),(MAP_SFJP(1,142),      33
     *MSFJP_SFLC_PX)                                                      33
      CHARACTER CSFJP_SFLC_PX*40/' '/                                     33
      CHARACTER NSFJP_SFLC_PX*12/'R.SFLC_PX'/                             33
      INTEGER MSFJP_SFLC_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFLC_PX                                                33
C -                                                                       33
      REAL SFJP_SFLC_PY                                                   33
      EQUIVALENCE (BANK_SFJP(145),SFJP_SFLC_PY,RE_SFJP_SFLC_PY),          33
     *(NAMES_SFJP(143),NSFJP_SFLC_PY)                                     33
      EQUIVALENCE (COMMEN_SFJP(143),CSFJP_SFLC_PY),(MAP_SFJP(1,143),      33
     *MSFJP_SFLC_PY)                                                      33
      CHARACTER CSFJP_SFLC_PY*40/' '/                                     33
      CHARACTER NSFJP_SFLC_PY*12/'R.SFLC_PY'/                             33
      INTEGER MSFJP_SFLC_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFLC_PY                                                33
C -                                                                       33
      REAL SFJP_SFES_PX                                                   33
      EQUIVALENCE (BANK_SFJP(146),SFJP_SFES_PX,RE_SFJP_SFES_PX),          33
     *(NAMES_SFJP(144),NSFJP_SFES_PX)                                     33
      EQUIVALENCE (COMMEN_SFJP(144),CSFJP_SFES_PX),(MAP_SFJP(1,144),      33
     *MSFJP_SFES_PX)                                                      33
      CHARACTER CSFJP_SFES_PX*40/' '/                                     33
      CHARACTER NSFJP_SFES_PX*12/'R.SFES_PX'/                             33
      INTEGER MSFJP_SFES_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFES_PX                                                33
C -                                                                       33
      REAL SFJP_SFES_PY                                                   33
      EQUIVALENCE (BANK_SFJP(147),SFJP_SFES_PY,RE_SFJP_SFES_PY),          33
     *(NAMES_SFJP(145),NSFJP_SFES_PY)                                     33
      EQUIVALENCE (COMMEN_SFJP(145),CSFJP_SFES_PY),(MAP_SFJP(1,145),      33
     *MSFJP_SFES_PY)                                                      33
      CHARACTER CSFJP_SFES_PY*40/' '/                                     33
      CHARACTER NSFJP_SFES_PY*12/'R.SFES_PY'/                             33
      INTEGER MSFJP_SFES_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFES_PY                                                33
C -                                                                       33
      REAL SFJP_SFES_PZ                                                   33
      EQUIVALENCE (BANK_SFJP(148),SFJP_SFES_PZ,RE_SFJP_SFES_PZ),          33
     *(NAMES_SFJP(146),NSFJP_SFES_PZ)                                     33
      EQUIVALENCE (COMMEN_SFJP(146),CSFJP_SFES_PZ),(MAP_SFJP(1,146),      33
     *MSFJP_SFES_PZ)                                                      33
      CHARACTER CSFJP_SFES_PZ*40/' '/                                     33
      CHARACTER NSFJP_SFES_PZ*12/'R.SFES_PZ'/                             33
      INTEGER MSFJP_SFES_PZ(2)/1,1/                                       33
      REAL RE_SFJP_SFES_PZ                                                33
C -                                                                       33
      REAL SFJP_SFEB_PX                                                   33
      EQUIVALENCE (BANK_SFJP(149),SFJP_SFEB_PX,RE_SFJP_SFEB_PX),          33
     *(NAMES_SFJP(147),NSFJP_SFEB_PX)                                     33
      EQUIVALENCE (COMMEN_SFJP(147),CSFJP_SFEB_PX),(MAP_SFJP(1,147),      33
     *MSFJP_SFEB_PX)                                                      33
      CHARACTER CSFJP_SFEB_PX*40/' '/                                     33
      CHARACTER NSFJP_SFEB_PX*12/'R.SFEB_PX'/                             33
      INTEGER MSFJP_SFEB_PX(2)/1,1/                                       33
      REAL RE_SFJP_SFEB_PX                                                33
C -                                                                       33
      REAL SFJP_SFEB_PY                                                   33
      EQUIVALENCE (BANK_SFJP(150),SFJP_SFEB_PY,RE_SFJP_SFEB_PY),          33
     *(NAMES_SFJP(148),NSFJP_SFEB_PY)                                     33
      EQUIVALENCE (COMMEN_SFJP(148),CSFJP_SFEB_PY),(MAP_SFJP(1,148),      33
     *MSFJP_SFEB_PY)                                                      33
      CHARACTER CSFJP_SFEB_PY*40/' '/                                     33
      CHARACTER NSFJP_SFEB_PY*12/'R.SFEB_PY'/                             33
      INTEGER MSFJP_SFEB_PY(2)/1,1/                                       33
      REAL RE_SFJP_SFEB_PY                                                33
C -                                                                       33
      INTEGER LEN_SFJP,LLEN_SFJP,LINK_SFJP                                33
      PARAMETER (LEN_SFJP=150)                                            33
      PARAMETER (LLEN_SFJP=148)                                           33
      INTEGER FLAG_SFJP/0/,MAP_SFJP(2,LLEN_SFJP),IDNUM_SFJP(10)           33
      CHARACTER NAMES_SFJP(LLEN_SFJP)*12,COMMEN_SFJP(LLEN_SFJP)*40        33
      DATA IDNUM_SFJP/10*0/                                               33
      REAL BANK_SFJP(LEN_SFJP)                                            33
      CHARACTER*60 BPATH_SFJP/' '/                                        33
      SAVE FLAG_SFJP,MAP_SFJP,IDNUM_SFJP,NAMES_SFJP,COMMEN_SFJP,          33
     *BPATH_SFJP                                                          33
      DATA LINK_SFJP/0/,BANK_SFJP/LEN_SFJP*0/                             33
C *                                                                       33
C -                                                                       34
      REAL SFPB_HHIGHT                                                    34
      EQUIVALENCE (BANK_SFPB(3),SFPB_HHIGHT,RE_SFPB_HHIGHT),              34
     *(NAMES_SFPB(1),NSFPB_HHIGHT)                                        34
      EQUIVALENCE (COMMEN_SFPB(1),CSFPB_HHIGHT),(MAP_SFPB(1,1),           34
     *MSFPB_HHIGHT)                                                       34
      CHARACTER CSFPB_HHIGHT*40/' '/                                      34
      CHARACTER NSFPB_HHIGHT*12/'R.HHIGHT'/                               34
      INTEGER MSFPB_HHIGHT(2)/1,1/                                        34
      REAL RE_SFPB_HHIGHT                                                 34
C -                                                                       34
      REAL SFPB_KHIGHT                                                    34
      EQUIVALENCE (BANK_SFPB(4),SFPB_KHIGHT,RE_SFPB_KHIGHT),              34
     *(NAMES_SFPB(2),NSFPB_KHIGHT)                                        34
      EQUIVALENCE (COMMEN_SFPB(2),CSFPB_KHIGHT),(MAP_SFPB(1,2),           34
     *MSFPB_KHIGHT)                                                       34
      CHARACTER CSFPB_KHIGHT*40/' '/                                      34
      CHARACTER NSFPB_KHIGHT*12/'R.KHIGHT'/                               34
      INTEGER MSFPB_KHIGHT(2)/1,1/                                        34
      REAL RE_SFPB_KHIGHT                                                 34
C -                                                                       34
      REAL SFPB_HBASE                                                     34
      EQUIVALENCE (BANK_SFPB(5),SFPB_HBASE,RE_SFPB_HBASE),                34
     *(NAMES_SFPB(3),NSFPB_HBASE)                                         34
      EQUIVALENCE (COMMEN_SFPB(3),CSFPB_HBASE),(MAP_SFPB(1,3),            34
     *MSFPB_HBASE)                                                        34
      CHARACTER CSFPB_HBASE*40/' '/                                       34
      CHARACTER NSFPB_HBASE*12/'R.HBASE'/                                 34
      INTEGER MSFPB_HBASE(2)/1,1/                                         34
      REAL RE_SFPB_HBASE                                                  34
C -                                                                       34
      REAL SFPB_KBASE                                                     34
      EQUIVALENCE (BANK_SFPB(6),SFPB_KBASE,RE_SFPB_KBASE),                34
     *(NAMES_SFPB(4),NSFPB_KBASE)                                         34
      EQUIVALENCE (COMMEN_SFPB(4),CSFPB_KBASE),(MAP_SFPB(1,4),            34
     *MSFPB_KBASE)                                                        34
      CHARACTER CSFPB_KBASE*40/' '/                                       34
      CHARACTER NSFPB_KBASE*12/'R.KBASE'/                                 34
      INTEGER MSFPB_KBASE(2)/1,1/                                         34
      REAL RE_SFPB_KBASE                                                  34
C -                                                                       34
      REAL SFPB_FSIZE                                                     34
      EQUIVALENCE (BANK_SFPB(7),SFPB_FSIZE,RE_SFPB_FSIZE),                34
     *(NAMES_SFPB(5),NSFPB_FSIZE)                                         34
      EQUIVALENCE (COMMEN_SFPB(5),CSFPB_FSIZE),(MAP_SFPB(1,5),            34
     *MSFPB_FSIZE)                                                        34
      CHARACTER CSFPB_FSIZE*40/' '/                                       34
      CHARACTER NSFPB_FSIZE*12/'R.FSIZE'/                                 34
      INTEGER MSFPB_FSIZE(2)/1,1/                                         34
      REAL RE_SFPB_FSIZE                                                  34
C -                                                                       34
      REAL SFPB_ZCOOR                                                     34
      EQUIVALENCE (BANK_SFPB(8),SFPB_ZCOOR,RE_SFPB_ZCOOR),                34
     *(NAMES_SFPB(6),NSFPB_ZCOOR)                                         34
      EQUIVALENCE (COMMEN_SFPB(6),CSFPB_ZCOOR),(MAP_SFPB(1,6),            34
     *MSFPB_ZCOOR)                                                        34
      CHARACTER CSFPB_ZCOOR*40/' '/                                       34
      CHARACTER NSFPB_ZCOOR*12/'R.ZCOOR'/                                 34
      INTEGER MSFPB_ZCOOR(2)/1,1/                                         34
      REAL RE_SFPB_ZCOOR                                                  34
C -                                                                       34
      INTEGER LEN_SFPB,LLEN_SFPB,LINK_SFPB                                34
      PARAMETER (LEN_SFPB=8)                                              34
      PARAMETER (LLEN_SFPB=6)                                             34
      INTEGER FLAG_SFPB/0/,MAP_SFPB(2,LLEN_SFPB),IDNUM_SFPB(10)           34
      CHARACTER NAMES_SFPB(LLEN_SFPB)*12,COMMEN_SFPB(LLEN_SFPB)*40        34
      DATA IDNUM_SFPB/10*0/                                               34
      REAL BANK_SFPB(LEN_SFPB)                                            34
      CHARACTER*60 BPATH_SFPB/' '/                                        34
      SAVE FLAG_SFPB,MAP_SFPB,IDNUM_SFPB,NAMES_SFPB,COMMEN_SFPB,          34
     *BPATH_SFPB                                                          34
      DATA LINK_SFPB/0/,BANK_SFPB/LEN_SFPB*0/                             34
C *                                                                       34
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
      REAL SFPA_SOUTRINN                                                  37
      EQUIVALENCE (BANK_SFPA(7),SFPA_SOUTRINN,RE_SFPA_SOUTRINN),          37
     *(NAMES_SFPA(5),NSFPA_SOUTRINN)                                      37
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_SOUTRINN),(MAP_SFPA(1,5),         37
     *MSFPA_SOUTRINN)                                                     37
      CHARACTER CSFPA_SOUTRINN*40/' '/                                    37
      CHARACTER NSFPA_SOUTRINN*12/'R.SOUTRINN'/                           37
      INTEGER MSFPA_SOUTRINN(2)/1,1/                                      37
      REAL RE_SFPA_SOUTRINN                                               37
C -                                                                       37
      REAL SFPA_SOUTROUT                                                  37
      EQUIVALENCE (BANK_SFPA(8),SFPA_SOUTROUT,RE_SFPA_SOUTROUT),          37
     *(NAMES_SFPA(6),NSFPA_SOUTROUT)                                      37
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_SOUTROUT),(MAP_SFPA(1,6),         37
     *MSFPA_SOUTROUT)                                                     37
      CHARACTER CSFPA_SOUTROUT*40/' '/                                    37
      CHARACTER NSFPA_SOUTROUT*12/'R.SOUTROUT'/                           37
      INTEGER MSFPA_SOUTROUT(2)/1,1/                                      37
      REAL RE_SFPA_SOUTROUT                                               37
C -                                                                       37
      REAL SFPA_SOUTLEN                                                   37
      EQUIVALENCE (BANK_SFPA(9),SFPA_SOUTLEN,RE_SFPA_SOUTLEN),            37
     *(NAMES_SFPA(7),NSFPA_SOUTLEN)                                       37
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_SOUTLEN),(MAP_SFPA(1,7),          37
     *MSFPA_SOUTLEN)                                                      37
      CHARACTER CSFPA_SOUTLEN*40/' '/                                     37
      CHARACTER NSFPA_SOUTLEN*12/'R.SOUTLEN'/                             37
      INTEGER MSFPA_SOUTLEN(2)/1,1/                                       37
      REAL RE_SFPA_SOUTLEN                                                37
C -                                                                       37
      REAL SFPA_RAD                                                       37
      EQUIVALENCE (BANK_SFPA(10),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(8),    37
     *NSFPA_RAD)                                                          37
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_RAD),(MAP_SFPA(1,8),MSFPA_RAD)    37
      CHARACTER CSFPA_RAD*40/' '/                                         37
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     37
      INTEGER MSFPA_RAD(2)/1,1/                                           37
      REAL RE_SFPA_RAD                                                    37
C -                                                                       37
      REAL SFPA_NSSD                                                      37
      EQUIVALENCE (BANK_SFPA(11),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(9),  37
     *NSFPA_NSSD)                                                         37
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_NSSD),(MAP_SFPA(1,9),             37
     *MSFPA_NSSD)                                                         37
      CHARACTER CSFPA_NSSD*40/' '/                                        37
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   37
      INTEGER MSFPA_NSSD(2)/1,1/                                          37
      REAL RE_SFPA_NSSD                                                   37
C -                                                                       37
      REAL SFPA_DMWID                                                     37
      EQUIVALENCE (BANK_SFPA(12),SFPA_DMWID,RE_SFPA_DMWID),               37
     *(NAMES_SFPA(10),NSFPA_DMWID)                                        37
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_DMWID),(MAP_SFPA(1,10),          37
     *MSFPA_DMWID)                                                        37
      CHARACTER CSFPA_DMWID*40/' '/                                       37
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 37
      INTEGER MSFPA_DMWID(2)/1,1/                                         37
      REAL RE_SFPA_DMWID                                                  37
C -                                                                       37
      REAL SFPA_DMTHK                                                     37
      EQUIVALENCE (BANK_SFPA(13),SFPA_DMTHK,RE_SFPA_DMTHK),               37
     *(NAMES_SFPA(11),NSFPA_DMTHK)                                        37
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_DMTHK),(MAP_SFPA(1,11),          37
     *MSFPA_DMTHK)                                                        37
      CHARACTER CSFPA_DMTHK*40/' '/                                       37
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 37
      INTEGER MSFPA_DMTHK(2)/1,1/                                         37
      REAL RE_SFPA_DMTHK                                                  37
C -                                                                       37
      REAL SFPA_DMLEN                                                     37
      EQUIVALENCE (BANK_SFPA(14),SFPA_DMLEN,RE_SFPA_DMLEN),               37
     *(NAMES_SFPA(12),NSFPA_DMLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_DMLEN),(MAP_SFPA(1,12),          37
     *MSFPA_DMLEN)                                                        37
      CHARACTER CSFPA_DMLEN*40/' '/                                       37
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 37
      INTEGER MSFPA_DMLEN(2)/1,1/                                         37
      REAL RE_SFPA_DMLEN                                                  37
C -                                                                       37
      REAL SFPA_SMWID                                                     37
      EQUIVALENCE (BANK_SFPA(15),SFPA_SMWID,RE_SFPA_SMWID),               37
     *(NAMES_SFPA(13),NSFPA_SMWID)                                        37
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SMWID),(MAP_SFPA(1,13),          37
     *MSFPA_SMWID)                                                        37
      CHARACTER CSFPA_SMWID*40/' '/                                       37
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 37
      INTEGER MSFPA_SMWID(2)/1,1/                                         37
      REAL RE_SFPA_SMWID                                                  37
C -                                                                       37
      REAL SFPA_SMTHK                                                     37
      EQUIVALENCE (BANK_SFPA(16),SFPA_SMTHK,RE_SFPA_SMTHK),               37
     *(NAMES_SFPA(14),NSFPA_SMTHK)                                        37
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_SMTHK),(MAP_SFPA(1,14),          37
     *MSFPA_SMTHK)                                                        37
      CHARACTER CSFPA_SMTHK*40/' '/                                       37
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 37
      INTEGER MSFPA_SMTHK(2)/1,1/                                         37
      REAL RE_SFPA_SMTHK                                                  37
C -                                                                       37
      REAL SFPA_SMLEN                                                     37
      EQUIVALENCE (BANK_SFPA(17),SFPA_SMLEN,RE_SFPA_SMLEN),               37
     *(NAMES_SFPA(15),NSFPA_SMLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SMLEN),(MAP_SFPA(1,15),          37
     *MSFPA_SMLEN)                                                        37
      CHARACTER CSFPA_SMLEN*40/' '/                                       37
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 37
      INTEGER MSFPA_SMLEN(2)/1,1/                                         37
      REAL RE_SFPA_SMLEN                                                  37
C -                                                                       37
      REAL SFPA_SSLEN                                                     37
      EQUIVALENCE (BANK_SFPA(18),SFPA_SSLEN,RE_SFPA_SSLEN),               37
     *(NAMES_SFPA(16),NSFPA_SSLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_SSLEN),(MAP_SFPA(1,16),          37
     *MSFPA_SSLEN)                                                        37
      CHARACTER CSFPA_SSLEN*40/' '/                                       37
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 37
      INTEGER MSFPA_SSLEN(2)/1,1/                                         37
      REAL RE_SFPA_SSLEN                                                  37
C -                                                                       37
      REAL SFPA_WPLEN                                                     37
      EQUIVALENCE (BANK_SFPA(19),SFPA_WPLEN,RE_SFPA_WPLEN),               37
     *(NAMES_SFPA(17),NSFPA_WPLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_WPLEN),(MAP_SFPA(1,17),          37
     *MSFPA_WPLEN)                                                        37
      CHARACTER CSFPA_WPLEN*40/' '/                                       37
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 37
      INTEGER MSFPA_WPLEN(2)/1,1/                                         37
      REAL RE_SFPA_WPLEN                                                  37
C -                                                                       37
      REAL SFPA_SDLEN                                                     37
      EQUIVALENCE (BANK_SFPA(20),SFPA_SDLEN,RE_SFPA_SDLEN),               37
     *(NAMES_SFPA(18),NSFPA_SDLEN)                                        37
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_SDLEN),(MAP_SFPA(1,18),          37
     *MSFPA_SDLEN)                                                        37
      CHARACTER CSFPA_SDLEN*40/' '/                                       37
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 37
      INTEGER MSFPA_SDLEN(2)/1,1/                                         37
      REAL RE_SFPA_SDLEN                                                  37
C -                                                                       37
      REAL SFPA_TILT                                                      37
      EQUIVALENCE (BANK_SFPA(21),SFPA_TILT,RE_SFPA_TILT),                 37
     *(NAMES_SFPA(19),NSFPA_TILT)                                         37
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_TILT),(MAP_SFPA(1,19),           37
     *MSFPA_TILT)                                                         37
      CHARACTER CSFPA_TILT*40/' '/                                        37
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   37
      INTEGER MSFPA_TILT(2)/1,1/                                          37
      REAL RE_SFPA_TILT                                                   37
C -                                                                       37
      REAL SFPA_CPRAD                                                     37
      EQUIVALENCE (BANK_SFPA(22),SFPA_CPRAD,RE_SFPA_CPRAD),               37
     *(NAMES_SFPA(20),NSFPA_CPRAD)                                        37
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_CPRAD),(MAP_SFPA(1,20),          37
     *MSFPA_CPRAD)                                                        37
      CHARACTER CSFPA_CPRAD*40/' '/                                       37
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 37
      INTEGER MSFPA_CPRAD(2)/1,1/                                         37
      REAL RE_SFPA_CPRAD                                                  37
C -                                                                       37
      REAL SFPA_CPRAL                                                     37
      EQUIVALENCE (BANK_SFPA(23),SFPA_CPRAL,RE_SFPA_CPRAL),               37
     *(NAMES_SFPA(21),NSFPA_CPRAL)                                        37
      EQUIVALENCE (COMMEN_SFPA(21),CSFPA_CPRAL),(MAP_SFPA(1,21),          37
     *MSFPA_CPRAL)                                                        37
      CHARACTER CSFPA_CPRAL*40/' '/                                       37
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 37
      INTEGER MSFPA_CPRAL(2)/1,1/                                         37
      REAL RE_SFPA_CPRAL                                                  37
C -                                                                       37
      REAL SFPA_CFRAD                                                     37
      EQUIVALENCE (BANK_SFPA(24),SFPA_CFRAD,RE_SFPA_CFRAD),               37
     *(NAMES_SFPA(22),NSFPA_CFRAD)                                        37
      EQUIVALENCE (COMMEN_SFPA(22),CSFPA_CFRAD),(MAP_SFPA(1,22),          37
     *MSFPA_CFRAD)                                                        37
      CHARACTER CSFPA_CFRAD*40/' '/                                       37
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 37
      INTEGER MSFPA_CFRAD(2)/1,1/                                         37
      REAL RE_SFPA_CFRAD                                                  37
C -                                                                       37
      REAL SFPA_GPTHK                                                     37
      EQUIVALENCE (BANK_SFPA(25),SFPA_GPTHK,RE_SFPA_GPTHK),               37
     *(NAMES_SFPA(23),NSFPA_GPTHK)                                        37
      EQUIVALENCE (COMMEN_SFPA(23),CSFPA_GPTHK),(MAP_SFPA(1,23),          37
     *MSFPA_GPTHK)                                                        37
      CHARACTER CSFPA_GPTHK*40/' '/                                       37
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 37
      INTEGER MSFPA_GPTHK(2)/1,1/                                         37
      REAL RE_SFPA_GPTHK                                                  37
C -                                                                       37
      INTEGER SFPA_LADDERMAP(20)                                          37
      EQUIVALENCE (BANK_SFPA(26),SFPA_LADDERMAP,RE_SFPA_LADDERMAP),       37
     *(NAMES_SFPA(24),NSFPA_LADDERMAP)                                    37
      EQUIVALENCE (COMMEN_SFPA(24),CSFPA_LADDERMAP),(MAP_SFPA(1,24),      37
     *MSFPA_LADDERMAP)                                                    37
      CHARACTER CSFPA_LADDERMAP*40/' '/                                   37
      CHARACTER NSFPA_LADDERMAP*12/'I.LADDERMAP'/                         37
      INTEGER MSFPA_LADDERMAP(2)/20,1/                                    37
      REAL RE_SFPA_LADDERMAP                                              37
C -                                                                       37
      REAL SFPA_LADDERANGLE(20)                                           37
      EQUIVALENCE (BANK_SFPA(46),SFPA_LADDERANGLE,RE_SFPA_LADDERANGLE),   37
     *(NAMES_SFPA(25),NSFPA_LADDERANGLE)                                  37
      EQUIVALENCE (COMMEN_SFPA(25),CSFPA_LADDERANGLE),(MAP_SFPA(1,25),    37
     *MSFPA_LADDERANGLE)                                                  37
      CHARACTER CSFPA_LADDERANGLE*40/' '/                                 37
      CHARACTER NSFPA_LADDERANGLE*12/'R.LADDERANGLE'/                     37
      INTEGER MSFPA_LADDERANGLE(2)/20,1/                                  37
      REAL RE_SFPA_LADDERANGLE                                            37
C -                                                                       37
      REAL SFPA_LADDERTILT(20)                                            37
      EQUIVALENCE (BANK_SFPA(66),SFPA_LADDERTILT,RE_SFPA_LADDERTILT),     37
     *(NAMES_SFPA(26),NSFPA_LADDERTILT)                                   37
      EQUIVALENCE (COMMEN_SFPA(26),CSFPA_LADDERTILT),(MAP_SFPA(1,26),     37
     *MSFPA_LADDERTILT)                                                   37
      CHARACTER CSFPA_LADDERTILT*40/' '/                                  37
      CHARACTER NSFPA_LADDERTILT*12/'R.LADDERTILT'/                       37
      INTEGER MSFPA_LADDERTILT(2)/20,1/                                   37
      REAL RE_SFPA_LADDERTILT                                             37
C -                                                                       37
      REAL SFPA_LADDERRADIUS(20)                                          37
      EQUIVALENCE (BANK_SFPA(86),SFPA_LADDERRADIUS,                       37
     *RE_SFPA_LADDERRADIUS),(NAMES_SFPA(27),NSFPA_LADDERRADIUS)           37
      EQUIVALENCE (COMMEN_SFPA(27),CSFPA_LADDERRADIUS),(MAP_SFPA(1,27),   37
     *MSFPA_LADDERRADIUS)                                                 37
      CHARACTER CSFPA_LADDERRADIUS*40/' '/                                37
      CHARACTER NSFPA_LADDERRADIUS*12/'R.LADDERRADIUS'/                   37
      INTEGER MSFPA_LADDERRADIUS(2)/20,1/                                 37
      REAL RE_SFPA_LADDERRADIUS                                           37
C -                                                                       37
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                37
      PARAMETER (LEN_SFPA=105)                                            37
      PARAMETER (LLEN_SFPA=27)                                            37
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           37
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        37
      DATA IDNUM_SFPA/10*0/                                               37
      REAL BANK_SFPA(LEN_SFPA)                                            37
      CHARACTER*60 BPATH_SFPA/' '/                                        37
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          37
     *BPATH_SFPA                                                          37
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             37
C *                                                                       37
      INTEGER ILAD,IWAF,JWAF,NC                                           38
      REAL WAFPCKLEN, DTHK, RADTILT, ANG, RADIUS, TILT, X, Y, XPOS,       41
     *YPOS, DOFF, XC, YC                                                  41
      REAL ESSAI,HIGHT,LNG,LENG,YOFFSET                                   43
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank SSDP - SILICON STRIPS                                      45
      BANK_TITLE='SILICON STRIPS'                                         45
      BPATH_SSDP='SSDP*'                                                  45
      IDNUM_SSDP(1)=0                                                     46
C VERSION                                                                 46
      SSDP_VERSION=1                                                      46
      CSSDP_VERSION='VERSION'                                             46
C THERE ARE A FEW CONFIGURAIONS POSSIBLE                                  47
      SSDP_CONFIG=1                                                       47
      CSSDP_CONFIG='THERE ARE A FEW CONFIGURAIONS POSSIBLE'               47
C 0=CAVE, 1=IDSM                                                          48
      SSDP_PLACEMENT=1                                                    48
      CSSDP_PLACEMENT='0=CAVE, 1=IDSM'                                    48
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    49
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           49
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   49
C *                                                                       49
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    49
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           49
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   49
C *                                                                       50
      CALL AGSBEGM(MODULE,IPRIN)                                          50
      IF (IPRIN.LT.0) RETURN                                              50
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      50
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         50
      BPATH_SFJP='SFJP*'                                                  50
      IDNUM_SFJP(1)=0                                                     51
C NEW VERSION INFORMATION                                                 51
      SFJP_VERSION=1                                                      51
      CSFJP_VERSION='NEW VERSION INFORMATION'                             51
C ALPHA OF THE SFLA AND OTHER PARTS                                       52
      SFJP_ALPHAZ=54                                                      52
      CSFJP_ALPHAZ='ALPHA OF THE SFLA AND OTHER PARTS'                    52
C ALPHA OF OTHER PARTS                                                    53
      SFJP_ALPHAZH=45                                                     53
      CSFJP_ALPHAZH='ALPHA OF OTHER PARTS'                                53
C MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        54
      SFJP_SSST_RMIN=31.285                                               54
      CSFJP_SSST_RMIN='MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUT  54
     *SIDE STRUCTURE)'                                                    54
C MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        55
      SFJP_SSST_RMAX=31.8                                                 55
      CSFJP_SSST_RMAX='MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUT  55
     *SIDE STRUCTURE)'                                                    55
C WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)                 56
      SFJP_SSST_WIDTH=2.5                                                 56
      CSFJP_SSST_WIDTH='WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STR  56
     *UCTURE)'                                                            56
C ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS                          57
      SFJP_SSST_PZ=0.2                                                    57
      CSFJP_SSST_PZ='ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS'      57
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           58
      SFJP_SSSS_RMIN=23.3                                                 58
      CSFJP_SSSS_RMIN='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSID  58
     *E STRUCTURE)'                                                       58
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           59
      SFJP_SSSS_WIDTH=0.5                                                 59
      CSFJP_SSSS_WIDTH='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSI  59
     *DE STRUCTURE)'                                                      59
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  60
      SFJP_SSRS_RMIN=28.0                                                 60
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  60
     *TURE)'                                                              60
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   61
      SFJP_SSLB_DX=8.5                                                    61
      CSFJP_SSLB_DX='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  61
     *E)'                                                                 61
C X-POSITION = Y-POSITION OF THE MOTHER VOLUME OF THE LINKING BOX         62
      SFJP_SSLB_PX=19.1                                                   62
      CSFJP_SSLB_PX='X-POSITION = Y-POSITION OF THE MOTHER VOLUME OF THE  62
     * LINKING BOX'                                                       62
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   63
      SFJP_SSLB_DY=7.3                                                    63
      CSFJP_SSLB_DY='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  63
     *E)'                                                                 63
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   64
      SFJP_SSLB_DZ=2.5                                                    64
      CSFJP_SSLB_DZ='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  64
     *E)'                                                                 64
C LINKING BOX (SECTOR TO THE CONE)                                        65
      SFJP_SSBQ_DX=7.8                                                    65
      CSFJP_SSBQ_DX='LINKING BOX (SECTOR TO THE CONE)'                    65
C LINKING BOX (SECTOR TO THE CONE)                                        66
      SFJP_SSBQ_DY=7.3                                                    66
      CSFJP_SSBQ_DY='LINKING BOX (SECTOR TO THE CONE)'                    66
C LINKING BOX (SECTOR TO THE CONE)                                        67
      SFJP_SSBQ_DZ=2.5                                                    67
      CSFJP_SSBQ_DZ='LINKING BOX (SECTOR TO THE CONE)'                    67
C LINKING CROSS TOTAL THICKNESS (ALUMINUM PART)                           68
      SFJP_SSCR_THA=1.5                                                   68
      CSFJP_SSCR_THA='LINKING CROSS TOTAL THICKNESS (ALUMINUM PART)'      68
C LINKING CROSS TOTAL THICKNESS (DELRIN PART)                             69
      SFJP_SSCR_THD=1.0                                                   69
      CSFJP_SSCR_THD='LINKING CROSS TOTAL THICKNESS (DELRIN PART)'        69
C LINKING CROSS TOTAL WIDTH                                               70
      SFJP_SSCR_WD=1.2                                                    70
      CSFJP_SSCR_WD='LINKING CROSS TOTAL WIDTH'                           70
C LINKING CROSS HORIZONTAL ARM FULL LENGTH                                71
      SFJP_SSCR_HL=8.5                                                    71
      CSFJP_SSCR_HL='LINKING CROSS HORIZONTAL ARM FULL LENGTH'            71
C LINKING CROSS VERTICAL ARM FULL LENGTH                                  72
      SFJP_SSCR_VL=1.6                                                    72
      CSFJP_SSCR_VL='LINKING CROSS VERTICAL ARM FULL LENGTH'              72
C X-POSITION = Y-POSITION OF THE LINKING TUBE                             73
      SFJP_SSLT_PX=18.1                                                   73
      CSFJP_SSLT_PX='X-POSITION = Y-POSITION OF THE LINKING TUBE'         73
C RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)                         74
      SFJP_SSLT_RMAX=0.8                                                  74
      CSFJP_SSLT_RMAX='RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)'   74
C WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)                          75
      SFJP_SSLT_DZ=5.05                                                   75
      CSFJP_SSLT_DZ='WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)'      75
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       76
      SFJP_SCMP_DX=6.3                                                    76
      CSFJP_SCMP_DX='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   76
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       77
      SFJP_SCMP_DY=0.4                                                    77
      CSFJP_SCMP_DY='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   77
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       78
      SFJP_SCMP_DZ=3.6                                                    78
      CSFJP_SCMP_DZ='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   78
C X-POSITION = Y-POSITION OF THE MOUNTING PLATE                           79
      SFJP_SCMP_PX=15.945                                                 79
      CSFJP_SCMP_PX='X-POSITION = Y-POSITION OF THE MOUNTING PLATE'       79
C MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH                               80
      SFJP_SCVM_DZ=3.6                                                    80
      CSFJP_SCVM_DZ='MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH'           80
C MOTHER VOLUME OF THE V-SHAPE PIECE DX                                   81
      SFJP_SCVM_DX=6.4                                                    81
      CSFJP_SCVM_DX='MOTHER VOLUME OF THE V-SHAPE PIECE DX'               81
C X-POSITION = Y-POSITION OF THE V-SHAPE PIECE                            82
      SFJP_SCVM_PX=17.00                                                  82
      CSFJP_SCVM_PX='X-POSITION = Y-POSITION OF THE V-SHAPE PIECE'        82
C MOTHER VOLUME OF THE V-SHAPE PIECE DY                                   83
      SFJP_SCVM_DY=2.6                                                    83
      CSFJP_SCVM_DY='MOTHER VOLUME OF THE V-SHAPE PIECE DY'               83
C Z-POSITION OF THE MOTHER VOLUME                                         84
      SFJP_SCVM_PZ=57.5                                                   84
      CSFJP_SCVM_PZ='Z-POSITION OF THE MOTHER VOLUME'                     84
C HEIGHT OF THE BASE OF THE V-SHAPE PIECE                                 85
      SFJP_SCVB_DY=0.5                                                    85
      CSFJP_SCVB_DY='HEIGHT OF THE BASE OF THE V-SHAPE PIECE'             85
C DX OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES                      86
      SFJP_SCVS_DX=2.3                                                    86
      CSFJP_SCVS_DX='DX OF THE V-PLATES OF THE V-SHAPE PIECE 45           86
     *DEGREES'                                                            86
C MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES           87
      SFJP_SCVS_DY=0.6                                                    87
      CSFJP_SCVS_DY='MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE   87
     *45 DEGREES'                                                         87
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              88
      SFJP_SCVS_PX=1.0                                                    88
      CSFJP_SCVS_PX='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   88
     *DEGREES'                                                            88
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              89
      SFJP_SCVS_PY=0.22                                                   89
      CSFJP_SCVS_PY='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   89
     *DEGREES'                                                            89
C DX OF THE CONNECTION BOARD (END OF THE LADDER)                          90
      SFJP_SFCO_DX=4.4                                                    90
      CSFJP_SFCO_DX='DX OF THE CONNECTION BOARD (END OF THE LADDER)'      90
C WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)                       91
      SFJP_SFCO_DY=0.37                                                   91
      CSFJP_SFCO_DY='WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)'   91
C LENGTH OF THE CONNECTION BOARD (END OF THE LADDER)                      92
      SFJP_SFCO_DZ=15.8                                                   92
      CSFJP_SFCO_DZ='LENGTH OF THE CONNECTION BOARD (END OF THE           92
     *LADDER)'                                                            92
C X-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  93
      SFJP_SFCO_PX=1.7                                                    93
      CSFJP_SFCO_PX='X-POSITION OF THE CONNECTION BOARD (END OF THE LADD  93
     *ER)'                                                                93
C Y-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  94
      SFJP_SFCO_PY=0.2                                                    94
      CSFJP_SFCO_PY='Y-POSITION OF THE CONNECTION BOARD (END OF THE LADD  94
     *ER)'                                                                94
C DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE ABOVE ONE!!)        95
      SFJP_SFCM_DX=3.998                                                  95
      CSFJP_SFCM_DX='DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE  95
     * ABOVE ONE!!)'                                                      95
C DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD                     96
      SFJP_SFCS_DZ=7.5                                                    96
      CSFJP_SFCS_DZ='DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD  96
     *'                                                                   96
C WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS          97
      SFJP_SFKF_DY=0.06                                                   97
      CSFJP_SFKF_DY='WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNEC  97
     *TION BOARDS'                                                        97
C LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS         98
      SFJP_SFKF_DZ=8.1                                                    98
      CSFJP_SFKF_DZ='LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNE  98
     *CTION BOARDS'                                                       98
C DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                         99
      SFJP_SFKF_DX=0.44                                                   99
      CSFJP_SFKF_DX='DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT'     99
C X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 100
      SFJP_SFKF_PX=0.16                                                   100
      CSFJP_SFKF_PX='X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  100
     *CUIT'                                                               100
C Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 101
      SFJP_SFKF_PY=2.07                                                   101
      CSFJP_SFKF_PY='Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  101
     *CUIT'                                                               101
C DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT                        102
      SFJP_SFKS_DX=1.65                                                   102
      CSFJP_SFKS_DX='DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT'    102
C X-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        103
      SFJP_SFKS_PX=0.505                                                  103
      CSFJP_SFKS_PX='X-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    103
C Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        104
      SFJP_SFKS_PY=1.585                                                  104
      CSFJP_SFKS_PY='Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    104
C Y-POSITION OF THE MECHANICAL PARTS                                      105
      SFJP_SFPR_PY=1.79                                                   105
      CSFJP_SFPR_PY='Y-POSITION OF THE MECHANICAL PARTS'                  105
C Y-POSITION OF THE MECHANICAL PARTS 2                                    106
      SFJP_SFPB_PY=3.48                                                   106
      CSFJP_SFPB_PY='Y-POSITION OF THE MECHANICAL PARTS 2'                106
C Y-POSITION OF THE MECHANICAL PARTS 2                                    107
      SFJP_SFPB_PY2=0.08                                                  107
      CSFJP_SFPB_PY2='Y-POSITION OF THE MECHANICAL PARTS 2'               107
C DX OF THE BIG PART OF THE SECOND CONNECTION BOARD                       108
      SFJP_SFCB_DX=3.0                                                    108
      CSFJP_SFCB_DX='DX OF THE BIG PART OF THE SECOND CONNECTION BOARD'   108
C WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO THE SECTOR          109
      SFJP_SSBS_DY=0.5                                                    109
      CSFJP_SSBS_DY='WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO  109
     * THE SECTOR'                                                        109
C DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER          110
      SFJP_SSBS_DX=2.5                                                    110
      CSFJP_SSBS_DX='DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING  110
     * THE LADDER'                                                        110
C DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            111
      SFJP_SSBB_DX=4.3                                                    111
      CSFJP_SSBB_DX='DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  111
     *HE LADDER'                                                          111
C DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            112
      SFJP_SSBB_DZ=1.9                                                    112
      CSFJP_SSBB_DZ='DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  112
     *HE LADDER'                                                          112
C DISTANZE BETWEEN OVERLAYING FLEXES                                      113
      SFJP_FLEX_DI=0.02                                                   113
      CSFJP_FLEX_DI='DISTANZE BETWEEN OVERLAYING FLEXES'                  113
C DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)            114
      SFJP_SFPB_DX=4.3                                                    114
      CSFJP_SFPB_DX='DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE -   114
     *BOX BASE)'                                                          114
C WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)         115
      SFJP_SFPB_DY=0.8                                                    115
      CSFJP_SFPB_DY='WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  115
     * - BOX BASE)'                                                       115
C LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)        116
      SFJP_SFPBDZ=0.95                                                    116
      CSFJP_SFPBDZ='LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  116
     * - BOX BASE)'                                                       116
C DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME           117
      SFJP_SAPP_DXE=7.6                                                   117
      CSFJP_SAPP_DXE='DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MO  117
     *THER VOLUME'                                                        117
C DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPENDICE                  118
      SFJP_SAPP_DXZ=0.04                                                  118
      CSFJP_SAPP_DXZ='DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPEN  118
     *DICE'                                                               118
C WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME        119
      SFJP_SAPP_DY=0.5                                                    119
      CSFJP_SAPP_DY='WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE   119
     *MOTHER VOLUME'                                                      119
C LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME                         120
      SFJP_SAPP_DZ=1.275                                                  120
      CSFJP_SAPP_DZ='LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME'     120
C 1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     121
      SFJP_SAPP_PY1=0.04                                                  121
      CSFJP_SAPP_PY1='1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  121
     *D'                                                                  121
C 2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     122
      SFJP_SAPP_PY2=0.5                                                   122
      CSFJP_SAPP_PY2='2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  122
     *D'                                                                  122
C 3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     123
      SFJP_SAPP_PY3=0.08                                                  123
      CSFJP_SAPP_PY3='3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  123
     *D'                                                                  123
C DX OF THE MOTHER VOLUME OF THE ADC BOARD                                124
      SFJP_SFAM_DXE=7.2                                                   124
      CSFJP_SFAM_DXE='DX OF THE MOTHER VOLUME OF THE ADC BOARD'           124
C DX OF THE SMALL VOLUME OF THE ADC BOARD                                 125
      SFJP_SFAM_DXZ=2.1                                                   125
      CSFJP_SFAM_DXZ='DX OF THE SMALL VOLUME OF THE ADC BOARD'            125
C WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD                             126
      SFJP_SFAM_DY=0.3                                                    126
      CSFJP_SFAM_DY='WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD'         126
C LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD                            127
      SFJP_SFAM_DZ=12.4                                                   127
      CSFJP_SFAM_DZ='LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD'        127
C LENGTH OF THE SMALL VOLUME OF THE ADC BOARD                             128
      SFJP_SFAM_DZS=2.15                                                  128
      CSFJP_SFAM_DZS='LENGTH OF THE SMALL VOLUME OF THE ADC BOARD'        128
C DX OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES                129
      SFJP_SFLA_DX=1.5                                                    129
      CSFJP_SFLA_DX='DX OF THE LONG PART OF THE BUS CABLE LINKING THE MO  129
     *DULES'                                                              129
C WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES             130
      SFJP_SFLA_DY=0.0130                                                 130
      CSFJP_SFLA_DY='WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE  130
     * MODULES'                                                           130
C LENGTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES            131
      SFJP_SFLA_DZ=4.3                                                    131
      CSFJP_SFLA_DZ='LENGTH OF THE LONG PART OF THE BUS CABLE LINKING TH  131
     *E MODULES'                                                          131
C LENGTH OF THE LONG BUS CABLE                                            132
      SFJP_SFLB_DZ=7.5                                                    132
      CSFJP_SFLB_DZ='LENGTH OF THE LONG BUS CABLE'                        132
C LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CONNECTOR              133
      SFJP_SFLC_DZ=0.93                                                   133
      CSFJP_SFLC_DZ='LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CO  133
     *NNECTOR'                                                            133
C DX OF THE BIG BUS ELBOW                                                 134
      SFJP_SFEB_DX=3.89                                                   134
      CSFJP_SFEB_DX='DX OF THE BIG BUS ELBOW'                             134
C LENGTH OF THE BUS ELBOW                                                 135
      SFJP_SFEB_DZ=1.5                                                    135
      CSFJP_SFEB_DZ='LENGTH OF THE BUS ELBOW'                             135
C DX OF THE BIG BUS ELBOW                                                 136
      SFJP_SFES_DX=1.875                                                  136
      CSFJP_SFES_DX='DX OF THE BIG BUS ELBOW'                             136
C DX OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE            137
      SFJP_SFFK_DXE=0.4                                                   137
      CSFJP_SFFK_DXE='DX OF THE HORIZONTAL PART OF THE LADDER SKELETON C  137
     *ARBON BASE'                                                         137
C WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE         138
      SFJP_SFFK_DY=0.02                                                   138
      CSFJP_SFFK_DY='WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON  138
     * CARBON BASE'                                                       138
C LENGTH OF OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE     139
      SFJP_SFFK_DZ=69.75                                                  139
      CSFJP_SFFK_DZ='LENGTH OF OF THE HORIZONTAL PART OF THE LADDER SKEL  139
     *ETON CARBON BASE'                                                   139
C DX OF THE TILTED PART OF THE LADDER                                     140
      SFJP_SFFL_DX=0.6                                                    140
      CSFJP_SFFL_DX='DX OF THE TILTED PART OF THE LADDER'                 140
C DX OF THE HORIZONTAL PART OF THE KAPTON FILM                            141
      SFJP_SFFK_DXZ=5.1                                                   141
      CSFJP_SFFK_DXZ='DX OF THE HORIZONTAL PART OF THE KAPTON FILM'       141
C X-POSITION OF THE CARBON BASE UNDER THE LADDER                          142
      SFJP_SFFK_PX=3.5                                                    142
      CSFJP_SFFK_PX='X-POSITION OF THE CARBON BASE UNDER THE LADDER'      142
C 1. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       143
      SFJP_SFFK_PY1=0.02                                                  143
      CSFJP_SFFK_PY1='1. Y-POSITION OF THE CARBON BASE UNDER THE          143
     *LADDER'                                                             143
C 2. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       144
      SFJP_SFFK_PY2=0.04                                                  144
      CSFJP_SFFK_PY2='2. Y-POSITION OF THE CARBON BASE UNDER THE          144
     *LADDER'                                                             144
C X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADDER                   145
      SFJP_SFKL_PX=2.55                                                   145
      CSFJP_SFKL_PX='X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADD  145
     *ER'                                                                 145
C WIDTH OF THE KAPTON FILM                                                146
      SFJP_SFKK_DY=0.0025                                                 146
      CSFJP_SFKK_DY='WIDTH OF THE KAPTON FILM'                            146
C LENGTH OF THE TRIANGULAR LADDER SKELETON                                147
      SFJP_SFLU_DZ=49.8                                                   147
      CSFJP_SFLU_DZ='LENGTH OF THE TRIANGULAR LADDER SKELETON'            147
C HEIGHT OF THE TRIANGULAR LADDER SKELETON                                148
      SFJP_SFLU_H1=0.02                                                   148
      CSFJP_SFLU_H1='HEIGHT OF THE TRIANGULAR LADDER SKELETON'            148
C BASE LENGTH OF THE TRIANGULAR LADDER SKELETON                           149
      SFJP_SFLU_BL1=0.2                                                   149
      CSFJP_SFLU_BL1='BASE LENGTH OF THE TRIANGULAR LADDER SKELETON'      149
C DX OF THE HYBRID STIFFNER                                               150
      SFJP_SFRA_DX=3.76                                                   150
      CSFJP_SFRA_DX='DX OF THE HYBRID STIFFNER'                           150
C WIDTH OF THE HYBRID STIFFNER                                            151
      SFJP_SFRA_DY=0.019                                                  151
      CSFJP_SFRA_DY='WIDTH OF THE HYBRID STIFFNER'                        151
C Y-POSITION OF THE HYBRID STIFFNER                                       152
      SFJP_SFRA_PY=0.099                                                  152
      CSFJP_SFRA_PY='Y-POSITION OF THE HYBRID STIFFNER'                   152
C LENGTH OF THE HYBRID STIFFNER                                           153
      SFJP_SFRA_DZ=1.0                                                    153
      CSFJP_SFRA_DZ='LENGTH OF THE HYBRID STIFFNER'                       153
C Z-POSITION OF THE HYBRID STIFFNER                                       154
      SFJP_SFRA_PZ=1.1                                                    154
      CSFJP_SFRA_PZ='Z-POSITION OF THE HYBRID STIFFNER'                   154
C WIDTH OF A SINGLE WAFER CONTAINER                                       155
      SFJP_SFSW_DY=1.0                                                    155
      CSFJP_SFSW_DY='WIDTH OF A SINGLE WAFER CONTAINER'                   155
C LENGTHOF OF A SINGLE WAFER CONTAINER                                    156
      SFJP_SFSW_DZ=2.25                                                   156
      CSFJP_SFSW_DZ='LENGTHOF OF A SINGLE WAFER CONTAINER'                156
C DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           157
      SFJP_SFRS_DX=0.04                                                   157
      CSFJP_SFRS_DX='DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       157
C DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           158
      SFJP_SFRS_DY=0.0915                                                 158
      CSFJP_SFRS_DY='DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       158
C X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   159
      SFJP_SFRS_PX=3.8                                                    159
      CSFJP_SFRS_PX='X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  159
     *ER'                                                                 159
C Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   160
      SFJP_SFRS_PY=0.0265                                                 160
      CSFJP_SFRS_PY='Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  160
     *ER'                                                                 160
C LENGTH OF THE TRIANGULAR BASE                                           161
      SFJP_SFSM_LL=2.6                                                    161
      CSFJP_SFSM_LL='LENGTH OF THE TRIANGULAR BASE'                       161
C DX OF THE FLEX                                                          162
      SFJP_SFFX_DX=3.84                                                   162
      CSFJP_SFFX_DX='DX OF THE FLEX'                                      162
C WIDTH OF THE FLEX                                                       163
      SFJP_SFFX_DYZ=0.0007                                                163
      CSFJP_SFFX_DYZ='WIDTH OF THE FLEX'                                  163
C RMIN OF THE PIONS                                                       164
      SFJP_SFPI_RMIN=0.15                                                 164
      CSFJP_SFPI_RMIN='RMIN OF THE PIONS'                                 164
C RMAX OF THE PIONS                                                       165
      SFJP_SFPI_RMAX=0.2                                                  165
      CSFJP_SFPI_RMAX='RMAX OF THE PIONS'                                 165
C X-POSITION OF THE FOUR PIONS                                            166
      SFJP_SFPI_PX=3.2                                                    166
      CSFJP_SFPI_PX='X-POSITION OF THE FOUR PIONS'                        166
C 1. Y-POSITION OF THE FOUR PIONS                                         167
      SFJP_SFPI_PY1=0.35                                                  167
      CSFJP_SFPI_PY1='1. Y-POSITION OF THE FOUR PIONS'                    167
C 2. Y-POSITION OF THE FOUR PIONS                                         168
      SFJP_SFPI_PY2=0.025                                                 168
      CSFJP_SFPI_PY2='2. Y-POSITION OF THE FOUR PIONS'                    168
C Z-POSITION OF THE FOUR PIONS                                            169
      SFJP_SFPI_PZ=0.7                                                    169
      CSFJP_SFPI_PZ='Z-POSITION OF THE FOUR PIONS'                        169
C 1. DX OF THE BASE OF THE PIONS                                          170
      SFJP_SFPJ_DX1=0.075                                                 170
      CSFJP_SFPJ_DX1='1. DX OF THE BASE OF THE PIONS'                     170
C 2. DX OF THE BASE OF THE PIONS                                          171
      SFJP_SFPJ_DX2=0.025                                                 171
      CSFJP_SFPJ_DX2='2. DX OF THE BASE OF THE PIONS'                     171
C DY OF THE BASE OF THE PIONS                                             172
      SFJP_SFPJ_DY=0.3                                                    172
      CSFJP_SFPJ_DY='DY OF THE BASE OF THE PIONS'                         172
C DZ OF THE BASE OF THE PIONS                                             173
      SFJP_SFPJ_DZ=0.0125                                                 173
      CSFJP_SFPJ_DZ='DZ OF THE BASE OF THE PIONS'                         173
C DX OF THE A128C CHIP                                                    174
      SFJP_SFAA_DX=0.3                                                    174
      CSFJP_SFAA_DX='DX OF THE A128C CHIP'                                174
C DY OF THE A128C CHIP                                                    175
      SFJP_SFAA_DY=0.015                                                  175
      CSFJP_SFAA_DY='DY OF THE A128C CHIP'                                175
C DZ OF THE A128C CHIP                                                    176
      SFJP_SFAA_DZ=0.4                                                    176
      CSFJP_SFAA_DZ='DZ OF THE A128C CHIP'                                176
C 1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSITION OF THE A128C CHI  177
C PS                                                                      177
      SFJP_SFAA_PX1=0.325                                                 177
      CSFJP_SFAA_PX1='1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSIT  177
     *ION OF THE A128C CHIPS'                                             177
C 2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITION OF THE A128C CHIP  178
C S                                                                       178
      SFJP_SFAA_PX2=0.65                                                  178
      CSFJP_SFAA_PX2='2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITI  178
     *ON OF THE A128C CHIPS'                                              178
C 3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITION OF THE A128C CHIPS  179
      SFJP_SFAA_PX3=0.6                                                   179
      CSFJP_SFAA_PX3='3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITIO  179
     *N OF THE A128C CHIPS'                                               179
C 1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITION OF THE A128C CHIPS  180
      SFJP_SFAA_PZ1=1.1                                                   180
      CSFJP_SFAA_PZ1='1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITIO  180
     *N OF THE A128C CHIPS'                                               180
C 2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITION OF THE A128C CHIPS  181
      SFJP_SFAA_PZ2=1.0                                                   181
      CSFJP_SFAA_PZ2='2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITIO  181
     *N OF THE A128C CHIPS'                                               181
C 3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITION OF THE A128C CHIP  182
C S                                                                       182
      SFJP_SFAA_PZ3=0.02                                                  182
      CSFJP_SFAA_PZ3='3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITI  182
     *ON OF THE A128C CHIPS'                                              182
C DX OF THE STRIP DETECTOR                                                183
      SFJP_SFSL_DX=3.75                                                   183
      CSFJP_SFSL_DX='DX OF THE STRIP DETECTOR'                            183
C DY OF THE STRIP DETECTOR                                                184
      SFJP_SFSL_DY=0.015                                                  184
      CSFJP_SFSL_DY='DY OF THE STRIP DETECTOR'                            184
C DZ OF THE STRIP DETECTOR                                                185
      SFJP_SFSL_DZ=2.1                                                    185
      CSFJP_SFSL_DZ='DZ OF THE STRIP DETECTOR'                            185
C DX OF THE SENSITIVE AREA                                                186
      SFJP_SFSD_DX=3.65                                                   186
      CSFJP_SFSD_DX='DX OF THE SENSITIVE AREA'                            186
C DY OF THE SENSITIVE AREA                                                187
      SFJP_SFSD_DY=0.015                                                  187
      CSFJP_SFSD_DY='DY OF THE SENSITIVE AREA'                            187
C DZ OF THE SENSITIVE AREA                                                188
      SFJP_SFSD_DZ=2.0                                                    188
      CSFJP_SFSD_DZ='DZ OF THE SENSITIVE AREA'                            188
C X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             189
      SFJP_SFLA_PX=1.1                                                    189
      CSFJP_SFLA_PX='X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         189
C Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             190
      SFJP_SFLA_PY=1.35                                                   190
      CSFJP_SFLA_PY='Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         190
C X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          191
      SFJP_SFLC_PX=2.3                                                    191
      CSFJP_SFLC_PX='X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      191
C Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          192
      SFJP_SFLC_PY=0.3                                                    192
      CSFJP_SFLC_PY='Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      192
C X-POSITION OF THE SMALL ELBOW BUS                                       193
      SFJP_SFES_PX=3.32                                                   193
      CSFJP_SFES_PX='X-POSITION OF THE SMALL ELBOW BUS'                   193
C Y-POSITION OF THE SMALL ELBOW BUS                                       194
      SFJP_SFES_PY=1.6                                                    194
      CSFJP_SFES_PY='Y-POSITION OF THE SMALL ELBOW BUS'                   194
C Z-POSITION OF THE SMALL ELBOW BUS                                       195
      SFJP_SFES_PZ=0.1                                                    195
      CSFJP_SFES_PZ='Z-POSITION OF THE SMALL ELBOW BUS'                   195
C X-POSITION OF THE BIG ELBOW BUS                                         196
      SFJP_SFEB_PX=2.71                                                   196
      CSFJP_SFEB_PX='X-POSITION OF THE BIG ELBOW BUS'                     196
C Y-POSITION OF THE BIG ELBOW BUS                                         197
      SFJP_SFEB_PY=0.75                                                   197
      CSFJP_SFEB_PY='Y-POSITION OF THE BIG ELBOW BUS'                     197
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    198
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           198
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   198
C *                                                                       198
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    198
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           198
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   198
C *                                                                       199
      CALL AGSBEGM(MODULE,IPRIN)                                          199
      IF (IPRIN.LT.0) RETURN                                              199
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      199
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         199
      BPATH_SFJP='SFJP*'                                                  199
      IDNUM_SFJP(1)=0                                                     200
C VERSION                                                                 200
      SFJP_VERSION=2                                                      200
      CSFJP_VERSION='VERSION'                                             200
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  201
      SFJP_SSRS_RMIN=23.3                                                 201
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  201
     *TURE)'                                                              201
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    202
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           202
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   202
C *                                                                       202
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    202
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           202
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   202
C *                                                                       203
      CALL AGSBEGM(MODULE,IPRIN)                                          203
      IF (IPRIN.LT.0) RETURN                                              203
C Filling bank SFPB - SOME SSD SHELL DIMENSIONS                           203
      BANK_TITLE='SOME SSD SHELL DIMENSIONS'                              203
      BPATH_SFPB='SFPB*'                                                  203
      IDNUM_SFPB(1)=0                                                     204
C HAUT 1                                                                  204
      SFPB_HHIGHT=(0.02/TAN(54*PI/180)+0.02/(2*TAN(PI/5))+0.2)*SIN(PI/    204
     *5)                                                                  204
      CSFPB_HHIGHT='HAUT 1'                                               204
C HAUT 2                                                                  205
      SFPB_KHIGHT=0.02/SIN(54*PI/180)                                     205
      CSFPB_KHIGHT='HAUT 2'                                               205
C BAS 1                                                                   206
      SFPB_HBASE=(0.02/TAN(63*PI/180)+0.01/TAN(27*PI/                     206
     *180)+0.2)*SIN(27*PI/180)                                            206
      CSFPB_HBASE='BAS 1'                                                 206
C BAS 2                                                                   207
      SFPB_KBASE=0.02/SIN(63*PI/180)                                      207
      CSFPB_KBASE='BAS 2'                                                 207
C LENGHT OF THE SIDE KAPTON FILM                                          208
      SFPB_FSIZE=0.6/COS(15*PI/180)+0.02*TAN(15*PI/180)                   208
      CSFPB_FSIZE='LENGHT OF THE SIDE KAPTON FILM'                        208
C Z COORD                                                                 209
      SFPB_ZCOOR=5.18462095393834410                                      209
      CSFPB_ZCOOR='Z COORD'                                               209
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    210
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           210
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   210
C *                                                                       210
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    210
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           210
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   210
C *                                                                       211
      CALL AGSBEGM(MODULE,IPRIN)                                          211
      IF (IPRIN.LT.0) RETURN                                              211
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   211
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      211
      BPATH_SFPA='SFPA*'                                                  211
      IDNUM_SFPA(1)=0                                                     212
C GEOMETRY VERSION                                                        212
      SFPA_VERSION=1                                                      212
      CSFPA_VERSION='GEOMETRY VERSION'                                    212
C MOTHER RMIN                                                             213
      SFPA_RMIN=21.6                                                      213
      CSFPA_RMIN='MOTHER RMIN'                                            213
C MOTHER RMAX                                                             214
      SFPA_RMAX=28.5                                                      214
      CSFPA_RMAX='MOTHER RMAX'                                            214
C OUTER SHIELD CYLINDER, INNER RADIUS                                     215
      SFPA_SOUTRINN=29.5                                                  215
      CSFPA_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                215
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     216
      SFPA_SOUTROUT=29.52                                                 216
      CSFPA_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                216
C OUTER SHIELD CYLINDER, HALF LENGTH                                      217
      SFPA_SOUTLEN=65.4                                                   217
      CSFPA_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  217
C MOTHER LEN ALONG THE Z DIRECTION                                        218
      SFPA_LEN=107.                                                       218
      CSFPA_LEN='MOTHER LEN ALONG THE Z DIRECTION'                        218
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              219
      SFPA_RAD=23.                                                        219
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              219
C NUMBER OF SILICON STRIP DETECTORS                                       220
      SFPA_NSSD=16                                                        220
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      220
C DETECTOR MOTHER WIDTH                                                   221
      SFPA_DMWID=7.8                                                      221
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 221
C DETECTOR MOTHER THICKNESS                                               222
      SFPA_DMTHK=2.0                                                      222
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             222
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          223
      SFPA_DMLEN=90.                                                      223
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        223
C STRUCTURE MOTHER WIDTH                                                  224
      SFPA_SMWID=7.5                                                      224
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                224
C STRUCTURE MOTHER THICKNESS                                              225
      SFPA_SMTHK=5.0                                                      225
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            225
C STRUCTURE MOTHER LENGTH                                                 226
      SFPA_SMLEN=101.9                                                    226
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH'                               226
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  227
      SFPA_SSLEN=95./20.                                                  227
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                227
C LENGTH OF WAFER PACK                                                    228
      SFPA_WPLEN=69.6                                                     228
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  228
C LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          229
      SFPA_SDLEN=4.2                                                      229
      CSFPA_SDLEN='LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        229
C TILING ANGLE (DEGREES)                                                  230
      SFPA_TILT=5.0                                                       230
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 230
C COOLING PIPE OUTER RADIUS                                               231
      SFPA_CPRAD=0.1                                                      231
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             231
C COOLING PIPE INNER RADIUS                                               232
      SFPA_CPRAL=0.09                                                     232
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             232
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            233
      SFPA_CFRAD=0.1                                                      233
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          233
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               234
      SFPA_GPTHK=-1.0                                                     234
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             234
C PRESENCE OF LADDERS                                                     236
      SFPA_LADDERMAP(1)=0                                                 236
      SFPA_LADDERMAP(2)=0                                                 236
      SFPA_LADDERMAP(3)=1                                                 236
      SFPA_LADDERMAP(4)=0                                                 236
      SFPA_LADDERMAP(5)=0                                                 236
      SFPA_LADDERMAP(6)=0                                                 236
      SFPA_LADDERMAP(7)=0                                                 236
      SFPA_LADDERMAP(8)=0                                                 236
      SFPA_LADDERMAP(9)=0                                                 236
      SFPA_LADDERMAP(10)=0                                                236
      SFPA_LADDERMAP(11)=0                                                236
      SFPA_LADDERMAP(12)=0                                                236
      SFPA_LADDERMAP(13)=0                                                236
      SFPA_LADDERMAP(14)=0                                                236
      SFPA_LADDERMAP(15)=0                                                236
      SFPA_LADDERMAP(16)=0                                                236
      SFPA_LADDERMAP(17)=0                                                236
      SFPA_LADDERMAP(18)=0                                                236
      SFPA_LADDERMAP(19)=0                                                236
      SFPA_LADDERMAP(20)=0                                                236
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               236
C INDIVIDUAL ANGLES                                                       240
      SFPA_LADDERANGLE(1)=-1.0                                            240
      SFPA_LADDERANGLE(2)=-1.0                                            240
      SFPA_LADDERANGLE(3)=45.0                                            240
      SFPA_LADDERANGLE(4)=-1.0                                            240
      SFPA_LADDERANGLE(5)=-1.0                                            240
      SFPA_LADDERANGLE(6)=-1.0                                            240
      SFPA_LADDERANGLE(7)=-1.0                                            240
      SFPA_LADDERANGLE(8)=-1.0                                            240
      SFPA_LADDERANGLE(9)=-1.0                                            240
      SFPA_LADDERANGLE(10)=-1.0                                           240
      SFPA_LADDERANGLE(11)=-1.0                                           240
      SFPA_LADDERANGLE(12)=-1.0                                           240
      SFPA_LADDERANGLE(13)=-1.0                                           240
      SFPA_LADDERANGLE(14)=-1.0                                           240
      SFPA_LADDERANGLE(15)=-1.0                                           240
      SFPA_LADDERANGLE(16)=-1.0                                           240
      SFPA_LADDERANGLE(17)=-1.0                                           240
      SFPA_LADDERANGLE(18)=-1.0                                           240
      SFPA_LADDERANGLE(19)=-1.0                                           240
      SFPA_LADDERANGLE(20)=-1.0                                           240
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               240
C INDIVIDUAL TILTS                                                        244
      SFPA_LADDERTILT(1)=-1.0                                             244
      SFPA_LADDERTILT(2)=-1.0                                             244
      SFPA_LADDERTILT(3)=0.0                                              244
      SFPA_LADDERTILT(4)=-1.0                                             244
      SFPA_LADDERTILT(5)=-1.0                                             244
      SFPA_LADDERTILT(6)=-1.0                                             244
      SFPA_LADDERTILT(7)=-1.0                                             244
      SFPA_LADDERTILT(8)=-1.0                                             244
      SFPA_LADDERTILT(9)=-1.0                                             244
      SFPA_LADDERTILT(10)=-1.0                                            244
      SFPA_LADDERTILT(11)=-1.0                                            244
      SFPA_LADDERTILT(12)=-1.0                                            244
      SFPA_LADDERTILT(13)=-1.0                                            244
      SFPA_LADDERTILT(14)=-1.0                                            244
      SFPA_LADDERTILT(15)=-1.0                                            244
      SFPA_LADDERTILT(16)=-1.0                                            244
      SFPA_LADDERTILT(17)=-1.0                                            244
      SFPA_LADDERTILT(18)=-1.0                                            244
      SFPA_LADDERTILT(19)=-1.0                                            244
      SFPA_LADDERTILT(20)=-1.0                                            244
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 244
C INDIVIDUAL RADII                                                        249
      SFPA_LADDERRADIUS(1)=-1.0                                           249
      SFPA_LADDERRADIUS(2)=-1.0                                           249
      SFPA_LADDERRADIUS(3)=23.000                                         249
      SFPA_LADDERRADIUS(4)=-1.0                                           249
      SFPA_LADDERRADIUS(5)=-1.0                                           249
      SFPA_LADDERRADIUS(6)=-1.0                                           249
      SFPA_LADDERRADIUS(7)=-1.0                                           249
      SFPA_LADDERRADIUS(8)=-1.0                                           249
      SFPA_LADDERRADIUS(9)=-1.0                                           249
      SFPA_LADDERRADIUS(10)=-1.0                                          249
      SFPA_LADDERRADIUS(11)=-1.0                                          249
      SFPA_LADDERRADIUS(12)=-1.0                                          249
      SFPA_LADDERRADIUS(13)=-1.0                                          249
      SFPA_LADDERRADIUS(14)=-1.0                                          249
      SFPA_LADDERRADIUS(15)=-1.0                                          249
      SFPA_LADDERRADIUS(16)=-1.0                                          249
      SFPA_LADDERRADIUS(17)=-1.0                                          249
      SFPA_LADDERRADIUS(18)=-1.0                                          249
      SFPA_LADDERRADIUS(19)=-1.0                                          249
      SFPA_LADDERRADIUS(20)=-1.0                                          249
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               249
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    250
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           250
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   250
C *                                                                       250
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    250
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           250
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   250
C *                                                                       251
      CALL AGSBEGM(MODULE,IPRIN)                                          251
      IF (IPRIN.LT.0) RETURN                                              251
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   251
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      251
      BPATH_SFPA='SFPA*'                                                  251
      IDNUM_SFPA(1)=0                                                     252
C GEOMETRY VERSION                                                        252
      SFPA_VERSION=2                                                      252
      CSFPA_VERSION='GEOMETRY VERSION'                                    252
C PRESENCE OF LADDERS                                                     254
      SFPA_LADDERMAP(1)=1                                                 254
      SFPA_LADDERMAP(2)=1                                                 254
      SFPA_LADDERMAP(3)=1                                                 254
      SFPA_LADDERMAP(4)=0                                                 254
      SFPA_LADDERMAP(5)=0                                                 254
      SFPA_LADDERMAP(6)=0                                                 254
      SFPA_LADDERMAP(7)=0                                                 254
      SFPA_LADDERMAP(8)=0                                                 254
      SFPA_LADDERMAP(9)=1                                                 254
      SFPA_LADDERMAP(10)=1                                                254
      SFPA_LADDERMAP(11)=1                                                254
      SFPA_LADDERMAP(12)=1                                                254
      SFPA_LADDERMAP(13)=1                                                254
      SFPA_LADDERMAP(14)=0                                                254
      SFPA_LADDERMAP(15)=0                                                254
      SFPA_LADDERMAP(16)=0                                                254
      SFPA_LADDERMAP(17)=0                                                254
      SFPA_LADDERMAP(18)=0                                                254
      SFPA_LADDERMAP(19)=1                                                254
      SFPA_LADDERMAP(20)=1                                                254
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               254
C INDIVIDUAL ANGLES                                                       258
      SFPA_LADDERANGLE(1)=90.0                                            258
      SFPA_LADDERANGLE(2)=108.3                                           258
      SFPA_LADDERANGLE(3)=130.0                                           258
      SFPA_LADDERANGLE(4)=-1.0                                            258
      SFPA_LADDERANGLE(5)=-1.0                                            258
      SFPA_LADDERANGLE(6)=-1.0                                            258
      SFPA_LADDERANGLE(7)=-1.0                                            258
      SFPA_LADDERANGLE(8)=-1.0                                            258
      SFPA_LADDERANGLE(9)=230.0                                           258
      SFPA_LADDERANGLE(10)=251.7                                          258
      SFPA_LADDERANGLE(11)=270.0                                          258
      SFPA_LADDERANGLE(12)=288.3                                          258
      SFPA_LADDERANGLE(13)=310.0                                          258
      SFPA_LADDERANGLE(14)=-1.0                                           258
      SFPA_LADDERANGLE(15)=-1.0                                           258
      SFPA_LADDERANGLE(16)=-1.0                                           258
      SFPA_LADDERANGLE(17)=-1.0                                           258
      SFPA_LADDERANGLE(18)=-1.0                                           258
      SFPA_LADDERANGLE(19)=50.0                                           258
      SFPA_LADDERANGLE(20)=71.7                                           258
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               258
C INDIVIDUAL TILTS                                                        262
      SFPA_LADDERTILT(1)=0.0                                              262
      SFPA_LADDERTILT(2)=-6.0                                             262
      SFPA_LADDERTILT(3)=0.0                                              262
      SFPA_LADDERTILT(4)=-1.0                                             262
      SFPA_LADDERTILT(5)=-1.0                                             262
      SFPA_LADDERTILT(6)=-1.0                                             262
      SFPA_LADDERTILT(7)=-1.0                                             262
      SFPA_LADDERTILT(8)=0.0                                              262
      SFPA_LADDERTILT(9)=0.0                                              262
      SFPA_LADDERTILT(10)=6.0                                             262
      SFPA_LADDERTILT(11)=0.0                                             262
      SFPA_LADDERTILT(12)=-6.0                                            262
      SFPA_LADDERTILT(13)=0.0                                             262
      SFPA_LADDERTILT(14)=-1.0                                            262
      SFPA_LADDERTILT(15)=-1.0                                            262
      SFPA_LADDERTILT(16)=-1.0                                            262
      SFPA_LADDERTILT(17)=-1.0                                            262
      SFPA_LADDERTILT(18)=-1.0                                            262
      SFPA_LADDERTILT(19)=0.0                                             262
      SFPA_LADDERTILT(20)=6.0                                             262
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 262
C INDIVIDUAL RADII                                                        267
      SFPA_LADDERRADIUS(1)=23.174                                         267
      SFPA_LADDERRADIUS(2)=22.800                                         267
      SFPA_LADDERRADIUS(3)=24.600                                         267
      SFPA_LADDERRADIUS(4)=-1.0                                           267
      SFPA_LADDERRADIUS(5)=-1.0                                           267
      SFPA_LADDERRADIUS(6)=-1.0                                           267
      SFPA_LADDERRADIUS(7)=-1.0                                           267
      SFPA_LADDERRADIUS(8)=-1.0                                           267
      SFPA_LADDERRADIUS(9)=24.600                                         267
      SFPA_LADDERRADIUS(10)=22.800                                        267
      SFPA_LADDERRADIUS(11)=23.174                                        267
      SFPA_LADDERRADIUS(12)=22.800                                        267
      SFPA_LADDERRADIUS(13)=24.600                                        267
      SFPA_LADDERRADIUS(14)=-1.0                                          267
      SFPA_LADDERRADIUS(15)=-1.0                                          267
      SFPA_LADDERRADIUS(16)=-1.0                                          267
      SFPA_LADDERRADIUS(17)=-1.0                                          267
      SFPA_LADDERRADIUS(18)=-1.0                                          267
      SFPA_LADDERRADIUS(19)=24.600                                        267
      SFPA_LADDERRADIUS(20)=22.800                                        267
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               267
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    268
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           268
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   268
C *                                                                       268
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    268
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           268
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   268
C *                                                                       269
      CALL AGSBEGM(MODULE,IPRIN)                                          269
      IF (IPRIN.LT.0) RETURN                                              269
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   269
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      269
      BPATH_SFPA='SFPA*'                                                  269
      IDNUM_SFPA(1)=0                                                     270
C GEOMETRY VERSION                                                        270
      SFPA_VERSION=3                                                      270
      CSFPA_VERSION='GEOMETRY VERSION'                                    270
C PRESENCE OF LADDERS                                                     272
      SFPA_LADDERMAP(1)=1                                                 272
      SFPA_LADDERMAP(2)=1                                                 272
      SFPA_LADDERMAP(3)=1                                                 272
      SFPA_LADDERMAP(4)=1                                                 272
      SFPA_LADDERMAP(5)=1                                                 272
      SFPA_LADDERMAP(6)=1                                                 272
      SFPA_LADDERMAP(7)=1                                                 272
      SFPA_LADDERMAP(8)=1                                                 272
      SFPA_LADDERMAP(9)=1                                                 272
      SFPA_LADDERMAP(10)=1                                                272
      SFPA_LADDERMAP(11)=1                                                272
      SFPA_LADDERMAP(12)=1                                                272
      SFPA_LADDERMAP(13)=1                                                272
      SFPA_LADDERMAP(14)=1                                                272
      SFPA_LADDERMAP(15)=1                                                272
      SFPA_LADDERMAP(16)=1                                                272
      SFPA_LADDERMAP(17)=1                                                272
      SFPA_LADDERMAP(18)=1                                                272
      SFPA_LADDERMAP(19)=1                                                272
      SFPA_LADDERMAP(20)=1                                                272
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               272
C INDIVIDUAL ANGLES                                                       276
      SFPA_LADDERANGLE(1)=90.0                                            276
      SFPA_LADDERANGLE(2)=108.3                                           276
      SFPA_LADDERANGLE(3)=126.6                                           276
      SFPA_LADDERANGLE(4)=144.4                                           276
      SFPA_LADDERANGLE(5)=162.2                                           276
      SFPA_LADDERANGLE(6)=180.0                                           276
      SFPA_LADDERANGLE(7)=197.8                                           276
      SFPA_LADDERANGLE(8)=215.6                                           276
      SFPA_LADDERANGLE(9)=233.4                                           276
      SFPA_LADDERANGLE(10)=251.7                                          276
      SFPA_LADDERANGLE(11)=270.0                                          276
      SFPA_LADDERANGLE(12)=288.3                                          276
      SFPA_LADDERANGLE(13)=306.6                                          276
      SFPA_LADDERANGLE(14)=324.4                                          276
      SFPA_LADDERANGLE(15)=342.2                                          276
      SFPA_LADDERANGLE(16)=0.0                                            276
      SFPA_LADDERANGLE(17)=17.8                                           276
      SFPA_LADDERANGLE(18)=35.6                                           276
      SFPA_LADDERANGLE(19)=53.4                                           276
      SFPA_LADDERANGLE(20)=71.7                                           276
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               276
C INDIVIDUAL TILTS                                                        280
      SFPA_LADDERTILT(1)=0.0                                              280
      SFPA_LADDERTILT(2)=-6.0                                             280
      SFPA_LADDERTILT(3)=-7.0                                             280
      SFPA_LADDERTILT(4)=-7.0                                             280
      SFPA_LADDERTILT(5)=-7.0                                             280
      SFPA_LADDERTILT(6)=0.0                                              280
      SFPA_LADDERTILT(7)=7.0                                              280
      SFPA_LADDERTILT(8)=7.0                                              280
      SFPA_LADDERTILT(9)=7.0                                              280
      SFPA_LADDERTILT(10)=6.0                                             280
      SFPA_LADDERTILT(11)=0.0                                             280
      SFPA_LADDERTILT(12)=-6.0                                            280
      SFPA_LADDERTILT(13)=-7.0                                            280
      SFPA_LADDERTILT(14)=-7.0                                            280
      SFPA_LADDERTILT(15)=-7.0                                            280
      SFPA_LADDERTILT(16)=0.0                                             280
      SFPA_LADDERTILT(17)=7.0                                             280
      SFPA_LADDERTILT(18)=7.0                                             280
      SFPA_LADDERTILT(19)=7.0                                             280
      SFPA_LADDERTILT(20)=6.0                                             280
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 280
C INDIVIDUAL RADII                                                        285
      SFPA_LADDERRADIUS(1)=23.177                                         285
      SFPA_LADDERRADIUS(2)=22.800                                         285
      SFPA_LADDERRADIUS(3)=22.800                                         285
      SFPA_LADDERRADIUS(4)=22.800                                         285
      SFPA_LADDERRADIUS(5)=23.800                                         285
      SFPA_LADDERRADIUS(6)=22.500                                         285
      SFPA_LADDERRADIUS(7)=23.800                                         285
      SFPA_LADDERRADIUS(8)=22.800                                         285
      SFPA_LADDERRADIUS(9)=22.800                                         285
      SFPA_LADDERRADIUS(10)=22.800                                        285
      SFPA_LADDERRADIUS(11)=23.177                                        285
      SFPA_LADDERRADIUS(12)=22.800                                        285
      SFPA_LADDERRADIUS(13)=22.800                                        285
      SFPA_LADDERRADIUS(14)=22.800                                        285
      SFPA_LADDERRADIUS(15)=23.800                                        285
      SFPA_LADDERRADIUS(16)=22.500                                        285
      SFPA_LADDERRADIUS(17)=23.800                                        285
      SFPA_LADDERRADIUS(18)=22.800                                        285
      SFPA_LADDERRADIUS(19)=22.800                                        285
      SFPA_LADDERRADIUS(20)=22.800                                        285
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               285
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    286
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           286
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   286
C *                                                                       286
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    286
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           286
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   286
C *                                                                       287
      CALL AGSBEGM(MODULE,IPRIN)                                          287
      IF (IPRIN.LT.0) RETURN                                              287
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   287
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      287
      BPATH_SFPA='SFPA*'                                                  287
      IDNUM_SFPA(1)=0                                                     288
C GEOMETRY VERSION                                                        288
      SFPA_VERSION=4                                                      288
      CSFPA_VERSION='GEOMETRY VERSION'                                    288
C PRESENCE OF LADDERS                                                     290
      SFPA_LADDERMAP(1)=1                                                 290
      SFPA_LADDERMAP(2)=1                                                 290
      SFPA_LADDERMAP(3)=1                                                 290
      SFPA_LADDERMAP(4)=1                                                 290
      SFPA_LADDERMAP(5)=1                                                 290
      SFPA_LADDERMAP(6)=1                                                 290
      SFPA_LADDERMAP(7)=1                                                 290
      SFPA_LADDERMAP(8)=1                                                 290
      SFPA_LADDERMAP(9)=1                                                 290
      SFPA_LADDERMAP(10)=1                                                290
      SFPA_LADDERMAP(11)=1                                                290
      SFPA_LADDERMAP(12)=1                                                290
      SFPA_LADDERMAP(13)=1                                                290
      SFPA_LADDERMAP(14)=1                                                290
      SFPA_LADDERMAP(15)=1                                                290
      SFPA_LADDERMAP(16)=1                                                290
      SFPA_LADDERMAP(17)=1                                                290
      SFPA_LADDERMAP(18)=1                                                290
      SFPA_LADDERMAP(19)=1                                                290
      SFPA_LADDERMAP(20)=1                                                290
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               290
C INDIVIDUAL ANGLES                                                       294
      SFPA_LADDERANGLE(1)=90.0                                            294
      SFPA_LADDERANGLE(2)=108.3                                           294
      SFPA_LADDERANGLE(3)=126.6                                           294
      SFPA_LADDERANGLE(4)=144.4                                           294
      SFPA_LADDERANGLE(5)=162.2                                           294
      SFPA_LADDERANGLE(6)=180.0                                           294
      SFPA_LADDERANGLE(7)=197.8                                           294
      SFPA_LADDERANGLE(8)=215.6                                           294
      SFPA_LADDERANGLE(9)=233.4                                           294
      SFPA_LADDERANGLE(10)=251.7                                          294
      SFPA_LADDERANGLE(11)=270.0                                          294
      SFPA_LADDERANGLE(12)=288.3                                          294
      SFPA_LADDERANGLE(13)=306.6                                          294
      SFPA_LADDERANGLE(14)=324.4                                          294
      SFPA_LADDERANGLE(15)=342.2                                          294
      SFPA_LADDERANGLE(16)=0.0                                            294
      SFPA_LADDERANGLE(17)=17.8                                           294
      SFPA_LADDERANGLE(18)=35.6                                           294
      SFPA_LADDERANGLE(19)=53.4                                           294
      SFPA_LADDERANGLE(20)=71.7                                           294
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               294
C INDIVIDUAL TILTS                                                        298
      SFPA_LADDERTILT(1)=0.0                                              298
      SFPA_LADDERTILT(2)=-6.0                                             298
      SFPA_LADDERTILT(3)=-7.0                                             298
      SFPA_LADDERTILT(4)=-7.0                                             298
      SFPA_LADDERTILT(5)=-7.0                                             298
      SFPA_LADDERTILT(6)=0.0                                              298
      SFPA_LADDERTILT(7)=7.0                                              298
      SFPA_LADDERTILT(8)=7.0                                              298
      SFPA_LADDERTILT(9)=7.0                                              298
      SFPA_LADDERTILT(10)=6.0                                             298
      SFPA_LADDERTILT(11)=0.0                                             298
      SFPA_LADDERTILT(12)=-6.0                                            298
      SFPA_LADDERTILT(13)=-7.0                                            298
      SFPA_LADDERTILT(14)=-7.0                                            298
      SFPA_LADDERTILT(15)=-7.0                                            298
      SFPA_LADDERTILT(16)=0.0                                             298
      SFPA_LADDERTILT(17)=7.0                                             298
      SFPA_LADDERTILT(18)=7.0                                             298
      SFPA_LADDERTILT(19)=7.0                                             298
      SFPA_LADDERTILT(20)=6.0                                             298
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 298
C INDIVIDUAL RADII                                                        303
      SFPA_LADDERRADIUS(1)=23.177                                         303
      SFPA_LADDERRADIUS(2)=22.800                                         303
      SFPA_LADDERRADIUS(3)=22.800                                         303
      SFPA_LADDERRADIUS(4)=22.800                                         303
      SFPA_LADDERRADIUS(5)=22.800                                         303
      SFPA_LADDERRADIUS(6)=22.500                                         303
      SFPA_LADDERRADIUS(7)=22.800                                         303
      SFPA_LADDERRADIUS(8)=22.800                                         303
      SFPA_LADDERRADIUS(9)=22.800                                         303
      SFPA_LADDERRADIUS(10)=22.800                                        303
      SFPA_LADDERRADIUS(11)=23.177                                        303
      SFPA_LADDERRADIUS(12)=22.800                                        303
      SFPA_LADDERRADIUS(13)=22.800                                        303
      SFPA_LADDERRADIUS(14)=22.800                                        303
      SFPA_LADDERRADIUS(15)=22.800                                        303
      SFPA_LADDERRADIUS(16)=22.500                                        303
      SFPA_LADDERRADIUS(17)=22.800                                        303
      SFPA_LADDERRADIUS(18)=22.800                                        303
      SFPA_LADDERRADIUS(19)=22.800                                        303
      SFPA_LADDERRADIUS(20)=22.800                                        303
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               303
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    304
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           304
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   304
C *                                                                       304
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    304
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           304
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   304
C *                                                                       305
      CALL AGSBEGM(MODULE,IPRIN)                                          305
      IF (IPRIN.LT.0) RETURN                                              305
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   305
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      305
      BPATH_SFPA='SFPA*'                                                  305
      IDNUM_SFPA(1)=0                                                     306
C GEOMETRY VERSION                                                        306
      SFPA_VERSION=5                                                      306
      CSFPA_VERSION='GEOMETRY VERSION'                                    306
C PRESENCE OF LADDERS                                                     308
      SFPA_LADDERMAP(1)=1                                                 308
      SFPA_LADDERMAP(2)=1                                                 308
      SFPA_LADDERMAP(3)=1                                                 308
      SFPA_LADDERMAP(4)=1                                                 308
      SFPA_LADDERMAP(5)=1                                                 308
      SFPA_LADDERMAP(6)=1                                                 308
      SFPA_LADDERMAP(7)=1                                                 308
      SFPA_LADDERMAP(8)=1                                                 308
      SFPA_LADDERMAP(9)=1                                                 308
      SFPA_LADDERMAP(10)=1                                                308
      SFPA_LADDERMAP(11)=1                                                308
      SFPA_LADDERMAP(12)=1                                                308
      SFPA_LADDERMAP(13)=1                                                308
      SFPA_LADDERMAP(14)=1                                                308
      SFPA_LADDERMAP(15)=1                                                308
      SFPA_LADDERMAP(16)=1                                                308
      SFPA_LADDERMAP(17)=1                                                308
      SFPA_LADDERMAP(18)=1                                                308
      SFPA_LADDERMAP(19)=1                                                308
      SFPA_LADDERMAP(20)=1                                                308
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               308
C INDIVIDUAL ANGLES                                                       312
      SFPA_LADDERANGLE(1)=90.0                                            312
      SFPA_LADDERANGLE(2)=108.3                                           312
      SFPA_LADDERANGLE(3)=126.6                                           312
      SFPA_LADDERANGLE(4)=144.4                                           312
      SFPA_LADDERANGLE(5)=162.2                                           312
      SFPA_LADDERANGLE(6)=180.0                                           312
      SFPA_LADDERANGLE(7)=197.8                                           312
      SFPA_LADDERANGLE(8)=215.6                                           312
      SFPA_LADDERANGLE(9)=233.4                                           312
      SFPA_LADDERANGLE(10)=251.7                                          312
      SFPA_LADDERANGLE(11)=270.0                                          312
      SFPA_LADDERANGLE(12)=288.3                                          312
      SFPA_LADDERANGLE(13)=306.6                                          312
      SFPA_LADDERANGLE(14)=324.4                                          312
      SFPA_LADDERANGLE(15)=342.2                                          312
      SFPA_LADDERANGLE(16)=0.0                                            312
      SFPA_LADDERANGLE(17)=17.8                                           312
      SFPA_LADDERANGLE(18)=35.6                                           312
      SFPA_LADDERANGLE(19)=53.4                                           312
      SFPA_LADDERANGLE(20)=71.7                                           312
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               312
C INDIVIDUAL TILTS                                                        316
      SFPA_LADDERTILT(1)=0.0                                              316
      SFPA_LADDERTILT(2)=-6.0                                             316
      SFPA_LADDERTILT(3)=-7.0                                             316
      SFPA_LADDERTILT(4)=-7.0                                             316
      SFPA_LADDERTILT(5)=-7.0                                             316
      SFPA_LADDERTILT(6)=0.0                                              316
      SFPA_LADDERTILT(7)=7.0                                              316
      SFPA_LADDERTILT(8)=7.0                                              316
      SFPA_LADDERTILT(9)=7.0                                              316
      SFPA_LADDERTILT(10)=6.0                                             316
      SFPA_LADDERTILT(11)=0.0                                             316
      SFPA_LADDERTILT(12)=-6.0                                            316
      SFPA_LADDERTILT(13)=-7.0                                            316
      SFPA_LADDERTILT(14)=-7.0                                            316
      SFPA_LADDERTILT(15)=-7.0                                            316
      SFPA_LADDERTILT(16)=0.0                                             316
      SFPA_LADDERTILT(17)=7.0                                             316
      SFPA_LADDERTILT(18)=7.0                                             316
      SFPA_LADDERTILT(19)=7.0                                             316
      SFPA_LADDERTILT(20)=6.0                                             316
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 316
C INDIVIDUAL RADII                                                        321
      SFPA_LADDERRADIUS(1)=23.177                                         321
      SFPA_LADDERRADIUS(2)=22.800                                         321
      SFPA_LADDERRADIUS(3)=22.600                                         321
      SFPA_LADDERRADIUS(4)=22.600                                         321
      SFPA_LADDERRADIUS(5)=22.600                                         321
      SFPA_LADDERRADIUS(6)=22.300                                         321
      SFPA_LADDERRADIUS(7)=22.600                                         321
      SFPA_LADDERRADIUS(8)=22.600                                         321
      SFPA_LADDERRADIUS(9)=22.600                                         321
      SFPA_LADDERRADIUS(10)=22.800                                        321
      SFPA_LADDERRADIUS(11)=23.177                                        321
      SFPA_LADDERRADIUS(12)=22.800                                        321
      SFPA_LADDERRADIUS(13)=22.600                                        321
      SFPA_LADDERRADIUS(14)=22.600                                        321
      SFPA_LADDERRADIUS(15)=22.600                                        321
      SFPA_LADDERRADIUS(16)=22.500                                        321
      SFPA_LADDERRADIUS(17)=22.600                                        321
      SFPA_LADDERRADIUS(18)=22.600                                        321
      SFPA_LADDERRADIUS(19)=22.600                                        321
      SFPA_LADDERRADIUS(20)=22.800                                        321
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               321
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    322
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           322
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   322
C *                                                                       322
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    322
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           322
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   322
C *                                                                       323
      CALL AGSBEGM(MODULE,IPRIN)                                          323
      IF (IPRIN.LT.0) RETURN                                              323
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   323
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      323
      BPATH_SFPA='SFPA*'                                                  323
      IDNUM_SFPA(1)=0                                                     324
C GEOMETRY VERSION                                                        324
      SFPA_VERSION=6                                                      324
      CSFPA_VERSION='GEOMETRY VERSION'                                    324
C PRESENCE OF LADDERS                                                     326
      SFPA_LADDERMAP(1)=1                                                 326
      SFPA_LADDERMAP(2)=1                                                 326
      SFPA_LADDERMAP(3)=1                                                 326
      SFPA_LADDERMAP(4)=1                                                 326
      SFPA_LADDERMAP(5)=1                                                 326
      SFPA_LADDERMAP(6)=1                                                 326
      SFPA_LADDERMAP(7)=1                                                 326
      SFPA_LADDERMAP(8)=1                                                 326
      SFPA_LADDERMAP(9)=1                                                 326
      SFPA_LADDERMAP(10)=1                                                326
      SFPA_LADDERMAP(11)=1                                                326
      SFPA_LADDERMAP(12)=1                                                326
      SFPA_LADDERMAP(13)=1                                                326
      SFPA_LADDERMAP(14)=1                                                326
      SFPA_LADDERMAP(15)=1                                                326
      SFPA_LADDERMAP(16)=1                                                326
      SFPA_LADDERMAP(17)=1                                                326
      SFPA_LADDERMAP(18)=1                                                326
      SFPA_LADDERMAP(19)=1                                                326
      SFPA_LADDERMAP(20)=1                                                326
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               326
C INDIVIDUAL ANGLES                                                       330
      SFPA_LADDERANGLE(1)=99.0                                            330
      SFPA_LADDERANGLE(2)=117.3                                           330
      SFPA_LADDERANGLE(3)=135.6                                           330
      SFPA_LADDERANGLE(4)=153.4                                           330
      SFPA_LADDERANGLE(5)=171.2                                           330
      SFPA_LADDERANGLE(6)=189.0                                           330
      SFPA_LADDERANGLE(7)=206.8                                           330
      SFPA_LADDERANGLE(8)=224.6                                           330
      SFPA_LADDERANGLE(9)=242.4                                           330
      SFPA_LADDERANGLE(10)=260.7                                          330
      SFPA_LADDERANGLE(11)=279.0                                          330
      SFPA_LADDERANGLE(12)=297.3                                          330
      SFPA_LADDERANGLE(13)=315.6                                          330
      SFPA_LADDERANGLE(14)=333.4                                          330
      SFPA_LADDERANGLE(15)=351.2                                          330
      SFPA_LADDERANGLE(16)=9.0                                            330
      SFPA_LADDERANGLE(17)=26.8                                           330
      SFPA_LADDERANGLE(18)=44.6                                           330
      SFPA_LADDERANGLE(19)=62.4                                           330
      SFPA_LADDERANGLE(20)=80.2                                           330
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               330
C INDIVIDUAL TILTS                                                        334
      SFPA_LADDERTILT(1)=-12.0                                            334
      SFPA_LADDERTILT(2)=-12.0                                            334
      SFPA_LADDERTILT(3)=-12.0                                            334
      SFPA_LADDERTILT(4)=-12.0                                            334
      SFPA_LADDERTILT(5)=-12.0                                            334
      SFPA_LADDERTILT(6)=-12.0                                            334
      SFPA_LADDERTILT(7)=-12.0                                            334
      SFPA_LADDERTILT(8)=-12.0                                            334
      SFPA_LADDERTILT(9)=-12.0                                            334
      SFPA_LADDERTILT(10)=-12.0                                           334
      SFPA_LADDERTILT(11)=-12.0                                           334
      SFPA_LADDERTILT(12)=-12.0                                           334
      SFPA_LADDERTILT(13)=-12.0                                           334
      SFPA_LADDERTILT(14)=-12.0                                           334
      SFPA_LADDERTILT(15)=-12.0                                           334
      SFPA_LADDERTILT(16)=-12.0                                           334
      SFPA_LADDERTILT(17)=-12.0                                           334
      SFPA_LADDERTILT(18)=-12.0                                           334
      SFPA_LADDERTILT(19)=-12.0                                           334
      SFPA_LADDERTILT(20)=-12.0                                           334
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 334
C INDIVIDUAL RADII                                                        338
      SFPA_LADDERRADIUS(1)=22.3                                           338
      SFPA_LADDERRADIUS(2)=22.3                                           338
      SFPA_LADDERRADIUS(3)=22.3                                           338
      SFPA_LADDERRADIUS(4)=22.3                                           338
      SFPA_LADDERRADIUS(5)=22.3                                           338
      SFPA_LADDERRADIUS(6)=22.3                                           338
      SFPA_LADDERRADIUS(7)=22.3                                           338
      SFPA_LADDERRADIUS(8)=22.3                                           338
      SFPA_LADDERRADIUS(9)=22.3                                           338
      SFPA_LADDERRADIUS(10)=22.3                                          338
      SFPA_LADDERRADIUS(11)=22.3                                          338
      SFPA_LADDERRADIUS(12)=22.3                                          338
      SFPA_LADDERRADIUS(13)=22.3                                          338
      SFPA_LADDERRADIUS(14)=22.3                                          338
      SFPA_LADDERRADIUS(15)=22.3                                          338
      SFPA_LADDERRADIUS(16)=22.3                                          338
      SFPA_LADDERRADIUS(17)=22.3                                          338
      SFPA_LADDERRADIUS(18)=22.3                                          338
      SFPA_LADDERRADIUS(19)=22.3                                          338
      SFPA_LADDERRADIUS(20)=22.3                                          338
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               338
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    339
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           339
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   339
C *                                                                       339
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    339
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           339
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   339
C *                                                                       340
      CALL AGSBEGM(MODULE,IPRIN)                                          340
      IF (IPRIN.LT.0) RETURN                                              340
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   340
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      340
      BPATH_SFPA='SFPA*'                                                  340
      IDNUM_SFPA(1)=0                                                     341
C GEOMETRY VERSION                                                        341
      SFPA_VERSION=7                                                      341
      CSFPA_VERSION='GEOMETRY VERSION'                                    341
C PRESENCE OF LADDERS                                                     343
      SFPA_LADDERMAP(1)=1                                                 343
      SFPA_LADDERMAP(2)=1                                                 343
      SFPA_LADDERMAP(3)=1                                                 343
      SFPA_LADDERMAP(4)=1                                                 343
      SFPA_LADDERMAP(5)=1                                                 343
      SFPA_LADDERMAP(6)=1                                                 343
      SFPA_LADDERMAP(7)=1                                                 343
      SFPA_LADDERMAP(8)=1                                                 343
      SFPA_LADDERMAP(9)=1                                                 343
      SFPA_LADDERMAP(10)=1                                                343
      SFPA_LADDERMAP(11)=1                                                343
      SFPA_LADDERMAP(12)=1                                                343
      SFPA_LADDERMAP(13)=1                                                343
      SFPA_LADDERMAP(14)=1                                                343
      SFPA_LADDERMAP(15)=1                                                343
      SFPA_LADDERMAP(16)=1                                                343
      SFPA_LADDERMAP(17)=1                                                343
      SFPA_LADDERMAP(18)=1                                                343
      SFPA_LADDERMAP(19)=1                                                343
      SFPA_LADDERMAP(20)=1                                                343
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               343
C INDIVIDUAL ANGLES                                                       347
      SFPA_LADDERANGLE(1)=81.0                                            347
      SFPA_LADDERANGLE(2)=99.0                                            347
      SFPA_LADDERANGLE(3)=117.0                                           347
      SFPA_LADDERANGLE(4)=135.0                                           347
      SFPA_LADDERANGLE(5)=153.0                                           347
      SFPA_LADDERANGLE(6)=171.0                                           347
      SFPA_LADDERANGLE(7)=189.0                                           347
      SFPA_LADDERANGLE(8)=207.0                                           347
      SFPA_LADDERANGLE(9)=225.0                                           347
      SFPA_LADDERANGLE(10)=243.0                                          347
      SFPA_LADDERANGLE(11)=261.0                                          347
      SFPA_LADDERANGLE(12)=279.0                                          347
      SFPA_LADDERANGLE(13)=297.0                                          347
      SFPA_LADDERANGLE(14)=315                                            347
      SFPA_LADDERANGLE(15)=333                                            347
      SFPA_LADDERANGLE(16)=351                                            347
      SFPA_LADDERANGLE(17)=9.0                                            347
      SFPA_LADDERANGLE(18)=27.0                                           347
      SFPA_LADDERANGLE(19)=45.0                                           347
      SFPA_LADDERANGLE(20)=63.0                                           347
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               347
C INDIVIDUAL TILTS                                                        351
      SFPA_LADDERTILT(1)=-7.0                                             351
      SFPA_LADDERTILT(2)=-7.0                                             351
      SFPA_LADDERTILT(3)=-7.0                                             351
      SFPA_LADDERTILT(4)=-7.0                                             351
      SFPA_LADDERTILT(5)=-7.0                                             351
      SFPA_LADDERTILT(6)=-7.0                                             351
      SFPA_LADDERTILT(7)=-7.0                                             351
      SFPA_LADDERTILT(8)=-7.0                                             351
      SFPA_LADDERTILT(9)=-7.0                                             351
      SFPA_LADDERTILT(10)=-7.0                                            351
      SFPA_LADDERTILT(11)=-7.0                                            351
      SFPA_LADDERTILT(12)=-7.0                                            351
      SFPA_LADDERTILT(13)=-7.0                                            351
      SFPA_LADDERTILT(14)=-7.0                                            351
      SFPA_LADDERTILT(15)=-7.0                                            351
      SFPA_LADDERTILT(16)=-7.0                                            351
      SFPA_LADDERTILT(17)=-7.0                                            351
      SFPA_LADDERTILT(18)=-7.0                                            351
      SFPA_LADDERTILT(19)=-7.0                                            351
      SFPA_LADDERTILT(20)=-7.0                                            351
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 351
C INDIVIDUAL RADII                                                        355
      SFPA_LADDERRADIUS(1)=22.3                                           355
      SFPA_LADDERRADIUS(2)=22.3                                           355
      SFPA_LADDERRADIUS(3)=22.3                                           355
      SFPA_LADDERRADIUS(4)=22.3                                           355
      SFPA_LADDERRADIUS(5)=22.3                                           355
      SFPA_LADDERRADIUS(6)=22.3                                           355
      SFPA_LADDERRADIUS(7)=22.3                                           355
      SFPA_LADDERRADIUS(8)=22.3                                           355
      SFPA_LADDERRADIUS(9)=22.3                                           355
      SFPA_LADDERRADIUS(10)=22.3                                          355
      SFPA_LADDERRADIUS(11)=22.3                                          355
      SFPA_LADDERRADIUS(12)=22.3                                          355
      SFPA_LADDERRADIUS(13)=22.3                                          355
      SFPA_LADDERRADIUS(14)=22.3                                          355
      SFPA_LADDERRADIUS(15)=22.3                                          355
      SFPA_LADDERRADIUS(16)=22.3                                          355
      SFPA_LADDERRADIUS(17)=22.3                                          355
      SFPA_LADDERRADIUS(18)=22.3                                          355
      SFPA_LADDERRADIUS(19)=22.3                                          355
      SFPA_LADDERRADIUS(20)=22.3                                          355
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               355
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    356
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           356
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   356
C *                                                                       356
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    356
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           356
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   356
C *                                                                       357
      CALL AGSBEGM(MODULE,IPRIN)                                          357
      IF (IPRIN.LT.0) RETURN                                              357
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   357
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      357
      BPATH_SFPA='SFPA*'                                                  357
      IDNUM_SFPA(1)=0                                                     358
C GEOMETRY VERSION                                                        358
      SFPA_VERSION=7.1                                                    358
      CSFPA_VERSION='GEOMETRY VERSION'                                    358
C PRESENCE OF LADDERS                                                     360
      SFPA_LADDERMAP(1)=1                                                 360
      SFPA_LADDERMAP(2)=1                                                 360
      SFPA_LADDERMAP(3)=1                                                 360
      SFPA_LADDERMAP(4)=1                                                 360
      SFPA_LADDERMAP(5)=1                                                 360
      SFPA_LADDERMAP(6)=1                                                 360
      SFPA_LADDERMAP(7)=1                                                 360
      SFPA_LADDERMAP(8)=1                                                 360
      SFPA_LADDERMAP(9)=1                                                 360
      SFPA_LADDERMAP(10)=1                                                360
      SFPA_LADDERMAP(11)=1                                                360
      SFPA_LADDERMAP(12)=1                                                360
      SFPA_LADDERMAP(13)=1                                                360
      SFPA_LADDERMAP(14)=1                                                360
      SFPA_LADDERMAP(15)=1                                                360
      SFPA_LADDERMAP(16)=1                                                360
      SFPA_LADDERMAP(17)=1                                                360
      SFPA_LADDERMAP(18)=1                                                360
      SFPA_LADDERMAP(19)=1                                                360
      SFPA_LADDERMAP(20)=1                                                360
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               360
C INDIVIDUAL ANGLES, REVISED NUMBERING 1/12/15                            364
      SFPA_LADDERANGLE(1)=81.0                                            364
      SFPA_LADDERANGLE(2)=63.0                                            364
      SFPA_LADDERANGLE(3)=45.0                                            364
      SFPA_LADDERANGLE(4)=27.0                                            364
      SFPA_LADDERANGLE(5)=9.0                                             364
      SFPA_LADDERANGLE(6)=351.0                                           364
      SFPA_LADDERANGLE(7)=333.0                                           364
      SFPA_LADDERANGLE(8)=315.0                                           364
      SFPA_LADDERANGLE(9)=297.0                                           364
      SFPA_LADDERANGLE(10)=279.0                                          364
      SFPA_LADDERANGLE(11)=261.0                                          364
      SFPA_LADDERANGLE(12)=243.0                                          364
      SFPA_LADDERANGLE(13)=225.0                                          364
      SFPA_LADDERANGLE(14)=207.0                                          364
      SFPA_LADDERANGLE(15)=189.0                                          364
      SFPA_LADDERANGLE(16)=171.0                                          364
      SFPA_LADDERANGLE(17)=153.0                                          364
      SFPA_LADDERANGLE(18)=135.0                                          364
      SFPA_LADDERANGLE(19)=117.0                                          364
      SFPA_LADDERANGLE(20)=99.0                                           364
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES, REVISED NUMBERING 1/12/15'    364
C INDIVIDUAL TILTS                                                        368
      SFPA_LADDERTILT(1)=-7.0                                             368
      SFPA_LADDERTILT(2)=-7.0                                             368
      SFPA_LADDERTILT(3)=-7.0                                             368
      SFPA_LADDERTILT(4)=-7.0                                             368
      SFPA_LADDERTILT(5)=-7.0                                             368
      SFPA_LADDERTILT(6)=-7.0                                             368
      SFPA_LADDERTILT(7)=-7.0                                             368
      SFPA_LADDERTILT(8)=-7.0                                             368
      SFPA_LADDERTILT(9)=-7.0                                             368
      SFPA_LADDERTILT(10)=-7.0                                            368
      SFPA_LADDERTILT(11)=-7.0                                            368
      SFPA_LADDERTILT(12)=-7.0                                            368
      SFPA_LADDERTILT(13)=-7.0                                            368
      SFPA_LADDERTILT(14)=-7.0                                            368
      SFPA_LADDERTILT(15)=-7.0                                            368
      SFPA_LADDERTILT(16)=-7.0                                            368
      SFPA_LADDERTILT(17)=-7.0                                            368
      SFPA_LADDERTILT(18)=-7.0                                            368
      SFPA_LADDERTILT(19)=-7.0                                            368
      SFPA_LADDERTILT(20)=-7.0                                            368
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 368
C INDIVIDUAL RADII                                                        372
      SFPA_LADDERRADIUS(1)=22.3                                           372
      SFPA_LADDERRADIUS(2)=22.3                                           372
      SFPA_LADDERRADIUS(3)=22.3                                           372
      SFPA_LADDERRADIUS(4)=22.3                                           372
      SFPA_LADDERRADIUS(5)=22.3                                           372
      SFPA_LADDERRADIUS(6)=22.3                                           372
      SFPA_LADDERRADIUS(7)=22.3                                           372
      SFPA_LADDERRADIUS(8)=22.3                                           372
      SFPA_LADDERRADIUS(9)=22.3                                           372
      SFPA_LADDERRADIUS(10)=22.3                                          372
      SFPA_LADDERRADIUS(11)=22.3                                          372
      SFPA_LADDERRADIUS(12)=22.3                                          372
      SFPA_LADDERRADIUS(13)=22.3                                          372
      SFPA_LADDERRADIUS(14)=22.3                                          372
      SFPA_LADDERRADIUS(15)=22.3                                          372
      SFPA_LADDERRADIUS(16)=22.3                                          372
      SFPA_LADDERRADIUS(17)=22.3                                          372
      SFPA_LADDERRADIUS(18)=22.3                                          372
      SFPA_LADDERRADIUS(19)=22.3                                          372
      SFPA_LADDERRADIUS(20)=22.3                                          372
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               372
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    373
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           373
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   373
C *                                                                       373
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    373
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           373
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   373
C use default SSDP bank                                                   374
      BPATH_SSDP='SSDP*'                                                  374
      IDNUM_SSDP(1)=0                                                     374
      CALL AGDGETP('SISDGEO7','*',-999,'SSDP',' ',BANK_SSDP, BPATH_SSDP,  374
     *IDNUM_SSDP,1,MAP_SSDP,NAMES_SSDP,BANK_SSDP,LEN_SSDP,LLEN_SSDP,      374
     *LINK_SSDP,FLAG_SSDP)                                                374
      CONFIGA = MOD(SSDP_CONFIG ,10)                                      375
      CONFIGB = MOD(SSDP_CONFIG/10,10)                                    376
C use SFPA bank                                                           377
      BPATH_SFPA='SFPA*'                                                  377
      IDNUM_SFPA(1)=0                                                     377
      SFPA_VERSION=7.1                                                    377
      CALL AGDGETP('SISDGEO7','*',-999,'SFPA',NSFPA_VERSION,              377
     *RE_SFPA_VERSION,BPATH_SFPA,IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,        377
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   377
C use SFJP bank                                                           378
      BPATH_SFJP='SFJP*'                                                  378
      IDNUM_SFJP(1)=0                                                     378
      SFJP_VERSION=2                                                      378
      CALL AGDGETP('SISDGEO7','*',-999,'SFJP',NSFJP_VERSION,              378
     *RE_SFJP_VERSION,BPATH_SFJP,IDNUM_SFJP,1,MAP_SFJP,NAMES_SFJP,        378
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   378
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             379
      CALL AGSRESET                                                       379
      AG_A = 28.08                                                        379
      AG_Z = 14                                                           379
      AG_W = 0.6*1*28./60.                                                379
      AG_TITLE = 'SI'                                                     379
      AG_EXNAME = 'COMPONENT'                                             379
      AG_PARLIST = 'A_Z_W'                                                379
      CALL AXCOMPONENT                                                    379
C *                                                                       379
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  379
C =                                                                       379
      CALL AGSRESET                                                       380
      AG_A = 16                                                           380
      AG_Z = 8                                                            380
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               380
      AG_TITLE = 'O'                                                      380
      AG_EXNAME = 'COMPONENT'                                             380
      AG_PARLIST = 'A_Z_W'                                                380
      CALL AXCOMPONENT                                                    380
C *                                                                       380
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 380
      CALL AGSRESET                                                       381
      AG_A = 12                                                           381
      AG_Z = 6                                                            381
      AG_W = 0.4*8*12./174.                                               381
      AG_TITLE = 'C'                                                      381
      AG_EXNAME = 'COMPONENT'                                             381
      AG_PARLIST = 'A_Z_W'                                                381
      CALL AXCOMPONENT                                                    381
C *                                                                       381
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  381
      CALL AGSRESET                                                       382
      AG_A = 1                                                            382
      AG_Z = 1                                                            382
      AG_W = 0.4*14*1./174.                                               382
      AG_TITLE = 'H'                                                      382
      AG_EXNAME = 'COMPONENT'                                             382
      AG_PARLIST = 'A_Z_W'                                                382
      CALL AXCOMPONENT                                                    382
C *                                                                       382
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  382
      CALL AGSRESET                                                       383
      AG_DENS = 1.7                                                       383
      AG_TITLE = 'G10'                                                    383
      AG_EXNAME = 'MIXTURE'                                               383
      AG_PARLIST = 'DENS'                                                 383
      CALL AXMIXTURE                                                      383
C *                                                                       383
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             383
      CALL AGSRESET                                                       384
      AG_A = 28.08                                                        384
      AG_Z = 14                                                           384
      AG_W = 0.6*1*28./60.                                                384
      AG_TITLE = 'SI'                                                     384
      AG_EXNAME = 'COMPONENT'                                             384
      AG_PARLIST = 'A_Z_W'                                                384
      CALL AXCOMPONENT                                                    384
C *                                                                       384
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  384
C =                                                                       384
      CALL AGSRESET                                                       385
      AG_A = 16                                                           385
      AG_Z = 8                                                            385
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               385
      AG_TITLE = 'O'                                                      385
      AG_EXNAME = 'COMPONENT'                                             385
      AG_PARLIST = 'A_Z_W'                                                385
      CALL AXCOMPONENT                                                    385
C *                                                                       385
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 385
      CALL AGSRESET                                                       386
      AG_A = 12                                                           386
      AG_Z = 6                                                            386
      AG_W = 0.4*8*12./174.                                               386
      AG_TITLE = 'C'                                                      386
      AG_EXNAME = 'COMPONENT'                                             386
      AG_PARLIST = 'A_Z_W'                                                386
      CALL AXCOMPONENT                                                    386
C *                                                                       386
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  386
      CALL AGSRESET                                                       387
      AG_A = 1                                                            387
      AG_Z = 1                                                            387
      AG_W = 0.4*14*1./174.                                               387
      AG_TITLE = 'H'                                                      387
      AG_EXNAME = 'COMPONENT'                                             387
      AG_PARLIST = 'A_Z_W'                                                387
      CALL AXCOMPONENT                                                    387
C *                                                                       387
C Geant MIXTURE : G5 ===> DENS=0.85 <===                                  387
      CALL AGSRESET                                                       388
      AG_DENS = 0.85                                                      388
      AG_TITLE = 'G5'                                                     388
      AG_EXNAME = 'MIXTURE'                                               388
      AG_PARLIST = 'DENS'                                                 388
      CALL AXMIXTURE                                                      388
C *                                                                       388
C Geant COMPONENT : O ===> A=16 Z=8 W=2*16./100. <===                     388
      CALL AGSRESET                                                       389
      AG_A = 16                                                           389
      AG_Z = 8                                                            389
      AG_W = 2*16./100.                                                   389
      AG_TITLE = 'O'                                                      389
      AG_EXNAME = 'COMPONENT'                                             389
      AG_PARLIST = 'A_Z_W'                                                389
      CALL AXCOMPONENT                                                    389
C *                                                                       389
C Geant COMPONENT : C ===> A=12 Z=6 W=5*12./100. <===                     389
      CALL AGSRESET                                                       390
      AG_A = 12                                                           390
      AG_Z = 6                                                            390
      AG_W = 5*12./100.                                                   390
      AG_TITLE = 'C'                                                      390
      AG_EXNAME = 'COMPONENT'                                             390
      AG_PARLIST = 'A_Z_W'                                                390
      CALL AXCOMPONENT                                                    390
C *                                                                       390
C Geant COMPONENT : H ===> A=1 Z=1 W=8*1.0/100. <===                      390
      CALL AGSRESET                                                       391
      AG_A = 1                                                            391
      AG_Z = 1                                                            391
      AG_W = 8*1.0/100.                                                   391
      AG_TITLE = 'H'                                                      391
      AG_EXNAME = 'COMPONENT'                                             391
      AG_PARLIST = 'A_Z_W'                                                391
      CALL AXCOMPONENT                                                    391
C *                                                                       391
C Geant MIXTURE : DELRIN ===> DENS=1.2 <===                               391
      CALL AGSRESET                                                       392
      AG_DENS = 1.2                                                       392
      AG_TITLE = 'DELRIN'                                                 392
      AG_EXNAME = 'MIXTURE'                                               392
      AG_PARLIST = 'DENS'                                                 392
      CALL AXMIXTURE                                                      392
C *                                                                       392
      CALL AGSBEGM(MODULE,IPRIN)                                          392
C Check IPRIN<0                                                           393
      IF (IPRIN.LT.0) RETURN                                              393
C Create SFMO :                                                           393
C Call To Local Procedure SFMO                                            393
      ASSIGN 5020 TO BLOCK_SFMO                                           393
      GO TO  5010                                                         393
5020  CONTINUE                                                            393
C *                                                                       393
C *                                                                       394
C    Check SSDP_PLACEMENT==1                                              394
         IF (SSDP_PLACEMENT.EQ.1) THEN                                    394
C    Geant POSITION : SFMO ===> MOTHER='IDSM' KONLY='ONLY' <===           395
         CALL AGSRESET                                                    395
         AG_MOTHER = 'IDSM'                                               395
         AG_KONLY = 'ONLY'                                                395
         AG_TITLE = 'SFMO'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'MOTHER_KONLY'                                      395
         CALL AXPOSITION                                                  395
C    *                                                                    395
      END IF                                                              396
C *                                                                       397
C    Check SSDP_PLACEMENT==0                                              397
         IF (SSDP_PLACEMENT.EQ.0) THEN                                    397
C    Geant POSITION : SFMO ===> MOTHER='CAVE' KONLY='MANY' <===           398
         CALL AGSRESET                                                    398
         AG_MOTHER = 'CAVE'                                               398
         AG_KONLY = 'MANY'                                                398
         AG_TITLE = 'SFMO'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'MOTHER_KONLY'                                      398
         CALL AXPOSITION                                                  398
C    *                                                                    398
      END IF                                                              399
C ----------------------------------------------------------------------  400
C ------------ SFMO                                                       400
      CALL AGSENDM                                                        401
      RETURN                                                              401
C Begin Local Procedure SFMO                                              401
C SFMO IS THE MOTHER OF ALL SILICON STRIP DETECTOR VOLUMES                401
5010  CONTINUE                                                            401
         AG_TITLE='SFMO'                                                  401
         AG_MODULE=MODULE                                                 401
         AG_MARK='V00'                                                    401
         CALL AGSPUSH                                                     401
         AG_MODULE=MODULE                                                 401
C    *                                                                    401
C    Geant MATERIAL : AIR ===> inherited arguments used <===              402
         CALL AGSRESET                                                    402
         AG_TITLE = 'AIR'                                                 402
         AG_EXNAME = 'MATERIAL'                                           402
         AG_PARLIST = 'NONE'                                              402
         CALL AXMATERIAL                                                  402
C    *                                                                    402
C    Geant ATTRIBUTE : SFMO ===> SEEN=1 COLO=6 <===                       402
         CALL AGSRESET                                                    403
         AG_SEEN = 1                                                      403
         AG_COLO = 6                                                      403
         AG_TITLE = 'SFMO'                                                403
         AG_EXNAME = 'ATTRIBUTE'                                          403
         AG_PARLIST = 'SEEN_COLO'                                         403
         CALL AXATTRIBUTE                                                 403
C    *                                                                    403
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  403
C    2 <===                                                               403
         CALL AGSRESET                                                    405
         AG_RMIN = SFPA_RMIN                                              405
         AG_RMAX = SFPA_RMAX                                              405
         AG_DZ = SFPA_LEN/2                                               405
         AG_TITLE = 'TUBE'                                                405
         AG_EXNAME = 'SHAPE'                                              405
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      405
         CALL AXSHAPE                                                     405
C    *                                                                    405
         DTHK = SFPA_SMTHK + SFPA_GPTHK                                   405
C    Check CONFIGB.GE.7                                                   407
         IF (CONFIGB.GE.7) THEN                                           407
C    Create SOSK :                                                        408
C    Call To Local Procedure SOSK                                         408
         ASSIGN 5040 TO BLOCK_SOSK                                        408
         GO TO  5030                                                      408
5040     CONTINUE                                                         408
C    *                                                                    408
         ENDIF                                                            409
C    Create SFLM :                                                        410
C    Call To Local Procedure SFLM                                         410
         ASSIGN 5060 TO BLOCK_SFLM                                        410
         GO TO  5050                                                      410
5060     CONTINUE                                                         410
C    *                                                                    410
C    *                                                                    411
         ILAD=1                                                           411
            GO TO 5073                                                    411
5071        ILAD=ILAD+1                                                   411
5073        IF(ILAD-(20).GT.0) GO TO 5072                                 411
C       Check SFPA_LADDERMAP(ILAD).GT.0                                   412
            IF (SFPA_LADDERMAP(ILAD).GT.0) THEN                           412
            ANG = (SFPA_LADDERANGLE(ILAD)*PI)/180.0                       413
C       Check ILAD.EQ.1                                                   414
            IF (ILAD.EQ.1) THEN                                           414
            NC=1                                                          415
            ELSE                                                          416
            NC=20-ILAD+2                                                  417
            ENDIF                                                         418
C       Geant POSITION : SFLM ===> X=0.0 Y=0.0 Z=0.0 ALPHAZ=SFPA_LADDERA  419
C       NGLE(ILAD)-90 KONLY='MANY' <===                                   419
            CALL AGSRESET                                                 419
            AG_X = 0.0                                                    419
            AG_Y = 0.0                                                    419
            AG_Z = 0.0                                                    419
            AG_ALPHAN=AG_ALPHAN+1                                         419
            AG_ALPHAK(AG_ALPHAN)=3                                        419
            AG_ALPHAL(AG_ALPHAN)=SFPA_LADDERANGLE(ILAD)-90                419
            AG_KONLY = 'MANY'                                             419
            AG_TITLE = 'SFLM'                                             419
            AG_EXNAME = 'POSITION'                                        419
            AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                             419
            CALL AXPOSITION                                               419
C       *                                                                 419
            ENDIF                                                         419
         GO TO 5071                                                       421
5072     CONTINUE                                                         421
C    Create SSST :                                                        422
C    Call To Local Procedure SSST                                         422
         ASSIGN 5090 TO BLOCK_SSST                                        422
         GO TO  5080                                                      422
5090     CONTINUE                                                         422
C    *                                                                    422
C    Create SSSS :                                                        423
C    Call To Local Procedure SSSS                                         423
         ASSIGN 5110 TO BLOCK_SSSS                                        423
         GO TO  5100                                                      423
5110     CONTINUE                                                         423
C    *                                                                    423
C    Create SSRT :                                                        424
C    Call To Local Procedure SSRT                                         424
         ASSIGN 5130 TO BLOCK_SSRT                                        424
         GO TO  5120                                                      424
5130     CONTINUE                                                         424
C    *                                                                    424
C    Create SSRS :                                                        425
C    Call To Local Procedure SSRS                                         425
         ASSIGN 5150 TO BLOCK_SSRS                                        425
         GO TO  5140                                                      425
5150     CONTINUE                                                         425
C    *                                                                    425
C    Create SSLB :                                                        426
C    Call To Local Procedure SSLB                                         426
         ASSIGN 5170 TO BLOCK_SSLB                                        426
         GO TO  5160                                                      426
5170     CONTINUE                                                         426
C    *                                                                    426
C    Create SSLT :                                                        427
C    Call To Local Procedure SSLT                                         427
         ASSIGN 5190 TO BLOCK_SSLT                                        427
         GO TO  5180                                                      427
5190     CONTINUE                                                         427
C    *                                                                    427
C    Create SCMP :                                                        428
C    Call To Local Procedure SCMP                                         428
         ASSIGN 5210 TO BLOCK_SCMP                                        428
         GO TO  5200                                                      428
5210     CONTINUE                                                         428
C    *                                                                    428
C    Create SCVM :                                                        429
C    Call To Local Procedure SCVM                                         429
         ASSIGN 5230 TO BLOCK_SCVM                                        429
         GO TO  5220                                                      429
5230     CONTINUE                                                         429
C    *                                                                    429
5011  CALL AGSPOPB                                                        430
      GO TO BLOCK_SFMO, (5010 ,5020)                                      430
C End of Local Procedure                                                  430
C ----------------------------------------------------------------------  431
C ------------ SFLM                                                       431
C Begin Local Procedure SFLM                                              432
C SFLM IS THE MOTHER OF THE LADDER                                        432
5050  CONTINUE                                                            432
         AG_TITLE='SFLM'                                                  432
         AG_MODULE=MODULE                                                 432
         AG_MARK='V00'                                                    432
         CALL AGSPUSH                                                     432
         AG_MODULE=MODULE                                                 432
C    *                                                                    432
C    Geant MATERIAL : AIR ===> inherited arguments used <===              433
         CALL AGSRESET                                                    433
         AG_TITLE = 'AIR'                                                 433
         AG_EXNAME = 'MATERIAL'                                           433
         AG_PARLIST = 'NONE'                                              433
         CALL AXMATERIAL                                                  433
C    *                                                                    433
C    Geant ATTRIBUTE : SFLM ===> SEEN=1 COLO=7 <===                       433
         CALL AGSRESET                                                    434
         AG_SEEN = 1                                                      434
         AG_COLO = 7                                                      434
         AG_TITLE = 'SFLM'                                                434
         AG_EXNAME = 'ATTRIBUTE'                                          434
         AG_PARLIST = 'SEEN_COLO'                                         434
         CALL AXATTRIBUTE                                                 434
C    *                                                                    434
C    Geant SHAPE : TUBS ===> RMIN=21.6 RMAX=SFPA_RMAX-0.2, PHI1=-11+90 P  434
C    HI2=11+90 DZ=SFPA_SMLEN/2 <===                                       434
         CALL AGSRESET                                                    436
         AG_RMIN = 21.6                                                   436
         AG_RMAX = SFPA_RMAX-0.2                                          436
         AG_PHI1 = -11+90                                                 436
         AG_PHI2 = 11+90                                                  436
         AG_DZ = SFPA_SMLEN/2                                             436
         AG_TITLE = 'TUBS'                                                436
         AG_EXNAME = 'SHAPE'                                              436
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            436
         CALL AXSHAPE                                                     436
C    *                                                                    436
         DOFF = DTHK/2.0 - (SFPA_SMTHK + SFPA_GPTHK)/2                    436
         RADTILT = - (SFPA_LADDERTILT(1)*PI)/180.0                        438
         XC = DOFF*SIN(RADTILT)                                           439
         YC = SFPA_LADDERRADIUS(1) + DOFF*COS(RADTILT)                    440
         TILT = SFPA_LADDERTILT(1)                                        441
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              442
         X = 0                                                            443
         Y = 0                                                            444
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              445
C    *                                                                    446
         IWAF=1                                                           446
            GO TO 5243                                                    446
5241        IWAF=IWAF+1                                                   446
5243        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 5242                          446
C       Create SFSW :                                                     447
C       Call To Local Procedure SFSW                                      447
            ASSIGN 5260 TO BLOCK_SFSW                                     447
            GO TO  5250                                                   447
5260        CONTINUE                                                      447
C       *                                                                 447
C       Geant POSITION : SFSW ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPCKLE  449
C       N)/2+IWAF*WAFPCKLEN ALPHAZ=TILT KONLY='MANY' <===                 449
            CALL AGSRESET                                                 449
            AG_X = XPOS                                                   449
            AG_Y = YPOS                                                   449
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               449
            AG_ALPHAN=AG_ALPHAN+1                                         449
            AG_ALPHAK(AG_ALPHAN)=3                                        449
            AG_ALPHAL(AG_ALPHAN)=TILT                                     449
            AG_KONLY = 'MANY'                                             449
            AG_TITLE = 'SFSW'                                             449
            AG_EXNAME = 'POSITION'                                        449
            AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                             449
            CALL AXPOSITION                                               449
C       *                                                                 449
         GO TO 5241                                                       450
5242     CONTINUE                                                         450
         DOFF = DTHK/2.0 + (SFPA_DMTHK + SFPA_GPTHK)/2                    451
         XC = DOFF*SIN(RADTILT)                                           452
         YC = SFPA_LADDERRADIUS(1) + DOFF*COS(RADTILT)                    453
         YOFFSET = - 1.7                                                  454
C    Create SFLT :                                                        455
C    Call To Local Procedure SFLT                                         455
         ASSIGN 5280 TO BLOCK_SFLT                                        455
         GO TO  5270                                                      455
5280     CONTINUE                                                         455
C    *                                                                    455
         HIGHT = SFJP_SFSM_LL*TAN(54*PI/180) - SFPB_HHIGHT/TAN(PI/5) -    456
     *   0.02                                                             456
         X = - (SFPB_HHIGHT - SFPB_KHIGHT)                                457
         Y = HIGHT + YOFFSET                                              458
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              459
C    Geant POSITION : SFLT ===> X=XPOS Y=YPOS Z=0.0 ALPHAZ=-90-36+TILT K  460
C    ONLY='ONLY' <===                                                     460
         CALL AGSRESET                                                    460
         AG_X = XPOS                                                      460
         AG_Y = YPOS                                                      460
         AG_Z = 0.0                                                       460
         AG_ALPHAN=AG_ALPHAN+1                                            460
         AG_ALPHAK(AG_ALPHAN)=3                                           460
         AG_ALPHAL(AG_ALPHAN)=-90-36+TILT                                 460
         AG_KONLY = 'ONLY'                                                460
         AG_TITLE = 'SFLT'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                460
         CALL AXPOSITION                                                  460
C    *                                                                    460
         X = + (SFPB_HHIGHT - SFPB_KHIGHT)                                460
         Y = HIGHT + YOFFSET                                              462
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              463
C    Geant POSITION : SFLT ===> X=XPOS Y=YPOS Z=0 ALPHAY=180. ALPHAZ=90.  464
C    +36.+TILT KONLY='ONLY' <===                                          464
         CALL AGSRESET                                                    464
         AG_X = XPOS                                                      464
         AG_Y = YPOS                                                      464
         AG_Z = 0                                                         464
         AG_ALPHAN=AG_ALPHAN+1                                            464
         AG_ALPHAK(AG_ALPHAN)=2                                           464
         AG_ALPHAL(AG_ALPHAN)=180.                                        464
         AG_ALPHAN=AG_ALPHAN+1                                            464
         AG_ALPHAK(AG_ALPHAN)=3                                           464
         AG_ALPHAL(AG_ALPHAN)=90.+36.+TILT                                464
         AG_KONLY = 'ONLY'                                                464
         AG_TITLE = 'SFLT'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         464
         CALL AXPOSITION                                                  464
C    *                                                                    464
         X = 0. - (SFJP_SFSM_LL - (SFJP_SFLU_BL1 + SFJP_SFLU_H1/2*1/      464
     *   TAN(PI/5)))                                                      464
         Y = YOFFSET                                                      466
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              467
C    Create SFLU :                                                        468
C    Call To Local Procedure SFLU                                         468
         ASSIGN 5300 TO BLOCK_SFLU                                        468
         GO TO  5290                                                      468
5300     CONTINUE                                                         468
C    *                                                                    468
C    Geant POSITION : SFLU ===> X=XPOS Y=YPOS Z=0. ALPHAZ=TILT KONLY='ON  469
C    *LY' <===                                                            469
         CALL AGSRESET                                                    469
         AG_X = XPOS                                                      469
         AG_Y = YPOS                                                      469
         AG_Z = 0.                                                        469
         AG_ALPHAN=AG_ALPHAN+1                                            469
         AG_ALPHAK(AG_ALPHAN)=3                                           469
         AG_ALPHAL(AG_ALPHAN)=TILT                                        469
         AG_KONLY = 'ONLY'                                                469
         AG_TITLE = 'SFLU'                                                469
         AG_EXNAME = 'POSITION'                                           469
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                469
         CALL AXPOSITION                                                  469
C    *                                                                    469
         ESSAI = 180                                                      469
         ESSAI = ESSAI + 27 + 27                                          471
         Y = COS(27*PI/180)*2*(SFPB_HBASE - SFPB_KBASE) + YOFFSET         472
         X = - COS(63*PI/180)*2*(SFPB_HBASE - SFPB_KBASE) -               473
     *   (SFJP_SFSM_LL - (0.2 + 0.01/TAN(PI/5)))                          473
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              474
C    Geant POSITION : SFLU ===> X=XPOS Y=YPOS Z=0. ALPHAY=180 ALPHAZ=ESS  475
C    AI+TILT KONLY='ONLY' <===                                            475
         CALL AGSRESET                                                    475
         AG_X = XPOS                                                      475
         AG_Y = YPOS                                                      475
         AG_Z = 0.                                                        475
         AG_ALPHAN=AG_ALPHAN+1                                            475
         AG_ALPHAK(AG_ALPHAN)=2                                           475
         AG_ALPHAL(AG_ALPHAN)=180                                         475
         AG_ALPHAN=AG_ALPHAN+1                                            475
         AG_ALPHAK(AG_ALPHAN)=3                                           475
         AG_ALPHAL(AG_ALPHAN)=ESSAI+TILT                                  475
         AG_KONLY = 'ONLY'                                                475
         AG_TITLE = 'SFLU'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         475
         CALL AXPOSITION                                                  475
C    *                                                                    475
         X = 0. + (SFJP_SFSM_LL - (0.2 + 0.01/TAN(PI/5)))                 475
         Y = 0. + YOFFSET                                                 477
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              478
C    Geant POSITION : SFLU ===> X=XPOS Y=YPOS Z=0. ALPHAY=180. ALPHAZ=TI  479
C    LT KONLY='ONLY' <===                                                 479
         CALL AGSRESET                                                    479
         AG_X = XPOS                                                      479
         AG_Y = YPOS                                                      479
         AG_Z = 0.                                                        479
         AG_ALPHAN=AG_ALPHAN+1                                            479
         AG_ALPHAK(AG_ALPHAN)=2                                           479
         AG_ALPHAL(AG_ALPHAN)=180.                                        479
         AG_ALPHAN=AG_ALPHAN+1                                            479
         AG_ALPHAK(AG_ALPHAN)=3                                           479
         AG_ALPHAL(AG_ALPHAN)=TILT                                        479
         AG_KONLY = 'ONLY'                                                479
         AG_TITLE = 'SFLU'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         479
         CALL AXPOSITION                                                  479
C    *                                                                    479
         X = COS(63*PI/180)*2*(SFPB_HBASE - SFPB_KBASE) + (SFJP_SFSM_LL   479
     *   - (0.2 + 0.01/TAN(PI/5)))                                        479
         Y = COS(27*PI/180)*2*(SFPB_HBASE - SFPB_KBASE) + YOFFSET         481
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              482
C    Geant POSITION : SFLU ===> X=XPOS Y=YPOS Z=0. ALPHAZ=-ESSAI+TILT KO  483
C    NLY='ONLY' <===                                                      483
         CALL AGSRESET                                                    483
         AG_X = XPOS                                                      483
         AG_Y = YPOS                                                      483
         AG_Z = 0.                                                        483
         AG_ALPHAN=AG_ALPHAN+1                                            483
         AG_ALPHAK(AG_ALPHAN)=3                                           483
         AG_ALPHAL(AG_ALPHAN)=-ESSAI+TILT                                 483
         AG_KONLY = 'ONLY'                                                483
         AG_TITLE = 'SFLU'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                483
         CALL AXPOSITION                                                  483
C    *                                                                    483
         X = SFJP_SFFK_PX/2 + SFJP_SFFK_DXE                               483
         Y = - SFJP_SFFK_PY1 - SFJP_SFFK_PY2 - SFJP_SFFK_DY + YOFFSET     485
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              486
C    Create SFFK :                                                        487
C    Call To Local Procedure SFFK                                         487
         ASSIGN 5320 TO BLOCK_SFFK                                        487
         GO TO  5310                                                      487
5320     CONTINUE                                                         487
C    *                                                                    487
C    Geant POSITION : SFFK ===> X=XPOS Y=YPOS Z=0. ALPHAZ=TILT KONLY='ON  488
C    *LY' <===                                                            488
         CALL AGSRESET                                                    488
         AG_X = XPOS                                                      488
         AG_Y = YPOS                                                      488
         AG_Z = 0.                                                        488
         AG_ALPHAN=AG_ALPHAN+1                                            488
         AG_ALPHAK(AG_ALPHAN)=3                                           488
         AG_ALPHAL(AG_ALPHAN)=TILT                                        488
         AG_KONLY = 'ONLY'                                                488
         AG_TITLE = 'SFFK'                                                488
         AG_EXNAME = 'POSITION'                                           488
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                488
         CALL AXPOSITION                                                  488
C    *                                                                    488
         X = - (SFJP_SFFK_PX/2 + SFJP_SFFK_DXE)                           488
         Y = - SFJP_SFFK_PY1 - SFJP_SFFK_PY2 - SFJP_SFFK_DY + YOFFSET     490
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              491
C    Geant POSITION : SFFK ===> X=XPOS Y=YPOS Z=0. ALPHAZ=TILT KONLY='ON  492
C    *LY' <===                                                            492
         CALL AGSRESET                                                    492
         AG_X = XPOS                                                      492
         AG_Y = YPOS                                                      492
         AG_Z = 0.                                                        492
         AG_ALPHAN=AG_ALPHAN+1                                            492
         AG_ALPHAK(AG_ALPHAN)=3                                           492
         AG_ALPHAL(AG_ALPHAN)=TILT                                        492
         AG_KONLY = 'ONLY'                                                492
         AG_TITLE = 'SFFK'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                492
         CALL AXPOSITION                                                  492
C    *                                                                    492
         X = - SFJP_SFKL_PX - SFPB_FSIZE*COS(15*PI/180)                   492
         Y = - SFJP_SFFK_PY1 - SFJP_SFFK_PY2 - SFJP_SFFK_DY -             495
     *   SFPB_FSIZE*COS(75*PI/180) + YOFFSET                              495
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              496
C    Create SFFL :                                                        497
C    Call To Local Procedure SFFL                                         497
         ASSIGN 5340 TO BLOCK_SFFL                                        497
         GO TO  5330                                                      497
5340     CONTINUE                                                         497
C    *                                                                    497
C    Geant POSITION : SFFL ===> X=XPOS Y=YPOS Z=0. ALPHAZ=15+TILT KONLY=  498
C    'ONLY' <===                                                          498
         CALL AGSRESET                                                    498
         AG_X = XPOS                                                      498
         AG_Y = YPOS                                                      498
         AG_Z = 0.                                                        498
         AG_ALPHAN=AG_ALPHAN+1                                            498
         AG_ALPHAK(AG_ALPHAN)=3                                           498
         AG_ALPHAL(AG_ALPHAN)=15+TILT                                     498
         AG_KONLY = 'ONLY'                                                498
         AG_TITLE = 'SFFL'                                                498
         AG_EXNAME = 'POSITION'                                           498
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                498
         CALL AXPOSITION                                                  498
C    *                                                                    498
         X = SFJP_SFKL_PX + SFPB_FSIZE*COS(15*PI/180)                     498
         Y = - SFJP_SFFK_PY1 - SFJP_SFFK_PY2 - SFJP_SFFK_DY -             501
     *   SFPB_FSIZE*COS(75*PI/180) + YOFFSET                              501
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              502
C    Geant POSITION : SFFL ===> X=XPOS Y=YPOS Z=0. ALPHAZ=-15+TILT KONLY  503
C    ='ONLY' <===                                                         503
         CALL AGSRESET                                                    503
         AG_X = XPOS                                                      503
         AG_Y = YPOS                                                      503
         AG_Z = 0.                                                        503
         AG_ALPHAN=AG_ALPHAN+1                                            503
         AG_ALPHAK(AG_ALPHAN)=3                                           503
         AG_ALPHAL(AG_ALPHAN)=-15+TILT                                    503
         AG_KONLY = 'ONLY'                                                503
         AG_TITLE = 'SFFL'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                503
         CALL AXPOSITION                                                  503
C    *                                                                    503
         X = 0.0                                                          503
         Y = - SFJP_SFFK_PY1 - SFJP_SFFK_PY2 - SFJP_SFFK_PY2 + YOFFSET    505
     *   - SFJP_SFKK_DY/2.                                                505
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              506
C    Create SFKK :                                                        507
C    Call To Local Procedure SFKK                                         507
         ASSIGN 5360 TO BLOCK_SFKK                                        507
         GO TO  5350                                                      507
5360     CONTINUE                                                         507
C    *                                                                    507
C    Geant POSITION : SFKK ===> X=XPOS Y=YPOS Z=0. ALPHAZ=TILT KONLY='ON  508
C    *LY' <===                                                            508
         CALL AGSRESET                                                    508
         AG_X = XPOS                                                      508
         AG_Y = YPOS                                                      508
         AG_Z = 0.                                                        508
         AG_ALPHAN=AG_ALPHAN+1                                            508
         AG_ALPHAK(AG_ALPHAN)=3                                           508
         AG_ALPHAL(AG_ALPHAN)=TILT                                        508
         AG_KONLY = 'ONLY'                                                508
         AG_TITLE = 'SFKK'                                                508
         AG_EXNAME = 'POSITION'                                           508
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                508
         CALL AXPOSITION                                                  508
C    *                                                                    508
C    Create SFKL :                                                        509
C    Call To Local Procedure SFKL                                         509
         ASSIGN 5380 TO BLOCK_SFKL                                        509
         GO TO  5370                                                      509
5380     CONTINUE                                                         509
C    *                                                                    509
         X = - SFJP_SFKL_PX - SFPB_FSIZE*COS(15*PI/180) + 0.005           510
         Y = - SFJP_SFFK_PY1 - SFJP_SFFK_PY2 - SFJP_SFFK_DY -             512
     *   SFPB_FSIZE*COS(75*PI/180) + YOFFSET - 0.021                      512
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              513
C    Geant POSITION : SFKL ===> X=XPOS Y=YPOS Z=0. ALPHAZ=15+TILT KONLY=  514
C    'ONLY' <===                                                          514
         CALL AGSRESET                                                    514
         AG_X = XPOS                                                      514
         AG_Y = YPOS                                                      514
         AG_Z = 0.                                                        514
         AG_ALPHAN=AG_ALPHAN+1                                            514
         AG_ALPHAK(AG_ALPHAN)=3                                           514
         AG_ALPHAL(AG_ALPHAN)=15+TILT                                     514
         AG_KONLY = 'ONLY'                                                514
         AG_TITLE = 'SFKL'                                                514
         AG_EXNAME = 'POSITION'                                           514
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                514
         CALL AXPOSITION                                                  514
C    *                                                                    514
         X = + SFJP_SFKL_PX + SFPB_FSIZE*COS(15*PI/180) - 0.005           514
         Y = - SFJP_SFFK_PY1 - SFJP_SFFK_PY2 - SFJP_SFFK_DY -             517
     *   SFPB_FSIZE*COS(75*PI/180) + YOFFSET - 0.021                      517
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              518
C    Geant POSITION : SFKL ===> X=XPOS Y=YPOS Z=0. ALPHAZ=-15+TILT KONLY  519
C    ='ONLY' <===                                                         519
         CALL AGSRESET                                                    519
         AG_X = XPOS                                                      519
         AG_Y = YPOS                                                      519
         AG_Z = 0.                                                        519
         AG_ALPHAN=AG_ALPHAN+1                                            519
         AG_ALPHAK(AG_ALPHAN)=3                                           519
         AG_ALPHAL(AG_ALPHAN)=-15+TILT                                    519
         AG_KONLY = 'ONLY'                                                519
         AG_TITLE = 'SFKL'                                                519
         AG_EXNAME = 'POSITION'                                           519
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                519
         CALL AXPOSITION                                                  519
C    *                                                                    519
         X = 0.0                                                          519
         Y = - SFJP_SAPP_PY1 - (SFJP_SAPP_PY2 + SFJP_SAPP_PY3)/2. +       521
     *   YOFFSET                                                          521
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              522
C    Create SAPP :                                                        523
C    Call To Local Procedure SAPP                                         523
         ASSIGN 5400 TO BLOCK_SAPP                                        523
         GO TO  5390                                                      523
5400     CONTINUE                                                         523
C    *                                                                    523
C    Geant POSITION : SAPP ===> X=XPOS Y=YPOS Z=SFJP_SFFK_DZ/2+SFJP_SAPP  525
C    _DZ/2. ALPHAY=180. ALPHAZ=TILT KONLY='ONLY' <===                     525
         CALL AGSRESET                                                    525
         AG_X = XPOS                                                      525
         AG_Y = YPOS                                                      525
         AG_Z = SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.                            525
         AG_ALPHAN=AG_ALPHAN+1                                            525
         AG_ALPHAK(AG_ALPHAN)=2                                           525
         AG_ALPHAL(AG_ALPHAN)=180.                                        525
         AG_ALPHAN=AG_ALPHAN+1                                            525
         AG_ALPHAK(AG_ALPHAN)=3                                           525
         AG_ALPHAL(AG_ALPHAN)=TILT                                        525
         AG_KONLY = 'ONLY'                                                525
         AG_TITLE = 'SAPP'                                                525
         AG_EXNAME = 'POSITION'                                           525
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         525
         CALL AXPOSITION                                                  525
C    *                                                                    525
C    Geant POSITION : SAPP ===> X=XPOS Y=YPOS Z=-(SFJP_SFFK_DZ/2+SFJP_SA  525
C    PP_DZ/2.) ALPHAZ=TILT KONLY='ONLY' <===                              525
         CALL AGSRESET                                                    527
         AG_X = XPOS                                                      527
         AG_Y = YPOS                                                      527
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.)                         527
         AG_ALPHAN=AG_ALPHAN+1                                            527
         AG_ALPHAK(AG_ALPHAN)=3                                           527
         AG_ALPHAL(AG_ALPHAN)=TILT                                        527
         AG_KONLY = 'ONLY'                                                527
         AG_TITLE = 'SAPP'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                527
         CALL AXPOSITION                                                  527
C    *                                                                    527
         X = 0.0                                                          527
         Y = - SFJP_SAPP_PY1 - (SFJP_SAPP_PY2 + SFJP_SAPP_PY3)/2. +       529
     *   YOFFSET                                                          529
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              530
C    Create SFAM :                                                        531
C    Call To Local Procedure SFAM                                         531
         ASSIGN 5420 TO BLOCK_SFAM                                        531
         GO TO  5410                                                      531
5420     CONTINUE                                                         531
C    *                                                                    531
C    Geant POSITION : SFAM ===> X=XPOS Y=YPOS Z=-(SFJP_SFFK_DZ/2+SFJP_SA  533
C    PP_DZ)-SFJP_SFAM_DZ/2. ALPHAZ=TILT KONLY='MANY' <===                 533
         CALL AGSRESET                                                    533
         AG_X = XPOS                                                      533
         AG_Y = YPOS                                                      533
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_DZ/2.            533
         AG_ALPHAN=AG_ALPHAN+1                                            533
         AG_ALPHAK(AG_ALPHAN)=3                                           533
         AG_ALPHAL(AG_ALPHAN)=TILT                                        533
         AG_KONLY = 'MANY'                                                533
         AG_TITLE = 'SFAM'                                                533
         AG_EXNAME = 'POSITION'                                           533
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                533
         CALL AXPOSITION                                                  533
C    *                                                                    533
C    Geant POSITION : SFAM ===> X=XPOS Y=YPOS Z=+(SFJP_SFFK_DZ/2+SFJP_SA  533
C    PP_DZ)+SFJP_SFAM_DZ/2. ALPHAY=180. ALPHAZ=TILT KONLY='MANY' <===     533
         CALL AGSRESET                                                    535
         AG_X = XPOS                                                      535
         AG_Y = YPOS                                                      535
         AG_Z = +(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_DZ/2.            535
         AG_ALPHAN=AG_ALPHAN+1                                            535
         AG_ALPHAK(AG_ALPHAN)=2                                           535
         AG_ALPHAL(AG_ALPHAN)=180.                                        535
         AG_ALPHAN=AG_ALPHAN+1                                            535
         AG_ALPHAK(AG_ALPHAN)=3                                           535
         AG_ALPHAL(AG_ALPHAN)=TILT                                        535
         AG_KONLY = 'MANY'                                                535
         AG_TITLE = 'SFAM'                                                535
         AG_EXNAME = 'POSITION'                                           535
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         535
         CALL AXPOSITION                                                  535
C    *                                                                    535
         YOFFSET = 0.0                                                    535
         X = - SFJP_SFCO_PX                                               537
         Y = SFJP_SFCO_PY + YOFFSET                                       538
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              539
C    Create SFCO :                                                        540
C    Call To Local Procedure SFCO                                         540
         ASSIGN 5440 TO BLOCK_SFCO                                        540
         GO TO  5430                                                      540
5440     CONTINUE                                                         540
C    *                                                                    540
C    Geant POSITION : SFCO ===> X=XPOS Y=YPOS Z=-(SFJP_SFFK_DZ/2-(SFJP_S  543
C    FKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=+SFJP_ALPHAZ+TILT <===  543
         CALL AGSRESET                                                    543
         AG_X = XPOS                                                      543
         AG_Y = YPOS                                                      543
         AG_Z = -(SFJP_SFFK_DZ/                                           543
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   543
         AG_ALPHAN=AG_ALPHAN+1                                            543
         AG_ALPHAK(AG_ALPHAN)=3                                           543
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                           543
         AG_TITLE = 'SFCO'                                                543
         AG_EXNAME = 'POSITION'                                           543
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      543
         CALL AXPOSITION                                                  543
C    *                                                                    543
         X = SFJP_SFCO_PX                                                 543
         Y = SFJP_SFCO_PY + YOFFSET                                       545
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              546
C    Geant POSITION : SFCO ===> X=XPOS Y=YPOS Z=+(SFJP_SFFK_DZ/2-(SFJP_S  549
C    FKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAZ=-SFJP_ALPHAZ+TILT <===  549
         CALL AGSRESET                                                    549
         AG_X = XPOS                                                      549
         AG_Y = YPOS                                                      549
         AG_Z = +(SFJP_SFFK_DZ/                                           549
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   549
         AG_ALPHAN=AG_ALPHAN+1                                            549
         AG_ALPHAK(AG_ALPHAN)=3                                           549
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                           549
         AG_TITLE = 'SFCO'                                                549
         AG_EXNAME = 'POSITION'                                           549
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      549
         CALL AXPOSITION                                                  549
C    *                                                                    549
         X = + SFJP_SFCO_PX                                               549
         Y = SFJP_SFCO_PY + YOFFSET                                       551
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              552
C    Create SFCM :                                                        553
C    Call To Local Procedure SFCM                                         553
         ASSIGN 5460 TO BLOCK_SFCM                                        553
         GO TO  5450                                                      553
5460     CONTINUE                                                         553
C    *                                                                    553
C    Geant POSITION : SFCM ===> X=XPOS Y=YPOS Z=-(SFJP_SFFK_DZ/2-(SFJP_S  556
C    FKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=-SFJP_ALPHAZ+TILT KONL  556
C    Y='MANY' <===                                                        556
         CALL AGSRESET                                                    556
         AG_X = XPOS                                                      556
         AG_Y = YPOS                                                      556
         AG_Z = -(SFJP_SFFK_DZ/                                           556
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   556
         AG_ALPHAN=AG_ALPHAN+1                                            556
         AG_ALPHAK(AG_ALPHAN)=3                                           556
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                           556
         AG_KONLY = 'MANY'                                                556
         AG_TITLE = 'SFCM'                                                556
         AG_EXNAME = 'POSITION'                                           556
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                556
         CALL AXPOSITION                                                  556
C    *                                                                    556
         X = - SFJP_SFCO_PX                                               556
         Y = SFJP_SFCO_PY + YOFFSET                                       558
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              559
C    Geant POSITION : SFCM ===> X=XPOS Y=YPOS Z=+(SFJP_SFFK_DZ/2-(SFJP_S  562
C    FKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAY=180 ALPHAZ=+SFJP_ALPHA  562
C    Z+TILT KONLY='MANY' <===                                             562
         CALL AGSRESET                                                    562
         AG_X = XPOS                                                      562
         AG_Y = YPOS                                                      562
         AG_Z = +(SFJP_SFFK_DZ/                                           562
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   562
         AG_ALPHAN=AG_ALPHAN+1                                            562
         AG_ALPHAK(AG_ALPHAN)=2                                           562
         AG_ALPHAL(AG_ALPHAN)=180                                         562
         AG_ALPHAN=AG_ALPHAN+1                                            562
         AG_ALPHAK(AG_ALPHAN)=3                                           562
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                           562
         AG_KONLY = 'MANY'                                                562
         AG_TITLE = 'SFCM'                                                562
         AG_EXNAME = 'POSITION'                                           562
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         562
         CALL AXPOSITION                                                  562
C    *                                                                    562
C    Create SFKF :                                                        563
C    Call To Local Procedure SFKF                                         563
         ASSIGN 5480 TO BLOCK_SFKF                                        563
         GO TO  5470                                                      563
5480     CONTINUE                                                         563
C    *                                                                    563
         X = - SFJP_SFKF_PX                                               564
         Y = SFJP_SFKF_PY + YOFFSET                                       565
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              566
C    Geant POSITION : SFKF ===> X=XPOS Y=YPOS Z=-(SFJP_SFFK_DZ/2-(SFJP_S  569
C    FKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2.-0.2 ALPHAZ=+SFJP_ALPHAZ+TILT   569
C    <===                                                                 569
         CALL AGSRESET                                                    569
         AG_X = XPOS                                                      569
         AG_Y = YPOS                                                      569
         AG_Z = -(SFJP_SFFK_DZ/                                           569
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2.-0.2               569
         AG_ALPHAN=AG_ALPHAN+1                                            569
         AG_ALPHAK(AG_ALPHAN)=3                                           569
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                           569
         AG_TITLE = 'SFKF'                                                569
         AG_EXNAME = 'POSITION'                                           569
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      569
         CALL AXPOSITION                                                  569
C    *                                                                    569
         X = SFJP_SFKF_PX                                                 569
         Y = SFJP_SFKF_PY + YOFFSET                                       571
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              572
C    Geant POSITION : SFKF ===> X=XPOS Y=YPOS Z=+(SFJP_SFFK_DZ/2-(SFJP_S  575
C    FKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2.+0.2 ALPHAZ=-SFJP_ALPHAZ+TILT   575
C    <===                                                                 575
         CALL AGSRESET                                                    575
         AG_X = XPOS                                                      575
         AG_Y = YPOS                                                      575
         AG_Z = +(SFJP_SFFK_DZ/                                           575
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2.+0.2               575
         AG_ALPHAN=AG_ALPHAN+1                                            575
         AG_ALPHAK(AG_ALPHAN)=3                                           575
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                           575
         AG_TITLE = 'SFKF'                                                575
         AG_EXNAME = 'POSITION'                                           575
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      575
         CALL AXPOSITION                                                  575
C    *                                                                    575
C    Create SFKS :                                                        576
C    Call To Local Procedure SFKS                                         576
         ASSIGN 5500 TO BLOCK_SFKS                                        576
         GO TO  5490                                                      576
5500     CONTINUE                                                         576
C    *                                                                    576
         X = + SFJP_SFKS_PX                                               577
         Y = SFJP_SFKS_PY + YOFFSET                                       578
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              579
C    Geant POSITION : SFKS ===> X=XPOS Y=YPOS Z=-(SFJP_SFFK_DZ/2-(SFJP_S  582
C    FKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=-SFJP_ALPHAZ+TILT <  582
C    ===                                                                  582
         CALL AGSRESET                                                    582
         AG_X = XPOS                                                      582
         AG_Y = YPOS                                                      582
         AG_Z = -(SFJP_SFFK_DZ/                                           582
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                582
         AG_ALPHAN=AG_ALPHAN+1                                            582
         AG_ALPHAK(AG_ALPHAN)=3                                           582
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                           582
         AG_TITLE = 'SFKS'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      582
         CALL AXPOSITION                                                  582
C    *                                                                    582
         X = - SFJP_SFKS_PX                                               582
         Y = SFJP_SFKS_PY + YOFFSET                                       584
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              585
C    Geant POSITION : SFKS ===> X=XPOS Y=YPOS Z=+(SFJP_SFFK_DZ/2-(SFJP_S  588
C    FKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=+SFJP_ALPHAZ+TILT <  588
C    ===                                                                  588
         CALL AGSRESET                                                    588
         AG_X = XPOS                                                      588
         AG_Y = YPOS                                                      588
         AG_Z = +(SFJP_SFFK_DZ/                                           588
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                588
         AG_ALPHAN=AG_ALPHAN+1                                            588
         AG_ALPHAK(AG_ALPHAN)=3                                           588
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                           588
         AG_TITLE = 'SFKS'                                                588
         AG_EXNAME = 'POSITION'                                           588
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      588
         CALL AXPOSITION                                                  588
C    *                                                                    588
C    Create SFPR :                                                        589
C    Call To Local Procedure SFPR                                         589
         ASSIGN 5520 TO BLOCK_SFPR                                        589
         GO TO  5510                                                      589
5520     CONTINUE                                                         589
C    *                                                                    589
         X = 0.0                                                          590
         Y = SFJP_SFPR_PY + YOFFSET                                       591
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              592
C    Geant POSITION : SFPR ===> X=XPOS Y=YPOS Z=-SFJP_SFLU_DZ ALPHAZ=TIL  593
C    T <===                                                               593
         CALL AGSRESET                                                    593
         AG_X = XPOS                                                      593
         AG_Y = YPOS                                                      593
         AG_Z = -SFJP_SFLU_DZ                                             593
         AG_ALPHAN=AG_ALPHAN+1                                            593
         AG_ALPHAK(AG_ALPHAN)=3                                           593
         AG_ALPHAL(AG_ALPHAN)=TILT                                        593
         AG_TITLE = 'SFPR'                                                593
         AG_EXNAME = 'POSITION'                                           593
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      593
         CALL AXPOSITION                                                  593
C    *                                                                    593
C    Geant POSITION : SFPR ===> X=XPOS Y=YPOS Z=SFJP_SFLU_DZ-3.7 ALPHAZ=  593
C    TILT <===                                                            593
         CALL AGSRESET                                                    594
         AG_X = XPOS                                                      594
         AG_Y = YPOS                                                      594
         AG_Z = SFJP_SFLU_DZ-3.7                                          594
         AG_ALPHAN=AG_ALPHAN+1                                            594
         AG_ALPHAK(AG_ALPHAN)=3                                           594
         AG_ALPHAL(AG_ALPHAN)=TILT                                        594
         AG_TITLE = 'SFPR'                                                594
         AG_EXNAME = 'POSITION'                                           594
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      594
         CALL AXPOSITION                                                  594
C    *                                                                    594
C    Create SFPB :                                                        595
C    Call To Local Procedure SFPB                                         595
         ASSIGN 5540 TO BLOCK_SFPB                                        595
         GO TO  5530                                                      595
5540     CONTINUE                                                         595
C    *                                                                    595
         X = 0.0                                                          596
         Y = + SFJP_SFPR_PY - SFJP_SFPB_PY + SFJP_SFPB_PY2 + YOFFSET      597
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              598
C    Geant POSITION : SFPB ===> X=XPOS Y=YPOS Z=-SFJP_SFLU_DZ-SFJP_SFPBD  599
C    Z/2. ALPHAZ=TILT <===                                                599
         CALL AGSRESET                                                    599
         AG_X = XPOS                                                      599
         AG_Y = YPOS                                                      599
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ/2.                              599
         AG_ALPHAN=AG_ALPHAN+1                                            599
         AG_ALPHAK(AG_ALPHAN)=3                                           599
         AG_ALPHAL(AG_ALPHAN)=TILT                                        599
         AG_TITLE = 'SFPB'                                                599
         AG_EXNAME = 'POSITION'                                           599
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      599
         CALL AXPOSITION                                                  599
C    *                                                                    599
C    Geant POSITION : SFPB ===> X=XPOS Y=YPOS Z=+SFJP_SFLU_DZ+SFJP_SFPBD  599
C    Z/2. ALPHAZ=TILT <===                                                599
         CALL AGSRESET                                                    600
         AG_X = XPOS                                                      600
         AG_Y = YPOS                                                      600
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ/2.                              600
         AG_ALPHAN=AG_ALPHAN+1                                            600
         AG_ALPHAK(AG_ALPHAN)=3                                           600
         AG_ALPHAL(AG_ALPHAN)=TILT                                        600
         AG_TITLE = 'SFPB'                                                600
         AG_EXNAME = 'POSITION'                                           600
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      600
         CALL AXPOSITION                                                  600
C    *                                                                    600
C    Create SSBS :                                                        601
C    Call To Local Procedure SSBS                                         601
         ASSIGN 5560 TO BLOCK_SSBS                                        601
         GO TO  5550                                                      601
5560     CONTINUE                                                         601
C    *                                                                    601
         X = 0.0                                                          602
         Y = + SFJP_SFPR_PY - SFJP_SFPB_PY + SFJP_SFPB_PY2 - 0.8/2. -     603
     *   SFJP_SSBS_DY/2.                                                  603
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              604
C    Geant POSITION : SSBS ===> X=XPOS Y=YPOS Z=-SFJP_SFLU_DZ-SFJP_SFPBD  606
C    Z-0.2+SFJP_SSBB_DZ+SFJP_SSBS_DX/2. ALPHAZ=TILT <===                  606
         CALL AGSRESET                                                    606
         AG_X = XPOS                                                      606
         AG_Y = YPOS                                                      606
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ+SFJP_SSBS_DX/  606
     *   2.                                                               606
         AG_ALPHAN=AG_ALPHAN+1                                            606
         AG_ALPHAK(AG_ALPHAN)=3                                           606
         AG_ALPHAL(AG_ALPHAN)=TILT                                        606
         AG_TITLE = 'SSBS'                                                606
         AG_EXNAME = 'POSITION'                                           606
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      606
         CALL AXPOSITION                                                  606
C    *                                                                    606
         X = 0                                                            606
         Y = + SFJP_SFPR_PY - SFJP_SFPB_PY + SFJP_SFPB_PY2 - 0.8/2. -     608
     *   SFJP_SSBS_DY/2.                                                  608
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              609
C    Geant POSITION : SSBS ===> X=XPOS Y=YPOS Z=+SFJP_SFLU_DZ+SFJP_SFPBD  611
C    Z+0.2-SFJP_SSBB_DZ-SFJP_SSBS_DX/2. ALPHAZ=TILT <===                  611
         CALL AGSRESET                                                    611
         AG_X = XPOS                                                      611
         AG_Y = YPOS                                                      611
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ-SFJP_SSBS_DX/  611
     *   2.                                                               611
         AG_ALPHAN=AG_ALPHAN+1                                            611
         AG_ALPHAK(AG_ALPHAN)=3                                           611
         AG_ALPHAL(AG_ALPHAN)=TILT                                        611
         AG_TITLE = 'SSBS'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Create SSBB :                                                        612
C    Call To Local Procedure SSBB                                         612
         ASSIGN 5580 TO BLOCK_SSBB                                        612
         GO TO  5570                                                      612
5580     CONTINUE                                                         612
C    *                                                                    612
         X = 0.0                                                          613
         Y = + SFJP_SFPR_PY - SFJP_SFPB_PY + SFJP_SFPB_PY2 - 0.8/2. -     614
     *   SFJP_SSBS_DY/2.                                                  614
         CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )              615
C    Geant POSITION : SSBB ===> X=XPOS Y=YPOS Z=-SFJP_SFLU_DZ-SFJP_SFPBD  617
C    Z-0.2+SFJP_SSBB_DZ/2. ALPHAZ=TILT <===                               617
         CALL AGSRESET                                                    617
         AG_X = XPOS                                                      617
         AG_Y = YPOS                                                      617
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ/2.             617
         AG_ALPHAN=AG_ALPHAN+1                                            617
         AG_ALPHAK(AG_ALPHAN)=3                                           617
         AG_ALPHAL(AG_ALPHAN)=TILT                                        617
         AG_TITLE = 'SSBB'                                                617
         AG_EXNAME = 'POSITION'                                           617
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      617
         CALL AXPOSITION                                                  617
C    *                                                                    617
C    Geant POSITION : SSBB ===> X=XPOS Y=YPOS Z=+SFJP_SFLU_DZ+SFJP_SFPBD  617
C    Z+0.2-SFJP_SSBB_DZ/2. ALPHAZ=TILT <===                               617
         CALL AGSRESET                                                    619
         AG_X = XPOS                                                      619
         AG_Y = YPOS                                                      619
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ/2.             619
         AG_ALPHAN=AG_ALPHAN+1                                            619
         AG_ALPHAK(AG_ALPHAN)=3                                           619
         AG_ALPHAL(AG_ALPHAN)=TILT                                        619
         AG_TITLE = 'SSBB'                                                619
         AG_EXNAME = 'POSITION'                                           619
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      619
         CALL AXPOSITION                                                  619
C    *                                                                    619
C    Create SFLA :                                                        620
C    Call To Local Procedure SFLA                                         620
         ASSIGN 5600 TO BLOCK_SFLA                                        620
         GO TO  5590                                                      620
5600     CONTINUE                                                         620
C    *                                                                    620
C    Create SFLB :                                                        621
C    Call To Local Procedure SFLB                                         621
         ASSIGN 5620 TO BLOCK_SFLB                                        621
         GO TO  5610                                                      621
5620     CONTINUE                                                         621
C    *                                                                    621
C    Create SFLC :                                                        622
C    Call To Local Procedure SFLC                                         622
         ASSIGN 5640 TO BLOCK_SFLC                                        622
         GO TO  5630                                                      622
5640     CONTINUE                                                         622
C    *                                                                    622
C    Create SFEB :                                                        623
C    Call To Local Procedure SFEB                                         623
         ASSIGN 5660 TO BLOCK_SFEB                                        623
         GO TO  5650                                                      623
5660     CONTINUE                                                         623
C    *                                                                    623
C    Create SFES :                                                        624
C    Call To Local Procedure SFES                                         624
         ASSIGN 5680 TO BLOCK_SFES                                        624
         GO TO  5670                                                      624
5680     CONTINUE                                                         624
C    *                                                                    624
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              625
C    *                                                                    626
         IWAF=1                                                           626
            GO TO 5693                                                    626
5691        IWAF=IWAF+1                                                   626
5693        IF(IWAF-(8).GT.0) GO TO 5692                                  626
C       *                                                                 627
            JWAF=1                                                        627
               GO TO 5703                                                 627
5701           JWAF=JWAF+1                                                627
5703           IF(JWAF-(IWAF).GT.0) GO TO 5702                            627
               X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                        628
               Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF + YOFFSET               629
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        630
C          Geant POSITION : SFLA ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPC  632
C          KLEN)/2+IWAF*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ+TILT <===           632
               CALL AGSRESET                                              632
               AG_X = XPOS                                                632
               AG_Y = YPOS                                                632
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN            632
               AG_ALPHAN=AG_ALPHAN+1                                      632
               AG_ALPHAK(AG_ALPHAN)=3                                     632
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                     632
               AG_TITLE = 'SFLA'                                          632
               AG_EXNAME = 'POSITION'                                     632
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                632
               CALL AXPOSITION                                            632
C          *                                                              632
               X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                        632
               Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF + YOFFSET               634
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        635
C          Geant POSITION : SFLA ===> X=XPOS Y=YPOS Z=+(SFPA_WPLEN+WAFPC  637
C          KLEN)/2-IWAF*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ+TILT <===           637
               CALL AGSRESET                                              637
               AG_X = XPOS                                                637
               AG_Y = YPOS                                                637
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-IWAF*WAFPCKLEN            637
               AG_ALPHAN=AG_ALPHAN+1                                      637
               AG_ALPHAK(AG_ALPHAN)=3                                     637
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                     637
               AG_TITLE = 'SFLA'                                          637
               AG_EXNAME = 'POSITION'                                     637
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                637
               CALL AXPOSITION                                            637
C          *                                                              637
               X = SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                         637
               Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF + YOFFSET               639
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        640
C          Geant POSITION : SFLC ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPC  643
C          KLEN)/2.+16*WAFPCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC  643
C          _DZ-IWAF*SFJP_SFLC_DZ KONLY='ONLY' ALPHAZ=-SFJP_ALPHAZ+TILT <  643
C          ===                                                            643
               CALL AGSRESET                                              643
               AG_X = XPOS                                                643
               AG_Y = YPOS                                                643
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2.+16*WAFPCKLEN+WAFPCKLEN/  643
     *         2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ         643
               AG_KONLY = 'ONLY'                                          643
               AG_ALPHAN=AG_ALPHAN+1                                      643
               AG_ALPHAK(AG_ALPHAN)=3                                     643
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                     643
               AG_TITLE = 'SFLC'                                          643
               AG_EXNAME = 'POSITION'                                     643
               AG_PARLIST = 'X_Y_Z_KONLY_ALPHAZ'                          643
               CALL AXPOSITION                                            643
C          *                                                              643
               X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                        643
               Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF+ YOFFSET                645
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        646
C          Geant POSITION : SFLC ===> X=XPOS Y=YPOS Z=+(SFPA_WPLEN+WAFPC  649
C          KLEN)/2.-16*WAFPCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC  649
C          _DZ+IWAF*SFJP_SFLC_DZ ALPHAZ=+SFJP_ALPHAZ+TILT <===            649
               CALL AGSRESET                                              649
               AG_X = XPOS                                                649
               AG_Y = YPOS                                                649
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2.-16*WAFPCKLEN-WAFPCKLEN/  649
     *         2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ         649
               AG_ALPHAN=AG_ALPHAN+1                                      649
               AG_ALPHAK(AG_ALPHAN)=3                                     649
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                     649
               AG_TITLE = 'SFLC'                                          649
               AG_EXNAME = 'POSITION'                                     649
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                649
               CALL AXPOSITION                                            649
C          *                                                              649
               X = SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                         649
               Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF + YOFFSET              651
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        652
C          Geant POSITION : SFLA ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPC  654
C          KLEN)/2+(IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ+TILT <===       654
               CALL AGSRESET                                              654
               AG_X = XPOS                                                654
               AG_Y = YPOS                                                654
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        654
               AG_ALPHAN=AG_ALPHAN+1                                      654
               AG_ALPHAK(AG_ALPHAN)=3                                     654
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                     654
               AG_TITLE = 'SFLA'                                          654
               AG_EXNAME = 'POSITION'                                     654
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                654
               CALL AXPOSITION                                            654
C          *                                                              654
               X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                        654
               Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF + YOFFSET              656
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        657
C          Geant POSITION : SFLA ===> X=XPOS Y=YPOS Z=+(SFPA_WPLEN+WAFPC  659
C          KLEN)/2-(IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ+TILT <===       659
               CALL AGSRESET                                              659
               AG_X = XPOS                                                659
               AG_Y = YPOS                                                659
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        659
               AG_ALPHAN=AG_ALPHAN+1                                      659
               AG_ALPHAK(AG_ALPHAN)=3                                     659
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                     659
               AG_TITLE = 'SFLA'                                          659
               AG_EXNAME = 'POSITION'                                     659
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                659
               CALL AXPOSITION                                            659
C          *                                                              659
               X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                        659
               Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF + YOFFSET              661
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        662
C          Geant POSITION : SFLC ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPC  665
C          KLEN)/2+16*WAFPCKLEN+WAFPCKLEN/2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_  665
C          SFLC_DZ ALPHAZ=-SFJP_ALPHAZ+TILT <===                          665
               CALL AGSRESET                                              665
               AG_X = XPOS                                                665
               AG_Y = YPOS                                                665
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/   665
     *         2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ                      665
               AG_ALPHAN=AG_ALPHAN+1                                      665
               AG_ALPHAK(AG_ALPHAN)=3                                     665
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                     665
               AG_TITLE = 'SFLC'                                          665
               AG_EXNAME = 'POSITION'                                     665
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                665
               CALL AXPOSITION                                            665
C          *                                                              665
               X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                        665
               Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF + YOFFSET              667
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        668
C          Geant POSITION : SFLC ===> X=XPOS Y=YPOS Z=+(SFPA_WPLEN+WAFPC  671
C          KLEN)/2-16*WAFPCKLEN-WAFPCKLEN/2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_  671
C          SFLC_DZ ALPHAZ=+SFJP_ALPHAZ+TILT <===                          671
               CALL AGSRESET                                              671
               AG_X = XPOS                                                671
               AG_Y = YPOS                                                671
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/   671
     *         2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ                      671
               AG_ALPHAN=AG_ALPHAN+1                                      671
               AG_ALPHAK(AG_ALPHAN)=3                                     671
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                     671
               AG_TITLE = 'SFLC'                                          671
               AG_EXNAME = 'POSITION'                                     671
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                671
               CALL AXPOSITION                                            671
C          *                                                              671
            GO TO 5701                                                    672
5702        CONTINUE                                                      672
C       *                                                                 673
            JWAF=1                                                        673
               GO TO 5713                                                 673
5711           JWAF=JWAF+1                                                673
5713           IF(JWAF-(8).GT.0) GO TO 5712                               673
               X = SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                         674
               Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF + YOFFSET               675
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        676
C          Geant POSITION : SFLA ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPC  678
C          KLEN)/2+(IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ+TILT <===       678
               CALL AGSRESET                                              678
               AG_X = XPOS                                                678
               AG_Y = YPOS                                                678
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        678
               AG_ALPHAN=AG_ALPHAN+1                                      678
               AG_ALPHAK(AG_ALPHAN)=3                                     678
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                     678
               AG_TITLE = 'SFLA'                                          678
               AG_EXNAME = 'POSITION'                                     678
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                678
               CALL AXPOSITION                                            678
C          *                                                              678
               X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                        678
               Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF + YOFFSET               680
               CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )        681
C          Geant POSITION : SFLA ===> X=XPOS Y=YPOS Z=+(SFPA_WPLEN+WAFPC  683
C          KLEN)/2-(IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ+TILT <===       683
               CALL AGSRESET                                              683
               AG_X = XPOS                                                683
               AG_Y = YPOS                                                683
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        683
               AG_ALPHAN=AG_ALPHAN+1                                      683
               AG_ALPHAK(AG_ALPHAN)=3                                     683
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                     683
               AG_TITLE = 'SFLA'                                          683
               AG_EXNAME = 'POSITION'                                     683
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                683
               CALL AXPOSITION                                            683
C          *                                                              683
            GO TO 5711                                                    684
5712        CONTINUE                                                      684
            X = +SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF                           685
            Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF +YOFFSET                   686
            CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )           687
C       Geant POSITION : SFLB ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPCKLE  690
C       N)/2+16*WAFPCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ/2. ALPHAZ=-SFJP_ALPH  690
C       AZ+TILT <===                                                      690
            CALL AGSRESET                                                 690
            AG_X = XPOS                                                   690
            AG_Y = YPOS                                                   690
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/      690
     *      2.+SFJP_SFLB_DZ/2.                                            690
            AG_ALPHAN=AG_ALPHAN+1                                         690
            AG_ALPHAK(AG_ALPHAN)=3                                        690
            AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ+TILT                        690
            AG_TITLE = 'SFLB'                                             690
            AG_EXNAME = 'POSITION'                                        690
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   690
            CALL AXPOSITION                                               690
C       *                                                                 690
            X = -SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF                           690
            Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF +YOFFSET                   692
            CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )           693
C       Geant POSITION : SFLB ===> X=XPOS Y=YPOS Z=+(SFPA_WPLEN+WAFPCKLE  696
C       N)/2-16*WAFPCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ/2. ALPHAZ=+SFJP_ALPH  696
C       AZ+TILT <===                                                      696
            CALL AGSRESET                                                 696
            AG_X = XPOS                                                   696
            AG_Y = YPOS                                                   696
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/      696
     *      2.-SFJP_SFLB_DZ/2.                                            696
            AG_ALPHAN=AG_ALPHAN+1                                         696
            AG_ALPHAK(AG_ALPHAN)=3                                        696
            AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ+TILT                        696
            AG_TITLE = 'SFLB'                                             696
            AG_EXNAME = 'POSITION'                                        696
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   696
            CALL AXPOSITION                                               696
C       *                                                                 696
            X = -SFJP_SFEB_PX                                             696
            Y = -SFJP_SFEB_PY                                             698
            CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )           699
C       Geant POSITION : SFEB ===> X=XPOS Y=YPOS Z=-(SFPA_WPLEN+WAFPCKLE  702
C       N)/2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_DZ/2.-SFJP_SFES_P  702
C       Z/2. ALPHAZ=55.35+TILT KONLY='ONLY' <===                          702
            CALL AGSRESET                                                 702
            AG_X = XPOS                                                   702
            AG_Y = YPOS                                                   702
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/                               702
     *      2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_DZ/               702
     *      2.-SFJP_SFES_PZ/2.                                            702
            AG_ALPHAN=AG_ALPHAN+1                                         702
            AG_ALPHAK(AG_ALPHAN)=3                                        702
            AG_ALPHAL(AG_ALPHAN)=55.35+TILT                               702
            AG_KONLY = 'ONLY'                                             702
            AG_TITLE = 'SFEB'                                             702
            AG_EXNAME = 'POSITION'                                        702
            AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                             702
            CALL AXPOSITION                                               702
C       *                                                                 702
            X = +SFJP_SFEB_PX                                             702
            Y = -SFJP_SFEB_PY                                             704
            CALL TRANS_TO_SFM( X, Y, XC, YC, TILT, XPOS, YPOS )           705
C       Geant POSITION : SFEB ===> X=XPOS Y=YPOS Z=+(SFPA_WPLEN+WAFPCKLE  708
C       N)/2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_DZ/2.+SFJP_SFES_P  708
C       Z/2. ALPHAZ=-55.35+TILT <===                                      708
            CALL AGSRESET                                                 708
            AG_X = XPOS                                                   708
            AG_Y = YPOS                                                   708
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/                               708
     *      2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_DZ/               708
     *      2.+SFJP_SFES_PZ/2.                                            708
            AG_ALPHAN=AG_ALPHAN+1                                         708
            AG_ALPHAK(AG_ALPHAN)=3                                        708
            AG_ALPHAL(AG_ALPHAN)=-55.35+TILT                              708
            AG_TITLE = 'SFEB'                                             708
            AG_EXNAME = 'POSITION'                                        708
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   708
            CALL AXPOSITION                                               708
C       *                                                                 708
         GO TO 5691                                                       709
5692     CONTINUE                                                         709
5051  CALL AGSPOPB                                                        710
      GO TO BLOCK_SFLM, (5050 ,5060)                                      710
C End of Local Procedure                                                  710
C ----------------------------------------------------------------------  711
C ------------ SFSW                                                       711
C Begin Local Procedure SFSW                                              712
C SFSW IS A SINGLE WAFER CONTAINER                                        712
5250  CONTINUE                                                            712
         AG_TITLE='SFSW'                                                  712
         AG_MODULE=MODULE                                                 712
         AG_MARK='V00'                                                    712
         CALL AGSPUSH                                                     712
         AG_MODULE=MODULE                                                 712
C    *                                                                    712
C    Geant MATERIAL : AIR ===> inherited arguments used <===              713
         CALL AGSRESET                                                    713
         AG_TITLE = 'AIR'                                                 713
         AG_EXNAME = 'MATERIAL'                                           713
         AG_PARLIST = 'NONE'                                              713
         CALL AXMATERIAL                                                  713
C    *                                                                    713
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=6 <===                       713
         CALL AGSRESET                                                    714
         AG_SEEN = 0                                                      714
         AG_COLO = 6                                                      714
         AG_TITLE = 'SFSW'                                                714
         AG_EXNAME = 'ATTRIBUTE'                                          714
         AG_PARLIST = 'SEEN_COLO'                                         714
         CALL AXATTRIBUTE                                                 714
C    *                                                                    714
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFJP_SFSW_DY, DZ=SFJP_SFS  714
C    W_DZ <===                                                            714
         CALL AGSRESET                                                    716
         AG_DX = SFPA_DMWID/2                                             716
         AG_DY = SFJP_SFSW_DY                                             716
         AG_DZ = SFJP_SFSW_DZ                                             716
         AG_TITLE = 'BOX'                                                 716
         AG_EXNAME = 'SHAPE'                                              716
         AG_PARLIST = 'DX_DY_DZ'                                          716
         CALL AXSHAPE                                                     716
C    *                                                                    716
C    Create SFSL :                                                        717
C    Call To Local Procedure SFSL                                         717
         ASSIGN 5730 TO BLOCK_SFSL                                        717
         GO TO  5720                                                      717
5730     CONTINUE                                                         717
C    *                                                                    717
C    Geant POSITION : SFSL ===> KONLY='ONLY' <===                         718
         CALL AGSRESET                                                    718
         AG_KONLY = 'ONLY'                                                718
         AG_TITLE = 'SFSL'                                                718
         AG_EXNAME = 'POSITION'                                           718
         AG_PARLIST = 'KONLY'                                             718
         CALL AXPOSITION                                                  718
C    *                                                                    718
C    Create SFRA :                                                        719
C    Call To Local Procedure SFRA                                         719
         ASSIGN 5750 TO BLOCK_SFRA                                        719
         GO TO  5740                                                      719
5750     CONTINUE                                                         719
C    *                                                                    719
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=SFJP_SFRA_PZ KONL  720
C    Y='ONLY' <===                                                        720
         CALL AGSRESET                                                    720
         AG_X = 0.0                                                       720
         AG_Y = SFJP_SFRA_PY                                              720
         AG_Z = SFJP_SFRA_PZ                                              720
         AG_KONLY = 'ONLY'                                                720
         AG_TITLE = 'SFRA'                                                720
         AG_EXNAME = 'POSITION'                                           720
         AG_PARLIST = 'X_Y_Z_KONLY'                                       720
         CALL AXPOSITION                                                  720
C    *                                                                    720
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=-SFJP_SFRA_PZ KON  720
C    LY='ONLY' <===                                                       720
         CALL AGSRESET                                                    721
         AG_X = 0.0                                                       721
         AG_Y = SFJP_SFRA_PY                                              721
         AG_Z = -SFJP_SFRA_PZ                                             721
         AG_KONLY = 'ONLY'                                                721
         AG_TITLE = 'SFRA'                                                721
         AG_EXNAME = 'POSITION'                                           721
         AG_PARLIST = 'X_Y_Z_KONLY'                                       721
         CALL AXPOSITION                                                  721
C    *                                                                    721
C    Create SFRS :                                                        722
C    Call To Local Procedure SFRS                                         722
         ASSIGN 5770 TO BLOCK_SFRS                                        722
         GO TO  5760                                                      722
5770     CONTINUE                                                         722
C    *                                                                    722
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SFR  723
C    A_PZ KONLY='ONLY' <===                                               723
         CALL AGSRESET                                                    723
         AG_X = SFJP_SFRS_PX                                              723
         AG_Y = SFJP_SFRS_PY                                              723
         AG_Z = SFJP_SFRA_PZ                                              723
         AG_KONLY = 'ONLY'                                                723
         AG_TITLE = 'SFRS'                                                723
         AG_EXNAME = 'POSITION'                                           723
         AG_PARLIST = 'X_Y_Z_KONLY'                                       723
         CALL AXPOSITION                                                  723
C    *                                                                    723
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_SF  723
C    RA_PZ KONLY='ONLY' <===                                              723
         CALL AGSRESET                                                    724
         AG_X = SFJP_SFRS_PX                                              724
         AG_Y = SFJP_SFRS_PY                                              724
         AG_Z = -SFJP_SFRA_PZ                                             724
         AG_KONLY = 'ONLY'                                                724
         AG_TITLE = 'SFRS'                                                724
         AG_EXNAME = 'POSITION'                                           724
         AG_PARLIST = 'X_Y_Z_KONLY'                                       724
         CALL AXPOSITION                                                  724
C    *                                                                    724
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SF  724
C    RA_PZ KONLY='ONLY' <===                                              724
         CALL AGSRESET                                                    725
         AG_X = -SFJP_SFRS_PX                                             725
         AG_Y = SFJP_SFRS_PY                                              725
         AG_Z = SFJP_SFRA_PZ                                              725
         AG_KONLY = 'ONLY'                                                725
         AG_TITLE = 'SFRS'                                                725
         AG_EXNAME = 'POSITION'                                           725
         AG_PARLIST = 'X_Y_Z_KONLY'                                       725
         CALL AXPOSITION                                                  725
C    *                                                                    725
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_S  725
C    FRA_PZ KONLY='ONLY' <===                                             725
         CALL AGSRESET                                                    726
         AG_X = -SFJP_SFRS_PX                                             726
         AG_Y = SFJP_SFRS_PY                                              726
         AG_Z = -SFJP_SFRA_PZ                                             726
         AG_KONLY = 'ONLY'                                                726
         AG_TITLE = 'SFRS'                                                726
         AG_EXNAME = 'POSITION'                                           726
         AG_PARLIST = 'X_Y_Z_KONLY'                                       726
         CALL AXPOSITION                                                  726
C    *                                                                    726
C    Create SFFX :                                                        727
C    Call To Local Procedure SFFX                                         727
         ASSIGN 5790 TO BLOCK_SFFX                                        727
         GO TO  5780                                                      727
5790     CONTINUE                                                         727
C    *                                                                    727
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  729
C    FFX_DYZ Z=SFJP_SFRA_PZ KONLY='ONLY' <===                             729
         CALL AGSRESET                                                    729
         AG_X = 0.0                                                       729
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   729
         AG_Z = SFJP_SFRA_PZ                                              729
         AG_KONLY = 'ONLY'                                                729
         AG_TITLE = 'SFFX'                                                729
         AG_EXNAME = 'POSITION'                                           729
         AG_PARLIST = 'X_Y_Z_KONLY'                                       729
         CALL AXPOSITION                                                  729
C    *                                                                    729
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  729
C    FFX_DYZ Z=-SFJP_SFRA_PZ KONLY='ONLY' <===                            729
         CALL AGSRESET                                                    731
         AG_X = 0.0                                                       731
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   731
         AG_Z = -SFJP_SFRA_PZ                                             731
         AG_KONLY = 'ONLY'                                                731
         AG_TITLE = 'SFFX'                                                731
         AG_EXNAME = 'POSITION'                                           731
         AG_PARLIST = 'X_Y_Z_KONLY'                                       731
         CALL AXPOSITION                                                  731
C    *                                                                    731
C    Create SFPI :                                                        732
C    Call To Local Procedure SFPI                                         732
         ASSIGN 5810 TO BLOCK_SFPI                                        732
         GO TO  5800                                                      732
5810     CONTINUE                                                         732
C    *                                                                    732
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  735
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 KONLY='ONLY  735
C    *' <===                                                              735
         CALL AGSRESET                                                    735
         AG_X = SFJP_SFPI_PX                                              735
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     735
         AG_Z = SFJP_SFPI_PZ                                              735
         AG_ALPHAN=AG_ALPHAN+1                                            735
         AG_ALPHAK(AG_ALPHAN)=1                                           735
         AG_ALPHAL(AG_ALPHAN)=90                                          735
         AG_KONLY = 'ONLY'                                                735
         AG_TITLE = 'SFPI'                                                735
         AG_EXNAME = 'POSITION'                                           735
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                735
         CALL AXPOSITION                                                  735
C    *                                                                    735
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  735
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 KONLY='ONL  735
C    *Y' <===                                                             735
         CALL AGSRESET                                                    738
         AG_X = -SFJP_SFPI_PX                                             738
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     738
         AG_Z = SFJP_SFPI_PZ                                              738
         AG_ALPHAN=AG_ALPHAN+1                                            738
         AG_ALPHAK(AG_ALPHAN)=1                                           738
         AG_ALPHAL(AG_ALPHAN)=90                                          738
         AG_KONLY = 'ONLY'                                                738
         AG_TITLE = 'SFPI'                                                738
         AG_EXNAME = 'POSITION'                                           738
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                738
         CALL AXPOSITION                                                  738
C    *                                                                    738
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  738
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 KONLY='ONL  738
C    *Y' <===                                                             738
         CALL AGSRESET                                                    741
         AG_X = SFJP_SFPI_PX                                              741
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     741
         AG_Z = -SFJP_SFPI_PZ                                             741
         AG_ALPHAN=AG_ALPHAN+1                                            741
         AG_ALPHAK(AG_ALPHAN)=1                                           741
         AG_ALPHAL(AG_ALPHAN)=90                                          741
         AG_KONLY = 'ONLY'                                                741
         AG_TITLE = 'SFPI'                                                741
         AG_EXNAME = 'POSITION'                                           741
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                741
         CALL AXPOSITION                                                  741
C    *                                                                    741
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  741
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 KONLY='ON  741
C    *LY' <===                                                            741
         CALL AGSRESET                                                    744
         AG_X = -SFJP_SFPI_PX                                             744
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     744
         AG_Z = -SFJP_SFPI_PZ                                             744
         AG_ALPHAN=AG_ALPHAN+1                                            744
         AG_ALPHAK(AG_ALPHAN)=1                                           744
         AG_ALPHAL(AG_ALPHAN)=90                                          744
         AG_KONLY = 'ONLY'                                                744
         AG_TITLE = 'SFPI'                                                744
         AG_EXNAME = 'POSITION'                                           744
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                744
         CALL AXPOSITION                                                  744
C    *                                                                    744
C    Create SFAA :                                                        745
C    Call To Local Procedure SFAA                                         745
         ASSIGN 5830 TO BLOCK_SFAA                                        745
         GO TO  5820                                                      745
5830     CONTINUE                                                         745
C    *                                                                    745
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  748
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  748
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ KONLY='ONLY' <===               748
         CALL AGSRESET                                                    748
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               748
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    748
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    748
         AG_KONLY = 'ONLY'                                                748
         AG_TITLE = 'SFAA'                                                748
         AG_EXNAME = 'POSITION'                                           748
         AG_PARLIST = 'X_Y_Z_KONLY'                                       748
         CALL AXPOSITION                                                  748
C    *                                                                    748
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  748
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  748
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ KO  748
C    NLY='ONLY' <===                                                      748
         CALL AGSRESET                                                    751
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   751
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    751
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    751
         AG_KONLY = 'ONLY'                                                751
         AG_TITLE = 'SFAA'                                                751
         AG_EXNAME = 'POSITION'                                           751
         AG_PARLIST = 'X_Y_Z_KONLY'                                       751
         CALL AXPOSITION                                                  751
C    *                                                                    751
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  751
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  751
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  751
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ KONLY='ONLY' <===                          751
         CALL AGSRESET                                                    755
         AG_X =                                                           755
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         755
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      755
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    755
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    755
         AG_KONLY = 'ONLY'                                                755
         AG_TITLE = 'SFAA'                                                755
         AG_EXNAME = 'POSITION'                                           755
         AG_PARLIST = 'X_Y_Z_KONLY'                                       755
         CALL AXPOSITION                                                  755
C    *                                                                    755
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  755
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  755
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ KONLY='ONLY' <===               755
         CALL AGSRESET                                                    758
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               758
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    758
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    758
         AG_KONLY = 'ONLY'                                                758
         AG_TITLE = 'SFAA'                                                758
         AG_EXNAME = 'POSITION'                                           758
         AG_PARLIST = 'X_Y_Z_KONLY'                                       758
         CALL AXPOSITION                                                  758
C    *                                                                    758
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  758
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  758
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ KO  758
C    NLY='ONLY' <===                                                      758
         CALL AGSRESET                                                    761
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   761
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    761
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    761
         AG_KONLY = 'ONLY'                                                761
         AG_TITLE = 'SFAA'                                                761
         AG_EXNAME = 'POSITION'                                           761
         AG_PARLIST = 'X_Y_Z_KONLY'                                       761
         CALL AXPOSITION                                                  761
C    *                                                                    761
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  761
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  761
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  761
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ KONLY='MANY' <===                          761
         CALL AGSRESET                                                    765
         AG_X =                                                           765
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         765
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      765
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    765
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    765
         AG_KONLY = 'MANY'                                                765
         AG_TITLE = 'SFAA'                                                765
         AG_EXNAME = 'POSITION'                                           765
         AG_PARLIST = 'X_Y_Z_KONLY'                                       765
         CALL AXPOSITION                                                  765
C    *                                                                    765
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  765
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  765
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ KONLY='ONLY' <===              765
         CALL AGSRESET                                                    768
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               768
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    768
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   768
         AG_KONLY = 'ONLY'                                                768
         AG_TITLE = 'SFAA'                                                768
         AG_EXNAME = 'POSITION'                                           768
         AG_PARLIST = 'X_Y_Z_KONLY'                                       768
         CALL AXPOSITION                                                  768
C    *                                                                    768
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  768
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  768
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ K  768
C    ONLY='ONLY' <===                                                     768
         CALL AGSRESET                                                    771
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   771
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    771
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   771
         AG_KONLY = 'ONLY'                                                771
         AG_TITLE = 'SFAA'                                                771
         AG_EXNAME = 'POSITION'                                           771
         AG_PARLIST = 'X_Y_Z_KONLY'                                       771
         CALL AXPOSITION                                                  771
C    *                                                                    771
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  771
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  771
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  771
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ KONLY='ONLY' <===                         771
         CALL AGSRESET                                                    775
         AG_X =                                                           775
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         775
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      775
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    775
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   775
         AG_KONLY = 'ONLY'                                                775
         AG_TITLE = 'SFAA'                                                775
         AG_EXNAME = 'POSITION'                                           775
         AG_PARLIST = 'X_Y_Z_KONLY'                                       775
         CALL AXPOSITION                                                  775
C    *                                                                    775
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  775
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  775
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ KONLY='ONLY' <===              775
         CALL AGSRESET                                                    778
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               778
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    778
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   778
         AG_KONLY = 'ONLY'                                                778
         AG_TITLE = 'SFAA'                                                778
         AG_EXNAME = 'POSITION'                                           778
         AG_PARLIST = 'X_Y_Z_KONLY'                                       778
         CALL AXPOSITION                                                  778
C    *                                                                    778
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  778
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  778
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ K  778
C    ONLY='ONLY' <===                                                     778
         CALL AGSRESET                                                    781
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   781
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    781
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   781
         AG_KONLY = 'ONLY'                                                781
         AG_TITLE = 'SFAA'                                                781
         AG_EXNAME = 'POSITION'                                           781
         AG_PARLIST = 'X_Y_Z_KONLY'                                       781
         CALL AXPOSITION                                                  781
C    *                                                                    781
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  781
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  781
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  781
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ KONLY='ONLY' <===                         781
         CALL AGSRESET                                                    785
         AG_X =                                                           785
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         785
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      785
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    785
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   785
         AG_KONLY = 'ONLY'                                                785
         AG_TITLE = 'SFAA'                                                785
         AG_EXNAME = 'POSITION'                                           785
         AG_PARLIST = 'X_Y_Z_KONLY'                                       785
         CALL AXPOSITION                                                  785
C    *                                                                    785
5251  CALL AGSPOPB                                                        786
      GO TO BLOCK_SFSW, (5250 ,5260)                                      786
C End of Local Procedure                                                  786
C ----------------------------------------------------------------------  787
C ------------ SFRA                                                       787
C Begin Local Procedure SFRA                                              788
C SFRA IS THE HYBRID STIFFNEER                                            788
5740  CONTINUE                                                            788
         AG_TITLE='SFRA'                                                  788
         AG_MODULE=MODULE                                                 788
         AG_MARK='V00'                                                    788
         CALL AGSPUSH                                                     788
         AG_MODULE=MODULE                                                 788
C    *                                                                    788
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           789
         CALL AGSRESET                                                    789
         AG_TITLE = 'CARBON'                                              789
         AG_EXNAME = 'MATERIAL'                                           789
         AG_PARLIST = 'NONE'                                              789
         CALL AXMATERIAL                                                  789
C    *                                                                    789
C    Geant ATTRIBUTE : SFRA ===> SEEN=2 COLO=1 <===                       789
         CALL AGSRESET                                                    790
         AG_SEEN = 2                                                      790
         AG_COLO = 1                                                      790
         AG_TITLE = 'SFRA'                                                790
         AG_EXNAME = 'ATTRIBUTE'                                          790
         AG_PARLIST = 'SEEN_COLO'                                         790
         CALL AXATTRIBUTE                                                 790
C    *                                                                    790
C    Geant SHAPE : BOX ===> DX=SFJP_SFRA_DX DY=SFJP_SFRA_DY, DZ=SFJP_SFR  790
C    A_DZ <===                                                            790
         CALL AGSRESET                                                    792
         AG_DX = SFJP_SFRA_DX                                             792
         AG_DY = SFJP_SFRA_DY                                             792
         AG_DZ = SFJP_SFRA_DZ                                             792
         AG_TITLE = 'BOX'                                                 792
         AG_EXNAME = 'SHAPE'                                              792
         AG_PARLIST = 'DX_DY_DZ'                                          792
         CALL AXSHAPE                                                     792
C    *                                                                    792
5741  CALL AGSPOPB                                                        793
      GO TO BLOCK_SFRA, (5740 ,5750)                                      793
C End of Local Procedure                                                  793
C ----------------------------------------------------------------------  794
C ------------ SFRS                                                       794
C Begin Local Procedure SFRS                                              795
C SFRS TWO SUPPORTS OF THE HYBRID STIFFNEER (PIECE OF IT)                 795
5760  CONTINUE                                                            795
         AG_TITLE='SFRS'                                                  795
         AG_MODULE=MODULE                                                 795
         AG_MARK='V00'                                                    795
         CALL AGSPUSH                                                     795
         AG_MODULE=MODULE                                                 795
C    *                                                                    795
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           796
         CALL AGSRESET                                                    796
         AG_TITLE = 'CARBON'                                              796
         AG_EXNAME = 'MATERIAL'                                           796
         AG_PARLIST = 'NONE'                                              796
         CALL AXMATERIAL                                                  796
C    *                                                                    796
C    Geant ATTRIBUTE : SFRS ===> SEEN=2 COLO=1 <===                       796
         CALL AGSRESET                                                    797
         AG_SEEN = 2                                                      797
         AG_COLO = 1                                                      797
         AG_TITLE = 'SFRS'                                                797
         AG_EXNAME = 'ATTRIBUTE'                                          797
         AG_PARLIST = 'SEEN_COLO'                                         797
         CALL AXATTRIBUTE                                                 797
C    *                                                                    797
C    Geant SHAPE : BOX ===> DX=SFJP_SFRS_DX DY=SFJP_SFRS_DY, DZ=SFJP_SFR  797
C    A_DZ <===                                                            797
         CALL AGSRESET                                                    799
         AG_DX = SFJP_SFRS_DX                                             799
         AG_DY = SFJP_SFRS_DY                                             799
         AG_DZ = SFJP_SFRA_DZ                                             799
         AG_TITLE = 'BOX'                                                 799
         AG_EXNAME = 'SHAPE'                                              799
         AG_PARLIST = 'DX_DY_DZ'                                          799
         CALL AXSHAPE                                                     799
C    *                                                                    799
5761  CALL AGSPOPB                                                        800
      GO TO BLOCK_SFRS, (5760 ,5770)                                      800
C End of Local Procedure                                                  800
C ----------------------------------------------------------------------  801
C ------------ SFFX                                                       801
C Begin Local Procedure SFFX                                              802
C SFFX IS THE FLEX                                                        802
5780  CONTINUE                                                            802
         AG_TITLE='SFFX'                                                  802
         AG_MODULE=MODULE                                                 802
         AG_MARK='V00'                                                    802
         CALL AGSPUSH                                                     802
         AG_MODULE=MODULE                                                 802
C    *                                                                    802
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           803
         CALL AGSRESET                                                    803
         AG_TITLE = 'COPPER'                                              803
         AG_EXNAME = 'MATERIAL'                                           803
         AG_PARLIST = 'NONE'                                              803
         CALL AXMATERIAL                                                  803
C    *                                                                    803
C    Geant ATTRIBUTE : SFFX ===> SEEN=1 COLO=42 <===                      803
         CALL AGSRESET                                                    804
         AG_SEEN = 1                                                      804
         AG_COLO = 42                                                     804
         AG_TITLE = 'SFFX'                                                804
         AG_EXNAME = 'ATTRIBUTE'                                          804
         AG_PARLIST = 'SEEN_COLO'                                         804
         CALL AXATTRIBUTE                                                 804
C    *                                                                    804
C    Geant SHAPE : BOX ===> DX=SFJP_SFFX_DX DY=SFJP_SFFX_DYZ, DZ=SFJP_SF  804
C    RA_DZ <===                                                           804
         CALL AGSRESET                                                    806
         AG_DX = SFJP_SFFX_DX                                             806
         AG_DY = SFJP_SFFX_DYZ                                            806
         AG_DZ = SFJP_SFRA_DZ                                             806
         AG_TITLE = 'BOX'                                                 806
         AG_EXNAME = 'SHAPE'                                              806
         AG_PARLIST = 'DX_DY_DZ'                                          806
         CALL AXSHAPE                                                     806
C    *                                                                    806
5781  CALL AGSPOPB                                                        807
      GO TO BLOCK_SFFX, (5780 ,5790)                                      807
C End of Local Procedure                                                  807
C ----------------------------------------------------------------------  808
C ------------ SFAA                                                       808
C Begin Local Procedure SFAA                                              809
C SFAA IS THE A128C CHIP                                                  809
5820  CONTINUE                                                            809
         AG_TITLE='SFAA'                                                  809
         AG_MODULE=MODULE                                                 809
         AG_MARK='V00'                                                    809
         CALL AGSPUSH                                                     809
         AG_MODULE=MODULE                                                 809
C    *                                                                    809
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          810
         CALL AGSRESET                                                    810
         AG_TITLE = 'SILICON'                                             810
         AG_EXNAME = 'MATERIAL'                                           810
         AG_PARLIST = 'NONE'                                              810
         CALL AXMATERIAL                                                  810
C    *                                                                    810
C    Geant ATTRIBUTE : SFAA ===> SEEN=1 COLO=41 <===                      810
         CALL AGSRESET                                                    811
         AG_SEEN = 1                                                      811
         AG_COLO = 41                                                     811
         AG_TITLE = 'SFAA'                                                811
         AG_EXNAME = 'ATTRIBUTE'                                          811
         AG_PARLIST = 'SEEN_COLO'                                         811
         CALL AXATTRIBUTE                                                 811
C    *                                                                    811
C    Geant SHAPE : BOX ===> DX=SFJP_SFAA_DX DY=SFJP_SFAA_DY, DZ=SFJP_SFA  811
C    A_DZ <===                                                            811
         CALL AGSRESET                                                    813
         AG_DX = SFJP_SFAA_DX                                             813
         AG_DY = SFJP_SFAA_DY                                             813
         AG_DZ = SFJP_SFAA_DZ                                             813
         AG_TITLE = 'BOX'                                                 813
         AG_EXNAME = 'SHAPE'                                              813
         AG_PARLIST = 'DX_DY_DZ'                                          813
         CALL AXSHAPE                                                     813
C    *                                                                    813
5821  CALL AGSPOPB                                                        814
      GO TO BLOCK_SFAA, (5820 ,5830)                                      814
C End of Local Procedure                                                  814
C ----------------------------------------------------------------------  815
C ------------ SFPI                                                       815
C Begin Local Procedure SFPI                                              816
C SFPI ARE THE PIONS                                                      816
5800  CONTINUE                                                            816
         AG_TITLE='SFPI'                                                  816
         AG_MODULE=MODULE                                                 816
         AG_MARK='V00'                                                    816
         CALL AGSPUSH                                                     816
         AG_MODULE=MODULE                                                 816
C    *                                                                    816
C    Geant MATERIAL : AIR ===> inherited arguments used <===              817
         CALL AGSRESET                                                    817
         AG_TITLE = 'AIR'                                                 817
         AG_EXNAME = 'MATERIAL'                                           817
         AG_PARLIST = 'NONE'                                              817
         CALL AXMATERIAL                                                  817
C    *                                                                    817
C    Geant ATTRIBUTE : SFPI ===> SEEN=2 COLO=33 <===                      817
         CALL AGSRESET                                                    818
         AG_SEEN = 2                                                      818
         AG_COLO = 33                                                     818
         AG_TITLE = 'SFPI'                                                818
         AG_EXNAME = 'ATTRIBUTE'                                          818
         AG_PARLIST = 'SEEN_COLO'                                         818
         CALL AXATTRIBUTE                                                 818
C    *                                                                    818
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPI_RMIN/2. RMAX=SFJP_SFPJ_DY, D  818
C    Z=SFJP_SFPI_PZ/2.0+SFJP_SFPJ_DZ <===                                 818
         CALL AGSRESET                                                    820
         AG_RMIN = SFJP_SFPI_RMIN/2.                                      820
         AG_RMAX = SFJP_SFPJ_DY                                           820
         AG_DZ = SFJP_SFPI_PZ/2.0+SFJP_SFPJ_DZ                            820
         AG_TITLE = 'TUBE'                                                820
         AG_EXNAME = 'SHAPE'                                              820
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      820
         CALL AXSHAPE                                                     820
C    *                                                                    820
C    Create SFPK :                                                        821
C    Call To Local Procedure SFPK                                         821
         ASSIGN 5850 TO BLOCK_SFPK                                        821
         GO TO  5840                                                      821
5850     CONTINUE                                                         821
C    *                                                                    821
C    Geant POSITION : SFPK ===> X=0. Y=0. Z=0 <===                        822
         CALL AGSRESET                                                    822
         AG_X = 0.                                                        822
         AG_Y = 0.                                                        822
         AG_Z = 0                                                         822
         AG_TITLE = 'SFPK'                                                822
         AG_EXNAME = 'POSITION'                                           822
         AG_PARLIST = 'X_Y_Z'                                             822
         CALL AXPOSITION                                                  822
C    *                                                                    822
C    Create SFPJ :                                                        823
C    Call To Local Procedure SFPJ                                         823
         ASSIGN 5870 TO BLOCK_SFPJ                                        823
         GO TO  5860                                                      823
5870     CONTINUE                                                         823
C    *                                                                    823
C    Geant POSITION : SFPJ ===> X=0. Y=0. Z=-SFJP_SFPI_PZ/2.0+SFJP_SFPJ_  824
C    DZ <===                                                              824
         CALL AGSRESET                                                    824
         AG_X = 0.                                                        824
         AG_Y = 0.                                                        824
         AG_Z = -SFJP_SFPI_PZ/2.0+SFJP_SFPJ_DZ                            824
         AG_TITLE = 'SFPJ'                                                824
         AG_EXNAME = 'POSITION'                                           824
         AG_PARLIST = 'X_Y_Z'                                             824
         CALL AXPOSITION                                                  824
C    *                                                                    824
5801  CALL AGSPOPB                                                        825
      GO TO BLOCK_SFPI, (5800 ,5810)                                      825
C End of Local Procedure                                                  825
C ----------------------------------------------------------------------  826
C ------------ SFPK                                                       826
C Begin Local Procedure SFPK                                              827
C SFPK IS THE SMALL RADIUS CYLINDER OF THE PIONS                          827
5840  CONTINUE                                                            827
         AG_TITLE='SFPK'                                                  827
         AG_MODULE=MODULE                                                 827
         AG_MARK='V00'                                                    827
         CALL AGSPUSH                                                     827
         AG_MODULE=MODULE                                                 827
C    *                                                                    827
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        828
         CALL AGSRESET                                                    828
         AG_TITLE = 'ALUMINIUM'                                           828
         AG_EXNAME = 'MATERIAL'                                           828
         AG_PARLIST = 'NONE'                                              828
         CALL AXMATERIAL                                                  828
C    *                                                                    828
C    Geant ATTRIBUTE : SFPJ ===> SEEN=2 COLO=33 <===                      828
         CALL AGSRESET                                                    829
         AG_SEEN = 2                                                      829
         AG_COLO = 33                                                     829
         AG_TITLE = 'SFPJ'                                                829
         AG_EXNAME = 'ATTRIBUTE'                                          829
         AG_PARLIST = 'SEEN_COLO'                                         829
         CALL AXATTRIBUTE                                                 829
C    *                                                                    829
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPI_RMIN/2. RMAX=SFJP_SFPI_RMAX/  829
C    2., DZ=SFJP_SFPI_PZ/2.0 <===                                         829
         CALL AGSRESET                                                    831
         AG_RMIN = SFJP_SFPI_RMIN/2.                                      831
         AG_RMAX = SFJP_SFPI_RMAX/2.                                      831
         AG_DZ = SFJP_SFPI_PZ/2.0                                         831
         AG_TITLE = 'TUBE'                                                831
         AG_EXNAME = 'SHAPE'                                              831
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      831
         CALL AXSHAPE                                                     831
C    *                                                                    831
5841  CALL AGSPOPB                                                        832
      GO TO BLOCK_SFPK, (5840 ,5850)                                      832
C End of Local Procedure                                                  832
C ----------------------------------------------------------------------  833
C ------------ SFPJ                                                       833
C Begin Local Procedure SFPJ                                              834
C SFPJ IS THE BASE OF THE PIONS                                           834
5860  CONTINUE                                                            834
         AG_TITLE='SFPJ'                                                  834
         AG_MODULE=MODULE                                                 834
         AG_MARK='V00'                                                    834
         CALL AGSPUSH                                                     834
         AG_MODULE=MODULE                                                 834
C    *                                                                    834
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        835
         CALL AGSRESET                                                    835
         AG_TITLE = 'ALUMINIUM'                                           835
         AG_EXNAME = 'MATERIAL'                                           835
         AG_PARLIST = 'NONE'                                              835
         CALL AXMATERIAL                                                  835
C    *                                                                    835
C    Geant ATTRIBUTE : SFPJ ===> SEEN=2 COLO=33 <===                      835
         CALL AGSRESET                                                    836
         AG_SEEN = 2                                                      836
         AG_COLO = 33                                                     836
         AG_TITLE = 'SFPJ'                                                836
         AG_EXNAME = 'ATTRIBUTE'                                          836
         AG_PARLIST = 'SEEN_COLO'                                         836
         CALL AXATTRIBUTE                                                 836
C    *                                                                    836
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPJ_DX1+SFJP_SFPJ_DX2, RMAX=SFJP  836
C    _SFPJ_DY DZ=SFJP_SFPJ_DZ <===                                        836
         CALL AGSRESET                                                    838
         AG_RMIN = SFJP_SFPJ_DX1+SFJP_SFPJ_DX2                            838
         AG_RMAX = SFJP_SFPJ_DY                                           838
         AG_DZ = SFJP_SFPJ_DZ                                             838
         AG_TITLE = 'TUBE'                                                838
         AG_EXNAME = 'SHAPE'                                              838
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      838
         CALL AXSHAPE                                                     838
C    *                                                                    838
5861  CALL AGSPOPB                                                        839
      GO TO BLOCK_SFPJ, (5860 ,5870)                                      839
C End of Local Procedure                                                  839
C ----------------------------------------------------------------------  840
C ------------ SFSL                                                       840
C Begin Local Procedure SFSL                                              841
C SFSL IS THE SILICON OF THE STRIP DETECTOR                               841
5720  CONTINUE                                                            841
         AG_TITLE='SFSL'                                                  841
         AG_MODULE=MODULE                                                 841
         AG_MARK='V00'                                                    841
         CALL AGSPUSH                                                     841
         AG_MODULE=MODULE                                                 841
C    *                                                                    841
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          842
         CALL AGSRESET                                                    842
         AG_TITLE = 'SILICON'                                             842
         AG_EXNAME = 'MATERIAL'                                           842
         AG_PARLIST = 'NONE'                                              842
         CALL AXMATERIAL                                                  842
C    *                                                                    842
C    Geant ATTRIBUTE : SFSL ===> SEEN=2 COLO=1 <===                       842
         CALL AGSRESET                                                    843
         AG_SEEN = 2                                                      843
         AG_COLO = 1                                                      843
         AG_TITLE = 'SFSL'                                                843
         AG_EXNAME = 'ATTRIBUTE'                                          843
         AG_PARLIST = 'SEEN_COLO'                                         843
         CALL AXATTRIBUTE                                                 843
C    *                                                                    843
C    Geant SHAPE : BOX ===> DX=SFJP_SFSL_DX DY=SFJP_SFSL_DY, DZ=SFJP_SFS  843
C    L_DZ <===                                                            843
         CALL AGSRESET                                                    845
         AG_DX = SFJP_SFSL_DX                                             845
         AG_DY = SFJP_SFSL_DY                                             845
         AG_DZ = SFJP_SFSL_DZ                                             845
         AG_TITLE = 'BOX'                                                 845
         AG_EXNAME = 'SHAPE'                                              845
         AG_PARLIST = 'DX_DY_DZ'                                          845
         CALL AXSHAPE                                                     845
C    *                                                                    845
C    Create SFSD :                                                        846
C    Call To Local Procedure SFSD                                         846
         ASSIGN 5890 TO BLOCK_SFSD                                        846
         GO TO  5880                                                      846
5890     CONTINUE                                                         846
C    *                                                                    846
C    *                                                                    847
C       Check SFPA_VERSION<=7.0                                           847
            IF (SFPA_VERSION.LE.7.0) THEN                                 847
C       Geant POSITION : SFSD ===> inherited arguments used <===          848
            CALL AGSRESET                                                 848
            AG_TITLE = 'SFSD'                                             848
            AG_EXNAME = 'POSITION'                                        848
            AG_PARLIST = 'NONE'                                           848
            CALL AXPOSITION                                               848
C       *                                                                 848
         END IF                                                           849
C    *                                                                    850
C       Check SFPA_VERSION> 7.0                                           850
            IF (SFPA_VERSION.GT. 7.0) THEN                                850
C       Geant POSITION : SFSD ===> ALPHAZ=180 <===                        851
            CALL AGSRESET                                                 851
            AG_ALPHAN=AG_ALPHAN+1                                         851
            AG_ALPHAK(AG_ALPHAN)=3                                        851
            AG_ALPHAL(AG_ALPHAN)=180                                      851
            AG_TITLE = 'SFSD'                                             851
            AG_EXNAME = 'POSITION'                                        851
            AG_PARLIST = 'ALPHAZ'                                         851
            CALL AXPOSITION                                               851
C       *                                                                 851
         END IF                                                           852
5721  CALL AGSPOPB                                                        853
      GO TO BLOCK_SFSL, (5720 ,5730)                                      853
C End of Local Procedure                                                  853
C ----------------------------------------------------------------------  854
C ------------ SFSD                                                       854
C Begin Local Procedure SFSD                                              855
C SFSD IS THE STRIP DETECTOR                                              855
5880  CONTINUE                                                            855
         AG_TITLE='SFSD'                                                  855
         AG_MODULE=MODULE                                                 855
         AG_MARK='V00'                                                    855
         CALL AGSPUSH                                                     855
         AG_MODULE=MODULE                                                 855
C    *                                                                    855
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          856
         CALL AGSRESET                                                    856
         AG_TITLE = 'SILICON'                                             856
         AG_EXNAME = 'MATERIAL'                                           856
         AG_PARLIST = 'NONE'                                              856
         CALL AXMATERIAL                                                  856
C    *                                                                    856
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         856
         CALL AGSRESET                                                    857
         AG_ISVOL = 1                                                     857
         AG_TITLE = 'SENSITIVE'                                           857
         AG_EXNAME = 'MATERIAL'                                           857
         AG_PARLIST = 'ISVOL'                                             857
         CALL AXMATERIAL                                                  857
C    *                                                                    857
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=41 <===                      857
         CALL AGSRESET                                                    858
         AG_SEEN = 2                                                      858
         AG_COLO = 41                                                     858
         AG_TITLE = 'SFSD'                                                858
         AG_EXNAME = 'ATTRIBUTE'                                          858
         AG_PARLIST = 'SEEN_COLO'                                         858
         CALL AXATTRIBUTE                                                 858
C    *                                                                    858
C    Geant SHAPE : BOX ===> DX=SFJP_SFSD_DX DY=SFJP_SFSD_DY, DZ=SFJP_SFS  858
C    D_DZ <===                                                            858
         CALL AGSRESET                                                    860
         AG_DX = SFJP_SFSD_DX                                             860
         AG_DY = SFJP_SFSD_DY                                             860
         AG_DZ = SFJP_SFSD_DZ                                             860
         AG_TITLE = 'BOX'                                                 860
         AG_EXNAME = 'SHAPE'                                              860
         AG_PARLIST = 'DX_DY_DZ'                                          860
         CALL AXSHAPE                                                     860
C    *                                                                    860
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 860
C    Defining Hits: X:0.001:S Y:0.001: Z:0.001: CX:10: CY:10: CZ:10: STE  863
C    P:0.01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100.0) ELOSS  863
C    :16:(0,0.01)                                                         863
         AG_TITLE = 'SFSD'                                                863
         AG_EXNAME = 'HITS'                                               863
         AG_TYPE=AG_MAGIC                                                 863
         CALL AGSDETV('SFSD')                                             863
C    *                                                                    863
         AG_PARLIST='X'                                                   863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = AG_MAGIC                                              863
         AG_BIN=0.001                                                     863
         AG_OPTION='S'                                                    863
         AG_FHMIN = AG_MAGIC                                              863
         AG_FHMAX = AG_MAGIC                                              863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='Y'                                                   863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = AG_MAGIC                                              863
         AG_BIN=0.001                                                     863
         AG_FHMIN=AG_MAGIC                                                863
         AG_FHMAX=AG_MAGIC                                                863
         AG_OPTION='*'                                                    863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='Z'                                                   863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = AG_MAGIC                                              863
         AG_BIN=0.001                                                     863
         AG_FHMIN=AG_MAGIC                                                863
         AG_FHMAX=AG_MAGIC                                                863
         AG_OPTION='*'                                                    863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='CX'                                                  863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = 10                                                    863
         AG_BIN=AG_MAGIC                                                  863
         AG_FHMIN=AG_MAGIC                                                863
         AG_FHMAX=AG_MAGIC                                                863
         AG_OPTION='*'                                                    863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='CY'                                                  863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = 10                                                    863
         AG_BIN=AG_MAGIC                                                  863
         AG_FHMIN=AG_MAGIC                                                863
         AG_FHMAX=AG_MAGIC                                                863
         AG_OPTION='*'                                                    863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='CZ'                                                  863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = 10                                                    863
         AG_BIN=AG_MAGIC                                                  863
         AG_FHMIN=AG_MAGIC                                                863
         AG_FHMAX=AG_MAGIC                                                863
         AG_OPTION='*'                                                    863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='STEP'                                                863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = AG_MAGIC                                              863
         AG_BIN=0.01                                                      863
         AG_FHMIN=AG_MAGIC                                                863
         AG_FHMAX=AG_MAGIC                                                863
         AG_OPTION='*'                                                    863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='SLENG'                                               863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = 16                                                    863
         AG_BIN=AG_MAGIC                                                  863
         AG_OPTION='*'                                                    863
         AG_FHMIN = 0                                                     863
         AG_FHMAX = 500                                                   863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='TOF'                                                 863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = 16                                                    863
         AG_BIN=AG_MAGIC                                                  863
         AG_OPTION='*'                                                    863
         AG_FHMIN = 0                                                     863
         AG_FHMAX = 1.0E-6                                                863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='PTOT'                                                863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = 16                                                    863
         AG_BIN=AG_MAGIC                                                  863
         AG_OPTION='*'                                                    863
         AG_FHMIN = 0                                                     863
         AG_FHMAX = 100.0                                                 863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         AG_PARLIST='ELOSS'                                               863
         AG_TITLE = 'DETH'                                                863
         AG_NBITS = 16                                                    863
         AG_BIN=AG_MAGIC                                                  863
         AG_OPTION='*'                                                    863
         AG_FHMIN = 0                                                     863
         AG_FHMAX = 0.01                                                  863
         CALL AGSDETH (0)                                                 863
C    *                                                                    863
         CALL AGSDETU('SFSD')                                             863
C    *                                                                    863
5881  CALL AGSPOPB                                                        864
      GO TO BLOCK_SFSD, (5880 ,5890)                                      864
C End of Local Procedure                                                  864
C ----------------------------------------------------------------------  865
C ------------ SFLT                                                       865
C Begin Local Procedure SFLT                                              866
C SFLT IS (HALF) THE TOP CORNER OF THE TRIANGULAR LADDER SKELETON         866
5270  CONTINUE                                                            866
         AG_TITLE='SFLT'                                                  866
         AG_MODULE=MODULE                                                 866
         AG_MARK='V00'                                                    866
         CALL AGSPUSH                                                     866
         AG_MODULE=MODULE                                                 866
C    *                                                                    866
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           867
         CALL AGSRESET                                                    867
         AG_TITLE = 'CARBON'                                              867
         AG_EXNAME = 'MATERIAL'                                           867
         AG_PARLIST = 'NONE'                                              867
         CALL AXMATERIAL                                                  867
C    *                                                                    867
C    Geant ATTRIBUTE : SFLT ===> SEEN=1 COLO=1 <===                       867
         CALL AGSRESET                                                    868
         AG_SEEN = 1                                                      868
         AG_COLO = 1                                                      868
         AG_TITLE = 'SFLT'                                                868
         AG_EXNAME = 'ATTRIBUTE'                                          868
         AG_PARLIST = 'SEEN_COLO'                                         868
         CALL AXATTRIBUTE                                                 868
C    *                                                                    868
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  868
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP1  868
C    =(PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI), H2=SFJP_SFLU_H1 BL2=SFJP_SFLU  868
C    _BL1, TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP2=(PI/2-ATAN(2*T  868
C    AN(PI/5)))*360/(2*PI) <===                                           868
         CALL AGSRESET                                                    874
         AG_DZ = SFJP_SFLU_DZ                                             874
         AG_THET = 0                                                      874
         AG_PHI = 0                                                       874
         AG_H1 = SFJP_SFLU_H1                                             874
         AG_BL1 = SFJP_SFLU_BL1                                           874
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    874
         AG_ALP1 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    874
         AG_H2 = SFJP_SFLU_H1                                             874
         AG_BL2 = SFJP_SFLU_BL1                                           874
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    874
         AG_ALP2 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    874
         AG_TITLE = 'TRAP'                                                874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       874
         CALL AXSHAPE                                                     874
C    *                                                                    874
5271  CALL AGSPOPB                                                        875
      GO TO BLOCK_SFLT, (5270 ,5280)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ SFLU                                                       876
C Begin Local Procedure SFLU                                              877
C SFLU IS (HALF) THE SIDE CORNER OF THE TRIANGULAR LADDER SKELETON        877
5290  CONTINUE                                                            877
         AG_TITLE='SFLU'                                                  877
         AG_MODULE=MODULE                                                 877
         AG_MARK='V00'                                                    877
         CALL AGSPUSH                                                     877
         AG_MODULE=MODULE                                                 877
C    *                                                                    877
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           878
         CALL AGSRESET                                                    878
         AG_TITLE = 'CARBON'                                              878
         AG_EXNAME = 'MATERIAL'                                           878
         AG_PARLIST = 'NONE'                                              878
         CALL AXMATERIAL                                                  878
C    *                                                                    878
C    Geant ATTRIBUTE : SFLU ===> SEEN=1 COLO=1 <===                       878
         CALL AGSRESET                                                    879
         AG_SEEN = 1                                                      879
         AG_COLO = 1                                                      879
         AG_TITLE = 'SFLU'                                                879
         AG_EXNAME = 'ATTRIBUTE'                                          879
         AG_PARLIST = 'SEEN_COLO'                                         879
         CALL AXATTRIBUTE                                                 879
C    *                                                                    879
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  879
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360  879
C    ), ALP1=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI), H2=SFJP_SFLU_H1  879
C    , BL2=SFJP_SFLU_BL1 TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)  879
C    , ALP2=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI) <===               879
         CALL AGSRESET                                                    885
         AG_DZ = SFJP_SFLU_DZ                                             885
         AG_THET = 0                                                      885
         AG_PHI = 0                                                       885
         AG_H1 = SFJP_SFLU_H1                                             885
         AG_BL1 = SFJP_SFLU_BL1                                           885
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             885
         AG_ALP1 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             885
         AG_H2 = SFJP_SFLU_H1                                             885
         AG_BL2 = SFJP_SFLU_BL1                                           885
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             885
         AG_ALP2 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             885
         AG_TITLE = 'TRAP'                                                885
         AG_EXNAME = 'SHAPE'                                              885
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       885
         CALL AXSHAPE                                                     885
C    *                                                                    885
5291  CALL AGSPOPB                                                        886
      GO TO BLOCK_SFLU, (5290 ,5300)                                      886
C End of Local Procedure                                                  886
C ----------------------------------------------------------------------  887
C ------------ SFFK                                                       887
C Begin Local Procedure SFFK                                              888
C SFFK HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE                 888
5310  CONTINUE                                                            888
         AG_TITLE='SFFK'                                                  888
         AG_MODULE=MODULE                                                 888
         AG_MARK='V00'                                                    888
         CALL AGSPUSH                                                     888
         AG_MODULE=MODULE                                                 888
C    *                                                                    888
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           889
         CALL AGSRESET                                                    889
         AG_TITLE = 'CARBON'                                              889
         AG_EXNAME = 'MATERIAL'                                           889
         AG_PARLIST = 'NONE'                                              889
         CALL AXMATERIAL                                                  889
C    *                                                                    889
C    Geant ATTRIBUTE : SFFK ===> SEEN=1 COLO=1 <===                       889
         CALL AGSRESET                                                    890
         AG_SEEN = 1                                                      890
         AG_COLO = 1                                                      890
         AG_TITLE = 'SFFK'                                                890
         AG_EXNAME = 'ATTRIBUTE'                                          890
         AG_PARLIST = 'SEEN_COLO'                                         890
         CALL AXATTRIBUTE                                                 890
C    *                                                                    890
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXE DY=SFJP_SFFK_DY, DZ=SFJP_SF  890
C    FK_DZ/2 <===                                                         890
         CALL AGSRESET                                                    892
         AG_DX = SFJP_SFFK_DXE                                            892
         AG_DY = SFJP_SFFK_DY                                             892
         AG_DZ = SFJP_SFFK_DZ/2                                           892
         AG_TITLE = 'BOX'                                                 892
         AG_EXNAME = 'SHAPE'                                              892
         AG_PARLIST = 'DX_DY_DZ'                                          892
         CALL AXSHAPE                                                     892
C    *                                                                    892
5311  CALL AGSPOPB                                                        893
      GO TO BLOCK_SFFK, (5310 ,5320)                                      893
C End of Local Procedure                                                  893
C ----------------------------------------------------------------------  894
C ------------ SFFL                                                       894
C Begin Local Procedure SFFL                                              895
C SFFL TITLED PART OF THE LADDER SKELETON CARBON BASE                     895
5330  CONTINUE                                                            895
         AG_TITLE='SFFL'                                                  895
         AG_MODULE=MODULE                                                 895
         AG_MARK='V00'                                                    895
         CALL AGSPUSH                                                     895
         AG_MODULE=MODULE                                                 895
C    *                                                                    895
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           896
         CALL AGSRESET                                                    896
         AG_TITLE = 'CARBON'                                              896
         AG_EXNAME = 'MATERIAL'                                           896
         AG_PARLIST = 'NONE'                                              896
         CALL AXMATERIAL                                                  896
C    *                                                                    896
C    Geant ATTRIBUTE : SFFL ===> SEEN=1 COLO=1 <===                       896
         CALL AGSRESET                                                    897
         AG_SEEN = 1                                                      897
         AG_COLO = 1                                                      897
         AG_TITLE = 'SFFL'                                                897
         AG_EXNAME = 'ATTRIBUTE'                                          897
         AG_PARLIST = 'SEEN_COLO'                                         897
         CALL AXATTRIBUTE                                                 897
C    *                                                                    897
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFFK  897
C    _DY DZ=SFJP_SFFK_DZ/2 <===                                           897
         CALL AGSRESET                                                    899
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              899
         AG_DY = SFJP_SFFK_DY                                             899
         AG_DZ = SFJP_SFFK_DZ/2                                           899
         AG_TITLE = 'BOX'                                                 899
         AG_EXNAME = 'SHAPE'                                              899
         AG_PARLIST = 'DX_DY_DZ'                                          899
         CALL AXSHAPE                                                     899
C    *                                                                    899
5331  CALL AGSPOPB                                                        900
      GO TO BLOCK_SFFL, (5330 ,5340)                                      900
C End of Local Procedure                                                  900
C ----------------------------------------------------------------------  901
C ------------ SFKK                                                       901
C Begin Local Procedure SFKK                                              902
C SFKK HORIZONTAL PART OF THE KAPTON FILM UNDER THE LADDER BASE           902
5350  CONTINUE                                                            902
         AG_TITLE='SFKK'                                                  902
         AG_MODULE=MODULE                                                 902
         AG_MARK='V00'                                                    902
         CALL AGSPUSH                                                     902
         AG_MODULE=MODULE                                                 902
C    *                                                                    902
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            903
         CALL AGSRESET                                                    903
         AG_TITLE = 'MYLAR'                                               903
         AG_EXNAME = 'MATERIAL'                                           903
         AG_PARLIST = 'NONE'                                              903
         CALL AXMATERIAL                                                  903
C    *                                                                    903
C    Geant ATTRIBUTE : SFKK ===> SEEN=1 COLO=1 <===                       903
         CALL AGSRESET                                                    904
         AG_SEEN = 1                                                      904
         AG_COLO = 1                                                      904
         AG_TITLE = 'SFKK'                                                904
         AG_EXNAME = 'ATTRIBUTE'                                          904
         AG_PARLIST = 'SEEN_COLO'                                         904
         CALL AXATTRIBUTE                                                 904
C    *                                                                    904
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXZ/2. DY=SFJP_SFKK_DY/2., DZ=S  904
C    FJP_SFFK_DZ/2 <===                                                   904
         CALL AGSRESET                                                    906
         AG_DX = SFJP_SFFK_DXZ/2.                                         906
         AG_DY = SFJP_SFKK_DY/2.                                          906
         AG_DZ = SFJP_SFFK_DZ/2                                           906
         AG_TITLE = 'BOX'                                                 906
         AG_EXNAME = 'SHAPE'                                              906
         AG_PARLIST = 'DX_DY_DZ'                                          906
         CALL AXSHAPE                                                     906
C    *                                                                    906
5351  CALL AGSPOPB                                                        907
      GO TO BLOCK_SFKK, (5350 ,5360)                                      907
C End of Local Procedure                                                  907
C ----------------------------------------------------------------------  908
C ------------ SFKL                                                       908
C Begin Local Procedure SFKL                                              909
C SFKL TITLED PART OF THE KPATON FILM UNDER THE LADDER BASE               909
5370  CONTINUE                                                            909
         AG_TITLE='SFKL'                                                  909
         AG_MODULE=MODULE                                                 909
         AG_MARK='V00'                                                    909
         CALL AGSPUSH                                                     909
         AG_MODULE=MODULE                                                 909
C    *                                                                    909
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            910
         CALL AGSRESET                                                    910
         AG_TITLE = 'MYLAR'                                               910
         AG_EXNAME = 'MATERIAL'                                           910
         AG_PARLIST = 'NONE'                                              910
         CALL AXMATERIAL                                                  910
C    *                                                                    910
C    Geant ATTRIBUTE : SFKL ===> SEEN=1 COLO=1 <===                       910
         CALL AGSRESET                                                    911
         AG_SEEN = 1                                                      911
         AG_COLO = 1                                                      911
         AG_TITLE = 'SFKL'                                                911
         AG_EXNAME = 'ATTRIBUTE'                                          911
         AG_PARLIST = 'SEEN_COLO'                                         911
         CALL AXATTRIBUTE                                                 911
C    *                                                                    911
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFKK  911
C    _DY/2. DZ=SFJP_SFFK_DZ/2 <===                                        911
         CALL AGSRESET                                                    913
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              913
         AG_DY = SFJP_SFKK_DY/2.                                          913
         AG_DZ = SFJP_SFFK_DZ/2                                           913
         AG_TITLE = 'BOX'                                                 913
         AG_EXNAME = 'SHAPE'                                              913
         AG_PARLIST = 'DX_DY_DZ'                                          913
         CALL AXSHAPE                                                     913
C    *                                                                    913
5371  CALL AGSPOPB                                                        914
      GO TO BLOCK_SFKL, (5370 ,5380)                                      914
C End of Local Procedure                                                  914
C ----------------------------------------------------------------------  915
C ------------ SFLA                                                       915
C Begin Local Procedure SFLA                                              917
C SFLA IS THE LONG PART OF THE BUS CABLE LINKING THE MODULES TO THE CONN  917
C ECTION, BOARD                                                           917
5590  CONTINUE                                                            917
         AG_TITLE='SFLA'                                                  917
         AG_MODULE=MODULE                                                 917
         AG_MARK='V00'                                                    917
         CALL AGSPUSH                                                     917
         AG_MODULE=MODULE                                                 917
C    *                                                                    917
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            918
         CALL AGSRESET                                                    918
         AG_TITLE = 'MYLAR'                                               918
         AG_EXNAME = 'MATERIAL'                                           918
         AG_PARLIST = 'NONE'                                              918
         CALL AXMATERIAL                                                  918
C    *                                                                    918
C    Geant ATTRIBUTE : SFLA ===> SEEN=1 COLO=1 <===                       918
         CALL AGSRESET                                                    919
         AG_SEEN = 1                                                      919
         AG_COLO = 1                                                      919
         AG_TITLE = 'SFLA'                                                919
         AG_EXNAME = 'ATTRIBUTE'                                          919
         AG_PARLIST = 'SEEN_COLO'                                         919
         CALL AXATTRIBUTE                                                 919
C    *                                                                    919
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  919
C    JP_SFLA_DZ/2. <===                                                   919
         CALL AGSRESET                                                    921
         AG_DX = SFJP_SFLA_DX/2.                                          921
         AG_DY = SFJP_SFLA_DY/2.                                          921
         AG_DZ = SFJP_SFLA_DZ/2.                                          921
         AG_TITLE = 'BOX'                                                 921
         AG_EXNAME = 'SHAPE'                                              921
         AG_PARLIST = 'DX_DY_DZ'                                          921
         CALL AXSHAPE                                                     921
C    *                                                                    921
5591  CALL AGSPOPB                                                        922
      GO TO BLOCK_SFLA, (5590 ,5600)                                      922
C End of Local Procedure                                                  922
C ----------------------------------------------------------------------  923
C ------------ SFLB                                                       923
C Begin Local Procedure SFLB                                              924
C SFLB IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD          924
5610  CONTINUE                                                            924
         AG_TITLE='SFLB'                                                  924
         AG_MODULE=MODULE                                                 924
         AG_MARK='V00'                                                    924
         CALL AGSPUSH                                                     924
         AG_MODULE=MODULE                                                 924
C    *                                                                    924
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            925
         CALL AGSRESET                                                    925
         AG_TITLE = 'MYLAR'                                               925
         AG_EXNAME = 'MATERIAL'                                           925
         AG_PARLIST = 'NONE'                                              925
         CALL AXMATERIAL                                                  925
C    *                                                                    925
C    Geant ATTRIBUTE : SFLB ===> SEEN=1 COLO=1 <===                       925
         CALL AGSRESET                                                    926
         AG_SEEN = 1                                                      926
         AG_COLO = 1                                                      926
         AG_TITLE = 'SFLB'                                                926
         AG_EXNAME = 'ATTRIBUTE'                                          926
         AG_PARLIST = 'SEEN_COLO'                                         926
         CALL AXATTRIBUTE                                                 926
C    *                                                                    926
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  926
C    JP_SFLB_DZ/2. <===                                                   926
         CALL AGSRESET                                                    928
         AG_DX = SFJP_SFLA_DX/2.                                          928
         AG_DY = SFJP_SFLA_DY/2.                                          928
         AG_DZ = SFJP_SFLB_DZ/2.                                          928
         AG_TITLE = 'BOX'                                                 928
         AG_EXNAME = 'SHAPE'                                              928
         AG_PARLIST = 'DX_DY_DZ'                                          928
         CALL AXSHAPE                                                     928
C    *                                                                    928
5611  CALL AGSPOPB                                                        929
      GO TO BLOCK_SFLB, (5610 ,5620)                                      929
C End of Local Procedure                                                  929
C ----------------------------------------------------------------------  930
C ------------ SFLC                                                       930
C Begin Local Procedure SFLC                                              932
C SFLC IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD UP TO,   932
C THE CONNECTOR                                                           932
5630  CONTINUE                                                            932
         AG_TITLE='SFLC'                                                  932
         AG_MODULE=MODULE                                                 932
         AG_MARK='V00'                                                    932
         CALL AGSPUSH                                                     932
         AG_MODULE=MODULE                                                 932
C    *                                                                    932
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            933
         CALL AGSRESET                                                    933
         AG_TITLE = 'MYLAR'                                               933
         AG_EXNAME = 'MATERIAL'                                           933
         AG_PARLIST = 'NONE'                                              933
         CALL AXMATERIAL                                                  933
C    *                                                                    933
C    Geant ATTRIBUTE : SFLC ===> SEEN=1 COLO=1 <===                       933
         CALL AGSRESET                                                    934
         AG_SEEN = 1                                                      934
         AG_COLO = 1                                                      934
         AG_TITLE = 'SFLC'                                                934
         AG_EXNAME = 'ATTRIBUTE'                                          934
         AG_PARLIST = 'SEEN_COLO'                                         934
         CALL AXATTRIBUTE                                                 934
C    *                                                                    934
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  934
C    JP_SFLC_DZ/2. <===                                                   934
         CALL AGSRESET                                                    936
         AG_DX = SFJP_SFLA_DX/2.                                          936
         AG_DY = SFJP_SFLA_DY/2.                                          936
         AG_DZ = SFJP_SFLC_DZ/2.                                          936
         AG_TITLE = 'BOX'                                                 936
         AG_EXNAME = 'SHAPE'                                              936
         AG_PARLIST = 'DX_DY_DZ'                                          936
         CALL AXSHAPE                                                     936
C    *                                                                    936
5631  CALL AGSPOPB                                                        937
      GO TO BLOCK_SFLC, (5630 ,5640)                                      937
C End of Local Procedure                                                  937
C ----------------------------------------------------------------------  938
C ------------ SFEB                                                       938
C Begin Local Procedure SFEB                                              939
C SFEB IS THE BIG BUS ELBOW                                               939
5650  CONTINUE                                                            939
         AG_TITLE='SFEB'                                                  939
         AG_MODULE=MODULE                                                 939
         AG_MARK='V00'                                                    939
         CALL AGSPUSH                                                     939
         AG_MODULE=MODULE                                                 939
C    *                                                                    939
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            940
         CALL AGSRESET                                                    940
         AG_TITLE = 'MYLAR'                                               940
         AG_EXNAME = 'MATERIAL'                                           940
         AG_PARLIST = 'NONE'                                              940
         CALL AXMATERIAL                                                  940
C    *                                                                    940
C    Geant ATTRIBUTE : SFEB ===> SEEN=1 COLO=1 <===                       940
         CALL AGSRESET                                                    941
         AG_SEEN = 1                                                      941
         AG_COLO = 1                                                      941
         AG_TITLE = 'SFEB'                                                941
         AG_EXNAME = 'ATTRIBUTE'                                          941
         AG_PARLIST = 'SEEN_COLO'                                         941
         CALL AXATTRIBUTE                                                 941
C    *                                                                    941
C    Geant SHAPE : BOX ===> DX=SFJP_SFEB_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  941
C    JP_SFEB_DZ/2. <===                                                   941
         CALL AGSRESET                                                    943
         AG_DX = SFJP_SFEB_DX/2.                                          943
         AG_DY = SFJP_SFLA_DY/2.                                          943
         AG_DZ = SFJP_SFEB_DZ/2.                                          943
         AG_TITLE = 'BOX'                                                 943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'DX_DY_DZ'                                          943
         CALL AXSHAPE                                                     943
C    *                                                                    943
5651  CALL AGSPOPB                                                        944
      GO TO BLOCK_SFEB, (5650 ,5660)                                      944
C End of Local Procedure                                                  944
C ----------------------------------------------------------------------  945
C ------------ SFES                                                       945
C Begin Local Procedure SFES                                              946
C SFES IS THE SMALL BUS ELBOW                                             946
5670  CONTINUE                                                            946
         AG_TITLE='SFES'                                                  946
         AG_MODULE=MODULE                                                 946
         AG_MARK='V00'                                                    946
         CALL AGSPUSH                                                     946
         AG_MODULE=MODULE                                                 946
C    *                                                                    946
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            947
         CALL AGSRESET                                                    947
         AG_TITLE = 'MYLAR'                                               947
         AG_EXNAME = 'MATERIAL'                                           947
         AG_PARLIST = 'NONE'                                              947
         CALL AXMATERIAL                                                  947
C    *                                                                    947
C    Geant ATTRIBUTE : SFES ===> SEEN=1 COLO=1 <===                       947
         CALL AGSRESET                                                    948
         AG_SEEN = 1                                                      948
         AG_COLO = 1                                                      948
         AG_TITLE = 'SFES'                                                948
         AG_EXNAME = 'ATTRIBUTE'                                          948
         AG_PARLIST = 'SEEN_COLO'                                         948
         CALL AXATTRIBUTE                                                 948
C    *                                                                    948
C    Geant SHAPE : BOX ===> DX=SFJP_SFES_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  948
C    JP_SFEB_DZ/2. <===                                                   948
         CALL AGSRESET                                                    950
         AG_DX = SFJP_SFES_DX/2.                                          950
         AG_DY = SFJP_SFLA_DY/2.                                          950
         AG_DZ = SFJP_SFEB_DZ/2.                                          950
         AG_TITLE = 'BOX'                                                 950
         AG_EXNAME = 'SHAPE'                                              950
         AG_PARLIST = 'DX_DY_DZ'                                          950
         CALL AXSHAPE                                                     950
C    *                                                                    950
5671  CALL AGSPOPB                                                        951
      GO TO BLOCK_SFES, (5670 ,5680)                                      951
C End of Local Procedure                                                  951
C ----------------------------------------------------------------------  952
C ------------ SFAM                                                       952
C Begin Local Procedure SFAM                                              953
C SFAM IS THE MOTHER VOLUME OF THE ADC BOARD                              953
5410  CONTINUE                                                            953
         AG_TITLE='SFAM'                                                  953
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
C    Geant ATTRIBUTE : SFAM ===> SEEN=0 COLO=6 <===                       954
         CALL AGSRESET                                                    955
         AG_SEEN = 0                                                      955
         AG_COLO = 6                                                      955
         AG_TITLE = 'SFAM'                                                955
         AG_EXNAME = 'ATTRIBUTE'                                          955
         AG_PARLIST = 'SEEN_COLO'                                         955
         CALL AXATTRIBUTE                                                 955
C    *                                                                    955
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=S  955
C    FJP_SFAM_DZ/2 <===                                                   955
         CALL AGSRESET                                                    957
         AG_DX = SFJP_SFAM_DXE/2.                                         957
         AG_DY = SFJP_SFAM_DY/2.                                          957
         AG_DZ = SFJP_SFAM_DZ/2                                           957
         AG_TITLE = 'BOX'                                                 957
         AG_EXNAME = 'SHAPE'                                              957
         AG_PARLIST = 'DX_DY_DZ'                                          957
         CALL AXSHAPE                                                     957
C    *                                                                    957
C    Create SFAB :                                                        958
C    Call To Local Procedure SFAB                                         958
         ASSIGN 5910 TO BLOCK_SFAB                                        958
         GO TO  5900                                                      958
5910     CONTINUE                                                         958
C    *                                                                    958
C    Geant POSITION : SFAB ===> X=0. Y=0. Z=SFJP_SFAM_DZS/2 <===          959
         CALL AGSRESET                                                    959
         AG_X = 0.                                                        959
         AG_Y = 0.                                                        959
         AG_Z = SFJP_SFAM_DZS/2                                           959
         AG_TITLE = 'SFAB'                                                959
         AG_EXNAME = 'POSITION'                                           959
         AG_PARLIST = 'X_Y_Z'                                             959
         CALL AXPOSITION                                                  959
C    *                                                                    959
C    Create SFAS :                                                        960
C    Call To Local Procedure SFAS                                         960
         ASSIGN 5930 TO BLOCK_SFAS                                        960
         GO TO  5920                                                      960
5930     CONTINUE                                                         960
C    *                                                                    960
C    Geant POSITION : SFAS ===> X=(SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2. Y=0.   962
C    Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                               962
         CALL AGSRESET                                                    962
         AG_X = (SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2.                          962
         AG_Y = 0.                                                        962
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           962
         AG_TITLE = 'SFAS'                                                962
         AG_EXNAME = 'POSITION'                                           962
         AG_PARLIST = 'X_Y_Z'                                             962
         CALL AXPOSITION                                                  962
C    *                                                                    962
C    Geant POSITION : SFAS ===> X=(-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2. Y=0.  962
C     Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                              962
         CALL AGSRESET                                                    964
         AG_X = (-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2.                         964
         AG_Y = 0.                                                        964
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           964
         AG_TITLE = 'SFAS'                                                964
         AG_EXNAME = 'POSITION'                                           964
         AG_PARLIST = 'X_Y_Z'                                             964
         CALL AXPOSITION                                                  964
C    *                                                                    964
5411  CALL AGSPOPB                                                        965
      GO TO BLOCK_SFAM, (5410 ,5420)                                      965
C End of Local Procedure                                                  965
C ----------------------------------------------------------------------  966
C ------------ SFAB                                                       966
C Begin Local Procedure SFAB                                              967
C SFAB IS THE BIG VOLUME OF THE ADC BOARD                                 967
5900  CONTINUE                                                            967
         AG_TITLE='SFAB'                                                  967
         AG_MODULE=MODULE                                                 967
         AG_MARK='V00'                                                    967
         CALL AGSPUSH                                                     967
         AG_MODULE=MODULE                                                 967
C    *                                                                    967
C    Geant MATERIAL : G10 ===> inherited arguments used <===              968
         CALL AGSRESET                                                    968
         AG_TITLE = 'G10'                                                 968
         AG_EXNAME = 'MATERIAL'                                           968
         AG_PARLIST = 'NONE'                                              968
         CALL AXMATERIAL                                                  968
C    *                                                                    968
C    Geant ATTRIBUTE : SFAB ===> SEEN=1 COLO=30 <===                      968
         CALL AGSRESET                                                    969
         AG_SEEN = 1                                                      969
         AG_COLO = 30                                                     969
         AG_TITLE = 'SFAB'                                                969
         AG_EXNAME = 'ATTRIBUTE'                                          969
         AG_PARLIST = 'SEEN_COLO'                                         969
         CALL AXATTRIBUTE                                                 969
C    *                                                                    969
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=(  969
C    SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2 <===                                   969
         CALL AGSRESET                                                    971
         AG_DX = SFJP_SFAM_DXE/2.                                         971
         AG_DY = SFJP_SFAM_DY/2.                                          971
         AG_DZ = (SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2                           971
         AG_TITLE = 'BOX'                                                 971
         AG_EXNAME = 'SHAPE'                                              971
         AG_PARLIST = 'DX_DY_DZ'                                          971
         CALL AXSHAPE                                                     971
C    *                                                                    971
5901  CALL AGSPOPB                                                        972
      GO TO BLOCK_SFAB, (5900 ,5910)                                      972
C End of Local Procedure                                                  972
C ----------------------------------------------------------------------  973
C ------------ SFAS                                                       973
C Begin Local Procedure SFAS                                              974
C SFAS IS THE SMALL VOLUME OF THE ADC BOARD                               974
5920  CONTINUE                                                            974
         AG_TITLE='SFAS'                                                  974
         AG_MODULE=MODULE                                                 974
         AG_MARK='V00'                                                    974
         CALL AGSPUSH                                                     974
         AG_MODULE=MODULE                                                 974
C    *                                                                    974
C    Geant MATERIAL : G10 ===> inherited arguments used <===              975
         CALL AGSRESET                                                    975
         AG_TITLE = 'G10'                                                 975
         AG_EXNAME = 'MATERIAL'                                           975
         AG_PARLIST = 'NONE'                                              975
         CALL AXMATERIAL                                                  975
C    *                                                                    975
C    Geant ATTRIBUTE : SFAS ===> SEEN=1 COLO=30 <===                      975
         CALL AGSRESET                                                    976
         AG_SEEN = 1                                                      976
         AG_COLO = 30                                                     976
         AG_TITLE = 'SFAS'                                                976
         AG_EXNAME = 'ATTRIBUTE'                                          976
         AG_PARLIST = 'SEEN_COLO'                                         976
         CALL AXATTRIBUTE                                                 976
C    *                                                                    976
C    Geant SHAPE : BOX ===> DX=(SFJP_SFAM_DXZ)/2. DY=(SFJP_SFAM_DY)/2.,   976
C    DZ=(SFJP_SFAM_DZS)/2 <===                                            976
         CALL AGSRESET                                                    978
         AG_DX = (SFJP_SFAM_DXZ)/2.                                       978
         AG_DY = (SFJP_SFAM_DY)/2.                                        978
         AG_DZ = (SFJP_SFAM_DZS)/2                                        978
         AG_TITLE = 'BOX'                                                 978
         AG_EXNAME = 'SHAPE'                                              978
         AG_PARLIST = 'DX_DY_DZ'                                          978
         CALL AXSHAPE                                                     978
C    *                                                                    978
5921  CALL AGSPOPB                                                        979
      GO TO BLOCK_SFAS, (5920 ,5930)                                      979
C End of Local Procedure                                                  979
C ----------------------------------------------------------------------  980
C ------------ SAPP                                                       980
C Begin Local Procedure SAPP                                              981
C SAPP IS THE MOTHER VOLUME OF THE ADC BOARD APPENDICE                    981
5390  CONTINUE                                                            981
         AG_TITLE='SAPP'                                                  981
         AG_MODULE=MODULE                                                 981
         AG_MARK='V00'                                                    981
         CALL AGSPUSH                                                     981
         AG_MODULE=MODULE                                                 981
C    *                                                                    981
C    Geant MATERIAL : AIR ===> inherited arguments used <===              982
         CALL AGSRESET                                                    982
         AG_TITLE = 'AIR'                                                 982
         AG_EXNAME = 'MATERIAL'                                           982
         AG_PARLIST = 'NONE'                                              982
         CALL AXMATERIAL                                                  982
C    *                                                                    982
C    Geant ATTRIBUTE : SAPP ===> SEEN=0 COLO=6 <===                       982
         CALL AGSRESET                                                    983
         AG_SEEN = 0                                                      983
         AG_COLO = 6                                                      983
         AG_TITLE = 'SAPP'                                                983
         AG_EXNAME = 'ATTRIBUTE'                                          983
         AG_PARLIST = 'SEEN_COLO'                                         983
         CALL AXATTRIBUTE                                                 983
C    *                                                                    983
C    Geant SHAPE : BOX ===> DX=(SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2., DY=(S  983
C    FJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===             983
         CALL AGSRESET                                                    986
         AG_DX = (SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2.                       986
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        986
         AG_DZ = SFJP_SAPP_DZ/2.                                          986
         AG_TITLE = 'BOX'                                                 986
         AG_EXNAME = 'SHAPE'                                              986
         AG_PARLIST = 'DX_DY_DZ'                                          986
         CALL AXSHAPE                                                     986
C    *                                                                    986
C    Create SAPC :                                                        987
C    Call To Local Procedure SAPC                                         987
         ASSIGN 5950 TO BLOCK_SAPC                                        987
         GO TO  5940                                                      987
5950     CONTINUE                                                         987
C    *                                                                    987
C    Geant POSITION : SAPC ===> X=0. Y=0. Z=(-SFJP_SAPP_DZ+SFJP_SAPP_DY)  988
C    /2. <===                                                             988
         CALL AGSRESET                                                    988
         AG_X = 0.                                                        988
         AG_Y = 0.                                                        988
         AG_Z = (-SFJP_SAPP_DZ+SFJP_SAPP_DY)/2.                           988
         AG_TITLE = 'SAPC'                                                988
         AG_EXNAME = 'POSITION'                                           988
         AG_PARLIST = 'X_Y_Z'                                             988
         CALL AXPOSITION                                                  988
C    *                                                                    988
C    Create SAPS :                                                        989
C    Call To Local Procedure SAPS                                         989
         ASSIGN 5970 TO BLOCK_SAPS                                        989
         GO TO  5960                                                      989
5970     CONTINUE                                                         989
C    *                                                                    989
C    Geant POSITION : SAPS ===> X=SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2 Y=0 Z  990
C    =0. <===                                                             990
         CALL AGSRESET                                                    990
         AG_X = SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2                          990
         AG_Y = 0                                                         990
         AG_Z = 0.                                                        990
         AG_TITLE = 'SAPS'                                                990
         AG_EXNAME = 'POSITION'                                           990
         AG_PARLIST = 'X_Y_Z'                                             990
         CALL AXPOSITION                                                  990
C    *                                                                    990
C    Geant POSITION : SAPS ===> X=-(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2) Y=  990
C    0 Z=0. <===                                                          990
         CALL AGSRESET                                                    991
         AG_X = -(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2)                       991
         AG_Y = 0                                                         991
         AG_Z = 0.                                                        991
         AG_TITLE = 'SAPS'                                                991
         AG_EXNAME = 'POSITION'                                           991
         AG_PARLIST = 'X_Y_Z'                                             991
         CALL AXPOSITION                                                  991
C    *                                                                    991
C    Create SAPT :                                                        992
C    Call To Local Procedure SAPT                                         992
         ASSIGN 5990 TO BLOCK_SAPT                                        992
         GO TO  5980                                                      992
5990     CONTINUE                                                         992
C    *                                                                    992
C    Geant POSITION : SAPT ===> X=0. Y=(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2. Z  993
C    =0. <===                                                             993
         CALL AGSRESET                                                    993
         AG_X = 0.                                                        993
         AG_Y = (SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                           993
         AG_Z = 0.                                                        993
         AG_TITLE = 'SAPT'                                                993
         AG_EXNAME = 'POSITION'                                           993
         AG_PARLIST = 'X_Y_Z'                                             993
         CALL AXPOSITION                                                  993
C    *                                                                    993
C    Geant POSITION : SAPT ===> X=0. Y=-(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.   993
C    Z=0. <===                                                            993
         CALL AGSRESET                                                    994
         AG_X = 0.                                                        994
         AG_Y = -(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                          994
         AG_Z = 0.                                                        994
         AG_TITLE = 'SAPT'                                                994
         AG_EXNAME = 'POSITION'                                           994
         AG_PARLIST = 'X_Y_Z'                                             994
         CALL AXPOSITION                                                  994
C    *                                                                    994
5391  CALL AGSPOPB                                                        995
      GO TO BLOCK_SAPP, (5390 ,5400)                                      995
C End of Local Procedure                                                  995
C ----------------------------------------------------------------------  996
C ------------ SAPC                                                       996
C Begin Local Procedure SAPC                                              997
C SAPC IS THE CORE (EPOXY) OF THE ADC BOARD APPENDICE                     997
5940  CONTINUE                                                            997
         AG_TITLE='SAPC'                                                  997
         AG_MODULE=MODULE                                                 997
         AG_MARK='V00'                                                    997
         CALL AGSPUSH                                                     997
         AG_MODULE=MODULE                                                 997
C    *                                                                    997
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           998
         CALL AGSRESET                                                    998
         AG_TITLE = 'CARBON'                                              998
         AG_EXNAME = 'MATERIAL'                                           998
         AG_PARLIST = 'NONE'                                              998
         CALL AXMATERIAL                                                  998
C    *                                                                    998
C    Geant ATTRIBUTE : SAPC ===> SEEN=1 COLO=1 <===                       998
         CALL AGSRESET                                                    999
         AG_SEEN = 1                                                      999
         AG_COLO = 1                                                      999
         AG_TITLE = 'SAPC'                                                999
         AG_EXNAME = 'ATTRIBUTE'                                          999
         AG_PARLIST = 'SEEN_COLO'                                         999
         CALL AXATTRIBUTE                                                 999
C    *                                                                    999
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DY/2., DZ=S  999
C    FJP_SAPP_DY/2. <===                                                  999
         CALL AGSRESET                                                    1001
         AG_DX = SFJP_SAPP_DXE/2.                                         1001
         AG_DY = SFJP_SAPP_DY/2.                                          1001
         AG_DZ = SFJP_SAPP_DY/2.                                          1001
         AG_TITLE = 'BOX'                                                 1001
         AG_EXNAME = 'SHAPE'                                              1001
         AG_PARLIST = 'DX_DY_DZ'                                          1001
         CALL AXSHAPE                                                     1001
C    *                                                                    1001
5941  CALL AGSPOPB                                                        1002
      GO TO BLOCK_SAPC, (5940 ,5950)                                      1002
C End of Local Procedure                                                  1002
C ----------------------------------------------------------------------  1003
C ------------ SAPS                                                       1003
C Begin Local Procedure SAPS                                              1004
C SAPS IS THE SIDE SHELL (CARBON) OF THE ADC BOARD APPENDICE              1004
5960  CONTINUE                                                            1004
         AG_TITLE='SAPS'                                                  1004
         AG_MODULE=MODULE                                                 1004
         AG_MARK='V00'                                                    1004
         CALL AGSPUSH                                                     1004
         AG_MODULE=MODULE                                                 1004
C    *                                                                    1004
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1005
         CALL AGSRESET                                                    1005
         AG_TITLE = 'CARBON'                                              1005
         AG_EXNAME = 'MATERIAL'                                           1005
         AG_PARLIST = 'NONE'                                              1005
         CALL AXMATERIAL                                                  1005
C    *                                                                    1005
C    Geant ATTRIBUTE : SAPS ===> SEEN=1 COLO=1 <===                       1005
         CALL AGSRESET                                                    1006
         AG_SEEN = 1                                                      1006
         AG_COLO = 1                                                      1006
         AG_TITLE = 'SAPS'                                                1006
         AG_EXNAME = 'ATTRIBUTE'                                          1006
         AG_PARLIST = 'SEEN_COLO'                                         1006
         CALL AXATTRIBUTE                                                 1006
C    *                                                                    1006
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXZ/2. DY=(SFJP_SAPP_DY+2*SFJP_  1006
C    SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===                                1006
         CALL AGSRESET                                                    1008
         AG_DX = SFJP_SAPP_DXZ/2.                                         1008
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        1008
         AG_DZ = SFJP_SAPP_DZ/2.                                          1008
         AG_TITLE = 'BOX'                                                 1008
         AG_EXNAME = 'SHAPE'                                              1008
         AG_PARLIST = 'DX_DY_DZ'                                          1008
         CALL AXSHAPE                                                     1008
C    *                                                                    1008
5961  CALL AGSPOPB                                                        1009
      GO TO BLOCK_SAPS, (5960 ,5970)                                      1009
C End of Local Procedure                                                  1009
C ----------------------------------------------------------------------  1010
C ------------ SAPT                                                       1010
C Begin Local Procedure SAPT                                              1011
C SAPT IS THE TOP-BOTTOM SHELL (CARBON) OF THE ADC BOARD APPENDICE        1011
5980  CONTINUE                                                            1011
         AG_TITLE='SAPT'                                                  1011
         AG_MODULE=MODULE                                                 1011
         AG_MARK='V00'                                                    1011
         CALL AGSPUSH                                                     1011
         AG_MODULE=MODULE                                                 1011
C    *                                                                    1011
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1012
         CALL AGSRESET                                                    1012
         AG_TITLE = 'CARBON'                                              1012
         AG_EXNAME = 'MATERIAL'                                           1012
         AG_PARLIST = 'NONE'                                              1012
         CALL AXMATERIAL                                                  1012
C    *                                                                    1012
C    Geant ATTRIBUTE : SAPT ===> SEEN=1 COLO=1 <===                       1012
         CALL AGSRESET                                                    1013
         AG_SEEN = 1                                                      1013
         AG_COLO = 1                                                      1013
         AG_TITLE = 'SAPT'                                                1013
         AG_EXNAME = 'ATTRIBUTE'                                          1013
         AG_PARLIST = 'SEEN_COLO'                                         1013
         CALL AXATTRIBUTE                                                 1013
C    *                                                                    1013
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DXZ/2., DZ=  1013
C    SFJP_SAPP_DZ/2. <===                                                 1013
         CALL AGSRESET                                                    1015
         AG_DX = SFJP_SAPP_DXE/2.                                         1015
         AG_DY = SFJP_SAPP_DXZ/2.                                         1015
         AG_DZ = SFJP_SAPP_DZ/2.                                          1015
         AG_TITLE = 'BOX'                                                 1015
         AG_EXNAME = 'SHAPE'                                              1015
         AG_PARLIST = 'DX_DY_DZ'                                          1015
         CALL AXSHAPE                                                     1015
C    *                                                                    1015
5981  CALL AGSPOPB                                                        1016
      GO TO BLOCK_SAPT, (5980 ,5990)                                      1016
C End of Local Procedure                                                  1016
C ----------------------------------------------------------------------  1017
C ------------ SFCO                                                       1017
C Begin Local Procedure SFCO                                              1018
C SFCO IS THE CONNECTION BOARD (RECTANGULAR WITH HIROSE CONNECTORS)       1018
5430  CONTINUE                                                            1018
         AG_TITLE='SFCO'                                                  1018
         AG_MODULE=MODULE                                                 1018
         AG_MARK='V00'                                                    1018
         CALL AGSPUSH                                                     1018
         AG_MODULE=MODULE                                                 1018
C    *                                                                    1018
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1019
         CALL AGSRESET                                                    1019
         AG_TITLE = 'G10'                                                 1019
         AG_EXNAME = 'MATERIAL'                                           1019
         AG_PARLIST = 'NONE'                                              1019
         CALL AXMATERIAL                                                  1019
C    *                                                                    1019
C    Geant ATTRIBUTE : SFCO ===> SEEN=1 COLO=30 <===                      1019
         CALL AGSRESET                                                    1020
         AG_SEEN = 1                                                      1020
         AG_COLO = 30                                                     1020
         AG_TITLE = 'SFCO'                                                1020
         AG_EXNAME = 'ATTRIBUTE'                                          1020
         AG_PARLIST = 'SEEN_COLO'                                         1020
         CALL AXATTRIBUTE                                                 1020
C    *                                                                    1020
C    Geant SHAPE : BOX ===> DX=SFJP_SFCO_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  1020
C    JP_SFCO_DZ/2 <===                                                    1020
         CALL AGSRESET                                                    1022
         AG_DX = SFJP_SFCO_DX/2.                                          1022
         AG_DY = SFJP_SFCO_DY/2.                                          1022
         AG_DZ = SFJP_SFCO_DZ/2                                           1022
         AG_TITLE = 'BOX'                                                 1022
         AG_EXNAME = 'SHAPE'                                              1022
         AG_PARLIST = 'DX_DY_DZ'                                          1022
         CALL AXSHAPE                                                     1022
C    *                                                                    1022
5431  CALL AGSPOPB                                                        1023
      GO TO BLOCK_SFCO, (5430 ,5440)                                      1023
C End of Local Procedure                                                  1023
C ----------------------------------------------------------------------  1024
C ------------ SFCM                                                       1024
C Begin Local Procedure SFCM                                              1025
C SFCM IS THE MOTHER VOLUME OF THE SECOND CONNECTION BOARD                1025
5450  CONTINUE                                                            1025
         AG_TITLE='SFCM'                                                  1025
         AG_MODULE=MODULE                                                 1025
         AG_MARK='V00'                                                    1025
         CALL AGSPUSH                                                     1025
         AG_MODULE=MODULE                                                 1025
C    *                                                                    1025
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1026
         CALL AGSRESET                                                    1026
         AG_TITLE = 'AIR'                                                 1026
         AG_EXNAME = 'MATERIAL'                                           1026
         AG_PARLIST = 'NONE'                                              1026
         CALL AXMATERIAL                                                  1026
C    *                                                                    1026
C    Geant ATTRIBUTE : SFCM ===> SEEN=0 COLO=6 <===                       1026
         CALL AGSRESET                                                    1027
         AG_SEEN = 0                                                      1027
         AG_COLO = 6                                                      1027
         AG_TITLE = 'SFCM'                                                1027
         AG_EXNAME = 'ATTRIBUTE'                                          1027
         AG_PARLIST = 'SEEN_COLO'                                         1027
         CALL AXATTRIBUTE                                                 1027
C    *                                                                    1027
C    Geant SHAPE : BOX ===> DX=SFJP_SFCM_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  1027
C    JP_SFCO_DZ/2 <===                                                    1027
         CALL AGSRESET                                                    1029
         AG_DX = SFJP_SFCM_DX/2.                                          1029
         AG_DY = SFJP_SFCO_DY/2.                                          1029
         AG_DZ = SFJP_SFCO_DZ/2                                           1029
         AG_TITLE = 'BOX'                                                 1029
         AG_EXNAME = 'SHAPE'                                              1029
         AG_PARLIST = 'DX_DY_DZ'                                          1029
         CALL AXSHAPE                                                     1029
C    *                                                                    1029
C    Create SFCB :                                                        1030
C    Call To Local Procedure SFCB                                         1030
         ASSIGN 6010 TO BLOCK_SFCB                                        1030
         GO TO  6000                                                      1030
6010     CONTINUE                                                         1030
C    *                                                                    1030
C    Geant POSITION : SFCB ===> X=+0.5 Y=0 Z=0 <===                       1031
         CALL AGSRESET                                                    1031
         AG_X = +0.5                                                      1031
         AG_Y = 0                                                         1031
         AG_Z = 0                                                         1031
         AG_TITLE = 'SFCB'                                                1031
         AG_EXNAME = 'POSITION'                                           1031
         AG_PARLIST = 'X_Y_Z'                                             1031
         CALL AXPOSITION                                                  1031
C    *                                                                    1031
C    Create SFCS :                                                        1032
C    Call To Local Procedure SFCS                                         1032
         ASSIGN 6030 TO BLOCK_SFCS                                        1032
         GO TO  6020                                                      1032
6030     CONTINUE                                                         1032
C    *                                                                    1032
C    Geant POSITION : SFCS ===> X=-1.5 Y=0 Z=-SFJP_SFCO_DZ/2+SFJP_SFCS_D  1033
C    Z/2 <===                                                             1033
         CALL AGSRESET                                                    1033
         AG_X = -1.5                                                      1033
         AG_Y = 0                                                         1033
         AG_Z = -SFJP_SFCO_DZ/2+SFJP_SFCS_DZ/2                            1033
         AG_TITLE = 'SFCS'                                                1033
         AG_EXNAME = 'POSITION'                                           1033
         AG_PARLIST = 'X_Y_Z'                                             1033
         CALL AXPOSITION                                                  1033
C    *                                                                    1033
5451  CALL AGSPOPB                                                        1034
      GO TO BLOCK_SFCM, (5450 ,5460)                                      1034
C End of Local Procedure                                                  1034
C ----------------------------------------------------------------------  1035
C ------------ SFCB                                                       1035
C Begin Local Procedure SFCB                                              1036
C SFCB IS THE BIG PART OF THE SECOND CONNECTION BOARD                     1036
6000  CONTINUE                                                            1036
         AG_TITLE='SFCB'                                                  1036
         AG_MODULE=MODULE                                                 1036
         AG_MARK='V00'                                                    1036
         CALL AGSPUSH                                                     1036
         AG_MODULE=MODULE                                                 1036
C    *                                                                    1036
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1037
         CALL AGSRESET                                                    1037
         AG_TITLE = 'G10'                                                 1037
         AG_EXNAME = 'MATERIAL'                                           1037
         AG_PARLIST = 'NONE'                                              1037
         CALL AXMATERIAL                                                  1037
C    *                                                                    1037
C    Geant ATTRIBUTE : SFCB ===> SEEN=1 COLO=30 <===                      1037
         CALL AGSRESET                                                    1038
         AG_SEEN = 1                                                      1038
         AG_COLO = 30                                                     1038
         AG_TITLE = 'SFCB'                                                1038
         AG_EXNAME = 'ATTRIBUTE'                                          1038
         AG_PARLIST = 'SEEN_COLO'                                         1038
         CALL AXATTRIBUTE                                                 1038
C    *                                                                    1038
C    Geant SHAPE : BOX ===> DX=SFJP_SFCB_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  1038
C    JP_SFCO_DZ/2 <===                                                    1038
         CALL AGSRESET                                                    1040
         AG_DX = SFJP_SFCB_DX/2.                                          1040
         AG_DY = SFJP_SFCO_DY/2.                                          1040
         AG_DZ = SFJP_SFCO_DZ/2                                           1040
         AG_TITLE = 'BOX'                                                 1040
         AG_EXNAME = 'SHAPE'                                              1040
         AG_PARLIST = 'DX_DY_DZ'                                          1040
         CALL AXSHAPE                                                     1040
C    *                                                                    1040
6001  CALL AGSPOPB                                                        1041
      GO TO BLOCK_SFCB, (6000 ,6010)                                      1041
C End of Local Procedure                                                  1041
C ----------------------------------------------------------------------  1042
C ------------ SFCS                                                       1042
C Begin Local Procedure SFCS                                              1043
C SFCS IS THE BIG PART OF THE SECOND CONNECTION BOARD                     1043
6020  CONTINUE                                                            1043
         AG_TITLE='SFCS'                                                  1043
         AG_MODULE=MODULE                                                 1043
         AG_MARK='V00'                                                    1043
         CALL AGSPUSH                                                     1043
         AG_MODULE=MODULE                                                 1043
C    *                                                                    1043
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1044
         CALL AGSRESET                                                    1044
         AG_TITLE = 'G10'                                                 1044
         AG_EXNAME = 'MATERIAL'                                           1044
         AG_PARLIST = 'NONE'                                              1044
         CALL AXMATERIAL                                                  1044
C    *                                                                    1044
C    Geant ATTRIBUTE : SFCS ===> SEEN=1 COLO=30 <===                      1044
         CALL AGSRESET                                                    1045
         AG_SEEN = 1                                                      1045
         AG_COLO = 30                                                     1045
         AG_TITLE = 'SFCS'                                                1045
         AG_EXNAME = 'ATTRIBUTE'                                          1045
         AG_PARLIST = 'SEEN_COLO'                                         1045
         CALL AXATTRIBUTE                                                 1045
C    *                                                                    1045
C    Geant SHAPE : BOX ===> DX=(SFJP_SFCM_DX-SFJP_SFCB_DX)/2., DY=SFJP_S  1045
C    FCO_DY/2. DZ=SFJP_SFCS_DZ/2 <===                                     1045
         CALL AGSRESET                                                    1047
         AG_DX = (SFJP_SFCM_DX-SFJP_SFCB_DX)/2.                           1047
         AG_DY = SFJP_SFCO_DY/2.                                          1047
         AG_DZ = SFJP_SFCS_DZ/2                                           1047
         AG_TITLE = 'BOX'                                                 1047
         AG_EXNAME = 'SHAPE'                                              1047
         AG_PARLIST = 'DX_DY_DZ'                                          1047
         CALL AXSHAPE                                                     1047
C    *                                                                    1047
6021  CALL AGSPOPB                                                        1048
      GO TO BLOCK_SFCS, (6020 ,6030)                                      1048
C End of Local Procedure                                                  1048
C ----------------------------------------------------------------------  1049
C ------------ SFKF                                                       1049
C Begin Local Procedure SFKF                                              1050
C SFKF IS THE FIRST PART OF THE KAPTON FLEX CIRCUIT                       1050
5470  CONTINUE                                                            1050
         AG_TITLE='SFKF'                                                  1050
         AG_MODULE=MODULE                                                 1050
         AG_MARK='V00'                                                    1050
         CALL AGSPUSH                                                     1050
         AG_MODULE=MODULE                                                 1050
C    *                                                                    1050
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1051
         CALL AGSRESET                                                    1051
         AG_TITLE = 'G10'                                                 1051
         AG_EXNAME = 'MATERIAL'                                           1051
         AG_PARLIST = 'NONE'                                              1051
         CALL AXMATERIAL                                                  1051
C    *                                                                    1051
C    Geant ATTRIBUTE : SFKF ===> SEEN=1 COLO=30 <===                      1051
         CALL AGSRESET                                                    1052
         AG_SEEN = 1                                                      1052
         AG_COLO = 30                                                     1052
         AG_TITLE = 'SFKF'                                                1052
         AG_EXNAME = 'ATTRIBUTE'                                          1052
         AG_PARLIST = 'SEEN_COLO'                                         1052
         CALL AXATTRIBUTE                                                 1052
C    *                                                                    1052
C    Geant SHAPE : BOX ===> DX=SFJP_SFKF_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1052
C    JP_SFKF_DZ/2 <===                                                    1052
         CALL AGSRESET                                                    1054
         AG_DX = SFJP_SFKF_DX/2.                                          1054
         AG_DY = SFJP_SFKF_DY/2.                                          1054
         AG_DZ = SFJP_SFKF_DZ/2                                           1054
         AG_TITLE = 'BOX'                                                 1054
         AG_EXNAME = 'SHAPE'                                              1054
         AG_PARLIST = 'DX_DY_DZ'                                          1054
         CALL AXSHAPE                                                     1054
C    *                                                                    1054
5471  CALL AGSPOPB                                                        1055
      GO TO BLOCK_SFKF, (5470 ,5480)                                      1055
C End of Local Procedure                                                  1055
C ----------------------------------------------------------------------  1056
C ------------ SFKS                                                       1056
C Begin Local Procedure SFKS                                              1057
C SFKS IS THE SECOND PART OF THE KAPTON FLEX CIRCUIT                      1057
5490  CONTINUE                                                            1057
         AG_TITLE='SFKS'                                                  1057
         AG_MODULE=MODULE                                                 1057
         AG_MARK='V00'                                                    1057
         CALL AGSPUSH                                                     1057
         AG_MODULE=MODULE                                                 1057
C    *                                                                    1057
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1058
         CALL AGSRESET                                                    1058
         AG_TITLE = 'G10'                                                 1058
         AG_EXNAME = 'MATERIAL'                                           1058
         AG_PARLIST = 'NONE'                                              1058
         CALL AXMATERIAL                                                  1058
C    *                                                                    1058
C    Geant ATTRIBUTE : SFKS ===> SEEN=1 COLO=30 <===                      1058
         CALL AGSRESET                                                    1059
         AG_SEEN = 1                                                      1059
         AG_COLO = 30                                                     1059
         AG_TITLE = 'SFKS'                                                1059
         AG_EXNAME = 'ATTRIBUTE'                                          1059
         AG_PARLIST = 'SEEN_COLO'                                         1059
         CALL AXATTRIBUTE                                                 1059
C    *                                                                    1059
C    Geant SHAPE : BOX ===> DX=SFJP_SFKS_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1059
C    JP_SFKF_DZ/2 <===                                                    1059
         CALL AGSRESET                                                    1061
         AG_DX = SFJP_SFKS_DX/2.                                          1061
         AG_DY = SFJP_SFKF_DY/2.                                          1061
         AG_DZ = SFJP_SFKF_DZ/2                                           1061
         AG_TITLE = 'BOX'                                                 1061
         AG_EXNAME = 'SHAPE'                                              1061
         AG_PARLIST = 'DX_DY_DZ'                                          1061
         CALL AXSHAPE                                                     1061
C    *                                                                    1061
5491  CALL AGSPOPB                                                        1062
      GO TO BLOCK_SFKS, (5490 ,5500)                                      1062
C End of Local Procedure                                                  1062
C ----------------------------------------------------------------------  1063
C ------------ SFPR                                                       1063
C Begin Local Procedure SFPR                                              1065
C SFPR IS THE LADDER END INSIDE MECHANICAL PART (PRISM WITH G10 WITH, HA  1065
C LF DENSITY)                                                             1065
5510  CONTINUE                                                            1065
         AG_TITLE='SFPR'                                                  1065
         AG_MODULE=MODULE                                                 1065
         AG_MARK='V00'                                                    1065
         CALL AGSPUSH                                                     1065
         AG_MODULE=MODULE                                                 1065
C    *                                                                    1065
C    Geant MATERIAL : G5 ===> inherited arguments used <===               1066
         CALL AGSRESET                                                    1066
         AG_TITLE = 'G5'                                                  1066
         AG_EXNAME = 'MATERIAL'                                           1066
         AG_PARLIST = 'NONE'                                              1066
         CALL AXMATERIAL                                                  1066
C    *                                                                    1066
C    Geant ATTRIBUTE : SFPR ===> SEEN=1 COLO=4 <===                       1066
         CALL AGSRESET                                                    1067
         AG_SEEN = 1                                                      1067
         AG_COLO = 4                                                      1067
         AG_TITLE = 'SFPR'                                                1067
         AG_EXNAME = 'ATTRIBUTE'                                          1067
         AG_PARLIST = 'SEEN_COLO'                                         1067
         CALL AXATTRIBUTE                                                 1067
C    *                                                                    1067
C    Geant SHAPE : PGON ===> PHI1=-126 DPHI=72 NPDIV=1 NZ=2, ZI={0,3.7}   1067
C    RMN={0,0} RMX={3.48,3.48} <===                                       1067
         CALL AGSRESET                                                    1069
         AG_PHI1 = -126                                                   1069
         AG_DPHI = 72                                                     1069
         AG_NPDIV = 1                                                     1069
         AG_NZ = 2                                                        1069
         AG_ZI(1)=0                                                       1069
         AG_ZI(2)=3.7                                                     1069
         AG_RMN(1)=0                                                      1069
         AG_RMN(2)=0                                                      1069
         AG_RMX(1)=3.48                                                   1069
         AG_RMX(2)=3.48                                                   1069
         AG_TITLE = 'PGON'                                                1069
         AG_EXNAME = 'SHAPE'                                              1069
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1069
         CALL AXSHAPE                                                     1069
C    *                                                                    1069
5511  CALL AGSPOPB                                                        1070
      GO TO BLOCK_SFPR, (5510 ,5520)                                      1070
C End of Local Procedure                                                  1070
C ----------------------------------------------------------------------  1071
C ------------ SFPB                                                       1071
C Begin Local Procedure SFPB                                              1072
C SFPB IS THE LADDER END OUTSIDE MECHANICAL PART (RECTANGLE WITH G10)     1072
5530  CONTINUE                                                            1072
         AG_TITLE='SFPB'                                                  1072
         AG_MODULE=MODULE                                                 1072
         AG_MARK='V00'                                                    1072
         CALL AGSPUSH                                                     1072
         AG_MODULE=MODULE                                                 1072
C    *                                                                    1072
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1073
         CALL AGSRESET                                                    1073
         AG_TITLE = 'G10'                                                 1073
         AG_EXNAME = 'MATERIAL'                                           1073
         AG_PARLIST = 'NONE'                                              1073
         CALL AXMATERIAL                                                  1073
C    *                                                                    1073
C    Geant ATTRIBUTE : SFPB ===> SEEN=1 COLO=4 <===                       1073
         CALL AGSRESET                                                    1074
         AG_SEEN = 1                                                      1074
         AG_COLO = 4                                                      1074
         AG_TITLE = 'SFPB'                                                1074
         AG_EXNAME = 'ATTRIBUTE'                                          1074
         AG_PARLIST = 'SEEN_COLO'                                         1074
         CALL AXATTRIBUTE                                                 1074
C    *                                                                    1074
C    Geant SHAPE : BOX ===> DX=SFJP_SFPB_DX/2. DY=SFJP_SFPB_DY/2., DZ=SF  1074
C    JP_SFPBDZ/2. <===                                                    1074
         CALL AGSRESET                                                    1076
         AG_DX = SFJP_SFPB_DX/2.                                          1076
         AG_DY = SFJP_SFPB_DY/2.                                          1076
         AG_DZ = SFJP_SFPBDZ/2.                                           1076
         AG_TITLE = 'BOX'                                                 1076
         AG_EXNAME = 'SHAPE'                                              1076
         AG_PARLIST = 'DX_DY_DZ'                                          1076
         CALL AXSHAPE                                                     1076
C    *                                                                    1076
5531  CALL AGSPOPB                                                        1077
      GO TO BLOCK_SFPB, (5530 ,5540)                                      1077
C End of Local Procedure                                                  1077
C ----------------------------------------------------------------------  1078
C ------------ SSBS                                                       1078
C Begin Local Procedure SSBS                                              1080
C SSBS IS THE SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO, T  1080
C HE SECTOR                                                               1080
5550  CONTINUE                                                            1080
         AG_TITLE='SSBS'                                                  1080
         AG_MODULE=MODULE                                                 1080
         AG_MARK='V00'                                                    1080
         CALL AGSPUSH                                                     1080
         AG_MODULE=MODULE                                                 1080
C    *                                                                    1080
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1081
         CALL AGSRESET                                                    1081
         AG_TITLE = 'ALUMINIUM'                                           1081
         AG_EXNAME = 'MATERIAL'                                           1081
         AG_PARLIST = 'NONE'                                              1081
         CALL AXMATERIAL                                                  1081
C    *                                                                    1081
C    Geant ATTRIBUTE : SSBS ===> SEEN=1 COLO=1 <===                       1081
         CALL AGSRESET                                                    1082
         AG_SEEN = 1                                                      1082
         AG_COLO = 1                                                      1082
         AG_TITLE = 'SSBS'                                                1082
         AG_EXNAME = 'ATTRIBUTE'                                          1082
         AG_PARLIST = 'SEEN_COLO'                                         1082
         CALL AXATTRIBUTE                                                 1082
C    *                                                                    1082
C    Geant SHAPE : BOX ===> DX=SFJP_SSBS_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1082
C    JP_SSBS_DX/2. <===                                                   1082
         CALL AGSRESET                                                    1084
         AG_DX = SFJP_SSBS_DX/2.                                          1084
         AG_DY = SFJP_SSBS_DY/2.                                          1084
         AG_DZ = SFJP_SSBS_DX/2.                                          1084
         AG_TITLE = 'BOX'                                                 1084
         AG_EXNAME = 'SHAPE'                                              1084
         AG_PARLIST = 'DX_DY_DZ'                                          1084
         CALL AXSHAPE                                                     1084
C    *                                                                    1084
5551  CALL AGSPOPB                                                        1085
      GO TO BLOCK_SSBS, (5550 ,5560)                                      1085
C End of Local Procedure                                                  1085
C ----------------------------------------------------------------------  1086
C ------------ SSBB                                                       1086
C Begin Local Procedure SSBB                                              1088
C SSBB IS THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO THE,  1088
C  SECTOR                                                                 1088
5570  CONTINUE                                                            1088
         AG_TITLE='SSBB'                                                  1088
         AG_MODULE=MODULE                                                 1088
         AG_MARK='V00'                                                    1088
         CALL AGSPUSH                                                     1088
         AG_MODULE=MODULE                                                 1088
C    *                                                                    1088
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1089
         CALL AGSRESET                                                    1089
         AG_TITLE = 'ALUMINIUM'                                           1089
         AG_EXNAME = 'MATERIAL'                                           1089
         AG_PARLIST = 'NONE'                                              1089
         CALL AXMATERIAL                                                  1089
C    *                                                                    1089
C    Geant ATTRIBUTE : SSBB ===> SEEN=1 COLO=1 <===                       1089
         CALL AGSRESET                                                    1090
         AG_SEEN = 1                                                      1090
         AG_COLO = 1                                                      1090
         AG_TITLE = 'SSBB'                                                1090
         AG_EXNAME = 'ATTRIBUTE'                                          1090
         AG_PARLIST = 'SEEN_COLO'                                         1090
         CALL AXATTRIBUTE                                                 1090
C    *                                                                    1090
C    Geant SHAPE : BOX ===> DX=SFJP_SSBB_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1090
C    JP_SSBB_DZ/2. <===                                                   1090
         CALL AGSRESET                                                    1092
         AG_DX = SFJP_SSBB_DX/2.                                          1092
         AG_DY = SFJP_SSBS_DY/2.                                          1092
         AG_DZ = SFJP_SSBB_DZ/2.                                          1092
         AG_TITLE = 'BOX'                                                 1092
         AG_EXNAME = 'SHAPE'                                              1092
         AG_PARLIST = 'DX_DY_DZ'                                          1092
         CALL AXSHAPE                                                     1092
C    *                                                                    1092
5571  CALL AGSPOPB                                                        1093
      GO TO BLOCK_SSBB, (5570 ,5580)                                      1093
C End of Local Procedure                                                  1093
C ----------------------------------------------------------------------  1094
C ------------ SSST                                                       1094
C Begin Local Procedure SSST                                              1095
C SSST IS THE TOP OF THE SMALL SECTOR                                     1095
5080  CONTINUE                                                            1095
         AG_TITLE='SSST'                                                  1095
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
C    Geant ATTRIBUTE : SSST ===> SEEN=1 COLO=1 <===                       1096
         CALL AGSRESET                                                    1097
         AG_SEEN = 1                                                      1097
         AG_COLO = 1                                                      1097
         AG_TITLE = 'SSST'                                                1097
         AG_EXNAME = 'ATTRIBUTE'                                          1097
         AG_PARLIST = 'SEEN_COLO'                                         1097
         CALL AXATTRIBUTE                                                 1097
C    *                                                                    1097
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1097
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSST_WIDTH/2. <==  1097
C    =                                                                    1097
         CALL AGSRESET                                                    1100
         AG_RMIN = SFJP_SSST_RMIN                                         1100
         AG_RMAX = SFJP_SSST_RMAX                                         1100
         AG_PHI1 = SFJP_ALPHAZH                                           1100
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1100
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1100
         AG_TITLE = 'TUBS'                                                1100
         AG_EXNAME = 'SHAPE'                                              1100
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1100
         CALL AXSHAPE                                                     1100
C    *                                                                    1100
5081  CALL AGSPOPB                                                        1101
      GO TO BLOCK_SSST, (5080 ,5090)                                      1101
C End of Local Procedure                                                  1101
C ----------------------------------------------------------------------  1102
C ------------ SSSS                                                       1102
C Begin Local Procedure SSSS                                              1103
C SSSS IS THE SIDE OF THE SMALL SECTOR                                    1103
5100  CONTINUE                                                            1103
         AG_TITLE='SSSS'                                                  1103
         AG_MODULE=MODULE                                                 1103
         AG_MARK='V00'                                                    1103
         CALL AGSPUSH                                                     1103
         AG_MODULE=MODULE                                                 1103
C    *                                                                    1103
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1104
         CALL AGSRESET                                                    1104
         AG_TITLE = 'ALUMINIUM'                                           1104
         AG_EXNAME = 'MATERIAL'                                           1104
         AG_PARLIST = 'NONE'                                              1104
         CALL AXMATERIAL                                                  1104
C    *                                                                    1104
C    Geant ATTRIBUTE : SSSS ===> SEEN=1 COLO=1 <===                       1104
         CALL AGSRESET                                                    1105
         AG_SEEN = 1                                                      1105
         AG_COLO = 1                                                      1105
         AG_TITLE = 'SSSS'                                                1105
         AG_EXNAME = 'ATTRIBUTE'                                          1105
         AG_PARLIST = 'SEEN_COLO'                                         1105
         CALL AXATTRIBUTE                                                 1105
C    *                                                                    1105
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSSS_RMIN RMAX=SFJP_SSST_RMIN, PH  1105
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSSS_WIDTH/2. <==  1105
C    =                                                                    1105
         CALL AGSRESET                                                    1108
         AG_RMIN = SFJP_SSSS_RMIN                                         1108
         AG_RMAX = SFJP_SSST_RMIN                                         1108
         AG_PHI1 = SFJP_ALPHAZH                                           1108
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1108
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1108
         AG_TITLE = 'TUBS'                                                1108
         AG_EXNAME = 'SHAPE'                                              1108
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1108
         CALL AXSHAPE                                                     1108
C    *                                                                    1108
5101  CALL AGSPOPB                                                        1109
      GO TO BLOCK_SSSS, (5100 ,5110)                                      1109
C End of Local Procedure                                                  1109
C ----------------------------------------------------------------------  1110
C ------------ SSRT                                                       1110
C Begin Local Procedure SSRT                                              1111
C SSRT IS THE TOP OF THE SIDE RIB                                         1111
5120  CONTINUE                                                            1111
         AG_TITLE='SSRT'                                                  1111
         AG_MODULE=MODULE                                                 1111
         AG_MARK='V00'                                                    1111
         CALL AGSPUSH                                                     1111
         AG_MODULE=MODULE                                                 1111
C    *                                                                    1111
C    Check CONFIGB.LE.5                                                   1112
         IF (CONFIGB.LE.5) THEN                                           1112
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1113
         CALL AGSRESET                                                    1113
         AG_TITLE = 'ALUMINIUM'                                           1113
         AG_EXNAME = 'MATERIAL'                                           1113
         AG_PARLIST = 'NONE'                                              1113
         CALL AXMATERIAL                                                  1113
C    *                                                                    1113
         ELSE                                                             1113
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1115
         CALL AGSRESET                                                    1115
         AG_TITLE = 'CARBON'                                              1115
         AG_EXNAME = 'MATERIAL'                                           1115
         AG_PARLIST = 'NONE'                                              1115
         CALL AXMATERIAL                                                  1115
C    *                                                                    1115
         ENDIF                                                            1115
C    Geant ATTRIBUTE : SSRT ===> SEEN=1 COLO=1 <===                       1117
         CALL AGSRESET                                                    1117
         AG_SEEN = 1                                                      1117
         AG_COLO = 1                                                      1117
         AG_TITLE = 'SSRT'                                                1117
         AG_EXNAME = 'ATTRIBUTE'                                          1117
         AG_PARLIST = 'SEEN_COLO'                                         1117
         CALL AXATTRIBUTE                                                 1117
C    *                                                                    1117
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1117
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSST_WIDTH/2. <===      1117
         CALL AGSRESET                                                    1120
         AG_RMIN = SFJP_SSST_RMIN                                         1120
         AG_RMAX = SFJP_SSST_RMAX                                         1120
         AG_PHI1 = -SFJP_ALPHAZH                                          1120
         AG_PHI2 = +SFJP_ALPHAZH                                          1120
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1120
         AG_TITLE = 'TUBS'                                                1120
         AG_EXNAME = 'SHAPE'                                              1120
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1120
         CALL AXSHAPE                                                     1120
C    *                                                                    1120
5121  CALL AGSPOPB                                                        1121
      GO TO BLOCK_SSRT, (5120 ,5130)                                      1121
C End of Local Procedure                                                  1121
C ----------------------------------------------------------------------  1122
C ------------ SSRS                                                       1122
C Begin Local Procedure SSRS                                              1123
C SSRS IS THE SIDE OF THE SMALL RIB                                       1123
5140  CONTINUE                                                            1123
         AG_TITLE='SSRS'                                                  1123
         AG_MODULE=MODULE                                                 1123
         AG_MARK='V00'                                                    1123
         CALL AGSPUSH                                                     1123
         AG_MODULE=MODULE                                                 1123
C    *                                                                    1123
C    Check CONFIGB.LE.5                                                   1124
         IF (CONFIGB.LE.5) THEN                                           1124
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1125
         CALL AGSRESET                                                    1125
         AG_TITLE = 'ALUMINIUM'                                           1125
         AG_EXNAME = 'MATERIAL'                                           1125
         AG_PARLIST = 'NONE'                                              1125
         CALL AXMATERIAL                                                  1125
C    *                                                                    1125
         ELSE                                                             1125
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1127
         CALL AGSRESET                                                    1127
         AG_TITLE = 'CARBON'                                              1127
         AG_EXNAME = 'MATERIAL'                                           1127
         AG_PARLIST = 'NONE'                                              1127
         CALL AXMATERIAL                                                  1127
C    *                                                                    1127
         ENDIF                                                            1127
C    Geant ATTRIBUTE : SSRS ===> SEEN=1 COLO=1 <===                       1129
         CALL AGSRESET                                                    1129
         AG_SEEN = 1                                                      1129
         AG_COLO = 1                                                      1129
         AG_TITLE = 'SSRS'                                                1129
         AG_EXNAME = 'ATTRIBUTE'                                          1129
         AG_PARLIST = 'SEEN_COLO'                                         1129
         CALL AXATTRIBUTE                                                 1129
C    *                                                                    1129
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSRS_RMIN RMAX=SFJP_SSST_RMIN, PH  1129
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSSS_WIDTH/2. <===      1129
         CALL AGSRESET                                                    1132
         AG_RMIN = SFJP_SSRS_RMIN                                         1132
         AG_RMAX = SFJP_SSST_RMIN                                         1132
         AG_PHI1 = -SFJP_ALPHAZH                                          1132
         AG_PHI2 = +SFJP_ALPHAZH                                          1132
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1132
         AG_TITLE = 'TUBS'                                                1132
         AG_EXNAME = 'SHAPE'                                              1132
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1132
         CALL AXSHAPE                                                     1132
C    *                                                                    1132
5141  CALL AGSPOPB                                                        1133
      GO TO BLOCK_SSRS, (5140 ,5150)                                      1133
C End of Local Procedure                                                  1133
C ----------------------------------------------------------------------  1134
C ------------ SSLB                                                       1134
C Begin Local Procedure SSLB                                              1135
C SSLB IS THE LINKING (SECTOR TO THE CONE) BOX                            1135
5160  CONTINUE                                                            1135
         AG_TITLE='SSLB'                                                  1135
         AG_MODULE=MODULE                                                 1135
         AG_MARK='V00'                                                    1135
         CALL AGSPUSH                                                     1135
         AG_MODULE=MODULE                                                 1135
C    *                                                                    1135
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1136
         CALL AGSRESET                                                    1136
         AG_TITLE = 'AIR'                                                 1136
         AG_EXNAME = 'MATERIAL'                                           1136
         AG_PARLIST = 'NONE'                                              1136
         CALL AXMATERIAL                                                  1136
C    *                                                                    1136
C    Geant ATTRIBUTE : SSLB ===> SEEN=0 COLO=6 <===                       1136
         CALL AGSRESET                                                    1137
         AG_SEEN = 0                                                      1137
         AG_COLO = 6                                                      1137
         AG_TITLE = 'SSLB'                                                1137
         AG_EXNAME = 'ATTRIBUTE'                                          1137
         AG_PARLIST = 'SEEN_COLO'                                         1137
         CALL AXATTRIBUTE                                                 1137
C    *                                                                    1137
C    Geant SHAPE : BOX ===> DX=SFJP_SSLB_DX/2. DY=SFJP_SSLB_DY/2., DZ=SF  1137
C    JP_SSLB_DZ/2. <===                                                   1137
         CALL AGSRESET                                                    1139
         AG_DX = SFJP_SSLB_DX/2.                                          1139
         AG_DY = SFJP_SSLB_DY/2.                                          1139
         AG_DZ = SFJP_SSLB_DZ/2.                                          1139
         AG_TITLE = 'BOX'                                                 1139
         AG_EXNAME = 'SHAPE'                                              1139
         AG_PARLIST = 'DX_DY_DZ'                                          1139
         CALL AXSHAPE                                                     1139
C    *                                                                    1139
C    Create SBCH :                                                        1140
C    Call To Local Procedure SBCH                                         1140
         ASSIGN 6050 TO BLOCK_SBCH                                        1140
         GO TO  6040                                                      1140
6050     CONTINUE                                                         1140
C    *                                                                    1140
C    Geant POSITION : SBCH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/  1141
C    2. <===                                                              1141
         CALL AGSRESET                                                    1141
         AG_X = 0                                                         1141
         AG_Y = 0                                                         1141
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1141
         AG_TITLE = 'SBCH'                                                1141
         AG_EXNAME = 'POSITION'                                           1141
         AG_PARLIST = 'X_Y_Z'                                             1141
         CALL AXPOSITION                                                  1141
C    *                                                                    1141
C    Create SBCV :                                                        1142
C    Call To Local Procedure SBCV                                         1142
         ASSIGN 6070 TO BLOCK_SBCV                                        1142
         GO TO  6060                                                      1142
6070     CONTINUE                                                         1142
C    *                                                                    1142
C    Geant POSITION : SBCV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1144
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                  1144
         CALL AGSRESET                                                    1144
         AG_X = 0                                                         1144
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1144
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1144
         AG_TITLE = 'SBCV'                                                1144
         AG_EXNAME = 'POSITION'                                           1144
         AG_PARLIST = 'X_Y_Z'                                             1144
         CALL AXPOSITION                                                  1144
C    *                                                                    1144
C    Geant POSITION : SBCV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1144
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                 1144
         CALL AGSRESET                                                    1146
         AG_X = 0                                                         1146
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1146
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1146
         AG_TITLE = 'SBCV'                                                1146
         AG_EXNAME = 'POSITION'                                           1146
         AG_PARLIST = 'X_Y_Z'                                             1146
         CALL AXPOSITION                                                  1146
C    *                                                                    1146
C    Create SBFH :                                                        1147
C    Call To Local Procedure SBFH                                         1147
         ASSIGN 6090 TO BLOCK_SBFH                                        1147
         GO TO  6080                                                      1147
6090     CONTINUE                                                         1147
C    *                                                                    1147
C    Geant POSITION : SBFH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-  1148
C    SFJP_SSCR_THA/2. <===                                                1148
         CALL AGSRESET                                                    1148
         AG_X = 0                                                         1148
         AG_Y = 0                                                         1148
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1148
         AG_TITLE = 'SBFH'                                                1148
         AG_EXNAME = 'POSITION'                                           1148
         AG_PARLIST = 'X_Y_Z'                                             1148
         CALL AXPOSITION                                                  1148
C    *                                                                    1148
C    Create SBFV :                                                        1149
C    Call To Local Procedure SBFV                                         1149
         ASSIGN 6110 TO BLOCK_SBFV                                        1149
         GO TO  6100                                                      1149
6110     CONTINUE                                                         1149
C    *                                                                    1149
C    Geant POSITION : SBFV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1151
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                    1151
         CALL AGSRESET                                                    1151
         AG_X = 0                                                         1151
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1151
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1151
         AG_TITLE = 'SBFV'                                                1151
         AG_EXNAME = 'POSITION'                                           1151
         AG_PARLIST = 'X_Y_Z'                                             1151
         CALL AXPOSITION                                                  1151
C    *                                                                    1151
C    Geant POSITION : SBFV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1151
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                   1151
         CALL AGSRESET                                                    1153
         AG_X = 0                                                         1153
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1153
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1153
         AG_TITLE = 'SBFV'                                                1153
         AG_EXNAME = 'POSITION'                                           1153
         AG_PARLIST = 'X_Y_Z'                                             1153
         CALL AXPOSITION                                                  1153
C    *                                                                    1153
5161  CALL AGSPOPB                                                        1154
      GO TO BLOCK_SSLB, (5160 ,5170)                                      1154
C End of Local Procedure                                                  1154
C ----------------------------------------------------------------------  1155
C ------------ SBCH                                                       1155
C Begin Local Procedure SBCH                                              1156
C SBCH IS THE HORIZONTAL BRANCH OF THE DELRIN CROSS                       1156
6040  CONTINUE                                                            1156
         AG_TITLE='SBCH'                                                  1156
         AG_MODULE=MODULE                                                 1156
         AG_MARK='V00'                                                    1156
         CALL AGSPUSH                                                     1156
         AG_MODULE=MODULE                                                 1156
C    *                                                                    1156
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1157
         CALL AGSRESET                                                    1157
         AG_TITLE = 'DELRIN'                                              1157
         AG_EXNAME = 'MATERIAL'                                           1157
         AG_PARLIST = 'NONE'                                              1157
         CALL AXMATERIAL                                                  1157
C    *                                                                    1157
C    Geant ATTRIBUTE : SBCH ===> SEEN=1 COLO=2 <===                       1157
         CALL AGSRESET                                                    1158
         AG_SEEN = 1                                                      1158
         AG_COLO = 2                                                      1158
         AG_TITLE = 'SBCH'                                                1158
         AG_EXNAME = 'ATTRIBUTE'                                          1158
         AG_PARLIST = 'SEEN_COLO'                                         1158
         CALL AXATTRIBUTE                                                 1158
C    *                                                                    1158
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1158
C    JP_SSCR_THD/2. <===                                                  1158
         CALL AGSRESET                                                    1160
         AG_DX = SFJP_SSCR_HL/2.                                          1160
         AG_DY = SFJP_SSCR_WD/2.                                          1160
         AG_DZ = SFJP_SSCR_THD/2.                                         1160
         AG_TITLE = 'BOX'                                                 1160
         AG_EXNAME = 'SHAPE'                                              1160
         AG_PARLIST = 'DX_DY_DZ'                                          1160
         CALL AXSHAPE                                                     1160
C    *                                                                    1160
6041  CALL AGSPOPB                                                        1161
      GO TO BLOCK_SBCH, (6040 ,6050)                                      1161
C End of Local Procedure                                                  1161
C ----------------------------------------------------------------------  1162
C ------------ SBFH                                                       1162
C Begin Local Procedure SBFH                                              1163
C SBFH IS THE HORIZONTAL BRANCH OF THE ALUMIMIUM CROSS                    1163
6080  CONTINUE                                                            1163
         AG_TITLE='SBFH'                                                  1163
         AG_MODULE=MODULE                                                 1163
         AG_MARK='V00'                                                    1163
         CALL AGSPUSH                                                     1163
         AG_MODULE=MODULE                                                 1163
C    *                                                                    1163
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1164
         CALL AGSRESET                                                    1164
         AG_TITLE = 'ALUMINIUM'                                           1164
         AG_EXNAME = 'MATERIAL'                                           1164
         AG_PARLIST = 'NONE'                                              1164
         CALL AXMATERIAL                                                  1164
C    *                                                                    1164
C    Geant ATTRIBUTE : SBFH ===> SEEN=1 COLO=1 <===                       1164
         CALL AGSRESET                                                    1165
         AG_SEEN = 1                                                      1165
         AG_COLO = 1                                                      1165
         AG_TITLE = 'SBFH'                                                1165
         AG_EXNAME = 'ATTRIBUTE'                                          1165
         AG_PARLIST = 'SEEN_COLO'                                         1165
         CALL AXATTRIBUTE                                                 1165
C    *                                                                    1165
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1165
C    JP_SSCR_THA/2. <===                                                  1165
         CALL AGSRESET                                                    1167
         AG_DX = SFJP_SSCR_HL/2.                                          1167
         AG_DY = SFJP_SSCR_WD/2.                                          1167
         AG_DZ = SFJP_SSCR_THA/2.                                         1167
         AG_TITLE = 'BOX'                                                 1167
         AG_EXNAME = 'SHAPE'                                              1167
         AG_PARLIST = 'DX_DY_DZ'                                          1167
         CALL AXSHAPE                                                     1167
C    *                                                                    1167
6081  CALL AGSPOPB                                                        1168
      GO TO BLOCK_SBFH, (6080 ,6090)                                      1168
C End of Local Procedure                                                  1168
C ----------------------------------------------------------------------  1169
C ------------ SBCV                                                       1169
C Begin Local Procedure SBCV                                              1170
C SBCV IS THE VERTICAL BRANCH OF THE DELRIN CROSS                         1170
6060  CONTINUE                                                            1170
         AG_TITLE='SBCV'                                                  1170
         AG_MODULE=MODULE                                                 1170
         AG_MARK='V00'                                                    1170
         CALL AGSPUSH                                                     1170
         AG_MODULE=MODULE                                                 1170
C    *                                                                    1170
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1171
         CALL AGSRESET                                                    1171
         AG_TITLE = 'DELRIN'                                              1171
         AG_EXNAME = 'MATERIAL'                                           1171
         AG_PARLIST = 'NONE'                                              1171
         CALL AXMATERIAL                                                  1171
C    *                                                                    1171
C    Geant ATTRIBUTE : SBCV ===> SEEN=1 COLO=2 <===                       1171
         CALL AGSRESET                                                    1172
         AG_SEEN = 1                                                      1172
         AG_COLO = 2                                                      1172
         AG_TITLE = 'SBCV'                                                1172
         AG_EXNAME = 'ATTRIBUTE'                                          1172
         AG_PARLIST = 'SEEN_COLO'                                         1172
         CALL AXATTRIBUTE                                                 1172
C    *                                                                    1172
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1172
C    JP_SSCR_THD/2. <===                                                  1172
         CALL AGSRESET                                                    1174
         AG_DX = SFJP_SSCR_WD/2.                                          1174
         AG_DY = SFJP_SSCR_VL/2.                                          1174
         AG_DZ = SFJP_SSCR_THD/2.                                         1174
         AG_TITLE = 'BOX'                                                 1174
         AG_EXNAME = 'SHAPE'                                              1174
         AG_PARLIST = 'DX_DY_DZ'                                          1174
         CALL AXSHAPE                                                     1174
C    *                                                                    1174
6061  CALL AGSPOPB                                                        1175
      GO TO BLOCK_SBCV, (6060 ,6070)                                      1175
C End of Local Procedure                                                  1175
C ----------------------------------------------------------------------  1176
C ------------ SBFV                                                       1176
C Begin Local Procedure SBFV                                              1177
C SBFV IS THE VERTICAL BRANCH OF THE ALUMIMIUM CROSS                      1177
6100  CONTINUE                                                            1177
         AG_TITLE='SBFV'                                                  1177
         AG_MODULE=MODULE                                                 1177
         AG_MARK='V00'                                                    1177
         CALL AGSPUSH                                                     1177
         AG_MODULE=MODULE                                                 1177
C    *                                                                    1177
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1178
         CALL AGSRESET                                                    1178
         AG_TITLE = 'ALUMINIUM'                                           1178
         AG_EXNAME = 'MATERIAL'                                           1178
         AG_PARLIST = 'NONE'                                              1178
         CALL AXMATERIAL                                                  1178
C    *                                                                    1178
C    Geant ATTRIBUTE : SBFV ===> SEEN=1 COLO=1 <===                       1178
         CALL AGSRESET                                                    1179
         AG_SEEN = 1                                                      1179
         AG_COLO = 1                                                      1179
         AG_TITLE = 'SBFV'                                                1179
         AG_EXNAME = 'ATTRIBUTE'                                          1179
         AG_PARLIST = 'SEEN_COLO'                                         1179
         CALL AXATTRIBUTE                                                 1179
C    *                                                                    1179
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1179
C    JP_SSCR_THA/2. <===                                                  1179
         CALL AGSRESET                                                    1181
         AG_DX = SFJP_SSCR_WD/2.                                          1181
         AG_DY = SFJP_SSCR_VL/2.                                          1181
         AG_DZ = SFJP_SSCR_THA/2.                                         1181
         AG_TITLE = 'BOX'                                                 1181
         AG_EXNAME = 'SHAPE'                                              1181
         AG_PARLIST = 'DX_DY_DZ'                                          1181
         CALL AXSHAPE                                                     1181
C    *                                                                    1181
6101  CALL AGSPOPB                                                        1182
      GO TO BLOCK_SBFV, (6100 ,6110)                                      1182
C End of Local Procedure                                                  1182
C ----------------------------------------------------------------------  1183
C ------------ SSLT                                                       1183
C Begin Local Procedure SSLT                                              1184
C SSLT IS THE LINKING (SECTOR TO THE CONE) TUBE                           1184
5180  CONTINUE                                                            1184
         AG_TITLE='SSLT'                                                  1184
         AG_MODULE=MODULE                                                 1184
         AG_MARK='V00'                                                    1184
         CALL AGSPUSH                                                     1184
         AG_MODULE=MODULE                                                 1184
C    *                                                                    1184
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1185
         CALL AGSRESET                                                    1185
         AG_TITLE = 'ALUMINIUM'                                           1185
         AG_EXNAME = 'MATERIAL'                                           1185
         AG_PARLIST = 'NONE'                                              1185
         CALL AXMATERIAL                                                  1185
C    *                                                                    1185
C    Geant ATTRIBUTE : SSLT ===> SEEN=1 COLO=1 <===                       1185
         CALL AGSRESET                                                    1186
         AG_SEEN = 1                                                      1186
         AG_COLO = 1                                                      1186
         AG_TITLE = 'SSLT'                                                1186
         AG_EXNAME = 'ATTRIBUTE'                                          1186
         AG_PARLIST = 'SEEN_COLO'                                         1186
         CALL AXATTRIBUTE                                                 1186
C    *                                                                    1186
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=SFJP_SSLT_RMAX DZ=SFJP_SSLT_DZ  1186
C    /2. <===                                                             1186
         CALL AGSRESET                                                    1187
         AG_RMIN = 0.                                                     1187
         AG_RMAX = SFJP_SSLT_RMAX                                         1187
         AG_DZ = SFJP_SSLT_DZ/2.                                          1187
         AG_TITLE = 'TUBE'                                                1187
         AG_EXNAME = 'SHAPE'                                              1187
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1187
         CALL AXSHAPE                                                     1187
C    *                                                                    1187
5181  CALL AGSPOPB                                                        1188
      GO TO BLOCK_SSLT, (5180 ,5190)                                      1188
C End of Local Procedure                                                  1188
C ----------------------------------------------------------------------  1189
C ------------ SCMP                                                       1189
C Begin Local Procedure SCMP                                              1190
C SCMP IS THE MOUNTING PLATE INSERTED IN THE CONES.                       1190
5200  CONTINUE                                                            1190
         AG_TITLE='SCMP'                                                  1190
         AG_MODULE=MODULE                                                 1190
         AG_MARK='V00'                                                    1190
         CALL AGSPUSH                                                     1190
         AG_MODULE=MODULE                                                 1190
C    *                                                                    1190
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1191
         CALL AGSRESET                                                    1191
         AG_TITLE = 'ALUMINIUM'                                           1191
         AG_EXNAME = 'MATERIAL'                                           1191
         AG_PARLIST = 'NONE'                                              1191
         CALL AXMATERIAL                                                  1191
C    *                                                                    1191
C    Geant ATTRIBUTE : SCMP ===> SEEN=1 COLO=1 <===                       1191
         CALL AGSRESET                                                    1192
         AG_SEEN = 1                                                      1192
         AG_COLO = 1                                                      1192
         AG_TITLE = 'SCMP'                                                1192
         AG_EXNAME = 'ATTRIBUTE'                                          1192
         AG_PARLIST = 'SEEN_COLO'                                         1192
         CALL AXATTRIBUTE                                                 1192
C    *                                                                    1192
C    Geant SHAPE : BOX ===> DX=SFJP_SCMP_DX/2. DY=SFJP_SCMP_DY/2., DZ=SF  1192
C    JP_SCMP_DZ/2 <===                                                    1192
         CALL AGSRESET                                                    1194
         AG_DX = SFJP_SCMP_DX/2.                                          1194
         AG_DY = SFJP_SCMP_DY/2.                                          1194
         AG_DZ = SFJP_SCMP_DZ/2                                           1194
         AG_TITLE = 'BOX'                                                 1194
         AG_EXNAME = 'SHAPE'                                              1194
         AG_PARLIST = 'DX_DY_DZ'                                          1194
         CALL AXSHAPE                                                     1194
C    *                                                                    1194
5201  CALL AGSPOPB                                                        1195
      GO TO BLOCK_SCMP, (5200 ,5210)                                      1195
C End of Local Procedure                                                  1195
C ----------------------------------------------------------------------  1196
C ------------ SCVM                                                       1196
C Begin Local Procedure SCVM                                              1197
C SCVM IS THE MOTHER VOLUME OF THE V-SHAPE PIECE                          1197
5220  CONTINUE                                                            1197
         AG_TITLE='SCVM'                                                  1197
         AG_MODULE=MODULE                                                 1197
         AG_MARK='V00'                                                    1197
         CALL AGSPUSH                                                     1197
         AG_MODULE=MODULE                                                 1197
C    *                                                                    1197
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1198
         CALL AGSRESET                                                    1198
         AG_TITLE = 'AIR'                                                 1198
         AG_EXNAME = 'MATERIAL'                                           1198
         AG_PARLIST = 'NONE'                                              1198
         CALL AXMATERIAL                                                  1198
C    *                                                                    1198
C    Geant ATTRIBUTE : SCVM ===> SEEN=0 COLO=6 <===                       1198
         CALL AGSRESET                                                    1199
         AG_SEEN = 0                                                      1199
         AG_COLO = 6                                                      1199
         AG_TITLE = 'SCVM'                                                1199
         AG_EXNAME = 'ATTRIBUTE'                                          1199
         AG_PARLIST = 'SEEN_COLO'                                         1199
         CALL AXATTRIBUTE                                                 1199
C    *                                                                    1199
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVM_DY/2., DZ=SF  1199
C    JP_SCVM_DZ/2 <===                                                    1199
         CALL AGSRESET                                                    1201
         AG_DX = SFJP_SCVM_DX/2.                                          1201
         AG_DY = SFJP_SCVM_DY/2.                                          1201
         AG_DZ = SFJP_SCVM_DZ/2                                           1201
         AG_TITLE = 'BOX'                                                 1201
         AG_EXNAME = 'SHAPE'                                              1201
         AG_PARLIST = 'DX_DY_DZ'                                          1201
         CALL AXSHAPE                                                     1201
C    *                                                                    1201
C    Create SCVB :                                                        1202
C    Call To Local Procedure SCVB                                         1202
         ASSIGN 6130 TO BLOCK_SCVB                                        1202
         GO TO  6120                                                      1202
6130     CONTINUE                                                         1202
C    *                                                                    1202
C    Geant POSITION : SCVB ===> X=0. Y=-SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.   1203
C    Z=0. <===                                                            1203
         CALL AGSRESET                                                    1203
         AG_X = 0.                                                        1203
         AG_Y = -SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.                          1203
         AG_Z = 0.                                                        1203
         AG_TITLE = 'SCVB'                                                1203
         AG_EXNAME = 'POSITION'                                           1203
         AG_PARLIST = 'X_Y_Z'                                             1203
         CALL AXPOSITION                                                  1203
C    *                                                                    1203
C    Create SCVS :                                                        1204
C    Call To Local Procedure SCVS                                         1204
         ASSIGN 6150 TO BLOCK_SCVS                                        1204
         GO TO  6140                                                      1204
6150     CONTINUE                                                         1204
C    *                                                                    1204
C    Geant POSITION : SCVS ===> X=SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALPH  1205
C    AZ=SFJP_ALPHAZH <===                                                 1205
         CALL AGSRESET                                                    1205
         AG_X = SFJP_SCVS_PX                                              1205
         AG_Y = SFJP_SCVS_PY                                              1205
         AG_Z = 0.0                                                       1205
         AG_ALPHAN=AG_ALPHAN+1                                            1205
         AG_ALPHAK(AG_ALPHAN)=3                                           1205
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                1205
         AG_TITLE = 'SCVS'                                                1205
         AG_EXNAME = 'POSITION'                                           1205
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1205
         CALL AXPOSITION                                                  1205
C    *                                                                    1205
C    Geant POSITION : SCVS ===> X=-SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALP  1205
C    HAZ=-SFJP_ALPHAZH <===                                               1205
         CALL AGSRESET                                                    1206
         AG_X = -SFJP_SCVS_PX                                             1206
         AG_Y = SFJP_SCVS_PY                                              1206
         AG_Z = 0.0                                                       1206
         AG_ALPHAN=AG_ALPHAN+1                                            1206
         AG_ALPHAK(AG_ALPHAN)=3                                           1206
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               1206
         AG_TITLE = 'SCVS'                                                1206
         AG_EXNAME = 'POSITION'                                           1206
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1206
         CALL AXPOSITION                                                  1206
C    *                                                                    1206
5221  CALL AGSPOPB                                                        1207
      GO TO BLOCK_SCVM, (5220 ,5230)                                      1207
C End of Local Procedure                                                  1207
C ----------------------------------------------------------------------  1208
C ------------ SCVB                                                       1208
C Begin Local Procedure SCVB                                              1209
C SCVB IS THE BASE PLATE OF THE V-SHAPE PIECE                             1209
6120  CONTINUE                                                            1209
         AG_TITLE='SCVB'                                                  1209
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
C    Geant ATTRIBUTE : SCVB ===> SEEN=1 COLO=1 <===                       1210
         CALL AGSRESET                                                    1211
         AG_SEEN = 1                                                      1211
         AG_COLO = 1                                                      1211
         AG_TITLE = 'SCVB'                                                1211
         AG_EXNAME = 'ATTRIBUTE'                                          1211
         AG_PARLIST = 'SEEN_COLO'                                         1211
         CALL AXATTRIBUTE                                                 1211
C    *                                                                    1211
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVB_DY/2., DZ=SF  1211
C    JP_SCVM_DZ/2 <===                                                    1211
         CALL AGSRESET                                                    1213
         AG_DX = SFJP_SCVM_DX/2.                                          1213
         AG_DY = SFJP_SCVB_DY/2.                                          1213
         AG_DZ = SFJP_SCVM_DZ/2                                           1213
         AG_TITLE = 'BOX'                                                 1213
         AG_EXNAME = 'SHAPE'                                              1213
         AG_PARLIST = 'DX_DY_DZ'                                          1213
         CALL AXSHAPE                                                     1213
C    *                                                                    1213
6121  CALL AGSPOPB                                                        1214
      GO TO BLOCK_SCVB, (6120 ,6130)                                      1214
C End of Local Procedure                                                  1214
C ----------------------------------------------------------------------  1215
C ------------ SCVS                                                       1215
C Begin Local Procedure SCVS                                              1216
C SCVS IS THE SIDE PLATE OF THE V-SHAPE PIECE                             1216
6140  CONTINUE                                                            1216
         AG_TITLE='SCVS'                                                  1216
         AG_MODULE=MODULE                                                 1216
         AG_MARK='V00'                                                    1216
         CALL AGSPUSH                                                     1216
         AG_MODULE=MODULE                                                 1216
C    *                                                                    1216
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1217
         CALL AGSRESET                                                    1217
         AG_TITLE = 'ALUMINIUM'                                           1217
         AG_EXNAME = 'MATERIAL'                                           1217
         AG_PARLIST = 'NONE'                                              1217
         CALL AXMATERIAL                                                  1217
C    *                                                                    1217
C    Geant ATTRIBUTE : SCVS ===> SEEN=1 COLO=1 <===                       1217
         CALL AGSRESET                                                    1218
         AG_SEEN = 1                                                      1218
         AG_COLO = 1                                                      1218
         AG_TITLE = 'SCVS'                                                1218
         AG_EXNAME = 'ATTRIBUTE'                                          1218
         AG_PARLIST = 'SEEN_COLO'                                         1218
         CALL AXATTRIBUTE                                                 1218
C    *                                                                    1218
C    Geant SHAPE : BOX ===> DX=SFJP_SCVS_DX/2. DY=SFJP_SCVS_DY/2., DZ=SF  1218
C    JP_SCVM_DZ/2 <===                                                    1218
         CALL AGSRESET                                                    1220
         AG_DX = SFJP_SCVS_DX/2.                                          1220
         AG_DY = SFJP_SCVS_DY/2.                                          1220
         AG_DZ = SFJP_SCVM_DZ/2                                           1220
         AG_TITLE = 'BOX'                                                 1220
         AG_EXNAME = 'SHAPE'                                              1220
         AG_PARLIST = 'DX_DY_DZ'                                          1220
         CALL AXSHAPE                                                     1220
C    *                                                                    1220
6141  CALL AGSPOPB                                                        1221
      GO TO BLOCK_SCVS, (6140 ,6150)                                      1221
C End of Local Procedure                                                  1221
C ----------------------------------------------------------------------  1222
C ------------ SOSK                                                       1222
C Begin Local Procedure SOSK                                              1223
C SOSK IS THE SEPARATION SHIELD CYLINDER                                  1223
5030  CONTINUE                                                            1223
         AG_TITLE='SOSK'                                                  1223
         AG_MODULE=MODULE                                                 1223
         AG_MARK='V00'                                                    1223
         CALL AGSPUSH                                                     1223
         AG_MODULE=MODULE                                                 1223
C    *                                                                    1223
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                    1224
         CALL AGSRESET                                                    1224
         AG_A = 12                                                        1224
         AG_Z = 6                                                         1224
         AG_W = 10000*5                                                   1224
         AG_TITLE = 'C5'                                                  1224
         AG_EXNAME = 'COMPONENT'                                          1224
         AG_PARLIST = 'A_Z_W'                                             1224
         CALL AXCOMPONENT                                                 1224
C    *                                                                    1224
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                     1224
         CALL AGSRESET                                                    1225
         AG_A = 1                                                         1225
         AG_Z = 1                                                         1225
         AG_W = 10000*4                                                   1225
         AG_TITLE = 'H4'                                                  1225
         AG_EXNAME = 'COMPONENT'                                          1225
         AG_PARLIST = 'A_Z_W'                                             1225
         CALL AXCOMPONENT                                                 1225
C    *                                                                    1225
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                    1225
         CALL AGSRESET                                                    1226
         AG_A = 16                                                        1226
         AG_Z = 8                                                         1226
         AG_W = 10000*2                                                   1226
         AG_TITLE = 'O2'                                                  1226
         AG_EXNAME = 'COMPONENT'                                          1226
         AG_PARLIST = 'A_Z_W'                                             1226
         CALL AXCOMPONENT                                                 1226
C    *                                                                    1226
C    Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                       1226
         CALL AGSRESET                                                    1227
         AG_A = 27                                                        1227
         AG_Z = 13                                                        1227
         AG_W = 986                                                       1227
         AG_TITLE = 'AL'                                                  1227
         AG_EXNAME = 'COMPONENT'                                          1227
         AG_PARLIST = 'A_Z_W'                                             1227
         CALL AXCOMPONENT                                                 1227
C    *                                                                    1227
C    Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                       1227
         CALL AGSRESET                                                    1228
         AG_DENS = 1.40845                                                1228
         AG_TITLE = 'SSDALMY'                                             1228
         AG_EXNAME = 'MIXTURE'                                            1228
         AG_PARLIST = 'DENS'                                              1228
         CALL AXMIXTURE                                                   1228
C    *                                                                    1228
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1228
         CALL AGSRESET                                                    1229
         AG_TITLE = 'SSDALMY'                                             1229
         AG_EXNAME = 'MATERIAL'                                           1229
         AG_PARLIST = 'NONE'                                              1229
         CALL AXMATERIAL                                                  1229
C    *                                                                    1229
C    Geant ATTRIBUTE : SOSK ===> SEEN=1 COLO=3 <===                       1229
         CALL AGSRESET                                                    1230
         AG_SEEN = 1                                                      1230
         AG_COLO = 3                                                      1230
         AG_TITLE = 'SOSK'                                                1230
         AG_EXNAME = 'ATTRIBUTE'                                          1230
         AG_PARLIST = 'SEEN_COLO'                                         1230
         CALL AXATTRIBUTE                                                 1230
C    *                                                                    1230
C    Geant SHAPE : TUBE ===> RMIN=SFPA_SOUTRINN RMAX=SFPA_SOUTROUT, DZ=S  1230
C    FPA_SOUTLEN <===                                                     1230
         CALL AGSRESET                                                    1232
         AG_RMIN = SFPA_SOUTRINN                                          1232
         AG_RMAX = SFPA_SOUTROUT                                          1232
         AG_DZ = SFPA_SOUTLEN                                             1232
         AG_TITLE = 'TUBE'                                                1232
         AG_EXNAME = 'SHAPE'                                              1232
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1232
         CALL AXSHAPE                                                     1232
C    *                                                                    1232
5031  CALL AGSPOPB                                                        1233
      GO TO BLOCK_SOSK, (5030 ,5040)                                      1233
C End of Local Procedure                                                  1233
      WRITE(LOUT,6160)'===>>  [End AgML Module SISDGEO7]  <<==='          1233
6160  FORMAT(A32,/,/)                                                     1233
C MODULE SISDGEO7                                                         1235
      END                                                                 1235
C ----------------------------------------------------------------------  1236
C ------------ TRANS_TO_SFM                                               1236
      SUBROUTINE TRANS_TO_SFM(X, Y,XC, YC,TILT, XPOS, YPOS)               1237
C INTENT=IN                                                               1238
      REAL X                                                              1238
C INTENT=IN                                                               1239
      REAL Y                                                              1239
C INTENT=IN                                                               1240
      REAL XC                                                             1240
C INTENT=IN                                                               1241
      REAL YC                                                             1241
C INTENT=IN                                                               1242
      REAL TILT                                                           1242
C INTENT=INOUT                                                            1243
      REAL XPOS                                                           1243
C INTENT=INOUT                                                            1244
      REAL YPOS                                                           1244
      REAL *8 SINT, COST, DEGRAD                                          1245
      DEGRAD=3.1415926/180.0                                              1247
      SINT=SIN(DEGRAD*TILT)                                               1248
      COST=COS(DEGRAD*TILT)                                               1249
      XPOS = XC + COST*X - SINT*Y                                         1250
      YPOS = YC + SINT*X + COST*Y                                         1251
      RETURN                                                              1252
C SUBROUTINE TRANS_TO_SFM                                                 1253
      END                                                                 1253
